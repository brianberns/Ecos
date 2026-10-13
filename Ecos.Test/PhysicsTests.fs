namespace Ecos.Test

open System
open Xunit
open Ecos

/// Checks that the model reproduces known physics: small systems
/// match their exact equilibrium distributions, and chains diffuse as
/// expected.
///
/// Each measurement averages over many independent runs. Seeds are
/// fixed, so results are reproducible.
module PhysicsTests =

    /// Oracle: do atoms at the given locations overlap? Each atom
    /// occupies a 2x2 square, so they overlap if they are closer than
    /// 2 along both axes.
    let private overlaps (a : Point) (b : Point) =
        abs (a.X - b.X) < 2 && abs (a.Y - b.Y) < 2

    /// Chi-squared goodness of fit of observed counts against exact
    /// weights. Bins expected to hold fewer than 5 samples are merged
    /// into one. Fails if any observed state has no exact weight.
    let private checkFit name (exact : Map<'state, float>) (samples : 'state[]) =
        let observed = samples |> Array.countBy id |> Map.ofArray
        for state in observed.Keys do
            Assert.True(exact.ContainsKey state, $"{name}: invalid state {state}")
        let totalWeight = exact.Values |> Seq.sum
        let bins =
            exact
                |> Map.toArray
                |> Array.map (fun (state, weight) ->
                    let expected = float samples.Length * weight / totalWeight
                    let obs =
                        observed
                            |> Map.tryFind state
                            |> Option.defaultValue 0
                            |> float
                    expected, obs)
        let large, small = bins |> Array.partition (fun (expected, _) -> expected >= 5.0)
        let bins =
            if small.Length = 0 then large
            else Array.append large [| Array.sumBy fst small, Array.sumBy snd small |]
        let chi2 =
            bins |> Array.sumBy (fun (expected, obs) -> (obs - expected) ** 2.0 / expected)
        let df = float (bins.Length - 1)
        let limit = df + 5.0 * sqrt (2.0 * df)   // mean + 5 standard deviations
        Assert.True(chi2 < limit, $"{name}: chi^2 = {chi2:F1}, limit {limit:F1} (df = {df})")

    /// Runs nRuns independent copies of the given world with the given
    /// parameters. Each runs for burnIn sweeps, and then takes nSamples
    /// samples, sampleInterval sweeps apart, by applying the given
    /// function to the world.
    let private sample
        parms nRuns burnIn sampleInterval nSamples
        (observe : World -> 'sample) (world : World) =
        Array.Parallel.init nRuns (fun iRun ->
            let rng = Random(iRun)
            let mutable world = Chain.sweepsWith parms rng burnIn world
            let samples = Array.zeroCreate<'sample> nSamples
            for i in 0 .. nSamples - 1 do
                world <- Chain.sweepsWith parms rng sampleInterval world
                samples[i] <- observe world
            samples)
            |> Array.concat

    /// Adds a dictionary entry, or adds to an existing one.
    let private addWeight (dict : Collections.Generic.Dictionary<'key, float>) key weight =
        dict[key] <-
            match dict.TryGetValue key with
                | true, w -> w + weight
                | _ -> weight

    /// Converts a dictionary to a map.
    let private toMap (dict : Collections.Generic.Dictionary<'key, float>) =
        dict
            |> Seq.map (fun (KeyValue(key, weight)) -> key, weight)
            |> Map.ofSeq

    /// Equilibrium of a chain: update's hops are proposed symmetrically
    /// and accepted only when valid, so in the long run every valid
    /// conformation of a chain is equally likely.
    ///
    /// For a short chain, we can enumerate the conformations exactly: no
    /// two atoms overlap, the angle at every interior atom is wide
    /// enough, and no two bonds cross. (Hops can't create a crossing,
    /// and the chain starts without one.) We compare the exact
    /// distribution of the end-to-end vector against simulations started
    /// far from equilibrium (a straight rod).
    ///
    /// All atoms are type W, so no weak bonds can form, and strong
    /// bonds are frozen, so the chain can't break or close into a ring.
    let private checkChain n =

            // exact number of conformations for each end-to-end vector
        let counts = Collections.Generic.Dictionary<Point, float>()
        let locs = Array.zeroCreate<Point> n
        let crosses a b c d =
            Geometry.segmentsIntersect locs[a] locs[b] locs[c] locs[d]
        let rec enumerate i =
            if i = n then
                let anyCrossing =
                    List.exists id [
                        for a in 0 .. n - 2 do
                            for c in a + 2 .. n - 2 do   // bonds a-(a+1) and c-(c+1) share no atom
                                crosses a (a + 1) c (c + 1)
                    ]
                if not anyCrossing then
                    addWeight counts (locs[n - 1] - locs[0]) 1.0
            else
                for v in Chain.bondVectors do
                    let loc = locs[i - 1] + v
                    let clear =
                        Seq.forall (fun j -> not (overlaps loc locs[j])) (seq { 0 .. i - 1 })
                    let angleOk =
                        i < 2
                            || Bonds.isAngleAllowed Strong locs[i - 1] locs[i - 2] loc
                    if clear && angleOk then
                        locs[i] <- loc
                        enumerate (i + 1)
        locs[0] <- Point.create (0, 0)
        enumerate 1

            // sample end-to-end vectors, starting from a straight rod
        let world =
            (World.empty, [ 0 .. n - 1 ])
                ||> List.fold (fun world i ->
                    let world =
                        World.addAtomW (Point.create (2 * i, 0)) world
                    if i > 0 then World.addStrongBond (i - 1) i world
                    else world)
        let samples =
            world
                |> sample
                    (TestParameters.createFrozen Chain.athermalKT)
                    8 (200 * n) (20 * n) 5000
                    (fun world ->
                        world.Atoms[n - 1].Location - world.Atoms[0].Location)

        checkFit $"End-to-end vector, N = {n}" (toMap counts) samples

    /// A three-atom chain: the simplest case with a bond angle.
    [<Fact>]
    let ``Trimer samples all conformations uniformly`` () =
        checkChain 3

    /// A five-atom chain: long enough for atoms that aren't neighbors
    /// to get in each other's way.
    [<Fact>]
    let ``Pentamer samples all conformations uniformly`` () =
        checkChain 5

    /// The bond between two atoms, if any.
    type private Bond = NoBond | WeakBond | StrongBond

    /// Walls, free atoms, and both bond types: two complementary atoms
    /// (types W and X) are confined to a small box of walls, at a
    /// temperature high enough that strong bonds form and break many
    /// times. We can enumerate every state exactly: each placement of
    /// the two atoms in the box without overlap, and then
    /// * unbonded (E = 0), or
    /// * weakly bonded (E = weakBondEnergy), if in weak bond range, or
    /// * strongly bonded (E = strongBondEnergy), if in strong bond
    ///   range.
    /// States are grouped by the atoms' separation. The simulation must
    /// match the Boltzmann distribution, and must never produce a state
    /// that isn't in the box.
    ///
    /// The barrier to strong bonds affects only how fast they form and
    /// break, not this distribution, so this also checks that it's
    /// applied equally in both directions.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution`` () =
        let size = 2   // atom locations have |x| <= size and |y| <= size
        let kT = 12.0

            // exact Boltzmann weight of each (separation, bond) state
        let locs =
            [
                for x in -size .. size do
                    for y in -size .. size do
                        Point.create (x, y)
            ]
        let exact =
            [
                for a in locs do
                    for b in locs do
                        let d = b - a
                        if not (overlaps a b) then d
            ]
                |> List.countBy id
                |> List.collect (fun (d, count) ->
                    [
                        (d, NoBond), float count
                        if Array.contains d Chain.weakBondVectors then
                            (d, WeakBond), float count * exp (-Chain.weakBondEnergy / kT)
                        if Array.contains d Chain.bondVectors then
                            (d, StrongBond), float count * exp (-Chain.strongBondEnergy / kT)
                    ])
                |> Map.ofList

            // make sure every kind of state is part of the test
        for bond in [ NoBond; WeakBond; StrongBond ] do
            Assert.True(
                exact.Keys |> Seq.exists (fun (_, b) -> b = bond),
                $"No {bond} states enumerated")

            // sample states. A strong bond lasts hundreds of sweeps, so
            // samples must be far apart.
        let inBox (loc : Point) = abs loc.X <= size && abs loc.Y <= size
        let samples =
            World.empty
                |> World.addAtom AtomType.W (Point.create (-size, -size))
                |> World.addAtom AtomType.X (Point.create (size, size))
                |> Chain.addBox size
                |> sample
                    (TestParameters.create kT) 8 2000 1000 3000
                    (fun world ->
                        let a = world.Atoms[0].Location
                        let b = world.Atoms[1].Location
                        Assert.True(inBox a && inBox b, $"Atom escaped the box: {a}, {b}")
                        let bond =
                            if Bonds.areStronglyBonded world 0 1 then StrongBond
                            elif Bonds.areWeaklyBonded world 0 1 then WeakBond
                            else NoBond
                        b - a, bond)

        checkFit "Separation and bond" exact samples

    /// Pairing, opposite sides, and stacking: two dimers (W0-W1 and
    /// X2-X3, each strongly bonded) are confined to a small box. Either
    /// W can pair with either X, so there are four possible weak bonds:
    /// 0-2, 0-3, 1-2, and 1-3.
    ///
    /// We enumerate every state exactly: each placement of the two
    /// dimers in the box, without overlaps and without their strong
    /// bonds crossing (which hops can't cause), with each set of weak
    /// bonds that are in range and can exist together. Two weak bonds
    /// that share an atom need that atom's partners to be on opposite
    /// sides of it; two that don't share an atom must not cross. Those
    /// always form a stack, since their ends are strongly bonded on
    /// both sides. A state's energy is weakBondEnergy per weak bond
    /// plus stackEnergy per stack.
    ///
    /// The simulation must match the exact distribution of both the
    /// set of weak bonds and the separation of atoms 0 and 2. Strong
    /// bonds are frozen, so the dimers can't break or bond to each
    /// other.
    [<Fact>]
    let ``Two dimers in a box follow the Boltzmann distribution`` () =
        let size = 2   // atom locations have |x| <= size and |y| <= size
        let kT = 1.0
        let candidates = [ 0, 2; 0, 3; 1, 2; 1, 3 ]
        let isStrong (i, j) = (min i j, max i j) = (0, 1) || (min i j, max i j) = (2, 3)
        let sharesAtom (i, j) (k, l) = i = k || i = l || j = k || j = l

            // number of stacks formed by the given weak bonds
        let countStacks (bonds : list<int * int>) =
            List.sum [
                for iBond, (a, b) in List.indexed bonds do
                    for c, d in List.skip (iBond + 1) bonds do
                        if (isStrong (a, c) && isStrong (b, d))
                            || (isStrong (a, d) && isStrong (b, c)) then 1
            ]

            // every placement of a dimer in the box
        let inBox (loc : Point) = abs loc.X <= size && abs loc.Y <= size
        let dimers =
            [|
                for x in -size .. size do
                    for y in -size .. size do
                        for v in Chain.bondVectors do
                            let a = Point.create (x, y)
                            if inBox (a + v) then a, a + v
            |]

            // exact Boltzmann weights of weak bond sets and of separations
        let bondSetWeights = Collections.Generic.Dictionary<list<int * int>, float>()
        let separationWeights = Collections.Generic.Dictionary<Point, float>()
        for loc0, loc1 in dimers do
            for loc2, loc3 in dimers do
                let locs = [| loc0; loc1; loc2; loc3 |]
                let clear =
                    not (overlaps loc0 loc2) && not (overlaps loc0 loc3)
                        && not (overlaps loc1 loc2) && not (overlaps loc1 loc3)
                let crosses (a, b) (c, d) =
                    Geometry.segmentsIntersect locs[a] locs[b] locs[c] locs[d]
                if clear && not (crosses (0, 1) (2, 3)) then

                        // weak bonds that are in range. (Each shares an atom
                        // with both strong bonds, so can't cross either.)
                    let feasible =
                        candidates
                            |> List.filter (fun (i, j) ->
                                Array.contains (locs[j] - locs[i]) Chain.weakBondVectors)

                        // can the two given weak bonds exist together?
                    let compatible (a, b) (c, d) =
                        if sharesAtom (a, b) (c, d) then
                            let shared, other1, other2 =
                                if a = c then a, b, d
                                elif a = d then a, b, c
                                elif b = c then b, a, d
                                else b, a, c
                            Bonds.areOpposite locs[shared] locs[other1] locs[other2]
                        else not (crosses (a, b) (c, d))

                        // every set of feasible weak bonds in which each pair
                        // is compatible
                    let bondSets =
                        (feasible, [ [] ])
                            ||> List.foldBack (fun bond sets ->
                                sets @ (sets |> List.map (fun set -> bond :: set)))
                            |> List.where (fun bonds ->
                                List.allPairs bonds bonds
                                    |> List.forall (fun (bondA, bondB) ->
                                        bondA >= bondB || compatible bondA bondB))

                    for bonds in bondSets do
                        let energy =
                            Chain.weakBondEnergy * float bonds.Length
                                + Chain.stackEnergy * float (countStacks bonds)
                        let weight = exp (-energy / kT)
                        addWeight bondSetWeights bonds weight
                        addWeight separationWeights (loc2 - loc0) weight

            // make sure stacked states are actually part of the test
        Assert.True(
            bondSetWeights.Keys |> Seq.exists (fun bonds -> countStacks bonds > 0),
            "No stacked states enumerated")

            // sample states, starting with the dimers apart
        let samples =
            World.empty
                |> World.addAtom AtomType.W (Point.create(-2, -2))   // 0
                |> World.addAtom AtomType.W (Point.create(0, -2))    // 1
                |> World.addAtom AtomType.X (Point.create(0, 2))     // 2
                |> World.addAtom AtomType.X (Point.create(2, 2))     // 3
                |> World.addStrongBond 0 1
                |> World.addStrongBond 2 3
                |> Chain.addBox size
                |> sample
                    (TestParameters.createFrozen kT) 8 2000 300 3000
                    (fun world ->
                        Set.toList (Bonds.weakBonds world),
                        world.Atoms[2].Location - world.Atoms[0].Location)

        checkFit "Weak bonds" (toMap bondSetWeights) (Array.map fst samples)
        checkFit "Separation" (toMap separationWeights) (Array.map snd samples)

    /// Chain lengths for the diffusion tests.
    let private lengths = [ 8; 16; 32; 64 ]

    /// Independent chains per length.
    let private nChains = 32

    /// Centre-of-mass mean squared displacement (MSD) at various lags,
    /// for each chain length. Shared by the diffusion tests. Chains are
    /// all type W, and strong bonds are frozen.
    let private diffusionData =
        lazy (
            let parms = TestParameters.createFrozen Chain.athermalKT
            let nFrames, frameInterval = 200, 10   // sweeps
            let lags = [ 1; 2; 5; 10; 20 ]         // frames (i.e. 10 to 200 sweeps)
            [
                for n in lengths do

                        // centre-of-mass trajectory of each chain
                    let trajectories =
                        Array.Parallel.init nChains (fun iChain ->
                            let rng = Random(5000 * n + iChain)
                            let mutable world = Chain.grow rng n
                            Array.init (nFrames + 1) (fun iFrame ->
                                if iFrame > 0 then
                                    world <- Chain.sweepsWith parms rng frameInterval world
                                Chain.centerOfMass world))

                        // MSD averaged over chains and over all time origins
                    let msd lag =
                        trajectories
                            |> Array.collect (fun traj ->
                                Array.init (nFrames + 1 - lag) (fun i ->
                                    let x0, y0 = traj[i]
                                    let x1, y1 = traj[i + lag]
                                    (x1 - x0) ** 2.0 + (y1 - y0) ** 2.0))
                            |> Array.average

                    n, [ for lag in lags -> float (lag * frameInterval), msd lag ]
            ])

    /// Diffusion: the centre of mass performs a random walk, so its MSD
    /// grows linearly with time, i.e. MSD = 4 D t in 2D.
    ///
    /// Stiff chains fall slightly short of that over the times measured
    /// here (MSD ~ t^0.9, for every length). An atom inside a stiff
    /// chain can only move a little before its neighbors must follow,
    /// so a move is often followed by its reverse, and successive
    /// displacements are anticorrelated. The exponent rises slowly
    /// toward 1 at longer times.
    [<Fact>]
    let ``Chain's centre of mass diffuses`` () =
        for n, msds in diffusionData.Value do
            let exponent = Stats.logLogSlope msds
            Assert.True(
                exponent > 0.85 && exponent < 1.1,
                $"N = {n}: MSD ~ t^{exponent:F3}, expected about t^1")

    /// Rouse dynamics: with only local moves (and no solvent), each
    /// move shifts the centre of mass by 1/N of a step, so the diffusion
    /// constant scales roughly as D ~ 1/N. Chains are stiff, though, so
    /// the atoms at a chain's ends move more freely than those inside
    /// it. That helps short chains most, so D falls off somewhat faster
    /// than 1/N over these lengths.
    [<Fact>]
    let ``Diffusion constant scales inversely with chain length`` () =
        let points =
            [
                for n, msds in diffusionData.Value do
                    let d = Stats.slope ((0.0, 0.0) :: msds) / 4.0
                    float n, d
            ]
        let exponent = Stats.logLogSlope points
        Assert.InRange(exponent, -1.35, -0.85)
