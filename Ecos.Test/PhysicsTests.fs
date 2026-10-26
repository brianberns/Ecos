namespace Ecos.Test

open System
open Xunit
open Ecos

/// Helpers for checking that the model reproduces known physics: small
/// systems match their exact equilibrium distributions, and chains
/// diffuse as expected. Each measurement averages over many independent
/// runs. Seeds are fixed, so results are reproducible. (Each module of
/// tests below runs in parallel with the others.)
module PhysicsHelpers =

    /// Oracle: do atoms at the given locations overlap? Each atom
    /// occupies a 2x2 square, so they overlap if they are closer than
    /// 2 along both axes.
    let overlaps (a : Point) (b : Point) =
        abs (a.X - b.X) < 2 && abs (a.Y - b.Y) < 2

    /// Chi-squared goodness of fit of observed counts against exact
    /// weights. Bins expected to hold fewer than 5 samples are merged
    /// into one. Fails if any observed state has no exact weight.
    let checkFit name (exact : Map<'state, float>) (samples : 'state[]) =
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
    let sample
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
    let addWeight (dict : Collections.Generic.Dictionary<'key, float>) key weight =
        dict[key] <-
            match dict.TryGetValue key with
                | true, w -> w + weight
                | _ -> weight

    /// Converts a dictionary to a map.
    let toMap (dict : Collections.Generic.Dictionary<'key, float>) =
        dict
            |> Seq.map (fun (KeyValue(key, weight)) -> key, weight)
            |> Map.ofSeq

/// Chains: conformations with and without bend stiffness, bonds that
/// form and break with bends, and diffusion.
module ChainPhysicsTests =

    open PhysicsHelpers

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
    ///
    /// With a bend stiffness, conformations are no longer equally
    /// likely: each counts with its Boltzmann weight, the product of
    /// exp(-E/kT) over the bends at its interior atoms.
    let private checkChainWith (parms : Parameters) n =
        let kT = parms.GetTemperature Point.zero   // uniform
        let strong = parms.BondTypeParameterMap[Strong]

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
                    let weight =
                        Seq.init (n - 2) (fun i ->
                            exp (-Bonds.bendEnergy strong.BendStiffness locs[i + 1] locs[i] locs[i + 2] / kT))
                            |> Seq.fold (*) 1.0
                    addWeight counts (locs[n - 1] - locs[0]) weight
            else
                for v in Chain.bondVectors do
                    let loc = locs[i - 1] + v
                    let clear =
                        Seq.forall (fun j -> not (overlaps loc locs[j])) (seq { 0 .. i - 1 })
                    let angleOk =
                        i < 2
                            || Bonds.isBendAllowed strong.BendLimit locs[i - 1] locs[i - 2] loc
                    if clear && angleOk then
                        locs[i] <- loc
                        enumerate (i + 1)
        locs[0] <- Point.create (0, 0)
        enumerate 1

            // sample end-to-end vectors, starting from a straight rod
        let world =
            (TestWorld.empty, [ 0 .. n - 1 ])
                ||> List.fold (fun world i ->
                    let world =
                        World.addAtomW (Point.create (2 * i, 0)) world
                    if i > 0 then World.addStrongBond TestParameters.standard (i - 1) i world
                    else world)
        let samples =
            world
                |> sample
                    parms
                    8 (200 * n) (20 * n) 5000
                    (fun world -> World.getDisplacement world.Atoms[0] world.Atoms[n - 1] world)

        checkFit $"End-to-end vector, N = {n}" (toMap counts) samples

    /// Checks a chain of the given length with the standard parameters,
    /// whose strands are nearly straight and cost nothing to bend.
    let private checkChain n =
        checkChainWith (TestParameters.createFrozen Chain.athermalKT) n

    /// Parameters for chains that bend freely, up to right angles, at a
    /// cost.
    let private bendable kappa kT =
        TestParameters.createFrozen kT
            |> TestParameters.withBend Strong (Rational.create 0 1) kappa

    /// A three-atom chain: the simplest case with a bond angle.
    [<Fact>]
    let ``Trimer samples all conformations uniformly`` () =
        checkChain 3

    /// A five-atom chain: long enough for atoms that aren't neighbors
    /// to get in each other's way.
    [<Fact>]
    let ``Pentamer samples all conformations uniformly`` () =
        checkChain 5

    /// A three-atom chain that bends up to right angles, with a cost
    /// for bending: hops must sample each conformation with its
    /// Boltzmann weight.
    [<Fact>]
    let ``Trimer with bend stiffness follows the Boltzmann distribution`` () =
        checkChainWith (bendable 1.0 1.0) 3

    /// A five-atom chain that bends up to right angles, with a cost for
    /// bending: a move of one atom changes the bends at it and at its
    /// partners.
    [<Fact>]
    let ``Pentamer with bend stiffness follows the Boltzmann distribution`` () =
        checkChainWith (bendable 1.0 1.0) 5

    /// Bends from forming and breaking bonds: three atoms of type W are
    /// confined to a small box, at a temperature at which strong bonds
    /// form and break often, and strands bend up to right angles at a
    /// cost. We enumerate every state exactly: each placement of the
    /// three atoms without overlap, with each set of strong bonds that
    /// are in range and can exist together. (A triangle can't: one of
    /// its angles is too sharp.) A state's energy is -strongBondEnergy
    /// per bond, plus the bend energy where two bonds meet.
    ///
    /// States are grouped by the number of bonds and, for one bond, its
    /// squared length, or for two, the squared distance between the
    /// ends of the strand, which measures the bend.
    [<Fact>]
    let ``Three atoms in a box follow the Boltzmann distribution with bend stiffness`` () =
        let size = 4   // atom locations have |x| <= size and |y| <= size
        let kT = 4.0
        let parms =
            TestParameters.create kT
                |> TestParameters.withBend Strong (Rational.create 0 1) 4.0
        let strong = parms.BondTypeParameterMap[Strong]
        let bondWeight =
            exp (-Chain.bondEnergy parms Strong AtomType.W AtomType.W / kT)
        let lengthSq (d : Point) = d.X * d.X + d.Y * d.Y

            // the state of the given atom locations and set of bonds
        let getState (locs : Point[]) (bonds : (int * int) list) =
            match bonds with
                | [] -> 0, 0
                | [ a, b ] -> 1, lengthSq (locs[b] - locs[a])
                | _ ->
                    let ends =
                        bonds
                            |> List.collect (fun (a, b) -> [ a; b ])
                            |> List.countBy id
                            |> List.filter (fun (_, n) -> n = 1)
                            |> List.map fst
                    match ends with
                        | [ a; b ] -> 2, lengthSq (locs[b] - locs[a])
                        | _ -> failwith "Not a strand"

            // exact Boltzmann weight of each state
        let exact = Collections.Generic.Dictionary<int * int, float>()
        let grid =
            [|
                for x in -size .. size do
                    for y in -size .. size do
                        Point.create (x, y)
            |]
        let pairs = [ 0, 1; 0, 2; 1, 2 ]
        for a in grid do
            for b in grid do
                for c in grid do
                    let locs = [| a; b; c |]
                    if not (overlaps a b || overlaps a c || overlaps b c) then
                        let inRange =
                            pairs
                                |> List.filter (fun (i, j) ->
                                    Array.contains (locs[j] - locs[i]) Chain.bondVectors)
                        let subsets =
                            List.foldBack (fun pair subsets ->
                                subsets @ List.map (fun s -> pair :: s) subsets)
                                inRange [ [] ]
                        for bonds in subsets do
                                // weight of the bends where two bonds meet
                            let bendWeight =
                                [ 0; 1; 2 ]
                                    |> List.map (fun m ->
                                        let partners =
                                            bonds |> List.choose (fun (i, j) ->
                                                if i = m then Some j
                                                elif j = m then Some i
                                                else None)
                                        match partners with
                                            | [ p; q ] ->
                                                if Bonds.isBendAllowed strong.BendLimit locs[m] locs[p] locs[q] then
                                                    exp (-Bonds.bendEnergy strong.BendStiffness locs[m] locs[p] locs[q] / kT)
                                                else 0.0
                                            | _ -> 1.0)
                                    |> List.fold (*) 1.0
                            if bendWeight > 0.0 then
                                addWeight exact (getState locs bonds)
                                    (bondWeight ** float bonds.Length * bendWeight)

            // sample states
        let inBox (loc : Point) = abs loc.X <= size && abs loc.Y <= size
        let samples =
            TestWorld.empty
                |> World.addAtomW (Point.create (-size, -size))
                |> World.addAtomW (Point.create (size, size))
                |> World.addAtomW (Point.create (-size, size))
                |> Chain.addBox size
                |> sample
                    parms 8 2000 500 3000
                    (fun world ->
                        let centered (loc : Point) = Grid.getDisplacement Point.zero loc world.Cells
                        let locs = world.Atoms |> Array.map (fun atom -> centered atom.Location)
                        Assert.True(Array.forall inBox locs, $"Atom escaped the box: {locs}")
                        getState locs (Bonds.strongBonds world |> Set.toList))

        checkFit "Bonds and bend" (toMap exact) samples

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

                                // the center of mass is followed continuously: its
                                // step between frames is far less than the size of
                                // the torus, so it is measured the shorter way around
                            let wrap d size = d - float size * round (d / float size)
                            let traj = Array.zeroCreate (nFrames + 1)
                            let mutable com = Chain.centerOfMass world
                            traj[0] <- com
                            for iFrame in 1 .. nFrames do
                                world <- Chain.sweepsWith parms rng frameInterval world
                                let com' = Chain.centerOfMass world
                                let x, y = traj[iFrame - 1]
                                traj[iFrame] <-
                                    let size = world.Size
                                    x + wrap (fst com' - fst com) size.X,
                                    y + wrap (snd com' - snd com) size.Y
                                com <- com'
                            traj)

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

/// Atoms in a box: bonds of both types, entropy, the start barrier,
/// pair energies, and stacking.
module BoxPhysicsTests =

    open PhysicsHelpers

    /// The bond between two atoms, if any.
    type private Bond = NoBond | WeakBond | StrongBond

    /// Walls, free atoms, and both bond types: two complementary atoms
    /// of the given types are confined to a small box of walls, at a
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
    /// A bond with entropy counts as exp(-entropy) states, so its
    /// weight is multiplied by that.
    ///
    /// The barrier to strong bonds affects only how fast they form and
    /// break, not this distribution, so this also checks that it's
    /// applied equally in both directions.
    let private checkTwoAtomsInBox (typeA, typeB) (parms : Parameters) =
        let size = 2   // atom locations have |x| <= size and |y| <= size
        let kT = parms.GetTemperature Point.zero   // uniform
        let weight bondType =
            exp (-Chain.bondEnergy parms bondType typeA typeB / kT - Chain.bondEntropy parms bondType)

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
                            (d, WeakBond), float count * weight Weak
                        if Array.contains d Chain.bondVectors then
                            (d, StrongBond), float count * weight Strong
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
            TestWorld.empty
                |> World.addAtom typeA (Point.create (-size, -size))
                |> World.addAtom typeB (Point.create (size, size))
                |> Chain.addBox size
                |> sample
                    parms 8 2000 1000 3000
                    (fun world ->
                            // positions relative to the origin, the short way around
                        let centered (loc : Point) = Grid.getDisplacement Point.zero loc world.Cells
                        let a = centered world.Atoms[0].Location
                        let b = centered world.Atoms[1].Location
                        Assert.True(inBox a && inBox b, $"Atom escaped the box: {a}, {b}")
                        let bond =
                            if Bonds.areStronglyBonded world 0 1 then StrongBond
                            elif Bonds.areWeaklyBonded world 0 1 then WeakBond
                            else NoBond
                        b - a, bond)

        checkFit "Separation and bond" exact samples

    /// Two atoms in a box, with the standard parameters: bonds have
    /// energy but no entropy.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution`` () =
        checkTwoAtomsInBox (AtomType.W, AtomType.X) (TestParameters.create 12.0)

    /// Two atoms in a box, with bond entropy. At this temperature a
    /// bond's entropy outweighs its energy, so a pair in range is
    /// usually unbonded. Without entropy it would be bonded at least
    /// half the time, however hot.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution with bond entropy`` () =
        TestParameters.create 12.0
            |> TestParameters.withEntropy 1.5 2.0
            |> checkTwoAtomsInBox (AtomType.W, AtomType.X)

    /// Two atoms in a box, with a start barrier on both bond types.
    /// Every bond here starts a strand, so every bond has the barrier.
    /// It slows forming and breaking equally, so the distribution must
    /// be exactly as it is without one.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution with a start barrier`` () =
        TestParameters.create 12.0
            |> TestParameters.withStartBarrier 6.0 6.0
            |> checkTwoAtomsInBox (AtomType.W, AtomType.X)

    /// Parameters with energies for particular pairs: Y-Z weak bonds
    /// much deeper than the default, and strong bonds between Y and Z,
    /// and between W and X, at their own depths.
    let private pairEnergies =
        TestParameters.create 12.0
            |> TestParameters.withPairEnergies Weak [ (AtomType.Y, AtomType.Z), 12.0 ]
            |> TestParameters.withPairEnergies Strong
                [
                    (AtomType.Y, AtomType.Z), 20.0
                    (AtomType.W, AtomType.X), 4.0
                ]

    /// Two atoms in a box, Y and Z, whose bonds have energies of their
    /// own: both bond types must use them.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution with pair energies`` () =
        checkTwoAtomsInBox (AtomType.Z, AtomType.Y) pairEnergies

    /// Two atoms in a box, W and X, with the same parameters: their
    /// strong bond has an energy of its own, but their weak bond
    /// doesn't, so it must use the default.
    [<Fact>]
    let ``Two atoms in a box use the default energy for pairs without one`` () =
        checkTwoAtomsInBox (AtomType.W, AtomType.X) pairEnergies

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
            TestWorld.empty
                |> World.addAtom AtomType.W (Point.create(-2, -2))   // 0
                |> World.addAtom AtomType.W (Point.create(0, -2))    // 1
                |> World.addAtom AtomType.X (Point.create(0, 2))     // 2
                |> World.addAtom AtomType.X (Point.create(2, 2))     // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> World.addStrongBond TestParameters.standard 2 3
                |> Chain.addBox size
                |> sample
                    (TestParameters.createFrozen kT) 8 2000 300 3000
                    (fun world ->
                        Set.toList (Bonds.weakBonds world),
                        World.getDisplacement
                            world.Atoms[0] world.Atoms[2] world)

        checkFit "Weak bonds" (toMap bondSetWeights) (Array.map fst samples)
        checkFit "Separation" (toMap separationWeights) (Array.map snd samples)

/// Weak bends: an atom with two weak partners, within a bend limit and
/// at a cost for bending.
module WeakBendPhysicsTests =

    open PhysicsHelpers

    /// Weak bends: a dimer (X0-X1, strongly bonded) and two free atoms
    /// (W2 and W3) are confined to a small box. Each W can pair with
    /// either X, but with only one, since a second weak bond needs a
    /// strong one. An X can pair with both Ws, if they are on opposite
    /// sides of it, at a cost for the bend between its two weak bonds,
    /// and only if the bend is within the weak bend limit.
    ///
    /// We enumerate every state exactly: each placement of the dimer
    /// and the two free atoms in the box, without overlaps, with each
    /// set of weak bonds that are in range and can exist together. Two
    /// weak bonds that don't share an atom must not cross. Nothing
    /// stacks, since the Ws have no strong bonds. A state's energy is
    /// weakBondEnergy per weak bond plus the bend energy at an X with
    /// two weak partners.
    ///
    /// States are grouped by their set of weak bonds and, if an X has
    /// two weak partners, the squared distance between them, which
    /// measures the bend.
    let private checkWeakBend bendLimit kappa =
        let size = 4   // atom locations have |x| <= size and |y| <= size
        let kT = 1.0
        let parms =
            TestParameters.createFrozen kT
                |> TestParameters.withBend Weak bendLimit kappa
                |> TestParameters.withWeak (fun p -> { p with Barrier = 3.0 })   // weak bonds toggle rarely, so that hops do most of the bending
        let weak = parms.BondTypeParameterMap[Weak]
        let candidates = [ 0, 2; 0, 3; 1, 2; 1, 3 ]   // (X, W)
        let lengthSq (d : Point) = d.X * d.X + d.Y * d.Y

            // the state of the given locations and weak bonds
        let getState (locs : Point[]) (bonds : list<int * int>) =
            let bend =
                [ 0; 1 ]
                    |> List.tryPick (fun x ->
                        match bonds |> List.filter (fun (i, _) -> i = x) with
                            | [ (_, a); (_, b) ] -> Some (lengthSq (locs[b] - locs[a]))
                            | _ -> None)
            List.sort bonds, Option.defaultValue -1 bend

            // every placement of the dimer in the box
        let inBox (loc : Point) = abs loc.X <= size && abs loc.Y <= size
        let grid =
            [|
                for x in -size .. size do
                    for y in -size .. size do
                        Point.create (x, y)
            |]
        let dimers =
            [|
                for a in grid do
                    for v in Chain.bondVectors do
                        if inBox (a + v) then a, a + v
            |]

            // exact Boltzmann weight of each state
        let exact = Collections.Generic.Dictionary<list<int * int> * int, float>()
        for loc0, loc1 in dimers do
            for loc2 in grid do
                if not (overlaps loc0 loc2 || overlaps loc1 loc2) then
                    for loc3 in grid do
                        if not (overlaps loc0 loc3 || overlaps loc1 loc3 || overlaps loc2 loc3) then
                            let locs = [| loc0; loc1; loc2; loc3 |]
                            let crosses (a, b) (c, d) =
                                Geometry.segmentsIntersect locs[a] locs[b] locs[c] locs[d]

                                // weak bonds that are in range
                            let feasible =
                                candidates
                                    |> List.filter (fun (i, j) ->
                                        Array.contains (locs[j] - locs[i]) Chain.weakBondVectors)

                                // can the two given weak bonds exist together? A W
                                // has only one; an X's two partners must be on
                                // opposite sides of it, within the bend limit
                            let compatible (x1, w1) (x2, w2) =
                                if w1 = w2 then false
                                elif x1 = x2 then
                                    Bonds.areOpposite locs[x1] locs[w1] locs[w2]
                                        && Bonds.isBendAllowed weak.BendLimit locs[x1] locs[w1] locs[w2]
                                else not (crosses (x1, w1) (x2, w2))

                            let bondSets =
                                (feasible, [ [] ])
                                    ||> List.foldBack (fun bond sets ->
                                        sets @ (sets |> List.map (fun set -> bond :: set)))
                                    |> List.where (fun bonds ->
                                        List.allPairs bonds bonds
                                            |> List.forall (fun (bondA, bondB) ->
                                                bondA >= bondB || compatible bondA bondB))

                            for bonds in bondSets do
                                let bendEnergy =
                                    [ 0; 1 ]
                                        |> List.sumBy (fun x ->
                                            match bonds |> List.filter (fun (i, _) -> i = x) with
                                                | [ (_, a); (_, b) ] ->
                                                    Bonds.bendEnergy weak.BendStiffness locs[x] locs[a] locs[b]
                                                | _ -> 0.0)
                                let energy =
                                    Chain.weakBondEnergy * float bonds.Length + bendEnergy
                                addWeight exact (getState locs bonds) (exp (-energy / kT))

            // make sure bent states are actually part of the test
        Assert.True(
            exact.Keys |> Seq.exists (fun (_, bend) -> bend >= 0),
            "No bent states enumerated")

            // sample states, starting with the atoms apart
        let samples =
            TestWorld.empty
                |> World.addAtom AtomType.X (Point.create (-size, -size))       // 0
                |> World.addAtom AtomType.X (Point.create (2 - size, -size))    // 1
                |> World.addAtom AtomType.W (Point.create (size, size))         // 2
                |> World.addAtom AtomType.W (Point.create (-size, size))        // 3
                |> World.addStrongBond TestParameters.standard 0 1
                |> Chain.addBox size
                |> sample
                    parms 8 5000 1000 3000
                    (fun world ->
                        let centered (loc : Point) = Grid.getDisplacement Point.zero loc world.Cells
                        let locs = world.Atoms |> Array.map (fun atom -> centered atom.Location)
                        let bonds =
                            Bonds.weakBonds world
                                |> Set.toList
                                |> List.map (fun (i, j) -> min i j, max i j)   // (X, W): the Xs have the smaller IDs
                        getState locs bonds)

        checkFit "Weak bonds and bend" (toMap exact) samples

    /// Weak bends with a limit stricter than the weak bond's own rule:
    /// an X's two partners must be nearly in line.
    [<Fact>]
    let ``A dimer and two free atoms in a box follow the Boltzmann distribution with a weak bend limit`` () =
        checkWeakBend TestParameters.straightBend 0.0

    /// Weak bends with a cost. The weak bond's own rule keeps every bend
    /// gentle (cos phi <= -0.6), so a large stiffness is needed to make
    /// the differences between them count.
    [<Fact>]
    let ``A dimer and two free atoms in a box follow the Boltzmann distribution with weak bend stiffness`` () =
        checkWeakBend TestParameters.anyBend 8.0
