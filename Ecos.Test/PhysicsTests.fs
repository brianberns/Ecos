namespace Ecos.Test

open System
open Xunit
open Ecos

/// Checks that a single chain reproduces known polymer physics.
///
/// Each measurement averages over many independent chains, each grown
/// in equilibrium (see Chain.grow) and then evolved by World.update.
/// Seeds are fixed, so results are reproducible.
module PhysicsTests =

    /// Chain lengths to measure.
    let private lengths = [ 8; 16; 32; 64 ]

    /// Independent chains per length.
    let private nChains = 32

    /// Chain size: a single chain with excluded volume is a
    /// self-avoiding walk, whose size scales as R ~ N^nu, where
    /// nu = 3/4 exactly in 2D. (A random walk that can cross itself
    /// would give 1/2, and a rigid rod 1.)
    ///
    /// Note that chains are grown already in equilibrium, and the
    /// sweeps below are far shorter than a long chain's relaxation
    /// time. So this mainly checks the excluded-volume and bond rules
    /// (via Chain.grow), not World.update. The trimer test below checks
    /// that World.update itself reaches equilibrium.
    [<Fact>]
    let ``Chain size scales with the 2D Flory exponent`` () =

        let nSamples, sampleInterval = 20, 50   // sweeps

            // mean squared radius of gyration for each chain length
        let points =
            [
                for n in lengths do
                    let rg2s =
                        Array.Parallel.init nChains (fun iChain ->
                            let rng = Random(1000 * n + iChain)
                            let mutable world = Chain.grow rng n
                            Array.init nSamples (fun _ ->
                                world <- Chain.sweeps Chain.athermalKT rng sampleInterval world
                                Chain.radiusOfGyrationSquared world))
                    float n, Array.average (Array.concat rg2s)
            ]

            // <Rg^2> ~ N^(2 nu). Short chains show corrections to scaling
            // that push the estimate slightly above 3/4, hence the tolerance.
        let nu = Stats.logLogSlope points / 2.0
        Assert.InRange(nu, 0.70, 0.80)

    /// Equilibrium: update's events (hops and weak bond toggles) are
    /// proposed symmetrically, and are accepted only when valid and
    /// then with the Metropolis probability, so in the long run each
    /// state's probability is proportional to its Boltzmann factor
    /// exp(-E/kT).
    ///
    /// For a three-atom chain we can enumerate all states exactly. A
    /// state is a conformation, identified by end-to-end vector e,
    /// plus whether the ends are weakly bonded. The middle atom is
    /// strongly bonded to both ends, so it can never pair with either
    /// of them. The ends can pair only if their types are complementary
    /// and e is an allowed weak bond vector, in which case that
    /// conformation has two states: unpaired (E = 0) and paired
    /// (E = weakBondEnergy). We compare the exact distribution against
    /// simulations started far from equilibrium (a straight rod).
    let private checkTrimer (typeA, typeB, typeC) kT =

            // exact Boltzmann weight of each state
        let exact =
            [
                for b1 in Chain.bondVectors do
                    for b2 in Chain.bondVectors do
                        let e = b1 + b2
                        if abs e.X >= 2 || abs e.Y >= 2 then   // end atoms don't overlap
                            e
            ]
                |> List.countBy id
                |> List.collect (fun (e, count) ->
                    [
                        (e, false), float count
                        if Chain.canPair typeA typeC
                            && Array.contains e Chain.weakBondVectors then
                            (e, true), float count * exp (-Chain.weakBondEnergy / kT)
                    ])
                |> Map.ofList
        let totalWeight = exact.Values |> Seq.sum

            // sample states from independent simulations
        let nRuns, nSamples = 8, 5000
        let burnIn, sampleInterval = 200, 50   // sweeps; the interval keeps samples roughly uncorrelated
        let samples =
            Array.Parallel.init nRuns (fun iRun ->
                let rng = Random(iRun)
                let atom typ (x, y) =
                    { Type = typ; Location = Point.create (x, y) }
                let mutable world =
                    World.empty
                        |> World.addAtom (atom typeA (0, 0))
                        |> World.addAtom (atom typeB (3, 0))
                        |> World.addAtom (atom typeC (6, 0))
                        |> World.addStrongBond 0 1
                        |> World.addStrongBond 1 2
                world <- Chain.sweeps kT rng burnIn world
                Array.init nSamples (fun _ ->
                    world <- Chain.sweeps kT rng sampleInterval world
                    let e = world.AtomMap[2].Location - world.AtomMap[0].Location
                    let paired = world.WeakBondMap.TryFind 0 = Some 2
                    e, paired))
                |> Array.concat
        let observed = samples |> Array.countBy id |> Map.ofArray

            // every sample is a valid state
        for state in observed.Keys do
            Assert.True(exact.ContainsKey state, $"Invalid state {state}")

            // chi-squared goodness of fit against the Boltzmann distribution
        let chi2 =
            exact
                |> Map.toSeq
                |> Seq.sumBy (fun (state, weight) ->
                    let expected =
                        float samples.Length * weight / totalWeight
                    let obs =
                        observed
                            |> Map.tryFind state
                            |> Option.defaultValue 0
                            |> float
                    (obs - expected) ** 2.0 / expected)
        let df = float (exact.Count - 1)
        let limit = df + 5.0 * sqrt (2.0 * df)   // mean + 5 standard deviations
        Assert.True(chi2 < limit, $"chi^2 = {chi2:F1}, limit {limit:F1} (df = {df})")

    /// Athermal limit: with no possible weak bonds, every valid
    /// conformation is equally likely.
    [<Fact>]
    let ``Trimer samples all conformations uniformly`` () =
        checkTrimer (AtomType.A, AtomType.A, AtomType.A) Chain.athermalKT

    /// Pairing: complementary ends spend time weakly bonded, with
    /// paired and unpaired states weighted by exactly the Boltzmann
    /// factor.
    [<Fact>]
    let ``Trimer with complementary ends follows the Boltzmann distribution`` () =
        checkTrimer (AtomType.A, AtomType.C, AtomType.B) 0.5

    /// Selectivity: the ends aren't complementary, and the middle atom
    /// is complementary to one end but strongly bonded to it, so no
    /// weak bond can ever form and every conformation is equally
    /// likely.
    [<Fact>]
    let ``Trimer with no allowed pairs samples uniformly`` () =
        checkTrimer (AtomType.A, AtomType.B, AtomType.C) 0.5

    /// Walls and free atoms: two complementary atoms (types A and B),
    /// not strongly bonded, are confined to a small box of walls. We
    /// can enumerate every state exactly: each placement of the two
    /// atoms in the box without overlap, unpaired (E = 0) or, when
    /// they're in weak bond range, paired (E = weakBondEnergy). States
    /// are grouped by the atoms' separation. The simulation must match
    /// the Boltzmann distribution, and must never produce a state that
    /// isn't in the box.
    [<Fact>]
    let ``Two atoms in a box follow the Boltzmann distribution`` () =
        let size = 2   // atom locations have |x| <= size and |y| <= size
        let kT = 0.7

            // exact Boltzmann weight of each (separation, paired) state
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
                        if abs d.X >= 2 || abs d.Y >= 2 then   // atoms don't overlap
                            d
            ]
                |> List.countBy id
                |> List.collect (fun (d, count) ->
                    [
                        (d, false), float count
                        if Array.contains d Chain.weakBondVectors then
                            (d, true), float count * exp (-Chain.weakBondEnergy / kT)
                    ])
                |> Map.ofList
        let totalWeight = exact.Values |> Seq.sum

            // sample states from independent simulations
        let nRuns, nSamples = 8, 4000
        let burnIn, sampleInterval = 500, 100   // sweeps; the interval keeps samples roughly uncorrelated
        let inBox (loc : Point) = abs loc.X <= size && abs loc.Y <= size
        let samples =
            Array.Parallel.init nRuns (fun iRun ->
                let rng = Random(iRun)
                let mutable world =
                    World.empty
                        |> World.addAtom { Type = AtomType.A; Location = Point.create (-size, -size) }
                        |> World.addAtom { Type = AtomType.B; Location = Point.create (size, size) }
                        |> Chain.addBox size
                world <- Chain.sweeps kT rng burnIn world
                Array.init nSamples (fun _ ->
                    world <- Chain.sweeps kT rng sampleInterval world
                    let a = world.AtomMap[0].Location
                    let b = world.AtomMap[1].Location
                    Assert.True(inBox a && inBox b, $"Atom escaped the box: {a}, {b}")
                    b - a, world.WeakBondMap.ContainsKey 0))
                |> Array.concat
        let observed = samples |> Array.countBy id |> Map.ofArray

            // every sample is a valid state
        for state in observed.Keys do
            Assert.True(exact.ContainsKey state, $"Invalid state {state}")

            // chi-squared goodness of fit against the Boltzmann distribution
        let chi2 =
            exact
                |> Map.toSeq
                |> Seq.sumBy (fun (state, weight) ->
                    let expected =
                        float samples.Length * weight / totalWeight
                    let obs =
                        observed
                            |> Map.tryFind state
                            |> Option.defaultValue 0
                            |> float
                    (obs - expected) ** 2.0 / expected)
        let df = float (exact.Count - 1)
        let limit = df + 5.0 * sqrt (2.0 * df)   // mean + 5 standard deviations
        Assert.True(chi2 < limit, $"chi^2 = {chi2:F1}, limit {limit:F1} (df = {df})")

    /// Centre-of-mass mean squared displacement (MSD) at various lags,
    /// for each chain length. Shared by the diffusion tests.
    let private diffusionData =
        lazy (
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
                                    world <- Chain.sweeps Chain.athermalKT rng frameInterval world
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
    [<Fact>]
    let ``Chain's centre of mass diffuses`` () =
        for n, msds in diffusionData.Value do
            let exponent = Stats.logLogSlope msds
            Assert.True(
                exponent > 0.9 && exponent < 1.1,
                $"N = {n}: MSD ~ t^{exponent:F3}, expected t^1")

    /// Rouse dynamics: with only local moves (and no solvent), each
    /// move shifts the centre of mass by 1/N of a step, so the diffusion
    /// constant scales as D ~ 1/N.
    [<Fact>]
    let ``Diffusion constant scales inversely with chain length`` () =
        let points =
            [
                for n, msds in diffusionData.Value do
                    let d = Stats.slope ((0.0, 0.0) :: msds) / 4.0
                    float n, d
            ]
        let exponent = Stats.logLogSlope points
        Assert.InRange(exponent, -1.15, -0.85)
