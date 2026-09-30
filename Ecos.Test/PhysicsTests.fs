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

    /// Oracle: contact energy between atoms of the given types. This
    /// is an independent statement of World's interaction table:
    /// hydrophobic atoms attract each other, and nothing else
    /// interacts.
    let private contactEnergy typeA typeB =
        match typeA, typeB with
            | Hydrophobic, Hydrophobic -> -1.0
            | _ -> 0.0

    /// Equilibrium: update's moves are symmetric, and are accepted
    /// only when valid and then with the Metropolis probability, so in
    /// the long run each conformation's probability is proportional to
    /// its Boltzmann factor exp(-E/kT).
    ///
    /// For a three-atom chain we can enumerate all conformations
    /// exactly. The only possible contact is between the two end atoms
    /// (the middle atom is bonded to both), and they are in contact
    /// when their separation is (2, 0), (2, 1), or (2, 2), i.e. when
    /// |e|^2 <= 8 for end-to-end vector e. We compare the exact
    /// distribution of e against simulations started far from
    /// equilibrium (a straight rod).
    let private checkTrimer (typeA, typeB, typeC) kT =

            // exact Boltzmann weight of each end-to-end vector
        let exact =
            [
                for b1 in Chain.bondVectors do
                    for b2 in Chain.bondVectors do
                        let e = b1 + b2
                        if abs e.X >= 2 || abs e.Y >= 2 then   // end atoms don't overlap
                            e
            ]
                |> List.countBy id
                |> List.map (fun (e, count) ->
                    let energy =
                        if e.X * e.X + e.Y * e.Y <= 8 then
                            contactEnergy typeA typeC
                        else 0.0
                    e, float count * exp (-energy / kT))
                |> Map.ofList
        let totalWeight = exact.Values |> Seq.sum

            // sample end-to-end vectors from independent simulations
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
                        |> World.addBond 0 1
                        |> World.addBond 1 2
                world <- Chain.sweeps kT rng burnIn world
                Array.init nSamples (fun _ ->
                    world <- Chain.sweeps kT rng sampleInterval world
                    world.AtomMap[2].Location - world.AtomMap[0].Location))
                |> Array.concat
        let observed = samples |> Array.countBy id |> Map.ofArray

            // every sample is a valid conformation
        for e in observed.Keys do
            Assert.True(exact.ContainsKey e, $"Invalid end-to-end vector {e}")

            // chi-squared goodness of fit against the Boltzmann distribution
        let chi2 =
            exact
                |> Map.toSeq
                |> Seq.sumBy (fun (e, weight) ->
                    let expected =
                        float samples.Length * weight / totalWeight
                    let obs =
                        observed
                            |> Map.tryFind e
                            |> Option.defaultValue 0
                            |> float
                    (obs - expected) ** 2.0 / expected)
        let df = float (exact.Count - 1)
        let limit = df + 5.0 * sqrt (2.0 * df)   // mean + 5 standard deviations
        Assert.True(chi2 < limit, $"chi^2 = {chi2:F1}, limit {limit:F1} (df = {df})")

    /// Athermal limit: with no interactions, every valid conformation
    /// is equally likely.
    [<Fact>]
    let ``Trimer samples all conformations uniformly`` () =
        checkTrimer (Polar, Polar, Polar) Chain.athermalKT

    /// Attraction: sticky ends spend more time in contact, by exactly
    /// the Boltzmann factor.
    [<Fact>]
    let ``Trimer with sticky ends follows the Boltzmann distribution`` () =
        checkTrimer (Hydrophobic, Polar, Hydrophobic) 0.5

    /// Consistency: a hydrophobic end and a polar end don't interact,
    /// so the ends must sample uniformly, even though a hydrophobic
    /// atom is moving. This fails if a contact's energy depends on
    /// which of the two atoms moves.
    [<Fact>]
    let ``Trimer with one sticky end samples uniformly`` () =
        checkTrimer (Hydrophobic, Hydrophobic, Polar) 0.5

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
