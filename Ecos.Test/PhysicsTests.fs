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
                                world <- Chain.sweeps rng sampleInterval world
                                Chain.radiusOfGyrationSquared world))
                    float n, Array.average (Array.concat rg2s)
            ]

            // <Rg^2> ~ N^(2 nu). Short chains show corrections to scaling
            // that push the estimate slightly above 3/4, hence the tolerance.
        let nu = Stats.logLogSlope points / 2.0
        Assert.InRange(nu, 0.70, 0.80)

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
                                    world <- Chain.sweeps rng frameInterval world
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
