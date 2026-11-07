namespace Ecos.Experiment

open System
open Ecos

/// A proposed standard physics, under trial: weak pairs weak enough
/// (1.4) that lone pairs rarely start a copy, an H-H pair strong
/// enough (2.5) to serve as a start site, and faster strong-bond
/// toggles (event weights 0.5/0.2/0.3) to keep copying apace. Under
/// it, copying starts at an H-H pair: scenario C's template begins
/// with one, and H atoms are added to the food so that start sites
/// can also arise by chance.
module ProposedPhysics =

    /// Number of free H atoms added to the food (as many as of each
    /// other type in the standard scenarios).
    let numFreeH = 120

    /// Applies the proposed physics to the given scenario.
    let adapt (scenario : Scenario) =
        scenario
            |> Scenario.mapBondTypeParameters Weak (fun parms ->
                { parms with Energy = 1.4 })
            |> Scenario.withPairEnergy Weak AtomType.H AtomType.H 2.5
            |> Scenario.withEventWeights 0.5 0.2 0.3

    /// Adds the given number of free H atoms at random empty places
    /// (with even coordinates) anywhere in the given world.
    let addFreeH (rng : Random) n (world : World) =
        (world, [ 1 .. n ])
            ||> Seq.fold (fun world _ ->
                let rec loop () =
                    let loc =
                        Point.create (
                            2 * rng.Next(world.Size.X / 2),
                            2 * rng.Next(world.Size.Y / 2))
                    match World.tryAddAtom AtomType.H loc world with
                        | Some (world, _) -> world
                        | None -> loop ()
                loop ())

    /// Scenario C under the proposed physics, with the given cold
    /// temperature. Its template, if any, is P H H, then eight random
    /// W or Y, then Q, so that it starts with a start site.
    let createScenarioC withTemplate coldTemperature =
        let scenario = Scenario.createScenarioC withTemplate |> adapt
        let build parms (rng : Random) =
            let world = World.createSquare 160
            let world, template =
                if withTemplate then
                    let atomTypes =
                        [|
                            AtomType.P; AtomType.H; AtomType.H
                            for _ in 1 .. 8 do
                                if rng.Next(2) = 0 then AtomType.W else AtomType.Y
                            AtomType.Q
                        |]
                    let world =
                        Build.tryAddStrand parms (-2 - 10) 0 atomTypes world
                            |> Option.get
                    world, [| 0 .. atomTypes.Length - 1 |]
                else world, Array.empty
            let world =
                world
                    |> Build.addFreeAtomsAnywhere rng 120
                    |> addFreeH rng numFreeH
            world, template
        {
            scenario with
                Name = scenario.Name + ", proposed physics"
                Build = build
                Schedule =
                    Cycle [|
                        { Name = Some "cold"; Temperature = coldTemperature; NumSweeps = 200_000; HydrolysisRate = None; CapPeelBarrier = None }
                        { Name = Some "hot"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None; CapPeelBarrier = None }
                    |]
        }

    /// The living world of experiment 5 under the proposed physics,
    /// with free H atoms added to its food.
    let createLivingWorld strongBarrier =
        let scenario =
            Experiment4.getScenario Experiment4.livingWorldSettings strongBarrier
                |> Scenario.withBuildStep (fun _ rng world ->
                    addFreeH rng numFreeH world)
                |> adapt
        { scenario with Name = scenario.Name + ", proposed physics" }
