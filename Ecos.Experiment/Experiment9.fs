namespace Ecos.Experiment

open System
open System.IO
open System.Threading

open Ecos
open Ecos.Experiment.Experiment3
open Ecos.Experiment.Experiment7

/// Experiment 9: a vent. W/X strands compete with Y/Z strands, as in
/// experiment 8, but the world is heated by a vent along its left wall
/// instead of uniformly, so each place has its own cycle: the nearer
/// the vent, the hotter its pulses. The weak Y-Z bond is stronger than
/// the W-X one, which helps Y/Z strands copy, but a Y/Z copy needs a
/// hotter pulse to separate from its template. Does each kind of strand
/// win where its pairing suits the local temperature: Y/Z near the
/// vent, W/X farther out?
///
/// A control run has equal pair energies, so that any pattern there
/// comes from the vent alone.
module Experiment9 =

    /// Settings for the experiment.
    type Settings =
        {
            /// Energies of the weak Y-Z bond to run with. W-X keeps the
            /// default weak bond energy.
            YZEnergies : float[]

            /// Number of strands of each kind to start with.
            NumSeeds : int

            /// Number of bands, side by side, that the box is divided
            /// into, from the vent outward.
            NumBands : int

            /// Number of independent runs (seeds) with each energy.
            NumRuns : int

            /// Number of times to run through the vent's cycle.
            NumCycles : int
        }

    /// Default settings.
    let defaultSettings =
        {
            YZEnergies = [| 2.0; 3.0 |]
            NumSeeds = 8
            NumBands = 8
            NumRuns = 12
            NumCycles = 30
        }

    /// Left and right edges of the region that the bands divide: the
    /// inside of the standard box.
    let private minX, maxX = -80, 80

    /// Index of the band containing the given x coordinate.
    let private getBand settings (x : float) =
        let width = float (maxX - minX) / float settings.NumBands
        int ((x - float minX) / width)
            |> max 0
            |> min (settings.NumBands - 1)

    /// Center of the given band.
    let private getBandCenter settings iBand =
        let width = float (maxX - minX) / float settings.NumBands
        float minX + (float iBand + 0.5) * width

    /// What one band of a world contains.
    type private BandResult =
        {
            /// Capped chains whose coding atoms are all W or X.
            WX : int

            /// Capped chains whose coding atoms are all Y or Z.
            YZ : int

            /// Free W and X atoms.
            FreeWX : int

            /// Free Y and Z atoms.
            FreeYZ : int
        }

    /// Counts capped chains of each kind, and free atoms, in each band
    /// of the given world. A chain belongs to the band containing the
    /// average x coordinate of its atoms.
    let private getCensus settings (world : World) =
        let strands = getStrands world
        let chains =
            strands
                |> Array.where (isCapped world)
                |> Array.map (fun strand ->
                    let x =
                        strand.AtomIds
                            |> Array.averageBy (fun id ->
                                float world.Atoms[id].Location.X)
                    let types =
                        strand.AtomIds
                            |> Array.map (fun id -> world.Atoms[id].Type)
                            |> Array.where (fun typ -> isWX typ || isYZ typ)
                    getBand settings x, types)
        let inStrands =
            strands
                |> Seq.collect _.AtomIds
                |> set
        let free =
            world.Atoms
                |> Array.where (fun atom -> not (inStrands.Contains atom.Id))
        [|
            for iBand in 0 .. settings.NumBands - 1 do
                let countChains test =
                    chains
                        |> Array.where (fun (band, types) ->
                            band = iBand && test types)
                        |> Array.length
                let countFree test =
                    free
                        |> Array.where (fun atom ->
                            getBand settings (float atom.Location.X) = iBand
                                && test atom.Type)
                        |> Array.length
                {
                    WX = countChains (Array.forall isWX)
                    YZ = countChains (Array.forall isYZ)
                    FreeWX = countFree isWX
                    FreeYZ = countFree isYZ
                }
        |]

    /// The experiment's scenarios: one for each Y-Z energy.
    let getScenarios settings =
        settings.YZEnergies
            |> Array.map (Scenario.createVentScenario settings.NumSeeds)

    /// Runs one world through the cycles. Answers a result for each
    /// band at the start and at the end of each cycle.
    let private runOne settings yzEnergy seed =
        let scenario = Scenario.createVentScenario settings.NumSeeds yzEnergy
        let period =
            Schedule.tryGetPeriod scenario.Schedule |> Option.get
        let mutable run = Run.create seed scenario
        [|
            getCensus settings run.World
            for iCycle in 1 .. settings.NumCycles do
                run <- Run.advance period run
                eprintfn "Y-Z energy %g, seed %d: finished cycle %d of %d" yzEnergy seed iCycle settings.NumCycles
                getCensus settings run.World
        |]

    /// Runs the experiment, prints tables of results, and writes them
    /// to the given CSV file.
    let run (settings : Settings) (csvPath : string) =

        let combos =
            [|
                for yzEnergy in settings.YZEnergies do
                    for seed in 0 .. settings.NumRuns - 1 do
                        yzEnergy, seed
            |]

            // run every combination in parallel
        let mutable nDone = 0
        let results =
            combos
                |> Array.Parallel.map (fun (yzEnergy, seed) ->
                    let cycles = runOne settings yzEnergy seed
                    let n = Interlocked.Increment(&nDone)
                    eprintfn "Finished run %d of %d" n combos.Length
                    yzEnergy, cycles)
                |> Array.groupBy fst
                |> Array.map (fun (yzEnergy, group) ->
                    yzEnergy, Array.map snd group)
                |> Map

        let scenario = Scenario.createVentScenario settings.NumSeeds 2.0
        let vent =
            match scenario.Schedule with
                | Vent vent -> vent
                | _ -> failwith "Not a vent"
        printfn "Experiment 9: W/X and Y/Z strands in a world heated by a vent"
        printfn
            "%d runs of %d cycles with each Y-Z energy; %d strands of each kind to start (P-W-Q and P-Y-Q)"
            settings.NumRuns settings.NumCycles settings.NumSeeds
        printfn
            "Vent at x = %d: kT %g for %d sweeps between pulses; during a %d-sweep pulse, kT %g at the vent, falling linearly to %g at %d away"
            vent.X vent.ColdTemperature vent.ColdSweeps vent.HotSweeps
            vent.HotTemperature vent.ColdTemperature vent.Width
        printParameters scenario.Parameters

            // the bands, with the temperature at their centers during a pulse
        let bands = [| 0 .. settings.NumBands - 1 |]
        let bandHeader =
            bands
                |> Array.map (fun iBand ->
                    sprintf "%7.0f" (getBandCenter settings iBand))
                |> String.concat ""
        let kTHeader =
            bands
                |> Array.map (fun iBand ->
                    let x = getBandCenter settings iBand |> round |> int
                    sprintf "%7.2f" (Vent.getHotTemperature vent (Point.create (x, 0))))
                |> String.concat ""

        use writer = new StreamWriter(csvPath)
        writer.WriteLine("yz_energy,cycle,band,band_center,pulse_kT,wx,yz,free_wx,free_yz")

        for yzEnergy in settings.YZEnergies do
            let runs = results[yzEnergy]
            let average iCycle iBand (f : BandResult -> int) =
                runs |> Array.averageBy (fun cycles -> float (f (cycles[iCycle][iBand])))

            printfn ""
            printfn "Weak Y-Z bond energy %g (W-X %g)" yzEnergy scenario.Parameters.BondParameterMap[Weak].Energy
            printfn "Y/Z share of capped chains, by band (pooled over runs; blank where there are none)"
            printfn "band center: %s" bandHeader
            printfn "pulse kT:    %s" kTHeader
            for iCycle in 0 .. settings.NumCycles do
                let shares =
                    bands
                        |> Array.map (fun iBand ->
                            let nWX = average iCycle iBand _.WX
                            let nYZ = average iCycle iBand _.YZ
                            if nWX + nYZ > 0.0 then
                                sprintf "%7.2f" (nYZ / (nWX + nYZ))
                            else String.replicate 7 " ")
                        |> String.concat ""
                printfn "cycle %5d: %s" iCycle shares

                for iBand in bands do
                    let center = getBandCenter settings iBand
                    let x = center |> round |> int
                    let values =
                        [
                            yzEnergy
                            float iCycle
                            float iBand
                            center
                            Vent.getHotTemperature vent (Point.create (x, 0))
                            average iCycle iBand _.WX
                            average iCycle iBand _.YZ
                            average iCycle iBand _.FreeWX
                            average iCycle iBand _.FreeYZ
                        ]
                    writer.WriteLine(
                        String.Join(
                            ",",
                            values
                                |> Seq.map (fun (x : float) ->
                                    x.ToString(Globalization.CultureInfo.InvariantCulture))))

            let final = settings.NumCycles
            let row label f =
                let cells =
                    bands
                        |> Array.map (fun iBand ->
                            sprintf "%7.1f" (average final iBand f))
                        |> String.concat ""
                printfn "%-12s %s" label cells
            printfn ""
            printfn "At the end (per run, by band)"
            printfn "band center: %s" bandHeader
            row "W/X chains:" _.WX
            row "Y/Z chains:" _.YZ
            row "free W+X:" _.FreeWX
            row "free Y+Z:" _.FreeYZ

        printfn ""
        printfn "Bands divide the box from the vent (left) outward. A chain belongs to the band holding"
        printfn "the average x coordinate of its atoms. W/X, Y/Z: capped chains whose coding atoms are"
        printfn "all of that kind. pulse kT: the temperature at the band's center during a pulse."
        printfn "Results written to %s" (Path.GetFullPath csvPath)
