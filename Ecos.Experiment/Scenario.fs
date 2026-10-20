namespace Ecos.Experiment

open System
open Ecos

/// A stretch of time at one temperature.
type Phase =
    {
        /// Temperature scaled by Boltzmann's constant (kT).
        Temperature : float

        /// Number of sweeps to run at this temperature.
        NumSweeps : int
    }

/// A vent: a vertical line of hot rock that pulses. Between pulses,
/// the temperature is cold everywhere. During a pulse, it is hottest
/// at the vent and falls linearly with distance from it, back to cold
/// at the given width.
type Vent =
    {
        /// X coordinate of the vent.
        X : int

        /// Temperature (kT) at the vent during a pulse.
        HotTemperature : float

        /// Temperature (kT) everywhere between pulses, and beyond the
        /// vent's reach during one.
        ColdTemperature : float

        /// Distance from the vent at which a pulse no longer warms
        /// anything.
        Width : int

        /// Number of sweeps between pulses.
        ColdSweeps : int

        /// Number of sweeps that a pulse lasts.
        HotSweeps : int
    }

module Vent =

    /// Temperature at the given location during a pulse of the given
    /// vent.
    let getHotTemperature vent (pt : Point) =
        let dist = float (abs (pt.X - vent.X))
        let fraction = max 0.0 (1.0 - dist / float vent.Width)
        vent.ColdTemperature
            + fraction * (vent.HotTemperature - vent.ColdTemperature)

/// Temperature over time. The temperature at any moment depends only
/// on the number of sweeps run so far, so a schedule holds no state.
type Schedule =

    /// Fixed temperature scaled by Boltzmann's constant (kT).
    | Constant of float

    /// Phases that repeat in a loop.
    | Cycle of Phase[]

    /// A vent that pulses in a loop, starting between pulses.
    | Vent of Vent

module Schedule =

    /// Answers the temperature after the given number of sweeps, and
    /// the number of sweeps until it next changes, if it ever does.
    let getTemperature numSweeps schedule =
        assert(numSweeps >= 0)
        match schedule with
            | Constant kT ->
                Parameters.getUniformTemperature kT, None
            | Cycle phases ->
                assert(phases.Length > 0)
                assert(Array.forall (fun phase ->
                    phase.NumSweeps > 0) phases)

                let rec loop t iPhase =
                    let phase = phases[iPhase]
                    if t < phase.NumSweeps then
                        phase.Temperature, Some (phase.NumSweeps - t)
                    else loop (t - phase.NumSweeps) (iPhase + 1)

                let period = Array.sumBy _.NumSweeps phases
                let kT, nSweepsOpt = loop (numSweeps % period) 0
                Parameters.getUniformTemperature kT, nSweepsOpt
            | Vent vent ->
                assert(vent.ColdSweeps > 0 && vent.HotSweeps > 0)
                assert(vent.Width > 0)
                let t = numSweeps % (vent.ColdSweeps + vent.HotSweeps)
                if t < vent.ColdSweeps then
                    Parameters.getUniformTemperature vent.ColdTemperature,
                    Some (vent.ColdSweeps - t)
                else
                    Vent.getHotTemperature vent,
                    Some (vent.ColdSweeps + vent.HotSweeps - t)

    /// Number of sweeps after which the given schedule repeats, if it
    /// changes at all.
    let tryGetPeriod = function
        | Constant _ -> None
        | Cycle phases -> Some (Array.sumBy _.NumSweeps phases)
        | Vent vent -> Some (vent.ColdSweeps + vent.HotSweeps)

    /// The phases of the given schedule, if it cycles uniformly.
    let getPhases = function
        | Cycle phases -> phases
        | Constant _ | Vent _ -> failwith "Schedule has no uniform phases"

/// A description of a world to run: how to build it, its parameters,
/// and its temperature over time. It holds no state, so the same
/// scenario can be run any number of times, in parallel. (It holds a
/// function, so two scenarios are equal only if they are the same one.)
[<ReferenceEquality>]
type Scenario =
    {
        /// Name of the scenario.
        Name : string

        /// Builds the initial world, using the given random number
        /// generator. Answers the world and the IDs of the atoms of
        /// its template strand, if it has one.
        Build : Random -> World * AtomId[]

        /// Parameters, apart from temperature, which comes from the
        /// schedule. (Their temperature is NaN.)
        Parameters : Parameters

        /// Temperature over time.
        Schedule : Schedule
    }

/// Building blocks for a scenario's world.
module Build =

    let private addVerticalWall x yStart yEnd world =
        assert(x % 2 = 0)
        assert(yStart % 2 = 0)
        assert(yEnd % 2 = 0)
        (world, [ yStart .. 2 .. yEnd ])
            ||> Seq.fold (fun world y ->
                World.addWall (Point.create (x, y)) world)

    let private addHorizontalWall xStart xEnd y world =
        assert(xStart % 2 = 0)
        assert(xEnd % 2 = 0)
        assert(y % 2 = 0)
        (world, [ xStart .. 2 .. xEnd ])
            ||> Seq.fold (fun world x ->
                World.addWall (Point.create (x, y)) world)

    /// Adds a box of walls with the given outer size, centered on the
    /// origin.
    let addBox width height world =
        let height' = height - 2 * 2
        world
            |> addHorizontalWall -(width/2) (width/2) -(height/2)
            |> addHorizontalWall -(width/2) (width/2) (height/2)
            |> addVerticalWall -(width/2) -(height'/2) (height'/2)
            |> addVerticalWall (width/2) -(height'/2) (height'/2)

    /// Adds an atom of the given type at the given location, which
    /// must be empty, optionally joined to the atom added just before
    /// it.
    let private addAtom atomType x y joinPrevious world =
        assert(x % 2 = 0)
        assert(y % 2 = 0)
        let world, atom =
            let location = Point.create (x, y)
            match World.tryAddAtom atomType location world with
                | Some pair -> pair
                | None -> failwith "Location is not empty"
        if joinPrevious then
            World.addStrongBond (atom.Id - 1) atom.Id world
        else world

    /// Adds a horizontal template strand of the given number of
    /// coding atoms, each a random W or Y, with a P cap at its start
    /// and a Q cap at its end. The world must have no atoms yet.
    /// Answers the world and the template's atom IDs.
    let addTemplate (rng : Random) nCoding (world : World) =
        assert(world.Atoms.Length = 0)

            // start cap
        let world =
            addAtom AtomType.P (-2 - nCoding) 0 false world

            // coding atoms
        let codingTypes = [| AtomType.W; AtomType.Y |]
        let world =
            (world, [ 0 .. nCoding - 1 ])
                ||> Seq.fold (fun world i ->
                    let atomType =
                        codingTypes[rng.Next(codingTypes.Length)]
                    addAtom atomType (2 * i - nCoding) 0 true world)

            // end cap
        let world =
            addAtom AtomType.Q nCoding 0 true world

        world, Array.map _.Id world.Atoms

    /// Adds rows of free atoms, each row the given number of atoms
    /// long, the given distance apart, in strips by type above and
    /// below the origin: weak partners of the template nearest to it,
    /// then strong partners, then caps.
    let addFreeAtoms nPerRow dist world =
        let rows =
            [
                AtomType.P,  5; AtomType.P,  6
                AtomType.Y,  3; AtomType.Y,  4
                AtomType.X,  1; AtomType.X,  2
                AtomType.Z, -1; AtomType.Z, -2
                AtomType.W, -3; AtomType.W, -4
                AtomType.Q, -5; AtomType.Q, -6
            ]
        (world, rows)
            ||> Seq.fold (fun world (atomType, iRow) ->
                (world, [ 0 .. nPerRow - 1 ])
                    ||> Seq.fold (fun world i ->
                        addAtom atomType (2 * i - nPerRow)
                            (iRow * dist) false world))

    /// Adds a horizontal strand of the given atom types, joined in
    /// order, at a random empty place where every one of its atoms is
    /// within the given distance of the origin along each axis.
    let addStrandAtRandom (rng : Random) maxDist (atomTypes : _[]) world =
        assert(atomTypes.Length >= 2)

        let minX = -maxDist / 2
        let minY = -maxDist / 2
        let maxX = (maxDist - 2 * atomTypes.Length) / 2 + 1
        let maxY = maxDist / 2 + 1

        let rec loop () =
            let x = 2 * rng.Next(minX, maxX)
            let y = 2 * rng.Next(minY, maxY)
            (Some world, Array.indexed atomTypes)
                ||> Array.fold (fun worldOpt (i, atomType) ->
                    worldOpt
                        |> Option.bind (fun world ->
                            let loc = Point.create (x + 2 * i, y)
                            World.tryAddAtom atomType loc world)
                        |> Option.map (fun (world, atom) ->
                            if i > 0 then
                                World.addStrongBond
                                    (atom.Id - 1) atom.Id world
                            else world))
                |> Option.defaultWith loop

        loop ()

module Scenario =

    /// Number of coding atoms in a template.
    let private nCoding = 10

    /// Builds the standard world: a 160x160 box of free atoms, with or
    /// without a template.
    let private build withTemplate (rng : Random) =
        let world = Build.addBox 160 160 World.empty
        let world, template =
            if withTemplate then
                Build.addTemplate rng nCoding world
            else world, Array.empty
        let world = Build.addFreeAtoms (6 * nCoding) 6 world
        world, template

    /// Scenario A: a fixed temperature, with soft strong bonds.
    let createScenarioA withTemplate =
        {
            Name = if withTemplate then "A" else "A, no template"
            Build = build withTemplate
            Parameters =
                {
                    GetTemperature =
                        Parameters.getUniformTemperature nan
                    BondParameterMap =
                        Map [
                            Strong, {
                                Energy = 17
                                PairEnergyMap = Map.empty
                                Barrier = 3
                                BarrierStackReduction = 3
                                StartBarrier = 1
                                Entropy = 0
                            }
                            Weak, {
                                Energy = 2
                                PairEnergyMap = Map.empty
                                Barrier = 0
                                BarrierStackReduction = 0
                                StartBarrier = 0
                                Entropy = 3
                            }
                        ]
                    PeelBarrier = 6
                    CapPeelBarrier = 20
                }
            Schedule = Constant 0.25
        }

    /// Scenario B: a cold/hot temperature cycle, with hard strong
    /// bonds.
    let createScenarioB withTemplate =
        let scenario = createScenarioA withTemplate
        {
            scenario with
                Name = if withTemplate then "B" else "B, no template"
                Parameters =
                    { scenario.Parameters with
                        BondParameterMap =
                            scenario.Parameters.BondParameterMap
                                |> Map.add Strong {
                                    Energy = 34
                                    PairEnergyMap = Map.empty
                                    Barrier = 20
                                    BarrierStackReduction = 20
                                    StartBarrier = 1
                                    Entropy = 0
                                } }
                Schedule =
                    Cycle [|
                        { Temperature = 0.3; NumSweeps = 90_000 }   // cold
                        { Temperature = 3.0; NumSweeps = 10_000 }   // hot
                    |]
        }

    /// Renames the given scenario.
    let withName name scenario =
        { scenario with Name = name }

    /// Changes the given scenario's parameters.
    let mapParameters f scenario =
        { scenario with Parameters = f scenario.Parameters }

    /// Changes the given scenario's parameters for the given bond type.
    let mapBondParameters bondType f scenario =
        scenario
            |> mapParameters (fun parms ->
                let bondParms = f parms.BondParameterMap[bondType]
                { parms with
                    BondParameterMap =
                        Map.add bondType bondParms parms.BondParameterMap })

    /// Sets the given scenario's strong bond barrier, and its
    /// reduction per stack to the same value, so that a fully
    /// templated join has no barrier (apart from the start barrier).
    let withStrongBarrier barrier scenario =
        scenario
            |> mapBondParameters Strong (fun bondParms ->
                { bondParms with
                    Barrier = barrier
                    BarrierStackReduction = barrier })

    /// Sets the given scenario's barrier to a cap coming off the strand
    /// it ends. Infinity means caps never come off.
    let withCapPeelBarrier barrier scenario =
        scenario
            |> mapParameters (fun parms ->
                { parms with CapPeelBarrier = barrier })

    /// Sets the energy of the given type of bond between the given
    /// atom types in the given scenario.
    let withPairEnergy bondType atomTypeA atomTypeB energy scenario =
        scenario
            |> mapBondParameters bondType
                (BondTypeParameters.addPairEnergy atomTypeA atomTypeB energy)

    /// Scenario C: the most faithful copying of a template so far (as
    /// of Oct 13). This is scenario B with a strong bond barrier high
    /// enough that free atoms almost never join without a template,
    /// caps that never come off, and a hot phase as long as the cold
    /// one, so that copies have time to separate. Nothing arises by
    /// chance and nothing dies, so it shows copying on its own.
    let createScenarioC withTemplate =
        { createScenarioB withTemplate with
            Name = if withTemplate then "C" else "C, no template"
            Schedule =
                Cycle [|
                    { Temperature = 0.3; NumSweeps = 100_000 }   // cold
                    { Temperature = 3.0; NumSweeps = 100_000 }   // hot
                |] }
            |> withStrongBarrier 40
            |> withCapPeelBarrier Double.PositiveInfinity

    /// Adds a step to the end of building the given scenario's world.
    let withBuildStep (step : Random -> World -> World) scenario =
        let build rng =
            let world, template = scenario.Build rng
            step rng world, template
        { scenario with Build = build }

    /// A vent along the left wall of the standard box: during a pulse,
    /// kT 3.0 at the wall (the hot temperature of scenario B), falling
    /// linearly to 0.3 (its cold temperature) at the right wall, with
    /// scenario B's cold and hot durations.
    let wallVent =
        {
            X = -80
            HotTemperature = 3.0
            ColdTemperature = 0.3
            Width = 160
            ColdSweeps = 90_000
            HotSweeps = 10_000
        }

    /// The vent scenario: scenario B's box, free atoms and parameters,
    /// heated by a vent along its left wall instead of uniformly. Each
    /// place gets its own cycle, hotter nearer the vent. The world is
    /// seeded with equal numbers of three-atom strands of each kind
    /// (P-W-Q and P-Y-Q), at random places across the whole box, and
    /// the weak Y-Z bond has the given energy.
    ///
    /// Only Y-Z pairs held more strongly than W-X ones should separate
    /// near the vent and copy better there, while farther out, where
    /// the pulse is cooler, only W-X pairs should separate.
    let createVentScenario nSeeds yzEnergy =
        let addStrands rng world =
            (world, [ 1 .. nSeeds ])
                ||> Seq.fold (fun world _ ->
                    world
                        |> Build.addStrandAtRandom rng 74
                            [| AtomType.P; AtomType.W; AtomType.Q |]
                        |> Build.addStrandAtRandom rng 74
                            [| AtomType.P; AtomType.Y; AtomType.Q |])
        let scenario = createScenarioB false
        let name = $"Vent, Y-Z energy {yzEnergy}"
        { scenario with
            Name = name
            Schedule = Vent wallVent }
            |> withBuildStep addStrands
            |> withPairEnergy Weak AtomType.Y AtomType.Z yzEnergy

/// A scenario being run.
type Run =
    {
        /// Scenario being run.
        Scenario : Scenario

        /// IDs of the atoms of the scenario's template, if any.
        Template : AtomId[]

        /// Current state of the world.
        World : World

        /// Temperature over time. This starts as the scenario's own,
        /// but can be replaced, e.g. by a manual setting.
        Schedule : Schedule

        /// Random number generator. It is mutable, so a run must not
        /// be advanced more than once.
        Random : Random

        /// Number of sweeps run so far. (One sweep is one update per
        /// atom, on average.)
        NumSweeps : int
    }

module Run =

    /// Starts running the given scenario with the given seed.
    let create seed scenario =
        let rng = Random(seed)
        let world, template = scenario.Build rng
        {
            Scenario = scenario
            Template = template
            World = world
            Schedule = scenario.Schedule
            Random = rng
            NumSweeps = 0
        }

    /// Current temperature of the given run.
    let getTemperature run =
        Schedule.getTemperature run.NumSweeps run.Schedule |> fst

    /// Current parameters of the given run, including its temperature.
    let getParameters run =
        { run.Scenario.Parameters with
            GetTemperature = getTemperature run }

    /// Runs the given number of sweeps, changing temperature as the
    /// schedule says, and calling the given function after each event.
    /// (See World.sweepWith.)
    let advanceWith onEvent numSweeps run =
        assert(numSweeps >= 0)
        let rec loop numSweeps run =
            if numSweeps = 0 then run
            else
                let getTemp, untilChangeOpt =
                    Schedule.getTemperature run.NumSweeps run.Schedule
                let n =
                    match untilChangeOpt with
                        | Some untilChange -> min numSweeps untilChange
                        | None -> numSweeps
                let parms =
                    { run.Scenario.Parameters with GetTemperature = getTemp }
                let run =
                    { run with
                        World = World.sweepWith parms run.Random n onEvent run.World
                        NumSweeps = run.NumSweeps + n }
                loop (numSweeps - n) run
        loop numSweeps run

    /// Runs the given number of sweeps, changing temperature as the
    /// schedule says.
    let advance numSweeps run =
        let onEvent _ _ = ()
        advanceWith onEvent numSweeps run
