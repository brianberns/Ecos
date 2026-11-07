namespace Ecos.Experiment

open System
open System.IO
open System.Text.Json
open System.Text.Json.Serialization

open Ecos
open Ecos.Experiment.Experiment3

/// Where a strand of a scenario spec is put when its world is built.
/// Locations are relative to the center of the world, as in Build.
type StrandPlacement =

    /// Straight, horizontal, at a random empty place within the given
    /// distance of the center along each axis (or anywhere, if none).
    | Scattered of maxDist : int option

    /// Straight, horizontal, with its first atom at the given location,
    /// running toward +x (rightward) or -x.
    | At of x : int * y : int * rightward : bool

    /// Each atom at the given location, in order; a ring also joins the
    /// last atom to the first. (Strands taken from a built world, which
    /// may be bent or folded, are placed this way.)
    | Shape of locations : (int * int)[] * isRing : bool

/// A strand of a scenario spec.
type StrandSpec =
    {
        /// Pattern of its atom types, e.g. "PHHWYQ" or "P-HH-8*[WY]-Q"
        /// (see Pattern). Each copy is drawn from it separately.
        Pattern : string

        /// Where to put it.
        Placement : StrandPlacement

        /// Number of copies to add. (Only a randomly placed strand can
        /// have more than one.)
        Count : int

        /// Whether its atoms are shown as the scenario's template.
        IsTemplate : bool
    }

/// Parameters of a bond type, as data: those of BondTypeParameters, with
/// atom types as letters.
type BondTypeSpec =
    {
        Energy : float

        /// Pair energies that override the default, by pair, e.g. "H-H".
        PairEnergies : Map<string, float>

        Barrier : float
        BarrierStackReduction : float
        StartBarrier : float
        Entropy : float
        HydrolysisRate : float
        ProtectedHydrolysisRate : float

        /// Sharpest bend allowed, as the largest allowed cosine.
        BendLimit : Rational

        BendStiffness : float

        /// Bend stiffnesses that override the default, by atom type,
        /// e.g. "H".
        BendStiffnesses : Map<string, float>
    }

/// Parameters, as data: those of Parameters, apart from temperature,
/// which comes from the schedule.
type ParametersSpec =
    {
        MoveAtomWeight : float
        ToggleStrongBondWeight : float
        ToggleWeakBondWeight : float
        Strong : BondTypeSpec
        Weak : BondTypeSpec
        StackEnergy : float
        PeelBarrier : float
        CapPeelBarrier : float
    }

/// A scenario as plain data, so that it can be edited, saved and loaded.
/// Building its world is deterministic for a given seed.
type ScenarioSpec =
    {
        /// Name of the scenario.
        Name : string

        /// Width and height of the world (a torus), each even.
        Width : int
        Height : int

        /// Walls, by location relative to the center.
        Walls : (int * int)[]

        /// Strands, added in order before the free atoms.
        Strands : StrandSpec[]

        /// Weak bonds between strand atoms, each given as (strand index,
        /// atom index) at each end. Only strands with a count of one can
        /// be referred to.
        WeakBonds : ((int * int) * (int * int))[]

        /// Free atoms of each type, by letter, as a percentage of the
        /// sites at which atoms can be put (see getSiteCount).
        Occupancy : Map<string, float>

        /// Free atoms are put at random empty places within this
        /// distance of the center along each axis, or anywhere, if none.
        FoodWithin : int option

        /// Parameters.
        Parameters : ParametersSpec

        /// Temperature over time.
        Schedule : Schedule
    }

module ScenarioSpec =

    /// Every atom type, in display order.
    let atomTypes =
        [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z; AtomType.H; AtomType.P; AtomType.Q |]

    /// Letter of the given atom type.
    let toLetter (atomType : AtomType) =
        match atomType with
            | AtomType.W -> "W" | AtomType.X -> "X" | AtomType.Y -> "Y" | AtomType.Z -> "Z"
            | AtomType.H -> "H" | AtomType.P -> "P" | AtomType.Q -> "Q"

    /// Atom type of the given letter, if any.
    let tryParseAtomType (letter : string) =
        atomTypes |> Array.tryFind (fun t -> toLetter t = letter.Trim().ToUpperInvariant())

    /// Atom types of the given sequence, if every letter is one.
    let tryParseSequence (sequence : string) =
        let types = sequence.Trim() |> Seq.map (string >> tryParseAtomType) |> Seq.toArray
        if types |> Array.forall Option.isSome then Some (Array.choose id types)
        else None

    /// Atom types of the given sequence; fails on an unknown letter.
    let parseSequence (sequence : string) =
        match tryParseSequence sequence with
            | Some types -> types
            | None -> failwith $"Sequence \"{sequence}\" has a letter that is not an atom type"

    /// Named classes of atom types in patterns.
    let private namedClasses =
        Map [
            "CODING", [| AtomType.W; AtomType.X; AtomType.Y; AtomType.Z |]
            "CAP", [| AtomType.P; AtomType.Q |]
        ]

    /// A term of a pattern: one of the given alternatives (each a run of
    /// atom types), drawn a number of times from the given range.
    type PatternTerm =
        {
            MinRepeat : int
            MaxRepeat : int
            Alternatives : AtomType[][]
        }

    /// Splits a pattern into terms at each "-" outside brackets.
    let private splitTerms (pattern : string) =
        let terms = ResizeArray()
        let current = Text.StringBuilder()
        let mutable depth = 0
        for c in pattern do
            match c with
                | '[' -> depth <- depth + 1; current.Append(c) |> ignore
                | ']' -> depth <- depth - 1; current.Append(c) |> ignore
                | '-' when depth = 0 ->
                    terms.Add(current.ToString())
                    current.Clear() |> ignore
                | _ -> current.Append(c) |> ignore
        terms.Add(current.ToString())
        terms.ToArray()

    /// Parses a pattern: terms separated by "-", each optionally
    /// repeated as "n*term", or a random number of times in a range as
    /// "[m-n]*term". A term is a run of atom letters ("PHH"),
    /// a set of letters of which one is drawn ("[WY]"), or a named set:
    /// "Coding" ([WXYZ]) or "Cap" ([PQ]). E.g. "Cap-H-10*Coding-Cap".
    let tryParsePattern (pattern : string) : Result<PatternTerm[], string> =
        let parseTerm (term : string) =
            let term = term.Trim()
            let tryParseCount (text : string) =
                match Int32.TryParse(text.Trim()) with
                    | true, n when n >= 0 -> Some n
                    | _ -> None
            let repeat, element =
                match term.IndexOf('*') with
                    | -1 -> Ok (1, 1), term
                    | i ->
                        let count = term.Substring(0, i).Trim()
                        let element = term.Substring(i + 1).Trim()
                        let range =
                            if count.StartsWith("[") && count.EndsWith("]") then
                                match count.Substring(1, count.Length - 2).Split('-') with
                                    | [| a; b |] ->
                                        match tryParseCount a, tryParseCount b with
                                            | Some a, Some b when a <= b -> Some (a, b)
                                            | _ -> None
                                    | _ -> None
                            else tryParseCount count |> Option.map (fun n -> n, n)
                        match range with
                            | Some range -> Ok range, element
                            | None -> Error $"\"{term}\" doesn't start with a count, e.g. \"8*[WY]\" or \"[1-2]*H\"", ""
            let alternatives =
                if element = "" then Error $"\"{term}\" is empty"
                elif element.StartsWith("[") then
                    if not (element.EndsWith("]")) || element.Length < 3 then Error $"\"{element}\" is not a set, e.g. \"[WY]\""
                    else
                        match tryParseSequence (element.Substring(1, element.Length - 2)) with
                            | Some types -> Ok (types |> Array.distinct |> Array.map Array.singleton)
                            | None -> Error $"\"{element}\" has a letter that is not an atom type"
                else
                    match namedClasses.TryFind (element.ToUpperInvariant()) with
                        | Some types -> Ok (types |> Array.map Array.singleton)
                        | None ->
                            match tryParseSequence element with
                                | Some types -> Ok [| types |]
                                | None -> Error $"\"{element}\" is not atom letters, a set like [WY], Coding or Cap"
            match repeat, alternatives with
                | Ok (lo, hi), Ok alts -> Ok { MinRepeat = lo; MaxRepeat = hi; Alternatives = alts }
                | Error msg, _ | _, Error msg -> Error msg
        if String.IsNullOrWhiteSpace pattern then Error "The pattern is empty"
        else
            let results = splitTerms pattern |> Array.map parseTerm
            match results |> Array.tryPick (function Error msg -> Some msg | Ok _ -> None) with
                | Some msg -> Error msg
                | None -> Ok (results |> Array.choose (function Ok t -> Some t | Error _ -> None))

    /// Draws a sequence from the given pattern terms. A term with a
    /// single alternative and a fixed count uses no random numbers.
    let samplePattern (rng : Random) (terms : PatternTerm[]) =
        [|
            for term in terms do
                let repeat =
                    if term.MinRepeat = term.MaxRepeat then term.MinRepeat
                    else rng.Next(term.MinRepeat, term.MaxRepeat + 1)
                for _ in 1 .. repeat do
                    if term.Alternatives.Length = 1 then yield! term.Alternatives[0]
                    else yield! term.Alternatives[rng.Next(term.Alternatives.Length)]
        |]

    /// Parses a pattern; fails if it is invalid.
    let parsePattern pattern =
        match tryParsePattern pattern with
            | Ok terms -> terms
            | Error msg -> failwith $"Pattern \"{pattern}\": {msg}"

    /// Number of sites at which atoms can be put in the given spec's
    /// world (atoms are 2x2), or in its food's part of it.
    let getSiteCount (spec : ScenarioSpec) =
        match spec.FoodWithin with
            | Some d -> (d / 2 + 1) * (d / 2 + 1)
            | None -> spec.Width * spec.Height / 4

    /// Number of free atoms of the given type in the given spec.
    let getFoodCount (spec : ScenarioSpec) (atomType : AtomType) =
        let percent = spec.Occupancy |> Map.tryFind (toLetter atomType) |> Option.defaultValue 0.0
        int (Math.Round(percent / 100.0 * float (getSiteCount spec)))

    /// Key of a pair of atom types, e.g. "H-H", in alphabetical order.
    let toPairKey a b =
        let a, b = toLetter a, toLetter b
        if a <= b then $"{a}-{b}" else $"{b}-{a}"

    /// Atom types of the given pair key, if valid.
    let tryParsePairKey (key : string) =
        match key.Split('-') with
            | [| a; b |] ->
                match tryParseAtomType a, tryParseAtomType b with
                    | Some a, Some b -> Some (a, b)
                    | _ -> None
            | _ -> None

    /// Bond-type parameters as data.
    let ofBondTypeParameters (parms : BondTypeParameters) =
        {
            Energy = parms.Energy
            PairEnergies =
                parms.PairEnergyMap
                    |> Map.toSeq
                    |> Seq.map (fun ((a, b), e) -> toPairKey a b, e)
                    |> Map
            Barrier = parms.Barrier
            BarrierStackReduction = parms.BarrierStackReduction
            StartBarrier = parms.StartBarrier
            Entropy = parms.Entropy
            HydrolysisRate = parms.HydrolysisRate
            ProtectedHydrolysisRate = parms.ProtectedHydrolysisRate
            BendLimit = parms.BendLimit
            BendStiffness = parms.BendStiffness
            BendStiffnesses =
                parms.BendStiffnessMap
                    |> Map.toSeq
                    |> Seq.map (fun (t, k) -> toLetter t, k)
                    |> Map
        }

    /// Bond-type parameters from data.
    let toBondTypeParameters (spec : BondTypeSpec) : BondTypeParameters =
        let pairEnergyMap =
            (Map.empty, spec.PairEnergies)
                ||> Map.fold (fun map key energy ->
                    match tryParsePairKey key with
                        | Some (a, b) -> map |> Map.add (a, b) energy |> Map.add (b, a) energy
                        | None -> failwith $"Pair \"{key}\" is not two atom types, e.g. \"H-H\"")
        let bendStiffnessMap =
            spec.BendStiffnesses
                |> Map.toSeq
                |> Seq.map (fun (letter, k) ->
                    match tryParseAtomType letter with
                        | Some t -> t, k
                        | None -> failwith $"\"{letter}\" is not an atom type")
                |> Map
        {
            Energy = spec.Energy
            PairEnergyMap = pairEnergyMap
            Barrier = spec.Barrier
            BarrierStackReduction = spec.BarrierStackReduction
            StartBarrier = spec.StartBarrier
            Entropy = spec.Entropy
            HydrolysisRate = spec.HydrolysisRate
            ProtectedHydrolysisRate = spec.ProtectedHydrolysisRate
            BendLimit = spec.BendLimit
            BendStiffness = spec.BendStiffness
            BendStiffnessMap = bendStiffnessMap
        }

    /// Parameters as data.
    let ofParameters (parms : Parameters) =
        {
            MoveAtomWeight = parms.MoveAtomWeight
            ToggleStrongBondWeight = parms.ToggleStrongBondWeight
            ToggleWeakBondWeight = parms.ToggleWeakBondWeight
            Strong = ofBondTypeParameters parms.BondTypeParameterMap[Strong]
            Weak = ofBondTypeParameters parms.BondTypeParameterMap[Weak]
            StackEnergy = parms.StackEnergy
            PeelBarrier = parms.PeelBarrier
            CapPeelBarrier = parms.CapPeelBarrier
        }

    /// Parameters from data. (Their temperature is NaN: it comes from
    /// the schedule.)
    let toParameters (spec : ParametersSpec) : Parameters =
        {
            MoveAtomWeight = spec.MoveAtomWeight
            ToggleStrongBondWeight = spec.ToggleStrongBondWeight
            ToggleWeakBondWeight = spec.ToggleWeakBondWeight
            GetTemperature = Parameters.getUniformTemperature nan
            BondTypeParameterMap =
                BondTypeParameterMap.ofMap (
                    Map [
                        Strong, toBondTypeParameters spec.Strong
                        Weak, toBondTypeParameters spec.Weak
                    ])
            StackEnergy = spec.StackEnergy
            PeelBarrier = spec.PeelBarrier
            CapPeelBarrier = spec.CapPeelBarrier
        }

    /// Adds atoms of the given types at the given locations (absolute),
    /// if there is room for all of them. Answers the world and their IDs.
    let private tryAddAtoms (atomTypes : AtomType[]) (locations : Point[]) (world : World) =
        (Some (world, []), Array.zip atomTypes locations)
            ||> Array.fold (fun acc (atomType, loc) ->
                acc |> Option.bind (fun (world, ids) ->
                    World.tryAddAtom atomType loc world
                        |> Option.map (fun (world, atom) -> world, atom.Id :: ids)))
            |> Option.map (fun (world, ids) -> world, ids |> List.rev |> List.toArray)

    /// Joins the given atoms in order (and the last to the first, for a
    /// ring).
    let private join parms (ids : AtomId[]) isRing (world : World) =
        let tryJoin (world : World) (a, b) =
            match World.tryAddStrongBond parms a b world with
                | Some world -> world
                | None -> failwith "A strand's atoms can't all be joined (too far apart, or bent too sharply)"
        let world = Array.pairwise ids |> Array.fold tryJoin world
        if isRing && ids.Length > 2 then tryJoin world (ids[ids.Length - 1], ids[0])
        else world

    /// Adds a strand of the given types with its atoms at the given
    /// locations (absolute), joined in order. Answers the world and the
    /// strand's atom IDs.
    let private addShape parms (atomTypes : AtomType[]) (locations : Point[]) isRing (world : World) =
        if atomTypes.Length <> locations.Length then
            failwith $"A shaped strand has {atomTypes.Length} atom types but {locations.Length} locations"
        match tryAddAtoms atomTypes locations world with
            | Some (world, ids) -> join parms ids isRing world, ids
            | None -> failwith "No room for a shaped strand's atoms"

    /// Adds a straight strand with its first atom at the given location
    /// (relative to the center), stepping by dx. Answers the world and
    /// the strand's atom IDs, if there is room.
    let private tryAddStraight parms x y dx (atomTypes : AtomType[]) (world : World) =
        let center = Build.center world
        let locations = Array.init atomTypes.Length (fun i -> center + Point.create (x + dx * i, y))
        tryAddAtoms atomTypes locations world
            |> Option.map (fun (world, ids) -> join parms ids false world, ids)

    /// Adds the given strand spec's strands. Answers the world and the
    /// atom IDs of each strand added.
    let private addStrands parms (rng : Random) (spec : StrandSpec) (world : World) =
        let terms = parsePattern spec.Pattern
        let draw () =
            let atomTypes = samplePattern rng terms
            if atomTypes.Length = 0 then failwith $"Pattern \"{spec.Pattern}\" has no atoms"
            atomTypes
        match spec.Placement with
            | At (x, y, rightward) ->
                if x % 2 <> 0 || y % 2 <> 0 then failwith $"Strand location ({x}, {y}) must be even"
                let atomTypes = draw ()
                match tryAddStraight parms x y (if rightward then 2 else -2) atomTypes world with
                    | Some (world, ids) -> world, [ ids ]
                    | None -> failwith $"No room for strand {spec.Pattern} at ({x}, {y})"
            | Shape (locations, isRing) ->
                let center = Build.center world
                let locations = locations |> Array.map (fun (x, y) -> center + Point.create (x, y))
                let world, ids = addShape parms (draw ()) locations isRing world
                world, [ ids ]
            | Scattered maxDistOpt ->
                let maxX, maxY =
                    match maxDistOpt with
                        | Some d -> d, d
                        | None -> world.Size.X, world.Size.Y
                ((world, []), [ 1 .. spec.Count ])
                    ||> Seq.fold (fun (world, idss) _ ->
                        let atomTypes = draw ()
                        let rec loop nTries =
                            if nTries > 10_000 then
                                failwith $"No room for strand {spec.Pattern} at random"
                            let x = 2 * rng.Next(-maxX / 2, (maxX - 2 * atomTypes.Length) / 2 + 1)
                            let y = 2 * rng.Next(-maxY / 2, maxY / 2 + 1)
                            match tryAddStraight parms x y 2 atomTypes world with
                                | Some (world, ids) -> world, ids :: idss
                                | None -> loop (nTries + 1)
                        loop 0)
                    |> fun (world, idss) -> world, List.rev idss

    /// Adds the given number of free atoms of the given type, at random
    /// empty places within the given distance of the center, if any.
    let private addFood (rng : Random) maxDistOpt (atomType, n) (world : World) =
        let pick () =
            match maxDistOpt with
                | Some maxDist ->
                    let x = 2 * rng.Next(-maxDist / 2, maxDist / 2 + 1)
                    let y = 2 * rng.Next(-maxDist / 2, maxDist / 2 + 1)
                    Build.center world + Point.create (x, y)
                | None ->
                    Point.create (2 * rng.Next(world.Size.X / 2), 2 * rng.Next(world.Size.Y / 2))
        (world, [ 1 .. n ])
            ||> Seq.fold (fun world _ ->
                let rec loop nTries =
                    if nTries > 100_000 then failwith "No room for all the free atoms"
                    match World.tryAddAtom atomType (pick ()) world with
                        | Some (world, _) -> world
                        | None -> loop (nTries + 1)
                loop 0)

    /// Builds the given spec's world with the given parameters and
    /// random number generator. Answers the world and its template's
    /// atom IDs.
    let build (spec : ScenarioSpec) parms (rng : Random) =
        if spec.Width % 2 <> 0 || spec.Height % 2 <> 0 || spec.Width <= 0 || spec.Height <= 0 then
            failwith "World width and height must be positive and even"
        let world = World.create (Point.create (spec.Width, spec.Height))
        let world =
            (world, spec.Walls)
                ||> Array.fold (fun world (x, y) ->
                    World.addWall (Build.center world + Point.create (x, y)) world)
        let world, strandIds =
            ((world, []), spec.Strands)
                ||> Array.fold (fun (world, acc) strandSpec ->
                    let world, idss = addStrands parms rng strandSpec world
                    world, (strandSpec, idss) :: acc)
        let strandIds = List.rev strandIds |> List.toArray
        let world =
            (world, spec.WeakBonds)
                ||> Array.fold (fun world ((iA, jA), (iB, jB)) ->
                    let getId i j =
                        match strandIds |> Array.tryItem i with
                            | Some (_, [ ids ]) when j >= 0 && j < ids.Length -> ids[j]
                            | _ -> failwith $"Weak bond refers to strand {i}, atom {j}, which isn't a single strand's atom"
                    match World.tryAddWeakBond parms (getId iA jA) (getId iB jB) world with
                        | Some world -> world
                        | None -> failwith $"Can't pair strand {iA} atom {jA} with strand {iB} atom {jB}")
        let world =
            (world, atomTypes)
                ||> Array.fold (fun world atomType ->
                    addFood rng spec.FoodWithin (atomType, getFoodCount spec atomType) world)
        let template =
            strandIds
                |> Array.collect (fun (strandSpec, idss) ->
                    if strandSpec.IsTemplate then idss |> List.toArray |> Array.concat
                    else Array.empty)
        world, template

    /// The scenario that the given spec describes.
    let toScenario (spec : ScenarioSpec) : Scenario =
        {
            Name = spec.Name
            Build = build spec
            Parameters = toParameters spec.Parameters
            Schedule = spec.Schedule
            TryLabelStrands = None
        }

    /// A spec of the given scenario, from its world as built with the
    /// given seed: its strands, in their places, and its free atoms, by
    /// type, scattered as widely as the scenario scatters them. (So a
    /// scenario that builds random strands is fixed to one seed's.)
    let ofScenario seed (scenario : Scenario) =
        let world, template = scenario.Build scenario.Parameters (Random(seed))
        let center = Build.center world
        let relative (loc : Point) =
            let d = loc - center
            d.X, d.Y
        let templateIds = set template
        let strands = getStrands world
        let strandSpecs =
            strands
                |> Array.map (fun strand ->
                    {
                        Pattern =
                            strand.AtomIds |> Array.map (fun id -> toLetter world.Atoms[id].Type) |> String.concat ""
                        Placement =
                            let locations = strand.AtomIds |> Array.map (fun id -> relative world.Atoms[id].Location)
                            let steps = locations |> Array.pairwise |> Array.map (fun ((x0, y0), (x1, y1)) -> x1 - x0, y1 - y0) |> Array.distinct
                            match steps with
                                | [| (2, 0) |] when not strand.IsRing -> At (fst locations[0], snd locations[0], true)
                                | [| (-2, 0) |] when not strand.IsRing -> At (fst locations[0], snd locations[0], false)
                                | _ -> Shape (locations, strand.IsRing)
                        Count = 1
                        IsTemplate = strand.AtomIds |> Array.exists templateIds.Contains
                    })
        let strandIndex =
            strands
                |> Array.indexed
                |> Array.collect (fun (i, strand) ->
                    strand.AtomIds |> Array.mapi (fun j id -> id, (i, j)))
                |> Map
        let weakBonds =
            world.BondTypeMap[Weak]
                |> Map.toSeq
                |> Seq.collect (fun (a, bs) -> bs |> Seq.where (fun b -> a < b) |> Seq.map (fun b -> a, b))
                |> Seq.choose (fun (a, b) ->
                    match strandIndex.TryFind a, strandIndex.TryFind b with
                        | Some ra, Some rb -> Some (ra, rb)
                        | _ -> None)   // a free atom's pairing is not kept
                |> Seq.toArray
        let freeAtoms =
            world.Atoms |> Array.where (fun atom -> not (strandIndex.ContainsKey atom.Id))
        let foodWithin =
            if freeAtoms.Length = 0 then None
            else
                let reach =
                    freeAtoms
                        |> Array.map (fun atom ->
                            let x, y = relative atom.Location
                            max (abs x) (abs y))
                        |> Array.max
                let maxDist = 2 * reach
                if maxDist >= min world.Size.X world.Size.Y - 4 then None   // scattered everywhere
                else Some maxDist
        let nSites =
            match foodWithin with
                | Some d -> (d / 2 + 1) * (d / 2 + 1)
                | None -> world.Size.X * world.Size.Y / 4
        let occupancy =
            freeAtoms
                |> Array.countBy (fun atom -> toLetter atom.Type)
                |> Array.map (fun (letter, n) -> letter, 100.0 * float n / float nSites)
                |> Map
        {
            Name = scenario.Name
            Width = world.Size.X
            Height = world.Size.Y
            Walls = world.Walls |> Seq.map relative |> Seq.toArray
            Strands = strandSpecs
            WeakBonds = weakBonds
            Occupancy = occupancy
            FoodWithin = foodWithin
            Parameters = ofParameters scenario.Parameters
            Schedule = scenario.Schedule
        }

    /// JSON options for specs: F# types, readable, and infinite barriers
    /// allowed.
    let private jsonOptions =
        let options =
            JsonFSharpOptions.Default()
                .WithUnionExternalTag()
                .WithUnionNamedFields()
                .WithUnionUnwrapFieldlessTags()
                .ToJsonSerializerOptions()
        options.WriteIndented <- true
        options.NumberHandling <- JsonNumberHandling.AllowNamedFloatingPointLiterals
        options

    /// The given spec as JSON.
    let toJson (spec : ScenarioSpec) =
        JsonSerializer.Serialize(spec, jsonOptions)

    /// The spec in the given JSON.
    let ofJson (json : string) =
        JsonSerializer.Deserialize<ScenarioSpec>(json, jsonOptions)

    /// Saves the given spec to the given file.
    let save path spec =
        File.WriteAllText(path, toJson spec)

    /// Loads a spec from the given file.
    let load path =
        File.ReadAllText(path) |> ofJson
