namespace Ecos.Builder

open System

open Avalonia.Controls
open Avalonia.FuncUI
open Avalonia.FuncUI.DSL
open Avalonia.FuncUI.Types
open Avalonia.Layout
open Avalonia.Media

open Ecos
open Ecos.Experiment
open Fields

/// The spec editor: a tab for each part of a spec. Every change is an
/// Edit message, which rebuilds the run.
module Editor =

    /// Rounds the given size down to an even number of at least 2.
    let private toEven n = max 2 (n - n % 2)

    /// Rounds a percentage for display.
    let private toDecimal (percent : float) = decimal (Math.Round(percent, 3))

    /// The given spec's occupancy of the given type, in percent.
    let private getOccupancy (spec : ScenarioSpec) letter =
        spec.Occupancy |> Map.tryFind letter |> Option.defaultValue 0.0

    /// Total occupancy of free atoms, in percent.
    let private getTotalOccupancy (spec : ScenarioSpec) =
        spec.Occupancy |> Map.toSeq |> Seq.sumBy snd

    /// The given spec with the given occupancy of the given type, kept
    /// within what the other types leave (the total is at most 100%).
    let private setOccupancy letter percent (spec : ScenarioSpec) =
        let others = getTotalOccupancy spec - getOccupancy spec letter
        let percent = percent |> max 0.0 |> min (100.0 - others)
        { spec with Occupancy = spec.Occupancy |> Map.add letter percent }

    /// The given spec with every type's occupancy scaled to the given
    /// total, keeping their ratios. (If there are none, every type gets
    /// an equal share.)
    let private scaleOccupancy total (spec : ScenarioSpec) =
        let total = total |> max 0.0 |> min 100.0
        let current = getTotalOccupancy spec
        let occupancy =
            if current > 0.0 then spec.Occupancy |> Map.map (fun _ p -> p * total / current)
            else
                ScenarioSpec.atomTypes
                    |> Array.map (fun t -> ScenarioSpec.toLetter t, total / float ScenarioSpec.atomTypes.Length)
                    |> Map
        { spec with Occupancy = occupancy }

    let private renderWorld (spec : ScenarioSpec) dispatch =
        let edit f = dispatch (Edit f)
        let nSites = ScenarioSpec.getSiteCount spec
        let nFood = ScenarioSpec.atomTypes |> Array.sumBy (ScenarioSpec.getFoodCount spec)
        let nStrandAtoms =
            spec.Strands
                |> Array.sumBy (fun st ->
                    match ScenarioSpec.tryParsePattern st.Pattern with
                        | Ok terms -> st.Count * (ScenarioSpec.samplePattern (Random(0)) terms).Length
                        | Error _ -> 0)
        let total = getTotalOccupancy spec
        column [
            heading "World"
            intField "world.width" "Width" 4 2000 2 spec.Width (fun n -> edit (fun s -> { s with Width = toEven n }))
            intField "world.height" "Height" 4 2000 2 spec.Height (fun n -> edit (fun s -> { s with Height = toEven n }))
            note $"A torus with {nSites:N0} sites for atoms (each atom is 2 x 2)."
            if spec.Walls.Length > 0 then
                row [
                    note $"{spec.Walls.Length} walls."
                    smallButton "world.clearWalls" "Clear" "Remove every wall" (fun () -> edit (fun s -> { s with Walls = [||] }))
                ]

            heading "Free atoms"
            note "Each type's share of the sites, scattered at random."
            for atomType in ScenarioSpec.atomTypes do
                let letter = ScenarioSpec.toLetter atomType
                row [
                    label letter 40.0
                    spinner $"food.{letter}" 110.0 0m 100m 1m "0.##" (toDecimal (getOccupancy spec letter)) (fun v ->
                        edit (setOccupancy letter (float v)))
                    label "%" 15.0
                    note $"{ScenarioSpec.getFoodCount spec atomType:N0} atoms"
                ]
            row [
                label "All" 40.0
                Slider.create [
                    Slider.minimum 0.0
                    Slider.maximum 100.0
                    Slider.value total
                    Slider.width 260.0
                    Slider.margin margin
                    Slider.verticalAlignment VerticalAlignment.Center
                    Slider.onValueChanged ((fun v ->
                        dispatch (Edit (fun s ->
                            if abs (v - getTotalOccupancy s) < 0.005 then s
                            else scaleOccupancy v s))), SubPatchOptions.OnChangeOf "food.total")
                ]
                note $"{total:F1}%% in all"
            ]
            note $"{nFood:N0} free atoms; {nStrandAtoms:N0} more in strands."
            match spec.FoodWithin with
                | Some d ->
                    row [
                        note $"Free atoms are scattered within {d} of the center."
                        smallButton "food.everywhere" "Everywhere" "Scatter free atoms over the whole world" (fun () ->
                            edit (fun s -> { s with FoodWithin = None }))
                    ]
                | None -> ()
        ]

    /// A description of what the given pattern draws, or why it's invalid.
    let private describePattern (pattern : string) =
        match ScenarioSpec.tryParsePattern pattern with
            | Ok terms ->
                let lengths =
                    terms |> Array.map (fun t ->
                        let lens = t.Alternatives |> Array.map Array.length
                        t.MinRepeat * Array.min lens, t.MaxRepeat * Array.max lens)
                let lo, hi = Array.sumBy fst lengths, Array.sumBy snd lengths
                let sample =
                    ScenarioSpec.samplePattern (Random(0)) terms
                        |> Array.map ScenarioSpec.toLetter
                        |> String.concat ""
                let length = if lo = hi then $"{lo} atoms" else $"{lo} to {hi} atoms"
                Ok $"{length}, e.g. {sample}"
            | Error msg -> Error msg

    /// Strong-bond colors offered for a strand's family, by name.
    let strandColors =
        [|
            "Black", None
            "Green", Some "#00A000"
            "Red", Some "#D00000"
            "Blue", Some "#1F5FD0"
            "Orange", Some "#E07000"
            "Purple", Some "#8000C0"
            "Teal", Some "#008080"
            "Brown", Some "#8B4513"
            "Magenta", Some "#D000A0"
        |]

    let private renderStrand index (strand : StrandSpec) dispatch =
        let key = $"strand.{index}"
        let edit f =
            dispatch (Edit (fun (s : ScenarioSpec) ->
                { s with Strands = s.Strands |> Array.updateAt index (f s.Strands[index]) }))
        Border.create [
            Border.borderBrush Brushes.LightGray
            Border.borderThickness 1.0
            Border.cornerRadius 4.0
            Border.margin (Avalonia.Thickness(4.0, 4.0))
            Border.padding 4.0
            Border.child (
                column [
                    row [
                        textBox $"{key}.pattern" 250.0 strand.Pattern (fun text ->
                            edit (fun st -> { st with Pattern = text.Trim() }))
                        label "x" 10.0
                        intSpinner $"{key}.count" 100.0 0 100_000 1 strand.Count (fun n ->
                            edit (fun st -> { st with Count = n }))
                        checkBox $"{key}.template" "Template" strand.IsTemplate (fun flag ->
                            edit (fun st -> { st with IsTemplate = flag }))
                        let colorIndex =
                            strandColors
                                |> Array.tryFindIndex (fun (_, c) -> c = strand.Color)
                                |> Option.defaultValue 0
                        comboBox $"{key}.color" 100.0 (Array.map fst strandColors) colorIndex (fun i ->
                            edit (fun st -> { st with Color = snd strandColors[i] }))
                        smallButton $"{key}.remove" "✕" "Remove this strand" (fun () ->
                            dispatch (Edit (fun s ->
                                { s with
                                    Strands = s.Strands |> Array.removeAt index
                                    WeakBonds = [||] })))
                    ]
                    match describePattern strand.Pattern with
                        | Ok text -> note text
                        | Error msg -> warning msg
                    match strand.Placement with
                        | Scattered None -> ()
                        | Scattered (Some d) -> note $"Scattered within {d} of the center."
                        | At (x, y, rightward) ->
                            let direction = if rightward then "rightward" else "leftward"
                            note $"Placed straight at ({x}, {y}), running {direction}."
                        | Shape (locations, _) -> note $"Placed in a fixed shape of {locations.Length} atoms."
                ])
        ] :> IView

    let private renderStrands (spec : ScenarioSpec) dispatch =
        column [
            heading "Strands"
            note "Each copy is drawn from its pattern and scattered at random, straight. A color shows the strong bonds of every chain in the strand's family: any chain the pattern could draw, or a complete copy of one."
            note "A pattern is terms separated by \"-\": atom letters (PHH), a set of which one is drawn ([WY]), Coding (one of WXYZ) or Cap (P or Q). A term can repeat a fixed number of times (8*[WY]) or a random number in a range ([1-2]*H). E.g. Cap-H-10*Coding-Cap."
            for i, strand in Array.indexed spec.Strands do
                renderStrand i strand dispatch
            row [
                button "strands.add" "Add strand" (fun () ->
                    dispatch (Edit (fun s ->
                        { s with
                            Strands =
                                Array.append s.Strands [|
                                    { Pattern = "P-10*Coding-Q"; Placement = Scattered None; Count = 1; IsTemplate = true; Color = None }
                                |] })))
            ]
            if spec.WeakBonds.Length > 0 then
                row [
                    note $"{spec.WeakBonds.Length} pairings between strands."
                    smallButton "strands.clearPairs" "Clear" "Remove the pairings" (fun () ->
                        dispatch (Edit (fun s -> { s with WeakBonds = [||] })))
                ]
        ]

    /// Formats a bend limit.
    let private formatRational (r : Rational) = $"{r.Numerator}/{r.Denominator}"

    /// Parses a bend limit, e.g. "0/1" or "-4/5".
    let private tryParseRational (text : string) =
        match text.Split('/') with
            | [| n; d |] ->
                match tryParseInt n, tryParseInt d with
                    | Some n, Some d when d > 0 -> Some (Rational.create n d)
                    | _ -> None
            | [| n |] -> tryParseInt n |> Option.map (fun n -> Rational.create n 1)
            | _ -> None

    /// Pairs offered when one is added, in order.
    let private pairChoices = [| "H-H"; "W-X"; "Y-Z"; "P-Q" |]

    let private renderBondType name (getBond : ParametersSpec -> BondTypeSpec) (setBond : BondTypeSpec -> ParametersSpec -> ParametersSpec) (bond : BondTypeSpec) dispatch =
        let key = $"bond.{name}"
        let edit f =
            dispatch (Edit (fun (s : ScenarioSpec) ->
                { s with Parameters = setBond (f (getBond s.Parameters)) s.Parameters }))
        let field id text value set =
            floatField $"{key}.{id}" text value (fun v -> edit (fun b -> set v b))
        column [
            heading $"{name} bonds"
            field "energy" "Energy" bond.Energy (fun v b -> { b with Energy = v })
            for pairKey, energy in Map.toSeq bond.PairEnergies do
                row [
                    label "" 16.0
                    textBox $"{key}.pair.{pairKey}.key" 60.0 pairKey (fun text ->
                        match ScenarioSpec.tryParsePairKey text with
                            | Some (a, b) ->
                                let newKey = ScenarioSpec.toPairKey a b
                                edit (fun b ->
                                    match b.PairEnergies.TryFind pairKey with
                                        | Some e -> { b with PairEnergies = b.PairEnergies |> Map.remove pairKey |> Map.add newKey e }
                                        | None -> b)
                            | None -> ())
                    label "energy" 50.0
                    textBox $"{key}.pair.{pairKey}.value" valueWidth (formatFloat energy) (fun text ->
                        tryParseFloat text |> Option.iter (fun v ->
                            edit (fun b -> { b with PairEnergies = b.PairEnergies |> Map.add pairKey v })))
                    smallButton $"{key}.pair.{pairKey}.remove" "✕" "Remove this pair energy" (fun () ->
                        edit (fun b -> { b with PairEnergies = b.PairEnergies |> Map.remove pairKey }))
                ]
            row [
                label "" 16.0
                smallButton $"{key}.pair.add" "Add pair energy" "Override the energy of one pair of atom types" (fun () ->
                    edit (fun b ->
                        let newKey =
                            pairChoices
                                |> Array.tryFind (fun k -> not (b.PairEnergies.ContainsKey k))
                                |> Option.defaultValue "H-H"
                        { b with PairEnergies = b.PairEnergies |> Map.add newKey b.Energy }))
            ]
            field "barrier" "Barrier" bond.Barrier (fun v b -> { b with Barrier = v })
            field "stackReduction" "Barrier reduction per stack" bond.BarrierStackReduction (fun v b -> { b with BarrierStackReduction = v })
            field "startBarrier" "Start barrier" bond.StartBarrier (fun v b -> { b with StartBarrier = v })
            field "entropy" "Entropy" bond.Entropy (fun v b -> { b with Entropy = v })
            field "hydrolysis" "Hydrolysis rate" bond.HydrolysisRate (fun v b -> { b with HydrolysisRate = v })
            field "protectedHydrolysis" "Protected hydrolysis rate" bond.ProtectedHydrolysisRate (fun v b -> { b with ProtectedHydrolysisRate = v })
            row [
                label "Bend limit (cosine)" labelWidth
                textBox $"{key}.bendLimit" valueWidth (formatRational bond.BendLimit) (fun text ->
                    tryParseRational text |> Option.iter (fun r -> edit (fun b -> { b with BendLimit = r })))
                note "0/1 right angle, -4/5 straight, 1/1 any"
            ]
            field "bendStiffness" "Bend stiffness" bond.BendStiffness (fun v b -> { b with BendStiffness = v })
            for letter, kappa in Map.toSeq bond.BendStiffnesses do
                row [
                    label "" 16.0
                    label "at" 20.0
                    textBox $"{key}.bend.{letter}.key" 40.0 letter (fun text ->
                        match ScenarioSpec.tryParseAtomType text with
                            | Some t ->
                                let newLetter = ScenarioSpec.toLetter t
                                edit (fun b ->
                                    match b.BendStiffnesses.TryFind letter with
                                        | Some k -> { b with BendStiffnesses = b.BendStiffnesses |> Map.remove letter |> Map.add newLetter k }
                                        | None -> b)
                            | None -> ())
                    label "stiffness" 60.0
                    textBox $"{key}.bend.{letter}.value" valueWidth (formatFloat kappa) (fun text ->
                        tryParseFloat text |> Option.iter (fun v ->
                            edit (fun b -> { b with BendStiffnesses = b.BendStiffnesses |> Map.add letter v })))
                    smallButton $"{key}.bend.{letter}.remove" "✕" "Remove this bend stiffness" (fun () ->
                        edit (fun b -> { b with BendStiffnesses = b.BendStiffnesses |> Map.remove letter }))
                ]
            row [
                label "" 16.0
                smallButton $"{key}.bend.add" "Add bend stiffness" "Override the bend stiffness at one atom type" (fun () ->
                    edit (fun b ->
                        let letter =
                            ScenarioSpec.atomTypes
                                |> Array.map ScenarioSpec.toLetter
                                |> Array.tryFind (fun l -> not (b.BendStiffnesses.ContainsKey l))
                                |> Option.defaultValue "H"
                        { b with BendStiffnesses = b.BendStiffnesses |> Map.add letter b.BendStiffness }))
            ]
        ]

    let private renderPhysics (spec : ScenarioSpec) dispatch =
        let parms = spec.Parameters
        let edit f = dispatch (Edit (fun s -> { s with Parameters = f s.Parameters }))
        let field key text value set =
            floatField $"physics.{key}" text value (fun v -> edit (set v))
        column [
            heading "Event weights"
            field "moveAtom" "Move an atom" parms.MoveAtomWeight (fun v p -> { p with MoveAtomWeight = v })
            field "toggleStrong" "Toggle a strong bond" parms.ToggleStrongBondWeight (fun v p -> { p with ToggleStrongBondWeight = v })
            field "toggleWeak" "Toggle a weak bond" parms.ToggleWeakBondWeight (fun v p -> { p with ToggleWeakBondWeight = v })
            note "Only their ratios matter."
            heading "Stacking and peeling"
            field "stackEnergy" "Stack energy" parms.StackEnergy (fun v p -> { p with StackEnergy = v })
            infiniteFloatField "physics.peelBarrier" "Peel barrier" parms.PeelBarrier "no peeling" (fun v ->
                edit (fun p -> { p with PeelBarrier = v }))
            infiniteFloatField "physics.capPeelBarrier" "Cap peel barrier" parms.CapPeelBarrier "no cap peeling" (fun v ->
                edit (fun p -> { p with CapPeelBarrier = v }))
            note "Blank for none."
            renderBondType "Strong" _.Strong (fun b p -> { p with Strong = b }) parms.Strong dispatch
            renderBondType "Weak" _.Weak (fun b p -> { p with Weak = b }) parms.Weak dispatch
        ]

    let private scheduleNames = [| "Constant"; "Cycle"; "Vent" |]

    let private renderPhase index (phase : Phase) dispatch =
        let key = $"phase.{index}"
        let edit f =
            dispatch (Edit (fun s ->
                match s.Schedule with
                    | Cycle phases -> { s with Schedule = Cycle (phases |> Array.updateAt index (f phases[index])) }
                    | _ -> s))
        let optText (v : float option) = v |> Option.map formatFloat |> Option.defaultValue ""
        let parseOpt (text : string) =
            if String.IsNullOrWhiteSpace text then Some None
            else tryParseFloat text |> Option.map Some
        row [
            textBox $"{key}.name" 80.0 (phase.Name |> Option.defaultValue "") (fun text ->
                edit (fun p -> { p with Name = if String.IsNullOrWhiteSpace text then None else Some (text.Trim()) }))
            textBox $"{key}.kT" 55.0 (formatFloat phase.Temperature) (fun text ->
                tryParseFloat text |> Option.iter (fun v -> edit (fun p -> { p with Temperature = v })))
            intSpinner $"{key}.sweeps" 120.0 1 1_000_000_000 10_000 phase.NumSweeps (fun n ->
                edit (fun p -> { p with NumSweeps = max 1 n }))
            textBox $"{key}.hydrolysis" 65.0 (optText phase.HydrolysisRate) (fun text ->
                parseOpt text |> Option.iter (fun v -> edit (fun p -> { p with HydrolysisRate = v })))
            textBox $"{key}.capPeel" 65.0 (optText phase.CapPeelBarrier) (fun text ->
                parseOpt text |> Option.iter (fun v -> edit (fun p -> { p with CapPeelBarrier = v })))
            smallButton $"{key}.remove" "✕" "Remove this phase" (fun () ->
                dispatch (Edit (fun s ->
                    match s.Schedule with
                        | Cycle phases when phases.Length > 1 -> { s with Schedule = Cycle (phases |> Array.removeAt index) }
                        | _ -> s)))
        ]

    let private renderVent (vent : Vent) dispatch =
        let edit f =
            dispatch (Edit (fun s ->
                match s.Schedule with
                    | Vent v -> { s with Schedule = Vent (f v) }
                    | _ -> s))
        column [
            intField "vent.x" "Vent x" -10_000 10_000 2 vent.X (fun n -> edit (fun v -> { v with X = n }))
            floatField "vent.hot" "Hot kT (at vent)" vent.HotTemperature (fun t -> edit (fun v -> { v with HotTemperature = t }))
            floatField "vent.cold" "Cold kT" vent.ColdTemperature (fun t -> edit (fun v -> { v with ColdTemperature = t }))
            floatField "vent.floor" "Floor kT (in a pulse)" vent.FloorTemperature (fun t -> edit (fun v -> { v with FloorTemperature = t }))
            intField "vent.period" "Period (world width)" 2 10_000 2 vent.Period (fun n -> edit (fun v -> { v with Period = max 2 n }))
            intField "vent.width" "Reach" 1 10_000 2 vent.Width (fun n -> edit (fun v -> { v with Width = max 1 n }))
            intField "vent.coldSweeps" "Sweeps between pulses" 1 1_000_000_000 10_000 vent.ColdSweeps (fun n -> edit (fun v -> { v with ColdSweeps = max 1 n }))
            intField "vent.hotSweeps" "Sweeps per pulse" 1 1_000_000_000 10_000 vent.HotSweeps (fun n -> edit (fun v -> { v with HotSweeps = max 1 n }))
        ]

    let private renderSchedule (spec : ScenarioSpec) dispatch =
        let index =
            match spec.Schedule with
                | Constant _ -> 0
                | Cycle _ -> 1
                | Vent _ -> 2
        let changeKind newIndex =
            dispatch (Edit (fun s ->
                let firstKT =
                    match s.Schedule with
                        | Constant kT -> kT
                        | Cycle phases -> phases[0].Temperature
                        | Vent v -> v.ColdTemperature
                let schedule =
                    match newIndex with
                        | 0 -> Constant firstKT
                        | 1 ->
                            Cycle [|
                                { Name = Some "cold"; Temperature = firstKT; NumSweeps = 200_000; HydrolysisRate = None; CapPeelBarrier = None }
                                { Name = Some "hot"; Temperature = 3.0; NumSweeps = 100_000; HydrolysisRate = None; CapPeelBarrier = None }
                            |]
                        | _ -> Vent { Scenario.seamVent with Period = s.Width }
                { s with Schedule = schedule }))
        column [
            heading "Schedule"
            row [
                label "Kind" labelWidth
                comboBox "schedule.kind" 120.0 scheduleNames index changeKind
            ]
            match spec.Schedule with
                | Constant kT ->
                    floatField "schedule.kT" "Temperature (kT)" kT (fun v ->
                        dispatch (Edit (fun s -> { s with Schedule = Constant v })))
                | Cycle phases ->
                    row [
                        label "name" 80.0
                        label "kT" 55.0
                        label "sweeps" 120.0
                        label "hydrolysis" 65.0
                        label "cap peel" 65.0
                    ]
                    for i, phase in Array.indexed phases do
                        renderPhase i phase dispatch
                    row [
                        button "schedule.addPhase" "Add phase" (fun () ->
                            dispatch (Edit (fun s ->
                                match s.Schedule with
                                    | Cycle phases ->
                                        let last = phases[phases.Length - 1]
                                        { s with Schedule = Cycle (Array.append phases [| { last with Name = None } |]) }
                                    | _ -> s)))
                    ]
                    note $"""Period {Array.sumBy (fun (p : Phase) -> p.NumSweeps) phases:N0} sweeps. Hydrolysis and cap peel are blank to use the physics' own."""
                | Vent vent -> renderVent vent dispatch
        ]

    /// Wraps the given view in a scroll viewer.
    let private scroll (view : IView) =
        ScrollViewer.create [
            ScrollViewer.content view
            ScrollViewer.padding (Avalonia.Thickness(4.0, 0.0, 16.0, 8.0))
        ]

    /// The editor.
    let render (model : Model) dispatch =
        let spec = model.Spec
        TabControl.create [
            TabControl.viewItems [
                TabItem.create [
                    TabItem.header "World"
                    TabItem.content (scroll (renderWorld spec dispatch))
                ]
                TabItem.create [
                    TabItem.header $"Strands ({spec.Strands.Length})"
                    TabItem.content (scroll (renderStrands spec dispatch))
                ]
                TabItem.create [
                    TabItem.header "Physics"
                    TabItem.content (scroll (renderPhysics spec dispatch))
                ]
                TabItem.create [
                    TabItem.header "Schedule"
                    TabItem.content (scroll (renderSchedule spec dispatch))
                ]
            ]
        ]
            |> View.withKey $"editor {model.LoadCount}"   // a loaded spec gets a fresh editor
