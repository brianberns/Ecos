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
                    spinner $"food.{letter}" 110.0 0m 100m 0.1m "0.0##" (toDecimal (getOccupancy spec letter)) (fun v ->
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
                        if abs (v - total) >= 0.05 then dispatch (ScaleOccupancy v)), SubPatchOptions.OnChangeOf (box ("food.total", total)))
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
            Border.borderBrush lineBrush
            Border.borderThickness 1.0
            Border.cornerRadius 4.0
            Border.margin (Avalonia.Thickness(4.0, 4.0))
            Border.padding 4.0
            Border.child (
                column [
                    row [
                        textBox $"{key}.pattern" 270.0 strand.Pattern (fun text ->
                            edit (fun st -> { st with Pattern = text.Trim() }))
                        label "x" 10.0
                        intSpinner $"{key}.count" 100.0 0 100_000 1 strand.Count (fun n ->
                            edit (fun st -> { st with Count = n }))
                        smallButton $"{key}.remove" "✕" "Remove this strand" (fun () ->
                            dispatch (Edit (fun s ->
                                { s with
                                    Strands = s.Strands |> Array.removeAt index
                                    WeakBonds = [||] })))
                    ]
                    row [
                        checkBox $"{key}.template" "Template" strand.IsTemplate (fun flag ->
                            edit (fun st -> { st with IsTemplate = flag }))
                        label "Color" 40.0
                        // a color without a name (e.g. from a hand-edited spec) is offered by its hex string
                        let sameColor a b =
                            match a, b with
                                | Some (a : string), Some b -> a.Equals(b, StringComparison.OrdinalIgnoreCase)
                                | None, None -> true
                                | _ -> false
                        let colors =
                            if strandColors |> Array.exists (fun (_, c) -> sameColor c strand.Color) then strandColors
                            else Array.append strandColors [| strand.Color.Value, strand.Color |]
                        let colorIndex =
                            colors |> Array.findIndex (fun (_, c) -> sameColor c strand.Color)
                        comboBox $"{key}.color" 110.0 (Array.map fst colors) colorIndex (fun i ->
                            edit (fun st -> { st with Color = snd colors[i] }))
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
            heading "Help"
            note "Each copy is drawn from its pattern and scattered at random, straight. A color shows the strong bonds of every chain in the strand's family: any chain the pattern could draw, or a complete copy of one."
            note "A pattern is terms separated by \"-\": atom letters (PHH), a set of which one is drawn ([WY]), Coding (one of WXYZ) or Cap (P or Q). A term can repeat a fixed number of times (8*[WY]) or a random number in a range ([1-2]*H). E.g. Cap-H-10*Coding-Cap."
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
        let field id text tip value set =
            floatField $"{key}.{id}" text value (fun v -> edit (fun b -> set v b))
                |> tipped tip
        let isWeak = (name = "Weak")
        let noChain = if isWeak then "\n\nNo effect on weak bonds: an atom has at most one, so they never form a chain." else ""

            // rows that override a value for pairs of atom types, with a
            // button to add one (starting at the given default)
        let pairRows id (what : string) tip (map : Map<string, float>) (setMap : Map<string, float> -> BondTypeSpec -> BondTypeSpec) getMap getDefault =
            [
                for pairKey, value in Map.toSeq map do
                    tipped tip <| row [
                        label "" 16.0
                        textBox $"{key}.{id}.{pairKey}.key" 60.0 pairKey (fun text ->
                            match ScenarioSpec.tryParsePairKey text with
                                | Some (a, b) ->
                                    let newKey = ScenarioSpec.toPairKey a b
                                    edit (fun b ->
                                        let map : Map<string, float> = getMap b
                                        match map.TryFind pairKey with
                                            | Some x -> setMap (map |> Map.remove pairKey |> Map.add newKey x) b
                                            | None -> b)
                                | None -> ())
                        label what 90.0
                        textBox $"{key}.{id}.{pairKey}.value" valueWidth (formatFloat value) (fun text ->
                            tryParseFloat text |> Option.iter (fun v ->
                                edit (fun b -> setMap (getMap b |> Map.add pairKey v) b)))
                        smallButton $"{key}.{id}.{pairKey}.remove" "✕" $"Remove this pair {what}" (fun () ->
                            edit (fun b -> setMap (getMap b |> Map.remove pairKey) b))
                    ]
                row [
                    label "" 16.0
                    smallButton $"{key}.{id}.add" $"Add pair {what}" $"Override the {what} of one pair of atom types" (fun () ->
                        edit (fun b ->
                            let map : Map<string, float> = getMap b
                            let newKey =
                                pairChoices
                                    |> Array.tryFind (fun k -> not (map.ContainsKey k))
                                    |> Option.defaultValue "H-H"
                            setMap (map |> Map.add newKey (getDefault b)) b))
                ]
            ]
        let getPairStartBarriers (b : BondTypeSpec) = b.PairStartBarriers |> Option.defaultValue Map.empty
        let setPairStartBarriers (map : Map<string, float>) (b : BondTypeSpec) =
            { b with PairStartBarriers = if map.IsEmpty then None else Some map }

        column [
            heading $"{name} bonds"
            field "energy" "Energy"
                "E: how much lower in energy the bonded state is than the unbonded one. With entropy s, the odds of bonded vs unbonded are e^(E/kT − s). Each stack adds the stack energy, and a bend subtracts its bend energy."
                bond.Energy (fun v b -> { b with Energy = v })
            yield! pairRows "pair" "energy"
                "Energy of bonds between these two atom types, in place of the default energy above."
                bond.PairEnergies (fun map b -> { b with PairEnergies = map }) (fun b -> b.PairEnergies) (fun b -> b.Energy)
            field "barrier" "Barrier"
                "Height of the hump a bond must cross to form or to break. It slows both directions by the same factor, e^(−barrier/kT), so it changes how fast bonds change, not which state is favored. Heat makes it easier to cross."
                bond.Barrier (fun v b -> { b with Barrier = v })
            field "stackReduction" "Barrier reduction per stack"
                "How much each stack lowers the barrier (never below 0). A stack is a neighboring bond that this bond would line up with, e.g. a strong bond whose atoms are weak-paired to a template's bonded atoms. This is what makes templated joins fast."
                bond.BarrierStackReduction (fun v b -> { b with BarrierStackReduction = v })
            infiniteFloatField $"{key}.startBarrier" "Start barrier" bond.StartBarrier "no strand can start"
                (fun v -> edit (fun b -> { b with StartBarrier = v }))
                |> tipped "Extra barrier for a bond between two atoms that have no other bond of this type, i.e. one that starts (or is all of) a strand. It applies to forming and to breaking such a bond. Leave it blank for infinity: no strand can start, except between pairs given their own start barrier below."
            yield! pairRows "startPair" "start barrier"
                "Start barrier for a bond between these two atom types, in place of the default start barrier above. E.g. with the default at inf (no strand can start) and H-H at 0, strands start only from a pair of H atoms, a primer."
                (getPairStartBarriers bond) setPairStartBarriers getPairStartBarriers (fun b -> b.StartBarrier)
            field "entropy" "Entropy"
                "s: the entropy lost when this bond forms (the bonded state has e^s times fewer arrangements). It lifts the bonded state's free energy by kT·s, so the bond melts above kT = energy/s. With 0, the bond never melts."
                bond.Entropy (fun v b -> { b with Entropy = v })
            field "hydrolysis" "Hydrolysis rate"
                "Chance that a bond of this type breaks outright, regardless of energy and barrier, each time it is picked for a toggle attempt. It doesn't depend on temperature. A phase can override it for strong bonds."
                bond.HydrolysisRate (fun v b -> { b with HydrolysisRate = v })
            field "protectedHydrolysis" "Protected hydrolysis rate"
                ("The hydrolysis rate for a bond that is protected: a strong bond that is part of a stack, e.g. in a paired double strand."
                    + (if isWeak then "\n\nNo effect on weak bonds: they are never protected." else ""))
                bond.ProtectedHydrolysisRate (fun v b -> { b with ProtectedHydrolysisRate = v })
            tipped ("Sharpest bend allowed where two bonds of this type meet at an atom: the cosine of the angle between the two bonds must be at most this. 0/1 allows bends up to a right angle, -4/5 only nearly straight, and 1/1 any bend." + noChain) <| row [
                label "Bend limit (cosine)" labelWidth
                textBox $"{key}.bendLimit" valueWidth (formatRational bond.BendLimit) (fun text ->
                    tryParseRational text |> Option.iter (fun r -> edit (fun b -> { b with BendLimit = r })))
            ]
            field "bendStiffness" "Bend stiffness"
                ("κ: the energy cost of a bend where two bonds of this type meet at an atom, κ·(1 + cos φ) for an angle φ between the bonds. It's 0 when straight and 2κ when folded back, and it's taken off the bond's energy." + noChain)
                bond.BendStiffness (fun v b -> { b with BendStiffness = v })
            for letter, kappa in Map.toSeq bond.BendStiffnesses do
                tipped "Bend stiffness at atoms of this type, in place of the default bend stiffness above." <| row [
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
        let field key text tip value set =
            floatField $"physics.{key}" text value (fun v -> edit (set v))
                |> tipped tip
        column [
            heading "Event weights"
            field "moveAtom" "Move an atom"
                "How often an event tries to move an atom one step, relative to the other events. Free atoms and strands spread by these moves."
                parms.MoveAtomWeight (fun v p -> { p with MoveAtomWeight = v })
            field "toggleStrong" "Toggle a strong bond"
                "How often an event picks an atom and a neighbor and tries to make the strong bond between them (if unbonded) or break it (if bonded), relative to the other events."
                parms.ToggleStrongBondWeight (fun v p -> { p with ToggleStrongBondWeight = v })
            field "toggleWeak" "Toggle a weak bond"
                "How often an event picks an atom and a neighbor and tries to make or break the weak bond between them, relative to the other events."
                parms.ToggleWeakBondWeight (fun v p -> { p with ToggleWeakBondWeight = v })
            note "Only their ratios matter."
            heading "Stacking and peeling"
            field "stackEnergy" "Stack energy"
                "Energy added to a bond for each stack it is part of, i.e. each neighboring bond it lines up with. It deepens the bonded state, so paired double strands hold together and stacked pairs melt at a higher kT than lone ones."
                parms.StackEnergy (fun v p -> { p with StackEnergy = v })
            infiniteFloatField "physics.peelBarrier" "Peel barrier" parms.PeelBarrier "no peeling" (fun v ->
                edit (fun p -> { p with PeelBarrier = v }))
                |> tipped "A strong bond at the end of a strand (a non-cap atom with no other strong bond) can also break with chance e^(−barrier/kT) per attempt, with no energy test. Blank: strands never peel."
            infiniteFloatField "physics.capPeelBarrier" "Cap peel barrier" parms.CapPeelBarrier "no cap peeling" (fun v ->
                edit (fun p -> { p with CapPeelBarrier = v }))
                |> tipped "A strong bond to a cap (P or Q) that isn't in a stack can also break with chance e^(−barrier/kT) per attempt, with no energy test. A phase can override it. Blank: caps never peel."
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
            textBox $"{key}.name" 72.0 (phase.Name |> Option.defaultValue "") (fun text ->
                edit (fun p -> { p with Name = if String.IsNullOrWhiteSpace text then None else Some (text.Trim()) }))
            textBox $"{key}.kT" 55.0 (formatFloat phase.Temperature) (fun text ->
                tryParseFloat text |> Option.iter (fun v -> edit (fun p -> { p with Temperature = v })))
            intSpinner $"{key}.sweeps" 140.0 1 1_000_000_000 10_000 phase.NumSweeps (fun n ->
                edit (fun p -> { p with NumSweeps = max 1 n }))
            textBox $"{key}.hydrolysis" 60.0 (optText phase.HydrolysisRate) (fun text ->
                parseOpt text |> Option.iter (fun v -> edit (fun p -> { p with HydrolysisRate = v })))
            textBox $"{key}.capPeel" 60.0 (optText phase.CapPeelBarrier) (fun text ->
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
                        label "name" 72.0 |> tipped "Name of the phase, shown in the status bar. Optional."
                        label "kT" 55.0 |> tipped "Temperature of the phase, in energy units (kT)."
                        label "sweeps" 140.0 |> tipped "How long the phase lasts. In a sweep, each atom gets about one event on average."
                        label "hydrolysis" 60.0 |> tipped "Hydrolysis rate of unprotected strong bonds during this phase, in place of the Physics tab's. Blank: use the Physics tab's."
                        label "cap peel" 60.0 |> tipped "Cap peel barrier during this phase, in place of the Physics tab's. Blank: use the Physics tab's."
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
