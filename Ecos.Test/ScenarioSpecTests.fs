namespace Ecos.Test

open System
open Xunit
open Ecos
open Ecos.Experiment
open Ecos.Experiment.Experiment3

/// Tests of scenario specs, which the scenario builder app edits, saves
/// and runs.
module ScenarioSpecTests =

    /// Every registered scenario, with its group's name.
    let private registered =
        [|
            for group in Registry.groups do
                for scenario in group.Scenarios do
                    $"{group.Name}: {scenario.Name}", scenario
        |]

    /// Strands of the given world, as (sequence, locations, is ring),
    /// sorted, and the world's walls, sorted.
    let private describe (world : World) =
        let strands =
            getStrands world
                |> Array.map (fun strand ->
                    strand.AtomIds |> Array.map (fun id -> world.Atoms[id].Type),
                    strand.AtomIds |> Array.map (fun id -> world.Atoms[id].Location),
                    strand.IsRing)
                |> Array.sort
        let walls = world.Walls |> Seq.sort |> Seq.toArray
        strands, walls

    /// Number of atoms of each type, sorted by type.
    let private countTypes (world : World) =
        world.Atoms |> Array.countBy _.Type |> Array.sort

    /// Number of weak bonds in the given world.
    let private countWeakBonds (world : World) =
        world.BondTypeMap[Weak] |> Map.toSeq |> Seq.sumBy (snd >> Set.count) |> fun n -> n / 2

    [<Fact>]
    let ``A spec of each registered scenario builds the same strands, walls and atoms`` () =
        for name, scenario in registered do
            let original, originalTemplate = scenario.Build scenario.Parameters (Random(0))
            let spec = ScenarioSpec.ofScenario 0 scenario
            let rebuilt = ScenarioSpec.toScenario spec
            let world, template = rebuilt.Build rebuilt.Parameters (Random(1))
            Assert.True((describe original = describe world), $"{name}: strands or walls differ")
            Assert.True((countTypes original = countTypes world), $"{name}: atom counts differ")
            Assert.True((world.Size = original.Size), $"{name}: sizes differ")
            Assert.True(
                (set (originalTemplate |> Array.map (fun id -> original.Atoms[id].Location))
                    = set (template |> Array.map (fun id -> world.Atoms[id].Location))),
                $"{name}: templates differ")
            let strandWeakBonds (w : World) =   // free atoms' pairings aren't kept
                let inStrands = getStrands w |> Seq.collect _.AtomIds |> set
                w.BondTypeMap[Weak]
                    |> Map.toSeq
                    |> Seq.sumBy (fun (a, bs) ->
                        if inStrands.Contains a then bs |> Set.filter inStrands.Contains |> Set.count
                        else 0)
            Assert.True((strandWeakBonds original = strandWeakBonds world), $"{name}: weak bonds differ")

    [<Fact>]
    let ``A spec's parameters convert both ways unchanged`` () =
        for name, scenario in registered do
            let spec = ScenarioSpec.ofParameters scenario.Parameters
            let parms = ScenarioSpec.toParameters spec
            Assert.True((ScenarioSpec.ofParameters parms = spec), name)
            Assert.True(
                (BondTypeParameterMap.toMap parms.BondTypeParameterMap
                    = BondTypeParameterMap.toMap scenario.Parameters.BondTypeParameterMap),
                $"{name}: bond-type parameters differ")
            Assert.Equal(scenario.Parameters.MoveAtomWeight, parms.MoveAtomWeight)
            Assert.Equal(scenario.Parameters.ToggleStrongBondWeight, parms.ToggleStrongBondWeight)
            Assert.Equal(scenario.Parameters.ToggleWeakBondWeight, parms.ToggleWeakBondWeight)
            Assert.Equal(scenario.Parameters.StackEnergy, parms.StackEnergy)
            Assert.Equal(scenario.Parameters.PeelBarrier, parms.PeelBarrier)
            Assert.Equal(scenario.Parameters.CapPeelBarrier, parms.CapPeelBarrier)

    [<Fact>]
    let ``A spec survives saving as JSON`` () =
        for name, scenario in registered do
            let spec = ScenarioSpec.ofScenario 0 scenario
            let json = ScenarioSpec.toJson spec
            Assert.True((ScenarioSpec.ofJson json = spec), name)

    /// A small hand-made spec: two placed strands, scattered copies of
    /// another, and food in part of the world.
    let private handMade =
        let standard = ScenarioSpec.ofScenario 0 (Scenario.createScenarioC true)
        {
            standard with
                Name = "hand-made"
                Width = 60
                Height = 40
                Walls = [| for x in -10 .. 2 .. 10 -> x, 16 |]
                Strands =
                    [|
                        { Pattern = "PHHWYQ"; Placement = At (-6, 0, true); Count = 1; IsTemplate = true; Color = None }
                        { Pattern = "QXZHHP"; Placement = At (4, 2, false); Count = 1; IsTemplate = false; Color = Some "#00A000" }
                        { Pattern = "PWXQ"; Placement = Scattered (Some 40); Count = 3; IsTemplate = false; Color = None }
                    |]
                WeakBonds = [| (0, 1), (1, 4) |]   // the H at x = -4 in each
                Occupancy = Map [ "W", 100.0 * 5.0 / 256.0; "X", 100.0 * 5.0 / 256.0; "H", 100.0 * 2.0 / 256.0 ]   // of 16 x 16 sites
                FoodWithin = Some 30
                Schedule = Cycle [| { Name = Some "stress"; Temperature = 0.3; NumSweeps = 100; HydrolysisRate = None; CapPeelBarrier = Some Double.PositiveInfinity } |]
        }

    [<Fact>]
    let ``A hand-made spec builds what it says`` () =
        let scenario = ScenarioSpec.toScenario handMade
        let world, template = scenario.Build scenario.Parameters (Random(7))
        Assert.Equal(Point.create (60, 40), world.Size)
        Assert.Equal(11, world.Walls.Length)
        let sequences =
            getStrands world
                |> Array.map (fun s -> s.AtomIds |> Array.map (fun id -> ScenarioSpec.toLetter world.Atoms[id].Type) |> String.concat "")
                |> Array.sort
        Assert.Equal<string[]>([| "PHHWYQ"; "PWXQ"; "PWXQ"; "PWXQ"; "QXZHHP" |], sequences)
        Assert.Equal(6, template.Length)
        Assert.Equal(1, countWeakBonds world)
        let center = Build.center world
        let template0 = world.Atoms[template[0]]
        Assert.Equal(center + Point.create (-6, 0), template0.Location)
        let free = world.Atoms.Length - 6 - 6 - 3 * 4
        Assert.Equal(12, free)

    [<Fact>]
    let ``Building a spec is deterministic for a seed`` () =
        let scenario = ScenarioSpec.toScenario handMade
        let worldA, _ = scenario.Build scenario.Parameters (Random(3))
        let worldB, _ = scenario.Build scenario.Parameters (Random(3))
        Assert.Equal<Point[]>(
            worldA.Atoms |> Array.map _.Location,
            worldB.Atoms |> Array.map _.Location)

    [<Fact>]
    let ``A hand-made spec survives saving as JSON, infinity included`` () =
        Assert.True((ScenarioSpec.ofJson (ScenarioSpec.toJson handMade) = handMade))

    [<Fact>]
    let ``A strand placed where there is no room is an error`` () =
        let spec =
            { handMade with
                Strands =
                    [|
                        { Pattern = "PWQ"; Placement = At (0, 0, true); Count = 1; IsTemplate = false; Color = None }
                        { Pattern = "PWQ"; Placement = At (2, 0, true); Count = 1; IsTemplate = false; Color = None }
                    |]
                WeakBonds = [||] }
        let scenario = ScenarioSpec.toScenario spec
        Assert.ThrowsAny<exn>(fun () -> scenario.Build scenario.Parameters (Random(0)) |> ignore) |> ignore

    [<Fact>]
    let ``A pattern draws sequences of its form`` () =
        let terms = ScenarioSpec.parsePattern "Cap-H-10*Coding-Cap"
        let rng = Random(0)
        let isCap t = t = AtomType.P || t = AtomType.Q
        for _ in 1 .. 50 do
            let types = ScenarioSpec.samplePattern rng terms
            Assert.Equal(13, types.Length)
            Assert.True(isCap types[0] && isCap types[12])
            Assert.Equal(AtomType.H, types[1])
            Assert.True(types[2 .. 11] |> Array.forall (fun t -> not (isCap t) && t <> AtomType.H))
        let drawn = Array.init 200 (fun _ -> ScenarioSpec.samplePattern rng terms) |> Array.collect id |> Array.distinct
        Assert.Equal(7, drawn.Length)   // every type turns up

    [<Fact>]
    let ``Literal runs, sets and repeats combine`` () =
        let types = ScenarioSpec.samplePattern (Random(1)) (ScenarioSpec.parsePattern "P-HH-8*[WY]-Q")
        Assert.Equal(12, types.Length)
        Assert.Equal<AtomType[]>([| AtomType.P; AtomType.H; AtomType.H |], types[0 .. 2])
        Assert.True(types[3 .. 10] |> Array.forall (fun t -> t = AtomType.W || t = AtomType.Y))
        Assert.Equal(AtomType.Q, types[11])
        Assert.Equal<AtomType[]>(
            [| AtomType.P; AtomType.H; AtomType.W; AtomType.Q |],
            ScenarioSpec.samplePattern (Random(2)) (ScenarioSpec.parsePattern "PHWQ"))
        Assert.Equal<AtomType[]>(
            [| AtomType.H; AtomType.H; AtomType.H |],
            ScenarioSpec.samplePattern (Random(2)) (ScenarioSpec.parsePattern "3*H"))

    [<Fact>]
    let ``Invalid patterns are errors`` () =
        for pattern in [ ""; "P--Q"; "PAQ"; "[WY"; "[]"; "x*W"; "-1*W"; "P-Foo-Q"; "[WA]"; "[2-1]*H"; "[1-]*H"; "[1-2*H" ] do
            match ScenarioSpec.tryParsePattern pattern with
                | Ok _ -> failwith $"\"{pattern}\" should be invalid"
                | Error _ -> ()

    [<Fact>]
    let ``Each scattered copy is drawn separately`` () =
        let spec =
            { handMade with
                Strands = [| { Pattern = "P-10*Coding-Q"; Placement = Scattered None; Count = 6; IsTemplate = true; Color = None } |]
                WeakBonds = [||] }
        let scenario = ScenarioSpec.toScenario spec
        let world, template = scenario.Build scenario.Parameters (Random(4))
        Assert.Equal(72, template.Length)
        let sequences =
            getStrands world
                |> Array.map (fun s -> s.AtomIds |> Array.map (fun id -> world.Atoms[id].Type))
        Assert.Equal(6, sequences.Length)
        Assert.True((sequences |> Array.distinct |> Array.length) > 1)

    [<Fact>]
    let ``Occupancy gives each type's share of the sites`` () =
        let spec =
            { handMade with
                Width = 40; Height = 40; FoodWithin = None   // 400 sites
                Strands = [||]; WeakBonds = [||]; Walls = [||]
                Occupancy = Map [ "W", 5.0; "Q", 2.5 ] }
        let scenario = ScenarioSpec.toScenario spec
        let world, _ = scenario.Build scenario.Parameters (Random(0))
        Assert.Equal(20, world.Atoms |> Array.where (fun a -> a.Type = AtomType.W) |> Array.length)
        Assert.Equal(10, world.Atoms |> Array.where (fun a -> a.Type = AtomType.Q) |> Array.length)
        Assert.Equal(30, world.Atoms.Length)

    [<Fact>]
    let ``A repeat count can be a range`` () =
        let terms = ScenarioSpec.parsePattern "P-[1-2]*H-[0-3]*[WY]-Q"
        let rng = Random(0)
        let lengths =
            Array.init 300 (fun _ ->
                let types = ScenarioSpec.samplePattern rng terms
                let nH = types |> Array.where ((=) AtomType.H) |> Array.length
                Assert.InRange(nH, 1, 2)
                Assert.Equal(AtomType.P, types[0])
                Assert.Equal(AtomType.Q, types[types.Length - 1])
                types.Length)
        Assert.Equal<int[]>([| 3 .. 7 |], lengths |> Array.distinct |> Array.sort)

    [<Fact>]
    let ``A strand's family includes its copies, read either way`` () =
        let regex = ScenarioSpec.toRegex (ScenarioSpec.parsePattern "P-HH-[1-2]*[WY]-Q")
        let matches (text : string) = ScenarioSpec.matchesFamily regex (ScenarioSpec.parseSequence text)
        Assert.True(matches "PHHWYQ")      // drawn
        Assert.True(matches "PHHYQ")
        Assert.True(matches "PZXHHQ")      // its copy (complement, reversed)
        Assert.True(matches "QHHXZP")      // the copy read from its other end
        Assert.True(matches "QYWHHP")      // drawn, read from the other end
        Assert.False(matches "PHHWYWQ")    // too long
        Assert.False(matches "PHWYQ")      // missing an H
        Assert.False(matches "PHHXWQ")     // X W is neither drawn nor a copy of a draw read this way
        Assert.False(matches "HHWY")       // uncapped

    [<Fact>]
    let ``A spec saved without strand colors loads with none`` () =
        let json = ScenarioSpec.toJson handMade
        let stripped = Text.RegularExpressions.Regex.Replace(json, "\\s*\"Color\":\\s*(null|\"[^\"]*\"),?", "")
        Assert.DoesNotContain("Color", stripped)
        let spec = ScenarioSpec.ofJson stripped
        Assert.True(spec.Strands |> Array.forall (fun st -> st.Color.IsNone))
