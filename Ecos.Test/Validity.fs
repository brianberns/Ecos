namespace Ecos.Test

open Ecos
open Ecos.Test.Geometry

/// Oracles for a world's validity: independent statements of the rules
/// that every world must obey, checked against World by the tests.
module Validity =

    /// Oracle: do atoms at the given locations overlap? Each atom
    /// occupies a 2x2 square, so they overlap if they are closer than
    /// 2 along both axes, measured around the torus.
    let overlaps (world : World) (a : Point) (b : Point) =
        let d = Grid.getDisplacement a b world.Cells
        abs d.X < 2 && abs d.Y < 2

    /// Oracle: is the given vector an allowed bond?
    let isBondVector (v : Point) =
        Array.contains v Chain.bondVectors

    /// Are the given atoms strongly bonded?
    let isStronglyBonded = Bonds.areStronglyBonded

    /// Strong bonds in the given world, as (smaller ID, larger ID)
    /// pairs.
    let strongBonds = Bonds.strongBonds

    /// Weak bonds in the given world, as (smaller ID, larger ID) pairs.
    let weakBonds = Bonds.weakBonds

    /// All bonds in the given world, strong and weak, as
    /// (smaller ID, larger ID) pairs.
    let allBonds (world : World) =
        Set.toList (strongBonds world) @ Set.toList (weakBonds world)

    /// Gets the displacement between the atoms with the given IDs in
    /// the given world.
    let delta idA idB (world : World) =
        World.getDisplacement world.Atoms[idA] world.Atoms[idB] world

    /// Oracle: would a bond between the given atoms cross any bond in
    /// the given world? Bonds that share an atom with it can't cross
    /// it. This checks every bond in the world, so it doesn't depend on
    /// World's assumption that only nearby bonds need checking.
    let crossesAnyBond (world : World) idA idB =
        allBonds world
            |> List.exists (fun (idC, idD) ->
                idC <> idA && idC <> idB && idD <> idA && idD <> idB
                    && (let rel id = delta idA id world   // relative to A, around the torus
                        let c = rel idC
                        segmentsIntersect Point.zero (rel idB) c (c + delta idC idD world)))

    /// The given atom's partners by bonds of the given type.
    let partners bondType (world : World) id =
        world.BondTypeMap[bondType]
            |> BondMap.getBonds id

    /// Oracle: is the angle between the given atom's strong bonds
    /// allowed, with the bend limit in the given parameters, when the
    /// atoms are at the locations given by the given function? (Only an
    /// atom with two strong bonds can fail this.) This keeps strands
    /// from bending too sharply.
    let anglesAreAllowed (parms : Parameters) (world : World) (loc : AtomId -> Point) id =
        let limit = parms.BondTypeParameterMap[Strong].BendLimit
        let ids = Set.toList (partners Strong world id)
        List.allPairs ids ids
            |> List.forall (fun (idB, idC) ->
                idB >= idC
                    || Bonds.isBendAllowed limit Point.zero
                        (Grid.getDisplacement (loc id) (loc idB) world.Cells)   // loc may place the atom elsewhere
                        (Grid.getDisplacement (loc id) (loc idC) world.Cells))

    /// Oracle: would the angle between the given atom's strong bonds
    /// still be allowed, with the given parameters, if it also had a
    /// strong bond to the given new partner?
    let hasRoomAtAngle (parms : Parameters) (world : World) id idNew =
        let limit = parms.BondTypeParameterMap[Strong].BendLimit
        partners Strong world id
            |> Set.forall (fun idB ->
                Bonds.isBendAllowed limit Point.zero
                    (delta id idB world)
                    (delta id idNew world))

    /// A view of a world's strands, for the face oracles below: each
    /// atom's strong partners, its successor (if any), and its location.
    /// To decide whether an event is allowed, the oracles are given a
    /// view of the world as the event would leave it.
    type Strands =
        {
            /// Strong partners of each atom.
            Partners : AtomId -> Set<AtomId>

            /// Successor of each atom, if any.
            Successor : AtomId -> AtomId option

            /// Location of each atom.
            Location : AtomId -> Point
        }

    /// The strands of the given world, as they are.
    let strandsOf (world : World) =
        {
            Partners = partners Strong world
            Successor = Bonds.successor world
            Location = fun id -> world.Atoms[id].Location
        }

    /// The given strands, with the given atom at the given location.
    let withLocation id loc (strands : Strands) =
        { strands with
            Location = fun idC -> if idC = id then loc else strands.Location idC }

    /// The given strands, with a strong bond from atom A to atom B.
    let withBond idA idB (strands : Strands) =
        { strands with
            Partners = fun id ->
                let ids = strands.Partners id
                if id = idA then ids.Add idB
                elif id = idB then ids.Add idA
                else ids
            Successor = fun id ->
                if id = idA then Some idB
                else strands.Successor id }

    /// The given strands, without the strong bond between atoms A and
    /// B, whichever way it runs.
    let withoutBond idA idB (strands : Strands) =
        { strands with
            Partners = fun id ->
                let ids = strands.Partners id
                if id = idA then ids.Remove idB
                elif id = idB then ids.Remove idA
                else ids
            Successor = fun id ->
                match strands.Successor id with
                    | Some idS when set [ id; idS ] = set [ idA; idB ] -> None
                    | succOpt -> succOpt }

    /// Oracle: is atom B on atom A's face, in the given world with the
    /// given strands? A's face is to the left of its strand's direction
    /// (see Bonds.isOnFace). An atom that isn't in a strand has no
    /// face, and pairs on any side.
    let isOnFace (world : World) (strands : Strands) idA idB =
        let ids = strands.Partners idA
        if ids.IsEmpty then true
        else
            let succOpt = strands.Successor idA
            let predOpt = ids |> Seq.tryFind (fun id -> Some id <> succOpt)
            let rel id =   // relative to A, around the torus
                Grid.getDisplacement (strands.Location idA) (strands.Location id) world.Cells
            let direction =
                Bonds.strandDirection (Option.map rel predOpt) Point.zero (Option.map rel succOpt)
            Bonds.isOnFace direction (rel idB)

    /// Oracle: is each atom of every weak bond in the given world on
    /// the other's face, with the given strands?
    let facesAreValid (world : World) strands =
        weakBonds world
            |> Set.forall (fun (idA, idB) ->
                isOnFace world strands idA idB
                    && isOnFace world strands idB idA)

    /// Checks all of a world's invariants, with the bend limits in the
    /// given parameters.
    let isValid parms (world : World) =
        let atoms = world.Atoms |> Seq.toArray
        let loc id = world.Atoms[id].Location

            // no two atoms overlap, no atom overlaps a wall, and no two
            // walls overlap (a wall occupies the same square as an atom)
        let noOverlaps =
            atoms
                |> Seq.forall (fun a ->
                    atoms
                        |> Array.forall (fun b ->
                            a.Id = b.Id || not (overlaps world a.Location b.Location))
                        && world.Walls
                            |> Seq.forall (fun wall -> not (overlaps world a.Location wall)))
                && world.Walls
                    |> Seq.forall (fun wallA ->
                        world.Walls
                            |> Seq.forall (fun wallB ->
                                wallA = wallB || not (overlaps world wallA wallB)))

            // every strong bond is allowed and symmetric, no atom has more
            // than its type permits (one for a cap, otherwise two), no two
            // caps are bonded, and two bonds at one atom don't bend too
            // sharply
        let strongBondsValid =
            (Bonds.strongBondMap world)
                |> Map.forall (fun idA ids ->
                    ids.Count <= Chain.strongValence world.Atoms[idA].Type
                        && anglesAreAllowed parms world loc idA
                        && ids
                            |> Set.forall (fun idB ->
                                isBondVector (delta idA idB world)
                                    && Chain.canJoin world.Atoms[idA].Type world.Atoms[idB].Type
                                    && isStronglyBonded world idB idA))

            // no atom is listed with an empty set of bonds
        let bondSetsValid =
            world.BondTypeMap.BondMaps
                |> Seq.forall (Map.forall (fun _ ids -> not ids.IsEmpty))

            // no bond, strong or weak, crosses another
        let noCrossings =
            allBonds world
                |> List.forall (fun (idA, idB) ->
                    not (crossesAnyBond world idA idB))

            // every weak bond is symmetric, in range, between
            // complementary types, and between atoms that aren't also
            // strongly bonded
        let weakBondsValid =
            (Bonds.weakBondMap world)
                |> Map.forall (fun idA ids ->
                    ids
                        |> Set.forall (fun idB ->
                            Bonds.areWeaklyBonded world idB idA
                                && Array.contains (delta idA idB world) Chain.weakBondVectors
                                && Chain.canPair world.Atoms[idA].Type world.Atoms[idB].Type
                                && not (isStronglyBonded world idA idB)))

            // every strong bond runs one way: one of its atoms is the
            // other's predecessor, and not the other way round; every
            // direction is that of a strong bond; and no atom has two
            // predecessors (nor two successors, which the map can't hold)
        let directionsValid =
            let directed = Bonds.directedStrongBonds world
            strongBonds world
                |> Set.forall (fun (idA, idB) ->
                    directed.Contains (idA, idB) <> directed.Contains (idB, idA))
                && directed
                    |> Set.forall (fun (idA, idB) -> isStronglyBonded world idA idB)
                && directed
                    |> Seq.countBy snd
                    |> Seq.forall (fun (_, n) -> n = 1)

            // no atom has more than one weak partner, and each atom of a
            // weak bond is on the other's face
        let weakPartnersValid =
            (Bonds.weakBondMap world)
                |> Map.forall (fun _ ids -> ids.Count <= 1)
                && facesAreValid world (strandsOf world)

            // every atom is stored under its own ID, and the world's cells
            // say exactly where the atoms and walls are: each atom's and
            // wall's cell holds it, and every other cell in the region they
            // occupy (with a margin) is empty
        let occupancyConsistent =
            let walls = set world.Walls
            let locations =
                Array.append (atoms |> Array.map _.Location) world.Walls
            let regionConsistent () =
                let xs = locations |> Array.map _.X
                let ys = locations |> Array.map _.Y
                Seq.allPairs
                    [ Array.min xs - 2 .. Array.max xs + 2 ]
                    [ Array.min ys - 2 .. Array.max ys + 2 ]
                    |> Seq.forall (fun (x, y) ->
                        let loc = Point.create (x, y)
                        match world[loc] with
                            | Cell.Atom id ->
                                id < atoms.Length && atoms[id].Location = world.NormalizeLocation(loc)
                            | Cell.Wall -> walls.Contains (world.NormalizeLocation(loc))
                            | Cell.Empty -> true)
            atoms
                |> Array.indexed
                |> Array.forall (fun (i, atom) ->
                    atom.Id = i && world[atom.Location] = Cell.Atom atom.Id)
                && world.Walls
                    |> Array.forall (fun loc -> world[loc] = Cell.Wall)
                && (locations.Length = 0 || regionConsistent ())

        noOverlaps && strongBondsValid && directionsValid && bondSetsValid
            && noCrossings && weakBondsValid && weakPartnersValid
            && occupancyConsistent
