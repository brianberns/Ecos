namespace Ecos.Engine

/// Interaction between two atoms.
[<Struct>]
type Interaction =

    /// Distance between the atoms.
    val Distance : float

    /// Repulsion between the atoms.
    val Repulsion : Point

    /// Possible attraction between the atoms.
    val Attraction : Point

    new(distance, repulsion, attraction) =
        {
            Distance = distance
            Repulsion = repulsion
            Attraction = attraction
        }

module Interaction =

    /// Depth of potential energy well.
    let epsilon = 1.0

    /// Equilibrium distance.
    let sigma = 1.0

    /// Maximum distance at which bonding occurs.
    let bondDistance = 2.0 * sigma

    /// Repulsion numerator.
    let private cRepulsion = 48.0 * epsilon * pown sigma 12

    /// Attraction numerator.
    let private cAttraction = -24.0 * epsilon * pown sigma 6

    /// Repulsion and attraction magnitudes for the given
    /// distance. (Lennard-Jones potential.)
    let getForce distance =
        let six = pown distance 6
        let seven = distance * six
        let thirteen = six * seven
        cRepulsion / thirteen,
        cAttraction / seven

    /// Creates an interaction.
    let create atomA atomB =
        let vector = atomA.Location - atomB.Location
        let distance = vector.Length
        if distance <= bondDistance then
            let norm = vector / distance
            let magRep, magAttr = getForce distance
            Interaction(distance, norm * magRep, norm * magAttr)
        else
            Interaction(distance, Point.zero, Point.zero)

    /// Calculates interaction between every pair of the given
    /// atoms. The result is the lower half of a symmetric lookup
    /// table (up to sign).
    let getInteractions (atoms : _[]) =
        Array.init atoms.Length (fun i ->
            let atom = atoms[i]
            Array.init i (fun j ->
                assert(i >= j)   // lower half of table only
                create atom atoms[j]))
