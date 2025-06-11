namespace Ecos.Engine

/// Interaction between two atoms.
[<Struct>]
type Interaction =

    /// Distance between the atoms.
    val Distance : float

    /// Repulsive force between the atoms.
    val Repulsion : Point

    /// Attractive force possible between the atoms.
    val Attraction : Point

    /// Creates an interaction.
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
    let private getForceMagnitudes distance =
        let six = pown distance 6
        let seven = distance * six
        let thirteen = six * seven
        cRepulsion / thirteen,
        cAttraction / seven

    /// Creates an interaction.
    let create atomA atomB =
        let vector = atomA.Location - atomB.Location
        let distance = vector.Length
        let norm = vector / distance
        let repulsion, attraction = getForceMagnitudes distance
        Interaction(distance, norm * repulsion, norm * attraction)

    /// Calculates interaction between every pair of the given
    /// atoms. The result is the lower half of a symmetric lookup
    /// table (up to sign).
    let getInteractions (atoms : _[]) =
        Array.init atoms.Length (fun i ->
            let atom = atoms[i]
            Array.init i (fun j ->
                assert(i >= j)   // lower half of table only
                create atom atoms[j]))

    /// Calculates the force between two interacting atoms.
    let getForce (interaction : Interaction) bound =
        if bound then
            interaction.Repulsion + interaction.Attraction
        else
            interaction.Repulsion
