namespace Ecos.Engine

/// Interaction between two atoms.
[<Struct>]
type Interaction =

    /// Distance between the atoms squared.
    val DistanceSquared : float

    /// Repulsive force between the atoms.
    val Repulsion : Point

    /// Attractive force possible between the atoms.
    val Attraction : Point

    /// Creates an interaction.
    new(distanceSquared, repulsion, attraction) =
        {
            DistanceSquared = distanceSquared
            Repulsion = repulsion
            Attraction = attraction
        }

module Interaction =

    /// Repulsion and attraction scalars for the given
    /// squared distance. (Lennard-Jones potential.)
    // https://gemini.google.com/app/910afad4050eac2b
    let private getForceScalars sigma epsilon rSquared =
        let invRSquared = 1.0 / rSquared
        let s2OverR2 = sigma * sigma * invRSquared
        let s6OverR6 = s2OverR2 * s2OverR2 * s2OverR2           // (sigma/r)^6 = (sigma^2/r^2)^3
        let s12OverR12 = s6OverR6 * s6OverR6                    // (sigma/r)^12 = ((sigma/r)^6)^2
        let commonFactor = 24.0 * epsilon * invRSquared
        let repulsiveScalar = 2.0 * commonFactor * s12OverR12   // (48*epsilon/r^2)*(sigma/r)^12
        let attractiveScalar = commonFactor * s6OverR6          // (24*epsilon/r^2)*(sigma/r)^6
        repulsiveScalar, attractiveScalar

    /// Depth of potential energy well.
    let epsilon = 1.0

    /// Equilibrium distance.
    let sigma = 1.0

    /// Maximum distance at which bonding occurs.
    let bondDistance = 2.0 * sigma

    /// Square of the bond distance.
    let bondDistanceSqured = bondDistance * bondDistance

    /// Creates an interaction.
    let create atomA atomB =
        let displacement = atomA.Location - atomB.Location
        let rSquared = displacement *. displacement
        let repulsion, attraction =
            getForceScalars sigma epsilon rSquared
        Interaction(
            rSquared,
            displacement * repulsion,
            displacement * attraction)

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
            interaction.Repulsion - interaction.Attraction
        else
            interaction.Repulsion
