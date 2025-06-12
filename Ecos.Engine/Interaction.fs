namespace Ecos.Engine

/// Lennard-Jones potential.
type Potential =
    {
        /// Depth of potential energy well.
        Epsilon : float

        /// Equilibrium distance.
        Sigma : float
    }

    /// Maximum distance at which bonding occurs.
    member this.BondDistance =
        2.0 * this.Sigma

module Potential =

    /// Gets the potential for the given atom type indexes.
    let getPotential (potentials : Potential[][]) i j =
        if i >= j then potentials[i][j]
        else potentials[j][i]

/// Interaction between two atoms.
[<Struct>]
type Interaction =

    /// Potential between the atoms
    val Potential : Potential

    /// Distance between the atoms squared.
    val DistanceSquared : float

    /// Repulsive force between the atoms.
    val Repulsion : Point

    /// Attractive force possible between the atoms.
    val Attraction : Point

    /// Creates an interaction.
    new(potential, distanceSquared, repulsion, attraction) =
        {
            Potential = potential
            DistanceSquared = distanceSquared
            Repulsion = repulsion
            Attraction = attraction
        }

module Interaction =

    /// Repulsion and attraction scalars for the given
    /// squared distance. (Lennard-Jones potential.)
    // https://gemini.google.com/app/910afad4050eac2b
    let private getForceScalars potential rSquared =
        let invRSquared = 1.0 / rSquared
        let s2OverR2 =
            potential.Sigma * potential.Sigma * invRSquared
        let s6OverR6 = s2OverR2 * s2OverR2 * s2OverR2           // (sigma/r)^6 = (sigma^2/r^2)^3
        let s12OverR12 = s6OverR6 * s6OverR6                    // (sigma/r)^12 = ((sigma/r)^6)^2
        let commonFactor =
            24.0 * potential.Epsilon * invRSquared
        let repulsiveScalar = 2.0 * commonFactor * s12OverR12   // (48*epsilon/r^2)*(sigma/r)^12
        let attractiveScalar = commonFactor * s6OverR6          // (24*epsilon/r^2)*(sigma/r)^6
        repulsiveScalar, attractiveScalar

    /// Creates an interaction.
    let create potential atomA atomB =
        let displacement = atomA.Location - atomB.Location
        let rSquared = displacement *. displacement
        let repulsion, attraction =
            getForceScalars potential rSquared
        Interaction(
            potential,
            rSquared,
            displacement * repulsion,
            displacement * attraction)

    /// Calculates an interaction between every pair of the given
    /// atoms. The result is the lower half of a symmetric lookup
    /// table (up to sign).
    let getInteractions potentials (atoms : _[]) =
        Array.init atoms.Length (fun i ->
            let atomA = atoms[i]
            Array.init i (fun j ->
                assert(i >= j)   // lower half of table only
                let atomB = atoms[j]
                let potential =
                    Potential.getPotential
                        potentials
                        atomA.Type.Index
                        atomB.Type.Index
                create potential atomA atomB))

    /// Calculates the force between two interacting atoms.
    let getForce (interaction : Interaction) bound =
        if bound then
            interaction.Repulsion - interaction.Attraction
        else
            interaction.Repulsion
