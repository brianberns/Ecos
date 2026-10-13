# Ecos

Ecos is an attempt to create digital life from simple chemistry. It simulates atoms on a 2D lattice that move, bond into strands, and copy those strands by pairing with a template. The goal is self-replication from raw materials that is robust enough for evolution by natural selection.

Replication is not scripted. The rules describe only local physics and chemistry: how atoms move, which bonds can form, and what they cost. Anything that looks like copying has to emerge from those rules.

## Where it stands

A strand placed among free atoms is copied, the copy separates from it when heated, and the number of complete strands grows from one temperature cycle to the next. That growth is steady, not exponential, so this is templated copying with heredity but not yet a population that doubles.

| Milestone | Status |
|---|---|
| Polymer physics that matches exact enumeration | Done |
| Free atoms pair with a template strand | Done |
| Paired atoms join into a copy, and only on a template | Done |
| Copy separates from the template when heated | Partly: needs a long hot phase |
| Template and copies survive repeated cycles unchanged | Done |
| Complete strands increase every cycle | Done, but about one or two per cycle, not doubling |
| Exponential growth | Not yet |
| Sequences that differ in fitness, and selection between them | Not started |
| A phenotype beyond a strand's own copying rate | Not started |

## The model

### Atoms and the lattice

- The world is a 2D square lattice. Each atom occupies a 2×2 block of sites, and no two atoms may overlap.
- Atoms are never created or destroyed, so mass is conserved.
- Walls are immovable blocks the size of an atom, used to enclose a world.
- There are six atom types. W, X, Y and Z are the coding atoms that make up a strand's sequence. P and Q are caps that end a strand.

### Bonds

| | Strong bonds | Weak bonds |
|---|---|---|
| Role | Join atoms along a strand | Pair atoms on different strands |
| Which atoms | Any two, except two caps | Complementary types only: W–X, Y–Z, P–Q |
| Range | Longer (up to about 3.6 sites) | Shorter (up to about 2.8 sites) |
| Limit per atom | Two, or one for a cap | Two, and the second only for an atom in a strand |
| Angle rule | Stiff: at most about 37° of bend at each atom | An atom's two partners must be on opposite sides of it |
| Energy and barrier | Large energy, large barrier | Small energy, no barrier |

No bond of either type may cross another.

### Dynamics

Time advances by single random events, accepted or rejected by the Metropolis rule at temperature kT:

- **Hop:** an atom moves one site, if that leaves it clear of other atoms and keeps all its bonds legal.
- **Toggle:** a bond between two atoms in range forms if absent or breaks if present. It must first cross the bond type's barrier and is then accepted according to its energy.

One *sweep* is one attempted event per atom.

### What makes copying happen

- **Stacks.** When two neighboring atoms of one strand are each paired with neighboring atoms of another, the two pairs form a stack. A stack adds binding energy.
- **Templated joining.** A stack also removes the barrier to the strong bond beside it. Two free atoms sitting side by side on a template therefore join readily, while two free atoms meeting anywhere else almost never do. This is the only route by which strands grow.
- **Caps.** A cap can have only one strong bond, so a strand that ends in caps cannot be extended. A cap is copied like any other atom, by pairing with its complement, so strand length is inherited.
- **Peeling.** A strong bond at an uncapped end of a strand breaks at a rate that rises with temperature. Unfinished strands melt back into free atoms during the hot phase, and finished, capped strands are untouched.
- **Temperature cycling.** A cold phase lets free atoms pair with strands and join into copies. A hot phase breaks the weak bonds so strands separate, and peels away the fragments.

Every rule except peeling obeys detailed balance, so at a fixed temperature the model would settle into thermal equilibrium. Peeling is deliberately one-way. Together with the temperature cycle, it is what keeps the system out of equilibrium, with atoms flowing from the free pool into strands and back.

## What we learned getting here

Each rule above was added to fix a specific failure that showed up in measurement:

- **Flexible strands with two-sided pairing collapsed into globules.** Stiffness fixed this, and it had to be a limit on the angle itself; a limit on the raw dot product allowed sharp bends wherever bonds were stretched.
- **Free atoms kept switching roles between strand and partner.** Allowing a second weak partner only for an atom already in a strand fixed this.
- **Random joining during the hot phase.** A high barrier that only a stack removes fixed this.
- **Strands grew longer every cycle.** Measurement showed free atoms being added to strand ends, templated by a partner strand that had slid out of alignment. Caps fixed this.
- **Fragments accumulated and used up the free atoms,** and many were completed into three-atom capped strands that then multiplied faster than full-length ones. Peeling removes the fragments and cut the short strands more than tenfold.

Two ideas were tried and dropped because measurement did not support them: requiring two strong bonds for a second weak partner, and four-letter templates.

## Open problems

- **Strands stick together when hot.** A weak bond between two atoms in range exists at least half the time at any temperature, because pairing has an energy but no entropy cost. Strands separate only slowly, and a stack of strands can add copies only on its two outer faces, which is why growth is steady and not doubling. The planned fix is an entropy term for weak bonds, giving them a true melting temperature.
- **Copies start in too many places at once.** Pairing next to a growing copy is no easier than pairing anywhere else, so each template produces many fragments and few complete copies. The planned fix is a reward for neighboring pairs.
- **Short strands have an advantage.** Nothing yet favors a long strand over a short one, so the shortest replicator tends to win. This needs sequences to matter.
- **No phenotype yet.** The candidates are different pairing strengths for the two letter pairs, strands that fold into shapes that do something, and closed rings that act as compartments.

## Repository layout

| Project | Contents |
|---|---|
| `Ecos` | The core library: atoms, bonds, the world, and the update rule |
| `Ecos.Desktop` | A desktop app (Avalonia) for watching a scenario run |
| `Ecos.Experiment` | Scenarios shared with the desktop app, and console experiments that measure them |
| `Ecos.Test` | Unit and property tests |
| `Avalonia.FuncUI` | Git submodule used by the desktop app |

## Building and running

The solution targets .NET 10. Clone with submodules:

```bash
git clone --recurse-submodules https://github.com/brianberns/Ecos
```

Run the desktop app:

```bash
dotnet run -c Release --project Ecos.Desktop
```

Run the tests:

```bash
dotnet test -c Release
```

Run an experiment, optionally naming a CSV file for the results:

```bash
dotnet run -c Release --project Ecos.Experiment -- experiment3 results.csv
```

| Experiment | Question |
|---|---|
| `experiment1` | What fraction of a template is paired at each temperature? |
| `experiment2` | How much faster do free atoms join when a template is present? |
| `experiment3` | Under temperature cycling, do copies form, separate intact, and multiply? |

Experiment 3 takes roughly ten minutes. It reports, for each phase of each cycle, the number of strands, complete copies and capped strands, what was joined to what, and the lengths of the capped strands.

## How the model is tested

- **Exact enumeration.** For systems small enough to list every state, the simulation's statistics are compared with the exact Boltzmann distribution.
- **Independent oracles.** Each rule is restated separately in the tests, and the library is checked against that statement, exhaustively where the cases are few and by random worlds otherwise.
- **Event rates.** In crowded random worlds, each kind of event must happen as often as the rules predict.
- **Mutation checks.** Each new test is run against deliberately broken copies of the library, to confirm that it fails when it should.

## Background

The lattice model is the Bond Fluctuation Model of polymer physics, with Metropolis dynamics. The approach follows Tim Hutton's artificial chemistry work on self-replicating molecules, with the aim of more realistic physics and less built-in machinery. Growth by layering and fragmentation is related to the crystal-based replicators of Schulman, Yurke and Winfree. The victory of short replicators echoes Spiegelman's experiments with RNA.

The core library is written by Brian Berns. The tests and experiments are written with Claude.
