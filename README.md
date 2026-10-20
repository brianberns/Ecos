# Ecos

Ecos is an attempt to create digital life from simple chemistry. It simulates atoms on a 2D lattice that move, bond into strands, and copy those strands by pairing with a template. The goal is self-replication from raw materials that is robust enough for evolution by natural selection.

Replication is not scripted. The rules describe only local physics and chemistry: how atoms move, which bonds can form, and what they cost. Anything that looks like copying has to emerge from those rules.

![A world after five temperature cycles](images/ecosystem.png)

*A world after five cold/hot cycles, during the sixth cold phase. It began with one 12-atom strand (the filled circles, centre) among free atoms. Every other strand is a descendant: a copy, or a copy of a copy. Black lines are strong bonds along a strand, pink lines are weak bonds between paired atoms, and the black and grey circles are the caps at each end of a strand.*

## Where it stands

A single strand placed among free atoms is copied, the copies separate when heated and are copied in turn, and the population grows until the world is full. One strand becomes about 3, then 6, then 9 or 10 over three cycles, after which space and free atoms run short.

Replicators also arise without any template. In a soup of free atoms, short capped strands occasionally form by chance during the hot phase, and once one exists it is copied and its lineage multiplies. This is the model's version of abiogenesis.

Strands also die: in the heat a cap sometimes comes off, and the uncapped strand then falls apart into free atoms. With births and deaths, a full world keeps turning over, and lineages compete. Natural selection now works on a heritable trait: when one letter pair binds more strongly than the other, strands made of it take over the population.

| Milestone | Status |
|---|---|
| Polymer physics that matches exact enumeration | Done |
| Free atoms pair with a template strand | Done |
| Paired atoms join into a copy, and only on a template | Done |
| Copy separates cleanly from the template when heated | Done |
| Finished strands survive repeated cycles unchanged | Done |
| Population multiplies from cycle to cycle | Done, until the world fills up |
| Sustained exponential growth in a large world | Partly: growth continues, but slows as the colony spreads |
| Replicators arise from free atoms with no template | Done, though only very short ones |
| Turnover, so that a full world keeps changing | Done |
| Sequences that differ in fitness, and selection between them | Done, for pairing strength |
| Adaptation to a varied environment | Tried: a world heated by a vent, which first needs material to recycle everywhere |
| A phenotype that does something beyond being copied | Not started |

**Growth in a large world.** In a world four times the area, with four times the atoms, one strand became about 4, 8, 15, 24 and 37 capped strands over five cycles. The world was far from full, with five atoms in six still free, yet the growth factor fell steadily from 2.1 to 1.5. The counts fit a square law, which is what a colony spreading outward from one spot would give: strands separate cleanly but drift slowly, so the population stays crowded near where it started. Exponential growth would need the strands to disperse.

**Emergence from the soup.** With no template, whether replicators arise depends on the strong-bond barrier, which sets how often two atoms join without a template. At a barrier of 20, a capped strand appeared in 3 runs out of 8 within ten cycles; each was then copied into a family of about 30. At 15 or below, they arise in every run within a few cycles, and the world fills with them until the caps run out. Almost all are three atoms long (a cap, one coding atom, a cap), and none is longer than five.

**Copying fidelity.** Copying is not perfectly faithful. Most errors are deletions, leaving a strand one to three atoms short. These mutants are capped and copied like any other strand, so they are heritable variation. In the large world their share rose from 9% to 24% of all capped strands over five cycles. Nothing restores a deleted atom, so their share can only grow even if they have no advantage.

**Length and copying speed.** Without the barrier to starting a strand, a four-atom template is copied about three times as fast as a twelve-atom one, since a short strand needs fewer joins. With the barrier, the two are copied at about the same rate: every copy needs one start, a short template has few places for one, and that becomes its bottleneck. The barrier therefore removes most of the speed advantage of short strands, though not the fact that chance produces them far more often.

**Faithful copying.** In conditions designed for copying alone (scenario C: a high barrier, so nothing joins without a template; a hot phase as long as the cold one; caps that never come off), one 12-atom template becomes about 3.4, 6.3 and 9.5 full-length capped strands per run over three cycles. The template survives intact in every run, copies of copies carry the template's own sequence forward, and about a quarter of the strands are short by the third cycle.

**Turnover and competition.** In a living world (scenario B), where caps come off in the heat and chance strands can arise, a 12-atom template competes with whatever the soup produces. Three-atom replicators appear in every run within a few cycles and win every time: after 40 cycles there were about 76 short chains per run against fewer than one of the template's family, which had died out in most runs. A short strand is cheap to make by chance and needs fewer free atoms to copy, and nothing yet rewards length.

**Selection on pairing strength.** The weak bond's energy can differ by letter pair. When Y–Z is made stronger than W–X, a free atom stays on its template longer in the cold, which gives it more time to be joined into the copy. Head to head, starting from equal numbers of P-W-Q and P-Y-Q strands, the stronger pair's share rose from 0.50 to about 0.82 over 30 cycles, and it was ahead in all 12 runs, while a control with equal energies stayed at 0.50. The advantage was already full-sized at a Y–Z energy of 3 (against 2 for W–X), and stronger bonds up to 18 bought nothing more and cost nothing either: in a uniformly heated world, stronger pairing is a one-way ratchet with no optimum.

**A vent.** A world heated from one side was meant to give pairing strength a trade-off: during each pulse the vent heats nearby strands more than distant ones, so each place has its own cycle, and a Y/Z copy needs a hotter pulse to separate from its template than a W/X copy. Five experiments (9 to 13) looked for Y/Z strands winning near the vent and W/X strands farther out. None showed it convincingly, but together they taught more about the model than about selection:

- *Long duplexes melt sharply, near energy divided by entropy for their pairs.* A ten-letter W/X duplex goes from never separating to always separating between kT 0.5 and 0.75, and a Y/Z one (with Y–Z stronger) between 0.75 and 1.0. Duplexes unzip from their ends, where stacking helps least, so stacking hardly raises the melting point.
- *Short strands aren't gated by temperature.* A three-atom duplex comes apart even in the cold phase, given tens of thousands of chances, so heat barely matters to how fast it is copied.
- *Competition needs a shared limiting resource.* W/X and Y/Z strands are built from different atoms, so when caps are plentiful each kind just fills up with its own atoms and the two never compete. Earlier selection results worked because caps ran out first. With caps scarce, the stronger Y–Z pair lost overall in a vent whose pulses mostly fell below its melting point, as the trade-off predicts, but on very few strands.
- *Where nothing melts, material is trapped for good.* On the cool side, a template is copied, its copy's free face templates another copy, and the stack grows into a sheet that never separates. Caps come off only in the heat, so nothing there dies either. These sheets soak up atoms and caps and never return them, until almost no free atoms are left anywhere.
- *Lineages don't travel.* A strand drifts about 20 sites per cycle, so where a kind lives is mostly set by where its founders started.

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
| Energy by pair | Can differ by the types of the two atoms | Can differ by letter pair, e.g. Y–Z stronger than W–X |
| Entropy | None | Pairing costs entropy, so pairs melt when hot |

No bond of either type may cross another.

### Dynamics

Time advances by single random events, accepted or rejected by the Metropolis rule at temperature kT:

- **Hop:** an atom moves one site, if that leaves it clear of other atoms and keeps all its bonds legal. A hop changes no energy, so it doesn't depend on temperature.
- **Toggle:** a bond between two atoms in range forms if absent or breaks if present. It must first cross the bond type's barrier and is then accepted according to its energy and entropy.

One *sweep* is one attempted event per atom.

Temperature can vary from place to place. A bond uses the average of the temperatures at its two atoms, so forming and breaking it see the same temperature whichever atom the event starts from.

### What makes copying happen

- **Stacks.** When two neighboring atoms of one strand are each paired with neighboring atoms of another, the two pairs form a stack. A stack adds binding energy.
- **Templated joining.** A stack also removes the barrier to the strong bond beside it. Two free atoms sitting side by side on a template therefore join readily, while two free atoms meeting anywhere else almost never do. This is the only route by which strands grow.
- **A barrier to starting.** Joining two free atoms, which starts a new strand, keeps a small barrier even on a template. Adding an atom to an existing strand does not. A copy therefore tends to grow from a few starts instead of many, which wastes fewer atoms on fragments and makes copies more faithful.
- **Caps.** A cap can have only one strong bond, so a strand that ends in caps cannot be extended. A cap is copied like any other atom, by pairing with its complement, so strand length is inherited. A strand has a P at one end and a Q at the other, which gives it a direction: read from P to Q, a copy is the reverse complement of its template.
- **Peeling.** A strong bond at an uncapped end of a strand breaks at a rate that rises with temperature. Unfinished strands melt back into free atoms during the hot phase, and finished, capped strands are untouched.
- **Bond entropy.** A weak bond has an entropy cost as well as an energy, so it has a melting point: stable when cold, and unlikely even between two atoms held in range when hot. Without this, paired strands stay stuck together at any temperature.
- **Caps come off.** In the heat, a cap occasionally breaks away from its strand, at a rate set by its own barrier. The uncapped strand then peels apart, returning its atoms to the soup. This is death, and it gives a full world turnover.
- **Temperature cycling.** A cold phase lets free atoms pair with strands and join into copies. A hot phase melts the weak bonds so strands separate, and peels away the fragments.

Every rule except peeling (of exposed ends and of caps) obeys detailed balance, so at a fixed, uniform temperature the model would settle into thermal equilibrium. Peeling is deliberately one-way. Together with the temperature cycle, it is what keeps the system out of equilibrium, with atoms flowing from the free pool into strands and back. A temperature that varies from place to place also prevents equilibrium, since a bonded pair can hop between temperatures for free.

## What we learned getting here

Each rule above was added to fix a specific failure that showed up in measurement:

- **Flexible strands with two-sided pairing collapsed into globules.** Stiffness fixed this, and it had to be a limit on the angle itself; a limit on the raw dot product allowed sharp bends wherever bonds were stretched.
- **Free atoms kept switching roles between strand and partner.** Allowing a second weak partner only for an atom already in a strand fixed this.
- **Random joining during the hot phase.** A high barrier that only a stack removes fixed this.
- **Strands grew longer every cycle.** Measurement showed free atoms being added to strand ends, templated by a partner strand that had slid out of alignment. Caps fixed this.
- **Fragments accumulated and used up the free atoms,** and many were completed into three-atom capped strands that then multiplied faster than full-length ones. Peeling removes the fragments each cycle.
- **Strands stayed stuck together when hot.** A pair of atoms in range was bonded at least half the time at any temperature, because pairing had an energy but no entropy cost. Bond entropy fixed this.
- **Growth stalled for want of free atoms,** most of which were tied up in fragments during each cold phase. More atoms helped; the barrier to starting a strand cut the fragments by two thirds and removed most of the short and truncated copies as well.

Three ideas were tried and dropped because measurement did not support them: requiring two strong bonds for a second weak partner, four-letter templates, and (more than once) the guess that strands clumping into sheets was what limited growth.

## Open problems

- **The simplest replicator wins.** Strands that arise from the soup are three atoms long, and nothing favors anything longer: in a living world they displace a seeded 12-atom template every time. Lineages seeded with a long template also drift shorter through deletions. Something has to make length, or the order of a sequence, worth having.
- **Material gets trapped.** Strands die only when a cap comes off in the heat, so wherever it stays cool, nothing is ever recycled: duplexes and sheets that never melt hold their atoms forever, and in a world with any cool region, material drains into it until the world stops. Every region needs some slow way for material to return.
- **Strands don't spread.** A lineage stays near where it started, so growth in a large world is limited by the colony's edge, not by the supply of atoms. This also means a strand can't follow a changing environment: nothing in its sequence affects where it goes.
- **Phenotypes don't do anything yet.** Pairing strength is heritable and selected, but it depends only on which letters a strand contains, and in a uniform world stronger is simply better. The aim is a phenotype that is a physical action with costs and benefits: strands that fold into shapes, bind to things, or hold atoms, in an environment that poses a problem worth solving. Folding would need strands that can bend, which at present they cannot.
- **Two partners can share a face at a bend.** The weak angle rule keeps an atom's two partners on opposite sides of the atom, which at a sharp bend is not the same as opposite sides of the strand. It is rare and corrects itself, so it has been left alone.

## Repository layout

| Project | Contents |
|---|---|
| `Ecos` | The core library: atoms, bonds, parameters, the world, and the update rule |
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

The app offers every scenario: the standard ones, and each experiment's variants. With the same scenario and seed, it replays exactly the world that an experiment ran. (The exception is experiment 2, which first lets its worlds settle with strong bonds frozen.)

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
| `experiment3c` | The same, in scenario C, which is designed for copying |
| `experiment4` | With no template, do replicators arise from free atoms, and do they multiply? |
| `experiment5` | The same in a living world, where strands also die |
| `experiment6` | Does a seeded template's lineage hold its own against chance replicators? |
| `experiment7` | With Y–Z pairs stronger than W–X, does the stronger kind take over? |
| `experiment8` | The same head to head, from equal numbers of each kind, over a range of strengths |
| `experiment9` | In a world heated by a vent, does each kind win where its pairing suits the temperature? |
| `experiment10` | The same, with ten-letter templates of each kind instead of three-atom strands |
| `experiment11` | At what temperature does each kind of ten-letter duplex melt? |
| `experiment12` | Templates in a vent whose pulses span the two melting points, counting only separated strands |
| `experiment13` | The same, with caps scarce, so that the two kinds compete for them |

Experiments run their worlds in parallel and print their progress. They take from a few minutes (experiment 3) to a few hours (experiments 7 and 8) on a 24-core machine. Each prints tables of results and writes them to the CSV file.

### Scenarios

| Scenario | World |
|---|---|
| A | A fixed temperature, with soft strong bonds |
| B | The living world: a cold/hot cycle (90,000/10,000 sweeps), a strong barrier low enough that strands arise by chance, and caps that come off |
| C | Copying alone: a high strong barrier, a long hot phase, and caps that never come off |
| Vent | Scenario B's world heated by a pulsing vent along its left wall, seeded with W/X and Y/Z strands |
| Vent window | Templates of each kind in a vent whose pulses span their melting points, with caps that come off |

Each comes with or without a template strand. Experiments build their variants from these.

## How the model is tested

- **Exact enumeration.** For systems small enough to list every state, the simulation's statistics are compared with the exact Boltzmann distribution, with and without bond entropy and the start barrier.
- **Independent oracles.** Each rule is restated separately in the tests, and the library is checked against that statement, exhaustively where the cases are few and by random worlds otherwise.
- **Event rates.** In crowded random worlds, each kind of event must happen as often as the rules predict, including across a sharp step in temperature.
- **Regression fingerprints.** Recorded worlds must be reproduced exactly, update for update, so that changes made for speed can be shown to change nothing else.
- **Mutation checks.** Each new test is run against deliberately broken copies of the library, to confirm that it fails when it should.

## Background

The lattice model is the Bond Fluctuation Model of polymer physics, with Metropolis dynamics. The approach follows Tim Hutton's artificial chemistry work on self-replicating molecules, with the aim of more realistic physics and less built-in machinery. Growth by layering and fragmentation is related to the crystal-based replicators of Schulman, Yurke and Winfree. The advantage of short replicators echoes Spiegelman's experiments with RNA.

The core library is written by Brian Berns. The tests and experiments are written with Claude.
