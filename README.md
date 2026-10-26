# Ecos

Ecos is an attempt to create digital life from simple chemistry. It simulates atoms on a 2D lattice that move, bond into strands, and copy those strands by pairing with a template. The goal is self-replication from raw materials that is robust enough for evolution by natural selection.

Replication is not scripted. The rules describe only local physics and chemistry: how atoms move, which bonds can form, and what they cost. Anything that looks like copying has to emerge from those rules.

![A world after five temperature cycles](images/ecosystem.png)

*A world after five cold/hot cycles, during the sixth cold phase. It began with one 12-atom strand (the filled circles, centre) among free atoms. Every other strand is a descendant: a copy, or a copy of a copy. Black lines are strong bonds along a strand, pink lines are weak bonds between paired atoms, and the black and grey circles are the caps at each end of a strand.*

## Where it stands

A single strand placed among free atoms is copied, the copies separate when heated and are copied in turn, and the population grows until the world is full. One strand becomes about 3, then 6, then 9 or 10 over three cycles, after which space and free atoms run short.

Replicators also arise without any template. In a soup of free atoms, short capped strands occasionally form by chance during the hot phase, and once one exists it is copied and its lineage multiplies. This is the model's version of abiogenesis.

Strands also die: now and then a strong bond breaks of its own accord (hydrolysis), and the pieces of the cut strand fall apart into free atoms. With births and deaths, a full world keeps turning over, and lineages compete. Natural selection now works on a heritable trait: when one letter pair binds more strongly than the other, strands made of it take over the population.

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
| A phenotype that does something beyond being copied | Started: strands can fold into hairpins, which don't yet do anything |

**Growth in a large world.** In a world four times the area, with four times the atoms, one strand became about 4, 8, 15, 24 and 37 capped strands over five cycles. The world was far from full, with five atoms in six still free, yet the growth factor fell steadily from 2.1 to 1.5. The counts fit a square law, which is what a colony spreading outward from one spot would give: strands separate cleanly but drift slowly, so the population stays crowded near where it started. Exponential growth would need the strands to disperse.

**Emergence from the soup.** With no template, whether replicators arise depends on the strong-bond barrier, which sets how often two atoms join without a template. At a barrier of 20, a capped strand appeared in 3 runs out of 8 within ten cycles; each was then copied into a family of about 30. At 15 or below, they arise in every run within a few cycles, and the world fills with them until the caps run out. Almost all are three atoms long (a cap, one coding atom, a cap), and none is longer than five.

**Copying fidelity.** Copying is not perfectly faithful. Most errors are deletions, leaving a strand one to three atoms short. These mutants are capped and copied like any other strand, so they are heritable variation. In the large world their share rose from 9% to 24% of all capped strands over five cycles. Nothing restores a deleted atom, so their share can only grow even if they have no advantage.

**Length and copying speed.** Without the barrier to starting a strand, a four-atom template is copied about three times as fast as a twelve-atom one, since a short strand needs fewer joins. With the barrier, the two are copied at about the same rate: every copy needs one start, a short template has few places for one, and that becomes its bottleneck. The barrier therefore removes most of the speed advantage of short strands, though not the fact that chance produces them far more often.

**Faithful copying.** In conditions designed for copying alone (scenario C: a high barrier, so nothing joins without a template; a hot phase as long as the cold one; caps that never come off), one 12-atom template becomes about 3.4, 6.3 and 9.5 full-length capped strands per run over three cycles. The template survives intact in every run, copies of copies carry the template's own sequence forward, and about a quarter of the strands are short by the third cycle.

**Turnover and competition.** In a living world (scenario B), where strands die and chance strands can arise, a 12-atom template competes with whatever the soup produces. Three-atom replicators appear in every run within a few cycles and win every time: after 40 cycles there were about 76 short chains per run against fewer than one of the template's family, which had died out in most runs. A short strand is cheap to make by chance and needs fewer free atoms to copy, and nothing yet rewards length. (This was measured when strands died by losing caps in the heat; the experiment is archived.)

**Selection on pairing strength.** The weak bond's energy can differ by letter pair. When Y–Z is made stronger than W–X, a free atom stays on its template longer in the cold, which gives it more time to be joined into the copy. Head to head, starting from equal numbers of P-W-Q and P-Y-Q strands, the stronger pair's share rose from 0.50 to about 0.75 over 30 cycles, and it was ahead in all 12 runs, while a control with equal energies stayed near 0.50. The advantage was already full-sized at a Y–Z energy of 3 (against 2 for W–X), and stronger bonds (up to 18, in an earlier run) bought nothing more and cost nothing either: in a uniformly heated world, stronger pairing is a one-way ratchet with no optimum.

**A vent.** A world heated from one side was meant to give pairing strength a trade-off: during each pulse the vent heats nearby strands more than distant ones, so each place has its own cycle, and a Y/Z copy needs a hotter pulse to separate from its template than a W/X copy. Five experiments (9 to 13, now archived) looked for Y/Z strands winning near the vent and W/X strands farther out. None showed it convincingly, but together they taught more about the model than about selection:

- *Long duplexes melt sharply, near energy divided by entropy for their pairs.* A ten-letter W/X duplex goes from never separating to always separating between kT 0.5 and 0.75, and a Y/Z one (with Y–Z stronger) between 0.75 and 1.0. Duplexes unzip from their ends, where stacking helps least, so stacking hardly raises the melting point.
- *Short strands aren't gated by temperature.* A three-atom duplex comes apart even in the cold phase, given tens of thousands of chances, so heat barely matters to how fast it is copied.
- *Competition needs a shared limiting resource.* W/X and Y/Z strands are built from different atoms, so when caps are plentiful each kind just fills up with its own atoms and the two never compete. Earlier selection results worked because caps ran out first. With caps scarce, the stronger Y–Z pair lost overall in a vent whose pulses mostly fell below its melting point, as the trade-off predicts, but on very few strands.
- *Where nothing melts, material is trapped for good.* On the cool side, a template is copied, its copy's free face templates another copy, and the stack grows into a sheet that never separates. Caps come off only in the heat, so nothing there dies either. These sheets soak up atoms and caps and never return them, until almost no free atoms are left anywhere.
- *Lineages don't travel.* A strand drifts about 20 sites per cycle, so where a kind lives is mostly set by where its founders started.
- *Without walls, the two kinds take different strategies.* In a torus four times the usual area, with a vent whose pulses span the two melting points and scarce caps, Y/Z templates pile up in the cool zone as sheets: copies their pulses can't melt, each templating the next. W/X copies, which melt there, separate and spread thinly but evenly. One kind builds structures that hold atoms, the other disperses, from a single physical difference. (Observed in the desktop app's "Vent window, 2x" scenario; not yet measured. The sheets look like accumulation more than replication, since their copies are never released.)

**Walls.** Where a template starts matters. Alone in a box, a lineage that starts within 16 sites of a wall grows 15 to 25% less over three cycles than one at the center, and one tucked into a corner 40% less; the effect fades by 32 sites. Competing with three others for the same atoms, the disadvantage roughly doubles: a corner lineage reaches 58% of the center's size, and the center lineage ends largest in 13 of 24 runs. The cause looks like access. A template is copied along both faces, so one lying along a wall, with a face toward it, is copied 38% less from the first cycle, while one pointing its end at the wall hardly suffers; a corner adds less room for the copies to spread into. Position isn't inherited through sequence, but anything heritable that changed a strand's orientation or reach would be selected. Because walls affect lineages this strongly, and would confound other experiments, the standard world now has none: it is a torus (see the model, below), and walls appear only where an experiment adds them on purpose. Earlier results on this page were measured in a box of walls; the accomplishment checks confirm the key ones without them.

**Folding.** Strands can now bend up to a right angle at each atom, where before they were limited to about 37°, and each bend costs energy in proportion to how sharp it is. Alone in a small world, a self-complementary strand, P W₆ Y Y X₆ Q, folds back on itself through its Y Y loop and zips its two arms together into a hairpin: at the standard stiffness it is folded about half the time when cold. A hairpin is stable once formed, but starting one is slow, since the loop's two bends cost energy before any pair forms to pay for them; stiffness sets how often folds start rather than whether they last. Bendable strands copy as faithfully as straight ones did (over 48 runs of scenario C, full-length copies and exact copies were statistically unchanged), though they make more capped strands, most of the extra ones short or mutated. All three accomplishment checks pass with folding on. In the living world, though, the strands that arise by chance average just over three atoms, too short to fold: a hairpin needs about six. Seeded head to head with strands of the same atoms that can't fold (and that pair with them as little as possible), hairpin-formers neither win nor lose over 20 cycles, but only because they hardly ever fold: about 1 in 25 when cold for an eight-atom strand with a three-pair stem (P W W Y Y X X Q), and still under 1 in 10 for a ten-atom one with a four-pair stem (P W W W Y Y X X X Q), against about half the time for a lone strand. A strand in a crowded world spends the cold phase paired with other strands instead, and a hairpin-former's best partners are its own copies, which pair with it along its whole length. Protection from hydrolysis, a lower rate for a strong bond whose two atoms were both paired, didn't change the outcome: with full protection both seeded families ended about a quarter larger, but the hairpin share stayed at about 0.5, since both kinds are mostly paired with other strands and protection rewards pairing of any kind. Nor does heat favor hairpins. Alone, a four-pair hairpin melts at a lower temperature (about kT 0.45) than a duplex of five pairs (0.5) or ten (0.6): its loop's bends cost energy, and pairing within a strand earns no advantage over pairing between strands, since a weak bond's entropy cost is the same either way. (Measured by experiment 18, now archived.) Experiment 19 tested the idea that folding protects a strand through hard times: duplexes of each kind are melted, and then an adverse phase, at a temperature at which a hairpin stays folded, hydrolyzes exposed bonds faster. In the living world, both kinds survived equally. Hairpin-formers fold slowly (at a soft stiffness, a quarter or so within 10,000 sweeps; at the standard stiffness, hardly any), their loops stay exposed, and meanwhile free atoms copy the single non-formers, which leaves them double-stranded, and so protected, about as fast. With no free atoms, so that nothing could copy a single strand, hairpin-formers did better. On 32 runs (not those it was found on), after a long, slow adverse phase, 52% were intact against 40% of non-formers with protection: 0.12 ± 0.03 more per run, ahead in 20 runs and behind in 7. Protection raised hairpin-formers' survival from 43% to 52%, but non-formers' only from 37% to 40%, since about half the hairpin-formers fold during the phase, which stacks their stems, while the non-formers mostly stay single. A cut stem can also rejoin at once, its barrier taken away by the stack, but that repair adds little: without protection, 30% of hairpin-formers' broken bonds formed again against 24% of non-formers', and their survival differed by only 0.06 ± 0.03. Starvation, more than heat, is the adverse condition that folding answers.

## The model

### Atoms and the lattice

- The world is a 2D square lattice that wraps around: a torus, whose opposite edges meet, so it has no boundary. Each atom occupies a 2×2 block of sites, and no two atoms may overlap.
- Atoms are never created or destroyed, so mass is conserved.
- Walls are immovable blocks the size of an atom. The standard world has none; an experiment adds them where it wants edges.
- There are six atom types. W, X, Y and Z are the coding atoms that make up a strand's sequence. P and Q are caps that end a strand.

### Bonds

| | Strong bonds | Weak bonds |
|---|---|---|
| Role | Join atoms along a strand | Pair atoms on different strands |
| Which atoms | Any two, except two caps | Complementary types only: W–X, Y–Z, P–Q |
| Range | Longer (up to about 3.6 sites) | Shorter (up to about 2.8 sites) |
| Limit per atom | Two, or one for a cap | Two, and the second only for an atom in a strand |
| Angle rule | At most a right angle of bend at each atom, and each bend costs energy | An atom's two partners must be on opposite sides of it |
| Energy and barrier | Large energy, large barrier | Small energy, no barrier |
| Energy by pair | Can differ by the types of the two atoms | Can differ by letter pair, e.g. Y–Z stronger than W–X |
| Entropy | None | Pairing costs entropy, so pairs melt when hot |
| Hydrolysis | Rare one-way breaking, at a fixed rate, which can be lower in a double-stranded stretch | None |

No bond of either type may cross another.

Each bond type has a *bend limit*, the sharpest angle allowed between two of its bonds at an atom, and a *stiffness* κ: a bend whose two bonds make an angle φ costs κ(1 + cos φ), nothing for a straight strand and κ at a right angle. The standard strong bond allows right angles with κ = 1.5, so strands stay mostly straight but can fold where pairing pays for it. Weak bonds have no extra limit and cost nothing to bend.

### Dynamics

Time advances by single random events, accepted or rejected by the Metropolis rule at temperature kT:

- **Hop:** an atom moves one site, if that leaves it clear of other atoms and keeps all its bonds legal. The only energy a hop can change is bending, at the atom and at its bonded partners; a hop that sharpens bends is accepted according to the extra bend energy, and any other hop always is.
- **Toggle:** a bond between two atoms in range forms if absent or breaks if present. It must first cross the bond type's barrier and is then accepted according to its energy and entropy. A bond's energy is reduced by any bends it makes with the bonds beside it.

One *sweep* is one attempted event per atom.

Temperature can vary from place to place. A bond uses the average of the temperatures at its two atoms, so forming and breaking it see the same temperature whichever atom the event starts from.

### What makes copying happen

- **Stacks.** When two neighboring atoms of one strand are each paired with neighboring atoms of another, the two pairs form a stack. A stack adds binding energy.
- **Templated joining.** A stack also removes the barrier to the strong bond beside it. Two free atoms sitting side by side on a template therefore join readily, while two free atoms meeting anywhere else almost never do. This is the only route by which strands grow.
- **A barrier to starting.** Joining two free atoms, which starts a new strand, keeps a small barrier even on a template. Adding an atom to an existing strand does not. A copy therefore tends to grow from a few starts instead of many, which wastes fewer atoms on fragments and makes copies more faithful.
- **Caps.** A cap can have only one strong bond, so a strand that ends in caps cannot be extended. A cap is copied like any other atom, by pairing with its complement, so strand length is inherited. A strand has a P at one end and a Q at the other, which gives it a direction: read from P to Q, a copy is the reverse complement of its template.
- **Peeling.** A strong bond at an uncapped end of a strand breaks at a rate that rises with temperature. Unfinished strands melt back into free atoms during the hot phase, and finished, capped strands are untouched.
- **Bond entropy.** A weak bond has an entropy cost as well as an energy, so it has a melting point: stable when cold, and unlikely even between two atoms held in range when hot. Without this, paired strands stay stuck together at any temperature.
- **Hydrolysis.** Any strong bond, a cap's included, occasionally breaks one way, at a fixed rate whatever its energy, position or temperature. A bond can be protected: if it lies in a double-stranded stretch, as one side of a stack, it can be given a lower rate. Atoms merely paired with free atoms don't count. (The standard scenarios don't use this.) A cut strand has uncapped ends, which peel away in the heat (and hydrolyze in the cold), returning its atoms to the soup. This is death, and it gives a full world turnover everywhere, cool regions included. The rate is a trade-off: in scenario C, a 12-atom template still multiplies about 1.7-fold per cycle at 10⁻⁵ per attempt, 1.4-fold at 3 × 10⁻⁵, and not at all at 10⁻⁴, since long strands have more bonds to lose. Scenario B, whose strands are short, uses 10⁻⁴.
- **Temperature cycling.** A cold phase lets free atoms pair with strands and join into copies. A hot phase melts the weak bonds so strands separate, and peels away the fragments.

Every rule except peeling and hydrolysis obeys detailed balance, so at a fixed, uniform temperature the model would settle into thermal equilibrium. Peeling and hydrolysis are deliberately one-way. Together with the temperature cycle, it is what keeps the system out of equilibrium, with atoms flowing from the free pool into strands and back. A temperature that varies from place to place also prevents equilibrium, since a bonded pair can hop between temperatures for free.

## What we learned getting here

Each rule above was added to fix a specific failure that showed up in measurement:

- **Flexible strands with two-sided pairing collapsed into globules.** Stiffness fixed this, and it had to be a limit on the angle itself; a limit on the raw dot product allowed sharp bends wherever bonds were stretched. To let strands fold, the limit was later relaxed to a right angle, with an energy cost for every bend so that strands still prefer to be straight; without the cost, strands that can bend that far are floppy.
- **Free atoms kept switching roles between strand and partner.** Allowing a second weak partner only for an atom already in a strand fixed this.
- **Random joining during the hot phase.** A high barrier that only a stack removes fixed this.
- **Strands grew longer every cycle.** Measurement showed free atoms being added to strand ends, templated by a partner strand that had slid out of alignment. Caps fixed this.
- **Fragments accumulated and used up the free atoms,** and many were completed into three-atom capped strands that then multiplied faster than full-length ones. Peeling removes the fragments each cycle.
- **Strands stayed stuck together when hot.** A pair of atoms in range was bonded at least half the time at any temperature, because pairing had an energy but no entropy cost. Bond entropy fixed this.
- **Growth stalled for want of free atoms,** most of which were tied up in fragments during each cold phase. More atoms helped; the barrier to starting a strand cut the fragments by two thirds and removed most of the short and truncated copies as well.

Three ideas were tried and dropped because measurement did not support them: requiring two strong bonds for a second weak partner, four-letter templates, and (more than once) the guess that strands clumping into sheets was what limited growth.

## Open problems

- **The simplest replicator wins.** Strands that arise from the soup are three atoms long, and nothing favors anything longer: in a living world they displace a seeded 12-atom template every time. Lineages seeded with a long template also drift shorter through deletions. Something has to make length, or the order of a sequence, worth having.
- **Material gets trapped, or recycles slowly.** Before hydrolysis, strands died only when a cap came off in the heat, so duplexes and sheets that never melted held their atoms forever, and a world with any cool region drained into it until it stopped. Hydrolysis now returns material everywhere, but at rates that long strands can survive, a frozen sheet takes tens of cycles to dissolve. Faster recycling favors shorter strands still further.
- **Strands don't spread.** A lineage stays near where it started, so growth in a large world is limited by the colony's edge, not by the supply of atoms. This also means a strand can't follow a changing environment: nothing in its sequence affects where it goes.
- **Phenotypes don't do anything yet.** Pairing strength is heritable and selected, but it depends only on which letters a strand contains, and in a uniform world stronger is simply better. The aim is a phenotype that is a physical action with costs and benefits: strands that fold into shapes, bind to things, or hold atoms, in an environment that poses a problem worth solving. Strands can now fold into hairpins, but in this physics folding has nothing to offer: a hairpin is less stable than a duplex, folds rarely in a crowd, and gains nothing from protection from hydrolysis that pairing with another strand doesn't give as well. Protection does reward staying paired, though, which may make duplexes and sheets the more promising phenotype: a copy that stays with its template survives longer, but isn't free to be copied. For folding to pay, pairing within a strand would need an advantage of its own, as tethered partners have in real chemistry, or strands would need to pair on one face only, so that sheets couldn't form.
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
| `experiment8` | The same head to head, from equal numbers of each kind, over a range of strengths |
| `experiment11` | At what temperature does each kind of ten-letter duplex melt? |
| `experiment14` | Is a lone template copied less near a wall, or in a corner? |
| `experiment14b` | When four templates compete, from the center, a side, an end and a corner, which lineage wins? |
| `experiment15` | How often does a self-complementary strand fold into a hairpin, for each bend limit and stiffness? |
| `experiment16` | In a living world, how many strands fold, could fold, or pair into duplexes and sheets? |
| `experiment17` | Head to head, do ten-atom strands that can fold into hairpins multiply more or less than strands of the same atoms that can't, with and without protection from hydrolysis? |
| `experiment17s` | The same with eight-atom strands, without protection |
| `experiment19` | After the heat, does a strand that can fold survive an adverse phase better than one that can't? |
| `experiment19g`, `19s`, `19n`, `19c` | The same with a gentler adverse phase, a slow one, a slow one with no free atoms, and that again on more runs |

Experiments 6, 7, 9, 10, 12, 13 and 18 are archived: their results are summarized above, and their code is in the git history.

Experiments run their worlds in parallel and print their progress. They take from seconds (experiment 11) to an hour or so (experiment 8) on a 24-core machine. Each prints tables of results and writes them to the CSV file.

### Scenarios

| Scenario | World |
|---|---|
| A | A fixed temperature, with soft strong bonds |
| B | The living world: a cold/hot cycle (90,000/10,000 sweeps), a strong barrier low enough that strands arise by chance, and strong bonds that hydrolyze, so strands die |
| C | Copying alone: a high strong barrier, a long hot phase, and no hydrolysis, so nothing dies |
| Vent | Scenario B's world heated by a pulsing vent along its seam, seeded with W/X and Y/Z strands |
| Vent window | Templates of each kind in a cooler vent whose pulses span their melting points, with scarce caps, for watching in the desktop app (also at twice the size, "Vent window, 2x") |

Every standard world is a 160×160 torus with no walls; scenarios lay their atoms out around its center, halfway around from the seam. Each comes with or without a template strand, and experiments build their variants from these. In all of them, strands can bend to a right angle at the standard stiffness. To watch a strand fold, see the desktop app's "Experiment 15" group, which puts one self-complementary strand alone in a small world, stiff, at the standard stiffness, and floppy. The "Experiment 19" group starts at the adverse phase after the heat, in the living world or a starved one, with and without protection, and shows the seeded hairpin-formers filled.

## How the model is tested

- **Exact enumeration.** For systems small enough to list every state, the simulation's statistics are compared with the exact Boltzmann distribution, with and without bond entropy, the start barrier, and bend limits and stiffness for each bond type.
- **Independent oracles.** Each rule is restated separately in the tests, and the library is checked against that statement, exhaustively where the cases are few and by random worlds otherwise.
- **Event rates.** In crowded random worlds, each kind of event must happen as often as the rules predict, including across a sharp step in temperature.
- **Regression fingerprints.** Recorded worlds must be reproduced exactly, update for update, so that changes made for speed can be shown to change nothing else.
- **Torus equivalence.** The dynamics depend only on where atoms are relative to each other, so a world shifted across the torus's seam must behave exactly, update for update, as the same world away from it.
- **Mutation checks.** Each new test is run against deliberately broken copies of the library, to confirm that it fails when it should.
- **Accomplishment checks.** The unit tests check each rule; a separate, slower command checks that the rules still add up to the model's key accomplishments: faithful copying in scenario C, chance replicators arising and turning over in a living world, and selection on pairing strength. Each is a reduced experiment with a pass threshold set well below its measured results. Run it after changing the core or the scenarios (it takes several minutes):

  ```bash
  dotnet run -c Release --project Ecos.Experiment -- checks
  ```

## Background

The lattice model is the Bond Fluctuation Model of polymer physics, with Metropolis dynamics. The approach follows Tim Hutton's artificial chemistry work on self-replicating molecules, with the aim of more realistic physics and less built-in machinery. Growth by layering and fragmentation is related to the crystal-based replicators of Schulman, Yurke and Winfree. The advantage of short replicators echoes Spiegelman's experiments with RNA.

The core library is written by Brian Berns. The tests and experiments are written with Claude.
