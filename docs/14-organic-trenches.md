# Organic trench presentation

The 2026-09-21 pass builds on the existing night environment and stepped simulation fire bays.
It changes presentation geometry and dressing, leaving simulation data and routes intact.

[Before](reference/trenches-organic-before.png) / [After](reference/trenches-organic-after.png).
Both use the standard 25-degree lens, 25-degree pitch, zoom 30 view. Rain, lights and soldier positions vary with time.

## Shape hierarchy

- **Large:** approximately 40 m world-coordinate sway and asymmetric shoulder allowances. Shared contour vertices
  ease the grid corners; adjacent wall modules meet at the same endpoints. Bank height and width vary at a slower
  scale than individual props. The trench keeps its overall direction and existing fire bays.
- **Medium:** three revetment assemblies and parapet courses create repaired stretches, reinforced timber,
  stacked bags and lower worn sections. Coherent frontage noise controls the sandbag course and occasional gaps.
- **Small:** chipped upper planks, leaned supports, irregular board ends, settled bags, slight roll and yaw,
  and three uneven duckboard assemblies. The existing painted materials and sack geometry are retained.

`BattlefieldSurface.Edge` retains its original simulation boundary and additionally stores a cached dressing frame.
Bank profiles, trench wall placement, parapets and hanging lamps use that frame. The original floor and ladder
positions remain stable. Outside the original trench, the ground follows the widened visual wall toe.
The existing render-ground grid keeps units and effects aligned with the presentation surface.

Sway fades out around ladder openings. Tight notches relax their shared endpoints locally when an offset would
collapse a panel or reverse its direction; this maintains connected joins without removing the broad variation elsewhere.

## Cost and checks

The standard sample changed from 228,627 to 228,529 submitted base prop vertices. Instanced submissions increased
from 165 to 220 because the nine reusable trench mesh variants occupy separate spatial pages. These counts exclude
terrain, outline/shadow passes and soldiers. No new shader or frame-by-frame deformation was added.
GTX 1050 frame timing remains unmeasured; these captures use an RTX 4070 Laptop GPU.

The environment audit covers three seeds and map sizes, checking repeatability, unchanged simulation hashes,
unchanged trench/link floor heights, valid site footprints, connected contour endpoints, panel orientation,
bounded sway and unmoved ladder openings. Run it in Play mode.
Offline validation, all 57 EditMode tests and all 3 PlayMode tests passed before committing.

The existing atmosphere, rain, water, vegetation and site systems remain in place. Their simulation-generation
changes from the preceding agent are preserved; this pass does not edit files under `Assets/_Project/Sim`.

## The fire step (2026-09-23)

The owner, playing: *"we want them to spread out in the trench more, and also put their guns over the edge of the
trench instead of shooting through the ground."*

Both were geometry, and both were measured in a live match before anything was changed.

### There was no fire step

`AddFireTrench` carved every cell of a trench down together, so the cell the sim calls a *firing post* sat level
with the trench floor. Cross-section of the front trench at mid-column, in metres:

| | before | after |
|---|---|---|
| trench floor | 0.73 | 0.73 |
| firing post | **0.77** | **1.47** |
| parapet | 2.43 | 2.43 |

A man at a firing post therefore stood 1.66 m below the field with his rifle about **0.4 m under the lip**: aiming
into the parapet, which is what the owner saw. `GreyboxMapGenerator.FireStepRise` (0.7 m, about the two feet of the
real thing) gives that row back its height. The picture corrected itself with no presentation change, because
`VATRenderer` stands a man on the drawn ground (`VATRenderer.cs:258`); raise the ground and the man rises.

`Stance.FireStep` already existed and already changed his pose, his accuracy (`AccuracyMultiplier` 1.25) and his
cover. What never existed was anything to stand **on**.

### A third of a garrison had nowhere to be

Measured with 92 men in the team 0 front trench:

| | before | after |
|---|---|---|
| men holding no post | **31 of 92** | 5 of 84 |
| closest pair | 0.88 m | 1.58 m |
| distinct posts used | 61 | 79 |
| *light garrison*, mean spacing | — | **6.01 m** |

Two causes. Posts are nav cells and a nav cell is 2 m, so every man stood at a cell centre: a visible 2 m lattice.
`TrenchPost.Offset` (in `TW.Sim.Core`, because `TW.Sim.Units` already references `TW.Sim.Nav` and both ends need
it) gives him a spot inside his own cell, hashed from the cell so it is identical everywhere and never moves while
he holds the post. And `ThinnedInSixteen` was deleting 31% of candidate posts to make the line look irregular,
which bought irregularity with capacity the garrison could not spare; the jitter supplies it better, so thinning
dropped to 2/16.

Elbow room is now offered **by degrees** — the widely spaced posts first (`Roomiest` = 2 cells), then the closer
ones, then any free post at all. Those tiers are **nominally** 6 m and 4 m and are not that in fact: the post
displacement pulls the real floors down to about 4.9 m and 3.1 m. The nominal figures were quoted here as if they
were guarantees; they are not, and the only number with a test behind it is the adjacent-post floor below. An empty trench spreads a garrison out, a filling one packs it down,
a full one stands men shoulder to shoulder rather than leaving them postless. In a packed trench the mean spacing
barely moves (1.77 → 1.90 m) and it should not: 84 men in 90 m of two-row trench are shoulder to shoulder by
geometry, not by policy.

### The fire step reaches the picture, not the simulation

Worth stating plainly, because the commit that built it did not: **`FireStepRise` has one consumer.** It raises
the heightfield, the drawn ground follows, and a man is drawn standing on it — that part is real and measured
(his rifle clears the parapet in all 180 columns of `ShelledForest(1917)`, by 0.08–0.25 m, where before the raise
it was 0.55–0.70 m under). Nothing else reads it.

`TargetAcquisition.Muzzle` returns `Height.Sample(q) + 0.3f` for **every** man with `UnitFlags.InTrench`, so the
fire-step man and the crouching reserve man fire from the same height, and `HeightfieldRaycast.EyeHeight(FireStep)
= 1.5f` is unreachable code. `TankGunnery` does the same. So the step changes no combat outcome at all.

And the stance it serves is close to unreachable anyway: `MovementSystem` mounts a man on the step only if he
already has a target, while `TargetAcquisition` makes an in-trench man who is *not* on the step untargetable
beyond `BelowRimRevealRange`. Two opposed garrisons cannot see each other, so neither ever stands up. Fixing that
is a combat-design decision — what makes a garrison expose itself — not a terrain edit, and it is not made here.

One hypothesis worth recording as **disproved**: the raise cannot occlude anyone. The step's top sits
`1.8 − 0.7 = 1.1 m` below local grade on every map by identity, so a ray that clears the parapet clears the step
by over a metre.

Also: every measurement in the table above was taken on the 150-column *playtest* trench. A `ShelledForest`
trench is 45 columns, so it offers ~35 firing posts after thinning and links — against `FiringInTen = 6` asking
60% of the garrison. Above roughly 58 men the map cannot supply the demand, and the surplus become reserve posts,
which are permanently excluded from the fire step. The table should be re-measured on the map the game plays.

### Why nobody was dying

Three failures stacked, found while trying to write a two-world test that kills a unit:

1. `FlowFieldManager.DefaultGoal` returns `RearTrench`, so on `ShelledForest` every man garrisons trench 0 or 3 —
   **180 m apart, against a 130 m rifle.** The front trenches, whose fire steps this work built, hold nobody
   until a player orders an advance.
2. `TrenchAdvance` with `A = FrontTrench(team)` matched **zero units**, because those trenches were empty — and
   the order was *silently accepted*. The ownership check passed, so no rejection was emitted. That is now an
   `OrderFoundNoOne` event; it is deliberately not `CommandRejected`, because the command was legal.
3. The second barrage was refused anyway: `HeBarrage.CooldownTicks` is 1200 and the salvos were 60 ticks apart.
   `SimConfig.Default.StartingSilver` is also 120 against a cost of 150.

### Correction: the displacement was measured against the wrong baseline

The first version of this used independent per-cell noise, and the before/after table above credited it with an
improvement it did not make.

`SeparationJob.GarrisonSpacing` is 2 m and a nav cell is 2 m. The bare cell lattice was therefore **exactly** tuned
to the separation radius: a fully posted garrison stood at rest, with the post pull and the separation push both at
zero. Independent noise of ±0.55 m destroyed that invariant — computed over the real hash, the worst adjacent pair
of post points came out **0.933 m** apart, deep inside the radius, so those men shoved each other while their posts
pulled them back and never settled.

The reported "closest pair 0.88 m → 1.58 m" looked like progress and was not. 0.88 m came from the **31 postless
men** collapsing onto the centreline, which is a different fault fixed by a different change in the same commit
(`ThinnedInSixteen` 5 → 2). Measured against the baseline that actually applies — the lattice's guaranteed 2.00 m —
the displacement made the packed case worse. Every improvement in the table traces to the thinning constant.

The repair keeps what the displacement was for. It is now a **smooth low-frequency field** rather than per-cell
noise, so neighbouring posts share most of their displacement: the line wanders off true, which is what stops it
reading as ruled, while the distance *between* neighbours is nearly preserved. Same amplitude, worst adjacent
separation **0.933 m → 1.809 m**, held by `TrenchSpreadTests.NoTwoPostPointsStandInsideEachOther`.

1.809 m is still under 2 m, so the tightest pairs in a *full* trench keep a small standing push. That is stated
rather than rounded away, and it only arises through the `room = 0` fallback, which only runs when the trench is
full — where men standing shoulder to shoulder is right anyway.

**This is the third time in this project that a number flattered the change that produced it** (the others: a
single-hue score climbing to 97% while winter went monochrome, and a coverage claim that was never checked against
the file it cited). The pattern is the same each time — the baseline was chosen after the change, not before.

### The approach that was tried and thrown away

Each man was first given a hashed *stretch of the line* to hold, scored against how far he would have to walk.
The arithmetic kills it: minimising `D² + AnchorPull·(A−D)²` walks a man two thirds of the way to his anchor,
which on a 300 m trench is 200 m of marching. The garrison was still crossing the map when the shooting started,
and two existing tests caught it (`AManAtTheParapetMansTheFireStep`, `Garrison_SpreadsAlongItsTrench`). Spacing is
a local problem; the cure has to be local. Recorded in the comment on `Roomiest` so nobody re-tries it.
