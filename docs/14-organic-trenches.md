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

Elbow room is now offered **by degrees** — the widely spaced posts first (`Roomiest` = 2 cells = 6 m), then the
closer ones (4 m), then any free post at all. An empty trench spreads a garrison out, a filling one packs it down,
a full one stands men shoulder to shoulder rather than leaving them postless. In a packed trench the mean spacing
barely moves (1.77 → 1.90 m) and it should not: 84 men in 90 m of two-row trench are shoulder to shoulder by
geometry, not by policy.

### The approach that was tried and thrown away

Each man was first given a hashed *stretch of the line* to hold, scored against how far he would have to walk.
The arithmetic kills it: minimising `D² + AnchorPull·(A−D)²` walks a man two thirds of the way to his anchor,
which on a 300 m trench is 200 m of marching. The garrison was still crossing the map when the shooting started,
and two existing tests caught it (`AManAtTheParapetMansTheFireStep`, `Garrison_SpreadsAlongItsTrench`). Spacing is
a local problem; the cure has to be local. Recorded in the comment on `Roomiest` so nobody re-tries it.
