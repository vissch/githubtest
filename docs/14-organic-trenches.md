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
