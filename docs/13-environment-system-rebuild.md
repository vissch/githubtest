# Environment system rebuild and three-round critique

2026-09-21. Presentation only. Northstar: [reference](reference/battlefield-northstar.jpeg).
All scored images use the standard 25-degree lens, 25-degree pitch, zoom 30 view.

## Independent visual scoring

One harsh critique agent completed exactly three scored rounds. Architecture and optimization earned no visual points.

| Criterion | Weight | Round 1 /10 | Round 2 /10 | Round 3 /10 |
|---|---:|---:|---:|---:|
| Terrain and trench form | 30 | 3 | 4 | 5 |
| Material and painted detail | 25 | 2.5 | 3 | 4.5 |
| Composition and storytelling | 20 | 2.5 | 2.5 | 4.5 |
| Atmosphere and values | 10 | 4 | 4.5 | 4.5 |
| Effects | 10 | 3 | 3 | 3 |
| HUD | 5 | 4 | 4 | 4 |
| **Weighted total /100** | **100** | **29.25** | **34** | **44.75** |

Evidence: [baseline](reference/environment-standard.png), [round 2](reference/critique-round2.png),
[round 3](reference/critique-round3.png), [round 3 HUD](reference/critique-round3-hud.png).
Effects and HUD scores were held unchanged in rounds 2 and 3. HUD grading concerns compatibility with the illustration,
which does not itself contain a HUD. Soldier positions vary between captures; camera and environment comparison remain fixed.

Round 1 identified camouflage-like ground, thin straight timber channels and isolated box shelters.
Round 2 recognized more earth mass but rejected repeated bank strips and unmarked material faces.
Round 3 recognized continuous earth shoulders, material-specific marks and connected shelter compositions.
The critic explicitly concluded that reference-level beauty has **not** been achieved.

## Rebuilt generation pipeline

| System | Responsibility |
|---|---|
| `BattlefieldSurface` | Read-only trench boundaries, link exclusions, depression classification and continuous world-coordinate earth profiles |
| `GreyboxTerrainView` | Half-metre presentation mesh, shared surface pigmentation and queued terrain repainting |
| `RenderGround` | Triangle-matched presentation height grid shared with Burst unit placement and combat effects |
| `BattlefieldGeometry` | Reusable bevelled, worn solids |
| `BattlefieldPigment` | Shared mipmapped timber, bark, canvas, concrete and earth markings |
| `BattlefieldKit` | Owned mesh/material library, compound modules, part-oriented UVs and public module construction API |
| `BattlefieldBlueprint` | Named sockets, entrance, structural foundation and footprint derived from actual transformed meshes |
| `BattlefieldComposer` | Seeded boundary-derived site search, setbacks, readable entrance orientation, clearance checks and board approaches |
| `BattlefieldProps` | Spatial instance pages, culling, rendering and event-driven rebuilds |

The old fixed z-min/z-max shelter placement and separate repeating bank strips have been removed.
Banks are part of the continuous rendered terrain. Trench and link floors retain their simulation heights.
Units and effects sample the rendered ground so raised decorative shoulders do not bury their feet or origins.
No simulation height, traversal, cover or replay data is changed.

## Making additional content

After `BattlefieldProps.Start`, use its `Kit` to obtain owned primitives with `WornBox`, `Blob` or `Taper`.
Call `CreateModule(name, pigment, tint, parts)` with mesh, position, Euler rotation and scale tuples.
The kit combines parts, aligns grain to each part, generates outline normals and shares pigment textures.
Assemble modules into `BattlefieldBlueprint.Socket` entries, with a structural socket named `shell` first.
Use `Grounded = true` for loose props that should rest on the sampled terrain.
Pass the templates to `BattlefieldProps.UseBlueprints(templates, seed, preferredFront)`.
The renderer automatically registers new modules; callers do not need to modify batching or site placement.

The default preferred entrance direction is +X, matching the established standard viewing side.
Entrances remain splayed toward their associated trench. This direction is configurable for other level presentations.
An explicit layout regeneration can be requested through `BattlefieldComposer.Build(..., regenerateLayout: true)`.
Normal combat updates validate existing sites instead of searching the whole map or relocating shelters.
Invalid sites are removed; decoration does not become gameplay cover or a usable interior.

## Budget and verification

- Standard art sample: 752 submitted environment instances, 118,923 base prop vertices, 100 instanced submissions.
  Counts exclude terrain, outline/shadow passes and units. The 90 x 240 m ground has approximately 90,000 vertices.
- Ground pigment remains 8 texels/metre, approximately 7 MiB with mipmaps. Five shared 256-square RGB pigment maps
  add approximately 1.25 MiB; the presentation height grid adds approximately 0.35 MiB.
- Overlapping crater repaints share queued 2 m tiles. A 2 ms scheduling target permits the current tile to finish;
  it is not a hard frame-time ceiling. Hollow classification is coalesced once per affected frame.
- `AgentScripts/EnvironmentAudit.cs` checks independent regeneration, cached updates, transformed placement clearances,
  unchanged map hashes and unchanged trench/link floors on seeds 1917, 2054 and 2191 at three map sizes.
- Recorded cached composition updates: 2.95–5.63 ms on this machine. Twelve overlapping synthetic presentation stamps
  coalesced to 196 tiles and drained in 512 ms; peak observed tile work was 4.13 ms/frame. These figures exclude texture
  uploads and terrain mesh rebuilding. Synthetic stamps never enter the simulation event stream.
- Captures and measurements use an RTX 4070 Laptop GPU. GTX 1050 frame timing remains unverified.
- Final validation: `python validate.py`, all 57 EditMode tests and all 3 PlayMode tests passed with the editor closed.
  The final live console check had zero errors or warnings. Final stress review reported 3,001 alive (the stress
  population plus one transient scripted peer unit), no desync, 1,291 drawn and 1,199,339 unit vertices with shadows
  disabled by the existing budget guard. The scene's normal settings were restored before closing the editor.

## Biggest remaining wins

1. Extend the surface vocabulary with broken erosion cuts, uneven crest highlights and stronger rims around actual
   depressions. Long, smooth banks and nearly featureless open ground remain the largest visual gap.
2. Add relationships between templates: repaired frontage, damaged emplacement, supply route and shell-damaged ground.
   Current small sites remain isolated; larger compositions need density and damage hierarchies while preserving lanes.
3. Expand material-specific wear and silhouettes. Roof earth still resembles draped sheets; concrete, sacks and timber
   need less repetitive damage and more distinct construction. Two stock shelter templates are only a starting vocabulary.

Changing the actual winding trench network, traversal or functional bunker interiors is outside this presentation rebuild.
