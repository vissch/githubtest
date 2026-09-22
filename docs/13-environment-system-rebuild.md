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

## Imported environment sets (2026-09-22)

The owner supplied six Tripo sheets (`Downloads/env sets`; ten zips, four of them duplicates). Each sheet is one mesh
with its props laid side by side at an angle, normalised into a 1 m square with one 4096² texture.
`Tools/envsplit.py` (Blender 5.0, background) groups loose parts into props where their footprints overlap (the plant
sheet stands its props in two rows, so it groups by 3D boxes). It then squares each prop to the axes and turns its
front to Unity +Z, scales it to metres, puts the pivot at the middle of its base and exports one FBX per prop to
`Assets/_Project/Resources/Env/<Set>/`. One stray three-triangle card in the fence sheet is dropped.
`Tools/envgrade.py` grades each set texture to 2048 and pulls it into the field's muted range (greens hardest, poppy
and rust reds kept): ungraded, the gabions read as orange flower pots and the moss as lime.
`Editor/EnvKitImport.cs` sets the imports up: no materials or animation, normals recalculated at 60°, and outline
normals in UV3 plus painted-form vertex colours, as `BattlefieldKit.Combine` gives the procedural modules.
The Blender FBX exporter with a baked space transform always lands Blender −Y on Unity −Z, so each prop is turned
half round before export. The first import faced every gun backwards; this was checked by where the barrel vertices lie.

| Set | Prop | Mesh size (m) | Where it goes | How often |
|---|---|---|---|---|
| Siege | MGNest | 1.7 × 1.9 × 2.6 | On the lip of each fire trench's enemy-facing parapet, gun out, ≥ 6 m from a ladder | 1 per fire trench |
| Siege | Pillbox | 3.4 × 2.1 × 3.3 | Shell of the "Concrete pillbox" site blueprint, on the fog side (see below) | 1 per side |
| Siege | SodShelterRuin | 2.4 × 2.4 × 2.4 | Shell of the "Sod-roofed shelter" site blueprint, on the fog side (see below) | 1 per side |
| Siege | ArmouredStand | 2.3 × 2.8 × 2.1 | Along each rear edge and beyond the far edge (see below) | 4 per side |
| Siege | Well | 2.1 × 2.5 × 1.7 | Behind one line, near the map's rear edge | 1 |
| Weapons | FieldGun | 1.4 × 1.8 × 2.7 | Both flanks of each rear edge, laid toward the enemy, shells and a sack beside | 2 per side |
| Weapons | TankTurret | 1.5 × 1.05 × 2.3 | Blown off beside a wreck (`PropKind.Wreck`), canted | 60% of wrecks |
| Weapons | Biplane | 5.6 × 3.3 × 4.5 | Crashed nose-down just beyond the far map edge in no man's land | 1 |
| Weapons | ShellStack | 1.2 × 1.0 × 1.1 | By the field guns and the sod shelter | Few |
| Weapons | WreckedLimber | 1.6 × 1.3 × 1.8 | By one field gun per side | 1 per side |
| Weapons | DudShell | 0.7 × 0.95 × 0.7 | Nose-down in shell holes (dry 30%, flooded rims 14%) | Few |
| Stones | Gabion | 1.2 × 0.95 × 1.2 | Pairs on the parapet lip (one length in eight), at both new sites | Common along trenches |
| Stones | Sandbag | 0.8 × 0.36 × 0.5 | Fallen by trenches (debris), at sites and MG nests | Common near trenches |
| Stones | Boulder | 1.3 × 1.0 × 1.1 | Medium member of the gathered clumps, at 0.4–0.7 scale | Common |
| Stones | RebarSlab, WallStub | 1.6 × 0.6 × 1.6, 2.1 × 2.3 × 1.3 | Rubble at the pillbox; stubs and slabs round the horizon ruins | Sparing |
| Wood | BracedPlank | 1.5 × 0.44 × 0.7 | Lying in the mud everywhere (debris), bedded 10 cm | Everywhere |
| Wood | PlankDoor, CrossedBoards | 1.4 × 1.8, 1.5 × 1.5 | Laid flat and thinned as debris away from trenches | Occasional |
| Wood | CorrugatedSheet | 2.0 × 1.35 × 1.3 | Arch-up, half sunk, by the trenches | Occasional |
| Wood | HatchLid | 0.85 × 0.3 × 0.8 | Small kit (close camera only), by trenches | Rare |
| Fence | WireFence, TimberHedgehog, Stakes, WirePost | 1.3–1.6 wide, 1.3 high | Wire cells only; half the belt stays old knife rests | Every wire belt |
| Fence | StoneBarricade | 1.7 × 0.95 × 1.1 | Round the horizon ruins | Sparing |
| Plants | GrassClump | 1.05 × 0.55 × 1.07 | The cattail clump's blades alone: small member of every clump | Everywhere (24%) |
| Plants | Cattails | 1.2 × 1.3 × 1.2 | The big member of 65% of the reed stands at wet margins | Wet ground only |
| Plants | Poppies | 0.45 × 0.6 × 0.4 | Small member of clumps | 5% of small members |
| Plants | FallenLog | 2.4 × 1.4 × 1.4 | Replaces the cylinder for `PropKind.Log` | As the sim places logs |
| Plants | SplitStump, SplitStumpTall, MossStump | 0.7–1.0 wide, 0.85–1.5 high | Mixed with the old stump for `PropKind.Stump` (30% old); moss stump also a clump member | As the sim places stumps |

Rules kept from the rebuild: nothing writes MapData, gives cover or blocks a man. The big pieces therefore keep to
ground men do not cross in normal play: the parapet between ladders, the rear corners off the road, outside the map,
or the site blueprints (already accepted as decoration). `Room()` rejects any footprint touching a trench, ladder,
wire or blocked cell, water, wet ground, a slope over 0.9 m or a shell hole. Everything else goes through the existing
hash-placed scatter rules, so a rebuild never moves it. Kept: every procedural module except the plain cylinder log.
Replaced in part: stumps, reeds (cattails lead the stands), knife rests (half), parapet bags (one length in eight),
tufts and stones (grass among them), loose boards (braced planks among them).

Cost at the standard view (zoom 30, three framings along the field), with the imported modules against the same
scene with them hidden: +370 instances, +125k submitted prop vertices (205k → 330–340k), +65 draws (195–212 → about
270). The imported modules use 64 m pages (`Module.PageSize`). With 32 m pages they cost +120 draws; with 96 m pages,
+40 draws but +160k vertices from coarser culling. The largest per-instance meshes: ShellStack 763, Cattails 573,
MossStump 517, Poppies 414, Sandbag 390, WireFence 380, GrassClump 328 vertices. GTX 1050 timing still unmeasured.
Captures: [split props](reference/env/props-review.jpg), [grade before/after](reference/env/grade.png),
[in game, close](reference/env/in-game-close.jpg), [crashed biplane](reference/env/biplane.png).

## Hand placement and kind looks (2026-09-22)

The imported props can be edited by hand. In Play, click one in the Scene view, then move, rotate or scale it with
W, E and R. Delete removes it and Ctrl+D copies it. The picked prop becomes a stand-in GameObject (`PropHandle`) while
selected and goes back into the batched draw when deselected. Every change is saved at once to
`Resources/Layouts/Battlefield<seed>.asset` (`PropLayout`) and applied in the game whenever the battlefield is built.
A generated prop is matched by its kind and the spot the composer gave it, so an edit survives the rebuild after every
crater, but lapses if a rule change moves the prop. Picking is custom (`EnvPropEditor`): Unity's own pick sees only
the terrain, so the tool makes the terrain unpickable while it is on and tests the click against each prop's mesh,
stopping at the ground. The TW > Env Props window has the on/off switch, adding a prop, clearing all edits, "Learn
looks from my edits" and a look for every kind.

A kind's look (`PropLayout.Look`, applied by `BattlefieldProps.Styled`) sets:
- a baseline scale in place of the composer's;
- a range each prop varies around it, hashed per prop, so a battlefield always looks the same;
- a turn with its range;
- a lean tipped either way;
- a sink that grows with size.

The composer spaces the pieces round a prop by its kind's size (`Module.Size`): MG nest sandbags, the field gun's
shells, sack and limber, and the blueprint sockets and footprints. The owner's first round of edits was learned
this way:

| Kind | Baseline scale | ± | Sink | Other |
|---|---|---|---|---|
| ArmouredStand | 2.71 × 2.61 × 2.71 | 18% | 2.0 m | turn ±12° |
| FieldGun | 3.5 × 2.83 × 5.58 | 18% | 0.59 m | turn ±6° |
| SodShelterRuin | 2.97 | 10% | 0.25 m | turn ±10° |
| MGNest | 2.18 | 10% | 0.44 m | turn ±6° |
| Sandbag | 2.99 | 10% | 0.33 m | |
| SplitStump | 2.79 | 10% | 0.06 m | |
| FallenLog | 1.78 | 10% | 0.35 m | roll 36° either way ±25% |
| Gabion | composer's | | composer's | pitch 28° either way ±25% |

The owner also gave placement rules:
- **Bunkers and field guns** go at each side's back edge or out on the far side, where the fog lies (the side the
  standard view looks toward, x < 0), never in the open between the lines.
- **Field guns:** two per side, 2.5–9 m inside the rear edge on both flanks. The trail may run off the map.
- **Observation stands:** four per side. Two stand along the rear edge, half of them off the map; two stand 3–10 m
  beyond the far edge in the back 30% of the side.
- **Pillbox and sod shelter** (`PlaceSites`, first pass): on each side's trench stretch nearest the far edge, behind
  the line. The opening is splayed towards the fog and the back faces the field, as the owner turned the shelter.
- **Room checks:** `Room()` takes off-map ground for the stands and guns. A big footprint is allowed more rise, and
  the enlarged MG nest a metre of rise per unit of its scale, since it hangs over the parapet's fall.

The generated shelter lands 0.7 m and 10° from where the owner put theirs, and one generated MG nest 0.2 m from theirs.

The owner's edits of the rule-driven kinds (one moved and three added stands, two guns, the shelter, its sack and the
MG nest) were removed once learned. The rules now place those kinds on both sides. Their gabion, stump and log edits
stay as hand placements.

Captures: [rear edge](reference/env/looks-rear.jpg), [fog side](reference/env/looks-fog-side.jpg),
[MG nest](reference/env/looks-mg-nest.jpg).
