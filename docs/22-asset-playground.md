# 22 Asset playground

Lane: SHOW. Branch `lane/show/playground` (2026-09-26). Code in `trench-warfare-3d/Assets/_Project/Playground/`.

The owner's ask: before a new unit, vehicle or building goes into the battle, try it on a test range. Test and
improve the systems (rigs, LODs, destruction, fire) on it first. The first two subjects are the owner's Tripo
tank ("Brute") and frog infantryman, each delivered at three polygon levels.

## Bringing in the next asset
1. Unzip the Tripo LODs to SHORT paths (their texture paths pass MAX_PATH) with the base colour beside each FBX as
   `<stem>_tex0_0.jpg`.
2. A vehicle: `tank3split.py` (docs/reference/pipelines.md). Its REGIONS table and cut planes were measured on this tank;
   a new design needs its own (render the islands first: every part must exist at every LOD, the script fails if not).
   A figure: `frogrig.py`; its joint table (J, SIDE) was measured on this frog's T-pose.
3. Put the output in `Playground/Art/Tanks/<Name>/` or `Playground/Art/Units/<Name>/`, run TW/Playground/Build, run the
   `TW.Tests.Playground` tests, then Play and `bash Tools/playground/round.sh r1`.
4. Give the sheets and the JSON to a critic agent (the brief this loop used: harsh, numbers not adjectives, verify every
   claimed fix against the new stills); fix, `round.sh r2`, repeat.

## What it proves, and how

| Promise | How it is kept | Where it is checked |
|---|---|---|
| The tank has the same parts at every LOD | `Tools/tank3split.py` classifies by REGION, not by Tripo's loose islands (they differ per LOD): an island goes whole to the region holding most of its area; the hull body is cut by fixed planes (turret ring, four casemate plates), cuts capped with the atlas' soot texel. Fails if a part is empty at any LOD. | `Every_Vehicle_LOD_Has_Every_Part_Of_The_Manifest` |
| ...in the same place | Small fittings Tripo placed differently per LOD (the antenna stood 0.8 m further back at LOD1/2) are snapped onto LOD0's centre; the moves are logged in `tank3.json` (`snapped`). | `Every_Vehicle_Part_Sits_In_The_Same_Place_At_Every_LOD` |
| It falls apart the same way at every LOD | One transform per part; the LOD swaps its mesh and atlas. Flight (`Runtime/Tumble.cs`) runs in the vehicle's own frame on the LOD0 box and a seeded stream, so no mesh and no world position enter the arithmetic. | `Three_Copies_At_Three_LODs_Fall_Apart_Identically` (pose signatures equal to the centimetre); the "LODs side by side" view |
| The frog's rig works on every LOD | `Tools/frogrig.py`: one Mixamo-named skeleton; LOD0 skinned by bone heat (geometric fallback, accessories ride rigidly, tunic leans on the hips); LOD1-3 weights TRANSFERRED from LOD0's surface, so a point bends alike at every LOD | `Every_Unit_LOD_Is_Skinned_To_One_Skeleton...`, the four-frog view |
| Simpler rigs on simpler LODs | LOD0/1 22 bones x4, LOD2 18 x2 (spine1, neck, toes folded in), LOD3 13 x2 on a 300-tri mesh decimated from LOD2, its weights transferred from LOD2 (its parent mesh), gated anatomically (far out on an arm only that side's arm bones; never a forearm or hand on the torso) | same test; `hand`/`foot` in each capture's JSON: how far the skin carries them from the bind pose, per LOD |
| A cheap far LOD | LOD3 has no UVs: the atlas is baked into its vertex colours (face-centre samples). Tripo's atlas is cut into so many islands that every UV seam splits a vertex: 121 positions imported as 298 vertices, none split by normals. Now 121. LOD1-3 carry LOD0's smooth normals (imported, not recalculated at 55 degrees) | the four-frog view |
| The game's clips play on it | `Runtime/Retarget.cs` samples the game's own Generic Mixamo clips on `Art/Characters/Soldier.fbx` (what VATBaker samples) and carries each bone's change from a canonical T-pose onto the frog's. No clip is copied or reimported. | `The_Games_Clips_Leave_The_Figure_Standing_On_Its_Feet` |

## Running it
`TW/Playground/Build` writes `Playground/PlaygroundLibrary.asset` and `Playground/Playground.unity` (never in the
build settings). `TW/Playground/Open and Play`, or open the scene and press Play. F1 toggles the panel; Space freezes;
H hits the hull; G fires; a click on the vehicle is an AP round there; right-drag orbits, wheel zooms, middle-drag/WASD
pans. Everything the panel does is a command string (`PlaygroundHost.Do`), so a script can drive it:
`TW.Playground.PlaygroundHost.Instance.Queue("vehicle.compare; seq")`, and `shot <abs path> <w> <h>` writes a still
plus a JSON report (luma, fps, flipbook cards, and per object: LOD, cost, stage, pose signature, screen position).

Views: `vehicle`, `vehicle.compare` (LOD0/1/2 side by side, the same hits), `unit`, `unit.compare` (LOD0-3),
`unit.squad` (a file from 8 m to 300 m at the battle lens), `mixed`. Looks: `biome NightMud|Winter|Lava` (the game's
`Atmosphere`), `cam close|standard|far|top`. The fire, smoke and bursts are the game's `FlipbookFx`, and the scrap is
the game's `DebrisRenderer`, so what the playground shows is what the battle will draw.

## Measured LOD pops (`lodpop`, 2026-09-26)
Each boundary rendered at its own switch distance at the battle lens, magnified from the same spot, from four sides;
an isolated silhouette pass (the object's layer only, black, no fog, no grade) gives the overlap. Worst side:

| Boundary | Tank IoU | Frog IoU |
|---|---|---|
| LOD0 -> LOD1 | 0.909 | 0.918 |
| LOD1 -> LOD2 | 0.927 | 0.857 |
| LOD2 -> LOD3 | - | 0.850 |

The frog's LOD3 size was chosen by sweep: 219 tris 0.823, 300 tris 0.850, 380 tris 0.870 (ships 300: 168 vertices,
under the far budget). Weighting the decimation to keep the cap and the gap between the legs made it WORSE (0.769):
the triangles come off the shoulders. `TW_LOD3_KEEP` keeps the option for the next figure. Re-swept 2026-09-27 on
the derived LODs: decimated from LOD2, LOD1 or LOD0 made no difference (0.850 / 0.848 / 0.840 at 300 tris), nor did
the rig (2 or 4 weights, hands kept or folded: 0.862-0.865 at 380). The count does: 380 tris 0.865, 460 0.868.

The simpler rigs kept the shoulders from 2026-09-27 (LOD2 18 bones, LOD3 13, still 2 per vertex). A shoulder carries
the whole arm, and folded into the upper arm it moved the arm's outline at every switch below LOD1. The frog's 1->2
switch went from IoU 0.945 to 0.958 and block colour 8.8 to 6.7, 2->3 from 0.849 to 0.853 (round r21, the tank
unchanged). Folding nothing at LOD2 gave 2->3 0.874 but left LOD2 22 bones and LOD3 17: no simpler rig left.
More LOD2 triangles instead (1,500 or 2,000) moved the pop down to 2->3.

A trap for any mesh sweep: a reimported FBX keeps its Mesh objects and changes what is in them, and the playground
keeps one cloth-masked copy per source mesh. Keyed on the Mesh alone, a sweep measured the first build over and over;
keyed on the counts, it measured whichever build first had those counts (most rig variants have the same counts). The
cache is now keyed on the contents (`UnitRig.Signature`: positions and skin). Rebuild the same settings twice and check
the numbers repeat before believing a sweep.

The colour side of a pop is split three ways, each with a floor (the same LOD turned one degree): `dcol_inside` (per
pixel, where both LODs cover), `dmean` (the change of the mean colour: all a tint can remove) and `dblock` (12-pixel
block averages: the change a player sees as colour rather than as a line moving). Round r17, mean of four sides, /255:

| Boundary | Tank dblock (floor) / dmean | Frog dblock (floor) / dmean |
|---|---|---|
| LOD0 -> LOD1 | 8.3 (1.4) / 2.6 | 11.4 (1.2) / 5.2 |
| LOD1 -> LOD2 | 4.6 (0.8) / 1.2 | 13.7 (1.2) / 4.8 |
| LOD2 -> LOD3 | - | 6.5 (0.5) / 0.8 |

Most of it is SHAPE, not paint: Tripo's LODs are separate sculpts, so lines and shading move. What each remedy bought,
measured on the frog (0->1 and 1->2, block shift and worst-side IoU):

| Frog LODs | 0->1 | 1->2 |
|---|---|---|
| Tripo's three, as delivered (default) | 11.1, 0.919 | 12.6, 0.855 |
| + per-LOD tint (`Runtime/LodTint.cs`, on for figures) | 11.4 -> mean 4.6 to 4.2 | mean 4.8 to 4.1 |
| Tripo shapes repainted from LOD0 (`TW_REBAKE=1`, a Cycles bake) | 9.6, 0.918 | 11.7, 0.857 |
| LOD1 and LOD2 decimated from LOD0 (`TW_DERIVE=12`) | 5.0, 0.968 | 11.3, 0.837 (the cap breaks) |
| **LOD1 decimated from LOD0, Tripo's LOD2 (`TW_DERIVE=1`)** | **5.0, 0.968** | 13.6, **0.876** |

**Decided 2026-09-27 (the owner): both models' LOD1 and LOD2 are now derived from LOD0** (`TW_DERIVE`, default 12 in
`frogrig.py` and `tank3split.py`; 0 keeps Tripo's own). Round r20 against r19:

| Switch | Tank IoU / block colour | Frog IoU / block colour |
|---|---|---|
| LOD0 -> LOD1 | 0.909 / 8.6 -> **0.967 / 3.1** | 0.918 / 13.4 -> **0.986 / 3.2** |
| LOD1 -> LOD2 | 0.923 / 4.4 -> **0.932 / 2.8** | 0.857 / 15.4 -> **0.944 / 9.1** |
| LOD2 -> LOD3 | - | 0.850 / 6.6 -> 0.848 / 5.4 |

Round r21 (the frog's lower rigs keep the shoulders, below): frog 1->2 **0.958 / 6.7**, 2->3 **0.853 / 3.3**.

Two things had to be fixed for it. The derived frog LOD2 at first skinned to 8 bones and its head sank with the arms:
`rigid_accessories` decides "body" by a vertex count, and at a quarter of LOD0's vertices the torso, legs and head fell
under it and rode the nearest arm. A derived LOD now keeps the weights transferred from LOD0 (its islands are LOD0's,
already made rigid there), and the line scales with the mesh. And the tank's thin antenna, collapsed to Tripo's
handful of LOD2 triangles, folded into a lump 0.3 m off its place: a small part keeps at least 64 triangles (the tank's
LOD2 is 1,307 triangles against Tripo's 1,170). Both LOD2s step down from the derived LOD1, not straight from LOD0.

The per-LOD tint that goes on is fitted on the render, not estimated from the mesh (the mesh estimate counts undersides
the camera never sees and overshot on both models). `lodfit` draws each LOD alone from the four lodpop sides, compares
its mean colour over the silhouette with the LOD the battle's standard view shows (78 m: the frog's LOD2, the tank's
LOD1), and puts the ratio on as a tint (two passes, 0.75-1.25); `round.sh` runs it first. Matched to LOD0 instead, the
far frogs came out darker than they are and a squad's contrast on the mud fell from 20.1 to 14.3 (`figure_gap`, r18);
matched to the seen LOD it is 23.0, and 22.6 in r19's m3 (u9's file of men: -4.5 -> 3.4). Round r18: the tank's mean shift is 0.5-2.0 at every switch and side (up to 4.6 before) and its 1->2
block shift 3.9 (4.6); the frog's 2->3 block shift 5.2 (6.5). The frog's 0->1 and 1->2 shifts stay side-dependent
(3-8): shading on a different sculpt, which no single tint removes - derived LOD1 is the fix there.

## Readability
`ground mud` swaps the metre grid for a dark warm mud; `team 0|1|split` puts the battle's side colours on (the tank's
lamps and antenna, where the game paints a tank's horns, and field-grey over the olive for side 1; a light wash on a
figure). Every capture with figures reports `figure_luma`, `ground_luma` and `figure_gap` through the silhouette pass
and a 4 px ring: 4.5 on the grid at the standard view, 24.4 on mud with the side colours (27.2 before the rings).

A vehicle with a side also gets the game's own side ring (`TW/TankDisc`, TankRenderer's size rule). `sidehue <path>`
measures whether the sides read apart: time frozen, the frame is drawn with only side 0 coloured, only side 1, and
neither, and each side's added colour is summed on the opponent-colour plane over its own (grown) silhouettes. m3 (mud,
tank side 1, squad side 0): hues 199 and 9, a 170 degree gap; strength 0.013 for the figures against 0.041 for the tank.
Reading the raw frame's hue said 5 degrees: the night grade turns everything blue, and against blue every model reads
orange. Rings under figures (`unitrings 1`) add a third to the figures' strength but cost a quarter of `figure_gap`, and
the game shows an infantryman's side on his cloth, so they are off.

**The game's ring painted the enemy colour over friendly infantry** (found here, critic r8): TankDisc's second pass
draws the ring faintly wherever something hides it, and that included a squad standing beside an enemy tank, and the
tank's own hull. Tanks and figures now set stencil bit 8 (`Tank_URP`, `VAT_URP`) and the hidden pass skips it; terrain
still shows the ring through. `sidehue` reports `side_cross` (the other side's colour over this side's men): 0.0225
before, 0.0040 after.

A figure's side colour goes on its uniform only, as the game's VAT figures do: `frogrig`'s frog gets a cloth mask in
vertex alpha at build (blue cloth read from each LOD's atlas, LOD3 from its vertex colours) and `Tank_URP` takes it when
`_TeamByAlpha` is 1 (default 0: the game's tanks and landing craft are untouched). `team split` puts half the squad on
each side. m3 (r19): the figures alone read 155 and 18 degrees, 137 apart, and the side colour RAISES their contrast
(`figure_gap` 18.0 without sides, 23.0 with; the drop to 14.4 in r18 was the LOD tint's reference, not the cloth).

**What the playground's readability numbers are worth, measured against the battle** (2026-09-27). With the owner's
leave to change the shared look, a moonlit lift, a rim in the side's colour at range and a cut through the field fog
were added to the unit shaders: in the playground they took a file of frogs 150-300 m out from -4.4 to +12.4 (the
`figure_gap_far` number). In the battle scene (`CaptureRig`, GreyboxCorridor, 8 men at zoom 60) the same change made
the men LESS distinct: contrast 0.160 without, 0.125 with the rim, 0.067 at double strength; the fog cut alone was
neutral (0.157). At night the battle's men read as dark shapes on lighter mud, and lifting them spends that. All of it
was reverted. The playground now reports the battle's own number beside its own (`contrast_median`, and `_far` past
60 m: each man's centre against the median of a ground ring sized to him), and `ground mud` is the battle's mud colour
with its own detail map. On it, at the standard view (r20 m3), the frogs score 0.37, about twice the battle's own men;
the low file of men (u9) looks along the ground into the haze and scores ~0.03 on any ground - a stress view, not the
game's.

## LOD distances (screen-height share of the bounding sphere)
- Figure: LOD0 above 0.20 (under ~22 m at the 25 degree battle lens), LOD1 above 0.08 (~63 m), LOD2 above 0.03 (the
  standard view's 78 m out to ~167 m: 963 imported vertices, 18 bones; LOD1's 1,924 are over the 1,200-1,500 budget
  for a crowd), LOD3 beyond.
- Vehicle: LOD0 above 0.70 (close-ups under ~45 m; 7.8k tris is over the 3-5k vehicle budget), LOD1 above 0.22 (the
  standard view's 78 m), LOD2 beyond ~145 m. The picker walks one level at a time with a 10 % margin at each cut (a
  jump of two levels once left a vehicle 240 m out at LOD0).

## Traps found here (they will bite the battle too)
- **Unity builds an LODGroup by itself** for meshes named `*_LOD0..n` in one FBX. That group, not the renderers'
  `enabled`, decides what draws: it silently hid LOD1-3 whatever the code asked. `UnitRig.Build` removes it; any
  loader of such an FBX must do the same, or name the meshes otherwise.
- **Tripo LODs are not the same object decimated.** Islands differ (LOD0 welds the fenders into the hull, LOD2 the
  turret), and small fittings move between LODs. Never match parts by island; match by region and check placement.
- A part balanced on a corner after a landing looks broken: `Tumble` only rests on a broad face (at least 0.6 of the
  largest). Three physics bugs were behind "the gun stands on its muzzle": it turned pieces about their PIVOT not their
  centre (a pendulum), it scrubbed spin on every step a piece lay on the ground (freezing every topple), and a box with
  the antenna's base really is stable on end. A piece stopped on a small face is now pushed over about its ground
  corner, at most 100 degrees and 3 times (an unaligned axis once rolled a stack 140 m).
- The house kit's cut faces read as flat brown card. `housesplit.py`'s fix_fill_uvs gives each corner of a cut face
  the UV of the first wall loop it finds, and the playground's first re-projection (`BuildingRig.CutFaces`) spread them
  from the wall's centroid into the atlas's dark gutter. `cutsdebug 1` draws the found faces flat magenta (a magenta
  vertex colour vanished under the dark texels). They are now anchored at the incentre of the largest OUTWARD wall
  triangle and kept inside its incircle: the atlas under the cut faces averages 65/56/56 against the walls' 65/53/51.
  The same fix is now in `housesplit.py` (`project_fill_uvs`, 2026-09-27; `TW_OLDCAPS=1` keeps the old way): each cap
  is projected flat on its own cut plane at half the chunk's texel density, anchored in the largest outward, masonry-lit
  (luma 0.2 and up) wall triangle, and earlier caps are tagged so a later cut never borrows from one. Cut from the kit's
  `Stones/WallStub` at `TW_CUT=1.2` the caps went from 3.15x the walls' texel density (one atlas strip squeezed across,
  red-brown streaks) to 0.52x (plain stone in the chunk's own colour). The chunks in `Resources/Env/*/Chunks` were cut
  before and are NOT regenerated: most sets' split settings are unrecorded (pipelines.md), and re-cutting changes every
  building in the game, so that is the owner's call (decisions.md, Open). What still reads as brown crates in a heap is NOT a texture fault: the texels
  under those faces are the Boilerhouse's own tan plaster (82/68/57, textured), and tan reads dark brown under the night
  light. A palette question for the house sets (decisions.md, Open).
- A lone corner slab (0.44 x 1.43 x 3 m) kept standing as a ruin anchor read as a post on end. Corners must now be
  stout (0.6 m thick, or no taller than three times their thickness); `A_Shelled_Building_Leaves_Nothing_Floating_And_
  No_Piece_On_End` holds it (it failed on the old rule, naming that slab), and each capture reports `on_end`. Buildings
  keep their own clock (`Advance`), not `Time.time`, so a test can step them.
- **The project renders in Gamma colour space.** Colours are compared as stored (atlas and vertex colours alike); a
  linear conversion made the far frog's tint clamp at its limit.
- A Cycles selected-to-active bake writes BLACK where a ray finds nothing, not the image's generated colour: `frogrig`'s
  rebake finds its misses as black texels whose own texture is not black (5 % at LOD1, 9 % at LOD2) and keeps those.

## Buildings
`building` / `set Ruins|Houses|Military` / `house <name>` / `shell` / `rebuild`: a building from the game's own kit
(`HouseKit.Load`, `Resources/Env/<Set>`), one GameObject a chunk, the battle's look (TW/Toon on the env atlas cell, the
kit's paint, outline 1.3). A shell knocks out every chunk it hurts past its strength (stone 60, timber 30) and throws
it; whatever loses all its supports (`HouseKit.Solve`'s RestsOn) comes down a storey at a time, 0.45 s apart
(PropDestruction's pace), on the same `Tumble` as a vehicle's parts.

## Not done yet
 the vehicle and figure are not yet in `Resources/` nor known to the sim (archetype,
`TankModel` 3-LOD support, a VAT bake of the frog through the retarget).
