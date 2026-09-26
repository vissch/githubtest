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
| Simpler rigs on simpler LODs | LOD0/1 22 bones x4, LOD2 16 x2 (spine1, neck, shoulders, toes folded in), LOD3 11 x2 on a 300-tri mesh decimated from LOD2, its weights transferred from LOD2 (its parent mesh), gated anatomically (far out on an arm only that side's arm bones; never a forearm or hand on the torso) | same test; `hand`/`foot` in each capture's JSON: how far the skin carries them from the bind pose, per LOD |
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
the triangles come off the shoulders. `TW_LOD3_KEEP` keeps the option for the next figure.

## Readability
`ground mud` swaps the metre grid for a dark warm mud; `team 0|1|split` puts the battle's side colours on (the tank's
lamps and antenna, where the game paints a tank's horns, and field-grey over the olive for side 1; a light wash on a
figure). Every capture with figures reports `figure_luma`, `ground_luma` and `figure_gap` through the silhouette pass
and a 4 px ring: 4.5 on the grid at the standard view, 27.2 on mud with the side colours.

## LOD distances (screen-height share of the bounding sphere)
- Figure: LOD0 above 0.20 (under ~22 m at the 25 degree battle lens), LOD1 above 0.08 (~63 m), LOD2 above 0.03 (the
  standard view's 78 m out to ~167 m: 892 imported vertices, 16 bones; LOD1's 1,924 are over the 1,200-1,500 budget
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
- The house kit's cut faces read as flat brown card: measured, not a UV fault - the kit's texture is ~23 px a metre at
  building size. An art item for the house sets, not a playground one.

## Buildings
`building` / `set Ruins|Houses|Military` / `house <name>` / `shell` / `rebuild`: a building from the game's own kit
(`HouseKit.Load`, `Resources/Env/<Set>`), one GameObject a chunk, the battle's look (TW/Toon on the env atlas cell, the
kit's paint, outline 1.3). A shell knocks out every chunk it hurts past its strength (stone 60, timber 30) and throws
it; whatever loses all its supports (`HouseKit.Solve`'s RestsOn) comes down a storey at a time, 0.45 s apart
(PropDestruction's pace), on the same `Tumble` as a vehicle's parts.

## Not done yet
 the vehicle and figure are not yet in `Resources/` nor known to the sim (archetype,
`TankModel` 3-LOD support, a VAT bake of the frog through the retarget).
