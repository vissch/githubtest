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

## The second batch: an ambulance, a walker and a flyer (2026-09-27)
Three more machines, each a Tripo model at two or three levels of detail, all in the vehicle bay (`Playground/Art/Tanks/`)
so `VehicleRig` breaks, burns and cooks them off the way it does the tank, the same at every LOD:

| Machine | Source | Tool | LODs (tris) | Pop 0->1 / 1->2 (worst-side IoU / block colour) |
|---|---|---|---|---|
| **Mercy**, a field ambulance | `ambulance jeep` 6,236 + `green toy jeep` 880 (the same design; the two `green toy jeep` zips are identical) | `jeepsplit.py` | 7,023 / 2,551 / 1,214 | 0.979 / 4.2, 0.907 / 8.1 |
| **Croaker**, a two-legged frog mech | `steampunk+frog+robot` 9,738, `mecha frog` 3,702, `mech+robot` 1,008 | `mechsplit.py` | 9,738 / 3,687 / 1,010 | 0.968 / 2.1, 0.900 / 0.9 |
| **Skimmer**, a hovercraft (fan, machine gun, four pods) | `military tank 3d model` 7,098 (turned 90 degrees: `TW_TURN=90`), `(2)` 2,976, `(1)` 1,607 | `mechsplit.py` `TW_KIND=hover` `TW_LOD2_TRIS=1400` | 7,098 / 2,965 / 1,391 | 0.987 / 2.0, 0.963 / 1.3 |
| **Hopper**, a frog gunship on two ducted engines | `green+tank (1)` 5,903, `green+cartoon+tank` 3,297, `green+tank` 993 (`green+tank (2)` bundles the three) | `mechsplit.py` `TW_KIND=flyer` | 5,903 / 3,288 / 985 | 0.990 / 2.2, 0.967 / 1.9 |

(The Brute measured 0.967 / 2.9 and 0.932 / 1.8 in the same session.)

- **LOD2, derive or Tripo's:** measured, not assumed. The mech's LOD2 derived from LOD0 switches at 0.900; Tripo's own
  1,008-triangle model at 0.791 (a different sculpt). The ambulance is the other way: LOD0 decimated to 13-20 % of its
  triangles tore into spikes and holes at every budget tried (965 to 1,742 triangles: the roofs and the box's thin panels
  collapse first), so its LOD2 is Tripo's clean 880-triangle retopology, split by the same rules. What Tripo welded into
  that body (the rear doors, the back fittings) is carved out of the box each part fills at LOD0, and its open-backed
  tyres are closed. Its 8.1 block colour at 1->2 is the other texture bake, as with the first frog.
- **Cut outlines are filled, not fanned:** a fan round the centre of a concave cut outline overhangs it, and the overhang
  showed on the outside of the cab and box as black wedges. Only outlines the cut made are filled; a window a plane cuts
  through stays open.
- **The walker** (`Runtime/WalkerDrive.cs`) runs on the battle's own `WalkerGait`, fed a leg rig built from the manifest's
  pivots: where the feet go, how high the body rides and how it tilts. The legs are NOT solved by `WalkerGait.Solve`: it
  puts a knee on the side that raises it (a crab's leg, up and out), and a biped's knee has to go forward, as this one was
  modelled. The gait is given each leg as one piece from hip to toe as modelled, so the machine stands as sculpted; taken
  at full stretch it stood the frog up straight-legged. Two legs means one foot up at a time (the gait's own rule). A
  leg shot off is reported to the gait as lost and it limps. `walk 2.5` walks it round a circle; `walk 2.5 1` on the
  spot, to look at. **For the battle:** `WalkerGait.Solve` needs a knee-forward option before a biped can go in.
- **The flyer** (`Runtime/FlyerDrive.cs`) keeps the rig on the ground and lifts only the Hull, in the rig's own frame, so
  a part shot off in the air starts its flight where it was and falls to the ground. It hovers at 14 m with a bob and a
  sway; `fly 8` circles it banked into the turn. Knocked out, it falls nose down and turning and lands on its skids,
  where it burns and cooks off like the tank. The sim has no flying unit yet: that is the owner's call.
- **The hovercraft** is the flyer's drive held 0.45 m off the ground: a small bob and roll, its fan spinning (and winding
  down when it dies), spray blown out from under its pods (thicker on the move), and knocked out it settles flat.
- **Loop 2 (critic every cycle, `Tools/playground/round2.sh` + `score.py`):** the scripted hits fall back to a machine's
  own first tier-1/tier-2 part (they hit the Hull of anything without a tank's plate and track, and the Croaker went
  straight to the cook-off); overkill no longer cooks off in the same breath as the knock-out; a flyer's HE bursts
  beside it in the air. Parts torn off early are scorched, and the cook-off blackens everything already lying about
  (clean claws, pods and shrouds drew the eye first in every wreck). A walker's or flyer's `Centre` (report, LOD pick,
  labels) is its hull's. The Mercy's LOD2 is Tripo's mesh painted with LOD0's colours (a Cycles bake): 1->2 block colour
  11.0 -> 4.2-4.6. The walker runs its gait at a third of the world's size (`GaitScale` 3): WalkerGait times its steps by
  the pace in m/s, tuned on crabs lifting two or three legs at once, and on two legs it took four 0.4 m steps a second;
  now planted feet cover 0.7 rig units, centred under the hip (`Lead` 0.3), the body at its modelled height (`Reach` 1.1),
  measured with the stride probe (`WalkerDrive.FootMinZ/MaxZ`). The body also rises as a foot passes, leans over the
  standing foot and turns its hips with the stride.
- **Loop 2, cycles 3-10 (the critic's findings, each measured before and after):** a walker took its spawn only when
  `startRot == default`, which Unity's quaternion `==` never satisfies (it compares a dot product), so every walker walked
  from the world's origin - a test now spawns one 200 m out. The Croaker's fingertips hung in the air under the swinging
  arm (sorted into the hull); a dying walker's feet skidded into the splits (WalkerGait does that to a crab's rigid legs)
  and now stay put while the knees fold; stopped, it steps back onto both feet. A cook-off leaves on the hull what the
  machine stands, walks or flies on and the body round the crew (wrecks were bare boxes); every throw is at the machine's
  own `fling` (the gunship's farthest piece 25 m -> 13 m, the tank's 20.6 -> 14.7 m); a part riding a thrown part goes
  with it. A vehicle that loses a wheel or track tips 4-6 degrees about the gear beside it, which stays on the ground,
  and what that lifts droops back down (first version tipped the wrong way and sank the good tyre - a test guards it).
  A flyer that loses a wing or engine comes down, banks 15-30 degrees in a turn, crashes nose-in onto the side it lost,
  and burns before it cooks off (overkill cooks off only after half the cook-off delay). The tank's turret base disc
  (a piece standing on the deck) goes with the turret; it floated once the plates were thrown.
- **Loop 3 (settling and the flyer's ring):** a thrown piece that stops tilted on an edge, or on a small face with its
  three topple pushes spent, is laid over flat onto the broad face nearest down before it rests (`Tumble.Settling`,
  150 degrees a second about its centre). A piece tumbles as the tighter of its axis-aligned box and a box on its LOD0
  mesh's principal axes (`VehicleRig.PrincipalBox`): the antenna's axis-aligned box is three times its own, and "flat" on
  it had the antenna, the Croaker's jaw and the Mercy's stack lying 23-35 degrees off their length. Before, 12 pieces over
  three seeds rested up to 41 degrees off a face or stood on end; after, none of 391 loose pieces over 17 seeds, on all
  five machines. `Every_Machine_Keeps_Its_Thrown_Parts_Near` holds the lying-down rule on every machine. The tank's
  "plates" are not plates: their boxes are 2.4-3.5 m on every side and their largest flat face is 19-24% of their area,
  so they rest on a face as the chunks they are. A flyer is tied to its ring by a line in its side's colour, at least
  2.5 px wide at any range (`PlaygroundFx.Tether`); a crashed flyer rolls 12 degrees (was 22) and digs its lowest hull
  corner 0.6 m in, so its belly meets the ground along a side, not on one corner. A fire's light grows with the fire
  (`PlaygroundFx.SetPeak`: the lamp loop wrote its full strength over the rig's every frame, so a first flicker lit the
  machine at full). The Croaker's far LOD is derived at 1,443 triangles (was Tripo's count, 1,011; the far LOD's cap is
  1,500): its 1->2 pop 0.899/4.0 -> 0.937/2.9.
- **Open:** the Mercy's LOD1->2 colour (7.0) is the weakest pop; its rear doors carry a dark and a light triangle that
  are Tripo's own painting (the source model renders them the same), not a split fault: repainting is the owner's call,
  with the palettes (Skimmer, Croaker at night) and the ruin's flat brown faces.
- **Measuring on this bench, lessons:** m3's contrast flips 0.24/0.44 frame to frame on one build, so the round takes nine
  frames and scores the median (floor 0.08); a vehicle's block colour wanders 0.2-1.0 on an unchanged model; the editor's
  fps is not a signal. `round.sh` selects the tank itself (a round once scored the hovercraft as "the tank").
- Scales: the mech 5.8 m tall and the gunship 8 m long at `size 1`, the ambulance 4.6 m long. The battle draws walkers
  2.5x and tanks 1.7x (`VehicleSize`); how big these should be beside them is the owner's call too.

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

## Into the battle: the Skimmer and the Salvo (2026-09-28)
The Skimmer and a new half-track rocket truck, the Salvo, became battle units (archetypes 19 and 20, defined in
`Sim/Match/UnitDefinitions.cs`, fielded by no faction: the Unit Sandbox spawns them). Their battle copies are written
by `Tools/mechsplit.py TW_BATTLE=1` (commands in `pipelines.md`). The loop below judged them off those FBXs with
`Tools/battlelineup.py`: a lineup beside the Maw (drawn 1.7x), the Pincer (2.5x) and a 1.8 m man, and each machine's
LOD0 -> LOD1 pop from four sides, EEVEE, the normals smoothed at 55 degrees as `Editor/TankImport.cs` has Unity do.
Worst side, silhouette IoU / block colour (mean |RGB| of 8 px block means, 0-255), full frame and at the battle's
scale at the switch (85 px, 3 px blocks). Images: `docs/reference/figures/battle-loop/`.

| Round | What was seen | Changed | Skimmer | Salvo |
|---|---|---|---|---|
| before | the lineup came apart (the Pincer's turrets and the Skimmer's gun floating: `lineup-before-fix.jpg`) | `battlelineup.py` undoes the exporter's node turn exactly as TankImport does, nothing more (a first rule also moved the parts below a turned part, and every mesh sat mirrored about its pivot); checked on every Skimmer part against the manifest to 0.01 m | - | - |
| r0 | all four assemble (`lineup-r0.jpg`): noses left, the Maw 9.3 m long, the Pincer 9.0, the Skimmer 7.0, the Salvo 8.0, a man 1.8. At LOD1 the Skimmer's fan ring is an octagon, the Salvo's tyres too, its rocket box blotchy, its legs spiky | nothing (baseline); smoothing as Unity does moved the numbers by 0.3 at most | 0.954 / 9.2 | 0.957 / 9.5 |
| r1 | | `TW_KEEP`: the fan ring keeps 50 % of its LOD0 triangles, the fan 35 %; the Salvo's tyres 40 %, its rocket box 35 %. The ring and the tyres are round again | 0.976 / 8.8 (85 px 8.5) | 0.966 / 7.9 (7.4) |
| r2 | the Skimmer's camouflage and rim smear on the hull; the box is clean | both hulls keep 30 % | 0.977 / 8.1 (7.8) | 0.975 / 7.3 (7.1) |
| r3 | the Salvo's legs and joint balls still faceted | per-piece floor 20 (Salvo), 16 (Skimmer) | 0.978 / 8.1 (7.7) | 0.981 / 6.2 (6.0) |

Shipped with round 2's settings: the far LODs are 1,840 (Skimmer) and 2,539 (Salvo) triangles. Round 3 bought the
Salvo 1.1 of block colour for 650 more triangles (3,188, more than any walker's near LOD) and the Skimmer nothing, so
the loop stopped there. On this bench the shipped walkers measure Kettle 0.992 / 4.2, Redoubt 0.972 / 6.2, Pincer
0.951 / 11.1: both new machines sit inside the shipped range and, at the battle's scale, inside the playground's 1-8.
What is left of the Skimmer's 7.8 is its camouflage spots smeared by the decimation (UVs move with the collapsed
vertices); a UV-preserving decimation would be the next step, not more triangles. The coordinator's first run of the
tool (Workbench, 13.3 / 18.5) is not comparable: flat studio light and no smoothing.

**The numbers, against the machines that ship.** The Skimmer (220 silver, 1,300 hp, 3.4 m/s, 8 mm front, a machine
gun at 130 m) is the cheapest and quickest machine and the thinnest: it hunts men in the open and mud and trenches do
not slow it, but it cannot hurt a machine, any tank gun holes it anywhere, and a 37 mm Tusk (260) kills it. The Salvo
(380, 2,000 hp, 1.8 m/s, rockets to 380 m, blind inside 60 m, 360 over 7 m every 16 s) outranges everything (the
Pavise's gun reaches 360 m) but throws about half the Kettle's HE a second (Kettle: 320 every 6.5 s), cannot answer a
machine that closes inside 60 m and cooks off easily (ammunition risk 0.7). Neither should dominate; the Salvo may
read as weak for its price, which Play will tell.

**The salvo.** First drawn over the sim's one shell (which burst on the firing tick, 1-3 s before the drawn rockets
landed); then made the sim's own: the gun fires twelve rockets, each held by `TankGunnerySystem` until its own land
tick and announced by a `RocketFired` event with its launch and flight ticks and its landing point, so
`Presentation/Camera/TankRenderer.Salvo.cs` flies each from its tube on the sim's clock and it reaches the ground on
the frame its burst is drawn (format v11, `02-contracts.md`). The rack does the shell's harm, spread wider (decisions.md).

**Left for a look in Play:** each rocket reaching the ground on the frame its burst shows; its trails and bursts at night; the Skimmer's
turret following its machine gun's target and its fan spinning; the Salvo's wheels rolling; both far LODs at 170 m.

### The critic's round, in the battle (2026-09-28)
A critic looked at both machines live in GreyboxCorridor at night. Each finding, what it was, what changed, and how it
was checked in the real scene (Play in GreyboxCorridor, `TankCapture.Spawn`, `CaptureRig`); images under
`docs/reference/figures/battle-loop/critic-*`.

| Finding | Cause | Changed | Checked |
|---|---|---|---|
| The rack read as a dotted line of sparks (`critic-rack-before.jpg`) | a Flash card per frame at the rocket, a puff every 35 ms | `TankRenderer.Salvo.cs`: a rocket body (an 8-sided mesh along its velocity), its motor's flame along the flight, the trail laid by distance (a puff every 0.7 m, one ribbon at any speed), a flash and smoke at each tube, the back-blast cloud and dust at the rack, earth thrown up and dust where each lands, on top of the sim's own burst | 18 stills through one rack (`critic-rack-after-series.jpg`): sixteen arcing smoke ribbons, the back-blast, the bursts (`critic-rack-after.jpg`) |
| Rockets left a 4 x 4 guess at the tubes, for a rack of 12 | no tube positions in the model | the split writes `Socket_Tube00..15`, one at each of the model's sixteen tube mouths; the sim fires 16 rockets (was 12). Harm per shot unchanged: 23,907 hp off 25 men over 20 seeds (the shell 23,819), 163 killed | the manifest: 16 mouths 2.31-3.14 m ahead of the trunnion, hung off the Hull (a socket under the Gun is three nodes deep and came out of the import 1.5 m ahead: the gate caught it) |
| No team colour | the two drew in their own paint | `TankRenderer.SideColourOn`: the Skimmer's fan ring and pods wear the side's colour at 0.6, the Salvo's rocket box at 0.45 | stills: cyan for team 0 (`critic-salvo-game-after.jpg`) |
| The Salvo a grey lump at 45-120 m (`critic-salvo-game-before.jpg`) | the rack lay flat along its back; only its tubes could pitch, inside the box | re-split: the Turret is the yoke, the Gun is the whole box and its tubes, pitched at the yoke's top; a rack rides raised 16 degrees and lifts to 28 to fire (recoil 0.1 m, not a gun's 0.45) | the renderer's own state read in Play: pitch 16 driving, 28 with a target |
| The Skimmer inside the Maw's track (`critic-overlap-before.jpg`) | hulls are kept apart by round radii off their footprints, and the Maw is drawn 5.8 m out from its centre on a 3.8 m half width | `VehicleProfile.Clearance` (added to `Radius`): 1.2 m for the Skimmer and the Salvo | set down 3 m apart, they settle 9.49 m apart, hull beside track (`critic-overlap-after.jpg`) |
| The Salvo on the sea's edge, firing from there | not water: (51, 230) is dry ground, team B's deploy zone. A team-A machine drives to the enemy's line like every machine | `TankSpec.StandOff`: while gun 0 has a target the Salvo holds where it is. Its 380 m covers the whole of this 276 m map, so once the enemy has anything on the field it stays where it was put | `TheSalvoHoldsWhereItIsOnceItHasATarget` (fails with the flag off: 15.9 m moved) |
| The Skimmer's turret never tracked ("gun0 target -1") | TankGunnery's target is -1 by design for a machine with no TankGun; the turret follows `SimWorld.TargetSlot`. It had none: small arms look from a man's eye height even on a hull, and a trench berm hid the riflemen | nothing: that is the sim's line-of-sight rule for every machine's small arms | a foe in the open: target taken, turret at 51 degrees with the foe at 53; the fan turning; the Salvo's wheels 6.4 -> 11.6 rad over a few metres |
| `TankCapture.Status` called every machine but the Tusk "Maw" | a two-way ternary | `UnitLook.Name` | |

The re-split Salvo's LOD pop is unchanged (0.975 / 7.3, 7.1 at the battle's scale). **Left for Play:** the rack from
the player's own camera in a real fight; whether 0.45 of the side's colour on the box is too loud; the claw legs stay
folded into the hull (they are one piece with it: folding them needs a split into leg parts).

### The balance critic's round (2026-09-28)
A critic fought both machines offline (otr, 10 seeds a fight). Re-measured here with a scratch harness of the same
fights on the greybox map (open ground; the playtest map's berms hide a hull's small arms, which look from a man's eye
height), 60 s a fight, 10 seeds, one side the new machine (A). "Win" = every enemy dead or knocked out, A standing.

| Fight | Before | After |
|---|---|---|
| Salvo vs Tusk closing from 240 m | 0 win / 10 loss, 0 racks fired | 6 / 4, 1.9 racks, the Salvo keeps 76 % |
| Salvo vs 4 riflemen at 40 m (inside its rockets' 60 m) | 0 / 0 / 10 draw, 0 shots | 10 / 0, its machine gun |
| Salvo vs 15 riflemen 5 m apart at 200 m | 4.5 killed, 4 racks | the same (rack weight unchanged) |
| Skimmer vs 9 riflemen garrisoned in a trench | 0 dealt, 0 shots | 121 hp dealt, 0.9 killed, closes to 20 m |
| Skimmer vs Tusk from 240 m | 0 / 10 (the Tusk's 220 m gun outranges 130 m) | 0 / 10, unchanged |
| Skimmer vs a holding Salvo from 200 m | 0 / 0 / 10 draw | 0 / 10: the Salvo's rockets reach it first |
| Skimmer vs a holding Salvo from 120 m, side-on | (not run before) | 8 / 2: 12 mm through its side |
| Skimmer vs 9 riflemen, static, from 120 m | (not run before) | 8 / 0 / 2, 8.8 killed, holds at 90 m |
| Skimmer vs 9 riflemen advancing, from 120 m | (not run before) | 0 / 0 / 10, 217 hp dealt, closest 71 m (it holds; they did not reach it) |
| Kettle holding, the critic's reference | 0 shots | 0 shots: see below |

Changed (the critic's final report): the Skimmer's machine gun is 12 mm and hunts light machines (`InfantrySpec.HuntsArmour`:
a machine whose facing plate it beats, never a Maw's front), costs 180, holds at 90 m (`TankSpec.StandOffMetres`) and looks
down into a trench within 20 m (`InfantrySpec.LooksDownMetres`); the Salvo's rack may take a machine as its mark (its
bursts test the deck), it carries the Tusk's hull machine gun (110 m) for the 60 m its rockets cannot reach, and its hold
gives up after 32 s on a mark that loses no hit points (`StandOffPatience`). Not changed: the rack's weight (the critic
judged the fewer-kills calibration right), the Skimmer's hit points. **The Kettle's 0 shots** are the shipped Kettle's,
not the harness's: a machine turns its hull toward its path even standing still (yaw 3.14 -> -2.36 in the probe), and the
Kettle's mortar swings only 24 degrees either side of the nose, so a halted Kettle whose path does not point at its
targets never fires. A moving Kettle, pointed along its path, does (CrabTests). Left for the owner: it is the original
game's machine. **Open:** the Salvo beats a Tusk 6 in 10 (the critic expected the Tusk to win at a price): its bursts on
the deck strike the Tusk's modules; the Skimmer still cannot touch a Tusk (all of the Tusk's plate is thicker than 12 mm).

### Critic round 3: code and sim correctness (2026-09-28)
No leak into the original game, determinism rules held. Fixed, each with a test that fails on the old code (proved by
putting the old code back):

| Finding | Fix | Test |
|---|---|---|
| A unit spawned into a dead machine's slot inherited its stand-off hold | the hold is reset with the guns' state when the slot's generation changes | `ANewUnitInADeadSalvosSlotInheritsNoHold` (old: HoldTarget 1) |
| The hold (a hard halt every tick) would override a move order for up to 32 s | a machine holds only on the first goal it was given; re-goaled by anything it goes. The sim has no per-machine move order yet, so the test re-goals a held Salvo as an order would | `AHeldSalvoSentElsewhereGoes` (old: it stayed) |
| The patience counted hit points lost to anyone; the docs said "taken off it" | the docs and the spec now say what it does: a burst carries no shooter slot to credit | - |
| The canary ran with no latency; no replay with rockets in the air; nothing killed the Salvo mid-flight | the canary also runs at latency 2, jitter 1, loss 5 %; a recorded match with a Salvo deployed by command and its rockets in the air replays to the same hashes; a Salvo removed with its rack in the air still has every rocket burst, the same in two runs | `TheRackIsTheSameEveryRunAndInTheCanary`, `AMatchWithRocketsInTheAirReplays`, `RocketsInTheAirLandWhenTheSalvoDies` |
| Two header tests had been loosened to "v9 or later" | pinned to v14 again | `LoadoutTests`, `FactionRosterTests` |

Minor: `SimHashTests`' pinned chain renamed `SystemChain` (it has not changed since v9); the rack's tube sockets are
looked up once per model (`TankModel.Tubes`, no strings per rocket), the side's colour is set once per part
(`TankModel.Part.SideWear`, ordinal), the rack's recoil reads a per-model flag, the magic numbers are named
(`CombatTables.HullTargetBonus`, the rack's pitches and recoils, the fan's rates, a rocket's arc); rockets pending in a
world with no BlastSystem are dropped; the renderer drops the last match's rockets and views when a new match starts. The
Salvo's triangle counts after the re-split (7,863 / 2,539) are the ones in `pipelines.md`; its portraits are re-cut from
the current rack. Format v14.