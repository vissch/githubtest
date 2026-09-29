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

## The Bullfrog: a gatling toad (2026-09-28)
The owner's `frog+mecha+3d+model` (4,953 tris): a squat four-legged armoured toad with two gatling guns over its back.
`mecha+frog+3d+model` and its `(1)` copy (identical files) are the same design as a cruder 940-triangle sculpt; only its
count is used, for LOD2 (derived from LOD0, as decided 2026-09-27). `mechsplit.py TW_KIND=gatling TW_LOD2_TRIS=940`:

| Part | What | Moves |
|---|---|---|
| Hull | body, head and all four legs (one welded Tripo piece) | hops (`Runtime/HopDrive.cs`) |
| Turret | the saddle and brackets over the back | turns (`traverse`) |
| Gun_L / Gun_R | each housing, horn and fittings; the right one's ammo box and belt | recoil, droops when knocked out |
| Barrels_L / Barrels_R | six barrels and the clamp rings on each gun's axis | spin up, fire, wind down |

LODs 4,948 / 1,873 / 934 tris. Tripo's flat ground plate and a one-triangle sliver are dropped before the model is
centred (the plate set the model's bounds). Pieces go to a gun's barrels by their distance from that gun's axis (under
0.07 model units, ahead of the housing's back), so the spinning part holds only what lies on the axis; the barrels pivot
on it, and `A_Gatling_...` checks their mesh's middle does not move while they spin.

- **Firing** (`fire`, `VehicleRig.Gatling`): a 2.2 s burst; the barrels spin up in 0.5 s to 1,800 degrees a second
  (the pair against each other), rounds start at 70 % of that, 24 a second by turns from the two guns (a gun shot off,
  or its barrels, falls silent), and the barrels wind down over 2 s. Each round is `PlaygroundFx.GatlingShot`: a flash
  and a star that last 0.08 s (a stream, not a flicker; 0.05 s and a metre across hardly showed in a still), a small
  puff, a 0.05 s light. Each round warms its gun's barrels (`VehicleRig.Heat`, cools over 4 s) and they glow with it;
  firing, the body leans back 2 degrees and shudders (`HopDrive`).
- **Hopping** (`walk <speed> [1]`): crouch, flight (1.3 m at 2 m/s, forward only in the air), landing with dust from
  each foot. The legs are welded to the body, so on the ground its feet stay on it and the body squashes and stretches
  instead (`HopDrive.Shape`, the hull's scale with its volume kept, the saddle given the inverse so the guns never shear):
  it squashes as it gathers, stretches springing and reaching down, squashes to 0.84 on landing and bounces back; the
  saddle rides a spring (`SaddleHz` 2.2). The first cut sank the body 0.12 m and pitched it 5 degrees, and
  arriving nose down put the front feet 0.23 m into the ground (the test caught it). Knocked out it slumps onto its
  belly, over onto the side of a gun it has lost.
- **Aiming** (`target x y z` | `target near` | `target off`, `VehicleRig.AimAt`, any machine with a Turret and Gun): the
  turret turns to the point's bearing (150 degrees either side, 90 a second), each gun elevates to it from its own
  muzzle (-8 to +40 degrees, 50 a second; from the trunnion under the bore a gun 40 m off pointed 3.8 degrees high).
  Without a target the turret traverses at random as before.
- **Where the rounds go** (`VehicleRig.Shoot`, its own random stream so the destruction's stays the copies'): at the aim
  point scattered by 3.5 % of the range across the line of fire (twice that along it) and walked from 5 % of the range
  on one side to 5 % on the other over the burst (`Sweep`), else along the barrel to the ground, or 250 m up.
- **Tracers, strikes and cases** (`PlaygroundFx.Fly`, `Case`): each round flies at 700 m/s; every second one draws a
  streak (CombatFx's instanced box, at least 2 px thick) whose tail lingers 0.12 s shrinking into its strike; each lands
  as the battle's spurt with a column of earth and clods, a spark for a tracer, and one in three leaves a low dust haze
  that hangs 2.8 s, so a burst builds a cloud where it falls; a hot brass case and a dark belt link are thrown out of
  the gun's outer side every round.
- `model v <Name>` picks a machine by name: the Bullfrog shifted the library's alphabetical indices, and round2.sh's
  "Croaker 1" would have shot the Bullfrog as the Croaker. round.sh and round2.sh now use names; round2 shoots the
  Bullfrog firing (close and wide, slowed to a fifth) and hopping.
- **Critic rounds g1-g3** (`round2.sh gN`, stills and `lodpop` for every machine; the Bullfrog's sheet to a harsh critic):
  the wide view did not read as firing (rounds in one clump, grey strikes, dark specks for cases): spread 1.5 -> 3.5 %
  of the range across the line of fire and twice that along it, strikes tinted a dry mud lighter than the night ground
  with a small column of earth each and three clods, tracers every third round as a bright 1.5 m head on a dimmer 5 m
  tail (an even bar read as a laser close up; far off both lengthen to stay 30 px), cases glowing hot. The close flash
  was the cannon's Muzzle card seen side-on (a hook): a Flash and a Star turned at random, a small puff each round.
  The cook-off threw the small saddle with both guns riding it into the body: the twin guns now go first, 3-5 m out
  each, and smoulder; the saddle stays on the wreck (thrown alone, its large sparse box toppled corner over corner
  12.9 m). Knocked out it no longer sinks (feet 0.22 m through the floor) but tips 8-11 degrees onto the side of a gun
  it has lost, standing on the hull's lowest LOD0 vertices (the box's corners floated it 8 cm). The hop stills were one
  period (1.15 s) apart, so both showed one moment: `hopphase u` with time frozen shoots chosen moments.
- **LOD colour**: its switches shifted 3-7 in block colour against the Croaker's 2. Carrying LOD0's normals to the
  derived LODs (`TW_NORMALS=carry`) measured within the noise and cost IoU (the outline follows the normals): off.
  A larger share of each derived LOD for the hard-surface parts (`TW_PART_WEIGHT`, guns x1.8, barrels and saddle x1.4),
  A/B under one protocol, twice each: 0->1 mean 4.33 against 4.41 (worst side 5.5 against 6.45), 1->2 3.2 against 3.1,
  1->2 IoU 0.951 against 0.958: no gain, off. (A first "4.9 -> 4.3" compared round2.sh's lodpop, taken after a scripted
  destruction and other shots, with a lodpop on a fresh scene: they differ by about 0.5 on an unchanged model. Compare a
  change only against a baseline measured the same way.) The remaining shift is the eye and the receivers' texels.
- **Rounds g3-g5**: a lit card is multiplied by the night's blue moon, so a mud tint came out blue-grey (+25 luma, then
  +35 with glow): the strikes' tint is pre-divided by the moon and drawn with glow 2.6 at night, and they come out
  (133, 111, 109) against the ground's (33, 52, 91), red over blue, +59 luma. The cases got a small glint as they leave
  (a hot shard alone stayed a dark speck). The guns are thrown 2.5-3.5 m/s out and land 3.8-4.5 m from where they stood
  (4-6 m/s put them 7 m off, where they lay nearer the next wreck); the crouch is nose up, so it no longer reads as the
  landing. round2.sh waits 2.6 s at a fifth before the close firing still (the rounds start 0.35 s into a burst; at
  1.3 s it caught none). Verdict after g4/g5: fit for the Playground.
- **Rounds g6-g9** ("make it even better", the bar a polished unit in a shipped RTS; critic scores g6 -> g8: hop 4 -> 5,
  firing close 5 -> 6, wide 4 -> 5, silhouette 7, destruction 6, LOD 7; g9 fixed what g8 found but was not re-scored):
  on the ground the crouch and landing squat were thrown away (`lift = clear`), so three hop stills looked alike: the
  hull now squashes and stretches, the saddle rides a spring (driven only while hopping: the firing shudder rang it
  0.1 m, g8), the hop is 1.3 m, and in the air the legs move although they are welded on (the lowest 30 % of the rig's
  own hull mesh copies bend: hind feet trail back off the take-off, forefeet reach for the landing; the ground clamp
  bends its foot points the same way). Firing: 24 rounds a second, a tracer every second round at 350 m/s whose tail
  lingers shrinking into its strike (at 700 m/s neither wide still caught one), bursts that walk across the target,
  a dust haze that builds where they fall, hot barrels (drawn as soot and burn together: the shader lights embers only
  in soot), the body leaning and shuddering, belt links with the cases. Destruction: every Playground fire light had
  burnt at a fifth (`1 - Mathf.SmoothStep(0.8, 1, k)` is 0.2 at k = 0; Unity's SmoothStep interpolates, it is not
  GLSL's): fires now light the ground, in (1, 0.72, 0.4) since the old red-orange went magenta on the night ground;
  the one-piece hull chars instead of glowing lava orange; it belly-flops knocked out; rounds cook off out of the wreck.
  Three SHOW sites read the same SmoothStep way (board finding BUG-laptop-20260928-smoothstep-edges). round2.sh adds a
  four-frame exposure of the close burst, a still late in the burst for the heat, the landing at phase 0.72 (0.8 fell on
  the overshoot) and a mid-hop still at the battle's 78 m view.
- **The legs** (2026-09-28, "also do his legs"): Tripo welded all four legs into the hull, so no part moves them. They
  are skinned instead. `Tools/legrig.py` (Blender) lays an armature into the hull (per side: Thigh, Shin, Foot from the
  hip inside the thigh to the knee at the rear bulge, the ankle and the toes; Arm, Fore, Hand from the shoulder down the
  chest to the fingers; body bones on the spine, head, belly, back, flanks, cheeks and rump), weights each hull LOD with
  Blender's bone heat on the welded mesh (Unity splits vertices at UV seams; heat on the split mesh treats every UV
  island as its own piece), fades leg weights out towards the midline so the belly never follows a leg, and writes
  `Art/Tanks/Bullfrog/Bullfrog_legs.json`. The joints are laid out in the body's own frame: the sculpt stands turned
  15.3 degrees on its base (the four feet's clusters show it), which the guns and sockets are built around, so it is
  not re-turned. `Runtime/LegRig.cs` matches each Unity vertex to its welded weights (a vertex more than a millimetre
  from any: the mesh changed, the legs stay still with a warning, and the hopper test fails) and skins positions and
  normals of the LOD on show. Every low vertex of a limb past its wrist or ankle (on the digits' side of a plane through
  it, within 0.65) rides the hand or foot whole: shared with the forearm a finger stretched into a blade, and a capsule
  along the bone missed the splayed digits (43 of 109 finger vertices kept 30-80 % forearm weight, critic g11; now
  every mixed low vertex lies behind the wrist or ankle). The thigh and arm weights are averaged with their neighbours
  (the hip sheared at a narrow seam). `HopDrive.Drives` poses them: the hind legs push off, unfolding against the ground
  over the last 40 % of the crouch (`Push` 0.4) while leaning back, so it leaves along a diagonal (standing straight up
  first it looked on stilts); the flight is a parabola from the push's height to the ground (an arc plus a fading
  offset peaked at a quarter of the flight, then floated and dropped); in the air the thigh swings back first and then
  the knee opens to 100 degrees, so the leg trails nearly straight behind (unfolding while it swung, or trailing at 20
  degrees with the nose up, the legs propped the body up to 1.9 m over a 0.55 m arc); the forelegs sweep back under it
  and reach forward for the landing from a third of the way, and the hind knees fold as it lands; standing it shifts its
  weight on its haunches, firing the forelegs brace and the haunches pulse with the shudder; knocked out all four
  sprawl 20 degrees. Measured body lift through a hop, phase: metres, 0.15: 0.58, 0.18: 1.13 (take-off), 0.25: 1.42,
  0.42: 1.78 (the top), 0.5: 1.53, 0.62: 0.66, 0.72: 0.00. The body stands on the skinned feet
  and on everything that follows a leg. The forelegs do not fold on the ground (turned about the shoulder the hands left
  it and the body stood up on its elbows). `legpose e t r s` holds the drives for a still; `dumphull <dir>` writes the hull LODs for
  legrig.py. After a re-split: `dumphull`, then `blender -b --factory-startup -P Tools/legrig.py -- Bullfrog
  Art/Tanks/Bullfrog/Bullfrog_legs.json hull0.json hull1.json hull2.json`, then TW/Playground/Build.
- **Rounds g10-g12** (legs, critic scores g8 -> g11: hop 5 -> 7, legs 5 -> 6, firing close 6, wide 6, silhouette 7,
  destruction 6, LOD 7; g12 fixed the digits, the push and the arc g11 found, not re-scored): tracer tails were skipped
  and piled up when no head was in flight (an early return in `FlyRounds`); the "hot" still was taken after its burst
  ended (now 1.5 s into a second burst on the first one's heat); the wide shots get the same brightest-pixels exposure as
  the close one. Open: the barrels' heat is one value per part (`_Damage`), so it glows along their whole length, not
  from the muzzle; that needs the shader to read a position along the barrel.
- **Life** (2026-09-28, "you can do better than this"): a hit jolts it away from the blow (`HopDrive.Flinch`, from
  `VehicleRig.Hurt`: rocks up to 7 degrees, squashes 8 %, the legs take it; up in 0.05 s, gone over 0.2 s); it sits
  0.3 s between hops (`Rest`; hop after hop read as a machine bouncing), keeping its pace; sitting, its throat swells
  twice every 3.2 s (`LegRig.Throat`: the white under the chin pushed out along its normals, 0.11 hull units, weights
  found from the body frame at load); knocked out, its legs sprawl 45 degrees so it drops onto its belly and a hind leg
  kicks three times, weaker each time (1.1, 1.8, 2.6 s), and the cook-off throws the body 0.55 m up off them; the
  barrels' heat is a small warm light at each tip (`VehicleRig.HeatLamps`, as the square of the heat) with less soot
  and glow along the whole part, so it builds from the muzzle end. `Tools/playground/seq.sh NAME FRAMES TIMESCALE
  ["setup"]` records a run of stills at a slowed clock and a GIF, to judge motion rather than poses (stills can hide a
  pop or a jitter): the hop, a burst and a hit-to-death run were reviewed this way.
- **Weight** (round g14, from the first critic to see motion, g13: hop 7, firing 6/6, destruction 5): on the ground the
  body sinks over planted feet (`HopDrive.Fold`: two-bone IK in the body's side plane moves each ankle and wrist up by
  the sink, the foot or hand keeping its angle, and the clamp lowers the body by as much): -0.15 m in the crouch before
  the push, -0.12 m landing, a hit drops it (0.25 m times the jolt) and shoves it along the blow, firing it squats 5 cm
  and is pushed back 4 cm. The belly rests 1 cm above the feet, so it flattens and spreads under the sink
  (`LegRig.Squash`, the bottom 0.5 hull units by body weight); the palm's heel now rides the hand whole
  (`legrig.py PALM_BACK` 0.45: shared with the forearm it rose a third as far as the wrist). Knocked out it lies on its
  belly 0.1 m below its standing height (stood on its sprawled legs it was 0.44 m above, a table): the height comes
  from the body's underside alone and is kept once it is down (the kicks dropped it 0.21 m and back), the legs roll out
  before they stretch (together, a shin swung 0.46 m through the ground) and lie on a ground plane (`LegRig.HasGround`)
  with the roll outside the pitch, so a kick goes out along the ground (it went up over the back like a tail). Firing:
  lean 4 degrees and shake 1.5 (2 and 0.4 hardly showed), the saddle driven back 3-4.5 cm along its guns, the throat
  pumping with the burst, the heat light over the middle of the barrels and a wisp of smoke off them after a burst. A
  barrel set holds until the machine is Damaged (the first 15-damage hit took half its guns at 85 % health). Landing
  dust is a brown haze (`PlaygroundFx.Dust`), a hit's smoke a rising dust puff, the shell burst card lit and rising as
  the battle draws it. Round g15: the blue ball was the shell burst's own card (its last frames are smoke; drawing the burst without it showed it) and the three smoke cards, untinted under the moon: both now carry moon-divided tints (`TintDust`), the smoke thinner (0.65), after the flash and drifting apart; the throat swells 0.2 hull units for 0.45 s a pulse, weighted and pushed by position from the sac's centre (the normal-based push tore a dark slit at the lip seam once it was deeper).
- Open before the battle: the eye and the receivers brighten at the 0->1 switch (+29 and +18 luma on the eye's
  blocks). Re-baking LOD1/2 onto their own atlases from LOD0 (fbxlod2.py's bake: fresh UVs, emission, filled misses;
  1024/512 px) measured worse, twice each against the shared atlas: 0->1 block mean 4.70/4.67 against 4.40/4.42 (worst
  side 7.5 against 6.5), 1->2 3.88/4.12 against 3.12/3.10, IoU unchanged. The pop images show the switch is shading:
  the gun housings' faces go flatter and lighter at LOD1 with or without the bake, as `LodFit`'s note says of what no
  tint removes. Not kept. With the part weighting and the carried normals above also neutral, the rest is the housings'
  bevels collapsing into flat faces; nothing tried so far keeps them within the budget.
- Not done: the battle (a sim archetype and a `Resources/Vehicles` entry).

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
