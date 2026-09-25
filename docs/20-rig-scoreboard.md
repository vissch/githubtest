# The rig and animation scoreboard

What "triple-A" means for the six machines, as lines that can be scored rather than as an adjective. A harsh
critique agent scores every line 0-10 from rendered evidence each cycle, and the cycle after it either raises a
score or says why it could not.

**The rule for zooming in.** The camera starts far out and only comes closer when every line on the board is
**8 or better** at the current distance. Then the same board is scored again at the new distance, where the same
faults are bigger and new ones appear. A score is only ever valid for the zoom it was taken at, so moving in
resets nothing but re-earns everything.

**The rule for changes.** A fix is applied only if the gait tests stay green; if they go red it is reverted and
the attempt is logged as a failure with its reason. Nothing is committed — the owner commits.

**The rule for scoring.** The agent is instructed to be harsh and to withhold marks by default. 10 means "I
looked for the fault and could not find it"; anything above 7 needs evidence, not absence of evidence. A line
nobody has rendered evidence for scores 0, not "unknown" — an unmeasured thing is not a good thing.

---

## Zoom levels

| # | Name | Camera | What it is for |
|---|------|--------|----------------|
| 1 | Tactical | 14x the machine's height, 35 deg down | The distance the game is actually played at. Silhouette and gait rhythm only. |
| 2 | Engagement | 7x height, 22 deg | Where the player looks when something happens. Leg placement, body carriage. |
| 3 | Close | 3.5x height, 14 deg | Cinematic and screenshot distance. Joint bending, foot contact, weight. |
| 4 | Macro | 1.8x height, 9 deg | Where nothing may hide. Toe-ground intersection, mesh stretch, claw and muzzle detail. |

Distance is a multiple of each machine's own height, and the LENS is then chosen so the machine fills 72% of
the frame at every level. Fixed metres were wrong twice over: they were picked for a 2.7 m machine and put the
camera inside a 10.5 m one, and framing by walking closer would have made "zoom in" change the perspective,
which is not what a player's zoom does. Distance sets the perspective; the lens sets the framing.

Current level: **1 — Tactical**

---

## The board

Scores are `-` until the first cycle has rendered evidence for them.

### Walk — foot plant, slip, body carriage

| # | Line | Score | Evidence / why not higher |
|---|------|-------|---------------------------|
| W1 | A planted foot does not slide, skate or creep while the body moves over it | 6 | Cycle 46 MEASURED it, and cycle 43's "nothing slides" is WITHDRAWN - it does slide, badly. With the toe target nailed to one spot and the body swept through a 0.40 m stance, the vertex that actually touches the ground travels Pincer 0.77, Banner 0.86, Pavise 1.03, Censer 1.75 m. That is 1.9x to 4.4x the distance the body moves, in the wrong direction, on the leg the gait believes is planted. The reason it is geometry rather than a solver bug: a rigid leg holding ONE point still can only rotate, so every other point on it must move, and the contact vertex is far from the pinned toe. **Cycle 47, FIXED and gated:** measured in the shipped code while walking, the contact vertex now slides Censer 0.000, Pavise 0.000, Banner 0.033, Pincer 0.069, Kettle 0.116 m while its foot is called planted, against 0.037-0.183 before. Redoubt is the holdout at 0.901 m (was 1.056), because its front toe sockets sit 2.15 m below the foot mesh and the correction had to be declined there - see cycle 47 notes. Cycle 46's Blender figures of 0.77-1.75 m are WITHDRAWN: they swept a rigid leg through a fixed +/-0.40 m about the hip, which is not the stance the gait actually uses, and they overstated the fault by 4x to 15x. Scores 4, not higher, on Redoubt alone. |
| W2 | The body rides at a steady height on the level, with no bob, sink or hunting | 4 | Cycle 44: the ride height is now MEASURED rather than described - Pincer -0.62, Kettle -0.10, Censer -0.65, Pavise -0.59, Banner -0.09, Redoubt +2.24 m (the Body pivot's height above the ground it stands on). Belly clearance with it: Censer +0.04, Banner -0.32, Pavise -0.46, Pincer +0.33, Kettle +0.37, Redoubt +2.24. NO 2 m SOLDIER PASSES UNDER ANY OF THE SIX, which was cycle 1's stated target. **Cycle 47:** steadiness finally has a number, found by breaking it - `GaitTests` already watches ride height over a level walk and allows 0.34 m of sink, and the shipped machines pass inside it while LEAN 0.30 pushed Banner to 0.459 m and went red. So the worst sink on the level is under 0.34 m and is bounded by a test, which is worth 1. Belly clearance after cycle 47: Kettle +0.57, Redoubt +2.36, Censer +0.12, Pincer **-0.15**, Banner -0.32, Pavise -0.39. Pincer's went BACKWARDS (was +0.33) because correcting its toe lowered its ride height from -0.62 to -1.10; that is the price of the cycle 47 fix and it is logged as a trade rather than hidden. Still no 2 m soldier passes under any of the six. **Cycle A3 found the large fault on this line and FIXED it, gated green.** Crossing a parapet, a Pincer's hull lost about 1.4 m - 21% of its own height - squatted onto the bank, ploughed through it, and got it all back on the far side. Measured hull-top against its own selection ring (both in frame, so camera error cancels): 201 px before the bank, 108 px on it, 216 px after. Mounting a parapet the body should RISE. Cause and fix in the cycle A3 notes. Re-shot afterwards with the same camera: the hull now rides clear above the sandbag line for the whole crossing with the legs extended beneath it, and the squat frames are gone. **WITHDRAWN at cycle A6: that fix was intermittently red and has been reverted, so the squat is back. The measurement of the fault stands; the fix does not.** Scores 3 and not higher because steadiness on the LEVEL - bob and hunting over time - is still unmeasured, and belly clearance on the flat is unchanged. |
| W3 | Pitch and roll read as a body carried on legs, not a box on a spring | 5 | Cycle 48 measured the two things that stop it reading as carried, and the line's own wording turns out to be literal. **It IS a box on a spring:** the gait fits a plane to the feet, smooths it with `k2 = 1 - exp(-dt*9)`, and the renderer then puts THAT through the same critically damped spring a tank's hull uses (`omega 7`, TankRenderer:375). Two filters in series reach 90% of a new tilt after 0.700 s - 0.84 m of travel at 1.2 m/s, 2.10 m at 3.0 - and the second filter contributes 0.433 s of that, 162% more lag than the gait's own. **And the pitch axis is degenerate:** the feet span, across / along, Censer 5.31 / **0.01 m**, Pavise 8.53 / 0.17, Pincer 7.40 / 0.36, Banner 4.57 / 0.59, Kettle 6.13 / 1.38, Redoubt 7.39 / 3.32. Four machines have their feet in a line ACROSS the body, so a 0.5 m step under one foot demands 54-99 degrees of pitch and gets the 24.1 degree clamp instead. Roll has a real lever on all six (3.4-6.2 deg for the same step); pitch has one only on Redoubt and Kettle. Measured at the rest footprint, which the solved stance preserves because the feet splay radially from the hips. **Cycle A5 removed the second filter and measured the result A/B.** At world-position-matched frames the hull now carries **~9 degrees more of the terrain's attitude** (BEFORE +13.5 deg, AFTER +22 deg at frame 02; +13.5 / +22.5 at frame 03, against a +/-3 deg error bar). The shape of the win is not what was predicted: both hulls reach the crest at the same attitude (+24 / +25), and the difference is that **BEFORE relaxes back toward level over the next eight frames while AFTER holds**. The second filter was not delaying the onset so much as continuously dragging the hull back to its long-run mean. No overshoot and no oscillation in either. Scores 5, not 7: both hulls are still near-static - BEFORE varies 4.5 deg over eight frames, AFTER 4 deg over six - so the body still never answers an individual footfall. The lag is gone; the life is not there yet. |
| W4 | The gait has a rhythm - legs do not all lift at once, nor shuffle continuously | 5 | Cycle 49, computed from the step decision, which is pure scalar arithmetic and needs no editor. **The first half is guaranteed by construction:** `allowance = max(1, min(legs>=6 ? 3 : 2, alive/2))` hard-caps airborne legs at half the machine, so "all at once" cannot happen however the triggers fire. The adjacency rule (`Beside`) additionally blocks two legs next to each other on one side, which for 4 legs leaves only a diagonal pair - a trot - and for 6 leaves {0,2,4} - the alternating tripod the comment claims emerges. **The second half is not a shuffle either:** a leg trips at 0.32 x span, giving strides of Redoubt 2.09, Kettle 1.42, Pavise 1.29, Pincer 1.08, Censer 1.06, Banner 0.78 m, and each leg is planted 58-81% of the time at 1.2-3.0 m/s. Above 50% is a walk with a support phase. Average airborne load is 0.77-2.07 legs against caps of 2 and 3, so legs never queue for permission. **Scores 4, not 8:** none of this shows the pattern is PERIODIC rather than merely bounded, no footfall trace has been recorded, and the adjacency rule is explicitly bypassed by `runningOut` and `Frantic` - so the tripod is a preference the code hopes for, not an invariant it enforces. Banner is the machine to watch: shortest stride, busiest legs, and only 58% planted at speed. **Cycle 50 TESTED all of this and half of it was wrong.** Confirmed: the pattern IS periodic - Pincer walks {0,1,4}/{2,3,5} at 49/51% with **0% jitter**, and Censer, Pavise and Banner trot on diagonal pairs at 0% jitter. Refuted: the duty figures. Every leg is planted **52%** of the time, not 58-81%, because cycle 49 derived the period from a drift rule that never fires. Redoubt is the outlier at **44% planted with 91% jitter** and a lopsided 86/14 split between its two pairs. Scores 3: the rhythm is real and machine-verified, but the machine is stepping as fast as its allowance permits at all times rather than when a leg needs to, which is what this line calls shuffling. |
| W5 | The machine leans into a turn and its outside legs take longer steps | 6 | Cycle 51. **The lean half is not implemented at all.** `yawRate` occurs exactly twice in the whole of `WalkerGait`: once in `Step`'s signature and once as `sweep = cross(up, hipOut) * yawRate` on the step target. `Roll` is assigned only from the foot plane fit (`fitRoll`) and the lost-leg hole (`hole.x * 0.16f`). Nothing anywhere converts turning into roll, so a machine going round a corner stays as upright as one walking straight, and on flat ground the plane fit gives it nothing incidental either. This is settled by enumerating every use of the variable rather than by sampling behaviour, so it is not a matter of tuning being too subtle to see. **The longer-steps half works and has the right sense on all six:** turning left at 0.5 rad/s, the outside legs step further than the inside ones by Censer +70%, Pavise +66%, Pincer +65%, Kettle +48%, Redoubt +40%, Banner +31%. CAVEAT: the decision port does not implement Unity's two-pass clamp of the target into the leg's reachable annulus, so the absolute step lengths (1.3-3.35 m against 1.01 m straight) are certainly overstated, and since the outside leg is the one that would clamp first, the true ratio is probably smaller than these figures. Scores 3: one half of the line is absent, the other is present with the right sign and an unverified magnitude. **Cycle A8 added the lean and could not observe it doing anything.** The term `wantRoll += yawRate * pace * TurnLean` is in and green on four runs, but in five samples across two commanded turns, `yawRate * pace` was **zero every time** - the renderer's `YawRate` read 0.0000 and the sim's speed read 0.00 while the machine's yaw was visibly changing from 90 to 0 to 45 to 135 degrees. Either the turns complete between samples, or the machines stop dead to rotate and never turn while moving, or the machine was stuck again (the fault from A1/A4). I could not distinguish the three. Score unchanged at 3 because an unobserved effect is not an improvement. **Cycle A9 observed it.** Driving `WalkerGait` tick by tick with yawRate 0.5 AND speed 1.2 - the condition play mode would never hold still for - the roll rises 1.85 -> 2.04 -> **2.06 degrees** and holds there for the rest of the turn, against 0.00 for the whole of an otherwise identical straight walk. 2.06 degrees is the predicted value to two decimal places. **A8's 'never seen to fire' was a sampling failure in play mode, not a fault in the fix.** Scores 6: the lean is confirmed working in the shipped solver and the longer-outside-step half was confirmed with the right sign in cycle 51, but the magnitude of the step difference is still unverified in game and nothing has been rendered of a machine actually banking. |
| W6 | No leg is drawn visibly stretched or short to reach its foot | 3 | Cycle 45, MEASURED off the rig and the shipped constants: standing still on the level, every leg on every machine is asked for 89.3% of its own length. Pincer 3.37 m legs drawn 0.36 m short, Censer 3.30 m / 0.35 m, Kettle 4.44 m / 0.47 m, Pavise 4.04 m / 0.43 m, Banner 2.43 m / 0.26 m, Redoubt 6.53 m / 0.70 m. It is not a per-machine defect: the number is 89.3% to one decimal on all six, so it is the constants, not the sculpts. Jointed legs absorb it by bending, which is correct and invisible; a ONE-PIECE leg can only be scaled, which is why this scores at all - Pincer is one-piece on all 6 legs and Censer on all 4. Scores 2, not 0: the cause is now named and arithmetic rather than guessed, but nothing is fixed. **Cycle A6 measured the DRAWN length live in the running game**, per leg, as a percentage of that leg's own maximum span. Pincer 87-89%, spread 2 points - uniform, and a uniform crouch is not what this line is about. Censer **80-109%, spread 29 points**: one leg drawn 9% PAST full stretch while another is 20% short, on the same machine in the same frame. Redoubt 63-89%, spread 26. The mechanism is `stretch = clamp(dist / Bone[0], StretchMin 0.80, StretchMax 1.18)` applied to each one-piece leg independently - a +/-20% rubber allowance per leg, with no coupling between legs of the same machine. Scores 3 rather than 2 because the fault is now measured in the shipped renderer rather than derived, and the worst machine is identified. **Cycle A10 re-measured it properly and HALF OF A6 IS WITHDRAWN.** Driving the solver tick by tick over a six-second walk on flat ground and counting only PLANTED legs (a swinging leg is tucked and legitimately short, which A6's snapshots did not separate): Censer 88-93%, Pincer 88-97%, Pavise 80-93%, Kettle 77-94%, Redoubt 75-94%, **Banner 68-97%**. The demand - what the gait ASKS of a leg, before any clamp - **never exceeds 100% on any machine** except Banner, on 0.6% of leg-ticks. Toe error is 0.000 m throughout. So on level ground legs are drawn SHORT, never long, and the stretch clamp is essentially never binding; A6's Censer at 109% must come from terrain, not from the stance. The remaining fault is the SPREAD - Banner varies 29 points between its own legs while standing. **Cycle A15 took this line DOWN to 3.** A10 scored it on level ground, where the answer is genuinely good; it never counted the trench case A14 then measured, and that case is in this line's scope. A planted Pincer foot is separated from its own drawn toe by **1.165 m on a 3.37 m leg - 35% of the limb - on 29.3% of in-trench plant-ticks**, with the same machine hitting 118% of span. That is not a stretched leg, it is a foot flying detached from one, and no magnitude of that size can be invisible. It holds at 3 rather than lower because flat ground really is exact (toe error 0.000 m, demand never over 100%) and Redoubt is 0.000 m throughout the trench runs, so most of the play space is clean - and because there are still NO PICTURES, so "visibly" is unverified in both directions. **Cycle A16 explains the SPREAD that A10 left hanging**, off the shipped FBXs rather than by argument: the six machines do not have the same leg topology, and three of them do not have the same topology from end to end. Banner's front legs are two pieces and its rear legs one; Pavise is the mirror of that; Redoubt has three-piece front legs and two-piece rears. A leg's minimum fold depends on its chain (a one-piece leg is floored at a hard 80% of span, a jointed one at `|upper - lower|`, which can be 50-70%), so **legs on the same machine are working to different bounds** - which is what a 29-point spread between Banner's own legs looks like. It stays at 3: this is an explanation, not a fix. **Cycle A19 re-read A10's table and found it was measuring two different things - but the re-reading's own conclusion did not survive either.** For a ONE-PIECE leg the "drawn % of span" figure IS a literal anisotropic mesh scale (`Solve:771-782`, `Scale(1, 1, stretch)` along the hip-toe axis), because `Bone[0] == Reach == MaxSpan` for a single-part chain. For a JOINTED leg the same figure is just a folded knee and deforms nothing. **So A10's "up to 32% short on Banner" mixed folds with deformations and is misleading.** Two solid results: a one-piece leg's drawn length is provably bounded to [0.80, 1.18] of span, and **at home stance it is provably 0.878-0.893 for ANY hip height** - so Pincer's and Censer's 11-12% squash is the DESIGNED stance, not a transient. Three corrections against me: the >=80% inference is one-way only, so Pavise's 80% floor is NOT evidence of its one-piece front pair (a jointed `MinSpan` can sit far below 0.80 x span); 80% is unreachable at home, so that floor is a dynamic pin of **Carry's `floor`** rather than of `Solve`'s clamp; and restricting W6 to one-piece machines on flat ground **discards the worst W6 violation there is** - Pincer's toe drawn 1.165 m short of its own planted foot. Stays at 3, and cannot move without a frame. |

### Terrain — parapets, trenches, slopes

| # | Line | Score | Evidence / why not higher |
|---|------|-------|---------------------------|
| T1 | A step lands on top of a parapet rather than through it | 3 | Cycle A3, and the fault is STRUCTURAL, confirmed in code rather than inferred from pixels. `BattlefieldComposer:324` places `kit.sandbag` as a prop at `ground - 0.04f`, and the ground function the gait plants against is `TankRenderer.Ground` -> `RenderGround.Sample`, which is the heightfield plus crater deformation and nothing else. **The parapet contributes zero height to the function that places feet.** A foot targeted at a cell under the bag row is therefore planted at bare terrain, about one bag-stack (0.4-0.5 m) inside the geometry, every time. Rendered evidence agrees: in twelve frames of a Pincer crossing a revetted parapet there is not one frame where a foot rests unambiguously on a bag crest, and in frame 06 the far-side front claw ends 25 px (~0.34 m) below the crest with the bag silhouette still behind it. This cannot be fixed by a constant - the bags, revetment and duckboards need to contribute height to the sampler, or a second obstacle-height channel is needed that `ArcFor`, the step-target sampler and the new belly floor all consult. **Cycle A12 closed the hole in that reasoning.** It assumed the solver WOULD do the right thing if the ground told it the truth, and nobody had checked. Given a real berm in the ground function - 2 m x 0.5 m, 2 m x 1.0 m and 3 m x 1.8 m - **not one foot is ever planted inside it: 0.0% of plant-ticks on both machines at all three sizes, worst penetration 0.00 m.** Feet land ON the berm on 13-29% of plant-ticks, rising with its size, which is what stepping onto a parapet looks like. **So the solver is ready and the fault is entirely in the terrain data** - the fix is now known to be worth doing rather than merely necessary. Scores 3 rather than 2 because the remedy is validated; it cannot rise further until the shipped sampler actually carries the parapet, because this line scores the shipped machine. |
| T2 | The machine straddles a trench instead of walking into it | 6 | Cycle A3: it straddles, in all twelve frames, and no leg descends into the channel. But the line is being asked of the wrong trench and so the pass is not worth much - `GreyboxMapGenerator` cuts trenches 3 m wide and carves the heightfield -1.8 m, while `VehicleKinematics` gives the Pincer `TrenchCrossWidth = 3.6 m` and a body 8.5 m long. A 6.5 m machine stepping over a 3 m ditch is a crab stepping over a gutter. The 1.8 m carve IS in `ground()`, so the solver can see this trench; it simply has no reason to put a foot in it. Scores 3 until it is tested against a gap WIDER than `TrenchCrossWidth` - a dugout mouth, a sap, or a crater-widened section - where the solver is actually forced to choose. **Cycle A11 built that gap instead of looking for one.** The gait consults a ground function, not navigation, so a slot of any width can be put in front of it. Walking a Pincer and a Redoubt across flat-bottomed slots 1.8 m deep, and counting the fraction of planted leg-ticks whose anchor lies INSIDE the slot: **Pincer** 2 m -> 0.0%, 3 m -> 2.3%, 4 m -> 9.3%, 6 m -> 12.3%, 8 m -> 21.6%, 12 m -> 40.1%. **Redoubt** 2 m -> 0.0%, 3 m -> 0.0%, 4 m -> 2.3%, 6 m -> 2.3%, 8 m -> 18.5%, 12 m -> 46.3%. The behaviour is **graded and sensible**: narrow gaps are straddled outright, and as the gap grows past what the machine can span it starts putting feet on the floor, which is the only thing it can do - a 12 m trench cannot be straddled by an 8.5 m body. Nothing falls in, nothing refuses, nothing snaps. Scores 6 and not higher because this is numbers and not pictures: whether the half-in, half-out posture at 8-12 m READS well has not been looked at. |
| T3 | Legs reach down a revetment and find the duckboards | 2 | Cycle A4 established WHY this line has no evidence after 55 cycles, and it is not the gait's fault: **navigation never sends a walker anywhere that would test it.** Broken and flooded ground is marked `navCost 255` - impassable - so the flow field routes around every crater and revetment rather than through one. Measured at a real 3.07 m crater: `navCost 255` and layer 41 across its whole footprint (ground -1.05 to -1.55 m), against cost 4 and cost 1 on the ground either side. The gait is never ASKED to put a leg down a revetment, so no capture can show it doing so. This line cannot be earned by any change to `WalkerGait`; it needs a navigation decision first - see the cycle A4 notes. **Cycle A14 got a first measurement anyway**, by writing the trench into the ground function as A11 and A12 did rather than waiting for navigation. Walking a Pincer and a Redoubt across flat-bottomed slots at four sizes: **every anchor rests exactly on the trench floor, 0.0% off on all eight cases, worst 0.00 m** - feet neither hover above the floor nor sink through it, so the targeting half of the line works. But **Pincer draws a leg detached from its own planted foot on 29.3% of in-trench plant-ticks in the shallowest slot, worst 1.165 m** - about a third of a leg - while Redoubt is clean at 0.000 m throughout. Scores 2, not more: W6 fails outright inside T3's own scenario; there are no pictures, so nothing here says how it reads; and the geometry tested is 2.7-4x the game's real 3 m trench with no revetment face, no fire
step and no duckboards - the two things the line actually names are still not in the instrument. **Cycle A18 CAPS this line at 2 and recommends it be rewritten.** Both nouns in the line are props with no height in the function the gait plants against, so no change to `WalkerGait` can ever earn it. In-trench duckboards are `kit.TrenchFloors[variant]`, placed at `BattlefieldComposer:105` at `Height.Sample + 0.035`, with the panel's boards a further 0.08-0.13 m up in local space (`BattlefieldKit:481-485`) - so a foot on the only floor it can find is **0.14-0.17 m through the boards, every plant.** The revetment is worse: it is a prop whose height is `Clamp((upper - floor) / 2, 0.55, 1.3)` (`BattlefieldComposer:121`), so the drawn timber deliberately spans only HALF the carved drop, and it is placed off `edge.DressCenter`, which is swayed up to 0.52 m in z and 0.20 m in x from the real cell edge (`BattlefieldSurface:104`). **A leg reaching down the real terrain wall cannot track the drawn revetment in height OR in position.** Recommended wording if the line is kept: "Legs reach down into a trench and find its floor", which IS answerable. |
| T4 | On a slope the body stays level to the ground, not to the world | 7 | Cycle A3: the body is NOT world-locked, which is the important half. The hull carries a measured 5.5 degree roll on flat ground from where its trailing feet happen to lie, so the least-squares plane fit through the planted anchors is doing real work and the clamps are not eating it. Scores only 4 because the sequence contains exactly one slope, the parapet bank, and the body's response to that was dominated by the squat described under W2 - so whether the plane fit tracks a sustained gradient is still unmeasured. Needs a machine crossing a spoil heap or a large crater rim with a 20-30 degree face held for 1.5 s or more, camera truly side-on. **Cycle A9 measured it directly instead.** Walked on a constant 0.18 gradient, the body settles at a pitch of **10.20 degrees**, and `atan(0.18) = 10.20 degrees`. The body matches the ground exactly, not approximately. On a step-up ridge the pitch climbs 7.14 -> 8.81 -> 10.48 -> 13.39 degrees as the machine mounts it, so it tracks a changing gradient too. Scores 7 rather than 9 because both surfaces were analytic ground functions in a harness, not real battlefield terrain, and nothing was rendered. |
| T5 | Feet find shell holes and broken ground rather than hovering over them | 4 | Cycle 43, in Unity on FLAT ground - the easiest case there is. Contact is wrong in BOTH directions at once: Kettle drives leg geometry 1.05 m under the surface while Pincer hovers 0.64 m above it. Censer +0.16, Pavise +0.14, Banner +0.02 and Redoubt -0.09 are close enough to read as contact. Scores 1, not 0, because it is now measured in the shipped draw path rather than in a harness. **Cycle 47, FIXED and gated:** contact on the level is now Censer, Pavise, Banner and Redoubt exactly 0.00, Kettle -0.04, Pincer +0.12 - against +0.64 of float and -1.05 of burial before. Still 5 and not 8 because every one of these numbers is flat ground, which is the easiest case on the board: nothing has yet put a foot on a parapet, a slope or the lip of a shell hole. |

### Damage — limp, lost legs, death

| # | Line | Score | Evidence / why not higher |
|---|------|-------|---------------------------|
| D1 | A lost leg stops taking part and the stump does not flail | 2 | Cycle A17, from source rather than pixels - the first Damage line to be scored at all. The mechanism itself is right: `WalkerGait:244` reads `off = (lost & (1 << i)) != 0` and a lost leg is given `Swing = -1` and skipped, so it stops taking part and there is nothing to flail. **But on Redoubt the bit it reads is the wrong leg.** The damage system numbers legs from the PROFILE (`VehicleModules:262`, `perSide = prof.Legs / 2`, bit `= onRight ? perSide + k : k`) and the renderer numbers them from the MODEL (`TankModel.NumberLegs`, `perSide = max(left, right)` over the parts that exist). For five machines the two agree. **Redoubt's profile says `Legs = 6` while its FBX yields four thighs**, so the sim uses `perSide 3` (left 0,1,2 / right 3,4,5) against a model with `perSide 2` (left 0,1 / right 2,3): the sim's third LEFT leg lames the model's first RIGHT leg, and bits 4 and 5 are never read at all, so **two of Redoubt's six legs can be shot off with no visible effect whatsoever.** Destroy its entire right side and exactly one leg visibly fails. Scores 3: the behaviour is correct on five of six machines and provably miswired on the sixth, and there are no pictures, so "does not flail" is unverified even where the mapping is right. Needs an OWNER DECISION, not a fix - see cycle A17. **Cycle A19 found a second, independent fault and took this to 2.** A one-piece leg is drawn with an anisotropic mesh scale along its own length (`Solve:771-782`), and at home stance that scale is provably 0.878-0.893 - so Pincer's and Censer's legs are ALWAYS drawn 11-12% short. But `TankRenderer:770-785` rebuilds a detached part as `Matrix4x4.TRS(pos, rot, Vector3.one)`, discarding the scale: **a squashed leg POPS to full length the frame it is blown off**, on every one-piece machine - Pincer's six legs, Censer's four, Pavise's front pair and Banner's rear pair. The line says the stump does not flail; a 12% length snap at the moment of detachment is the most visible instant in the whole event. (`d.World.rotation` is also being read off a non-uniformly-scaled matrix, which is ill-defined.) |
| D2 | The machine leans into the hole a lost leg leaves and keeps walking | - | |
| D3 | Losing more legs degrades the walk progressively, not in one step | - | |
| D4 | A killed machine goes down on its legs and its feet splay outward | - | |
| D5 | The collapse takes weight — it does not drop at a constant rate | - | |
| D6 | A dead machine's final pose is stable and does not sink or twitch | - | |

### Guns — aim, traverse, firing

| # | Line | Score | Evidence / why not higher |
|---|------|-------|---------------------------|
| G1 | **The drawn muzzle points at the target when the shot leaves** | 8 | One failure mode now EXCLUDED by measurement: every gun part on all six machines plus both tanks has a modelled yaw of 0.00 deg, so the drawn bearing equals the sim's GunYaw exactly and there is no static offset. **TESTED IN UNITY (post-loop):** driven in the real sim for 1500 ticks per machine on Pincer, Kettle, Maw and Tusk. Aim error at the instant a shot leaves: worst **0.313 deg** (Pincer), 0.000-0.001 on the others, against a tolerance of 2.86 deg. **Zero shots outside tolerance on any machine.** No gun ever held a target outside its own arc (0 gun-ticks). Scores 8, not 10: the link from the sim's bearing to the DRAWN muzzle rests on cycle 7's measurement that all modelled gun yaws are 0.00 deg, plus the fact that the renderer interpolates prev->cur between ticks, which bounds the drawn lag at one tick of traverse (3.0 deg for a Pincer at full rate) - and at the moment of firing the gun has converged, so prev is approximately cur and the lag is negligible. That is an argument, not a rendered measurement. |
| G2 | Traverse reads as weight - the gun does not snap to bearing | 7 | **TESTED IN UNITY (post-loop).** The gun is exactly rate-limited: the largest single-tick swing equals `TraverseRate / TickRate` to three decimals on every machine - Pincer 3.000 of 3.000 deg, Tusk 2.500 of 2.500, Kettle 1.200 of 1.200 - and **zero ticks exceeded it**. A first pass reported 149 'snaps' on the Pincer; that was my own arithmetic assuming 30 Hz when `cfg.TickRate` is 20, and the ratio was a suspiciously exact 1.5x, which is what gave it away. Rates are Pincer 60 deg/s, Tusk 50, Maw 35, Kettle 24, so a Pincer takes 1.4 s to swing 85 deg. Scores 7: rate-limiting is verified, but there is no acceleration or easing at either end of a traverse, so it starts and stops instantly at full rate, and nothing visual was rendered. |
| G3 | Muzzle flash, smoke and recoil land on the barrel, not near it | 5 | By construction rather than by measurement: `MuzzleWorld` returns `world[gunPart].MultiplyPoint3x4(MuzzleLocal[k])`, so the flash and smoke hang off the gun part's own matrix and cannot drift from the barrel, and recoil is applied along the barrel axis (`PartLocal`, Gun and Sponson cases). The known fault is in the asset, not the code: cycle 7 measured the **Maw's two sponson muzzle sockets 21 deg asymmetric** (-134.72 and +155.85). Scores 5 and no higher because nothing has been rendered and looked at - this is a reading of where the anchors come from. |
| G4 | A gun tracks a moving target smoothly rather than in steps | 7 | **TESTED IN UNITY (post-loop).** Smooth because the traverse is rate-limited (see G2), and it does not thrash between targets: over 1500 ticks per machine, **0 targets were dropped while still alive** on Pincer, Kettle and Tusk - the only target changes followed a kill (1-2 each). Firing rate matches the cyclic rate: Pincer 13 shots where its reload allows 6.5 per gun over the 19 s it spent laid on a target, Kettle 10 against 9.8, Tusk 10 against 9.4. Scores 7: the targets in the test stood still, so tracking a MOVING target is still unmeasured, which is what this line actually asks. |
| G5 | A broken gun droops and stops tracking | 7 | **TESTED IN UNITY (post-loop), both halves.** Stops tracking: with `GunHealth` forced to 0, the gun held a target on **0 of 200 ticks** - `TankGunnery` clears `GunTarget` before anything else can use it. Droops: `TankRenderer:400` sets `want = -7 deg` when `GunHealth <= 0` and eases toward it at 12 deg/s, clamped to [-8, +22], so it sags over about 0.6 s rather than snapping down. Scores 7 rather than 9 because the droop is read from the code and its constant, not seen. Cycle 1's score of 0 here was about loose parts hanging off the machines and was never evidence about a BROKEN GUN at all - it is withdrawn. |

---

## Cycle log

Newest first. One line per cycle: what was rendered, what the score moved to, what changed, what it cost.

| Cycle | Time | Level | Board | Change | Tests |
|-------|------|-------|-------|--------|-------|
| A19 | 2026-09-25 12:16 | 1 | W6 stays 3, **D1 3 -> 2** | Fifth cycle with the editor held; source and arithmetic only. **A10's central W6 table was measuring two incomparable things.** For a ONE-PIECE leg the 'drawn % of span' figure IS a literal anisotropic mesh squash (`Solve:771-782`, `Scale(1,1,stretch)` along hip-toe; `Bone[0] == Reach == MaxSpan` verified as the same float); for a jointed leg it is just a folded knee and deforms nothing. So 'up to 32% short on Banner' mixed folds with deformations. Derived without an editor: **a one-piece leg at home is drawn 0.878-0.893 of span for ANY hip height**, so Pincer's and Censer's 11-12% squash is the DESIGNED stance, and Pincer's 88-97% band is a 10-point length pulse on all six legs every gait cycle. **Three corrections against my own reading:** the >=80% inference runs one way only, so Pavise's 80% is NOT evidence of its one-piece pair; 80% is unreachable at home, so that floor pins **Carry's `floor`** (which skips SWINGING legs) not Solve's clamp; and my framing discarded the worst W6 violation, the 1.165 m detach. **Four uncharged faults:** (1) `TankRenderer:770-785` rebuilds a detached part with `Vector3.one`, so **a squashed leg POPS to full length the frame it is blown off** - D1 to 2; (2) `grow > 1` applies a UNIFORM scale, so a jointed leg reaching long is drawn up to 18% FATTER down the chain; (3) **a planted foot is never clamped to [MinSpan, MaxSpan]** - the structural root of the detach; (4) `:552` deliberately violates `MinSpan` in swing over a raised lip. **Warning: Redoubt is not a clean control** - its front toe sockets sit 2.15 m below the foot mesh by design, so its perfect analytic scores say nothing about the visible machine. | none run - editor held; no code touched |
| A18 | 2026-09-25 11:52 | 1 | T3 CAPPED at 2 (rewrite recommended), T5 5 -> 4 | Still no editor; source only, every claim re-verified against the files. Took a cap argument to the critique and **it broke three of four links, two of them overturning findings this board has rested on for fifteen cycles.** (1) **`RenderGround.Sample` REPLACES the heightfield, it does not add to it** (`Grid.Sample:13-22` returns `fallback` only when the grid is absent) - the gait plants against the DRAWN mesh off `BattlefieldSurface`, which includes `BankRise` up to ~1.05 m. **So the parapet IS in the gait's function** and A3's basis for capping T1 is false; T1's 3 now rests on a wrong premise and is unmeasured. (2) **Craters are NOT impassable:** `NavLayer:24` costs a crater **2**, and `Blocked = 255` covers only bunker wall / DEEP WATER / map edge - A4 measured one FLOODED crater and generalised. The sim even budgets for it: `VehicleKinematics:118,402` applies `StepOverSpeed 0.72` in a trench and `PickSpeed 0.88` in a crater. **Walkers are authored to walk into both.** (3) My proposed relocation of A14's 29.3% to T5 was REJECTED: the trench carve is 2 cells (~4 m) with **half the floor raised to -1.1 m**, and with the bank in the function the step reaches ~2.85 m - A14's '1.8 m' matches no part of the real profile, and a null extrapolated from three samples was indefensible. (4) Genuine gait bug found: **`ArcFor:444-457` samples the ground at only THREE interior points**, so a ~1 m parapet crest or crater rim falls between samples and the swing is never raised - almost certainly A12's unexplained 0.76 m berm clip. (5) T5 down to 4: `GreyboxTerrainView:201` stores `Max(bed, top - 0.42)` into the one grid the gait reads, so in a flooded shell hole **feet hover up to 0.42 m above the bed** - T5's own wording - from a constant commented as a man's knee. Next fix ready: split that wading fudge. | none run - editor held; no code touched |
| A17 | 2026-09-25 11:28 | 1 | **D1 scored 3 - the first Damage line scored at all** | Still no editor; source and shipped FBXs only. **Redoubt's legs are numbered twice, differently, and the damage system uses the other numbering.** Its profile says `Legs = 6` (`VehicleKinematics`) and the roster prose agrees twice over, but its FBX has four thighs. Damage numbers legs from the profile (`VehicleModules:262`, perSide 3, right = bits 3,4,5); the renderer numbers them from the parts that exist (`TankModel.NumberLegs`, perSide 2, right = 2,3); `WalkerGait:244` reads the model index. So on Redoubt **the sim's third LEFT leg lames a RIGHT leg, and bits 4-5 are never read - two of its six legs can be shot off with no visible effect, and destroying its whole right side visibly fails exactly one leg.** Fails silently (no out-of-range), which is why 51 cycles of pictures missed it. Verified that the other five machines agree on both numberings. **NOT fixed - owner decision:** setting `Legs = 4` is a balance change (`want` drives how many legs a hit takes), and the alternative is that the MODEL lost two legs to `build_loose`'s quadrant grouping welding surplus pieces into `Body` - which would also explain Redoubt's `Socket_Toe_` sitting 2.15 m below its mesh, and would mean every Redoubt figure on this board is measuring a self-consistent solver on an incoherent skeleton. | none run - editor held; no code touched |
| A16 | 2026-09-25 11:10 | 1 | no score moved; W6's unexplained spread now EXPLAINED | Still no editor, so read the shipped FBXs instead - `grep -a` gets the part names out without Unity. **The six machines do not have the same leg topology, and three are not the same end to end:** Pincer 6x one-piece; **Censer 4x one-piece (new - only Pincer was known to be)**; Kettle 4x three-piece; **Pavise front 1/rear 2; Banner front 2/rear 1; Redoubt front 3/rear 2**. Consequences: (1) Censer is as exposed to the fold violation as Pincer and has never been tested, and this retires A6's unexplained 'Censer at 109%'. (2) `Carry`'s `floor` is a `Max` across legs, so on the three mixed machines the fold floor is set by ONE END - **which is the cause of Banner's 29-point spread that A10 recorded without explanation.** (3) The welded-leftover hazard (`group[3:]` joins into `Body`, lowest pieces first) **cannot be settled from names by construction** - stated as unproven rather than claimed. (4) A15's LOD1 hazard narrows: LOD1 part names are IDENTICAL to LOD0 on all six, so topology is fine and only the decimated lengths differ. Sharpens the parked fix: `MaxSink` cannot be one share per machine either, because the fold margin differs front to rear - it wants `MinSpan` of the leg that sets `floor`. | none run - editor held; no code touched |
| A15 | 2026-09-25 10:52 | 1 | W6 4 -> **3** (a score REMOVED) | NO EDITOR - an interactive one held 1.5 GB, so no captures, no batch run, no write into `Assets/`. Audited the board instead and **two prior claims did not survive.** (1) A14's mechanism was misattributed: **Pincer's legs are ONE PIECE** (from `crabsplit.build_pincer`, not from comments), so it takes the `Solve` branch that DOES have a `StretchMin` floor - the tempting one-line response to A14 would have been a **no-op on the machine that failed**, and the polarity was backwards, since a one-piece leg's 80% short limit is the TIGHTEST on any machine. (2) **A7's belly lift - my own applied change - CANNOT LIFT:** it computes a bounded vote up to `means + MaxSink` (`:588`) and then `Clamp(Height, means - MaxSink, means)` at `:600` deletes it every frame. It only cancels sink; the parapet case it was written for is exactly the one needing to exceed `means`. Also: the same clamp discards `ceiling`, which is the 118% long tail. **My own proposed safer reorder was broken** - `Mathf.Clamp` returns `min` when `min > max`, so it rides above `means` anyway and is discontinuous at `value == floor`. Re-derived the deciding arithmetic independently: fold margin is `means - floor = 0.0965*S`, but `MaxSink = 0.2226*S` on Pincer - **`MaxSink` exceeds the entire margin on all six machines (3.3x on Banner)**, so no ordering is safe and **the constant is the fix, not the statement order**. Unscored hazard found: `TankRenderer.Pose:1216` solves against `Lods[1]`, whose `Reach` comes from a decimated mesh - the far LOD is drawn against the wrong leg length. | none run - editor held; no code touched |
| A14 | 2026-09-25 10:30 | 1 | T3 0 -> 2, the last unscored Walk/Terrain line | ANIMATION LOOP (cycles A1-A13 are written as prose sections below rather than rows; this row resumes the table). Editor was closed, so A13's probe finally ran in batch mode - confirmed by name, no compile errors. **Every foot that goes down into a trench lands exactly on its floor: 0.0% off on all eight cases, both machines, worst 0.00 m.** But Pincer draws a leg detached from its own planted foot on **29.3%** of in-trench plant-ticks in a shallow 1.2 m slot, worst **1.165 m**; Redoubt is 0.000 m throughout. The harsh critique cleared my probe's arithmetic line by line and then found a defect I had not suspected: it counts only the LONG clamp, so ~26 of those 29 points are legs folded TOO SHORT - a jointed leg has no `StretchMin` at all (`:740`). Mechanism brackets a null: 1.2 m is the short tail, 1.8 m is legal, 2.5 m is the long tail. Proximate cause verified in source: the fold `floor` (`:599`) is overridden by `Clamp(Height, means - MaxSink, means)` on the next line (`:600`). **NO fix applied** - the reorder risks trading a short tail for a long one, and the comment at `:562-578` documents that hazard. Two-tail measurement written but NOT run: the editor came back mid-cycle and batch mode refused. Probe withdrawn, tree clean. Also found: **`WalkerGait.cs` is untracked in git**, and my `.before` backups were sitting inside `Assets/` (moved out). | probe 1/1 green; no game code touched |
| CLOSED | 2026-09-24 09:25 | 1 | W1 4, T5 5, W4 3, W5 3, W3 2, W6 2, W2 1, G1 1, G5 0, 13 lines never scored | LOOP ENDED at its 09:23 stop, cron 86f3198a deleted. 51 cycles over 7h 43m. ONE fix applied and gated green (cycle 47, the toe definition); one applied and REVERTED for going red (cycle 47, LEAN); three identified, tested and left parked for want of a free editor. Zoom level never advanced past 1 - the board requires 8 on every line and the best is 5. Nothing committed. | last run GREEN 13/13 (cycle 47) |
| 51 | 2026-09-24 09:22 | 1 | W5 - -> 3 (half the line is absent, half works) | attacked W5. THE LEAN DOES NOT EXIST: `yawRate` appears exactly TWICE in WalkerGait - the signature and the `sweep` term - and Roll is only ever the foot plane plus the lost-leg hole, so nothing in the gait leans a machine into a turn. Settled by enumeration, not sampling. The other half works: outside legs step 31-70% further than inside ones, correct sense on all six. CAVEAT recorded - the port omits Unity's reachable-annulus clamp, so the absolute step lengths overstate and the real ratio is likely smaller. Fix identified (a roll term from yawRate x pace) and NOT applied - the owner's editor is still in use. No project code changed. | not run (an interactive editor is in use) |
| 50 | 2026-09-24 09:20 | 1 | W4 4 -> 3 (cycle 49's score was too generous and its duty figures were WRONG) | TESTED W4 instead of arguing it, by porting the step DECISION (pure scalar arithmetic, no frames to get wrong) and running 12 s of walking. Rhythm CONFIRMED: Pincer walks the exact alternating tripod at 0% jitter. But the trigger is DEGENERATE - every step on every machine fires on `runningOut`, none on drift or stretch, so TriggerShare, DesperateShare, ExtendAt and Frantic are DEAD CONSTANTS. Proven cause: a foot is planted at 89% of its reach while the step threshold is 74-86%, so every leg is 'running out' from the instant it lands, by 0.33-0.51 m. Duty is 52% planted, not the 58-81% cycle 49 computed. Tested a fix in the port: splitting StepSafety's two jobs takes Redoubt from 44% planted / 91% jitter to 76% / 4%. NOT applied - the owner's editor is in use. No project code changed. | not run (an interactive editor is in use) |
| 49 | 2026-09-24 09:12 | 1 | W4 - -> 4 (the first line to score on a GUARANTEE rather than a fault) | attacked W4 analytically, which is legitimate here because the step decision is pure scalar arithmetic. 'Legs do not all lift at once' is guaranteed BY CONSTRUCTION, not by tuning: allowance = max(1, min(3 or 2, alive/2)) caps airborne legs at half the machine. Average load is 0.77-2.07 against caps of 2 and 3, so legs never queue. Duty factor 58-81% planted, which is a walk, not a shuffle. Banner is the outlier at 58% and the shortest stride, 0.78 m. NOT verified: that the footfall pattern is actually periodic. No project code changed. | not run (an interactive editor is in use) |
| 48 | 2026-09-24 09:02 | 1 | W3 0 -> 2 | attacked W3 with two independent measurements, neither needing the editor (an interactive one is in use). LAG: pitch/roll are filtered TWICE in series - the gait lerps at rate 9, then the renderer springs the result at omega 7 - and reach 90% of a new tilt after 0.700 s, 0.84 m of travel. The renderer's spring is 162% of the lag on its own. FOOTPRINT: the pitch axis is degenerate - Censer's four feet span 0.01 m along the machine, Pavise 0.17, Pincer 0.36. Fix identified (skip the renderer's spring for walkers) and NOT applied: it cannot be gated. No project code changed. | not run (an interactive editor is in use) |
| 47 | 2026-09-24 08:56 | 1 | W1 2 -> 4, T5 1 -> 5, W2 0 -> 1 | FIRST APPLIED FIX IN 47 CYCLES, gated green. The editor freed, so both parked changes were tried against GaitTests. LEAN 0.45->0.30 went RED (Banner sank 0.46 m walking the level, tolerance 0.34) and was REVERTED. The toe definition was the real fault and it is now fixed in two parts: the toe is a REAL VERTEX rather than the bounding box's bottom-face centre, and a Socket_Toe keeps its x/z but takes its HEIGHT from the mesh. Every contact and every slide improved; Pincer's belly regressed 0.33 -> -0.15 m, which is a trade, not a win. | GREEN 13/13 |
| 46 | 2026-09-24 08:52 | 1 | W1 0 -> 2 (cause measured, and it is the SAME cause as T5 and W6) | attacked W1 by sweeping the body over a NAILED toe target and following the contact vertex. It slides 0.77 m (Pincer) to 1.75 m (Censer) over a 0.40 m stance - 1.9x to 4.4x the body's own travel. First measurement separated a real slide from a lowest-point identity change and the correction MATTERED: raw numbers were 1.0-2.2 m, of which Censer's 2.15 m was 0.41 m of jumping. ONE CAUSE now explains three lines: rig.Toe is not the vertex that touches the ground. No critique agent - still no valid render, and stale renders from an older pose would be worse than none. No project code changed. | not run (a batch run holds the editor) |
| 45 | 2026-09-24 08:48 | 1 | W6 0 -> 2 (cause measured, not fixed) | attacked W6 with arithmetic on the rig rather than a picture, because the stance is fully determined by three constants. EVERY leg on ALL SIX machines is asked to span 89.3% of its own modelled length while standing still - a constant 10.7% mismatch, identical everywhere, caused by STEP_SAFETY 0.97 x cos(asin(LEAN 0.45)) with the foot at mid-ring. Jointed legs BEND it away invisibly; one-piece legs can only be SCALED, so Pincer (6 of 6 one-piece) and Censer (4 of 4) are drawn 0.35-0.36 m short. No critique agent: the harness refused both renders, so there was no valid image to score. Fix identified and NOT applied - see cycle 45 notes. No project code changed. | not run (a batch run holds the editor) |
| 44 | 2026-09-24 08:44 | 1 | W2 0 held, but measured for the first time | attacked W2 by feeding the Blender harness Unity's MEASURED ride heights. It REFUSED all three machines - Unity's number buries Pincer 4.68 m. Found why cycle 38's numbers disagreed: stand() returns a LIFT delta, Unity's Stand() an ABSOLUTE height from the Body pivot - not the same quantity. Tested the Body-pivot hypothesis and DISPROVED it in the same cycle: Body is found and its pivot is at z=0.000, so it coincides with the empty. Harness change KEPT (it turns a silent incomparability into a loud refusal). No project code changed. | not run (a batch run holds the editor) |
| 43 | 2026-09-24 08:32 | 1 | W1 0, T5 1 (first Unity evidence on either) | FIRST UNITY RUN IN 43 CYCLES. Measured the shipped draw path on flat ground. Cycle 42's prediction is MOSTLY WITHDRAWN: the gait's ride height lifts carried equipment clear, so Pincer's Reactor, Censer's Drum and Kettle's Mortar are all above ground. It holds for two machines only - Pavise (Body 0.46 m, Shield 0.29 m, Claw_R 0.13 m under) and Banner (both Claws 0.32 m). TWO NEW FAULTS instead: Kettle's leg geometry is 1.05 m UNDERGROUND, and Pincer's lowest leg vertex FLOATS 0.64 m clear. Probe deleted after reading; kept at scratchpad rig/BuriedProbe.cs. No project code changed. | GREEN 13/13 (GaitTests + probe, EditMode batch) |
| 42 | 2026-09-24 08:19 | 1 | unchanged | measured the RAW asset, unposed, in the verified frame. Five of six machines author carried equipment below their own lowest leg point - Pavise's gun by 2.32 m. Predicts the same fault in the shipped game. No project code changed. | not run (owner holds the editor) |
| 41 | 2026-09-24 08:09 | 1 | unchanged | re-measured in the verified frame. Bodies are NOT buried; carried equipment is - Drum 1.50, Gun 1.75, Reactor 1.79 m. Censer plants every leg at -0.00..+0.00. Cycle 38's ride-height diagnosis is withdrawn. No project code changed. | not run (owner holds the editor) |
| 40 | 2026-09-24 07:59 | 1 | unchanged | named the up axis from two Unity numbers at once. Blender's +Z with NO rotation reproduces the Body's 4.84 m extent AND its 6.52 m top exactly. Cycle 34 was right, cycle 37 wrong. Frame set to 0 and now verified on two independent quantities. No project code changed. | not run (owner holds the editor) |
| 39 | 2026-09-24 07:49 | 1 | unchanged | checked the Body's extent against Unity's. The -90 frame gives 6.90 m along the candidate up axis; Unity reports 4.84 m with the hull top at 6.52. The frame is still wrong - the turret test was necessary but not sufficient. No project code changed. | not run (owner holds the editor) |
| 38 | 2026-09-24 07:39 | 1 | unchanged | re-ran the posed set in the verified frame. Consistent signature: every Body buried 2.25-4.63 m with a computed lift of ~0, while Censer and Pavise plant every leg perfectly. The remaining gap is stand() returning ~0 where Unity's returns metres. No project code changed. | not run (owner holds the editor) |
| 37 | 2026-09-24 07:29 | 1 | unchanged | settled the orientation by testing all four rotations against a fact Unity states. -90 deg is the frame: turret +6.28 m above body, matching Unity's +6.28 exactly. Default set. Question closed after six cycles. No project code changed. | not run (owner holds the editor) |
| 36 | 2026-09-24 07:19 | 1 | unchanged | measured hip heights in the ROTX 0 frame against Unity's. They do not match on any axis. The turret delta settles it numerically: up is -Y, so cycle 34's "no rotation" is wrong too. Visual and numeric evidence now openly contradict; recorded as unresolved. No project code changed. | not run (owner holds the editor) |
| 35 | 2026-09-24 07:09 | 1 | unchanged | first posed run on upright machines. All six refused, with honest faults: carried equipment buried on five, Kettle's shin 4.39 m under, Redoubt's joint 0.39 m open. Censer's legs plant perfectly. No project code changed. | not run (owner holds the editor) |
| 34 | 2026-09-24 07:00 | 1 | unchanged | rendered the Pincer at four orientations and looked. NO ROTATION is correct - Blender's importer already converts the axes. The harness has had the machines on their side for 32 cycles. Default set to 0. No project code changed. | not run (owner holds the editor) |
| 33 | 2026-09-24 06:50 | 1 | unchanged | rendered the unposed asset at the "corrected" -90 deg axis. It is visually wrong too - the machine lies on its side. Neither sign is right, so cycle 32's derivation rested on a bad assumption. Axis to be settled by rendering all four, not by algebra. No project code changed. | not run (owner holds the editor) |
| 32 | 2026-09-24 06:39 | 1 | unchanged | settled the up-axis from Unity's own rig numbers: the harness has been rendering upside down since cycle 2. Corrected to -90 deg and KEPT, though it renders worse, because it exposes a second fault rather than causing one. No project code changed. | not run (owner holds the editor) |
| 31 | 2026-09-24 06:29 | 1 | unchanged | added the all-parts ground check the critic asked for. It names the buried parts at once - all of them carried equipment, not legs. Only Censer passes. An unresolved contradiction with how Unity draws the Pincer is recorded rather than guessed at. No project code changed. | not run (owner holds the editor) |
| 30 | 2026-09-24 06:19 | 1 | re-pose scored on assembly: Censer 4/Pavise 3/Redoubt 2/Pincer 1/Kettle 0 on "is it one object"; feet-on-ground ZERO for all five | put the elevations to the critic instead of building a sixth numeric proxy. It measured off the ground band and contradicted my readings; the contradiction is explained and it is my checks that are wrong. No project code changed. | not run (owner holds the editor) |
| 29 | 2026-09-24 06:08 | 1 | unchanged | scan-then-bisect fixed Kettle's numbers (-0.00..+0.00, joints 0.00) but the elevation shows it still in pieces. The gate passes it because the gaps PRE-EXIST and the check only catches gaps that grow. The critics were right about Kettle. No project code changed. | not run (owner holds the editor) |
| 28 | 2026-09-24 05:58 | 1 | unchanged | swept the aim function across Kettle's four legs. It is NOT monotonic - it rises, peaks and turns over - so the bracket has both ends negative and the bisection never runs. A solution exists; the bracket steps over it. No project code changed. | not run (owner holds the editor) |
| 27 | 2026-09-24 05:48 | 1 | unchanged | made the height bisection test the same solve it applies. Three machines now pass fully (Pincer, Censer, Pavise). Redoubt's hover 0.97 -> 0.50 m; Kettle regressed from hovering to one leg buried 1.25 m. No project code changed. | not run (owner holds the editor) |
| 26 | 2026-09-24 05:38 | 1 | unchanged | third knee attempt, guided by cycle 25's measurement. THE SCATTERING IS FIXED - joints +0.00 on Kettle, which came apart in both previous attempts. Articulated legs now bypass the per-leg settle and hover. No project code changed. | not run (owner holds the editor) |
| 25 | 2026-09-24 05:29 | 1 | unchanged | instrumented the knee solve before touching it. Measured cause of the Kettle scattering: the tucked foot target sits INSIDE the leg's minimum reach, so the solve folds the leg back on itself at 176.8 deg. Diagnostic mode contaminated the pose - noted. No project code changed. | not run (owner holds the editor) |
| 24 | 2026-09-24 05:21 | 1 | unchanged | tucked the foot radius to 0.30 of leg length to fight the sprawl. First edit silently missed its anchor; applied correctly on the second, numbers moved, picture essentially unchanged - the height bisection compensates for it. No project code changed. | not run (owner holds the editor) |
| 23 | 2026-09-24 05:10 | 1 | unchanged | retired the toe check; per-leg contact is now the gate. Four machines pass and render with every leg planted. The elevation shows the pose still does not read as standing, and a gun barrel sits below ground. No project code changed. | not run (owner holds the editor) |
| 22 | 2026-09-24 05:02 | 1 | unchanged | per-leg settle WORKS: every leg on five machines now plants at +0.00/-0.00. The toe check is left inconsistent with it and still refuses the renders. No project code changed. | not run (owner holds the editor) |
| 21 | 2026-09-24 04:50 | 1 | unchanged | swept the bisection function instead of guessing at it. It can never return a positive value by construction, which is why both cycle 20 attempts were no-ops. Correct fix identified: bisect the target HEIGHT, not the radius. No project code changed. | not run (owner holds the editor) |
| 20 | 2026-09-24 04:42 | 1 | unchanged | attempted a per-leg settle twice. Both attempts were NO-OPS - numbers identical to cycle 19 to the centimetre. Not guessing a third time; next cycle instruments the function instead. No project code changed. | not run (owner holds the editor) |
| 19 | 2026-09-24 04:29 | 1 | unchanged | added a settle so the lowest leg rests on the ground. No machine is buried any more; the burying is replaced by the shorter legs hovering 0.23-0.91 m. Pavise clean. No project code changed. | not run (owner holds the editor) |
| 18 | 2026-09-24 04:19 | 1 | unchanged | added per-leg ground contact. Confirms the Redoubt hovers (+0.42 m) and reveals Pincer and Kettle have legs buried 0.4-1.15 m. Only Pavise is clean. No project code changed. | not run (owner holds the editor) |
| 17 | 2026-09-24 04:10 | 1 | unchanged | fixed the orthographic elevation (ground slab, aspect-correct framing, figure clear). It immediately showed the Redoubt hovering with only loose parts touching, contradicting its own "lowest point -0.00". No project code changed. | not run (owner holds the editor) |
| 16 | 2026-09-24 04:00 | 1 | re-pose scored at tactical distance: Censer 19/60, Redoubt 14, Pincer 10, Pavise 9, Kettle 1 | rendered the five clean machines at zoom 1 and had them scored. Critic's binding theory answered by cycles 2 and 14. Three ortho renders found unusable. No project code changed. | not run (owner holds the editor) |
| 15 | 2026-09-24 03:48 | 1 | unchanged | measured every leg pivot against its parent's geometry. Result INCONCLUSIVE - the metric is a bounding box and cannot represent a domed carapace. Banner NOT shown to be an outlier; no fault claimed. No project code changed. | not run (owner holds the editor) |
| 14 | 2026-09-24 03:38 | 1 | unchanged | fixed the cycle 13 contact-measure regression; all six back to toe error 0.000. The came-apart check caught a real one: Banner's Thigh_LB opens 0.08 m under the plain rigid sweep. Five of six pose clean. No project code changed. | not run (owner holds the editor) |
| 13 | 2026-09-24 03:30 | 1 | unchanged | knee solve retried with the tip measured from the knee: stopped the Redoubt scattering but the Kettle still came apart. REVERTED a second time. Added a came-apart check that would have caught both. No project code changed. | not run (owner holds the editor) |
| 12 | 2026-09-24 03:18 | 1 | unchanged | attempted a two-bone knee solve for the four jointed machines. REVERTED - it scattered the Redoubt. Rigid sweep restored and confirmed (lowest point back to -0.00). No project code changed. | not run (owner holds the editor) |
| 11 | 2026-09-24 03:08 | 1 | unchanged | measured leg rotation angles and splay directions. Cycle 10's two structural claims DISPROVED: legs rotate 98-128 deg, splay mirrors correctly. Real defect identified: chains rotate rigidly without articulating. No project code changed. | not run (owner holds the editor) |
| 10 | 2026-09-24 03:00 | 1 | ship scores unchanged; re-pose scored separately W2 1, W3 1, W6 0, T5 1, S1 2, S2 2 | Socket_Toe now used where it exists; all six machines pose with 0 unreachable legs. Harsh critique run on the posed set: ship none. No project code changed. | not run (owner holds the editor) |
| 9 | 2026-09-24 02:50 | 1 | unchanged - scores still describe the shipped machine, not my re-pose | FIRST VALID STANCE. Toe redefined as the furthest point from the hip; spans now agree with Unity within ~5%. Censer stands on its legs, lowest point -2.03 -> -0.23 m; Pincer -3.13 -> -1.55 m. No project code changed. | not run (owner holds the editor) |
| 8 | 2026-09-24 02:37 | 1 | unchanged | measured carapace hang against Unity's leg lengths. Cycle 5's "legs too short to stand" speculation DISPROVED - every machine clears, margins +1.47 to +4.74 m. Derived belly clearance per machine. No project code changed. | not run (owner holds the editor) |
| 7 | 2026-09-24 02:27 | 1 | G1 evidence added, score held at 1 | measured every gun part's modelled yaw against the sim's AimTolerance. Hypothesis disproved: all 0.00 deg. Found a 21 deg asymmetry between the Maw's two sponson muzzle sockets. No project code changed. | not run (owner holds the editor) |
| 6 | 2026-09-24 02:18 | 1 | unchanged | measured leg length against how far each body hangs below its hips. Result REJECTED - the script reports 0.00 m legs on the Censer and sculpt-scale lengths on the Pincer, both impossible. No project code changed. | not run (owner holds the editor) |
| 5 | 2026-09-24 02:10 | 1 | unchanged - evidence still not valid | lift now positive (Pincer +3.51, Kettle +4.24) and toe error exactly 0.000 on Pincer/Kettle/Redoubt; Censer/Pavise/Banner refused with legs that cannot reach. But the "passing" machines render 3 m underground, so the checks still do not measure standing. No project code changed. | not run (owner holds the editor) |
| 4 | 2026-09-24 01:58 | 1 | unchanged - still no valid posed evidence | world-space solve replaces the two-origin one; added a refusal gate so an impossible stance is not rendered. Scale figure confirmed working. The gate passed the Censer on a VACUOUS assertion - see notes. No project code changed. | not run (owner holds the editor) |
| 3 | 2026-09-24 01:50 | 1 | unchanged - no valid evidence produced | ported WalkerGait's Stand() and a per-leg swing into Blender (scratchpad rig/pose.py). Solves a ride height of 3.51 m for the Pincer but puts the machine 3.13 m underground, so it rendered nothing worth scoring. No project code changed. | not run (owner holds the editor) |
| 2 | 2026-09-24 01:42 | 1 | unchanged; cycle 1 scores marked NOT VALID as evidence about the shipped machine | measured parent/child gaps on all six; added 2 m figure, 2 m ground grid and an orthographic elevation to the render harness. No code changed. | not run (owner holds the editor) |
| 1 | 2026-09-24 01:52 | 1 | W2 0, W3 0, W6 0, T5 0, G1 1, G5 0 | first evidence rendered: all six machines, 3/4 and side, shipped FBX at x2.5 standing on flat ground. No code changed. | not run (owner holds the editor) |
| 0 | setup | 1 | not yet scored | scoreboard created; Blender sheet mapping confirmed (A=Pincer B=Kettle C=Censer D=Pavise E=Banner G=Redoubt F=Cutter) | 255 EditMode / 14 PlayMode, 1 pre-existing red not ours |

---

## Standing constraints for the loop

- **Never commit.** The owner commits.
- **Never write into `Assets/` while another session holds the editor** (`Tools/editor_lock.py status`). Blender
  work happens in the scratchpad; only a verified re-pose is copied in, and only with the slot claimed.
- **Revert any change that turns the gait tests red**, and log why it failed rather than trying again blindly.
- The machines' size is fixed by `VehicleSize` (walkers x2.5, tanks x1.7). The board is about how they move and
  are built, not how big they are.


---

## Cycle 1 notes

**What the evidence says.** Every machine rests its belly on the ground in the pose it ships in. Ranked worst
to best for "reads as a machine standing on its legs": Kettle, Pavise, Censer, Pincer, Banner, Redoubt. Only
the Redoubt holds its hull above the plane, and only because it is stood on its hull base rather than on legs.

**The chosen fix, for cycle 2.** Pose Pincer's six legs, not Kettle's, even though Kettle is worst. Pincer's
limb chains are continuous and it already has two load-bearing pads under the front, so it is the cleanest
candidate; and Censer, Banner and Pavise share its radial-limb topology, so the joint angles settled on the
Pincer become a template for four machines. Target: hull underside about half the machine's width off the
ground, so a 2 m soldier passes under the belly.

**Two faults in my own render harness, found by the critique and worth fixing before more evidence is taken:**
1. There is no scale figure in frame. I told the agent a soldier is 2.0 m; none was rendered, so every
   judgement about clearance is relative to the machine's own proportions and cannot be checked. Add a 2.0 m
   figure standing beside each machine.
2. Neither view is a true side elevation - both are elevated perspective, and the "side" is a three-quarter
   rear. Ride height, belly clearance and foot penetration cannot be measured from those. Add an orthographic
   side pass at hull height with a ground grid, and a straight front pass.

**One claim deliberately NOT acted on yet.** The critique reports detached, floating limb segments on Kettle,
Pavise, Redoubt and Censer and calls it endemic. That may well be true, but twice today a machine that looked
scattered turned out to be my own transform bug rather than the asset - once in Unity (mesh vertices that
silently would not scale) and once in this very Blender script (parts scaled about their own pivots). The
hierarchy dump shows a clean parent chain: Body <- legs/claws/turrets, Turret <- Gun, Claw <- Jaw. So before
anyone rebuilds a rig on this, cycle 2 must prove the gaps are in the asset and not in the import.


---

## Cycle 2 notes - the evidence base was wrong, and cycle 1's scores with it

**Settled: the parts really are separated in the file.** Measured, not eyeballed: the gap between each child
part's bounding box and its parent's, in metres of the shipped machine. Pincer 9 of 16 children clear of their
parent (Body-Reactor 4.34 m, Body-Turret 3.01 m, Body-Leg_R1 2.71 m); Kettle 8 of 16; Censer 5 of 8; Banner 5
of 11; Redoubt 7 of 14; Pavise only 2 of 11. The metric validates itself: Claw_L to Jaw_L measures 0.000.

**But that is the REST pose, and the rest pose is not what ships.** The same machine in the same render looks
coherent from three-quarters and visibly exploded from a true side elevation - the angle was hiding the
daylight. And in Unity it looks solid, because `WalkerGait.Solve` rotates every leg out of the sculpted pose
into a solved stance before anything is drawn. The separation is the gap the gait closes.

So cycle 1 scored a pose the player never sees, and the critique's central finding - "five of six were never
modelled standing, bellies on the ground" - is true of the FILE and says nothing about the shipped machine.
Those scores stay on the board as a record but are **not valid evidence** about how the game looks. The
critique's structural recommendation, to rebuild rigs because segments float, would have been work done
against a phantom.

**What this changes for every later cycle.** Rendering the FBX is only useful for judging the asset. To judge
the animation and the rig as they ship, the evidence must be POSED:
- Blender must apply the gait's stance solution before rendering - the body settled until the shortest leg
  stands at `Lean` 0.45, each foot in the middle of the ring it can reach - which is the port of `Stand()` and
  `Solve()` from WalkerGait.cs. That is now the prerequisite for cycles 3 onward.
- Or capture from Unity, which poses for free, whenever the owner's editor is free.

**Cycle 3's job:** port `Stand()` and the two-bone solve into the Blender script and re-render the Pincer
posed. Only then re-score W2, W3, W6 and T5 - and expect them to move a long way, because the machine the
player sees is not the one on the board right now.

**Harness fixes landed this cycle:** a 2.0 m figure (a man at UnitScale 1.125) stands beside each machine, a
2 m ground grid is drawn, and a true orthographic side elevation is rendered alongside the two perspective
views. The ortho pass still needs its framing fixed - `ortho_scale` crops the machine and hides the grid.


---

## Cycle 3 notes - the stance port, half working

Wrote `scratchpad/rig/pose.py`, a port of the stance half of WalkerGait: settle the body until the shortest-
reaching leg stands at `Lean` 0.45, put each foot in the middle of the ring that leg can reach, out along the
way its own hip points, then swing each leg about its hip until its toe lands there. One-piece legs are exact;
jointed legs are swung as a rigid chain, which the real solver would bend.

It produces a sensible-looking ride height for the Pincer, 3.51 m, and then puts the machine 3.13 m into the
ground. **Not rendered and not scored** - a wrong pose would have produced worse evidence than no pose, and
this loop has already spent one cycle scoring a pose nobody sees.

The fault is almost certainly the frame the height is measured in. Unity's `Stand()` returns the height of the
model ROOT above the ground, with each `rig.Hip.y` measured from that root. In the Blender scene there are two
candidate origins - the FBX's own root object and the empty this script parents everything to in order to
scale and rotate it - and the lift is being computed against one while the hips are measured against the
other. The machine is being moved by the difference.

**Cycle 4:** measure hips against the FBX root rather than the wrapper empty, assert that after posing the
lowest point of the machine is 0.00 +/- 0.02 and that every toe is within 2 cm of the ground, and only render
once those hold. An assertion in the script is worth more than another look at a picture.


---

## Cycle 4 notes - the assertion was vacuous, and caught itself

**What works now.** The 2 m figure renders beside the machine and reads correctly - against it an 8 m Censer
is unmistakably gigantic, which is the scale check cycle 1 could not make. The refusal gate works too: the
Pincer was measured at 1.111 m of toe error and was NOT rendered.

**What does not.** The Censer passed with "worst toe off the ground 0.000" and is still lying belly-down with
its legs splayed sideways in the render. The assertion is worthless, and worth writing down exactly why:
`toe_of()` returns the LOWEST POINT of the leg chain, recomputed after the rotation. Rotating a leg changes
which vertex is lowest, and the body had just been lowered until something touched the ground, so the
recomputed lowest point sits on the ground by construction. The check confirms only that the machine is
resting on something. It cannot fail for the reason it was written to catch.

**The real fault underneath.** Both machines solved to a NEGATIVE lift - Pincer -3.13 m, Censer -2.03 m - so
the solver wants to lower a machine whose belly is already on the floor. Two compounding causes:
1. Hips are measured after the harness has already dropped the machine so its lowest point is zero, which
   puts them far higher than the solve expects, so it lowers them.
2. In the sculpt a leg sticks OUT sideways, so its "toe" is beside its hip, not below it. The ring radius is
   computed from `drop = hip.z`, and when the hip is higher than the leg is long `rise()` returns zero and the
   foot target collapses to a point directly under the hip that the leg cannot reach.

**Cycle 5:** two specific changes, neither of which is another look at a picture.
- Track the toe as a POINT carried by the leg's own transform, not as a recomputed lowest vertex, so the
  assertion measures where the toe actually went and can fail.
- Measure hips BEFORE the harness grounds the machine, and let the solve place the root itself rather than
  correcting a position something else chose.


---

## Cycle 5 notes - three real fixes, and the check that is still missing

**Fixed, and each was a genuine error:**
1. Hips are measured before anything moves the machine, and the solve now places the root itself. Lifts went
   from negative (lowering a machine already on the floor) to positive: Pincer +3.51, Kettle +4.24.
2. The toe is carried as a point on the leg rather than recomputed as the chain's lowest vertex, so the
   assertion can now fail - which is what cycle 4's could not do.
3. The foot ring radius is not free for a rigid leg. The gait picks the middle of the reachable ring and then
   bends or stretches to suit; a leg swung as one piece lands at exactly `span` from its hip, so the only
   radius whose foot touches is `flat = sqrt(span^2 - drop^2)`. Using the mid-ring left the toe 0.11-0.14 m
   out. With this, toe error is exactly 0.000.

**Still wrong, and the important part.** Pincer, Kettle and Redoubt pass every check - toe error 0.000, no
unreachable legs - and render with their bellies 3.13 m BELOW the ground plane. Two checks have now been
written that a machine lying underground can pass, which says the checks are testing the solver's internal
consistency rather than the thing a human looks at.

**What the evidence actually suggests**, and it is a claim about the asset rather than the harness: on these
machines the hips sit near the TOP of the carapace, and the body hangs below them by more than a leg is long.
No rotation of a rigid leg about its hip can lift that belly clear - the geometry forbids it. If that holds,
it is the real reason five of six were never modelled standing, and it means the fix the owner asked for
(re-pose in Blender) needs the legs lengthened or the hips moved, not just rotated.

That is a big claim and it is NOT yet proven - Unity does draw these machines standing, so either its solver
does something this port does not, or the Unity stance is achieved by bending chains this port swings rigid.

**Cycle 6:** add the only check that matters - after posing, the machine's lowest point must be at or above
the ground (-0.02 m tolerance) - and then answer the one question that decides the whole approach: for each
machine, how far below its hips does its body geometry hang, against how long its legs are? If the body hangs
lower than the legs are long, say so plainly and stop trying to pose it.


---

## Cycle 6 notes - the measurement was rejected, and the approach with it

The question was whether a rigid leg can lift these bellies clear: is a leg longer than the body hangs below
its hip? The script answered "CANNOT stand" for all three machines tested, with margins of -2 to -3 m. That
answer is **not recorded as a finding**, because the same output contains two impossible numbers:

- Censer's four legs measure **0.00 m long**. A leg of zero length is not a measurement, it is a bug - the
  part's origin happens to sit exactly at the bottom-centre of its own bounding box.
- Pincer's legs measure 1.22-1.39 m. Unity reports the same legs as **3.37 m** at shipped scale, a figure
  already measured and recorded on this board. The Blender script is reading sculpt-scale lengths while its
  body extents are correctly scaled, so it is comparing two different sizes against each other.

Had the verdict been taken at face value it would have justified lengthening every leg on every machine -
against a number that is out by a factor of nearly three.

**The lesson, and the change of approach.** Six cycles have now gone into re-deriving in Blender what Unity
already computes correctly: chains, toes, spans, stance heights. Every one of those re-derivations has been
wrong at least once - the up axis, the scale, the toe definition, the ring radius, the measuring frame - and
each error cost a cycle to find. Unity has this data right and has had it all along.

**So cycles 7 onward stop re-deriving and start importing.** When the owner's editor is free, dump the rig and
the solved stance straight out of Unity - `TankModel.Load` gives chain, hip, toe, bone lengths and reach per
leg, and `WalkerGait.Stand` gives the ride height - write it to JSON, and have Blender apply those numbers
rather than rediscover them. Blender's job is then only what it is actually good for and what the owner asked
for: moving the geometry and re-exporting. Until the editor frees, no more stance derivation.

**What is genuinely established so far, across six cycles:** the harness renders the shipped machine at the
right size, upright, textured, on a ground plane, with a 2 m figure for scale and a true orthographic
elevation; parts really are separated in the rest pose (measured, Pincer 9 of 16 children clear of their
parent); and the rest pose is not what the player sees, because the gait poses the legs at runtime. The board
still has no valid score, and saying so is the honest state.


---

## Cycle 7 notes - the aim hypothesis, disproved

The owner asked that vehicles shoot when their muzzle points at the target. The simulation already enforces
that: `TankGunnerySystem` refuses to fire until the gun is within `AimTolerance` (0.05 rad, 2.9 deg) of the
target bearing, with traverse simulated and hashed. The open question was whether the DRAWN barrel agrees
with the sim's belief.

The hypothesis was that it does not. TankRenderer draws a gun as `parent * LocalRot * yaw(GunYaw)`, so the
drawn bearing is `yaw(LocalRot) + GunYaw` while the sim believes it is `GunYaw` alone - meaning any gun part
modelled with a yaw of its own would be a permanent aim error of exactly that size.

**Measured, and it is not happening.** Every gun-like part on all six walkers and both tanks - Gun, Sponson,
Turret, Mortar, Cupola - has a modelled yaw of **0.00 deg**. The drawn bearing equals GunYaw exactly. That
failure mode is closed, and closed cheaply, before any code was changed to fix a fault that was not there.

What is still not established is the behaviour in play: whether the barrel visibly tracks and settles before
the shot, whether flash and recoil land on the barrel, and whether a moving target is led. Those need Unity
and stay unscored.

**One thing worth a look when the editor frees.** The Maw's two sponson muzzle sockets bear -134.72 deg and
+155.85 deg in their own frames. On a symmetric tank those should mirror, and they are 21 deg apart. It does
not produce an aim error - the renderer converts through `ArtRestYaw`, so the aimed pose stays consistent -
but it does mean the two sponsons sit at different rest bearings, and a Maw at rest will look subtly
lopsided. Cheap to check against the art, and cheap to fix in the splitter if it is wrong.


---

## Cycle 8 notes - the legs are long enough after all

Cycle 5 speculated that these machines physically cannot stand: that the carapace hangs below the hips by more
than a leg is long, so no re-pose could lift the belly clear, and the legs would have to be lengthened. It was
flagged as unproven. **It is wrong, and it is now measured.**

The error was in what counted as "body". Cycle 6 measured the hang of every non-leg part, which included the
claws and jaws - and a Pincer's claws reach forward and down by design, so the machine was being marked
unable to stand because its claws touch the ground, which is what claws are for. Measuring only the carapace,
and taking leg lengths from Unity rather than re-deriving them:

| machine | carapace hangs below highest hip | longest leg | margin | belly clearance standing at Lean 0.45 |
|---------|-------------------------------|-------------|--------|----------------------------------------|
| Pincer  | 1.90 m  | 3.37 m | +1.47 | **1.11 m** |
| Kettle  | -0.30 m | 4.44 m | +4.74 | 4.27 m |
| Censer  | -0.35 m | 2.96 m | +3.31 | 2.99 m |
| Pavise  | 1.31 m  | 3.76 m | +2.45 | 2.05 m |
| Banner  | 0.80 m  | 2.35 m | +1.55 | **1.30 m** |
| Redoubt | 1.86 m  | 6.53 m | +4.67 | 3.97 m |

Kettle's and Censer's hips sit BELOW their carapace bottoms, which is why their margins are so large.

**Two things this settles.**
1. Re-posing is viable on every machine - the job the owner picked for Blender is not blocked by geometry.
   Every underground render so far has been the solver's fault, not the asset's.
2. An art-direction fact worth knowing before anyone poses anything: **a 2 m soldier does not fit under a
   Pincer or a Banner.** Their standing belly clearance is 1.11 m and 1.30 m. The cycle 1 critique recommended
   posing every machine so infantry walk under the belly; for four of the six that is achievable, and for
   those two it is not without lengthening legs, which is a design change and the owner's call, not the
   loop's.

**Cycle 9:** with the geometry cleared, the fault is entirely in `rig/pose.py`. Take the leg span from the
same place this cycle did - Unity's rig numbers - instead of deriving a toe from a bounding box, which is the
single assumption that has been wrong in every cycle since 3.


---

## Cycle 9 notes - the toe was the whole problem

One line of definition, wrong since cycle 3, was behind every failure: the toe was taken as the bounding
box's bottom-centre. That is the tip only of a leg that hangs straight down. These legs stick out sideways in
the sculpt, so the bottom-centre sits near the MIDDLE of the limb - it measured Pincer's legs at 1.22 m
against Unity's 3.37 m, and Censer's at 0.00 m because the part's origin happened to land on it.

The tip is simply the point furthest from the hip. With that:

| machine | leg spans measured | Unity's figures | lowest point before -> after |
|---------|--------------------|-----------------|------------------------------|
| Pincer  | 3.56 / 3.21 / 3.14 | 3.37 / 3.16 / 3.04 | -3.13 -> -1.55 m |
| Censer  | 3.30 / 3.19        | 2.96 / 2.89        | -2.03 -> -0.23 m |

The spans agree with Unity to about 5% and overshoot slightly, which is expected - the furthest bounding box
CORNER lies a little beyond the mesh's actual tip. Good enough for a stance; worth replacing with the real
`Socket_Toe` or Unity's own rig data before anything is exported.

**The Censer now stands.** Body carried up, legs reaching down and out, feet on the ground. It is the first
image in nine cycles that shows a machine standing on its legs rather than lying on its belly.

**Not scored, deliberately.** The board's lines describe the machine the player sees, and this stance comes
from my Blender solver, not from Unity's. It is evidence that a re-pose is achievable and what it would look
like - not evidence about what ships. Scoring it would repeat cycle 1's mistake in the opposite direction.

**Defects visible in the first good stance**, for the cycle that takes it further: the legs splay very wide,
which will read as a spider rather than a crab and will push the footprint past the 10 m the sim already
allows; and the Censer's drum still floats clear of the body, which is the rest-pose separation the gait does
not close because the drum is not a leg.

**Cycle 10:** pose the remaining five, bring the splay in by using each hip's own outward direction rather
than the radial one, and put a true `Socket_Toe` lookup in place of the bounding-box corner.


---

## Cycle 10 notes - all six stand, and the critic says ship none of them

With `Socket_Toe` used where the splitter left one, every machine now poses: 0 unreachable legs, toe error
0.000. Lowest point: Redoubt -0.00 (exact), Censer -0.23, Pavise -0.94, Pincer -1.60, Kettle -2.02, Banner
-3.61.

**Critique of the proposed re-pose** (scored against the re-pose, NOT against what ships, so the board's own
scores are untouched): W2 1, W3 1, W6 0, T5 1, S1 2, S2 2. Ranking best to worst: Pincer, Censer, Pavise,
Redoubt, Banner, Kettle. Verdict: ship none. "A pose that only survives one camera is not a pose."

**The one diagnosis worth acting on, and it is testable:** *segments translate instead of rotate*. Every joint
opens a clean gap while each segment keeps its rest orientation, which is what you get if limb parts are
moved to target positions rather than the chain being rotated about its hips. And the collapse fans the SAME
direction on five of six machines - six independent per-hip solves should fail in six directions, so one
shared direction points at a world-space axis being used where a per-hip one belongs.

**Both claims are plausible and neither is accepted yet.** Cycle 2 already measured that the parts are
separated in the REST pose - 9 of 16 of Pincer's children clear of their parent before anything is posed - so
the gaps the critic attributes to my solver may be the asset's, and the Censer demonstrably did rotate
upright rather than translate. The shared fan direction is the stronger of the two: `away` is computed from
each hip's world x/y, which is only equivalent to Unity's per-hip `splay` if the model's origin sits at the
machine's centre, and that has not been checked.

**Cycle 11:** settle both by measurement, not by eye. Log each leg's rotation angle and its `away` vector; a
leg that "translates" will show a rotation near zero, and a shared bias will show as `away` vectors clustered
in one half-plane. Then fix whichever is real.

**Two more harness faults the critic found, both fair:** the 2 m figure is occluded behind geometry in four of
twelve frames and sits above the contact line in two, so size judgements from those frames are unsound; and
there is still no ground grid in shot, which is why "hovering versus buried" could not be scored at all.

**One asset fault worth the owner's eye regardless of the pose work:** Pavise's shield plate hangs detached
above the carapace in both views. It is the part that makes a Pavise a Pavise, and it is separated in the
rest pose, so it will not be closed by any leg solve.


---

## Cycle 11 notes - the critic's mechanism was wrong, its symptom was right

Cycle 10's critique made two confident structural claims. Both are now measured and both are false.

**"Segments translate instead of rotate."** Every leg rotates: Pincer 116.6, 127.6, 124.9 deg; Kettle 128.9,
107.2, 128.3, 98.6 deg. Nothing is being translated to a target.

**"The collapse fans the same way on five of six - a world-space axis where a per-hip one belongs."** The
splay vectors mirror and fan exactly as they should. Pincer's left legs point (-0.85,+0.53), (-0.98,+0.19),
(-0.99,-0.13) against the right's (+0.85,+0.53), (+0.98,+0.19), (+0.99,-0.13); Kettle's four are mirrored in
both axes. The hips are distributed around the machine and each leg goes out along its own.

**But the symptom behind the wrong mechanism is real, and this is the actual defect.** The chain is rotated
as ONE RIGID BODY about its hip. Thigh, shin and foot all swing through the same 98-128 degrees and keep
their orientations relative to each other, so no joint ever articulates - a leg that should fold into a
bracket instead sweeps down like a spoke. That is exactly what "segments keep their rest orientation" looks
like from outside, and it is why the jointed machines (Kettle, Redoubt, Banner, Pavise) read worse than the
one-piece ones (Pincer, Censer): a one-piece leg has nothing to articulate and so loses nothing.

**A pattern worth recording about this loop.** Two critique cycles have now produced confident causal
diagnoses - cycle 1's "rebuild the rigs, the segments float" and cycle 10's "segments translate instead of
rotate" - and measurement has contradicted both. What the critic is reliably good at is naming what looks
wrong; what it has been consistently wrong about is why. Every one of its mechanisms that was cheap to test
has failed the test, and two of them would have justified expensive rebuilds. The loop should keep asking it
what is ugly and stop taking its explanations at face value.

**Cycle 12:** give the jointed legs a real two-bone solve - hip and knee, the same analytic solve WalkerGait
already uses - instead of a rigid sweep. That is the one change that would move W6 and W3 for four of the six
machines, and it is the remaining difference between this port and what Unity actually draws.


---

## Cycle 12 notes - the knee solve failed and was reverted

Cycle 11 identified the real defect: the leg chain is swept rigidly about its hip, so thigh, shin and foot
keep their relative orientations and no joint folds. The fix was an analytic two-bone solve - place the knee
from the hip and target distances, take the bend plane from where the sculptor already bent it, then rotate
thigh and shin separately.

**It made things worse and was reverted.** The Redoubt came out scattered: hull tipped, legs detached and
lying on the ground beside it, loose parts in the dirt. Reverted in the same cycle; the rigid sweep is
restored and the Redoubt is back to a lowest point of -0.00 m.

**The likely cause, for whoever picks it up.** The second swing rotates the shin to put the leg's tip on the
target, but the tip is found with `toe_of(chain_of(lr), hip)` - the point furthest from the HIP across the
WHOLE chain, thigh included. After the thigh has moved, that point may well be on the thigh rather than at
the foot, so the shin is rotated to bring the wrong point to the target and the leg comes apart. The second
swing needs the tip measured from the KNEE and restricted to the shin's own sub-chain.

The failed code is left in `rig/pose.py`, disabled and commented, so the next attempt starts from something
rather than from scratch.

**Where this leaves the work at cycle 12 of 48.** The Blender stance harness is sound: it renders the shipped
mesh at the right size, upright, textured, lit, with a scale figure, and it refuses to render a stance whose
toes miss the ground. All six machines stand. What it cannot yet do is bend a knee, and that is the single
remaining difference between this port and what Unity draws. The board's own scores are still the cycle 1
ones because no valid evidence about the SHIPPED machine has been produced - every render since has been of a
proposed re-pose, which is a different question.

**The honest summary for the owner:** twelve cycles have produced a working re-pose pipeline, a measured
belly-clearance table, two disproved structural theories that would each have justified rebuilding rigs, and
one disproved aim theory. They have not yet produced a single score for how the machines actually look in
the game, because that needs Unity and the editor has been held for all twelve.


---

## Cycle 13 notes - second knee attempt failed, and the check that should have existed

Cycle 12 diagnosed the knee solve's fault precisely: the leg tip was being found as the furthest point from
the HIP across the whole chain, so once the thigh moved that point could sit on the thigh. Fixed - the tip is
now found within the shin's own sub-chain and measured from the knee.

**It half worked and was reverted anyway.** The Redoubt stopped scattering and came within 0.123 m. The
Kettle still came apart completely: body upright, limbs and loose blocks strewn across the ground. So the
knee solve has a second fault beyond the one diagnosed, and two failed attempts is enough to stop guessing at
it. The code is disabled in place for a third attempt, which should start by reproducing the Kettle failure
on its own rather than across the set.

**The check that was missing, and now exists.** Both failures were passed by the existing checks: a scattered
leg still has a lowest point at ground level, so toe error reads 0.000, and its foot still "reaches", so the
unreachable count reads 0. Neither could see a machine coming apart, which is the actual failure and the one
a human sees instantly. The new check compares every child part's gap to its parent BEFORE and AFTER posing
and refuses the render if any joint opens by more than 5 cm.

It earned itself immediately: under the restored rigid sweep, Kettle and Redoubt both report joints opening
**0.00 m**, which confirms the rigid sweep keeps machines intact and that the scattering was caused by the
articulation rather than present beforehand.

**One regression introduced and not yet fixed.** Changing the toe measure to the furthest-from-hip corner
made the Censer fail at 0.408 m; on a one-piece leg that corner is not the point that touches the ground. The
carried-toe form from cycle 5 was correct for this and should come back, with the furthest-corner form kept
only for measuring span.

**Three checks now guard the stance**, and the progression is worth noting: toe error (cycle 4, could not
fail), unreachable legs (cycle 5, could not see scattering), joint opening (cycle 13, catches what a human
sees). Each was added only after the previous one passed something visibly broken.


---

## Cycle 14 notes - regression closed, and the new check finds a real pivot fault

The cycle 13 regression is fixed. Span is measured with the furthest-from-hip corner; CONTACT is measured
with the toe the leg was actually solved for, carried through the same rotation. Conflating the two made the
Censer read 0.408 m when nothing was wrong with it. All six machines are back to a toe error of 0.000.

**The came-apart check found a real fault on its first clean run.** Banner's `Thigh_LB` moves 0.08 m away
from its parent during a plain rigid sweep - no articulation involved. A rigid rotation about a joint cannot
open that joint, so the pivot being rotated about is not where the thigh actually attaches: the part's origin
sits away from its socket. Banner is refused; the other five pose with joints opening 0.00 m.

That matters beyond this harness. The same pivot drives `WalkerGait` in the game - it is the `Hip` the solver
rotates the leg about - so if Banner's thigh origin is off its attachment, the shipped machine has the same
error, just hidden by the gait constantly moving the leg. Worth measuring against the other machines before
concluding it is a fault rather than a sculpt quirk.

**State of the stance harness at cycle 14.** Five of six machines pose with toes on the ground and no joint
opening. Three independent checks guard it, each added after the previous passed something visibly broken.
The knee still does not bend, and two attempts at it have been reverted.

**Cycle 15:** measure how far every leg-root origin sits from its parent's surface, across all six machines.
If Banner is an outlier, it is a sculpt fault worth fixing in the splitter; if they are all offset and Banner
is merely the worst, then the gait has been rotating every leg about a point that is not its hip, and that is
a finding about the shipped game rather than about this harness.


---

## Cycle 15 notes - the pivot question is still open, and the metric was wrong for it

The question was whether Banner's leg pivots are off their sockets, and whether the other five share it -
because that same pivot is the `Hip` WalkerGait rotates legs about in the shipped game.

The measure was the distance from each leg root's origin to its parent's bounding box. Every machine came
back "off socket" by 1.3 to 4.7 m, which looks damning and means nothing. Pincer's Body box spans z 0.67 to
2.61 while its leg pivots sit at z -0.5 to 0.15, so the legs are outside the box in depth - not because they
are detached, but because the carapace is a dome that does not extend over its own leg sockets. An
axis-aligned box of a curved shell is simply not a description of where that shell's surface is.

**So no fault is claimed and Banner is not shown to be an outlier.** The 0.08 m joint opening on Banner's
Thigh_LB is still real and still unexplained; this cycle did not explain it.

**What would answer it:** the distance from the pivot to the parent's nearest MESH FACE, not its box - a
closest-point-on-triangle query over the parent's polygons. That is a different and slower measurement, and
it is worth writing only if the pivot question matters more than the knee, which it probably does not.

**A note on this loop's failure mode, now that there are fifteen cycles of it.** The pattern that keeps
recurring is not bad reasoning about the machines; it is reaching for a cheap proxy and then reading it as
though it measured the real thing. Bounding-box gaps for "parts look detached" (cycle 2), a recomputed lowest
vertex for "the toe" (cycles 3-9), the furthest corner for "contact" (cycle 13), and now a bounding box for
"is the pivot seated". Each was cheap, each was plausible, and each answered a question adjacent to the one
asked. The checks that have actually held up - joint opening, carried-toe contact - are the ones that measure
the thing a human would point at.

**Cycle 16:** stop opening new questions. Re-render the five machines that pose cleanly at zoom level 1, put
them in front of the critic, and get the first scored evidence since cycle 10.


---

## Cycle 16 notes - first scored set since cycle 10, and a theory already answered

Five machines posed clean and were scored at tactical distance, out of 60: **Censer 19, Redoubt 14, Pincer
10, Pavise 9, Kettle 1.** Accept into a build tonight: none. Censer reads best - the one image of fifteen a
stranger would read as a single intact machine. Redoubt has the best designed silhouette and is the only one
nameable from across the room, but two of its claws are visibly airborne.

**The critic's central theory, and why it is already answered.** It argued the fault is mesh-to-skeleton
binding rather than the stance solver - rigid parts holding bind-pose position while the solved root moves
out from under them - and proposed re-rendering in bind pose to decide: whole means the solver, exploded
means the binding.

That experiment has been run twice already. Cycle 2 measured the REST pose with no solver involved and found
Pincer with 9 of 16 children clear of their parent, Kettle 8 of 16, Censer 5 of 8. Cycle 14's joint-gap check
measures every parent-child gap before and after posing and reports **0.00 m of opening** on the machines
that pass. The parts are apart before anything poses them, and posing does not move them further apart. There
is also no skeleton to mis-bind: these are rigid parts in a parent hierarchy, which is how crabsplit exports
them and how TankModel reads them.

So that is the third confident structural theory from a critique agent that measurement contradicts, after
"rebuild the rigs" (cycle 1) and "segments translate instead of rotate" (cycle 10). The consistent shape is
worth stating: the critic sees the separation correctly every time and reaches for a rigging mechanism to
explain it every time, when the cause is that these machines are cut from a single sculpt into parts that
were never modelled to meet.

**Where it is right, and these are real harness faults.** All five orthographic elevations are unusable: no
ground plane in frame, a different background from the perspective shots, the subject tilted 30-40 degrees
off vertical, and Pavise and Redoubt cropped at the bottom edge. An elevation with no ground in it cannot
show ground contact, which was the entire reason for adding it in cycle 2. The scale figure is also occluded
or intersecting in three of five orthos, and framing is not constant across machines, so the cross-silhouette
comparison S2 is meant to test cannot be made from this set.

**Cycle 17:** fix the orthographic pass - ground plane and grid in frame, camera level, consistent framing
across machines, figure always clear - and re-shoot. It is the only view that can settle contact, and it has
been broken since it was added.


---

## Cycle 17 notes - the elevation works, and the first thing it shows is a hovering machine

Three faults fixed, each with a definite cause:
1. **No ground in frame.** A zero-thickness plane is exactly edge-on to a level camera and renders as
   nothing. Replaced with a thin slab, which reads as a crisp line at z=0. This is why five elevations were
   produced that could not show ground contact - the single thing an elevation is for.
2. **Cropping.** `ortho_scale` governs the LONGER sensor axis, so vertical coverage was only 900/1400 of it
   and tall machines were cut off at the bottom. Now sized so the machine's height fits.
3. **Figure hidden.** Moved forward-left of the machine instead of beside it, clear of the hull in every view.

**And it earned itself on the first render.** The Redoubt's body and legs sit visibly clear of the ground
line, with only a detached claw and a drum resting on it - while the solver reports its lowest point as
-0.00 m. The machine is hovering and the number says it is planted.

**That is the fourth check in this family to be satisfied by the wrong thing**, and the pattern is now
unmistakable: toe error (cycle 4, satisfied by a recomputed lowest vertex), unreachable legs (cycle 5,
satisfied by a scattered leg), lowest point (here, satisfied by loose parts that are not feet), against
joint opening (cycle 13) which has held up. A check that aggregates over ALL parts can always be satisfied by
some part; a check that names the specific thing - this joint, this toe - cannot.

**The concrete consequence:** "lowest point" must be measured over the LEGS only, not the whole machine. A
claw or a drum touching the ground is not the machine standing. That is cycle 18's fix, and it is small.

**A caution for whoever reads this later.** Every score on this board below cycle 16 was taken from
perspective renders that could not show hovering. The Redoubt scored 14/60 and was called the best-designed
silhouette of the five; it is also, it now turns out, floating. Scores taken before this cycle should be
treated as upper bounds.


---

## Cycle 18 notes - per-leg contact, and it indicts four of six

Ground contact is now measured on each LEG's own geometry instead of over the whole machine. The result, in
metres off the ground, highest leg and lowest leg:

| machine | highest leg | lowest leg | verdict |
|---------|-------------|------------|---------|
| Pavise  | -0.00 | -0.00 | clean |
| Censer  | +0.00 | -0.23 | one leg planted, one 23 cm under |
| Pincer  | -0.37 | -0.69 | every leg buried |
| Kettle  | -0.85 | -1.15 | every leg buried about a metre |
| Redoubt | +0.42 | -0.00 | HOVERING - refused |
| Banner  | -0.00 | -0.91 | refused on the 0.08 m joint |

The Redoubt reading confirms by number what cycle 17's elevation showed by picture, which is the first time
this loop has had a measurement and an image agree on a fault.

**The cause of the burying, and it is the same shape of error as everything else here.** The leg is swung
until its TOE - the point furthest from the hip - lands at z=0. But the furthest point from the hip is not
the lowest point of an angled leg. On a leg that stands out at 30 degrees the knee or the shin's underside
hangs below the toe, so placing the toe on the ground drives the rest of the leg through it. Pincer buries
0.69 m; Kettle, whose legs are longest and most angled, buries 1.15 m.

**Cycle 19's fix, and it is small.** After posing, take the lowest point across all legs and shift the body
by exactly that much, so the lowest leg geometry rests on the ground. It costs a little of the intended
`Lean` - the machine settles slightly - but it puts feet on the floor, which is what a viewer checks first.
Then refuse on sink as well as hover, so a buried machine cannot pass either.

**Checks now guarding the stance, in the order they were added and what each was added because of:**
toe error (cycle 4, after a pose 3 m underground passed); unreachable legs (cycle 5, after a foot ring
collapsed silently); joint opening (cycle 13, after two knee attempts scattered machines while passing);
per-leg contact (cycle 18, after a hovering machine reported a lowest point of -0.00). Each one exists
because the previous set passed something visibly broken.


---

## Cycle 19 notes - burying fixed, hovering revealed underneath it

The settle works. Every machine's lowest leg now rests exactly on the ground, so the 0.37-1.15 m of buried
leg from cycle 18 is gone.

What it uncovers is the next layer, and it is not a bug: settling the body by its LOWEST leg necessarily
leaves the others in the air, because the legs differ in length and in how far they stand out.

| machine | highest leg off the ground | lowest leg |
|---------|---------------------------|------------|
| Pavise  | -0.00 | -0.00 |
| Censer  | +0.23 | -0.00 |
| Kettle  | +0.31 | -0.00 |
| Pincer  | +0.32 | -0.00 |
| Banner  | +0.91 | -0.00 |

Only the Pavise plants every leg, and it does so because its four legs are near enough identical.

**Why this cannot be fixed by moving the body.** One rigid body has one height; the legs have several
different lengths. Any single height plants some legs and leaves the rest short or long. The game does not
have this problem because `WalkerGait` places each foot independently on whatever ground is under it and
then solves that leg to reach it - the body height is a compromise and the LEGS absorb the difference.

**Cycle 20:** do the same here. After the first swing, rotate each leg a little further about its own hip
until that leg's own lowest geometry touches, instead of settling the whole machine by the worst one. Each
leg then stands at a slightly different angle, which is what a real machine on flat ground does, and the
`Lean` becomes a starting point rather than a rule applied identically to legs of different lengths.

**Honest note on the trade made this cycle.** Hovering is arguably worse to look at than slight burying - a
floating foot reads as broken while a foot sunk 10 cm reads as mud. The settle was kept because it halves
the magnitude of the error (0.69 m buried becomes 0.32 m hovering on the Pincer) and because per-leg solving
removes both, but if cycle 20 fails, reverting to a small deliberate bury would be the better-looking of the
two wrong answers.


---

## Cycle 20 notes - two attempts, no change, and the right response is to stop guessing

The plan was to let each leg find its own angle: bisect that leg's foot radius until its own lowest geometry
touches the ground, so the legs absorb the difference in their lengths instead of the body doing it for all
of them.

**Attempt one** bisected downward, guarded by `if reach_with(far) > 0` - only firing when a leg hovers at
full stretch. It never fired, because at the solved radius the legs are BURIED, not hovering; the hovering
appears later, when the whole-body settle lifts the machine by its worst-buried leg. Wrong direction.

**Attempt two** bisected upward with a bracket of `reach_with(near) < 0 < reach_with(far)`. Also no change,
to the centimetre: Pincer +0.32, Censer +0.23, Pavise +0.00, exactly as cycle 19. So the bracket is not being
satisfied either, and I do not know why.

**The arithmetic says it should be.** Pincer's hip stands about 3.08 m up and its leg is 3.56 m, so a
near-vertical leg reaches 0.48 m below the ground (negative) and a near-horizontal one leaves the tip at hip
height (positive). A root is bracketed. It is not being found, so the fault is in `reach_with` itself rather
than in the bracket - the restore, the swing, or the measurement inside it.

**What not to do is another guess at the bracket.** Two cycles have now been spent changing a condition and
re-running the whole set to see whether the numbers move. Cycle 21 prints `reach_with` across a sweep of
radii for a single leg on a single machine and looks at the curve. If it is flat, the restore or the swing is
broken; if it is monotonic and crosses zero, the bisection is being skipped for a reason the print will show.
One leg, one machine, one graph - not six machines and a hope.

**Twenty cycles in, the balance sheet.** Working: a render harness that poses the shipped mesh at true size,
upright, textured, on a visible ground line with a 2 m figure and a true elevation; four checks that between
them catch underground poses, collapsed foot rings, machines coming apart, and hovering; a measured belly
clearance table; and four confident theories disproved before anyone acted on them. Not working: no knee
articulation, no per-leg settle, and still not one score describing the machines as the game draws them,
because the editor has been held for all twenty cycles.


---

## Cycle 21 notes - one leg, one sweep, and the answer in eleven lines

Instead of changing the bracket a third time, the function was printed across a sweep of radii for Pincer's
`Leg_L1` (span 3.37 m, hip 2.57 m up):

    flat 0.17 -> lowest -0.701      flat 1.68 -> lowest -0.779
    flat 0.47 -> lowest -0.769      flat 1.99 -> lowest -0.725
    flat 0.77 -> lowest -0.815      flat 2.29 -> lowest -0.660
    flat 1.08 -> lowest -0.831      flat 2.59 -> lowest -0.590
    flat 1.38 -> lowest -0.817      flat 2.90 -> lowest -0.517
                                    flat 3.20 -> lowest -0.444

**It is negative everywhere.** No bracket exists, so both of cycle 20's bisections were correctly doing
nothing, and no amount of adjusting the condition would ever have made them fire.

**The cause is a flaw in the idea, not a bug in the code.** `reach_with(f)` aims the leg's toe at a target on
the GROUND - z is always zero - so the toe lands at zero and whatever part of the leg hangs below the toe is
below zero. The function is structurally incapable of returning a positive number. I spent two cycles
bisecting a function whose sign never changes.

**The correct fix is one variable over.** Bisect the target's HEIGHT rather than its radius: aim the toe at
z = h and find the h where the leg's lowest geometry lands at zero. That function does change sign - a high
enough target lifts the whole leg clear - and it is monotonic in h. The radius then follows from the stance
rule as before, and each leg gets its own small height offset, which is exactly the difference the legs are
supposed to absorb.

**The lesson, which is the same one as cycles 2, 13, 15 and 17 in a new costume:** I measured the quantity
that was easy to compute rather than the one the question was about. "Where does the toe go" was easy;
"where does the lowest part of the leg go" was the question. The sweep cost one cycle and would have cost
nothing if run before the first attempt rather than after the second.


---

## Cycle 22 notes - the per-leg settle finally works

Bisecting the target's HEIGHT instead of its radius - the fix cycle 21's sweep pointed at - works on the
first attempt. Every leg on Pincer, Kettle, Censer, Pavise and Banner now rests on the ground:

    highest leg +0.00 m, lowest leg -0.00 m, on all five

That is the thing three cycles failed at. The legs no longer need the body to settle for them; each one finds
its own angle, which is what the game's gait does and what the machines' differing leg lengths require. The
Redoubt is the one hold-out at +0.42 m - its bracket still does not fire, and it is the only machine whose
legs differ enough in length that one of them cannot reach at any aim height.

**What is now inconsistent, and it is the check rather than the pose.** The toe error reads 0.04 to 0.71 m
and refuses every render, because the toe is now deliberately aimed ABOVE the ground so that the leg's lowest
geometry - not its toe - is what touches. Comparing the landed toe against the aimed height rather than
against zero brought it down from 1.1-2.0 m to 0.04-0.71 m, but a residual remains. Since the legs are
measured as planted to the centimetre by an independent check, the toe error is now measuring a disagreement
between two of my own conventions, not a fault in the stance.

**Cycle 23:** reconcile them. The toe check was written when the toe was the contact point; it no longer is.
Either retire it in favour of the per-leg contact check, which measures the thing that matters and has been
right every time, or make it compare like with like. Retiring is probably correct - four checks guard this
stance and one of them is now asking a question that no longer has meaning.

**Worth noting against the pattern of the last twenty cycles.** This fix worked first time, and the reason is
that cycle 21 measured the function before changing it rather than after. Every no-op and every revert in
cycles 12 to 20 came from editing a condition and re-running the whole set to see if the numbers moved. One
sweep of eleven values, on one leg of one machine, replaced three cycles of that.


---

## Cycle 23 notes - the checks pass and the picture still is not right

The toe check is retired. It was written when the toe was the contact point and has not been since cycle 22,
so it was measuring a disagreement between two of my own conventions. The gate is now: no unreachable leg, no
joint opening beyond 5 cm, and every leg within 5 cm of the ground in both directions. Four machines pass -
Pincer, Kettle, Censer, Pavise - each with every leg planted at 0.00 m. Banner is refused on its 0.11 m joint
and Redoubt on its 0.42 m hover.

**And the elevation says it is still not a standing machine.** Looking at the Pincer: the legs do touch the
ground line and the body is held clear, which is what the numbers claim and they are not lying. But the legs
reach out SIDEWAYS at near-ground level rather than down and under, so the machine reads as something
sprawled and dragging rather than something standing. A measurement of contact cannot see that; only the
picture can.

**One fault the picture shows that no check is watching: the gun barrel and two small parts are BELOW the
ground line.** Every check added so far looks at legs or at joints. Nothing guards a non-leg part sinking
into the floor, and the Pincer is currently rendered with a gun buried in the mud.

**Two things for the next cycles, in order of value:**
1. Refuse any part below ground, not just legs. Cheap, and the Pincer needs it now.
2. The sideways sprawl is the real quality problem and it is the `Lean` stance rule applied to legs that
   stick out sideways in the sculpt. The foot goes out along the hip's own direction, which for these
   machines is almost horizontal, so the leg sweeps sideways instead of folding under. Bringing the foot
   further under the body - a smaller radius with the height bisection absorbing the difference - is the
   change most likely to make these read as standing rather than sprawling.

**Where the board actually stands after 23 cycles.** No line has moved from its cycle 1 score, because every
score since has been of a proposed re-pose rather than of the shipped machine, and the editor has not been
free once. The re-pose itself has gone from "belly on the floor" to "every leg planted, body carried, still
sprawling". That is real progress on a real artefact, and it is not the same thing as the board improving.


---

## Cycle 24 notes - the tuck is cancelled out by the aim

The sprawl was to be fixed with a constant: plant the foot at 0.30 of the leg's length instead of the ~0.50
that `Lean` implies, steepening the leg and bringing it under the body.

**Two things went wrong, one procedural and one real.**

The first edit did not apply - the replacement text no longer matched, because the surrounding lines had been
rewritten in cycle 22 - and the run afterwards produced numbers identical to the previous cycle's. I nearly
recorded that as "the tuck had no effect". It had no effect because it was never in the file. The only reason
it was caught is that a `grep` for the new line printed nothing. **Any cycle that reports "no change" must
first prove the change is present**; three of this loop's cycles have now been spent on edits that silently
did not apply.

Applied properly, the numbers move (Pincer's toe measure 0.710 to 1.216) and the picture does not. The reason
is real and worth recording: **the two controls cancel.** Tucking the foot in steepens the leg, a steeper leg
reaches deeper, and the height bisection then raises its aim to bring the lowest geometry back to the ground
- which flattens the leg again. Radius and aim are not independent; they are two ways of setting the same
angle, and I have been adjusting one while the other silently undoes it.

**What that means for the sprawl.** It cannot be fixed by moving the foot, because the foot's position is
already fully determined by two constraints: the leg's length and the requirement that its lowest geometry
touch the ground. The only remaining freedom is the one this harness does not have - bending the knee, which
would let the leg drop steeply and still reach out. The sprawl and the un-articulated knee are the same
problem, and cycles 12 and 13 already failed at it twice.

**Honest recommendation for the owner, at cycle 24 of 48.** The Blender re-pose has gone as far as a rigid
swing can take it: legs plant, bodies are carried, and the remaining fault needs articulation. Two attempts
at articulation have been reverted. The higher-value path from here is not a third attempt in Blender - it is
Unity, which already solves this correctly every frame, and which has been unavailable for all 24 cycles.


---

## Cycle 25 notes - why the knee solve scatters, measured rather than guessed

Applying cycle 21's lesson - print the function before changing it - the knee solve was instrumented on one
leg of the machine that came apart both times.

    Kettle Thigh_LB:  thigh 0.84 m,  shin 3.29 m,  so reach is 2.45 m to 4.12 m
                      target at 1.01 m from the hip  ->  clamped up to 2.45
                      hip angle 176.8 deg

**The target is inside the leg's dead zone.** A two-bone chain with a short thigh and a long shin cannot fold
tighter than the difference of its bones, 2.45 m here, and the tucked foot is asked for at 1.01 m. The code
clamps the DISTANCE up to 2.45 but still aims along the original direction, so it folds the leg back on
itself - 176.8 degrees at the hip - and the limb ends up pointing behind its own socket. That is the
scattering, and it is one wrong line rather than a wrong approach.

Redoubt's numbers are healthy by comparison: thigh 2.22, shin 2.97, target 2.20 within a reach of 0.75 to
5.19, hip angle 84.6 degrees. So Redoubt's hover is a different fault and should not be chased with the same
fix.

**The fix, for a third attempt:** clamp the TARGET, not the distance. Push the foot outward along its own
direction until it is at least `|thigh - shin|` from the hip, then solve. The leg then reaches a foot it can
actually reach, at the cost of a slightly wider stance on machines with lopsided bones - and Kettle's bones
are very lopsided, 0.84 against 3.29.

**A fault in my own diagnostic, worth recording because it nearly poisoned the result.** Running `articulate`
in "diagnostic mode" and restoring `lr.matrix_world` afterwards did NOT undo it: articulate swings the SHIN
as well as the thigh, and restoring only the leg root leaves the shin where the solve put it. The pose
numbers moved this cycle as a result (Kettle's lowest leg -0.31, Redoubt's highest +1.08) and those readings
should be ignored. A restore has to cover everything the function touched, which for this one is the whole
chain.


---

## Cycle 26 notes - the knee solve stops scattering

The fix cycle 25 measured: clamp the TARGET to the leg's reachable annulus rather than clamping only the
distance while still aiming the second swing at the original, unreachable point. One expression.

**It works.** Kettle - which came apart completely in cycles 12 and 13, and whose failure caused two reverts
- now poses with its joints opening **0.00 m**. So does the Redoubt. The knee solve is no longer destructive,
and the difference between this attempt and the two before it is entirely that this one was preceded by a
measurement rather than by a hypothesis.

**The gap it leaves is clean.** Jointed legs now hover: Kettle 0.17 to 0.34 m, Redoubt 0.57 to 0.97 m. The
cause is structural and obvious once seen - the per-leg height bisection from cycle 22 calls `swing`, the
rigid sweep, to test each candidate aim. An articulated leg is then posed by a different function than the
one the bisection measured, so the settle it computed does not apply to the pose that is finally used.
Pincer, whose legs are single parts and so are never articulated, still plants at 0.00.

**Cycle 27:** make the bisection test the same solve it will apply - call `articulate` inside
`lowest_aiming_at` for legs that have a knee, and `swing` for those that do not. That is the last known
disagreement between the two halves of this solver.

**Three attempts at the same feature, and what separated them.** Cycle 12 guessed at the mechanism and
scattered the Redoubt. Cycle 13 fixed the diagnosed fault and still scattered the Kettle. Cycle 26 printed
the intermediate values first, found the target sitting 1.01 m from a hip whose leg cannot fold tighter than
2.45 m, and fixed it in one line. The measurement cost one cycle; the guessing cost two reverts and two
cycles of no progress.


---

## Cycle 27 notes - half the disagreement closed

The bisection now calls the knee solve for legs that have a knee and the rigid sweep for those that do not,
so it measures the pose that is actually applied. Cycle 26 had it testing one and applying the other, which
is why every articulated leg hovered.

**State of the six after the change:**

| machine | legs off the ground | joints | verdict |
|---------|--------------------|--------|---------|
| Pincer  | -0.00 .. +0.00 | +0.00 | passes |
| Censer  | -0.00 .. +0.00 | +0.00 | passes |
| Pavise  | -0.00 .. +0.00 | +0.00 | passes |
| Redoubt | +0.00 .. +0.50 | +0.00 | hovering, improved from 0.97 |
| Banner  | -0.04 .. +0.01 | +0.11 | planted, but a joint opens |
| Kettle  | -1.25 .. +0.00 | +0.00 | one leg buried, REGRESSED from 0.17-0.34 hovering |

Three of six now stand with every leg on the ground and nothing coming apart - the best state this re-pose
has reached. Banner is within 4 cm of planted and fails only on its joint, which cycle 14 traced to a pivot
that is not where the thigh attaches.

**The Kettle regression is honest and should not be papered over.** Aligning the bisection made it worse, not
better: it now finds an aim at which the highest leg just touches while another sits 1.25 m under the floor.
That is the same lopsided-bone problem cycle 25 measured - a 0.84 m thigh against a 3.29 m shin - and the
per-leg bisection cannot fix it, because for that leg there may be no aim height at which the lowest geometry
of ALL its parts rests at zero. A leg whose shin is four times its thigh sweeps a very different path from
the others.

**Cycle 28:** do not chase the Kettle with another bisection change. Check first whether an aim exists for its
worst leg at all - sweep the function as cycle 21 did, on that leg alone, and look at whether it crosses zero.
If it does not, the Kettle needs a different treatment from the other five and should be set aside rather
than dragged along with them.


---

## Cycle 28 notes - the aim function turns over, and the bracket steps over the answer

The question was whether Kettle's worst leg has any aim height at which it stands. Swept, rather than
assumed:

    Thigh_LB (span 3.31):  h0.0 -0.57   h0.4 -0.57   h0.8 -0.52   h1.2 -0.34   h1.7 +0.14
                           h2.1 +0.93   h2.5 +0.52   h2.9 +0.13   h3.3 -0.10
    Thigh_LF (span 2.64):  h0.0 -1.25   ...          h1.6 +0.03   h2.0 +0.65   h2.3 +0.28   h2.6 -0.50

**The answer exists and the bisection cannot find it.** The function is not monotonic: it rises, peaks
around two thirds of the leg's length, then turns over and goes negative again. So the bracket `[0, span]`
has a NEGATIVE value at both ends, the guard `f(lo) < 0 < f(hi)` is false, no bisection runs at all, and the
leg keeps aim 0 - which for Thigh_LF is -1.25 m, exactly the burying cycle 27 reported.

Cycle 22 got away with this because on the machines it was tested against, `f(span)` happened to be positive.
It was never monotonic; it just looked it from two samples.

**Why it turns over:** as the aim rises the leg swings up, and past a certain angle a different part of the
chain becomes the lowest one - the knee, or the far side of the shin - so lifting the target starts lowering
the measured point instead of raising it. The curve has a peak where the lowest part changes identity.

**Cycle 29's fix, and it is small:** scan the sweep for the FIRST sign change and bisect inside that
sub-interval, instead of assuming one crossing across the whole range. Nine samples are enough to find it -
the sweep above already does - and it costs nine extra solves per leg, which is nothing at this scale.

**Third time this pattern has paid.** Cycle 21 swept a function and found it never changed sign. Cycle 25
printed intermediates and found a target inside a leg's dead zone. Cycle 28 swept and found a function that
turns over. Each took one cycle and each replaced a hypothesis that would have been wrong. The three cycles
that changed code without measuring first - 12, 13, 20 - produced two reverts and a no-op.


---

## Cycle 29 notes - Kettle's numbers pass and Kettle is in pieces

The scan-then-bisect works as designed: finding the FIRST sign change instead of assuming one crossing
brought Kettle to -0.00..+0.00 with joints at 0.00, and four machines now clear the gate on numbers - Pincer,
Kettle, Censer, Pavise.

**And the elevation shows the Kettle is a parts bin.** Limbs floating clear of the hull, one large limb lying
across the ground line, small blocks hanging in mid-air. Every number says it is standing.

**Why the gate cannot see it, and this is my error rather than the gate's.** The joint check compares each
gap BEFORE and AFTER posing and fires when one grows. Kettle's parts are already up to 3.87 m apart in the
rest pose - cycle 2 measured exactly that, Body to Thigh_LB 3.87 m, 8 of 16 children clear of their parent.
Posing does not make it worse, so nothing fires. The check was built in cycle 13 to catch the knee solve
tearing a machine apart, and it does that; it was never able to notice a machine that arrived apart.

**The critique agents were right about the Kettle and I discounted them three times.** Cycle 1 called it "not
a machine in a bad stance, a parts bin photographed from three angles". Cycle 10 and cycle 16 said the same.
Each time I answered that measurement showed posing was not causing the separation - which was true, and
beside the point. Posing is not causing it. The asset is already like that, and rotating a leg about a hip
that sits metres from the body cannot bring it back.

That also rehabilitates cycle 15's pivot measurement, which I dismissed as too crude. Kettle's worst pivot
was 4.66 m outside its parent - the largest of the six - and for this machine that reading was telling the
truth even if the metric is wrong for domed carapaces.

**What Kettle actually needs:** its parts TRANSLATED back onto their sockets, not rotated. That is a
different operation from anything this harness does, it is properly a fix in `Tools/crabsplit.py` where the
parts are cut and their pivots assigned, and it is the owner's call because it means re-exporting the asset.

**Standing recommendation, unchanged from cycle 24 and now stronger:** four machines pose acceptably and the
two that do not - Kettle and Banner - both fail for reasons in the source asset rather than in the stance
solver. The remaining value in Blender is small. Unity, which has not been free for 29 cycles, is where the
board's actual questions can be answered.


---

## Cycle 30 notes - the critic measured what five of my checks could not

Five numeric proxies for "does this look assembled" have now failed, so this cycle asked the question of the
thing that can answer it. The critic measured distances off the ground band in the orthographic elevations,
where there is no perspective and the band's top edge is exactly height zero.

**Verdict: not one of the five is assembled.** Ranked best to worst as "one object": Censer 4, Pavise 3,
Redoubt 2, Pincer 1, Kettle 0. Feet-on-ground scored **zero for every machine**.

**That contradicts my own numbers, which report legs at -0.00 to +0.00, and the contradiction resolves
against me.** My per-leg check collects parts named `Leg_*` or `Thigh_*` and their descendants. Anything not
in one of those chains is invisible to it. So:
- Kettle's largest part - a big tube lying through the ground plane, 1.9 m under - is not in a leg chain, and
  nothing noticed.
- Pincer has three parts buried to 1.48 m while its hull floats 0.7 m up. Being pulled apart vertically in
  both directions at once, and every check passes.
- Redoubt's only ground contact is its cauldron, which is attached to nothing.

**Two measurements that are worth more than the scores**, because they diagnose rather than describe:
- Redoubt's two feet sit 0.76 m and 1.18 m up and differ from each other by 0.42 m **on flat ground**. A
  per-leg solver that terminates on the ground plane cannot produce two different heights. Mine reports both
  at 0.00, so it is not measuring the feet the picture shows.
- Censer's three foot tips differ by 0.2 m while the whole body floats 0.42-0.79 m. A uniform offset would
  mean a single root-height error; the spread means the per-leg solve is also not landing.

**The fix the critic names is the right one and it is two lines:** check the minimum world-space vertex Y of
EVERY part, not just of leg chains - feet must be at zero within a few centimetres, and nothing else may be
below zero. That is cycle 31.

**The honest reckoning at cycle 30 of 48.** I have built five checks, four of which passed machines that were
visibly broken, and each time I learned the lesson narrowly - "measure the specific thing, not an aggregate"
- and then built the next check with the same flaw in a new place. The critic has been right about the
assembly problem since cycle 1 and I discounted it three times on the strength of measurements that were not
measuring what I thought. The Blender re-pose is not close to shippable and the asset faults under it are
real.


---

## Cycle 31 notes - the sixth check finally names names

Every part is now measured against the ground, not just the ones named like legs. It found the faults on the
first run and identified them by name:

| machine | deepest part | how far under | parts touching |
|---------|--------------|---------------|----------------|
| Censer  | -            | none          | 4 - PASSES |
| Pavise  | Gun          | 0.94 m        | 4 |
| Pincer  | Turret_L     | 1.60 m        | 6 |
| Kettle  | Mortar       | 2.02 m        | 4 |
| Banner  | Banner       | 3.61 m        | 3 |
| Redoubt | -            | none          | 3, but legs hover 0.50 m |

**Every buried part is carried equipment - a turret, a mortar, a gun, a standard. Not one is a leg.** That is
a pattern, and it matches the rest-pose separation measured in cycle 2: Pincer's Body-to-Turret_L gap was
3.01 m before anything was posed. The legs are being solved and the things bolted to the hull are being left
wherever the sculpt put them, which for these machines is well below the body.

**An unresolved contradiction, recorded rather than guessed at.** If a Pincer's turrets really sit 1.6 m below
its hull, Unity would draw them there too - `WalkerGait` poses legs and nothing else - yet the Unity capture
taken earlier in this session shows a Pincer with its twin turrets correctly on top. Both cannot be right.
Either this harness is still mishandling the axis for non-leg parts (it has had the up-axis wrong once
already, in cycle 2), or the Unity model differs from the FBX in a way not yet found. **This must not be
acted on until it is settled**, and settling it needs the editor: load the Pincer in Unity, print
`Turret_L`'s world position relative to `Body`, and compare with the same number from Blender. One eval.

**Six checks now, and the first one that names a part.** The five before it reported aggregate numbers that
were satisfied by the wrong thing every time. This one says "Turret_L is 1.60 m through the ground", which is
a sentence someone can act on. The lesson that took thirty cycles: a check should name the part and the
distance, because a number without a name cannot be wrong out loud.


---

## Cycle 32 notes - the harness has been upside down for thirty cycles

The contradiction from cycle 31 - my render buries Pincer's turrets while Unity draws them on top - is
settled, from numbers already on this board rather than from a new experiment.

Unity reports Pincer's front hip at **y = +1.224**, above the body. The raw FBX has the same hip at a local
**-1.225**. The magnitudes agree to a thousandth, so **Unity's up is the file's -Y**. Applying that to the
turret: its raw local height is -2.512, therefore **+2.512 in Unity** - the turrets sit two and a half metres
ABOVE the body, exactly as the Unity capture shows and exactly opposite to what this harness renders.

Rotating a point about X by +90 degrees sends -Y to -Z, which is down. The harness uses +90. **It has been
rendering these machines upside down since cycle 2**, where the sign was chosen by eye from a three-quarter
perspective view of a radially symmetric crab - the least reliable view available, and chosen before the
orthographic elevation that could have settled it existed at all.

**Corrected to -90, and the correction is KEPT even though it renders worse.** With the right axis the
turrets come up and the BODY goes 2.73 m under, because `stand()` now computes a lift of -0.03 instead of
+5.04. That is not the rotation causing a fault; it is the rotation exposing one that was hidden. The stance
solve was tuned over twenty cycles against an inverted machine, so every constant in it - which leg is
shortest, which hip is highest, where the ring of reachable ground lies - was derived upside down.

**What this means for the last thirty cycles, stated plainly.** The stance work is not wrong in its methods -
the per-leg aim, the scan for the first sign change, the knee clamp, the six checks - but it was all fitted
to a machine standing on its head. The findings that survive are the ones that did not depend on which way
was up: the parts are separated in the rest pose, Kettle is disassembled in the source, the belly clearance
table, the gun-yaw measurement, and the four disproved theories. The findings that do not survive are every
statement about what stands, hovers or is buried.

**Cycle 33:** re-derive the stance under the corrected axis. Do not tune anything until a Pincer renders with
its turrets up, its legs down and its body clear - and check that first in the elevation, not in perspective.


---

## Cycle 33 notes - neither sign is right, and the derivation was built on sand

Cycle 32 concluded from Unity's rig numbers that the harness was upside down and corrected +90 to -90. This
cycle rendered the asset with the solver switched off, to see the raw machine the "right" way up before
tuning anything on top of it - which is the correct order and caught the error.

**The -90 render is also wrong.** The Pincer lies on its side: legs stretched horizontally to the left, gun
barrels adrift at the top of frame, body down at the right. It is not upright, and neither is +90, which
merely looked plausible from a three-quarter angle.

**Where cycle 32's reasoning failed.** It assumed the raw Blender import preserves the file's Y-up layout, so
that a single rotation about X would stand the machine up. Blender's FBX importer applies its own axis
conversion on the way in, so that assumption is unfounded - and the numbers bear it out. Under it, Blender's
Y should match Unity's Y negated (it does: -1.225 against +1.224) AND Blender's Z should match Unity's Z (it
does not: -0.518 against -0.281). A mapping that holds on one axis and fails on another is not the mapping.

So cycle 32's confident correction was as wrong as cycle 2's confident original, and for the same reason:
both derived an orientation instead of looking at one. The difference is that this time it was caught in the
next cycle rather than in the thirtieth, because the elevation now exists and the solver could be switched
off to get a clean baseline.

**Cycle 34, and this is a small bounded experiment rather than another derivation:** render the unposed
Pincer at 0, +90, -90 and 180 degrees about X, four elevations, and pick the one where the turrets are on
top, the legs are underneath and the claws point forward. The elevation is a reliable view now - level
camera, visible ground line, scale figure - which is exactly what cycle 2 did not have when it guessed.

**Do not tune the stance again until that image exists.** Every constant in the solver has been fitted to an
orientation that is now known to be wrong, and fitting more of them to a second wrong one costs cycles for
nothing.


---

## Cycle 34 notes - the orientation, settled by looking at four pictures

Four elevations of the unposed Pincer, at 0, +90, -90 and 180 degrees about X. **Zero is correct.** The
carapace sits on top with its turret above it, the legs hang down beneath, the claws point forward. It is
unmistakably a machine standing the right way up, and it took one render to see what two derivations got
wrong in opposite directions.

**Blender's FBX importer already converts the file's axes.** No rotation is needed or wanted. Cycle 2 added
+90 by eye and cycle 32 "corrected" it to -90 by algebra; both were wrong, and the machines have been on
their side for thirty-two cycles.

**What that costs and what it does not.** Every statement this board has made about standing, hovering,
burying or sprawling was measured on a machine lying on its side, and none of it means anything - including
the belly clearance table from cycle 8, which used hip heights along the wrong axis. What survives is only
what never depended on which way was up: that the parts are separated in the rest pose, that Kettle is
disassembled at source, that every gun part has zero modelled yaw, and the four disproved theories.

**What it buys.** The harness is now pointed at the right machine for the first time. The stance solver, the
six checks, the per-leg aim, the knee clamp and the elevation are all still there and all still sound in
method; they have simply never been run on an upright machine.

**Cycle 35:** re-run the whole set, posed, at ROTX 0, and see what the numbers say about a machine that is
finally the right way up. Expect them to be different, and expect some of the six checks to start failing
honestly for the first time.

**The lesson, which this loop has now paid for three times over.** Cycle 2 guessed an orientation from a
three-quarter perspective view. Cycle 32 derived one from numbers whose frame it had not verified. Cycle 34
rendered four candidates and looked. The looking cost less than either guess and is the only one that
produced an answer. When a question is "which way is up", the answer is a picture, not an argument.


---

## Cycle 35 notes - the first honest failure

The solver has now run on upright machines for the first time, and every one is refused. That is the expected
and correct outcome: the checks are finally measuring a machine that is the right way up, and they are
failing for reasons that are real rather than for reasons produced by a sideways model.

| machine | deepest part | legs, lowest..highest | joints |
|---------|--------------|------------------------|--------|
| Censer  | Drum -1.50     | -0.00 .. +0.00 | +0.00 |
| Pavise  | Gun -1.75      | -0.53 .. -0.00 | +0.00 |
| Pincer  | Reactor -1.79  | -1.66 .. +0.32 | +0.02 |
| Banner  | Gun -3.09      | -1.51 .. +0.23 | +0.00 |
| Kettle  | Shin_RB -4.39  | -4.39 .. +0.00 | +0.00 |
| Redoubt | -              | -              | +0.39 apart |

**The pattern that held sideways still holds upright, which makes it likely to be true:** the buried part is
carried equipment on five of six - Drum, Gun, Reactor, Gun - and the solver only poses legs. Censer plants
every leg perfectly at -0.00..0.00 and still buries its drum by a metre and a half. Whatever is wrong with
these machines is not, for the most part, in the legs.

**One number to check before touching the solver.** Pincer's computed lift is 2.27 m. From Unity's own rig
data its front hip sits 3.06 m above the model root, and the stance rule wants that hip at span x 0.97 x
0.893 = 2.92 m, which implies a lift of -0.14 m, not +2.27. The solve and the rig data disagree by two and a
half metres, so the hip heights the solver is reading in the upright frame are not the ones Unity reports.
**That is the next thing to measure, and nothing in `stand()` should be tuned until it is settled** - the last
two times a constant was adjusted before the frame was verified, the result was thirty cycles of work on a
machine lying on its side.

**Thirteen cycles remain before the 09:23 stop.** The realistic outcome for them is a correct diagnosis of
what these assets need, not a finished re-pose. The board's own scores have not moved since cycle 1 and will
not move tonight, because they describe the shipped machine and the editor has been held for all 35 cycles.


---

## Cycle 36 notes - the numbers say -Y is up, and they disagree with the picture

Hip heights in the ROTX 0 frame that cycle 34 chose by looking:

    Leg_L1 -1.295   Leg_L2 -0.514   Leg_L3 +0.362   (above the body)

Unity reports the same three hips at **+3.060, +2.898, +3.255**. Not a match on magnitude, on sign or on
ordering, so ROTX 0 is not the frame Unity is working in.

**The turret settles which axis is up, without rendering anything.** In the raw file the vector from Body to
Turret_L is (-0.635, **-2.512**, -0.825). Unity reports the turret **+2.512** above the body. One axis
matches to three decimals and the other two do not come close. **Up is -Y.** The hips agree: Blender's Y
divided by the 2.5 scale gives -0.988, the raw dump's value, and Unity's hip of +1.224 is that number's
local form negated.

**So cycle 34 is wrong as well, and now the two kinds of evidence openly contradict each other.** The numbers
say up is -Y, which means a -90 degree rotation about X - exactly what cycle 33 rendered and I judged to be a
machine lying on its side. One of those readings is wrong and I do not yet know which.

**What I am NOT going to do is pick one.** Three orientations have now been chosen and each was confidently
justified: +90 by eye in cycle 2, -90 by algebra in cycle 32, 0 by eye in cycle 34. Two have been overturned
and the third is now contradicted. Choosing a fourth on the same quality of evidence would be the same
mistake a fourth time.

**What would actually settle it,** and it needs the editor rather than more Blender: load a Pincer in Unity,
print the world positions of `Body`, `Turret_L` and `Leg_L1`, and render one frame. Unity is the authority
here - it is the thing the board is ultimately about - and thirty-six cycles of inference from an exported
file have produced three contradictory answers to a question Unity can answer in one eval.

**The visual reading has one specific weakness worth naming:** a crab seen from directly above and a crab
seen from the side both show a carapace with limbs radiating around it. Cycle 34 judged "legs hang below the
body" from an image that may equally have been a top-down view. That is exactly the ambiguity that made cycle
2's guess wrong, and I did not account for it.


---

## Cycle 37 notes - orientation settled, by testing against a fact instead of looking

All four candidate rotations, each measured against something Unity states about this machine - that Turret_L
sits 2.512 sculpt units above Body, so +6.28 m at the shipped size:

    ROT    0     turret  -2.06 m     no
    ROT  +90     turret  -6.28 m     no
    ROT  -90     turret  +6.28 m     OK   <- the frame
    ROT +180     turret  +2.06 m     no

**-90 degrees about X.** It reproduces Unity's number exactly and the other three are wrong by 2 to 12 metres.
Cycle 32's algebra was right; my visual rejection of it in cycle 33 was wrong, and cycle 34's "no rotation"
was wrong. The default is set.

**One honest note on the test itself.** It used two criteria and only one of them worked. "Every toe below
its hip" passed in all four frames, because the toe is measured as the chain's lowest point, which is at or
below the hip whichever way the machine is turned. It discriminated nothing. The turret comparison did all
the work, and it worked because it checks a SIGNED DISTANCE against a number from an independent source
rather than a relationship that is true by construction. A test that cannot fail tells you nothing - the
fourth time this loop has learned that.

**Six cycles on one question, and what would have avoided them.** The orientation was first chosen in cycle 2
by eye. Had it been checked then against any single number Unity reports - the turret, a hip height, the hull
top - it would have been settled in one cycle and thirty cycles of stance work would have been done on an
upright machine. Every measurement between cycles 2 and 32 was taken in a frame nobody had verified.

**Cycle 38:** re-run the posed set in the correct frame. This is the third time the whole set has been
re-run after a frame change, and it should be the last.


---

## Cycle 38 notes - one fault left, and it is the one that needs Unity

The posed set, in the frame verified against Unity's own turret position:

| machine | body buried | legs, lowest..highest | lift |
|---------|-------------|------------------------|------|
| Banner  | 1.00 m | -0.22 .. +0.00 | -0.25 |
| Censer  | 2.25 m | -0.00 .. +0.00 | -0.12 |
| Pincer  | 2.73 m | -0.00 .. +0.30 | -0.03 |
| Kettle  | 3.35 m | -1.68 .. +0.16 | -1.10 |
| Pavise  | 4.63 m | -0.00 .. +0.00 | -0.19 |

**The legs are fine and the body is underground.** Censer and Pavise plant every leg at -0.00..0.00 - the
per-leg solve, the knee clamp and the scan-for-first-crossing all working - and still put their hulls two and
four metres under. The fault is no longer in the legs; it is in the ride height.

**The ride height is the one number that can be checked against Unity, and it disagrees badly.** This solver
computes a lift of about zero for every machine. Unity's `WalkerGait.Stand` returned 2.236 for a Redoubt in
the gait tests recorded earlier on this board - not zero. Same formula, same rig, two very different answers,
which means one of the inputs differs: either the hip heights, the span, or `MaxSpan` is being read
differently here than in `TankModel`.

**I am not going to guess which.** Three orientations were guessed and two were wrong; the frame was only
settled by checking against a number Unity states. The same discipline applies here: the fix is to print
`WalkerGait.Stand`'s inputs and output for one machine in Unity and compare them line for line with this
harness's. That is one eval and it needs the editor, which has now been held for all 38 cycles.

**What the owner should know, with nine cycles left.** The Blender harness is now correct in its frame, its
size, its ground plane and its six checks, and its leg solve plants feet on four of six machines. It has one
identified remaining fault - the ride height - and that fault cannot be diagnosed without comparing against
Unity. The honest recommendation is unchanged since cycle 24 and is now specific: free the editor for five
minutes and this is a single eval, or leave it and the loop will spend its remaining cycles measuring things
it has already measured.


---

## Cycle 39 notes - the frame is still not right, and the turret test did not prove it was

Cycle 37 settled the orientation on one criterion: Turret_L must sit +6.28 m above Body, which -90 degrees
reproduces exactly. That criterion is necessary. It is not sufficient, and this cycle shows why.

Measured in the -90 frame, Pincer's Body mesh spans **-2.70 to +4.20** along the candidate up axis - an
extent of 6.90 m, with the machine's lowest point at -2.70. Unity reports the same mesh with a Y-extent of
**4.84 m** and its top at **6.52 m** above the root. Neither number matches, and +90 does no better (-4.20 to
+2.70).

**What that means.** A rotation about X can put the turret above the body while still measuring the body's
height along the wrong axis, because turret-above-body only constrains the ordering of two points, not which
of the three axes is vertical. The extent is the stronger test: 6.90 against 4.84 says the axis being treated
as up is one along which the body is half as long again as it should be.

**This also explains the buried bodies of cycle 38 without any new theory.** If the body's extent is being
measured along the wrong axis, then so is the ride height that has to clear it, and every machine sinking its
hull by 1 to 4.6 m is the same error seen from the other end.

**The next test is specific and cheap:** find which Blender axis, after scaling, gives the Body an extent of
4.84 m and a maximum of 6.52 m. That names the up axis directly from a number Unity already reports, rather
than testing candidate rotations one at a time. It is one script and it does not need the editor.

**Four cycles remain before the 09:23 stop.** The realistic deliverable is a correctly framed harness and a
written account of what these assets need - not a finished re-pose. Three orientations have been tried and
each was justified by a test that turned out to be necessary but not sufficient; the discipline that has
actually worked here is to check a measured quantity against a number from an independent source, and the
Body extent is the next such number.


---

## Cycle 40 notes - the up axis, named at last, and verified twice over

Instead of testing rotations one at a time, this cycle asked which Blender axis reproduces a quantity Unity
already reports. Pincer's Body mesh, scaled, measured on all three axes with no rotation applied:

    x: body -3.90 .. +3.90   extent 7.81
    y: body -4.20 .. +2.70   extent 6.91
    z: body +1.67 .. +6.52   extent 4.84   <- Unity says extent 4.84, hull top 6.52

**Blender's +Z is Unity's up, with no rotation at all.** Two independent numbers - the extent and the top -
match to the centimetre. Cycle 34's "no rotation", chosen by eye, was right; cycle 37's -90, chosen on the
turret, was wrong. The default is set back to 0 and the frame is now verified on two quantities rather than
one.

**Why the turret test misled.** It checked a single signed distance whose expected sign I had derived myself
from the raw file, so an error in that derivation could not be caught by the test that used it. And the
physical intuition behind it was wrong too: the Pincer's gun housings sit on the FRONT FACE of the carapace,
below its centre - visible in the Unity capture taken earlier in this session - so "turret below the body's
middle" was never the absurdity I treated it as.

**The general lesson, and it is the one worth keeping from these forty cycles.** A check built on a number
you derived cannot catch an error in your derivation. A check built on two independent numbers from an
outside source can, because being wrong in a way that satisfies both at once is far harder. Every frame
decision made here on one criterion was overturned; the one made on two has held.

**Three cycles remain.** The harness is now, for the first time, verified in frame, size, ground plane,
checks and rendering. What it has never had is a correct ride height, and cycle 38's diagnosis of that stands
- but it was measured in the wrong frame, so it needs re-taking in this one before it can be trusted.


---

## Cycle 41 notes - the ride-height diagnosis is withdrawn; it was a frame artefact

Re-measured in the frame verified against two of Unity's own numbers:

| machine | deepest part | legs, lowest..highest | lift |
|---------|--------------|------------------------|------|
| Censer  | Drum -1.50    | -0.00 .. +0.00 | 2.19 |
| Pavise  | Gun -1.75     | -0.53 .. -0.00 | 2.37 |
| Pincer  | Reactor -1.79 | -1.66 .. +0.32 | 2.27 |

**Cycle 38 said the ride height was the one remaining fault. That is withdrawn.** It was measured in the -90
frame, which cycle 40 disproved, and in the correct frame the lift is a healthy 2.2 to 2.4 m and no body is
buried at all. The fault it described does not exist.

**What is actually left is narrower and more interesting.** The buried part on every machine is carried
equipment - a drum, a gun, a reactor - and on the Censer every leg plants at -0.00..+0.00 while its drum
still sits a metre and a half under. The legs are solved correctly and the things bolted to the hull hang
below the feet.

**That is a statement about the asset, not the solver**, and it is consistent with the rest-pose separation
measured in cycle 2 and never contradicted since: these machines are cut from a single sculpt into parts
whose pivots do not sit where the parts meet, so equipment that should ride on the hull is authored hanging
below it. `WalkerGait` poses legs and nothing else, so Unity cannot correct it either - which predicts that
the same drum, gun and reactor sit below the feet in the shipped game. **That prediction is checkable in one
eval and has not been checked**, because the editor has been held for all 41 cycles.

**Seven cycles remain before the 09:23 stop** (miscounted as two when this was written). The next one writes the handover: what is established, what is
withdrawn, what the owner should do. This board has recorded three orientations, five failed checks, four
disproved theories and two withdrawn diagnoses, and the useful residue is smaller than the cycle count
suggests but it is real and it is written down.


---

## Cycle 42 notes - the equipment is authored below the feet, in the file

No posing, no solver, the asset exactly as it ships, measured in the frame verified against two of Unity's
numbers. Distance each non-leg part hangs below the machine's own lowest leg point:

| machine | parts below the toes |
|---------|----------------------|
| Pavise  | Gun **2.32 m** |
| Censer  | Drum **2.14 m** |
| Banner  | Gun 1.49 m, Banner 0.18 m |
| Pincer  | Reactor 1.21 m, Turret_R 0.33 m, Turret_L 0.33 m |
| Kettle  | Mortar 0.54 m |
| Redoubt | none |

**Five of six machines carry equipment lower than their own feet.** That is in the file, before anything
poses anything, and it is the cleanest statement this loop has produced about what is actually wrong with
these assets.

**It predicts a fault in the shipped game.** `WalkerGait` solves legs and nothing else - no code moves a
drum, a gun or a reactor - so once the feet are planted on the ground, those parts are below it. A Pavise
standing on level ground should have two metres of gun underground. **This has not been verified in Unity**,
because the editor has been held for all 42 cycles, and it is one eval to check: spawn a Pavise, print the
world Y of the `Gun` part against the terrain height under it.

**Why this survives where earlier findings did not.** It needs no ride height, no stance rule, no knee solve
and no opinion about what looks right - only the asset, the frame, and a comparison between two parts of the
same machine. The frame is the one part that could still invalidate it, and that is now checked against two
independent Unity numbers rather than one.

**Redoubt is the control.** It alone authors nothing below its feet, and it is also the machine that has
consistently scored best on silhouette. That is what these assets look like when they are built correctly,
which means the other five are fixable rather than fundamentally misconceived.

---

## Cycle 45 notes - the fix that was identified and deliberately not applied

**The 10.7% is not a bug, it is a trade made in one line.** `Stand()` puts each foot in the MIDDLE of the ring of
ground the leg could reach at standing height, and the code says why: a foot at the OUTER edge leaves the leg
already at full stretch before the machine has moved, with no room to trail behind. Mid-ring buys stride room and
pays for it with a leg that is 10.7% short of straight. For a jointed leg that payment is invisible - the knee
takes it. For a one-piece leg there is no knee, so it is paid in scale, and Pincer and Censer are the two
machines that pay it on every single leg.

**The one-constant fix, with its predicted numbers.** `LEAN` 0.45 -> 0.30 raises the upright factor from 0.8930
to 0.9539, which moves the draw length from 89.3% to **93.7%** and cuts the shortening on Pincer from 0.36 m to
0.21 m. It also raises the ride height by about 0.06 x span - roughly +0.20 m on Pincer - which is the only
lever found so far that moves **W2** and **W6** in the same direction with one number.

**Why it was not applied this cycle.** It changes the standing stance of all six machines at once, and the two
assertions most likely to go red - a planted foot staying on its ground, and a leg reaching the foot the gait
chose - are exactly the ones this change perturbs. The editor has been held by a batch run all cycle, so it
could not be gated, and the rule for an ungated cycle is to apply only what a Blender render alone justifies.
No render was produced: the harness refused both machines. Applying a six-machine stance change on the strength
of arithmetic alone, with no test run and no picture, is how cycles 12, 13 and 20 were spent.

**It is one cycle's work the moment the editor frees**: change one constant, run `TW.Tests.GaitTests`, keep it if
green and revert it if red. The predicted numbers above are what a re-run of cycle 45's measurement should show,
so the change can be checked rather than believed.

---

## Cycle 46 notes - three lines, one cause

**`rig.Toe` is not the vertex that touches the ground, and that single fact now explains three separate scores.**

- **W1** (this cycle): the gait pins the toe POINT. A rigid leg that holds one point can only rotate about it, so
  every other point on the leg must travel. Because the contact vertex sits well away from the pinned toe, holding
  the toe still lets the visible contact skate 0.77-1.75 m across a 0.40 m stance.
- **T5** (cycle 43, in Unity): Pincer's lowest leg vertex floats 0.64 m above the ground while its toe is planted,
  and Kettle's drives 1.05 m under it. Same gap between the pinned point and the geometry, measured statically.
- **W6** (cycle 45): the stance asks every leg for 89.3% of its length, and where that is paid in scale rather than
  in a knee, it moves the contact geometry again.

**Why the toe drifted away from the contact point, in the project's own history.** Cycle 9 redefined the toe as
the FURTHEST POINT FROM THE HIP, which was right for the problem it solved - spans then agreed with Unity within
5% where before they had been wrong by however far the leg was modelled bent. But furthest-from-hip and
lowest-when-standing are different vertices on a leg that stands at an angle, and the stance leans every leg out
by LEAN 0.45. Cycle 22 then papered over the gap by aiming the toe deliberately ABOVE the ground so the leg's
lowest geometry would rest on it, and cycle 23 retired the toe check because it had become "a disagreement
between two of my own conventions". That was the moment this became invisible rather than fixed.

**The fix, identified and NOT applied.** Define a leg's contact as the lowest vertex of its last chain part when
the leg stands at LEAN, and pin THAT instead of the furthest-from-hip toe. It is rig data, not structure, which is
the preferred kind of change - but it moves every foot on every machine and it is the direct subject of two gait
assertions, so it must be gated, and the editor has been held by a batch run for three cycles. Parked beside
cycle 45's `LEAN` change; both want the same free editor, and W6's is the smaller of the two.

**A measurement correction worth keeping.** The first pass reported 1.0-2.2 m of travel by following "the lowest
vertex" sample by sample. That conflates a foot sliding with the job of being lowest passing from the toe to the
shin - a real thing a viewer sees, but not this line. Pinning one vertex and following it cut Censer's number from
2.15 to 1.75 m. The fault is still large and still real; it is 19% smaller than the first number, and the first
number was the kind this loop has published and had to withdraw twice.

---

## Cycle 47 notes - what was actually wrong, and the one thing that fixed it

**Both parked changes were tried. One was wrong.**

`LEAN` 0.45 -> 0.30 improved belly clearance on all six machines and went **red**: `GaitTests` watches ride height
across a level walk and allows 0.34 m of sink, and the flatter stance pushed Banner to 0.459 m - it stands at
0.045 and sinks to -0.414 while walking ground that is perfectly level. Reverted, as the rule requires. It is
worth knowing that the change looked good on every number cycle 45 predicted and still failed on a number cycle
45 never thought to predict.

**The toe definition was the real fault, and it was in two places.**

1. `ToeOf` returned `(bounds.center.x, bounds.min.y, bounds.center.z)` - the **bottom-face centre of the bounding
   box**, which is a synthetic point that need not lie on the mesh. On a limb modelled diagonally the lowest
   vertices sit at the box's x/z extremes, so its bottom centre hangs in open air below the geometry. The gait
   planted that point faithfully and the leg floated above it. Replaced with the lowest actual vertex. This alone
   took Censer, Pavise, Banner and Redoubt to a contact of exactly 0.00 and Censer and Pavise to a slide of
   exactly 0.000.
2. The machines with `Socket_Toe_` entries did not improve at all, because the socket branch returns first - and
   the sockets are misauthored. **Every one of Pincer's six toe sockets sits 0.59 m BELOW its own leg mesh**, and
   **Kettle's two front sockets sit 0.96 m ABOVE its lowest geometry**. Those are exactly the machines that
   floated 0.64 m and waded 1.05 m under. A socket now keeps its x/z, which is the artist saying where on the
   foot the weight goes, and takes its height from the mesh, which is the only thing that knows where the foot
   actually ends.

**Redoubt is the exception and it is declined rather than forced.** Its front sockets are 2.15 m below the foot
mesh - far enough that taking the mesh's height puts the toe ABOVE the hip, which is not a leg, and
`EveryWalkersEveryLegIsRiggedWithAToeUnderIt` said so. Where the correction produces an impossible leg the
authored socket is kept, on the grounds that it is carrying information this code does not have. That is why
Redoubt's slide is still 0.901 m and why W1 scores 4 rather than 7.

**The honest cost.** Pincer's belly clearance went from +0.33 to **-0.15 m**: correcting its toe raised the foot,
which lowered the solved ride height from -0.62 to -1.10, and 0.15 m of its hull is now under the ground. That is
a regression traded for removing 0.52 m of float and more than half its slide. Tests are green either way, so
this one is the owner's call, not the loop's.

**The real fix nobody in this loop can make.** Pincer's and Redoubt's toe sockets are wrong in the FBX. Correcting
them at the source would let the socket branch do its job honestly and would retire the fallback added here.
`Tools/crabsplit.py` is where they are exported.

---

## Cycle 48 notes - W3's wording was literal

**"Not a box on a spring" describes what the code does.** A walker's tilt is derived physically - a plane fitted
through the feet that are down - which is the right way round and is the part that works. It is then filtered
twice:

1. `WalkerGait` line 517: `Pitch = Lerp(Pitch, clamp(wantPitch), 1 - exp(-dt*9))`. Justified in place, and for a
   good reason: the tilt has to settle before hip positions can be worked out, so a jumpy plane would move every
   hip.
2. `TankRenderer` line 375: `v.Pitch.Step(pitch, dt, 7f)` - the critically damped spring written for a hull
   riding on tracks, applied to the already-smoothed output of step 1.

Step 2 is the one to question. It reaches 90% in 0.433 s more than step 1 alone, which is 162% added lag for a
quantity that was already smooth and already physical. A tank needs it because `Settle()` samples raw terrain
under the hull and that is noisy; a walker's plane comes from feet that are by construction resting on ground
they were placed on. **The fix is to feed a walker's `Legs.Pitch/Roll` to the hull rotation directly and leave
the spring to the tracked machines.** It is a small change at one site and it was NOT applied, because an
interactive editor is in use and an ungated stance change is how cycles 12, 13 and 20 were spent.

**The footprint problem cannot be fixed in code.** Censer's four feet span one centimetre along the machine.
There is no filter constant and no solver that makes a body pitch convincingly on a footprint with no length: the
plane fit is being asked to resolve a rotation about an axis its samples barely straddle, so the answer is either
clamped or absurd. Redoubt, with 3.32 m of pitch lever, is once again the machine built the way the others should
have been - the same conclusion cycle 42 reached from a completely different measurement.

**Still no critique agent, for the fifth cycle.** The harness refuses these machines and stale renders from an
older pose would be scored against the wrong evidence. That is a real gap in this loop: every score since cycle 41
rests on numbers I chose to measure, with no adversarial eye on what the numbers leave out.

---

## Cycle 49 notes - the first line that scored for being right

Every line attacked so far has scored by having a fault measured. W4 is the first to score because the code
**guarantees** something.

**What is guaranteed.** `allowance` caps the number of feet off the ground at half the machine, before any trigger
is consulted. No combination of drift, stretch, urgency or desperation can lift a fifth leg on a four-legged
machine. "Legs do not all lift at once" is therefore not a tuning achievement that could regress - it is a
property of the loop that decides steps.

**What merely works out.** The average airborne load, 0.77 to 2.07 legs, sits comfortably under the caps of 2 and
3. That matters more than it looks: if the load approached the cap, legs would queue for permission and the
footfall pattern would be dictated by the queue rather than by the gait, which is precisely what "shuffle
continuously" would look like. There is slack, so a pattern is free to form.

**What is not shown, and the reason the score is 4 and not 8.** That a pattern DOES form. Bounded is not the same
as periodic: a machine could respect every cap and still put its feet down in an order that never repeats. Proving
rhythm needs a footfall trace over a few seconds of walking - which leg is down, frame by frame - and that needs
the editor. It is a ten-minute measurement the moment one is free, and the numbers above say what it should show:
a Pincer cycling each leg every 1.32 s at 1.2 m/s with about two legs up at any time.

**One real risk found.** Banner has the shortest stride (0.78 m) and the busiest legs (1.57-1.69 airborne of four)
and is planted only 58% of the time at 3 m/s. Its duty factor is the closest of the six to 50%, below which there
is no support phase at all and the machine is running rather than walking. Banner is also the machine that failed
cycle 47's `LEAN` experiment by sinking 0.46 m. Two independent measurements now point at the same machine being
the one with the least margin.

---

## Cycle 50 notes - the gait's own explanation of itself is wrong

**Every step, on every machine, at every speed, is triggered by `runningOut`.** Not one by drift. Not one by
stretch. Counted over 12 s of walking for all six machines: `{'drift': 0, 'stretch': 0, 'runout': 84}` and the
same shape everywhere.

That makes four tuned constants dead code - `TriggerShare` 0.32, `DesperateShare` 0.52, `ExtendAt` 0.955 and
`Frantic` 0.968 are never the deciding condition - and it means the comment above the decision describes behaviour
the code does not have: *"A leg steps when its foot has fallen far enough behind where it wants to stand. Legs are
not on a timetable: at a crawl one foot moves at a time, at a run the whole side comes through together."* The
first sentence is the drift rule, which never fires. What actually happens is that every leg wants to step on
every frame and the `allowance` cap doles out permission.

**Why, proven rather than reasoned.** A foot is planted at `hypot(homeFlat, drop)` from its hip, and `Stand()`
puts that at **89% of the leg's span** - the same 89.3% cycle 45 found from a completely different direction. The
step threshold is `span * (StepSafety - 0.05) - pace * swingTime`, which is 74-86% depending on speed. Planted at
89%, stepping at 74-86%: the leg is over its threshold by 0.33-0.51 m **before it has moved at all**.

**One number, two lines.** Cycle 45 found the 89% and read it as W6: the leg is 10.7% short of straight, so a
one-piece leg has to be drawn scaled. It is also W4: the same 89% is past the stepping threshold, so the machine
can never stop stepping. `Stand()` placing the foot at 89% of reach is now the single most load-bearing wrong
number on this board.

**The fix, tested in the port and not applied.** `StepSafety` is used for two different jobs: how high the body
stands (`span * StepSafety * upright`) and when a leg must step (`span * (StepSafety - 0.05)`). Splitting them -
standing on less of the leg to buy stepping margin - gives:

| STAND_SHARE | Redoubt planted | Redoubt jitter | Pincer planted |
|---|---|---|---|
| 0.97 (as shipped) | 44% | 91% | 52% |
| 0.90 | 57% | 58% | 51% |
| 0.85 | 74% | 9% | 52% |
| 0.80 | 76% | 4% | 52% |

It fixes Redoubt completely and does **nothing** for the others, and the reason is worth writing down: for a
short-legged machine `pace * swingTime` eats the whole margin. Banner's legs are 2.30 m and it covers 0.50 m
during a swing, so no stand height leaves it any slack. Those machines need a shorter swing or a smaller
allowance, not a lower stance - and a lower stance costs belly clearance, which cycle 47 already pushed negative
on Pavise and Banner. This is a trade, it needs gating, and the owner's editor has been in use since cycle 48.

**What this cycle got right that earlier ones did not.** The port models only the DECISION - scalars, no poses, no
frames, no origins - which is the part of this system that has never been the source of a wrong answer. Every
previous harness failure in this loop came from geometry: axes, origins, pivots, bounding boxes. Porting the
arithmetic and leaving the geometry in Unity is why this cycle produced a result that survived its own checks.

---

## Cycle 51 notes - an absence proved by enumeration

**The useful thing here is the method, not the finding.** Twice in this loop a behaviour was declared missing on
the strength of not seeing it in a render, and twice that was wrong (cycle 10's "segments translate instead of
rotate", cycle 16's binding theory). This time the claim is that nothing leans the machine into a turn, and it is
established by listing **every** occurrence of `yawRate` in the file and **every** assignment to `Roll`. There are
two of the first and two of the second, and none of them connect. An absence enumerated is worth more than an
absence observed, because it cannot be hiding behind a camera angle or a filter time constant.

**The fix is small and the right shape for this board.** A centripetal lean is `roll += yawRate * pace * k`, added
to `wantRoll` beside the existing lost-leg term, which already demonstrates the pattern (`wantRoll += hole.x *
0.16f`) and is already clamped to +/-0.38 rad and smoothed by `k2`. A machine turning at 0.5 rad/s at 1.2 m/s has a
lateral acceleration of 0.6 m/s^2, which against gravity is about 3.5 degrees of honest lean; `k` around 0.06 would
produce that. It is one line, it inherits the existing clamp and filter, and it cannot affect foot placement
because `Roll` only rotates the drawn hull.

**Why it was not applied.** The owner's editor has been open since cycle 48 and a stance or carriage change cannot
be gated against `GaitTests` without it. This one is lower risk than the two already parked - it touches only what
is drawn, not where feet go - but "lower risk" is how cycles 12, 13 and 20 were spent, and the queue of ungated
changes is now three deep. That queue is the real state of this loop: the diagnosis has run well ahead of the
ability to verify anything.

**Three changes now wait on one free editor**, in the order they should be tried, cheapest and safest first:
1. **W5's turn lean** (this cycle) - one line, affects only the drawn hull, cannot move a foot.
2. **W3's double filter** (cycle 48) - stop springing a walker's pitch/roll a second time in TankRenderer; removes
   0.433 s of lag, 162% of the total.
3. **W4/W6's stand share** (cycle 50) - split `StepSafety`'s two jobs. The biggest win available (Redoubt 44% ->
   76% planted, 91% -> 4% jitter) and the only one that moves feet, so the most likely to go red.

---

# Closing summary - the loop is over

It ran 51 cycles between 01:42 and 09:25 on 2026-09-24 and stopped at the 09:23 deadline it was given. Cron
`86f3198a` is deleted. **Nothing is committed** - the owner commits.

## What is fixed, in the tree, uncommitted

**The toe definition (cycle 47), gated green at 13/13.** Two faults in one place, both in `TankModel.ToeOf`:

- It returned the bounding box's bottom-face **centre**, a synthetic point that need not lie on the mesh. On a limb
  modelled diagonally the lowest vertices are at the box's x/z extremes, so its bottom centre hangs in open air
  below the geometry - and the gait planted that faithfully while the leg floated above it. Now it is the lowest
  real vertex.
- Machines with `Socket_Toe_` entries were unaffected, because the socket branch returns first and the sockets are
  **misauthored**: all six of Pincer's sit 0.59 m BELOW its own leg mesh, and Kettle's front pair 0.96 m ABOVE its
  lowest geometry. A socket now keeps its x/z, which is the artist saying where on the foot the weight goes, and
  takes its height from the mesh.

Measured in the shipped draw path, before -> after: contact Censer 0.16 -> 0.00, Pavise 0.14 -> 0.00, Banner 0.02 ->
0.00, Redoubt -0.09 -> 0.00, Pincer 0.64 -> 0.12, Kettle -1.05 -> -0.04. Slide while a foot is called planted:
Censer 0.037 -> 0.000, Pavise 0.042 -> 0.000, Banner 0.056 -> 0.033, Pincer 0.183 -> 0.069, Kettle 0.178 -> 0.116,
Redoubt 1.056 -> 0.901. **One regression traded for it:** Pincer's belly clearance went +0.33 -> -0.15 m, because
raising its foot lowered the solved ride height. Tests are green either way, so that trade is the owner's to accept
or refuse.

## The three changes that are tested and parked

The owner's editor was in use from cycle 48 to the end, and none of these can be gated without it. In the order to
try them, safest first:

1. **A turn lean** (cycle 51). `yawRate` appears exactly twice in `WalkerGait` and `Roll` never derives from it, so
   nothing leans a machine into a turn. `wantRoll += yawRate * pace * k` with k around 0.06 gives the ~3.5 degrees
   that 0.6 m/s^2 of lateral acceleration justifies. One line, inherits the existing clamp and filter, only rotates
   the drawn hull, cannot move a foot.
2. **Stop filtering a walker's carriage twice** (cycle 48). The gait smooths its plane fit at rate 9, then
   `TankRenderer:375` puts the result through the spring written for a tank's hull at omega 7. Together they take
   0.700 s to reach 90% of a new tilt - 0.84 m of travel at 1.2 m/s - and the second filter is 0.433 s of that,
   162% more lag than the gait's own, for a quantity already smooth and already derived from ground the feet rest
   on.
3. **Split `StepSafety`'s two jobs** (cycle 50). It sets both how high the body stands and when a leg must step,
   and the two disagree: a foot is planted at **89% of its reach** while the step threshold is 74-86%, so every leg
   is 'running out' from the instant it lands, by 0.33-0.51 m. Standing on ~0.80 instead takes Redoubt from 44%
   planted with 91% jitter to 76% with 4%. It does nothing for the short-legged machines, whose margin is eaten by
   `pace * swingTime`, and it lowers ride height, which is already negative on Pavise and Banner. The biggest win
   available and the likeliest to go red.

## The two findings that matter most and are nobody's code

- **`Stand()` plants a foot at 89% of the leg's reach.** This one number is both W6 (the leg is 10.7% short of
  straight, so a one-piece leg must be drawn scaled) and W4 (it is past the step threshold, so the machine can
  never stop stepping). Two lines, one cause, found from two unrelated directions five cycles apart.
- **Four tuned constants are dead.** Every step on every machine at every speed fires on `runningOut`;
  `TriggerShare`, `DesperateShare`, `ExtendAt` and `Frantic` are never the deciding condition, and the comment
  above the decision describes a drift rule that never runs.

## The asset faults code cannot fix

- Pincer's six and Redoubt's two `Socket_Toe_` entries are wrong in the FBX (0.59 m and 2.15 m off the mesh).
  `Tools/crabsplit.py` exports them. Fixing them at source would retire the fallback cycle 47 had to add.
- **The pitch axis is degenerate on four machines.** Foot span along the body: Censer 0.01 m, Pavise 0.17, Pincer
  0.36, Banner 0.59, against Kettle 1.38 and Redoubt 3.32. No solver and no constant makes a body pitch
  convincingly on a footprint with no length.
- **Five of six machines author carried equipment below their own lowest leg point** (cycle 42). The gait's ride
  height lifts most of it clear, so only Pavise (Body 0.46 m, Shield 0.29) and Banner (both claws 0.32) are
  actually underground - but it is authored that way in every file.
- **Redoubt is repeatedly the machine built correctly** and the other five the exceptions: it alone authors nothing
  below its feet, has the only real pitch lever, and scores best on silhouette. Three unrelated measurements.

## What this loop was bad at, honestly

**Thirteen of twenty-two lines were never scored at all** - every Damage line, three of five Terrain lines, three
of five Gun lines. The zoom never advanced past level 1, which needs 8 on every line; the best line reached 5.

**No critique agent ran after cycle 41.** The render harness refused every machine from cycle 42 onward, and
scoring stale images from a superseded pose would have been worse than nothing - but the consequence is that every
score from cycle 42 to 51 rests on numbers I chose to measure, with no adversarial eye on what those numbers leave
out. That is the single biggest gap in this record.

**The editor was unavailable for 46 of 51 cycles**, so the loop spent most of its time measuring a Blender harness
rather than the game. Of the faults that harness reported, the ones that survived contact with Unity were the
arithmetic ones; the geometric ones were wrong repeatedly - axes twice, origins twice, and a slide figure
overstated 4x to 15x. **The lesson worth keeping: port the arithmetic, leave the geometry in Unity.** Cycle 50's
decision-only port is the one harness in this loop that produced a result which survived its own checks.

---

# After the loop: the guns and cannons, tested in Unity

The loop closed at its 09:23 deadline with every Gun line either unscored or at 1. The owner then asked for the
guns to be brought into Unity and checked for logical behaviour. This was done by driving the **real sim** through
`MatchSim.CreateGreybox` from `eval` against the live editor - no files written into `Assets/` and no recompile, so
it could be done while the owner's own editor was open and playing.

## The verdict: the gunnery logic is sound

Four machines (Pincer, Kettle, Maw, Tusk), 1500 ticks each, spawned against a dozen enemies.

| What was checked | Result |
|---|---|
| Aim error at the instant a shot leaves | worst **0.313 deg** against a 2.86 deg tolerance; **0 shots outside it** |
| A target held outside the gun's own arc | **0 gun-ticks** |
| Traverse vs its own rate limit | largest single-tick swing **equals** the limit to 3 decimals; **0 snaps** |
| Targets dropped while still alive | **0** - target changes only ever followed a kill |
| Firing rate vs cyclic rate | Pincer 13 vs 6.5/gun, Kettle 10 vs 9.8, Tusk 10 vs 9.4 |
| A mortar aiming inside its own 46 m dead zone | **0 gun-ticks** |
| A gun with `GunHealth = 0` still tracking | **0 of 200 ticks** |

## Every fault I thought I had found was my own measurement error

This is the part worth keeping, because five separate alarms were raised and all five were the instrument:

1. **"It never fires."** Firing sets `Reload` in the same tick it fires, so reading state after `Step` can never
   observe `Reload == 0` on a tick that fired. "Always reloading" was the signature of a gun firing normally.
2. **"It fires 85 degrees off target."** Both shots were at tick 0 - the per-slot spawn init sets
   `Reload = 30 + 10k` inside the first step, and I counted that as a shot.
3. **"149 snaps past its traverse limit."** I divided by 30 Hz; `cfg.TickRate` is 20. The observed maximum was
   exactly 1.5x my limit on all three machines, and that exactness is what exposed it.
4. **"Kettle never engages."** Its mortar has `RangeMin = 46 m` and `Indirect = true`, and my enemies were at
   26 m. Refusing that shot is correct behaviour.
5. **"It fires far below its cyclic rate."** My shot counter sat behind a `continue` that skipped any tick where
   the target had just died, so it missed the shot that killed it.

The pattern is the one the loop recorded over 51 cycles: a proxy read as though it measured the real thing. The
difference here is that the sim is deterministic and inspectable, so each wrong reading could be run down to its
cause in a single step instead of costing a cycle.

## What is actually left to do on the guns

- **G3 has never been rendered.** The flash and smoke anchors are correct by construction - they hang off the gun
  part's own matrix - but nobody has looked at one firing. The known asset fault is the **Maw's two sponson muzzle
  sockets, 21 degrees asymmetric** (-134.72 and +155.85), found at cycle 7 and still unfixed.
- **G4's real question is untested.** The enemies in this test stood still. Tracking a moving target is the thing
  the line asks about.
- **The traverse has no easing.** It is exactly rate-limited, which is why it does not snap, but it starts and
  stops at full rate with no acceleration. That is the one place where "reads as weight" could still be improved,
  and it is a small change at the same line that clamps the step.

---

# The animation loop (restarted 2026-09-25, 20 min cadence, cron bc8bb93e)

The owner restarted the loop with one change that matters more than anything in the first 51 cycles: **the evidence
is now rendered frames of the running game**, not a Blender harness. Cycles 42-51 produced no critique at all
because the harness refused every machine; this path does not refuse.

## The proven capture path — do not re-derive it

1. `open_scene Assets/_Project/Scenes/GreyboxCorridor.unity`, then `editor_play`. Bootstrap routes to MainMenu;
   GreyboxCorridor is the battle scene. If `SimHost.Local` reads null, the eval raced `Awake` - retry once. An
   earlier apparent NRE storm was a stale play session, not a fault in the scene.
2. `FindFirstObjectByType<TW.Presentation.SimHost>()`, then `host.Local` is the match. Spawn with
   `m.World.Spawn(0, archetype, at, 800f, 1.4f, true)` and give a goal with
   `m.Fields.GetGoal(GoalKey.Cell(m.Map.NavIndex(c.x, m.Map.NavLength-6), NavMode.Tracked))`.
3. Photograph with the project's OWN rig, `Assets/_Project/Editor/CaptureRig.cs`, which already does everything
   this loop spent cycles building badly in Blender: `Shot(path, x, z, zoom, yaw, pitch, w, h)` queues a still,
   `Pending()` drains it, `Series(dir, stem, x, z, zoom, yaw, pitch, count, everyFrames, w, h)` takes a sequence,
   `Sheet(dir, stem, out, cols, cellW)` tiles it into one image, and `Hold()`/`Release()` freeze the weather.
   Zoom 34 / pitch 22 frames a machine well at tactical distance.

## Cycle A1 - the pipeline works, and the first critique is half an artefact

**What was done.** Six walkers spawned into a live battle with enemy infantry, photographed at tactical zoom, and
put to a harsh critique agent with the Walk and Terrain lines in scope.

**What the critic found that stands up.** Its highest-value fix is the same fault this board reached from three
unrelated directions: **the body rides too low**. It measured belly clearance at roughly 38% of standing height
from the silhouette alone, which agrees with cycle 48's numbers (Censer +0.04 m, Banner -0.32, Pavise -0.46) and
with cycle 1's unmet target of a 2 m soldier passing underneath. Its mechanism is new and worth having: the hull
skirt hangs below the leg's articulation, so **the femur is never in silhouette** - the player sees a hull with
six spikes, and every solver refinement in `WalkerGait` is invisible behind the shell. It also points out that
this is why T1 and T5 cannot even be audited from a top-down camera: the machine's own skirt occludes the
foot-ground junction.

**What it found that is MY artefact, not the game's.** It reported, with six independent samples, that not one
foot is ever airborne, and concluded the duty factor is near 1.0. That contradicts cycle 50's measurement of 52%
planted. The contradiction resolves against the critic: **five of the six machines were standing still** when I
photographed them (`Velocity` 0.00 on slots 0,1,3,4,5; only Censer moving at 2.20 m/s). A stationary walker
correctly keeps every foot planted. The critic reasoned properly from the frames it was given; the frames were of
parked machines.

**A second capture fault, caught before it reached anyone.** A `Series` aimed at the Pincer's coordinates produced
eight frames of empty ground - the subject was not in shot. Evidence must be confirmed to contain its subject
before it is scored. This is the same failure the first loop made repeatedly, and it is now the first thing to
check each cycle.

**An incidental finding worth chasing separately.** Five of six walkers stopped dead after a few metres with goals
set and speed 1.4. That is navigation, not animation, but a machine that stands still is also a machine nobody can
judge the gait of.

**Uncovered by the board, raised by the critic and worth adding:** there is **no foot-contact feedback at all** -
no contact shadow, no mud decal, no splash, no ripple, in a rain-and-mud setting. And there is no lateral weight
transfer: the hull centroid sits at the centroid of the foot polygon, which reads as statically balanced rather
than carried.

**No fix applied.** The critique's top item is real but its second item was an artefact, and the capture method
needed fixing first. Next cycle: keep the machines walking, confirm the subject is in frame, and re-run the
critique on a genuine gait sequence before changing a constant.

## Cycle A2 - the gait is real, and the instrument fooled two critics in a row

**Target: W2. Outcome: W1 4 -> 6, W4 3 -> 5, and one new fault found. No code changed.**

### The capture problem is now understood, and it is the reason for both bad critiques

Three separate ways this loop's own evidence has lied, all found this cycle:

1. **Parked machines (cycle A1).** Five of six walkers had stopped because they were spawned 16 m apart and were
   blocking each other. Spacing them 36 m apart fixes it and they walk at 1.46-1.74 m/s. A parked walker correctly
   keeps every foot planted, which is why the A1 critic concluded there was no gait.
2. **THE GAIT ONLY ADVANCES FOR MACHINES THE RENDERER IS DRAWING.** `WalkerGait.Step` is called from
   `TankRenderer`'s per-view update, so a machine that is off-camera is not stepped at all. Reading its feet gives
   a frozen pose - I read `swing = 0.495` on two feet, byte-identical across thousands of frames, and briefly
   believed the gait had locked up. Pointing the camera at the machine and re-reading gave 0.24, then 0.61: it had
   been running all along. **Any measurement of a machine that is not on screen is meaningless**, whether taken
   in pixels or in state.
3. **The camera is not as fixed as the brief claimed.** The A2 critic checked the capture rig's own `.json`
   sidecars and found `pose_error_m` up to 0.841 on frame 01, and two frames blown out by a lightning strike with
   the harness's own `contrast_median` warning. It corrected for the drift and said so. Future cycles should gate
   captures on `pose_error_m < 0.02` and reject frames whose `contrast_median` collapses.

### W1 4 -> 6: the anchoring is exact, and the critic's "total slip" is refuted

The A2 critic scored W1 **0/10**, reporting that the foot band tracks the hull 1:1 (+26 px each) and calling it
"a rigid decal being dragged across the mud". That is a careful piece of pixel work and it is wrong, and it was
worth checking rather than believing because it contradicted two existing measurements - cycle 47's `RigMeasure`
put slide at 0.000-0.116 m for five machines, and `GaitTests.APlantedFootStaysOnTheGroundItWasPutOn` passes.

Read out of the live gait instead: a planted foot's `Anchor` is **byte-identical** across samples taken thousands
of frames apart - foot 0 at (85.033, 258.730) and foot 2 at (91.504, 259.329), unchanged - while `At` equals
`Anchor` exactly for every planted foot. The world anchor does not drift by a millimetre. The critic was tracking
a 6.5 m machine at ~18 px/m through rain at night; at that scale a band containing legs, hull skirt and shadow
tracks the hull because the hull dominates it.

Scores 6 and not 9: the anchor holding is not the same as the drawn mesh holding, and cycle 47 measured up to
0.116 m of mesh slide on Kettle and 0.901 m on Redoubt while the foot was called planted. That gap between the
anchor and the geometry is the remaining fault, and it is the one already recorded under the toe definition.

### W4 3 -> 5: there is a rhythm, and it is the one the code claims

Read live off the running machine: swing phases `-1.00, 0.24, -1.00, 0.24`, then `-1.00, 0.61, -1.00, 0.61`.
**Two of four feet airborne, in diagonal pairs, cycling.** That is a trot, and it matches cycle 50's port exactly
(diagonal pairs at 0% jitter). The A2 critic scored W4 **1/10** on "the count never changes and is always zero" -
again careful, again reading an unrendered or arrived machine.

Scores 5 and not 8 because of the new fault below.

### THE NEW FAULT: an arrived machine marches in place, forever

The Censer reached its goal and stopped - `simSpeed 0.000`, position static at (88.268, 260.047), `yaw 0.000` and
not rotating. **It is still stepping.** Feet 1 and 3 cycle indefinitely while feet 0 and 2 stay planted. A machine
standing still is lifting two of its four feet over and over and putting them back in the same place.

This is what both critics were reaching for and could not name - the A2 critic measured the near machine
travelling 0.14 m over a full gait cycle and called it "stepping in place", which is exactly right.

**Hypothesis for the mechanism, NOT yet tested and NOT acted on.** A step target is clamped into the leg's
reachable annulus (`reachable = clamp(flat.magnitude, inner, outer)` in `WalkerGait.Step`). If a foot's home
position lies outside that annulus, the foot can never land on its home, so its drift is non-zero the instant it
lands, and it re-triggers forever. Cycle 45 established that the stance already sits at **89% of reach**, hard
against the outer edge, so a small difference in ground height under one leg is enough to push its home outside
the annulus. That would also explain why it is the same two feet every time rather than all four.

**What would test it in one step:** log, for each foot at the moment it lands, the distance from its `Anchor` to
its `Home`, and whether the target was clamped. If the clamped feet are exactly the perpetually-stepping ones, the
hypothesis holds and the fix is rig data (bring the stance radius inside the annulus), not solver structure.

### Still uncovered by the board

The A1 critic's point stands and the A2 critic repeated it independently: there is **no foot-contact feedback of
any kind** - no contact shadow, no mud decal, no splash, no ripple - in a rain-and-mud setting. Both critics also
independently noted the machines show **no lateral weight transfer**: the hull centroid sits at the centroid of
the foot polygon, which reads as statically balanced rather than carried. Neither has a line on this board.

## Cycle A3 - the first critique whose mechanism survived code review, and the first applied fix of this loop

**Target: T1. Outcome: T1 - -> 2, T2 - -> 3, T4 - -> 4, W2 0 -> 3. ONE FIX APPLIED, gated GREEN at 338/338.**

### Two capture attempts were thrown away before a critic saw them

Pitch 9 degrees put the camera at ground level and the foreground parapet occluded the machine's feet in every
frame; a first side-on run drained before the machine reached the lip. Both were discarded rather than scored.
That is the A1/A2 lesson applied: confirm the evidence contains its subject before anyone grades it.

### The critic checked my capture against its own sidecars, and was right to

It read the `.json` sidecars and found what I had not: `pose_error_m` is exactly 0 for frames 00-05, jumps to
0.208 at frame 06 and decays 0.208, 0.080, 0.034, 0.013, 0.005, 0.002 - **a camera kicked by a shell burst at
frame 06 and still settling until 09**, which are precisely the frames where the machine is on the parapet. It
also caught that the machine travels about 60 degrees into depth rather than across frame, and that at 0.15 s
between samples against a 0.24 s swing, **the sample interval is 62% of a complete step**, so arc peaks are
sampled away. Future captures: shorter `everyFrames`, and check the travel axis is across the frame.

### T1: the parapet does not exist as far as the gait is concerned

Confirmed in code, not inferred. `BattlefieldComposer:324` emits `kit.sandbag` at `ground - 0.04f` as a prop;
`TankRenderer.Ground` is `RenderGround.Sample`, heightfield plus craters only. The bags, revetment and duckboards
contribute **no height** to the function that places feet, so a foot aimed under the bag row plants at bare
terrain, inside the stack. `ArcFor` has the same blind spot: it raises the step arc against the heightfield, so it
clears a 1.8 m carved ditch perfectly and has no idea a 0.5 m bag stack sits on the lip.

### The fix that was applied: the ground under the hull now gets a vote

**The fault.** `Carry` sets `height = meanY + Stand(rigs)`, where `meanY` is the mean height of the planted feet.
Nothing in it knows what is underneath the body. Coming over a bank, the leading feet reach forward and down onto
the lower grade, `meanY` falls, and the hull follows it straight into the parapet.

**Why `MaxSink` does not catch it, which is the sharp part.** The guard is
`height = Clamp(height, means - MaxSink, means)` with `means = meanY + Stand(rigs)` - **the same falling mean the
clamp is supposed to restrain**. A clamp expressed relative to the quantity that is dropping can never arrest the
drop. The file's own comment says MaxSink exists so "the machine does not walk itself into the ground"; against a
step in the terrain it provides literally zero restraint. That is a real bug in a guard that reads as if it works.

**The change**, in `WalkerGait.Carry`, after the existing clamps: sample `ground()` under the body centre and
part-way out toward each hip, take the highest, add `Clearance`, and raise the ride height to it - but never above
the existing `ceiling`, because a body lifted past what the legs can reach pulls planted feet off the ground,
which is a worse fault than a scraped belly. `Carry` had to be given the ground function, which only `Step` held.

**Gated:** 338 passed, 0 failed (the whole EditMode suite, not just GaitTests - the filter did not narrow and that
is a stronger result). **Verified by re-shooting the same crossing with the same camera**: the hull now rides clear
above the sandbag line for the entire crossing with the legs extended beneath it, and the squat frames are gone.

### Still open, in priority order

1. **Give the parapet height.** Until the bags contribute to the sampler, T1 cannot rise above about 3 however
   good the solver is. This is the one that needs an owner decision, because it touches terrain, not the rig.
2. **Leg-on-leg interpenetration during the crossing** - frames 04 and 05 show the screen-left pair folded into a
   single wad with the femur through the tibia and no resolvable claw. No self-collision and no per-joint limit.
3. The three parked fixes from the first loop are all still parked: turn lean, the double pitch/roll filter, and
   the stand share.

## Cycle A4 - the reason three cycles of terrain evidence kept failing

**Target: T3. Outcome: T3 scored 0 WITH A REASON, and the blocker behind A1-A4's capture failures identified.
No fix applied - the fix is not in the rig.**

### What happened

Found a genuine 3.07 m deep crater by scanning `RenderGround.Sample` over the open ground between the trench
lines, put a Pincer on its southern lip, and shot 20 frames at 5-frame spacing (tighter than A3, per the A3
critic's point that 0.15 s sampling against a 0.24 s swing loses the arc peaks).

The machine did not move. Twenty frames, same position, `x = 80.0, z = 127.0` at the start and at the end.

### Why: broken ground is impassable, so a walker is never routed into it

This is the third cycle in a row where machines stopped and spoiled the evidence, so it was worth running down
properly rather than working around again.

- No halt, no ditch, no bog: `HaltTicks 0, DitchTicks 0, BogTicks 0, SpeedFactor 1`, `Speed` field 1.30, a valid
  goal - and measured velocity **0.000**.
- The tracked flow field at the machine's own cell: `Direction 255` (NoDirection) and
  `Integration 2147483647` = `FlowField.Unreachable`. Same for the three cells north of it. The fourth cell north
  is reachable, integration 1276.
- The map itself: `navCost 255` - impassable - across the crater's footprint, with `layer 41` where the ground
  either side is cost 4 / layer 33 and cost 1 / layer 1. Ground heights across the pocket run -1.05 to -1.55 m
  against +1.38 m on the rim, and the differing layer bit plus the depth says this is a **flooded** shell hole.

So the crater is an impassable island. A machine standing in one has no direction out and stops forever.

### Two consequences, and the second is the important one

**1. A unit inside an impassable pocket is stranded permanently.** It has no escape direction, because the field
only tells it where to go from cells that can reach the goal. Whether units can END UP in one during real play -
pushed by a blast, or standing where a shell then digs and floods the ground under them - is not something this
cycle established, and it is worth an owner's attention because the failure mode is a frozen unit, not a slow one.

**2. T3 and T5 cannot be earned by any change to `WalkerGait`.** The gait is never asked to put a leg down a
revetment or into a shell hole, because the navigation refuses to route a walker through either. Three cycles of
captures failed to show a leg reaching into broken ground for exactly this reason. The line is not measuring the
solver; it is measuring a route that is never taken.

**The decision this needs, which is the owner's and not the loop's.** There are only two nav modes,
`NavMode.Infantry` and `NavMode.Tracked`, and the walkers use `Tracked` - **they are navigated as tanks.** A
six-legged machine that straddles a 3.6 m trench and stands 6.5 m tall is being told it may not cross ground a
tank cannot cross. Either walkers need a third mode whose passability reflects what legs can actually do, or T3
and T5 should be struck from this board as untestable and the effort moved to lines the gait actually controls.
Until that is decided, further captures aimed at these two lines will keep producing machines standing still.

### Note on the A3 fix

The belly floor applied last cycle is still in the tree and still green. Nothing this cycle touched it.

## Cycle A5 - the double filter removed, measured A/B, and it worked

**Target: W3. Outcome: W3 2 -> 5. Parked fix (b) APPLIED and gated GREEN at 338/338. One new artefact introduced,
flagged below.**

### The change

`TankRenderer` was putting a walker's pitch and roll through the critically damped spring written for a tank hull
on tracks (`omega 7`), on top of the exponential filter at rate 9 that `WalkerGait.Carry` had already applied. A
tank needs its spring because `Settle` samples raw terrain under the chassis and the result is noisy; a walker's
attitude is a plane fitted through feet that are resting on ground they were placed on. The second filter is now
skipped for legged machines - their already-smoothed values are written straight to the hull - and tracked
vehicles are untouched.

### The evidence: an A/B with one variable

Same machine, same parapet, same camera, same 12-frame sequence. The critic read all 24 sidecars first and its
triage is worth recording, because **this time the BEFORE sheet was the one with the kicked camera**: `fixed_10`
and `fixed_11` show `pose_error_m` 0.195 and 0.080 with `cam_pitch` 18.47 and 18.84 against a commanded 19.0, and
they were exactly where its line-fit produced its wildest outliers. Every AFTER frame has `pose_error_m == 0` and
`cam_pitch == 19.0`.

Hull dorsal angle, positive = nose-down, +/-3 deg:

| frame | 01 | 02 | 03 | 04 | 05 | 06 | 07 | 08 | 09 |
|---|---|---|---|---|---|---|---|---|---|
| BEFORE | +24 | **+13.5** | **+13.5** | +10 | +9 | +10.5 | +12.5 | +9.5 | +10.5 |
| AFTER | +25 | **+22** | **+22.5** | +24 | +24 | +26 | dark | dark | dark |

Frames 02 and 03 are position-matched to within 1-5 px using the selection ring as a world anchor, so those are
the controlled pairs: **+8.8 and +9.0 degrees of difference, three times the error bar.**

### Two honest limits on this result

- **The two runs are not frame-locked.** BEFORE spans engine frames 1771-1870, AFTER 876-975; the gait phases
  differ and from frame 04 onward AFTER falls 20-35 px behind. The 9 degree claim rests principally on two tightly
  matched frame pairs, consistent with three more.
- **The jitter risk is NOT cleared.** At 8 rendered frames between samples - 0.13 s - anything faster than about
  4 Hz is aliased away. A single-frame snap would be invisible. Clearing "is it snappy now" needs a capture of
  CONSECUTIVE frames, which no capture in this loop has yet taken.

### The new artefact this fix introduced

With the hull now genuinely nose-down about 23 degrees on the parapet, **the turret and gun barrel do not
compensate**, and the silhouette reads as a crab holding a rifle at the sky in AFTER frames 01-06. In BEFORE the
barrel looked level because the hull was. This is a consequence of fixing the hull, not a fault in the fix, but it
is a visible one and it needs its own answer: the turret should stabilise against world horizontal, wholly or
partly. Reported by the critic and **not independently verified by me** - next cycle should confirm it before
anyone changes the turret.

Also found, pre-existing and not caused by this change: in BEFORE frame 03 the front-left claw hangs in clear air
above the mud. Foot IK not reaching ground, on flat approach.

### What the critic says is now the ceiling on this line, and it is convincing

A least-squares plane through the planted feet **is a low-pass filter made of geometry**. One foot planting moves
the fitted plane by a fraction of a degree, so the body cannot answer an individual footfall however fast the
result is written to the hull. That is why both sheets show the hull holding one attitude for six to eight frames
while the legs do something completely different underneath. Removing the electronic lag exposed the structural
one. Its three suggestions, cheapest first:

1. **A heave channel** - the body has no vertical dynamics at all in any of the 24 frames; it never drops on a
   plant or rises on a push-off. A short stiff heave spring driven by total foot load. **This is the next fix to
   try.**
2. Weight the plane fit by per-foot load rather than treating every planted foot equally.
3. A small impulse torque on foot strike, decaying over ~0.15 s, on top of the plane solution. That is the lurch.

## Cycle A6 - a tested-and-rejected fix, and a correction to cycle A3

**Target: W6. Outcome: W6 2 -> 3. One fix tried and REVERTED on measurement. A3's fix also REVERTED - it was not
as green as I reported. Tree ends GREEN at 338/338.**

### W6 measured rather than argued

Reading the drawn geometry straight out of the running renderer - hip pivot to solved toe, over each leg's own
`MaxSpan`:

| machine | span used | spread |
|---|---|---|
| Pincer | 87-89% | 2 points |
| Censer | **80-109%** | **29 points** |
| Redoubt | 63-89% | 26 points |

Pincer's legs are uniformly 11% short, which matches cycle 45's analytic 89.3% exactly and reads as a crouch, not
a defect. **Censer is the fault**: one leg drawn 9% beyond full stretch while another is 20% short, in the same
frame. The mechanism is `stretch = Mathf.Clamp(dist / rig.Bone[0], StretchMin, StretchMax)` with 0.80 / 1.18 -
each one-piece leg is independently rubber by up to a fifth of its length, with nothing coupling it to its
neighbours.

### The fix that was tried and rejected, with the numbers that rejected it

Tightened the clamp to 0.88 / 1.10. Tests stayed green, and the first measurement looked like a clean win:

| | before | after, sample 1 | after, sample 2 |
|---|---|---|---|
| Censer spread | 29 | **9** | 22 |
| Censer worst planted toe error | 0.000 m | 0.000 m | **0.604 m** |

The second sample is the one that matters. A tighter clamp means a leg that needs more than 110% cannot reach,
and the toe leaves the ground it was planted on - 0.604 m of visible detachment, exactly what the comment beside
`StretchMax` warns about ("a foot detached from its leg rather than a leg reaching"). The baseline never once
showed toe error. **Trading a 29-point length spread for an intermittent 0.6 m detached foot is a bad trade**:
W1 scores 6 and T5 scores 5, and this would damage both to improve W6, which scores 3. Reverted.

**The useful negative result: the +/-20% rubber is load-bearing and the clamp cannot simply be tightened.** Any
real fix for W6 has to be upstream - stop asking legs for distances they cannot reach - not at the clamp that
hides it. The step target is already clamped into the reachable annulus, so the place to look is how that annulus
is computed, and cycle A6's second finding says exactly where.

### CORRECTION: cycle A3's belly floor was not reliably green, and has been reverted

I reported the belly floor as "gated green at 338/338" in A3 and it passed again in A5. It failed this cycle on
**identical code** - `TheLegTheArtistModelledReachesTheFootTheGaitChose`, Banner leg 1 missing its foot by
0.093 m, 4.1% of the leg against a 1.5% tolerance. Disabling only the belly floor returned 338/338; restoring it
failed again. So it was **intermittently red all along and passed twice by luck.** That is a reporting error on my
part and the board should carry it.

**Why it fails, which is also why W6 cannot be fixed at the clamp.** The belly floor raises the hull. Step targets
are clamped into the leg's reachable annulus using
`drop = Mathf.Max(hip.y - target.y, Stand(rigs) + rig.Hip.y)` - measured against the machine's *nominal stance
height*, not against the height it is actually riding at. Raise the body above `Stand` and every step target is
computed with too small a drop, so feet are placed too far out, and a leg then cannot reach the foot it was sent
to. The `ceiling` bound I added only considers feet that are ALREADY planted; a foot in swing when the body rises
lands somewhere unreachable.

**This is one root cause behind two lines.** W2's squat needs the body to rise over a bank; W6's rubber legs come
from feet placed further out than the leg can reach. Both meet at the same place: **the step-target annulus is
computed against the nominal stance height instead of the actual ride height.** A belly floor that also fed the
raised height into the step-target drop would fix the squat without stretching the legs. That is the fix to try
next, and it is a real change rather than a constant, so it wants its own cycle.

### State of the tree

- A5's pitch/roll filter change: **kept**, still green.
- A3's belly floor: **reverted**. The W2 score earned on it is annotated below.
- A6's stretch clamp: **reverted**.
- 338 passed, 0 failed.

## Cycle A7 - the belly floor re-applied with a bound, gated three times, and honestly unproven

**Target: W2. Outcome: W2 stays 3. Fix APPLIED and green on THREE consecutive runs. Its effect is NOT
demonstrated, and one term in it is dimensionally wrong. Both recorded rather than glossed.**

### What was applied

A6 showed why the first belly floor failed: `Step` picks each foot against the ride height the body had at the
time, and `Carry` then raised the body by an unbounded amount, so a leg could not reach the foot it had been sent
to. Re-applied with two bounds:

- it may not rise above `ceiling`, what the currently planted legs can reach; and
- it may not rise more than `MaxSink` above `means`, the stance the machine chose for itself - the same distance
  it is allowed to sink, used symmetrically, so the lift can never exceed the margin the step-target clamp already
  carries.

### Gated properly this time

**338 passed / 0 failed, three runs in a row.** A6 established that this test is intermittent and that A3's single
green run proved nothing; three consecutive passes is the standard this board should have been holding all along.

### What is NOT established, and a term that is wrong

**The fix is not shown to do anything.** Measured live after the crossing: `height = -0.25`, ground under the hull
`0.85`, lowest non-leg part `0.69` - a **belly clearance of -0.15 m**, exactly what cycle 48 measured for the
Pincer before any of this. The squat itself was not re-measured during a crossing this cycle, so whether the
parapet plough is fixed is **unknown**.

**And the `Clearance` term does not mean what its name says.** The code compares `ground + Clearance` against
`height`, but `height` is the height of the **body PIVOT**, and on these machines the pivot sits roughly 2.8 m
BELOW the hull's underside - the same fact cycle 44 established when Unity's ride heights came out negative
(Pincer -0.62, Censer -0.65). So `under + Clearance` is not a belly clearance at all; asking the pivot to sit
0.5 m above the ground asks the machine to levitate by nearly three metres, and the only reason it does not is
that the `means + MaxSink` bound clamps it. **The bound is doing all the work; the term it bounds is nonsense.**

What the change therefore amounts to, stated accurately: *the body may rise by up to MaxSink above its chosen
stance when the ground beneath the hull is higher than the ground beneath its feet.* That is a sensible behaviour
and it is in the right direction, which is why it is kept rather than reverted - but it is not the belly floor it
is named after.

**The fix for next cycle is now specific:** measure each machine's hull-underside offset from its body pivot once
at load (`TankModel` already measures `Height`, the hull's TOP, the same way), store it beside `Height`, and use
`under + hullUnderside + clearance` as the floor. Then the term means what it says and the bound stops being the
only thing holding it.

### State of the tree

- A5's pitch/roll filter change: kept, green.
- A7's bounded belly floor: kept, green x3, effect unproven.
- 338 passed, 0 failed.

## Cycle A8 - the turn lean applied, green four times, and never once seen to fire

**Target: W5. Outcome: W5 stays 3. Parked fix (a) APPLIED, green on four runs. Its effect could not be observed,
and one number I wrote this cycle was wrong and has been corrected.**

### What was applied

`wantRoll += yawRate * pace * TurnLean` with `TurnLean = 0.06`, sitting beside the existing lost-leg term so it
inherits the same +/-0.38 rad clamp and the same rate-9 filter. Roll is positive left-up and Unity's yaw is
positive clockwise from above, so a right turn lifts the left side, which is banking into the turn. It moves only
what is drawn - no foot is placed from `Roll` - which is why it was the cheapest of the three parked fixes.

Gated at **338 passed / 0 failed on four separate runs**, holding the standard A6 forced on this board.

### It has never been seen to fire

Five samples, across two deliberately commanded turns (a 90 degree turn east, then a 180 degree turn south):

| yaw | renderer YawRate | sim speed | lean term |
|---|---|---|---|
| 90.0 | 0.0000 | 1.25 | 0.00 deg |
| 0.0 | 0.0000 | 1.60 | 0.00 deg |
| 135.0 | 0.0000 | 0.00 | 0.00 deg |
| 135.0 | 0.0000 | 0.00 | 0.00 deg |

The yaw is demonstrably changing between samples - 90, then 0, then 45, then 135 - so the machine IS rotating.
But **`yawRate` and `pace` were never both non-zero in the same sample**, so the product was always zero and the
term contributed nothing.

Three explanations fit and I could not separate them:
1. Turns complete inside one eval round-trip (a few seconds), so every sample lands after the turn.
2. The machines **stop dead to rotate** and never turn while walking, in which case `pace` is zero throughout
   every turn and this term is permanently inert.
3. The machine was simply stuck, which is the fault A1 and A4 already recorded.

If (2) is the truth then the fix is worthless as written and **the thing to fix is the stopping, not the lean** -
a machine that halts to pivot will never bank however good the coefficient is. That is now a note in the code
beside the constant, so the next person does not tune a number that cannot fire.

### A number I got wrong, corrected in the same cycle

The parked note and my first comment both claimed `TurnLean = 0.06` gives "about 3.5 degrees" at 0.5 rad/s and
1.2 m/s. It does not: `0.5 * 1.2 * 0.06 = 0.036 rad = 2.06 degrees`. The 3.5 degrees is the TRUE centripetal
angle, `atan(v*w/g) = atan(0.6/9.81)`, which is what the coefficient is a fraction OF - 59% of it. The comment now
says so.

### State of the tree

- A5's pitch/roll filter change: kept, green.
- A7's bounded belly lift: kept, green, effect unproven.
- A8's turn lean: kept, green x4, effect unobservable.
- 338 passed, 0 failed.

**Two of the three changes now standing in this tree are unverified in effect.** That is worth saying plainly:
this loop is currently better at gating changes than at demonstrating they do anything, and the reason in both
cases is the same - the evidence path can photograph a machine but cannot reliably catch it in the act of the
thing being measured. The next cycle should fix the instrument rather than add a fourth change: a per-frame log
of `Height`, `Roll`, `yawRate` and `pace` over a few seconds of walking, written to a file and read back, would
have answered both A7 and A8 outright.

## Cycle A9 - built the instrument instead of a fourth change, and it settled both open questions

**Target: the instrument, as cycle A8 recommended. Outcome: W5 3 -> 6, T4 4 -> 7, W2 3 -> 4. No new change to the
game. Tree green at 338/338 with the probe removed, 340/340 with it in.**

A7 and A8 each left a change gated green but unproven, for the same reason both times: play mode would not hold
still long enough to catch the machine in the act. Rather than add a fourth unproven change, this cycle built the
measurement. `GaitTests` already has a `Walk()` harness that drives `WalkerGait` tick by tick with an explicit
`yawRate` alongside a non-zero velocity - exactly the condition play mode never produced - and a slope is just a
ground function. No play mode, no camera, no frames to alias.

### A8's turn lean DOES fire, at precisely its designed value

| | t0.23 | t0.48 | t0.73 | ... t2.98 |
|---|---|---|---|---|
| turning, yawRate 0.5, speed 1.2 | 1.85 | 2.04 | **2.06** | 2.06 |
| straight, same speed | 0.00 | 0.00 | 0.00 | 0.00 |

Roll in degrees. It rises through the rate-9 filter and settles at **2.06 degrees**, which is the predicted
`0.5 * 1.2 * 0.06` to two decimal places, and holds for the rest of the turn. **A8's conclusion that the term had
never been seen to fire was a failure of the instrument, not of the fix.** The note I left in the code beside
`TurnLean` warning that it might be inert is wrong and should come out.

### A7's belly lift engages, and it anticipates

| ground under hull | 0.00 flat | 0.05 -> 0.65 slope | ridge, step up at z=24 |
|---|---|---|---|
| ride height | -1.10, dead constant | -1.18 rising to -0.44 | -1.10, then -0.88 from t3.23 |

On the flat the height is constant to the centimetre for three seconds - no bob, no hunting, nothing. On the
slope the body rises with the ground. On the ridge the height changes at **t3.23, while the ground under the
machine's centre still reads 0.00** - the lift is sampling out toward the hips, which reach the step before the
centre does, so the body starts rising before it arrives. That is the intended behaviour and it is working.

W2 moves only 3 -> 4, because what this shows is that the lift engages, not that the parapet squat is cured, and
belly clearance on the flat is still negative.

### An incidental result that is worth more than the two it was chasing

**On a constant 0.18 gradient the body settles at a pitch of 10.20 degrees, and atan(0.18) is 10.20 degrees.**
The plane fit through the planted feet reproduces the ground slope exactly. T4 has been sitting at 4 for want of
a sustained slope to test on; it did not need a slope in the world, it needed a slope in a function.

### The lesson for the rest of this loop

Every question this loop has failed to answer in play mode - does a foot slide, does the body bob, how many feet
are airborne, does the lean fire - is a question about `WalkerGait`, which is deterministic, has no dependency on
rendering, and can be driven tick by tick from a test in about twenty lines. Play mode is needed for what the
machine LOOKS like; it is the wrong instrument for what the solver DOES, and this loop has spent several cycles
learning that the expensive way.

## Cycle A10 - W6 re-measured with A9's method, and half of cycle A6 is withdrawn

**Target: W6. Outcome: W6 3 -> 4. No change applied to the game beyond correcting a comment. Tree green.**

### What A6 got wrong, and why

A6 measured leg length from two play-mode snapshots and reported Censer at **80-109%** of its own span - one leg
past full stretch, another well short - then tightened the stretch clamp, saw a 0.604 m detached foot, and
reverted. Two snapshots are not a distribution, and they conflated two different things.

Re-measured with cycle A9's method - the solver driven tick by tick, six seconds of walking, flat ground - and
counting **planted legs only**, because a leg in swing is tucked and legitimately short:

| machine | drawn, planted | spread | demand over 100% |
|---|---|---|---|
| Censer | 88-93% | 5 | never |
| Pincer | 88-97% | 9 | never |
| Pavise | 80-93% | 13 | never |
| Kettle | 77-94% | 17 | never |
| Redoubt | 75-94% | 19 | never |
| Banner | **68-97%** | **29** | 0.6% of leg-ticks |

**The demand never exceeds 100%.** What the gait asks of a leg, before any clamp is applied, stays inside what
the leg has on every machine but Banner, and on Banner only 0.6% of the time. Toe error is 0.000 m throughout.

Two consequences:

1. **On level ground no leg is ever drawn stretched.** They are drawn SHORT - up to 32% short on Banner. A6's
   "one leg 9% past full stretch" does not happen on the flat; it must come from terrain, which is where A6 was
   measuring. That half of A6 is withdrawn for level ground and still stands for slopes.
2. **The stretch clamp is almost never binding**, which explains A6's negative result from the other direction:
   tightening a clamp that rarely fires cannot help much, and when it does fire it is in exactly the marginal
   cases where the alternative is a detached foot.

**The measurement error worth remembering: including swinging legs.** It made every machine look 10-15 points
worse at the bottom of its range - Banner read 53% rather than 68%, Kettle 58% rather than 77% - because it was
measuring the swing tuck, not a leg failing to reach. W6 is a question about legs that are standing on something.

### Housekeeping

The note cycle A8 left beside `TurnLean` warning that the term might be inert was wrong - A9 measured it firing
at 2.06 degrees - and has been replaced with A9's measurement.

### A tooling trap that cost three runs this cycle

Editing a `.cs` file from outside Unity does **not** reliably trigger a recompile: `recompile` reported
"No scripts needed recompilation" and the test run silently used the stale assembly, returning the previous
cycle's numbers verbatim. Twice. The giveaway was the test COUNT - 340 when it should have been 339 - and the
output being byte-identical. The fix is the `Assets/Refresh` menu item via the `menu` tool, after which the run
picked the change up immediately. **Check the test count, not just the pass count, after adding or editing a
test file**, or a stale run will be read as a result.

## Cycle A11 - T2 answered by building the trench instead of hunting for one

**Target: T2. Outcome: T2 3 -> 6. No change to the game. Tree green.**

Cycles A3 and A4 both stalled on the same thing: the map's trenches are 3 m wide, a Pincer spans 3.6 m, so it
steps over them without having to decide anything - and A4 then found that navigation marks wider broken ground
impassable, so a walker is never routed to a harder gap either. T2 looked untestable.

It was not. **The gait does not consult navigation** - it plants feet against a ground function - so the gap can
simply be written. A flat-bottomed slot 1.8 m deep, at six widths, with the fraction of planted leg-ticks whose
anchor falls inside it:

| gap | 2 m | 3 m | 4 m | 6 m | 8 m | 12 m |
|---|---|---|---|---|---|---|
| Pincer | 0.0% | 2.3% | 9.3% | 12.3% | 21.6% | 40.1% |
| Redoubt | 0.0% | 0.0% | 2.3% | 2.3% | 18.5% | 46.3% |

**The response is graded and it is the right shape.** Up to about its own span the machine straddles; past that
it starts placing feet on the floor of the trench, rising smoothly to about 40-46% of feet inside a 12 m gap,
which is simply what walking through a trench too wide to step over looks like. Nothing falls in, nothing
refuses to move, nothing snaps between the two behaviours.

An incidental corroboration of cycle A7: crossing the 2 m slot, the Redoubt's ride height **rises** to 2.30 while
centred over the gap, against a low of 1.61 elsewhere in the same run. That is the belly lift reading the rim
under the hull and holding the body up over the hole - the behaviour A7 added and could not demonstrate, showing
up unasked in a different test.

Scores 6 rather than 8 because it is numbers, not pictures. The half-in, half-out posture over an 8-12 m gap may
well be the correct mechanical answer and still look wrong, and nobody has looked at it.

### The tooling trap from A10 bit again, twice, and cost four runs

A10 recorded that an external `.cs` edit does not reliably recompile. This cycle it happened twice more:

1. `Assets/Refresh` alone is not enough - the test run can start before compilation finishes, and returns the
   previous assembly's results with a plausible-looking pass count.
2. When the new file had a **compile error**, the run did not fail. It silently ran the stale assembly and
   reported 339/339 green. The error (`VehicleKinematics` is not in `TW.Sim.Nav` - I guessed an API) only
   surfaced from an explicit `recompile`, which answered "Scripts still have compile errors; nothing was
   recompiled."

**The reliable sequence is: edit, `Assets/Refresh`, `recompile`, check it says completed and NOT failed, then
run, then confirm the test you added is actually present in the results by name.** A pass count alone will lie to
you, and so will a green run on a file that does not compile.

## Cycle A12 - the terrain fix for T1 is now validated, and one small real fault found

**Target: T1. Outcome: T1 2 -> 3. No change applied. Tree green at 339/339 with the probe, 338 without.**

### Closing a hole in cycle A3's reasoning

A3 established that sandbags are props at `ground - 0.04f` and contribute nothing to `RenderGround.Sample`, so
the gait cannot see a parapet. That is correct and it has kept T1 pinned. But it rests on an assumption nobody
had tested: **that the solver would step onto a parapet correctly if the ground told it one was there.** If that
were false, the terrain work would be wasted.

A11 showed a ground function can be written to order, so this put a real berm in front of two machines:

| berm | planted ON it | planted INSIDE the ground | swing cut through the ground |
|---|---|---|---|
| Pincer, 2 m x 0.5 m | 15.6% | **0.0%** (worst 0.00 m) | 0.3% (worst 0.17 m) |
| Pincer, 2 m x 1.0 m | 16.5% | **0.0%** | 0.1% (worst 0.71 m) |
| Pincer, 3 m x 1.8 m | 24.3% | **0.0%** | 0.4% (worst 0.76 m) |
| Censer, 2 m x 0.5 m | 13.0% | **0.0%** | **0.0%** |
| Censer, 2 m x 1.0 m | 15.6% | **0.0%** | **0.0%** |
| Censer, 3 m x 1.8 m | 28.6% | **0.0%** | **0.0%** |

**Not one foot, on either machine, at any berm size, is ever planted inside the ground.** Every planted foot rests
exactly on the surface it is standing on, and the share of feet up on the parapet rises with the parapet's size,
which is what mounting one should look like. The solver handles obstacles correctly the moment it is told they
exist. **Giving the bags, revetment and duckboards real height in the sampler is therefore worth doing**, and
that is a stronger statement than A3 could make.

### The one real fault this turned up

**Pincer's swing clips the berm.** On 0.1-0.4% of swing-ticks the travelling foot passes below the surface it is
crossing, worst case **0.76 m** through the 1.8 m berm. Censer never does it - 0.0% at every size. So `ArcFor`,
which raises the step arc by `high - basis + Clearance` with `Clearance = 0.20 * VehicleSize.Walker = 0.50 m`,
is usually enough and occasionally is not, and only on the six-legged machine.

**Not fixed this cycle, deliberately.** Raising `Clearance` is a one-constant change and would very likely clear
it, but it lifts every step on every machine to fix an event that happens on 0.4% of swing-ticks of one machine,
and higher steps read as mincing. The honest options are to raise the clearance only for the machine or the case
that needs it, or to leave a 0.76 m clip that nobody has yet seen on screen. That wants a look at a picture
before a constant is moved, which is the next cycle's job.

### Note on scoring

T1 goes to 3, not higher. The line scores the shipped machine, and the shipped sampler still has no parapet in
it, so a foot in the shipped game still plants inside the bags. What changed is that the remedy is validated
rather than assumed.

## Cycle A13 - no result: the editor went away mid-cycle

**Target: T3. Outcome: nothing measured. No change to the game. Tree clean.**

T3 was the last unscored line and cycles A11 and A12 had unblocked the method for it: navigation never routes a
walker into a trench, but the gait consults a ground function, so the trench can be written. A11 had already
shown that past about its own span a machine starts placing feet on the floor of a gap (Pincer 21.6% of planted
leg-ticks at 8 m, 40.1% at 12 m); what it did not ask is whether those feet ARRIVE properly. That is exactly T3 -
of the feet that go down into the trench, do they rest on the floor, hover above it, or sink through it - and the
probe to answer it was written (`RevetmentProbe`, kept in the scratchpad).

It never ran. The editor did not pick the new file up: `Assets/Refresh` followed by `recompile` answered
"No scripts needed recompilation" twice, a test run returned the stale assembly (338 tests, probe absent by
name - caught by the A11 check rather than trusted), and a second run timed out after six minutes. The Unity MCP
connection then dropped and `editor_lock.py` reports FREE with no lockfile: **the editor had shut down.** The
probe file has been removed from `Assets/` so the tree is left clean.

**Nothing is claimed from this cycle.** The probe is written and the question is well posed; it wants an editor.

### The toolchain is now the loop's main cost, and that is worth stating

Cycles A10, A11, A12 and A13 each lost runs to the same thing: an external edit to a `.cs` file does not reliably
reach Unity. The failure is quiet in three different ways - `recompile` reports "up to date" when it has not
compiled the new file, a run against a stale assembly returns a plausible green with the previous cycle's
numbers, and a file with a compile error returns green too because the old assembly still runs. The check that
catches all three is the same: **confirm the test you added is present in the results BY NAME, and confirm the
test count changed.** A pass count is not evidence.

## Cycle A14 - T3 measured at last, and the critique found the fault my instrument was hiding

**Target: T3. Outcome: T3 0 -> 2, the last unscored Walk/Terrain line. No change applied to the game.
Tree clean. Nothing committed.**

The editor was closed at the start of this cycle, so there were no captures - but a closed editor is exactly what
batch mode needs, and A13's probe finally ran. Confirmed by name in the results, 1 test, no compile errors.

| | anchor off the trench floor | worst toe-to-anchor | leg drawn |
|---|---|---|---|
| Pincer, 8 m x 1.2 m | **0.0%** (worst 0.00 m) | **1.165 m** | up to 118% of span |
| Pincer, 8 m x 1.8 m | 0.0% | 0.120 m | up to 118% |
| Pincer, 12 m x 1.8 m | 0.0% | 0.120 m | up to 118% |
| Pincer, 12 m x 2.5 m | 0.0% | 0.654 m | up to 118% |
| Redoubt, all four | 0.0% | **0.000 m** | 94-97% |

**The good half is real and it is worth having.** Every foot that goes down into a trench lands exactly on its
floor, on both machines, at every size. Nothing hovers and nothing sinks. That is the half of T3 that is about
finding the ground.

**The bad half is that Pincer cannot be DRAWN where its foot is.** On 29.3% of in-trench plant-ticks in the
shallow slot the toe is more than 5 cm from the foot it is standing on, worst case 1.165 m.

### The number I did not believe, and the critique that was right to reject my reason

Pincer is far worse in the SHALLOW 1.2 m trench (29.3%, worst 1.165 m) than in the deeper 1.8 m one (0.7%,
0.120 m). Non-monotonic in depth, so I put it to the critique as a suspected artefact of my own probe. **The
critique cleared the probe's arithmetic and then found a real defect in it that I had not suspected, which is the
more useful outcome.**

It cleared, with lines cited: `At == Anchor` on every tick counted (`WalkerGait.cs:266-271`, and the post-`Carry`
fix-up skips planted feet at `:386`); no swing-boundary leak, because the landing tick re-grounds the new anchor
in the same tick (`:252-256` then `:266`, not an `else`); and the drawn-chain algebra is exact rather than
approximate, because `Solve` forces every link past the first onto one direction (`:758-759`), which the
flat-ground result of 0.000 m toe error independently confirms.

**The defect: the probe counts only one of the two clamps.** A nonzero toe gap is only possible when a clamp
bites, and there are two. The long one is `StretchMax` (`:738`, `:773`) - a leg asked to reach further than
1.18x its span. The short one is the minimum fold: a one-piece leg is floored at `StretchMin` (`:773`), but a
**jointed** leg has no `StretchMin` at all - `reach` is clamped to `|upper - lower| + 1e-3` at `:740`, `alpha`
collapses, and the toe is drawn at `|upper - lower|` along the target direction, which can be 50-70% of span.
My only extension counter was `drawn > 1.17` and my only summary was a `Max`, so **the entire short tail was
invisible in both statistics.**

> **CORRECTED IN CYCLE A15, and the correction matters.** The paragraph above is right that the probe was
> blind to the short tail, and WRONG about which clamp produced it. **Pincer's legs are built in ONE PIECE**
> - not an inference from comments but from the tooling: `crabsplit.py:170-207` (`build_pincer`) emits only
> `Leg_L1..n`/`Leg_R1..n`, never a `Thigh_`/`Shin_`/`Foot_`, and parents every one straight to `Body`, so
> `TankModel.BuildRigs:202` builds a chain of exactly one member on all six legs. A one-piece leg takes the
> `n == 1` branch of `Solve`, which **does** have a short floor: `stretch = Clamp(dist / Bone[0], StretchMin,
> StretchMax)`. So the jointed no-`StretchMin` path cannot be what produced Pincer's 1.165 m, and the
> obvious code response to A14 - add a `StretchMin` to the jointed branch - **would have been a no-op on the
> machine that failed.** The claim was wrong twice over: a jointed leg is not unbounded either, because
> `reach` is clamped to `|upper - lower| + 1e-3` against `MinSpan`'s `|upper - lower| + 0.02`, agreeing with
> `Carry`'s bound to within 19 mm. And the polarity is backwards - a one-piece leg's short limit is a hard
> 80% of span while a jointed leg's is 50-70%, so **the one-piece machine is the most exposed, not the
> exempt one.** What survives, and is now proved rather than inferred: because `Solve`'s one-piece floor and
> `Carry`'s `MinSpan` are literally the same number (`Bone[0] == |Rest| == Reach` for a single-part chain,
> `BuildRigs:228-230`), the ONLY way a planted Pincer leg can show a short gap is for `floor` to have been
> violated. The 1.165 m gap puts the hip 1.53 m from its foot against a 2.70 m fold floor - 1.17 m inside
> the fold. **One genuinely new hazard, unscored: different legs on the SAME machine can have different
> chain lengths.** `crabsplit.build_loose:328-343` names only `group[:3]` per shell quadrant, so a quadrant
> holding one piece yields a bare `Thigh_*` (chain 1, an 80% floor) beside a quadrant holding three (chain
> 3, a floor possibly ~10% of span). `Carry`'s `floor` is a `Max` across all legs, so on Pavise or Banner a
> single one-piece leg dictates the fold floor for the whole machine. Nobody has measured that. `detached 29.3%` against `at the clamp 3.1%` is therefore not a mystery: it is
arithmetic proof that about 26 of those 29 points are legs folded TOO SHORT - hips too CLOSE to the planted foot
to be drawn there.

### The mechanism, and why 1.2 m is worse than 1.8 m

It is not monotonic because **it brackets a null.** Any step aimed into a slot lands in the collapsed branch of
the target clamp: `drop` at `:333` already exceeds `MaxSpan * StepSafety` for a 3.6 m leg at stance once any
depth is added, so `:338` tucks the target to nearly straight down and vertical distance alone then decides
everything. `Carry` then puts the hips too low for that, because `Height` may fall to `means - MaxSink` while the
feet in the slot drag `meanY` down. At 1.2 m the hip sits inside the leg's minimum fold - **the short tail.** Add
0.6 m and the same distance returns to the legal band - **1.8 m is the null**, 0.4-0.7%. Add another 0.7 m and the
OTHER tail opens, which is why 12 m x 2.5 m reports 3.9% detached against 4.0% over-extended, the two figures
agreeing because that regime really is pure over-extension. Three depths, three regimes. Redoubt escapes all
three because its longer span keeps every in-slot demand inside the band.

### The proximate cause is an ordering bug, and it is NOT fixed this cycle

`floor` is the fold bound that exists precisely to stop the hips getting inside a leg's minimum fold (`:550`),
and it is applied at `:555` and `:599`. **But `Height = Mathf.Clamp(Height, means - MaxSink, means)` runs on the
very next line (`:561`, `:600`) and is the last word, so the fold floor is computed and then silently discarded
whenever it disagrees with the stance band.** Verified by reading both sites, not taken on the critique's word.

The candidate fix is a statement reorder - `Height = Max(Clamp(height, means - MaxSink, means), floor)` - not a
constant and not new structure. **Deliberately not applied.** Letting `floor` win means the body may ride above
`means`, and the comment block at `:562-578` documents exactly why that is dangerous: every step target is chosen
against the lower body, so raising it invalidates targets that were correct, which is how a foot came to be
planted 1.73 m from a 1.62 m leg. Applied blind, the reorder may simply trade a 29.3% short tail for a long one.
`MaxSink` is the tempting constant and it is the wrong lever - raising it deepens the hole, lowering it brings
back the belly-ploughing that same comment records.

**So the next cycle's job is the measurement, not the fix:** split the extension counter into both tails and
report the signed `dist / MaxSpan` distribution with which bound bit. That change is written into the probe
already (`WalkerGait.StretchMax` and `WalkerGait.MinSpan` are both public, so the probe can use the shipped
bounds rather than restate them) and **it has not been run** - the editor came back mid-cycle and batch mode
refused with "another Unity instance is running with this project open", against an interactive editor holding
1.4 GB. Nothing is claimed from that run. The probe is withdrawn from `Assets/` and kept in the scratchpad.

### Two housekeeping findings worth the owner's eye

1. **`WalkerGait.cs` is untracked in git** - not ignored, never added. The whole procedural gait system, including
   A7's belly lift and A8's turn lean, exists only as a working-tree file with no baseline to diff or revert to.
   `TankModel.cs` and `TankRenderer.cs` are tracked and modified; `WalkerGait.cs` is not tracked at all.
2. My own `WalkerGait.cs.before`, `.beforeA7` and `.beforeA8` backups were sitting **inside `Assets/`**, where
   Unity had generated `.meta` files for them. Moved to the scratchpad and removed. A `TankRenderer.cs.before`
   remains and has been left alone - it is not certainly mine.

## Cycle A15 - no editor, so the board was audited instead: two prior claims corrected, one of them mine

**Target: W6. Outcome: W6 4 -> 3 (a score REMOVED). No change applied - an interactive editor holding 1.5 GB
had the project, so no captures, no batch run, and no write into `Assets/`. Tree untouched.**

A cycle with no instrument is a cycle for auditing what is already written, and two things on this board turn out
not to survive reading the source carefully.

### 1. Cycle A14's mechanism was misattributed - corrected in place above

A14 blamed Pincer's detached toe on the jointed-leg branch of `Solve` having no `StretchMin`. **Pincer's legs are
one piece**, established from `crabsplit.py`'s `build_pincer` rather than from comments, so it takes the branch
that DOES have a short floor. The full correction is quoted in the A14 section. The number and the proximate cause
both stand; what changes is that the tempting one-line response to A14 would have done nothing at all to the
machine that failed, and that the polarity was backwards - **a one-piece leg has the TIGHTEST short limit of any
leg on any machine**, so Pincer is the most exposed rather than the exempt one.

### 2. Cycle A7's belly lift - my own applied change - CANNOT LIFT. Verified by reading the order.

This is the more important finding and it is against my own work. A7 added a bounded belly vote so the hull would
stop ploughing a parapet, and it computes exactly what it says: `belly = Min(under + Clearance, means + MaxSink)`,
deliberately allowed up to `MaxSink` ABOVE the stance, then `if (belly > height) height = belly;` (`:588-590`).

**And then `Height = Mathf.Clamp(Height, means - MaxSink, means)` at `:600` caps it back to `means`.**

So the lift above the stance is computed, bounded, argued for in fourteen lines of comment - and then deleted
every frame. What A7 actually delivers is the sink-cancelling half: it can raise the body from below `means` up to
`means`, and no further. The parapet case it was written for is precisely the case that needs to exceed `means`,
because `means` is built from the feet already planted and a bank the feet have not reached yet cannot raise it.
**A7 is half a fix and the board previously recorded it as a whole one.** A11's incidental observation (a Redoubt
rising to 2.30 while centred over a 2 m gap) is still a true observation and is consistent with sink cancellation;
the attribution "holding the body up over the hole" above its stance is what is wrong.

### 3. The same last-word clamp also discards `ceiling`, which is the 118% figure

`if (ceiling < MaxValue) Height = Min(Height, ceiling);` then `Height = Clamp(Height, means - MaxSink, means)`.
Whenever `ceiling < means - MaxSink` the REACH bound is thrown away too and the body is held above what the
planted legs can reach. That is the mechanism of Pincer's long tail at 118% of span, and it was in the blind spot
of both clamp reorderings proposed so far. A cosmetic band is being applied after, and therefore above, two
geometric bounds.

### 4. The constant, not the statement order - and the arithmetic says so before any test

I proposed a safer reorder than A14's (raise the lower bound, `Clamp(height, Max(means - MaxSink, floor), means)`,
so `means` stays a hard ceiling). **The critique broke it on three counts and it is not worth applying.** `means`
is not an invariant - A7's belly block is an explicit documented path above it, twenty lines from the comment that
asserts the ceiling. `Mathf.Clamp` with `min > max` does not throw but returns `min`, so my form rides above
`means` anyway in exactly the case that distinguishes it, and is DISCONTINUOUS at `value == floor` - a pop of up
to `floor - means` as the smoothed height crosses it. And when `floor <= means` my form and A14's are the same
function, so the whole argument only has content in the regime where mine misbehaves.

**The deciding arithmetic, re-derived independently here rather than taken on trust:** at stance,
`drop = MaxSpan * StepSafety * sqrt(1 - Lean^2) = 0.97 * 0.8930 = 0.8662 * S`, and with `homeFlat = 0.2183 * S`,
`Rise(MinSpan, homeFlat) = sqrt(0.64 - 0.0477) * S = 0.7697 * S`. So the entire fold margin is
**`means - floor = 0.0965 * S`** - about 0.325 m on Pincer. Against it, `MaxSink = 0.30 * VehicleSize.Walker =
0.75 m = 0.2226 * S`. **`MaxSink` exceeds the whole fold margin by 2.3x on Pincer, and on every machine:**
Banner 0.319*S (3.3x), Censer 0.253, Pincer 0.222, Pavise 0.199, Kettle 0.169, Redoubt 0.115.

`means - MaxSink < floor` is therefore the NORMAL state on all six machines, not an edge case - flat ground only
looks clean because nothing drives `Height` to the bottom of the band there. **So no ordering of the two clamps
is safe, and the first change is the constant rather than the statement order:** make `MaxSink` a share of the
machine's own span, below its fold margin (about `0.09 * MaxSpan` of the leg that sets `Stand`), instead of an
absolute 0.75 m for every machine. That is a constant, it is per-machine rig data rather than structure, and it
is the cheapest thing on the list. It is NOT applied because it cannot be gated: the gait tests need an editor.

### 5. One hazard nobody has scored: the far LOD is drawn against the wrong leg length

`TankRenderer.Pose:1216` calls `Solve` with **`Lods[1]`**, whose rigs are built independently from the decimated
FBX (`crabsplit` decimate 0.34 on Pincer) with `useSockets: false`, so its toe is the lowest vertex of a
DECIMATED mesh. LOD1's `Reach`, and hence its `Bone[0]`, therefore differs from LOD0's - while the feet and the
ride height were solved against LOD0. The far-LOD leg is drawn against the wrong length with only +/-18% of
rubber to hide the difference. **Every figure on this board is LOD0.**

### Standing instruction for the next cycle with an editor

Two numbers decide everything above, and A14's probe is already written to take them:
1. Per machine on flat ground at stance, `means - floor` per leg against that machine's `MaxSink`. If `MaxSink`
   exceeds the margin the clamps are guaranteed to disagree and the constant is the fix.
2. The fraction of planted-leg ticks where `floor > means`, and the maximum of `floor - means`. **If that count
   is zero the two proposed clamp forms are the identical function and the argument is moot.**

## Cycle A16 - the six machines do not have the same legs, and three are not the same end to end

**Still no editor** (the interactive one held the project throughout), so again no captures and no change applied.
But the outstanding hazard from A15 - that legs on one machine might have different chain lengths - turns out to
be answerable with no editor at all, because the part names are in the shipped FBXs. `grep -a` reads them.

| machine | leg parts found in `<name>_LOD0.fbx` | chain per leg | uniform? |
|---|---|---|---|
| **Pincer** | `Leg_L1 L2 L3 R1 R2 R3` | **1** (one piece), six legs | yes |
| **Censer** | `Thigh_LB LF RB RF` | **1** (one piece), four legs | yes |
| **Kettle** | `Thigh/Shin/Foot_LB LF RB RF` | **3**, four legs | yes |
| **Pavise** | `Thigh_LB LF RB RF` + `Shin_LB RB` | **front 1, rear 2** | **NO** |
| **Banner** | `Thigh_LB LF RB RF` + `Shin_LF RF` | **front 2, rear 1** | **NO** |
| **Redoubt** | `Thigh/Shin_LB LF RB RF` + `Foot_LF RF` | **front 3, rear 2** | **NO** |

Four things follow, and none of them needed a running editor.

### 1. Half the fleet has one-piece legs, not just Pincer

**Censer's legs are one piece too** - four quadrants, one `Thigh_` each, no `Shin_`, no `Foot_`. This was not known;
the source comments name only Pincer. A one-piece leg is floored at a hard `StretchMin = 80%` of span (`Solve`'s
`n == 1` branch), which A15 established is the TIGHTEST short limit any leg has, so **Censer is as exposed to the
fold violation as Pincer is** and has never been tested for it. It also retires a loose end from cycle A6, which
measured "Censer at 109% of span" and could not say why a leg would be drawn past full stretch: a one-piece leg
has nothing to fold, so on any ground that moves the hip it has only the +/-18% of rubber and the clamp bites.

### 2. Three machines are asymmetric front to back, and that explains Banner's spread

`Carry` takes `floor` as a **`Max` across every planted leg**, so the leg that folds least dictates the ride
height for the whole machine. On Pavise, Banner and Redoubt that leg is at one END of the machine, so the fold
floor is set by the front or the rear alone. A10 measured Banner at **68-97%, a 29-point spread between its own
legs**, recorded it as "the remaining fault" and offered no cause. **This is the cause**: Banner's front legs are
two-piece and its rear legs one-piece, so they are working to different minimum folds against one shared body
height. The asymmetry is front/back and mirrored left/right, so it would not read as a limp on one side - it would
read as the front and back of the machine behaving like different animals.

### 3. The welded-leftover hazard can NOT be settled this way, and that is worth saying plainly

`build_loose:338-345` names only `group[:3]` of each quadrant (`Thigh`, `Shin`, `Foot`) and appends `group[3:]` to
`core`, which is joined into **`Body`**. The sort is `-centre(b).z`, top down, so the pieces that would be welded
to the static hull are the LOWEST ones - the toe end. On the shipped machines no quadrant appears to have produced
more than three named pieces, but **a fourth piece would be invisible in the names by construction**, since it is
absorbed into `Body` without ever being named. So this evidence cannot prove or disprove it either way, and I am
not claiming it does. It wants a face count on `Body` against the sum of the limbs, which wants an editor.

### 4. The LOD1 hazard from A15 is narrower than it looked

Every machine's LOD1 has **exactly the same leg part names** as its LOD0, so the chain topology is identical and
`Solve` is not being handed a different skeleton at distance. What remains is a METRIC mismatch only: LOD1's
`Reach` and `Bone[0]` come from a decimated mesh (`decimate` 0.34 on Pincer, 0.70 on Kettle, 0.60 on Banner,
0.55 Censer, 0.45 Redoubt, 0.36 Pavise), so the lowest vertex - and hence the toe - sits somewhere slightly
different. Real, but bounded by how far decimation moves one vertex, and no longer a structural worry.

### What this does to the fix list

A15's conclusion was that `MaxSink` should become a share of each machine's own span rather than a flat 0.75 m.
**A16 sharpens it: it cannot be one share per machine either.** On Pavise, Banner and Redoubt the fold margin
differs between the front legs and the rear legs, because their chains differ, so a single per-machine constant
is still the wrong shape - the bound wants to come from `MinSpan` of the leg that actually sets `floor`, which is
already computed per leg a few lines above. That is a smaller change than A15 proposed and a better-aimed one.
Still not applied: it cannot be gated without an editor.

## Cycle A17 - Redoubt's legs are numbered twice, differently, and the damage system uses the other numbering

**Outcome: D1 scored 3 - the first Damage line on the board to be scored. No change applied; this one needs a
decision, not a constant.** Still no editor; this is entirely from source and from the shipped FBXs.

A16 found that Redoubt's FBX contains four `Thigh_` parts. The roster says otherwise, in two places:
`RosterEntry:58` calls it "a blockhouse on **six legs**" and `TankSpec:148` says "the thickest plate on the field
and **six legs** to carry it". `VehicleKinematics` agrees with the prose and not with the mesh: Redoubt's profile
carries **`Legs = 6`**.

Both numberings are then used, by different systems, and nothing reconciles them.

| | where legs are numbered | Redoubt's perSide | left legs | right legs |
|---|---|---|---|---|
| damage | `VehicleModules:262`, from `prof.Legs / 2` | **3** | bits 0, 1, 2 | bits **3, 4, 5** |
| renderer | `TankModel.NumberLegs:166-168`, from the parts that exist | **2** | legs 0, 1 | legs **2, 3** |

`WalkerGait:244` reads `off = (lost & (1 << i)) != 0` where `i` is the MODEL's leg index. So on Redoubt:

- the sim's **third left leg** (bit 2) lames the model's **first right leg**;
- the sim's right legs are bits 3, 4, 5, of which the model only ever reads bit 3 - so **two of Redoubt's six legs
  can be destroyed with no visible effect at all**;
- destroy Redoubt's entire right side and **exactly one leg visibly fails**.

It does not crash or read out of range - `Feet` is sized from the model's rigs, so the high bits are silently
ignored. It is wrong quietly, which is why 51 cycles of looking at pictures never caught it.

The other five machines are fine, and it is worth saying why rather than assuming: Pincer's profile says `Legs = 6`
and its FBX has six one-piece legs (`Leg_L1..3`, `Leg_R1..3`), so both sides compute `perSide 3` and agree
exactly. Kettle, Censer, Pavise and Banner all declare `Legs = 4` and all have four thighs, so both compute
`perSide 2`. **Redoubt is the only machine where the mesh and the profile disagree.**

### This is an owner decision and deliberately not fixed

Two ways to reconcile it and they are not equivalent:

1. **Redoubt is a four-legged machine.** Then `Legs = 6` in its profile is wrong, and so are both comments. The fix
   is one constant - but it is a GAMEPLAY constant: `want = perSide - ceil(after * perSide)` drives how many legs
   a hit takes and when a side fails, so changing 6 to 4 changes how hard Redoubt is to lame. That is balance, and
   balance is not mine to move.
2. **Redoubt is a six-legged machine and the model lost two.** Plausible, and there is a mechanism for it:
   `crabsplit.build_loose` groups limbs into four SHELL QUADRANTS and names only `group[:3]` of each, appending
   `group[3:]` to `core`, which is joined into **`Body`**. Six legs over four quadrants means two quadrants hold
   two legs each, and the surplus pieces - sorted `-z`, so the LOWEST ones - are welded into the static hull. That
   would also explain the earlier finding that Redoubt's authored `Socket_Toe_` sat **2.15 m below its mesh**,
   which is what a chain assembled from pieces of two different legs would look like. **This is a lead, not a
   result:** a welded piece is invisible in the part names by construction, so nothing here proves it. It wants a
   face count on `Body` against the sum of the limbs, which wants an editor.

If (2) is what happened, then Redoubt's *rendered* legs may also be chimeras - a thigh from one leg above a shin
from its neighbour - and every Redoubt number on this board (including A14's clean 0.000 m toe error and A12's
0.0% penetration) would be measuring a self-consistent solver driving an incoherent skeleton. **That is the single
most valuable thing left to check on this board**, and it is cheap once an editor exists: load Redoubt, print each
leg's `Chain` with the part names and `Bone` lengths, and see whether the pieces of a leg are actually stacked
above one another.

## Cycle A18 - the ground function is not what this board has said it was for fifteen cycles

**Target: T3. Outcome: T3 capped at 2 with the reason stated, T5 5 -> 4, and TWO long-standing board findings
overturned.** Still no editor (the interactive one held 2 GB throughout), so source only, and every claim below was
re-verified against the files rather than taken from the critique that produced it.

I took a chain of reasoning to the critique expecting it to confirm a cap. It broke three of its four links, and
two of the breaks invalidate conclusions this board has been resting on since cycles A3 and A4.

### 1. OVERTURNED: the gait does not plant against the sim heightfield, and the parapet IS in its function

Cycle A3's finding - the basis of T1's cap for fifteen cycles - was that `RenderGround.Sample` is "the heightfield
plus crater deformation and nothing else", so the parapet contributes zero height. **That is wrong.**
`Grid.Sample` (`RenderGround.cs:13-22`) does not add to the heightfield, it **REPLACES** it: `original` is passed
as `fallback` and is returned only when `Heights.IsCreated` is false. Verified by reading the function.

That grid is filled from the DRAWN mesh (`GreyboxTerrainView.FillRow:198-201`), off `BattlefieldSurface`, which
includes `BankRise` - up to about 1.05 m of parapet bank - plus `Mound`, `Rill`, the crater ejecta lip up to
0.6 m, and a trench-lip blend, on a 0.5 m lattice interpolated along the terrain's own triangle diagonal.

**So the parapet bank DOES contribute height to the function that places feet.** Only the sandbag PROPS on top of
it do not. T1's cap reason as written is overstated and T1's score of 3 now rests on a false premise - it is not
obviously too low or too high, it is **unmeasured against what the function actually returns**, and it should be
re-measured before it is moved either way. Cycle A12's separate result stands untouched, because A12 wrote its own
ground function and never depended on this.

### 2. OVERTURNED: craters are NOT impassable, and walkers are authored to walk into trenches and shell holes

Cycle A4 concluded that broken ground is `navCost 255` - impassable - so a walker is never routed anywhere that
would test T3 or T5. That has excused both lines ever since. **The source says otherwise.** In
`Sim/Terrain/NavLayer.cs`, `Crater` costs **2** (`:24, :34`), and `Blocked = 255` (`:26`) is checked FIRST (`:30`)
and covers only "bunker wall, deep water, map edge" (`:11`). A4 measured 255 at one real crater and generalised -
that crater was **flooded**, and it was the water that blocked it, not the hole.

And the sim does not merely permit it, it **budgets for it**: `VehicleKinematics:118` declares
`StepOverSpeed = 0.72, WadeSpeed = 0.78, PickSpeed = 0.88`, and `:402` applies `StepOverSpeed` in a `Trench` cell,
`WadeSpeed` in `Mud` and `PickSpeed` in a `Crater`. **Somebody authored a speed for a walker picking its way
through a shell hole and stepping over a trench.** T3 and T5 are not unexercised because navigation forbids them.

### 3. The relocation I proposed is REJECTED, and A14's slot numbers do not map to the real game

I proposed that A14's alarming 29.3% detachment belonged to T5 rather than T3, on the grounds that the real trench
is 1.8 m deep and 1.8 m was A14's clean case. **Three breaks, all fair.**

- The real trench is not 3 m x 1.8 m. `WidthMeters = 3f` is metadata that nothing carves from; the carve loop runs
  `cz = from..to` **inclusive** - 2 nav cells, about **4 m** - and the fire step then raises **half those rows** by
  `+0.7`, so **half the trench floor sits at -1.1 m**, not -1.8. The -1.8 plateau is a single ~1 m strip.
- With the parapet bank now known to be in the function (finding 1), the step a foot crosses at a trench reaches
  about **2.85 m** - past the long-tail regime - while the fire-step row at -1.1 m sits **inside the short-tail
  band**. My single "1.8 m" corresponds to no part of the real profile.
- Craters cannot make a trench worse (`CraterStamp.TrenchKeep/TrenchGuard` zeroes carves near trenches), but a
  crater is a BOWL with an ejecta ring, not a flat slot, and `CraterStamp.MaxDepth = 4.5` with `DepthShare = 1.2`
  reaches nearly twice the deepest case A14 measured. **Three depths are three samples of an unmeasured curve, and
  a null is the least defensible thing to extrapolate from three samples.** That criticism is correct and it
  applies to how A14 was written up here.

### 4. A genuine `WalkerGait` bug, and it is the only thing in T1/T3/T5 the gait alone can fix

`ArcFor` (`:444-457`) samples the ground at exactly **three** interior points, `t = 0.25, 0.5, 0.75`. A parapet
crest about a metre wide, or a crater's ejecta ring, can fall **entirely between two samples**, so the swing arc is
never raised to clear it and the foot passes through. **This is almost certainly what cycle A12 measured** and
could not explain: Pincer's swing clipping a berm on 0.1-0.4% of swing-ticks, worst 0.76 m, while Censer never did
it. A12 wondered whether to raise `Clearance`; raising a clearance does nothing about an obstacle the sampler never
saw. Not applied - it needs an editor to gate.

### 5. T5 goes down to 4, on a constant whose own comment gives it away

`GreyboxTerrainView:201` stores `Mathf.Max(bed, top - 0.42f)` into the single grid that men, debris **and the
walker gait** all read, commented "men and debris stand on the bed of a flooded hole, but never deeper than the
knee". On dry ground `bed == top` and the term does nothing. **In a flooded shell hole it puts the planted surface
up to 0.42 m above the bed** - so a walker's feet hover, in a shell hole, by construction, from a constant authored
for a man's knee. T5's own words are "rather than hovering over them". Credit where it is due: mounds, rills and
the ejecta lip ARE in the gait's function, so feet do find dry broken ground - which is why this is 4 and not
lower.

### The single highest-value fix, ready for the next editor

**Split the wading fudge.** Give `RenderGround` a bed channel, or make the 0.42 a consumer-supplied constant, and
have `TankRenderer.Ground` (`:309`) sample the bed rather than the knee-deep wading surface. One constant, one
channel, no structural change, and it is the only fault in scope that matches a board line's own wording. It sits
ahead of the parked `MaxSink` work because it is smaller and because its correctness does not depend on any of the
clamp argument.

**Do not spend another cycle on duckboards.** T3 is capped by the prop architecture and wants a rewrite, not code.

## Cycle A19 - A10's central W6 table was measuring two different things, and my re-reading of it was half wrong too

**Target: W6. Outcome: W6 stays 3, D1 3 -> 2. No change applied - fifth consecutive cycle with an interactive
editor holding the project.** Source and arithmetic only, all of it re-verified against the files.

### What was genuinely wrong with A10

A10 measured the drawn hip-to-toe distance as a fraction of span and tabulated it across all six machines, ending
"legs are drawn SHORT, never long - up to 32% short on Banner". **That table compares two incomparable things.**

`Solve` treats a one-piece leg and a jointed leg completely differently. For a **one-piece** leg (`:771-782`) it
applies `Matrix4x4.Scale(new Vector3(1, 1, stretch))` about the hip, along the hip-to-toe axis - an **anisotropic
deformation of the drawn mesh** - and because `Bone[0] == Reach == MaxSpan` for a single-part chain (verified
through `TankModel:225-232`, `:240` and `WalkerGait:680` - the same float, not three definitions that resemble each
other), **the "drawn % of span" figure IS the squash factor.** For a **jointed** leg the knee takes up the slack
and nothing deforms. So 75% on Kettle is a fold and costs nothing; 88% on Censer is a 12% squash of geometry.

With A16's topology, the table splits cleanly: Pincer and Censer are all one-piece, so every figure is deformation;
Kettle and Redoubt have no one-piece legs at all, so none of theirs is.

### The arithmetic that makes it worse, and needed no editor

At home stance the drawn fraction can be derived rather than measured. `Stand` puts the deciding hip at
`MaxSpan * 0.97 * sqrt(1 - 0.45^2) = 0.8662 * S` above the foot plane, and sweeping the drop ratio over its whole
range gives a drawn fraction of **0.878 to 0.893 - for any hip height.** So Pincer's and Censer's 11-12% squash is
not a transient or a terrain artefact: **it is the designed standing pose.** Every one-piece leg on the field is
permanently drawn about an eighth short, and Pincer's 88-97% band means all six of its legs also pulse 10 points in
length every gait cycle. Modulation is what an eye detects, not absolute error.

### Three corrections against my own re-reading

1. **The `>= 80%` inference only runs one way.** One-piece implies drawn `>= 80%`, so Banner's 68% must indeed be
   its jointed front pair. But the converse fails: a jointed leg's `MinSpan` is `|upper - lower| + 0.02`, which can
   sit far below `0.80 * MaxSpan`, so **an 80% reading is not evidence of a one-piece leg.** My attribution of
   Pavise's floor to its one-piece front pair was unjustified.
2. **80% is unreachable at home** (the sweep above bottoms out at 0.878), so Pavise's floor is a dynamic event, and
   the likelier mechanism is not `Solve`'s clamp at all: `Carry`'s `floor` **skips swinging legs**, so while both
   one-piece front legs are in the air the floor is set only by the jointed rears, whose `MinSpan` is tiny; the body
   sinks to `means - MaxSink` and the front legs come down onto a demand at or below 0.80. **A pin of `Carry`'s
   floor, not of `Solve`'s clamp** - consistent with a toe error of exactly 0.000, and it still produces a 20%
   squash.
3. **My framing discarded the worst W6 violation in the file.** Pincer's toe drawn 1.165 m short of its own planted
   foot is, verbatim, a leg "drawn short to reach its foot". Restricting W6 to one-piece machines on flat ground
   quietly dropped it.

### Four things nobody had charged to any line

1. **A detached leg POPS to full length.** `TankRenderer:770-785` rebuilds a blown-off part as
   `Matrix4x4.TRS(pos, rot, Vector3.one)`, discarding the stretch. A leg standing at 88% snaps to 100% the frame it
   comes off, on every one-piece machine. **D1 goes to 2 for this.** It also reads `d.World.rotation` off a
   non-uniformly-scaled matrix, which is ill-defined.
2. **A jointed leg reaching long is drawn FATTER, not longer.** `grow > 1` applies `Vector3.one * grow` to the hip
   part (`:770`) - a **uniform** scale, inherited down the chain, girth included. Up to 18%. That is a silhouette
   change and arguably more visible than an axial squash.
3. **A planted foot is never clamped to `[MinSpan, MaxSpan]`.** The post-`Carry` fix-up loop bounds swinging feet
   and the dying case, but a planted foot relies entirely on `Carry`'s ceiling and floor - the two bounds `:600`
   discards. That is the structural root of the 1.165 m detach, and it is one place, not several.
4. **`At.y = Mathf.Max(ground(...), ceilingY)` (`:552`) deliberately violates `MinSpan` in swing** whenever the
   ground under a lifted foot is HIGHER than the fold ceiling - a parapet lip, a trench's far lip, precisely the
   interesting terrain. Every figure on this board measured PLANTED legs and cannot see it.

### And a warning about the board's most-used control

**Redoubt is not a clean control and several results lean on it as one.** `TankModel:209-220` records that its
front toe sockets sit **2.15 m below the foot mesh** and are used deliberately. So Redoubt can score a perfect
analytic toe error - A14's 0.000 m, A12's 0.0% penetration - while its visible foot geometry hangs about two metres
clear of the ground the solver thinks it is standing on. Those results are still correct about the SOLVER. They are
not evidence about the machine. Relatedly, `:758-759` aims every bone from the second onward along one shared
direction, so a three-piece chain is drawn as **two** links: Kettle's shin and foot are always collinear and
Redoubt's foot never flattens. "Three-piece" buys less than A16 implied.

### On visibility, which is what the line actually asks

Committing rather than hedging, while noting there are still no frames: 7-12% (Censer and Pincer on the flat) is
below threshold as a static value and **marginal as a per-step pulse**; 20% is visible, the more so because on
Pavise it would be an intra-machine asymmetry - a squashed front pair beside a correctly folded rear pair in the
same frame, and asymmetry needs far less contrast to spot than absolute error; 1.165 m is unmissable. Worth noting
that the file's own defence, "invisible at a few per cent" (`:697`), was written about 3% and is now covering 20%.
Also `TankModel:288-291` records that these limbs are modelled DIAGONALLY, so the hip-to-toe scale axis is not the
mesh's principal axis and the squash runs partly across the girth, bending the silhouette rather than just
shortening it.

### The deciding number for the next editor

Per LEG, not per machine, on flat ground: `min`/`max` of the **pre-clamp** `dist / Bone[0]`, a count of ticks where
the clamp actually bit, and which `Carry` bound was active on those ticks. **The decider: ticks with pre-clamp
demand below 0.80 on Pavise's `Thigh_LF` / `Thigh_RF`.** Non-zero means `Solve`'s clamp is pinning, the toe detaches
on FLAT ground, and W6 drops to 2 with a named fix - restore the planted-foot `MinSpan` clamp that `:600` discards.
Zero, with the minimum sitting at exactly 0.8000, means the pin is `Carry`'s floor and the fix is to include
swinging legs' one-piece `MinSpan` in it.
