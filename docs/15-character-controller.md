# 15. Character controller: how a man moves, fights and reacts

> **2026-09-25:** built. The ladder is `Presentation/Core/AnimationController.cs`, the clip table
> `Editor/InfantryClipTable.cs`, the bake `Editor/VATBaker.cs` (2026-09-22 onward). Where this says "nothing is built
> yet", read it as the design the code followed.

Design of record for infantry animation, written against the owner's Mixamo download of 2026-09-22 (205 clips,
measured in `docs/reference/animation-clips.md`). Nothing here is built yet. It replaces the eighteen-row
`AnimRow` scheme of B3 with a clip table, a per-man controller and a VAT atlas that can play clips once, cross-fade
and run at speed.

## 1. What we are building, and the rules it lives under

A man on the field should look like he knows where he is: stooped in the trench, upright on the march, flat in a
crater, wading, edging along a wall, ducking a near miss, staggering when hit, going down the way he was shot,
climbing out over the parapet, dropping into the next trench, changing a magazine, throwing, stabbing. He should do
these things because the **sim** put him in that situation, never the other way round.

Rules the design keeps:

- **The sim is untouched by animation.** The controller is presentation only: it reads the pose stream and the
  event buffer, keeps its own per-man memory, and writes what the renderer draws. Replays and hashes do not change.
  Two sim additions are proposed (magazines and melee) because they change what men *do*; they are separate steps.
- **Deterministic sim, hashed placement.** Where the controller picks a variant it hashes slot and tick; it never
  uses `System.Random` or `Mathf` in the sim assembly.
- **3,000 men, GTX 1050, no min-spec PC.** One indirect draw per tier, no per-man GameObjects, the controller is a
  Burst job over flat arrays. The atlas budget is stated below and is unmeasured on the minimum card.
- **One figure.** All infantry share the 929-vertex Mixamo figure and one atlas; an archetype is a *clip table*
  (which clips it may use and how it prefers to fight), not a second mesh. Officer is a future table on the same rig.

## 2. The clips: what they are and how they fit

The manifest (`docs/reference/animation-clips.md`) measures every file: length, whether it loops, root motion left
in it, hips height (the stance), the busiest bones. From that the 205 clips sort into families. Verdicts:
**131 use, 28 spare, 46 ditch** (diagonals of the 8-way sets, duplicate sling clips, running long jumps, root-motion
single steps, exact duplicates).

| Family | Clips used | What the data says |
|---|---|---|
| Stand idle / aim / fidget | 10 | Four long "Rifle Idle" loops (2.8 to 10.6 s) with the head turning; two aimed idles; Check Shoe, Inspecting, Rubbing Eyes and a stretch as one-shot fidgets |
| Sling / unsling | 2 | `Rifle Pull Out` / `Rifle Put Away`; nine other grab/put-back clips are duplicates with 20 to 27 cm of drift |
| Fire | 12 | Aimed single shots (0.27 s and 1.17 s), **fire while walking** (three, hips at 95 = the Walking clip), **fire while running** (two, hips 85 to 92 = the Run clips), hip fire at a sprint, stooped fire, kneeling fire, two prone shots (0.43 s = the MG burst cycle) |
| Aim up / down | 4 | standing and kneeling |
| Reload | 4 | standing (3.3 s), a walking variant (4.1 s), stooped (3.7 s), prone (6.4 s = belt change) |
| Kneel / squat | 12 | Two heights: one knee down (39 cm) and a squat (46 cm); idles, aimed idle, transitions to stand, stoop and prone, two 90-degree turns, a butt strike |
| Stooped walk (72 to 92 cm) | 16 | The trench gait. Two heights: a wary stoop (92, `Rifle Crouch Walk`) and a low one under the parapet (72, `Walk Crouching`). Forward, back, left, right, a crouched run and run-strafes, two cover sneaks |
| Prone | 9 | Idle, the **unnamed file** (a belly crawl forward, 1.16 m of travel: the only forward crawl in the set), crawl back, roll aside, hit reaction, dive |
| Stand walk / run / sprint | 20 | Walks at three attitudes (rifle up, rifle low, wary), back and sideways, runs forward / back / sideways, one sprint, start and stop clips |
| Turns | 10 | The eight-clip rifle turn set (45/90/135/180 each way, standing and stooped), plain 90s for the slung rig |
| Trench and cover | 7 | `Jumping Down` x2 start **0.65 m in the air** and land: the drop into a trench. `Jump Up` + `Jump Loop` + `Jump Down` is the climb out. `Taking Cover` runs in and turns to the wall; `Emerging` leaves it |
| Reactions | 9 | Heavy stagger (3.1 s), two 2.3 s hits, a 0.6 s hit that keeps running, a walking hit, a duck, a face shield that ends kneeling, a block, a trip that ends flat |
| Deaths | 13 | Seven standing (front, back, right, two headshots, rifle held, thrown back), two walking, one running, two kneeling, one squatting; **none prone** |
| Melee / throw | 5 | Bayonet stab, butt to the face, overhead smash, block, grenade toss (release at 1.4 s) |

Two things the numbers settled: the Mixamo "Firing Rifle (n)" files are not variants of one pose, they are the
fire-while-moving set (their hips heights match the walk and run cycles exactly), which is what lets a whole-body
atlas fire on the move without an upper-body layer; and eleven "transition" clips carry 0.5 to 4 m of root motion
that must be stripped, which the baker already does.

## 3. Where it runs

```
sim tick (20 Hz)  ──►  SimPresenter.Capture  ──►  AnimationController.Tick (Burst, per slot)
                       events (latched)      ──►      reads: stance, velocity, target, flags, cell, water, events
                                                       writes: AnimState[slot] (row, frame, rate, prev row, blend, aim yaw, memory)
render frame      ──►  SimPresenter.Interpolate ──►  AnimationController.Advance (Burst, per pose)
                                                       frame += rate * dt, blend -= dt/fade, one-shot ends -> next
                                                ──►  VATRenderer.FillJob  ──►  VatInstance (48 B)  ──►  VAT_URP.shader
```

- `Presentation/Units/AnimationController.cs`: a `NativeArray<AnimState>` sized to the world's slot count, keyed by
  slot and generation (a reused slot starts clean). Two Burst jobs: **Tick** decides, **Advance** plays.
- Events are one tick only (`SimEventBuffer` is cleared every step), so `SimHost` hands the controller the buffer
  each tick and the controller latches what it needs per slot: last hit tick and direction, last near-miss tick,
  nearest explosion this tick (position, radius), shot fired this tick, trench left this tick.
- The controller derives what the sim does not store: **aim yaw** from `TargetSlot` (direction to the target's
  position, else the velocity yaw), **speed** from the two captured positions over the tick, **ground under him**
  (nav layer bits, water depth from `MapData.WaterLevel` minus the height, slope from the heightfield, cover cell).

`AnimState` (per slot, ~40 bytes):

| Field | Meaning |
|---|---|
| `Row`, `Frame`, `Rate` | what is playing, where, how fast (frames/s; loops scale with speed) |
| `PrevRow`, `PrevFrame`, `Blend` | the clip being faded out, 0.12 to 0.25 s cross-fade |
| `Layer` | which ladder rung owns the body right now (see 4) |
| `Stance`, `WantStance` | the animated stance (may lag the sim's by a transition) |
| `AimYaw`, `BodyYaw` | where he aims, where his feet point (they differ while a turn clip plays or while firing on the move) |
| `IdleSince`, `LastShot`, `LastHit`, `LastNearMiss`, `LastBlast` | ticks, for cooldowns and fidget timers |
| `Shots` | rounds since the last reload (presentation counter until the sim has magazines) |
| `Seed` | hash of slot and generation: picks variants |

## 4. The ladder: who owns the body

The body plays one clip. Which one is decided by a priority ladder, top wins; a lower rung only starts when the
rung above has nothing to say. A rung with a one-shot playing keeps the body until the clip ends unless a higher
rung takes it (a hit interrupts a reload; nothing interrupts a death).

| Rung | Owns the body when | Clips | Ends |
|---|---|---|---|
| 1 Death | `Alive` cleared (Death event) | stance- and direction-matched death, then the fallen buffer (`VATRenderer.AddFallen`) | never |
| 2 Trench | crossing a trench edge (Vault stance, `UnitLeftTrench`, or cell Trench→Surface / Surface→Trench) | drop in, climb out (3 parts) | clip end |
| 3 Reaction | Hit event; explosion within 6 m; near miss while standing still; trip (Bogged, or a wire cell at speed) | hit x5 by gait, duck, shield, dive, prone roll, trip | clip end; shield ends kneeling, trip ends prone then gets up |
| 4 Action | throw (sim grenade), melee (sim), reload (magazine empty), sling/unsling (entering/leaving the rear line) | throw, stab / punch / smash / block, reload x4 by stance, pull out / put away | clip end |
| 5 Stance change | `Stance` (animated) differs from the stance the sim's stance and situation imply | stand↔kneel, kneel↔prone, stand↔stoop, stoop↔kneel, aim up / down | clip end |
| 6 Turn | idle, and `AimYaw` differs from `BodyYaw` by more than 60 degrees | rifle turn 90 / 180 each way, stooped set, kneeling 90s | clip end; `BodyYaw` snaps at the end |
| 7 Locomotion | speed > 0.15 m/s | walk / run / sprint / stoop / crawl by stance and speed, direction by heading vs aim, fire-on-the-move variants when the target is set | continuous |
| 8 Fire | target set and a Shot event this tick (or `FireCooldown` says one is due) | aimed, kneeling, prone, stooped shots at the weapon's rate | clip end, then aimed idle |
| 9 Idle | nothing else | ready idle (target lost > 4 s), aimed idle (target held), stance idles; a fidget after 12 to 30 s idle out of contact | fidget: clip end |

Two rules that make it read as one body and not a slideshow:

- **Start and stop clips are optional garnish.** `Rifle Start Run` / `Run To Stop` / `Walk To Stop` play only when the
  sim's speed ramps over more than two ticks and the man is within 60 m of the camera; otherwise the cross-fade does
  the job. They are cut to their last 1.2 s.
- **Everything below rung 4 is interruptible by a cross-fade**, so the sim is never waited for. A man ordered to
  move mid-reload finishes the reload only if the clip has under 0.4 s left; otherwise he cross-fades into the run
  with the reload dropped (and `Shots` kept, so he reloads when he stops).

## 5. Locomotion

Speed is measured (positions over the tick). Stance is the sim's, refined by situation:

| Sim stance / situation | Gait family | Chosen cycle |
|---|---|---|
| Standing, speed < 2.2 | walk | `Rifle Walk` (default), `Walk With Rifle` (target held, rifle up), `Walk With Rifle (1)` (wary: night, or Suppression > 20) |
| Standing, speed 2.2 to 3.6 | run | `Rifle Run` / `(1)` by seed |
| Sprint stance, or speed > 3.6 | sprint | `Sprint Forward`; MG and sniper cap at run |
| Crouch stance (in trench) | stoop | `Rifle Crouch Walk` (92); `Walk Crouching Forward` (72) on cells whose parapet is under 1.3 m above the floor or when Suppression > 40; `Crouched Run (1)` when speed > 1.8 |
| Prone stance | crawl | the unnamed forward crawl, `Moving Backward In Prone` when heading opposes aim |
| Pinned | none | `Prone Idle` with `Rifle Prone Hit Reaction` on each near miss |
| FireStep | none | see Fire; a man on the fire step edges with `Rifle Side Step` when the sim slides him along the bay |

**Direction.** The heading (velocity yaw) is compared with the aim yaw. With no target the body faces the heading
and the forward cycle plays. With a target: within 50 degrees, forward cycle facing the aim (a small yaw offset the
shader accepts); 50 to 130 degrees, the left / right cycle; beyond 130, the backward cycle (walking backwards
covering a withdrawal; running backwards for assault and rifle only, others turn and run). Diagonals were ditched:
the nearer cardinal is used, the yaw offset covers the rest.

**Rate.** Each cycle is authored at a speed (walk 1.4 m/s, stoop 1.0, run 3.2, sprint 5.0, crawl 0.5, measured
from stride length against the baked frames at bake time). `Rate = speed / authoredSpeed`, clamped 0.6 to 1.6, so
feet stop sliding and a bogged man plods. Mud cells (0.5x sim speed) therefore play the walk at half rate with
`Walk With Rifle (1)` (the heavy, wary walk); Bogged vehicles are not men.

**Fire on the move.** Target set, heading within 50 degrees of the aim, gait walk or run: the matching
`Firing Rifle (2/6/7)` (walk) or `(3)` / `Shoot Rifle` (run) loop replaces the cycle while the sim's `FireCooldown`
is short (a shot at least every 1.5 s); assault at a sprint uses `(5)`. Stooped fire: `Firing Rifle (4)`. The shot's
recoil frame is aligned to the Shot event by restarting the loop at the shot (rate-adjusted so the next recoil lands
on the next cooldown).

## 6. Actions

| Action | Trigger (today) | Trigger (with the sim step) | Clip |
|---|---|---|---|
| Aim up | target acquired while idle | same | `Rifle Down To Aim` / `Kneel To Aim` |
| Aim down | target lost for 4 s | same | `Rifle Aim To Down` / `Aim To Kneel` |
| Fire, standing still | Shot event, Standing/FireStep | same | `Firing Rifle` (rifle, sniper), `Firing Rifle (1)` at the 0.5 rps rifle rate |
| Fire, kneeling / prone | Shot event | same | `Fire Rifle`, `Prone Firing Rifle`; MG: `Prone Firing Rifle (1)` looped at 7 rps |
| Reload | presentation counter: rifle 5 shots, assault 20, MG 50, sniper every shot (bolt: `Reloading` at 2x, 1.2 s) | sim magazine (`WeaponDefinition.MagazineRounds/ReloadSeconds` exist, unused): fire pauses, one `Reload` event | by stance: `Reloading`, `Reload` (stoop), `Prone Reloading` |
| Throw | none (`GrenadeSystem` is a stub) | grenade at ≤ 20 m onto Trench / Crater cells, 6 s cooldown, Shot event with Scalar 1 | `Toss Grenade`, the release frame at 1.4 s spawns the presentation grenade |
| Melee | none (close assault is vehicles only) | infantry within 1.5 m of an enemy: strike every 1.2 s, defender rolls a block | `Bayonet Stab` / `Rifle Punch` / `Smash` by seed; `Block With Rifle`; `Upward Rifle Butt Strike` from a crouch |
| Sling | entering the rear trench with no enemy within 120 m; alarm = target or near miss | same | `Rifle Put Away` then the plain `Idle` / `Walking`; `Rifle Pull Out` on alarm |

Until the sim step lands, reload is cosmetic (the sim keeps firing: the controller only plays it when
`FireCooldown` shows no shot is due within the clip, which is true for the rifle and sniper rates and false for the
MG, whose belt change waits for the sim). Throw and melee simply do not happen until the sim does them.

## 7. Reactions

| Situation | Signal | Response |
|---|---|---|
| Shot and hurt | `Hit` event (target = him, Scalar > 0) | by gait: `Hit Reaction (1)`/`(3)` standing, `Hit Reaction` heavy when Scalar > 40% of MaxHp, `Walking Hit Reaction`, `Hit Reaction (2)` running (0.6 s, keeps going), `Rifle Prone Hit Reaction`. Direction: the clip is chosen by which of front/back/side the `Dir` falls in, else mirrored |
| Near miss, standing still | `NearMiss` event | `Dodging` (duck), not more than once per 3 s. Kneeling and prone men have no flinch clip in the set (listed in 13); they hold their idle |
| Suppressed | `Suppressed` event, meter ≥ 40 | goes to the low stoop / squat if not already; aimed idle becomes ready idle |
| Pinned | stance Pinned | `Prone Idle`; `Rifle Prone Hit Reaction` per near miss |
| Shell lands close | `Explosion` within radius + 3 m, standing | within radius x 0.6: `Dive Roll` (ends standing) for assault / rifle, `Rifle Shielding Face` for the rest (ends kneeling); within radius + 3: `Dodging`. Prone: `Prone Roll` away from the burst |
| Gas cloud on his cell | `GasCloudSpawned` / gas field > 6 on his cell | `Rifle Shielding Face` once, then stooped gait with `Walk With Rifle (1)`; with `Masked`: nothing |
| Trip | Bogged flag on a man (future), or wire cell entered at run speed, or hash 2% per second in Mud cells at a run | `Fall Over` (ends flat), then `Rifle Prone To Kneel` + `Rifle Kneel To Stand`; 2.5 s lost, the sim keeps moving him so the get-up is cut short at 60 m+ |
| Burning | `Burning` flag (future) | `Hit Reaction` heavy looped at 1.4x until it clears or he dies |
| Trench captured / lost, match ended | events | none on the body (banner only) |

## 8. Environment

| Where he is | What the controller reads | What he does |
|---|---|---|
| Trench floor | cell `Trench`; parapet height = `VisualHeight` at the lip minus the floor | stooped gait; the low stoop where the parapet is under 1.3 m; `Rifle Side Step` along the bay on the fire step; sling in the rear trench |
| Fire step | stance FireStep | aimed idle (`Rifle Aiming Idle`), fires standing; `Rifle Kneel Idle` behind the parapet between targets |
| Dropping in | cell Surface → Trench over one tick, drop ≥ 0.8 m | `Jumping Down` (starts 0.65 m up: the sim's own height drop plus the clip's makes the fall) |
| Climbing out | Vault stance / `UnitLeftTrench` | `Jump Up` (0.53 s) → `Jump Loop` while the sim lifts him → `Jump Down`; on a `Link` (ladder) cell the same until a ladder clip exists |
| Cover | arriving within 1.2 m of a cell with `CellCover` ≥ 30 and stopping, target on the far side | `Taking Cover` (runs in, turns to the wall), then `Left/Right Cover Sneak` when sliding along it, `Emerging` when he leaves toward the target |
| Crater | cell Crater, stopped, under fire | kneel → prone in the bowl (`Rifle Kneel To Prone`), `Prone Firing Rifle` |
| Water | depth = WaterLevel − ground under his feet | 0.15 to 0.5 m: walk at rate 0.7 with `Walk With Rifle (1)`, feet sunk (VAT y offset = −depth x 0.6); 0.5 to 1.0: the low stoop at rate 0.5 as a wade (no wading clip exists); prone is impossible in water: suppression puts him in the squat instead |
| Mud cell | NavLayer Mud | rate 0.5, wary walk; the 2% trip roll |
| Wire cell | NavLayer Wire | stooped low, rate 0.6; trip when entered above walk speed |
| Slope | heightfield gradient under him | the shader tilts the figure to the slope up to 12 degrees (a prone man lies on the crater wall); over 25 degrees uphill the walk drops to the stoop |
| Night / rain | `SceneMood.Night`, `_TWWet` | fidget pool adds `Rifle Rubbing Eyes` and `Check Shoe`; idle men in the rear pull the collar (`Rifle Idle (4)`); nothing changes in contact |
| Duckboards (composed sites) | `BattlefieldProps` duckboard placements | nothing on the body; footprints are already suppressed there |

## 9. Death

The `Death` event says what killed him (`b`: the killer's slot, or a `DeathCause` below zero: blast, gas, burning,
beam) and, for a blast, which way and how hard it threw him (`dir`, `scalar`); `AnimationController.Death.cs`
latches it per slot and `Die` reads it. The ladder, in order (2026-09-26, docs/21 phase 4):

1. **Alight, or a beam:** he was running in flames and drops mid-stride, charred (`Char` 3: `VatPad` bits 22-23,
   blackened with embers in `VAT_URP`; the embers go out after 4 s, the body smoulders 8 s and lies 14 s, shrinking
   as the mud takes it). `DeathBurning` (the run slowed into a fold) comes with the next bake; `DeathRunning` stands in.
2. **Gas:** to his knees (`DeathKneel`; `DeathProne` when flat). `DeathGas` with the bake.
3. **Under a track or a claw** (the killer is a vehicle): flat and hard (`DeathBlast`). `DeathCrushed` with the bake.
4. **Thrown:** a blast close enough (the latched burst inside four fifths of its radius, or the sim's knock at up to
   12 m/s) sends him up `DeathThrown` along the sim's knock, further and higher the harder it was, and by the heap:
   every man already down within 4 m in the last 12 ticks adds 35 % (up to five), capped at 14 m out and 9 m up. In
   flight he turns end over end (once past 0.9 s, twice past 1.4 s in a heap), whole turns, so he lands as the clip
   leaves him. A shell that throws him high takes limbs off (more in a heap).
5. **Stance:** prone `DeathProne`; crouch or fire step `DeathKneel` / `DeathSquat`.
6. **Blast, not thrown:** `DeathBlast`.
7. **Gait:** running `DeathRunning`, walking `DeathWalking` (`DeathWalking2` with the bake).
8. **Standing, shot:** by the side it came from (`DeathFront` / `Back` / `Right` / `Left`), one in five a headshot;
   and never the same death as a man within 6 m in the last 20 ticks (the second candidate is taken).

Every death is written to a 256-entry ring (`DeathRecord`: clip, yaw, throw, grime, char, density, cause) that
`CombatFx.Deaths.cs` reads by slot and the event's tick (`TryDeath`). Events are dispatched once per render frame
after every tick of the frame ran, so `State[slot]` may already be another man's: the record is the dead man's.
Men who die on one 2 m cell pile up (`VATRenderer.Fallen.cs`: each lifts the next 0.28 m, nudges him 0.4 m and
tilts him a 32nd of a turn). The clip plays once through the fallen path, cross-faded from the clip he was hit in,
and the man stays where the sim left him.

## 10. Archetypes: who uses what

One rig, four tables. A table is a bitmask over the clip list (which clips this archetype may play) plus a few
preferences the ladder reads.

| | Rifleman | Assault | Machine-gunner | Sniper | Tank |
|---|---|---|---|---|---|
| Exposed gait | run | **sprint** | run (never sprints) | run | — |
| Under fire (Suppression 20 to 60) | stoop | crouched run | **prone at once**, crawls | kneel, then prone | — |
| Fire from | standing / fire step | **on the move** (walk, run, sprint variants) | **prone** (goes prone to fire when Exposed and the target is beyond 30 m) | kneeling or prone, aimed idle held | — |
| Fire clip | `Firing Rifle (1)` | `Firing Rifle (3)/(5)` | `Prone Firing Rifle (1)` looped, `Firing Rifle (4)` stooped | `Firing Rifle` (0.27 s) + bolt | — |
| Reload | `Reloading` every 5 | `Reload (1)` every 20 | `Prone Reloading` every 50 (6.4 s) | bolt after every shot | — |
| Melee | stab / punch | **stab / smash first** within 2 m, turn-and-kick spare | none (backs off: walk backward) | none | — |
| Throw | yes | yes | no | no | — |
| Cover | yes | rarely (sprints past) | yes | **always**: seeks it before firing | — |
| Reactions | all | all, dive preferred | shield preferred, no dive | duck, no dive | none |
| Idle pool | ready x4, fidgets | ready x4, stretch | kneel idle, prone idle | aimed idles, kneel | — |
| Slung in the rear | yes | yes | yes | no | — |

Officer (future): `Idle`, `Walking`, `Turn 90 L/R`, `Inspecting`, plus the sling pair; no fire clips. Tank has no
figure; vehicles keep their box.

## 11. What changes in the renderer and the baker

- **Clip table.** `VATBaker` takes `Art/Characters/InfantryClips.asset` (a ScriptableObject list of `{FBX, clip name,
  state id, loop, fps, mirror, stripRootMotion, cutStart, cutEnd}`) instead of one hard-coded FBX. The row table
  becomes `RowTable[state]` with the state ids in `AnimClip` (a new enum in `Presentation/Units`, not in the sim
  assembly; `AnimRow` in the sim stays for the pose contract and is mapped, so the sim assembly does not change).
- **Play once and hold.** `VatInstance.animT` becomes an absolute **frame** (float); the shader does
  `f0 = floor(frame) mod rowLength` only for loops, else clamps. The controller owns looping.
- **Cross-fade.** `VatInstance` widens from 32 to 48 bytes: `prevRow`, `prevFrame`, `blend`, `aimYawOffset`, `tilt`.
  The shader samples both clips while `blend > 0` (two extra fetches per vertex during a fade, no cost otherwise).
- **Yaw offset and tilt.** Upper body twist toward the aim (up to 35 degrees, blended in by height above the hips)
  and a whole-body tilt to the slope, both in the vertex shader.
- **Mirroring.** Left-hand variants (turn left / death from left / strafe) are baked by mirroring X at bake time and
  flipping the triangle winding: no extra clips to download.
- **Rate.** Loops advance at `Rate` frames per second per man (the global 1 Hz cycle goes away).
- **Atlas.** 131 clips at 15 fps for loops and long one-shots and 30 fps for fire and short one-shots = **~4,800
  frames** x 929 vertices: positions RGBAHalf 36 MB, normals as RGBA32 octahedral 18 MB, **54 MB** total, one
  atlas for all archetypes (texture height 4,800 is under the 16k limit). The 18-row atlas today is 8 MB. This is
  the number to measure on the minimum card; the fallback is 12 fps loops (−30%) or a 600-vertex bake mesh (−35%).
- **Far tier.** The box soldier keeps its 18 procedural rows; a `FarRowOf[clip]` table maps every clip to one of
  them, so the 170 m+ tier costs nothing more.
- **Tests.** `VatAssetTests` pins the row table to the clip list instead of `AnimRow.Count`; a new test plays every
  one-shot to its end and checks the held frame; the pose contract test checks the 48-byte instance layout.

## 12. What the sim would need (separate steps, owner's call)

1. **Magazines** (small, deterministic, uses fields that already exist in `WeaponDefinition`): `MagazineRounds`,
   `ReloadSeconds` → per-unit `RoundsLeft`, a `Reload` event, fire held during the reload. Changes the balance a
   little (the MG's belt change is a real pause). Hashed; replay format unchanged; one test.
2. **Melee**: infantry within 1.5 m strike instead of shooting; a `Melee` event (attacker, defender, blocked). Gives
   the bayonet its place in trench fighting. Balance-visible; needs the owner's yes.
3. **`StanceChanged`** is declared and never emitted; emitting it is free and lets the controller stop diffing.
4. Nothing for aim yaw (derived from the target), water (derived from the heightfield), cover (`CellCover` exists),
   trench crossing (`UnitLeftTrench`, Vault stance and the cell bits exist).

## 13. Clips the set did not have (made 2026-09-22)

Fifteen were manufactured on the rig by `Tools/make_missing_clips.py` (see the manifest's "Made clips" table and
`docs/reference/made-clips/`): wading, a ladder climb, a hands-and-knees crawl forward, a second belly crawl, a
**prone death**, prone and kneeling flinches, a fast get-up from prone, a running stumble, mask donning, a burning
run, an officer's point and whistle, an MG carry, wire crossing. They are recipes over the originals (reversal,
retiming, limbs layered from another clip, IK-held poses, procedural cycles), so they can be re-tuned in minutes
and they import exactly like the originals. Two of them, wading and the ladder, would still be better as captured
motion if Mixamo has anything closer when the bake exists.

Second pass after a harsh animation critique (same day): four habits were fixed in the helpers rather than per clip:
stride scaling is asymmetric (the forward swing grows, the trailing swing shrinks, so a high step is not a hurdle),
every hips drop bends the knees to match (feet stay on the floor), arm layers are the live arm track and not a held
frame, reactions go in over 2 to 4 frames and out over 15. Prone Death's right arm target was out of reach (the hand
sat inside the torso) and the bent knee stayed up: fixed, and it now lands in 0.3 s with an overshoot. Get Up drifted
a metre forward, which under VAT would slide the man and snap him back: the root travel is stripped. Stumble is a
3-frame trip; Mask is four phases (bag, face into the mask, straps, drop); Burning is a folded stagger; the officer's
point raises then chops, the whistle adds the "follow me" arm. The critique's ranking of what the tactical camera sees
most: prone death, get up, stumble, wire / wade, crawl.

## 14. Order of work

Status 2026-09-22 (evening): steps 1 and 2 are built. The clip table is `Editor/InfantryClipTable.cs` (a C# list, not
the ScriptableObject of section 11: nobody edits it in the inspector), the bake writes one gzip'd atlas per figure
(`VatCodec`, `Resources/Units/Figure<Name>*`), `VATRenderer` plays once-and-hold rows with a 0.15 s cross-fade and
the controller's yaw, and the figures are the owner's two Tripo models (`Soldier`, `Sniper`; the officer rig has no
model yet, and mirroring was dropped because the outline pass cannot flip winding: left variants use real clips or
another death). Rungs 9, 7, 5, 6, 8, 3, 1 and 2 run on real clips; a trench routine (kneel, peek, squat, kneeling
fidgets, low under fire) fills the waiting. Left: rung 4's throw / melee / sling (sim steps), cover, the yaw offset
and tilt of section 11, archetype tables (only the MG carry, the sniper's bolt and the assault's fire-on-the-move
differ today), the officer, and a bake at 12 fps if the 35 MB per figure proves heavy on the GTX 1050.

Round 3 (2026-09-22, night): the corpse cross-fades from the clip the man was hit in (`AddFallen` carries
PrevClip/PrevFrame); Mixamo has no kneeling reload or bolt, so `ReloadKneel`, `BoltKneel` and `ReloadStoop` are
composed in the baker (`ClipSource.Lower`: the reload's upper body on the kneel's or the crouch's legs, the spine
kept 75 % at the reload's world lean); reloads play at 1.5× because the sim fires again 2.4 s after the shot's clip;
a one-shot fire plays out before a stance change or a turn can take it, a shot snaps the body yaw to the target (the
feet come round at 6 rad/s) and a shot mid-turn lets the turn finish; a garrison man with no target faces the enemy
side, not his last step; a moving man the sim flips between prone and upright holds his gait ten ticks, then plays
the transition (`FlipSince`); kneeling men turn on the knee (the "kneel turn" files are stoops); a stop under 0.4 s
in a queue keeps the gait (`StopTick`) and a man who has just walked up stands stooped 1.5 s before the knee; a
runner does not duck at a shell (he stumbles half the time). Sim step, agreed as needed to show the climb at all:
`MoveJob.VaultTicks` holds a man at the parapet half a second with stance Vault (the `Cooldown` array counts it),
so the ladder scramble / push-up has time and the drawn man rises up the wall (`Lift`); the landing is cut at
0.25 s when he runs on. The critic's round then fixed: the rifle reload rule (never reloaded: its cooldown equals the
threshold), target bookkeeping skipped by the fire guard (fired from the knee forever), a reload cutting a rise,
one-tick drops at a run, cut turns losing their rotation, phase-locked loops, near misses cutting fire clips,
14 m stumble slides, a quarter of the kneel's pelvis twist leaking into composed arms. Traces:
`docs/reference/controller/trace-sniper-kneeling-cycle.txt` (FireKneel → BoltKneel → KneelAimedIdle, ReloadKneel
after five), `trace-assault-over-the-top-round3.txt` (ClimbLadder in the vault hold, ClimbLand, Sprint, JumpDown at
a run into the enemy trench, then the routine), `trace-rifleman-garrison-round3.txt` (the rifleman rises to the
parapet to fight, reloads standing, drops back, rifle down after 8 s).

Round 4 (2026-09-22, owner: barrage and muzzles): rung 3 anticipates a shell 7 ticks out (a scheduled HE payload or
the ambient `Upcoming` peek). In the open he dives away from it (`DiveAway`, the first 0.78 s of Dive Roll) and
hugs the ground 16 ticks; a man already moving scrambles on. In a trench he shields. The sim's knockback
(`SimWorld.Knock`) carries him. A blast death plays `DeathThrown` on a ballistic arc sized by nearness and blast
radius. Aimed clips (`ClipSource.Aim`): the rifle lies from the right hand through the left, is levelled where the
clip strays (two-bone IK on the left arm) and faces the target, the clip turned by the rifle and not the pelvis (the
Mixamo aim clips stand 71° side-on). `RaiseAim`/`LowerAim` spread that turn over the raise and lower. The baked
muzzle, barrel and chest sockets (`TWVAT3`) put the flash and tracer on the drawn rifle. Figure:
`docs/reference/figures/aims-levelled.png`. Open: a cross-fade between a bladed fire clip and a square clip (a
reload) morphs the legs through 70° for its length. A pose-matched turn clip, or a longer fade at that one edge,
would hide it.


1. Baker: clip table, root-motion strip per clip, mirror, cut, frame rates; bake the 131; the far-tier map;
   tests. Nothing on screen changes yet (the old 18 rows are mapped from the new table).
2. Renderer: frame-based playback, hold, cross-fade, rate, yaw offset, tilt; `VatInstance` 48 B.
3. Controller rungs 9, 7, 5, 6 (idle, locomotion, stance, turn) with the derived inputs; men walk, stoop, crawl and
   turn correctly. Capture at zoom 8 to 30.
4. Rungs 8, 3, 1 (fire, reactions, deaths) from the events; the fallen path plays the matched death.
5. Rung 2 and the environment table (trench in / out, cover, water, mud, wire, slope, gas, night).
6. Archetype tables and the fidget pools.
7. Sim steps 1 to 3 if approved; rung 4 actions on top.
8. Re-tune the made clips (13) against the bake, spares as atlas room allows.

Each step gated (validate, EditMode, PlayMode), committed on its own, editor back in Play; captures at the standard
view and up close for each. Frame time with 3,000 men checked at steps 1, 2 and 4 (the atlas, the fade, the events).

## 15. Open decisions

- The atlas at 54 MB versus 8 MB today: acceptable, or bake at 12 fps / a lighter mesh first?
- Sim steps 1 (magazines) and 2 (melee): both change how fights go. Yes to either?
- Backward running: only rifle and assault, or nobody (turn and run)?
- Officer rig: worth the table now (idle, walk, point) or wait for the class?
