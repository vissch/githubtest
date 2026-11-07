# paradrop-inbound-marker, r2 fix 3: can the falling streaks be counted in a still?

**Answer: no.** At zoom 120, at night, in GreyboxCorridor, the drop's flare and its one streak per man do not
reach the screen in a way a still can be counted on. This folder is the measurement behind that sentence; it
replaces the line in commit `38b90a378` that said "do not treat probe.json as a real signal".

The code and the EditMode test are not touched. `CombatFx.OnDropInbound` does add the cards
(32 `ShellFall` + 1 `Smoulder` + 1 `Star`, read live off `FlipbookFx.cards`, positions dead on the ring:
`Smoulder(45.0, 0.7, 82.2)`, `ShellFall(52.0, 1.0, 83.0)` = 7 m out, `ShellFall(53.8, 0.6, 89.6)` = 11.5 m out).
`FarGrowTests.DropStreaksOnePerMan` holds the timing and still passes. What fails is the last step: the cards
are drawn and cannot be seen.

## The pose
| | |
|---|---|
| scene | `Assets/_Project/Scenes/GreyboxCorridor.unity`, in Play, fresh enter |
| stage | `GymDirector.Attach(host)`, `Quiet()`, `Clear()`; target = `d.Stage + (0, 30)` = **(45.00, 82.20)**, ground y 0.69 |
| ability | `d.Ability(OffMapAbilityId.ParaDrop, target)` -> true, Brass seat 1; `CanDropLand(1, target)` = true; `Scalar` 5.0 s, `TickSeconds` 0.05 |
| camera | focus (45.00, 82.20), **zoom 120**, yaw 30, pitch 25, 1600x900 |
| mark | `CaptureRig.Rig.Mark` = (45.00, 0.69, 82.20) -> `mark_px` (800.0, 446.0), `mark_on_screen` 1, depth 311 m |
| quieted | `CombatFx.ShowOverlays = false`, both `UIDocument`s hidden, `CameraShake.Strength = 0` + `Reset()`, `Storm.Hold = true`, `Atmosphere.Lightning = false`, `Atmosphere.PinnedClock = 120` |
| pose check | `pose_error_m` **0** on every still here; `rain` and `wind` identical across the set |

The weather is held **without** `CaptureRig.Hold`, which sets `Time.timeScale = 0` and would freeze the streak
cards themselves.

## The stills
| file | what it is |
|---|---|
| `z120-before.png` | frame 33245, the pose, one frame before the call |
| `after_00.png` | t = 0.10 s after the call: the drop is up but **no streak wave has started** (the first is at 1.42 s), so this is the drop's own baseline |
| `after_02.png` | t = 1.20 s |
| `z120-after.png` = `after_04.png` | **t = 2.30 s**, the frame nearest the 2.5 s the fix is judged at, picked by that rule and not by eye |
| `after_08.png` | t = 4.50 s, the last wave |
| `z120-control.png`, `z120-control-b.png` | the unchanged scene twice, 44 frames apart - the same gap as before -> after (48 frames). The noise floor. |

Times come from the frame numbers in the sidecars against a frame rate measured over the run itself
(`Time.frameCount` and `Time.time` at the call and after the queue drained: 20.0 fps). The editor's frame rate
wanders between runs - an earlier set at a nominal 0.5 s spacing landed inside 1 s of real time and missed the
streak window entirely - so a series is never timed in frames alone.

## The count
`python docs/evidence/paradrop-inbound-marker-r4/streakprobe.py` -> **exit 1**, `probe.json`:
`streaks` 0 of 8 men, `streak_ratio` 0.0, `noise_streaks` 0.

The probe counts a component of the before/after diff as a streak only when its shape and its place both fit a
card: area >= 40 px, >= 10 px tall, taller than wide, and its **ground anchor** (its bottom edge) on one of the
two rings. At pitch 25 the rings are ellipses, not circles - 7 m and 11.5 m are 45 px and 75 px across the
ground but only 19 px and 32 px up the screen - so the test is `hypot(dx, dy / sin 25)` against 45.5 px and
74.8 px, give or take 12 px. Every number is read off the pose (6.50 px/m at zoom 120 over 900 px) and the two
constants in `CombatFx.Barrage.cs`; none is fitted.

Nothing passes, at any of the four moments, and the control pair returns 0 as well: the probe is not blind, it
has nothing to find.

## What is on the screen instead
Over the window 1.2 - 4.5 s a loose window around the mark does pick up 5 - 8 components that are absent at
t = 0.10 s, so something of the drop reaches the frame. They do not sit on either ring: every one of them is
70 - 135 px to the **left** of the mark, which is ten metres past the far ring and all in one direction. A
3x crop centred on the mark shows no flare and no streaks to the eye in any frame of the set.

A capture at the tactical camera's own zoom 30 was tried first and is worse, not better: `FarGrow` is read
from `IZoomSource.CurrentZoom` **when the event arrives**, so a drop called at zoom 30 draws 3 m cards (19 px)
however far out it is then watched from. The set above was shot with the tactical camera itself moved to
zoom 120 first (`TacticalCamera.FrameFrom(target, 120, 30)`), which is the best case: `FarGrow(120)` = 1.50,
a 4.5 m card, 29 px. That best case is what measured 0.

## r4: is it the geometry, or the amplitude?
r2 fix 3's ring positions were computed by formula (`PX_PER_M`, an ellipse for pitch 25), never projected - the
weak link this leg replaces. `ringpx.cs`, run once per point through `Tools/tw run eval_file` (Play, same pose,
no drop), sets `CaptureRig.Rig.Mark` to each of the 8 men's world points and reads back `mark_px_x/y`, the rig's
own `Camera.main.WorldToScreenPoint`. Every point's sidecar reproduced the stills' camera exactly
(`cam_x/y/z` = 290.2854, 132.07288, -59.4156; the centre point's `mark_px` 800.00012, 445.94928 against the
stills' 800.0, 446.0), so the 8 measured points in `ringpx.json` apply to the committed `after_*.png` stills
without re-capturing. The measured points land close to where the formula put them (the formula's radius was
within its own 12 px tolerance for 6 of 8 points; `k=5` was 2 px over) - **the geometry was already about right.**

Rewriting `streakprobe.py`'s place test to match each diff component against its nearest measured point (20 px
tolerance: half a 29 px card plus the jitter) still counts **0 streaks of 8**, same as the formula version
(`python docs/evidence/paradrop-inbound-marker-r4/streakprobe.py` -> exit 1). So step 4: in a 20 px disc centred
on each measured point, the drop's own `|after - before|` max against the control pair's max in the identical
disc (`probe.json`'s `amplitudes`) -

| man (k) | drop max | control max | ratio |
|---|---|---|---|
| 0 | 263 | 77  | 3.42x |
| 1 | 318 | 168 | 1.89x |
| 2 | 244 | 121 | 2.02x |
| 3 | 158 | 99  | 1.60x |
| 4 | 167 | 230 | 0.73x |
| 5 | 321 | 120 | 2.68x |
| 6 | 330 | 230 | 1.43x |
| 7 | 340 | 152 | 2.24x |

Only 1 of 8 clears the control's own noise by 3x or more; one man's disc (k=4) sits **below** the control's own
noise in the same disc. The discs sit near the control's level, not clearly above it, so this is not "a measured
floor hid a real card": the cards do not reach the screen at this band, by amplitude as well as by shape and
place.

## So
The marker reads in the sim and in the test, and does not read on the screen at the overview band - confirmed by
projection, not by a formula that happened to be close. Making it read means a bigger or brighter card, which is
a change to an accepted look - the owner's call, filed under "Open: waiting on the owner" in
`docs/reference/decisions.md`. Nothing in the look is touched here.
