# ability-fx-captures-r2: the chlorine bank, 2026-10-19

**The result: BLOCKED, and the stills say why.** The sim TOOK every gas call (the manifest records the cooldown the
sim set, 1800 ticks, for all six calls), the stills were taken 133 ticks after the call — 73 ticks into the bank's
240-tick life — from a pose with `pose_error_m` 0.0, and **no gas bank appears in any of the four pictures**.

| still | what it is |
|---|---|
| `gas-t1.jpg` | the play zoom (30), chlorine called at the frame's centre, ability-area marker hidden |
| `gas-t1-plate.jpg` | the same pose at the same frame of the same script, with the gas call skipped |
| `gas-o240.jpg` | the overview (240), five calls 40 m apart along the corridor |
| `gas-o240-plate.jpg` | its plate |

`probe.json`, written by `probe.py` from the pixels of those four JPGs:

| number | measured | the bar |
|---|---|---|
| `gas_t1_green_low_share` | 0.89 of 28 pixels — noise, not a bank | >= 0.30 |
| `gas_t1_height_frac` | -0.0 (no bank to measure) | <= 0.15 |
| `gas_o240_effect_area_frac` | 0.027, all of it the battle moving between the two passes (the gas-coloured part is 0.0009) | >= 0.03 |
| `pose_error_max` | 0.000 | < 0.50 |

## What was ruled out

- **The call being refused.** `manifest.json` carries `calls_taken`, counted from the cooldown the sim sets only on a
  call it accepts: 5 of 5 at O240 and 1 of 1 at T1.
- **Shooting too late.** The first try waited 470 frames (313 ticks) and the bank had expired; this one waits 200
  frames (133 ticks), inside its life.
- **The pose.** All four sidecars read `pose_error_m` 0.0 (`CameraShake.Strength = 0` before the set).
- **The cards not being built.** In a live editor, at the same point of the cloud's life,
  `CombatFx.gasCards` held 22, 66, 99 and 133 packed cards (read by reflection). They are submitted and nothing of
  them reaches the picture.
- **The map corner.** Three different gas points were tried (x 26, 45 and 56 of a 90 x 276 m map).

What DID show up, in a shot with `CombatFx.ShowOverlays` left on, is the yellow **ability-area marker disc** — a flat
disc on the ground, which is what r1's "the gas pools on the ground" picture most likely showed.

## So what is left

Either the bank draws nothing a night frame can read, or `CaptureRig`'s own `Camera.Render()` misses the book's draw
path (the GasBank sheet is the only `Bank = true, Deep = true` book). Both are outside this unit: growing or
brightening the bank is a look change the owner has not picked. The question is in `docs/reference/decisions.md`
under "Open: waiting on the owner".

## Rerunning it

```bash
# from trench-warfare-3d, this checkout's editor closed; the pipeline's run_tests selects no [Explicit] test
TW_STILLS_DIR='C:\...\tw-gasbank' "/c/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" \
  -batchmode -projectPath "$(pwd -W)" -runTests -testPlatform EditMode -testFilter GasBankStills \
  -testResults Logs/gas.xml -logFile Logs/gas.log
# then the same line again with TW_GAS_PLATE=1 for the plates, and from the repo root:
python docs/evidence/ability-fx-captures-r2/probe.py --pack <that folder>
python docs/evidence/ability-fx-captures-r2/probe.py --selftest   # the camera maths against a hand calculation
```
