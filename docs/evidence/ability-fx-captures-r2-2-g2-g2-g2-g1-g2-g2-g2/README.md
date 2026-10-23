# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g2-g2: the far count keeps only a cloud a player can pick out, 2026-10-23

Probe-only. No `CombatFx` edit, no Unity, no fresh captures: `run1/`, `run2/`, `run3/` are carried as-is from
`-g1-g2-g2` (part 2's three fresh captures, GasPuffLift 0.025, tip 3486a68b99). Only `CLOUD_MIN_PX` and the four
O240 bars move.

## The owner's complaint

> The far still is a wash of green stripes. o240_blob_top is 4274/1242/1058/620 then 145,
> 4537/2324/375/269 then 157, and 4858/2312/332/209 then 166. CLOUD_MIN_PX 200 keeps four stripes, the count is
> 4/4/4, and the ceiling of 6 still passes that wash. Count only a cloud a player can pick out, and put the
> ceiling under the fourth blob. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g2`'s tip. Bar
> outside the spread of three captures of the current build.

## CLOUD_MIN_PX 200 -> 1150

Measured from `run1/gas-o240.json` and `run1/manifest.json`, not guessed. The O240 camera sits at y 264.15 over
ground 1.4684, pitch 25 deg, so the centre's slant distance is 262.68 / sin 25 = 621.7 m; with FOV_V 25 deg the
frame spans 2 * 621.7 * tan 12.5 = 275.7 m over 720 px, i.e. **2.61 px per metre**, and ground depth is
foreshortened by sin 25 = 0.423. A patch W metres across on the ground is therefore about
pi/4 * 2.61W * 1.103W = 2.26 W^2 px. The manifest says five calls and 395 gas cells of 4 m, ~79 cells each bank,
so one bank is ~40 m across = ~3600 px; **half a bank, 22.5 m, is 1150 px** — the smallest green patch a player
picks out as a cloud rather than a stripe at this band. It also sits in the one window the three captures leave
stable (1059..1241: above run1's third blob 1058, below its second 1242), so the count is 2/2/2 with no spread.

## The bars

Rebarred from the same `-g1-g2-g2/probe.json` component sizes, CLOUD_MIN_PX 1150: run1 clouds 4274+1242 = 5516 px,
run2 4537+2324 = 6861, run3 4858+2312 = 7170; green_px 9896 / 9805 / 10260. Only the four O240 bars move; the
T1 bars, `BARS` itself, the camera maths, `--pack`, `--calibrate`, `--selftest` and the spread check are
untouched.

| bar | cmp | was | now | runs | spread | margin |
|---|---|---|---|---|---|---|
| `o240_cloud_count` | ge | 3 | **1** | 2 / 2 / 2 | 0 | 1 |
| `o240_cloud_ceiling` | le | 6 | **3** | 2 / 2 / 2 | 0 | 1 |
| `o240_cloud_share` | ge | 0.65 | **0.40** | 0.5574 / 0.6995 / 0.6988 | 0.1421 | 0.1574 |
| `o240_green_area_frac` | ge | 0.006 | **0.004** | 0.0060 / 0.0074 / 0.0078 | 0.0018 | 0.0020 |

The ceiling 3 is the point: today's wash counts 4 and now fails. The floor drops to 1 because only two patches of
the current build are pickable — it is honest, not good.

**Risk.** 1150 is close to run1's second blob (1242). A later build whose second patch shrinks drops the count
to 1 and still passes the floor; the ceiling, not the floor, is this leg's bar.

## The 5-calls-2-clouds gap

`o240_calls_taken`, a new unbarred diagnostic read from `manifest.json`'s `calls_taken`, prints 5 next to a cloud
count of 2 in every run. O240 fires five separated gas calls but this build gives a player only two pickable
clouds. That gap is a look fault for a later leg to close with a `CombatFx` change — not this leg's to bar.

## The capture recipe (carried, unchanged, for reference only — no fresh captures taken this leg)

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
# then, same dir, same wait:
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 TW_GAS_PLATE=1 \
python Tools/pipeline/run_detached.py start gas-f1-plate --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1-plate.xml -logFile <out>/stills-f1-plate.log
python probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230: three waits are three frames, and the probe
refuses two runs that photographed one frame.

## Done when

`python probe.py` exits 0 (confirmed this leg: every bar `OK`, `o240_cloud_count` 2 in each run,
`o240_cloud_ceiling` margin 1).

## Out of scope

The close puff's look and the `cards/` isolation set (no `CombatFx` edit here), and closing the gap between 5
calls and 2 pickable clouds — that is a build change the owner has not asked for yet.
