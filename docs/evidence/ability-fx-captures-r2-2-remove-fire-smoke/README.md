# ability-fx-captures-r2-2-remove-fire-smoke: no fire, no chimney smoke in the gas captures, 2026-10-27

`python probe.py` exits 0: every bar `OK`, including the new `hearth_fx_ceiling`, and the hard check that every
still of every run has `fires_out` true. Three runs at three frames (`TW_GAS_WAIT_FRAMES` 200 / 215 / 230) are in
`run1/`, `run2/` and `run3/`.

## The owner's words

His answer on the Decide page to "ability-fx-captures-r2-2: chain of fix units 9 deep, 26 units":

> remove the fire and smoke from the chimneys and the feet

## Why the fire and the smoke were in the picture

`NightLights.Build` filled `SceneHooks.SmokeSources` from two loops - the dugout-stove **chimneys**
(`sites[i].Position + up*2.3`) and the **flame feet** (`flameFeet[i] + up*1.8`) - filled `firePoints` from the
same feet, and drew the flame mesh at them. `CombatFx.Ambient` turns every smoke source into rising grey chunks
every 0.45 s, and `SmallLife` puts ash motes over every `FirePoints` entry. So eight chimneys plus the feet were
puffing grey over the gas and ashing at their own flames in every still of this chain.

## The fix

**A capture switch, not a look change.** `NightLights.CutFires()` (part 1) sets `firesOut` and enforces it: the
flame `MeshRenderer` off, `SceneHooks.SmokeSources` cleared, `firePoints` cleared. It is idempotent, `Update`
keeps it enforced (because `Build` runs lazily, frames into Play), and `Build` re-applies it when the harness
asked before the scene was built. `GasBankStills` and `GasBankCards` call it before they shoot.

This is the shape `NightLights.CutLamps()` already has for the stills, and `CombatFx.ShowOverlays = false` in
`GasBankStills`. The game keeps its fires; the gas captures cut them. Out of scope and untouched: the lanterns,
torch posts, glow cards, hearth light, horizon fires, and the mud the running men kick up at their feet
(`CombatFx.Ambient`'s `nextKick`).

**If the owner meant the look, not the capture**, nothing of this is wasted: the same `CutFires()` call moves into
the scene build behind a knob, and the bar below keeps guarding the captures.

**The proof is in the manifest.** `GasBankStills` writes two new fields per still, read off the frame it
photographed: `hearth_fx` (smoke sources plus fire points, so 0 means nothing can puff or ash) and `fires_out`
(`NightLights.FiresOut`). The probe bars the first and hard-checks the second.

## The three runs of this build

One gas pass per run, all three against one no-gas plate pass (O240 plate frame 301, T1 plate frame 796).

| | run1 (wait 200) | run2 (wait 215) | run3 (wait 230) | spread |
|---|---|---|---|---|
| T1 / O240 frame | 804 / 341 | 819 / 356 | 834 / 371 | three different frames |
| `hearth_fx` (worst of 4 stills) | 0 | 0 | 0 | 0 |
| `fires_out` (all 4 stills) | true | true | true | - |
| `o240_rise_m` | 8.167 | 7.857 | 7.761 | 0.406 |
| `o240_base_m` | 0.000 | 0.000 | 0.000 | 0.000 |
| `o240_blob_max_px` | 2688 | 2705 | 2697 | 17 |
| `o240_cloud_count` | 12 | 11 | 10 | 2 |
| `o240_cloud_share` | 0.9551 | 0.9576 | 0.9677 | 0.0126 |
| `o240_green_area_frac` | 0.0183 | 0.0184 | 0.0183 | 0.0001 |
| `green_rise_m` (T1) | 2.575 | 2.529 | 2.568 | 0.046 |
| `green_height_frac` (T1) | 0.0950 | 0.0933 | 0.0948 | 0.0017 |
| `pose_error_max` | 0.000 | 0.000 | 0.000 | 0.000 |

**No carried bar moved.** Every gas number repeats `-g1-g1`'s to within its own run spread (run1 at wait 200
reads `o240_rise_m` 8.167 against 8.176, `green_area_frac` 0.01359 digit for digit): cutting the grey smoke did
not change what the green mask sees, because the plate is shot with the fires cut too, so the chunks were never
part of the plate difference either way. What did change is the picture.

## The bars

Carried from `-g1-g1` unchanged, re-read on these three captures, plus one new bar.

| bar | cmp | value | runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `hearth_fx_ceiling` | le | **0.5** | 0 / 0 / 0 | 0 | 0.5 | **new, the point of this unit.** No chimney smoke source and no fire point in the photographed frame. 0.5 and not 0 keeps a margin over an integer count, the shape `o240_base_ceiling` uses. The FAIL side is stated, not reshot: any build without `CutFires` reads the 8 dugout chimney sources plus its flame feet's fire points, and fails by eight |
| `o240_rise_floor` | ge | 7.0 | 8.167 / 7.857 / 7.761 | 0.406 | 0.761 | carried; the overview cloud still has a top |
| `o240_base_ceiling` | le | 1.0 | 0.000 / 0.000 / 0.000 | 0.000 | 1.0 | carried; the base sits on the mud |
| `o240_blob_floor` | ge | 1000 | 2688 / 2705 / 2697 | 17 | 1688 | the owner's own number, carried |
| `o240_cloud_count` | ge | 3 | 12 / 11 / 10 | 2 | 7 | clouds exist at all |
| `o240_cloud_ceiling` | lt | 15 | 12 / 11 / 10 | 2 | 3 | the owner's "under 15" |
| `o240_cloud_share` | ge | 0.85 | 0.9551 / 0.9576 / 0.9677 | 0.0126 | 0.1051 | carried |
| `o240_green_area_frac` | ge | 0.0060 | 0.0183 / 0.0184 / 0.0183 | 0.0001 | 0.0123 | carried |
| `green_low_share` | ge | 0.30 | 0.3462 / 0.3442 / 0.3450 | 0.0020 | 0.0442 | owner-decided, carried |
| `green_height_frac` | le | 0.15 | 0.0950 / 0.0933 / 0.0948 | 0.0017 | 0.0550 | owner-decided ceiling, carried |
| `green_height_floor` | ge | 0.08 | 0.0950 / 0.0933 / 0.0948 | 0.0017 | 0.0133 | carried |
| `green_rise_m` | ge | 2.4 | 2.575 / 2.529 / 2.568 | 0.046 | 0.129 | carried |
| `green_area_frac` | ge | 0.0135 | 0.01359 / 0.01358 / 0.01358 | 0.00001 | 0.00008 | carried |
| `pose_error_max` | lt | 0.50 | 0.000 / 0.000 / 0.000 | 0.000 | 0.50 | `GasBankStills.PoseBar` |

The hard check is not a bar: `fires_out` is true for all four stills of all three runs, or the probe exits 1
before it looks at a bar. A capture where the switch was never thrown is not evidence for this unit, whatever its
pixels read.

## What is here

- `probe.py` - `-g1-g1`'s, plus `manifest_rows` / `hearth_of`, the `hearth_fx_ceiling` bar and the `fires_out`
  hard check. See its docstring.
- `run1/`, `run2/`, `run3/` - the three captures, `TW_GAS_WAIT_FRAMES` 200 / 215 / 230, `pose_error_m` 0. Each
  holds its two gas stills with sidecars and `manifest.json`, plus the **one shared plate** pass's two stills and
  `manifest-plate.json`.
- `cards/` - **reshot for this build**, because `GasBankCards` now calls `CutFires` too. The five isolation cards
  are hand-packed at a fixed pose and never barred; the look report prints them.

## The capture recipe

This checkout's editor closed and the project free (`python Tools/editor_lock.py status`), one batch pass at a
time, each detached:

```bash
# the plate once (TW_GAS_PLATE=1), then a gas pass per run at its own TW_GAS_WAIT_FRAMES (200/215/230),
# then the cards pass with -testFilter GasBankCards
TW_STILLS_DIR=%TEMP%/tw-gas-run2-rfs TW_GAS_WAIT_FRAMES=215 \
python Tools/pipeline/run_detached.py start gas-run2-rfs --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <dir>/stills.xml -logFile <dir>/stills.log
python docs/evidence/ability-fx-captures-r2-2-remove-fire-smoke/probe.py --pack <dir> --into run2
# then copy run1's five plate files (two jpg, two json, manifest-plate.json) into run2 and run3
```

A GPU that is busy (other checkouts' editors) fails a pass with `d3d11: failed to create ... [8007000e]` and a
red `GasBankStills`: it leaves no PNGs, and the pass is simply run again.
