# ability-fx-captures-r2-2-g2-g2-g2-g1-g1: fewer, taller close puffs instead of a sheet, 2026-10-22

**`python probe.py` clears, exit 0.** Three fresh captures of this branch's build (`GasPuffBlock` 3,
`GasPuffStand` 1.2, `GasPuffFirst`/`GasPuffLast` 12/16), at three different frames, every bar outside their
spread.

## The owner's complaint

> At the close camera the gas is still the pale green stripe sheet (frames 804, 819 and 834). Paint one standing
> puff with a rim, the shape of cards-smoke, and put the height floor outside that sheet. The 0.05 floor sits
> under 0.0741 to 0.0768. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1`'s tip. Bar outside the
> spread of three captures of the current build.

## Why it was still a sheet

`-g1` added `Book.GasPuff` and `GasPuffStand` 0.32, but drew it **per 4 m field cell**, the same grid the old flat
blanket used. Its own sidecars read `gas_cards` 774 at the close camera: 774 low cards 4 m apart overlap into one
pale wash with stripes at their top edges. Height alone cannot fix that — `-g1`'s own sweep shows no stand clears
both bars from that grid (0.0768 was the best it found, barely under its own floor).

## The fix: fewer, taller cards, not a taller sheet

`CombatFx.Banks.cs`:
- **`GasPuffBlock` = 3**: the close drawing (`step == 1`) now calls `Refresh` with `step` 3, so one card stands
  for a 3 x 3 block of field cells — about 86 cards 12 m apart, not 774 4 m apart. The overview's merged-out draw
  (`step` 2, `GasBankSquash`) is untouched: `Refresh`'s own `step` parameter already keys the cache, so O240 does
  not move.
- **The card itself stays 5-7 m wide** at any block size (`width * (step == 1 ? 1f : step)` where `step` is the
  *local* zoom step, 1 at the close camera): the block widens the *gap* between puffs, not the puff. Gaps are
  what make a puff read as a puff instead of a wash.
- **`GasPuffStand`: 0.32 -> 1.2.** `-g1`'s 0.32 was fitted to the 774-card sheet and does not carry over to 86
  cards 12 m apart; re-measured against this build (see `CombatFx.Banks.cs`'s own doc comment for the sweep:
  1.0 / 1.5 / 2.0 and 1.2 at two different frame windows).
- **`GasPuffFirst`/`GasPuffLast` = 12/16**, a new frame window just for the close puff, inside
  `SmokeBankFirst`..`SmokeBankLast` (12..20.5): at `GasPuffStand` 1.2 the full SmokeBank window read
  `green_low_share` 0.1941 against the owner's 0.30 floor, so the puff's cross-section needed the mound's early,
  wider frames, not just a shorter stand. `SmokeBank` itself (and `OnBankSpawned`'s growing-mound animation) still
  uses the full window — only the close gas puff's steady-state card reads the narrower one.

## What is here

- `probe.py` — carried from `-g1` unchanged (camera maths, green mask, `largest_blob`, O240 clouds, `--pack`,
  `--calibrate`, `--selftest`, the two-sided height bar, the spread check). Only `BAR_VALUES` and the module
  docstring are rewritten for this build's own three captures.
- `run1/`, `run2/`, `run3/` — shot in this unit: the four `GasBankStills` captures with sidecars and manifests, at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 — T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371 (the same three
  frames the owner's complaint names for T1). `pose_error_m` 0 everywhere, `books_ready` true.
- `cards/` — reshot against this build: `cards-gaspuff` now stands at `GasPuffStand` 1.2's shape instead of 0.32's.
- `probe.json` — all three runs' numbers, frames, the look report and `BAR_VALUES`.

## The sweep that picked 1.2 at the narrow window

Each row is one `GasBankStills` gas pass plus its plate pass, at `TW_GAS_WAIT_FRAMES` 200 (frame 804):

| GasPuffStand | frame window | green_height_frac | green_low_share | verdict |
|---|---|---|---|---|
| 1.0 | SmokeBankFirst..Last (12..20.5) | 0.0786 | 0.3299 | clears both bars, but height margin over -g1's own 0.0768 is only 0.0018 |
| 1.5 | 12..20.5 | 0.2063 | 0.1437 | over the 0.15 ceiling |
| 2.0 | 12..20.5 | 0.1730 | 0.2171 | over the ceiling AND under the 0.30 low floor |
| 1.2 | 12..20.5 | 0.1057 | 0.1941 | height is in range, but low_share breaks the 0.30 floor |
| **1.2** | **GasPuffFirst..GasPuffLast (12..16)** | **0.0950** | **0.3462** | **kept: room at both ends of every bar** |

`green_height_frac` does not move monotonically with the stand: `largest_blob` keeps one connected component out
of a sparse, 12-m-gapped field, and which component (and its own screen-space height) can jump between nearby
stand values. The kept row is this build's own three-capture measurement, not an interpolation.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min).

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3462 / 0.3458 / 0.3462 | 0.0004 | 0.0458 | owner-decided, kept: the gas is a man's gas |
| `green_height_frac` | le | 0.15 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0503 | owner-decided ceiling, kept: not a wall |
| `green_height_floor` | ge | 0.08 | 0.0950 / 0.0933 / 0.0947 | 0.0017 | 0.0133 | re-set, above `-g1`'s own sheet range 0.0741-0.0768 (the goal's own bar) |
| `green_area_frac` | ge | 0.01 | 0.0136 / 0.0136 / 0.0136 | 0.0000 | 0.0036 | re-set lower: fewer, taller cards cover less of the frame than `-g1`'s wash, on purpose |
| `green_rise_m` | ge | 2.4 | 2.575 / 2.529 / 2.568 | 0.046 | 0.129 | the height in metres |
| `o240_green_area_frac` | ge | 0.008 | 0.0094 / 0.0093 / 0.0098 | 0.0005 | 0.0013 | carried: the overview's clouded pixels, unmoved |
| `o240_cloud_count` | ge | 8 | 21 / 15 / 20 | 6 | 7 | re-set: this build's own spread is wider than -g1's, still clouds not a wash |
| `o240_cloud_share` | ge | 0.85 | 0.8729 / 0.8696 / 0.8847 | 0.0151 | 0.0196 | carried |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

O240 did not move, by design: outside `BankMergeZoom` 120 the bank is still `GasBankSquash`'s squashed blanket
(step 2 of `BankStep`), and only the close camera draws `Book.GasPuff` with the new block. Its numbers match
`-g1`'s within capture noise.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait, so the plate hits the shot's frame:

```bash
# gas pass, then the same line again with TW_GAS_PLATE=1
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1/probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230. Only one batch Unity instance may run against this
checkout at a time (they all point at the same `-projectPath`): run the gas and plate passes, and each run, one
after another, never in parallel.

The `cards/` set is one launch, no wait knob and no plate pass (the test shoots its own plate):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-cards-g1g1 python Tools/pipeline/run_detached.py start cards-g1g1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults <out>/cards-g1g1.xml -logFile <out>/cards-g1g1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1/probe.py --pack %TEMP%/tw-gasbank-cards-g1g1 --into cards
```
