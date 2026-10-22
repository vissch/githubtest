# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1-g1: measure the FIELD's pad shape, not the carried card

**Part 1 of 2.** The probe's three new pad measures exist and are calibrated on the OLD build
(`python probe.py --calibrate` -> exit 0); the three new bars are not set yet, so `python probe.py` still exits 1
until part 2 draws the standing puff in the field and recaptures.

## The owner's complaint

> At zoom 30 the field is a carpet of flat oval pads. The standing puff is only on the carried card, and the README
> says GasPuffStand moves no card pixel, so that card is not this build. Draw that puff in the field the player
> sees, and measure the field, not the carried card. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1`'s tip. Bar outside the spread of three captures of the
> current build.

He is right twice over, and the numbers below prove both halves:

- **The carried card proved nothing.** `GasBankCards.Drawer` draws `cards-gaspuff` at `CardWidth` (12 m), which
  never reads `CombatFx.GasPuffStand`. The `-g1-g2-g1-g1` README said so itself ("no card pixel moves"), so the
  card's rise 8.6-9.2 m and aspect 0.84-0.93 described the card box, not this build's field.
- **The field IS a carpet of flat oval pads.** The new `t1_pad_aspect` on the old build's own three T1 frames
  (804/819/834) reads **0.2139 / 0.2367 / 0.2123** - a pad a fifth as tall as it is wide.

## What leaves and what arrives

**Gone:** `cards1`/`cards2`/`cards3`, the bars `cards_gaspuff_rise_m`, `cards_gaspuff_aspect` and
`cards_gaspuff_smoke_rise`, and the whole look report (`CARDS`, `CARD_NAMES`, `CARD_BARS`, `card_measure`,
`card_set`, `find_card_sets`, `look_report`, `same_card_frame_refusal`). `main()` no longer refuses a folder with
no card set. `GasBankCards.cs` and the `cards-gaspuff` drawing are out of scope for this unit entirely.

**Arrives:** `pad_shape(run_dir)` - the card's three measures moved onto the T1 FIELD frame, reusing
`green_mask`, `components` and `column_heights` unchanged:

| key | how | reads |
|---|---|---|
| `t1_pad_aspect` | undilated green mask (`components(mask, w, h, 0)`, so touching pads stay separate), components of 200 px or more, the ten biggest, each bbox `h_px/w_px`, median | flat oval pad near 0.2, standing puff near 0.7 |
| `t1_pad_rise_m` | the blobbed mask (as `measure_t1`), median of each column's MAXIMUM height | how high the gas TOPS OUT (`green_rise_m` takes the p45 instead) |
| `t1_pad_relief` | stdev over mean of the masked luma, same blobbed mask | flat wash low, mound with a rim high |
| `t1_pad_count` | how many pads were kept | unbarred diagnostic |

`run1`/`run2`/`run3` are carried unchanged from `-g1-g2-g1-g1`: they are commit `78320befa0`'s own three captures,
i.e. **the current build**, so the spread the new bars must sit outside needs no new Unity run.

## The old build's pad numbers (commit 78320befa0, its own run1/run2/run3)

| key | run1 | run2 | run3 | spread |
|---|---|---|---|---|
| `t1_pad_aspect` | 0.2139 | 0.2367 | 0.2123 | 0.0244 |
| `t1_pad_rise_m` | 6.278 | 6.317 | 6.321 | 0.043 |
| `t1_pad_relief` | 0.1043 | 0.1025 | 0.0990 | 0.0053 |
| `t1_pad_count` | 10 | 10 | 10 | - |

The aspect spread 0.0244 is well under the plan's 0.15 trip wire, so the ten-biggest median holds and the
five-biggest fallback is not needed. These three columns are what part 2's bars must clear by at least their own
new spread.

## The nine field bars, carried unchanged

| key | cmp | bar | the three (this folder's runs) | why |
|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3393 / 0.3522 / 0.3379 | owner-decided |
| `green_height_frac` | le | 0.15 | 0.0887 / 0.0913 / 0.0967 | owner-decided ceiling |
| `green_height_floor` | ge | 0.08 | 0.0887 / 0.0913 / 0.0967 | outside the owner's own 0.0741-0.0768 |
| `green_area_frac` | ge | 0.04 | 0.0723 / 0.0699 / 0.0681 | floor on how much gas is drawn at all |
| `green_rise_m` | ge | 2.0 | 2.643 / 2.718 / 2.850 | the p45 column height |
| `o240_green_area_frac` | ge | 0.008 | 0.0093 / 0.0093 / 0.0098 | O240 untouched |
| `o240_cloud_count` | ge | 10 | 20 / 15 / 19 | O240 untouched |
| `o240_cloud_share` | ge | 0.85 | 0.8681 / 0.8687 / 0.8781 | O240 untouched |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | `GasBankStills.PoseBar` |

Every bar is checked twice, as before: all three values on the pass side, AND the nearest value clearing the bar by
at least their own spread (`spread_check`). Part 2 adds `t1_pad_aspect`, `t1_pad_rise_m` and `t1_pad_relief` with
`ge` through the same check.

## The capture recipe (carried from `-g1-g2-g1-g1`)

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two batch
passes into the SAME directory at the SAME wait (gas, then the same line again with `TW_GAS_PLATE=1`), one run at a
time (two instances on one project conflict and both exit with STATUS_ACCESS_VIOLATION):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
# then the same line again with TW_GAS_PLATE=1
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1-g1/probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230, into `run2` and `run3` (T1 frames 804/819/834).
No card sets are shot in this unit.
