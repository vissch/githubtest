# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1: the puff reaches the T1 frame, and the height floor clears 0.0768

**`python probe.py` clears, exit 0.** Twelve bars: the nine field bars (one raised) and the three card bars,
carried, each outside the spread of three captures.

## The owner's complaint

> The play still at zoom 30 is the copied pale stripe sheet, frames 804, 819 and 834, because the field draw was
> not changed. Put the standing puff into that frame and move the height floor outside 0.0741 to 0.0768. Start
> from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1`'s tip. Bar outside the spread of three captures of
> the current build.

He is right: `-g1-g2-g1` raised the CARD to look like a standing puff (`cards-gaspuff` at full `CardWidth`), but
left `CombatFx.GasPuffStand` at 0.32 and the `step == 1` anchor at `Ground - 0.2f` - so the field itself, drawn at
zoom 30 (T1), was unchanged: still the flat pale stripe its three run stills (frames 804/819/834) had always
shown.

## The sweep

`-g1`'s own table (one capture each, anchor `Ground - 0.2f`):

| GasPuffStand | anchor | green_height_frac | green_low_share | green_rise_m |
|---|---|---|---|---|
| 0.32 (then) | -0.2f | 0.0768 | 0.414 | 2.38 |
| 0.35 | -0.2f | 0.0824 | 0.354 | 2.47 |
| 0.5 | -0.2f | 0.1440 | 0.206 | 4.13 |
| 1.0 | -0.2f | 0.514 | 0.071 | 10.16 |

This unit started the sweep at 0.42, at the old anchor: one calibration capture (wait 215, frame 819) read
`green_low_share` **0.2885** - under the owner's own floor 0.30 (and under 0.322, the floor plus twice `-g1`'s
spread 0.0106), so it fails outright. Per the plan, the `step == 1` anchor was sunk toward `Ground - 0.6f` before
cutting the stand: a second calibration capture at the same stand and wait, anchor `-0.6f`, read `green_low_share`
**0.3522** - clear. The sink works because more of the mound's ink then sits under head height, which is exactly
what the low-share measure counts.

| GasPuffStand | anchor | green_height_frac | green_low_share | green_rise_m |
|---|---|---|---|---|
| 0.42 (calibration) | -0.2f | 0.1094 | 0.2885 (FAILS 0.30 floor) | 3.13 |
| 0.42 (this build) | -0.6f | 0.0913 | 0.3522 | 2.72 |

So the field draw is now `GasPuffStand` 0.42, anchor `Ground - 0.6f`, `step == 1` only (`CombatFx.Banks.cs`); the
`step == 2` O240 blanket (`GasBankSquash`, anchor `-0.35f`) is untouched.

## Why the card sets are carried, and the run stills are fresh

- `cards1`/`cards2`/`cards3` - **carried from `-g1-g2-g1` unchanged**. `GasBankCards.Drawer` draws `cards-gaspuff`
  at the full `CardWidth` (12 x 12 m), not at `CardWidth * GasPuffStand`: the card's own height never read the
  field constant, so raising `GasPuffStand` moves no card pixel. The three `CARD_BARS` (`cards_gaspuff_rise_m`,
  `cards_gaspuff_aspect`, `cards_gaspuff_smoke_rise`) and their bar values are carried as-is and still clear.
- `run1`/`run2`/`run3` - **fresh captures of this build**, the `GasBankStills` set at `TW_GAS_WAIT_FRAMES`
  200/215/230 (T1 frames 804/819/834, O240 frames 341/356/371), the same recipe as `-g1-g2-g1`. These are the T1
  frames the owner pointed at; they now draw the standing puff, not the pale stripe.

## The bars

Every bar is checked twice: all three values on the pass side, AND the nearest value clearing the bar by at least
their own spread (max - min). Unchanged bars (field, carried) and the one raised bar:

| key | cmp | bar | the three | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3393 / 0.3522 / 0.3379 | 0.0143 | 0.0379 | owner-decided, carried |
| `green_height_frac` | le | 0.15 | 0.0887 / 0.0913 / 0.0967 | 0.0080 | 0.0533 | owner-decided ceiling, carried |
| `green_height_floor` | ge | **0.08** (was 0.05) | 0.0887 / 0.0913 / 0.0967 | 0.0080 | 0.0087 | **raised**: outside the owner's own 0.0741-0.0768 |
| `green_area_frac` | ge | 0.04 | 0.0723 / 0.0699 / 0.0681 | 0.0042 | 0.0281 | carried |
| `green_rise_m` | ge | 2.0 | 2.643 / 2.718 / 2.850 | 0.207 | 0.643 | carried |
| `o240_green_area_frac` | ge | 0.008 | 0.0093 / 0.0093 / 0.0098 | 0.0005 | 0.0013 | carried (O240 untouched) |
| `o240_cloud_count` | ge | 10 | 20 / 15 / 19 | 5 | 5 | carried (O240 untouched) |
| `o240_cloud_share` | ge | 0.85 | 0.8681 / 0.8687 / 0.8781 | 0.0100 | 0.0181 | carried (O240 untouched) |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | carried |
| `cards_gaspuff_rise_m` | ge | 8.0 | 8.769 / 9.189 / 8.598 | 0.591 | 0.598 | carried (card untouched) |
| `cards_gaspuff_aspect` | ge | 0.70 | 0.8354 / 0.9048 / 0.9264 | 0.0910 | 0.1354 | carried (card untouched) |
| `cards_gaspuff_smoke_rise` | ge | 0.90 | 1.1351 / 1.1761 / 1.0600 | 0.1161 | 0.1600 | carried (card untouched) |

`green_height_floor` is the round number at or below `min - spread` (0.0887 - 0.0080 = 0.0807) that also sits
above the owner's named target 0.0768: **0.08**. The old build's own run1/run2/run3 (0.0768/0.0741/0.0757) fail it
outright, which is the owner's ask.

O240 did not move, as designed: `step == 2` still draws `GasBankSquash`'s squashed blanket, untouched by this
change.

## The capture recipe

Per run, with this checkout's editor closed and the project free (`python Tools/editor_lock.py status`), two
batch passes into the SAME directory at the SAME wait (gas, then the same line again with `TW_GAS_PLATE=1`), one
run at a time (two instances on one project conflict and both exit with STATUS_ACCESS_VIOLATION):

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-f1 TW_GAS_WAIT_FRAMES=200 \
python Tools/pipeline/run_detached.py start gas-f1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills-f1.xml -logFile <out>/stills-f1.log
# then the same line again with TW_GAS_PLATE=1
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1-g1/probe.py --pack %TEMP%/tw-gasbank-f1 --into run1
```

Runs 2 and 3 are the same at `TW_GAS_WAIT_FRAMES` 215 and 230, into `run2` and `run3`. The card sets are not
reshot here; see `-g1-g2-g1`'s README for their recipe.
