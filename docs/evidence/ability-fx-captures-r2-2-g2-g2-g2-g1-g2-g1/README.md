# ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1: the gaspuff card stands, and three card bars, 2026-10-22

**`python probe.py` clears, exit 0.** Twelve bars: the nine field bars carried, and three new ones on the
`cards-gaspuff` isolation card, each outside the spread of three fresh card sets of this build.

## The owner's complaint

> cards-gaspuff is still a flat green puddle and the look report is empty, so the copied field stills pass. Draw
> that card as one standing puff with a rim, like cards-smoke, shoot three isolation cards, and bar its rise, its
> aspect, and how much it stands above the smoke card. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g2`'s tip. Bar outside the spread of three captures of the
> current build.

He is right three times over, and all three were real:

1. **The puddle.** `GasBankCards.Drawer` drew the gaspuff card at `CardWidth * CombatFx.GasPuffStand`, 12 m wide
   by 3.84 m tall. That squash IS the puddle.
2. **The empty report.** `card_rise_m` took each pixel's OWN ground distance and then its height in that same
   plane — 0 by construction. That is why `-g1`'s table printed rise 0.000 m for all four cards.
3. **Nothing barred.** The card report was printed and believed, never checked, so a puddle passed.

## What is here

- `probe.py` — carried from `-g1-g2` with three changes: `column_heights(...)` (the shared column maths, the fix
  for fault 2), `card_measure(dir, stem, ink)` (the card measure, with an INK mask as well as the green one) and
  `CARD_BARS` (the three new bars, through the same `spread_check` as the field's). The camera maths, the green
  mask, `largest_blob`, the O240 clouds, `--pack`, `--calibrate`, `--selftest` and the nine field `BAR_VALUES`
  are untouched.
- `run1/`, `run2/`, `run3/` — **carried from `-g1-g2` unchanged**, the `GasBankStills` captures at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 (T1 frames 804 / 819 / 834, O240 frames 341 / 356 / 371). This unit changes
  one TEST file and this probe; the field draw (`CombatFx`) is NOT touched, so these three are still captures of
  the current build and their nine bars still hold. The fresh captures of this unit are the card sets.
- `cards1/`, `cards2/`, `cards3/` — **shot in this unit**: the five `GasBankCards` stills (`cards-gas`,
  `cards-smoke`, `cards-puff`, `cards-gaspuff`, `cards-plate`) with sidecars, at `TW_CARDS_WAIT_FRAMES`
  0 / 15 / 30, so `cards-gaspuff` frames **147 / 162 / 177** — three frames, not one copied three times. Two sets
  on one frame are refused (`same_card_frame_refusal`).
- `probe.json` — the runs' numbers, the frames, the full card report and `BAR_VALUES`.

## Why the card is 12 m while the field puff stays 0.32

The card now draws at the full `CardWidth`, the same 12 by 12 m box as `cards-smoke` and `cards-puff`, same anchor
(`ground - 0.2`), same pose, same frame 16 — only the book differs. **`CombatFx.GasPuffStand` stays 0.32 and the
field is not redrawn in this unit.** `-g1` measured 0.32 as the only value clearing the owner's own field bars
(0.35 and up break `green_height_frac`'s ceiling 0.15 and `green_low_share`'s floor 0.30; see that folder's
table). So the card is a **look reading of the book's mound and rim at the smoke card's size**, not a picture of
the field puff's size. That is what makes `cards_gaspuff_smoke_rise` meaningful: two cards in one box, one book
apart.

## The rise maths, and its fix

`measure_t1`'s column maths is now `column_heights(mask, w, h, cam, pitch, yaw, ground)`, shared by the field and
the cards. The distance is fixed **per column**, from that column's LOWEST masked pixel (`max(py)`, where the card
meets the ground); then every pixel of that column is measured in that one vertical plane. `MIN_COLUMN_PX` 8, the
p45 per column and the bottom-pixel distance are kept exactly, so no field number moved:

    python -c "import json;d=json.load(open('probe.json'))['runs']['run1'];print(d['green_rise_m'],d['green_height_frac'])"
    2.38 0.0768

the same pair `-g1-g2`'s `probe.json` carries.

## The grey cards, and the ink mask

`cards-smoke` and `cards-puff` are grey: the green mask keeps ~300-500 px of noise on them, so a ratio against
their green rise means nothing. `card_measure` therefore builds its mask either green (as before, kept as a
diagnostic) or **ink** — the plate difference at `T1_DIFF_BAR`, in one blob — which finds every card whatever its
colour. The three bars all read the ink mask. `aspect` is the mask's bounding box `h_px / w_px`: every card is
drawn in one pose at one spot, so it compares between cards and between sets. A puddle is near 0.2, an upright
puff near 0.9.

The card set 1 report, ink mask (and the green mask, which is what `-g1` printed):

| card | book | box | ink rise | ink aspect | ink relief | green rise | green aspect |
|---|---|---|---|---|---|---|---|
| `cards-gas` | `GasBank` | 12 x 12*0.55 | 7.874 m | 0.7355 | 0.3290 | 1.511 m | 0.1878 |
| `cards-smoke` | `SmokeBank` | 12 x 12 | 7.725 m | 0.6360 | 0.3238 | 0.537 m | 0.2024 |
| `cards-puff` | `Puff` | 12 x 12 | 7.849 m | 0.6844 | 0.3182 | 0.486 m | 0.2024 |
| `cards-gaspuff` | `GasPuff` | **12 x 12** | **8.769 m** | **0.8354** | 0.1276 | 7.922 m | 0.7974 |

The pre-change card read green aspect **0.19** — the puddle the owner saw. It now reads 0.80 green / 0.84 ink.

## The bars

Every bar is checked twice: all three sets (or runs) on the pass side, AND the nearest value clearing the bar by at
least their own spread (max - min).

| key | cmp | bar | the three | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.4136 / 0.4242 / 0.4235 | 0.0106 | 0.1136 | owner-decided: the gas is a man's gas |
| `green_height_frac` | le | 0.15 | 0.0768 / 0.0741 / 0.0757 | 0.0027 | 0.0732 | owner-decided ceiling: not a wall |
| `green_height_floor` | ge | 0.05 | 0.0768 / 0.0741 / 0.0757 | 0.0027 | 0.0241 | carried: not a puddle |
| `green_area_frac` | ge | 0.04 | 0.0520 / 0.0508 / 0.0511 | 0.0012 | 0.0108 | carried: the puff is there to be seen |
| `green_rise_m` | ge | 2.0 | 2.380 / 2.304 / 2.353 | 0.0760 | 0.3040 | carried: the height in metres |
| `o240_green_area_frac` | ge | 0.008 | 0.0093 / 0.0093 / 0.0098 | 0.0005 | 0.0013 | carried: the overview's clouded pixels |
| `o240_cloud_count` | ge | 10 | 20 / 15 / 20 | 5 | 5 | carried: clouds, not a wash |
| `o240_cloud_share` | ge | 0.85 | 0.8681 / 0.8696 / 0.8847 | 0.0166 | 0.0181 | carried |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |
| `cards_gaspuff_rise_m` | ge | **8.0** | 8.769 / 9.189 / 8.598 | 0.5910 | 0.5980 | NEW: the card STANDS, in metres. The old squashed card was 3.84 m tall in total, so it cannot reach this at all |
| `cards_gaspuff_aspect` | ge | **0.70** | 0.8354 / 0.9048 / 0.9264 | 0.0910 | 0.1354 | NEW: upright, not a puddle (the pre-change card read 0.19 green) |
| `cards_gaspuff_smoke_rise` | ge | **0.90** | 1.1351 / 1.1761 / 1.0600 | 0.1161 | 0.1600 | NEW: "how much it stands above the smoke card" — the same box, one book apart, so near 1.0 is the right answer |

Each new bar is the round number at or below `min - (max - min)`, exactly as the nine field bars were set, and
never from one set. The green rows and the old gas/smoke relief ratios stay printed as unbarred diagnostics.

O240 did not move, by design: outside `BankMergeZoom` 120 the bank is still `GasBankSquash`'s squashed blanket and
only the close camera draws `Book.GasPuff`.

## The capture recipe

The three card sets, with this checkout's editor closed and the project free (`python Tools/editor_lock.py
status`), one launch at a time; no plate pass (the test shoots its own plate):

```bash
TW_STILLS_DIR=%TEMP%/tw-cards-1 TW_CARDS_WAIT_FRAMES=0 \
python Tools/pipeline/run_detached.py start cards-1 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults <out>/cards-1.xml -logFile <out>/cards-1.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g2-g1/probe.py --pack %TEMP%/tw-cards-1 --into cards1
```

Sets 2 and 3 are the same line at `TW_CARDS_WAIT_FRAMES` 15 and 30, into `cards2` and `cards3`: three waits are
three frames, and the probe refuses two sets that photographed one frame. The run stills are not reshot here; see
`-g1`'s README for their recipe.
