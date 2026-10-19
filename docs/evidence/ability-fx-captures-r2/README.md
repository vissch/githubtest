# ability-fx-captures-r2: the chlorine bank, 2026-10-19

**The result, after r2-2: THE BANK IS IN THE PICTURE. r2's "no gas bank at all" was wrong about the fact, and the
owner's option (a) — a capture-path fault — is wrong about the cause.** The bank is drawn, it reaches the capture,
and it covers 11% of the T1 frame. What is broken is **`probe.py`'s green test**: at night the bank does not arrive
green at all. Of 921,600 pixels in `gas-t1.jpg` exactly **one** has more green in it than red and blue. The green a
human reads in the still is contrast against a deep blue field, not green light.

## What the pictures are

| still | what it is |
|---|---|
| `gas-t1.jpg` | the play zoom (30), chlorine called at the frame's centre, ability-area marker hidden |
| `gas-t1-plate.jpg` | the same pose at the same frame of the same script, with the gas call skipped |
| `gas-o240.jpg` | the overview (240), five calls 40 m apart along the corridor |
| `gas-o240-plate.jpg` | its plate |
| `cards-gas.jpg` | r2-2's isolation still: ONE hand-packed GasBank card, no sim, no field, no ability (`GasBankCards`) |
| `cards-smoke.jpg`, `cards-puff.jpg` | the same card from the SmokeBank and Puff books, same pose, same frame |
| `cards-plate.jpg` | the same pose with no card drawn at all |

## The three measurements that settle it

**1. A single GasBank card drawn by hand reaches a batch-mode `Camera.Render`.** `GasBankCards` makes its own
`FlipbookFx`, submits one packed card from an ordinary `Update`, and shoots it. Against the plate
(`sheetink.py --blobs`): gas 99,843 changed pixels, smoke 115,738, puff 68,932 — all three books draw, and the gas
card is plainly visible as a patch lying on the ground in `cards-gas.jpg`. So neither `FrameBudget.Draw`, nor the
`Bank`/`Deep` sheet, nor `CaptureRig`'s own `Camera.Render()` loses a card.

**2. The sheet has ink where the book reads it.** `sheetink.py GasBank.png 8 4 30`: frames 17-28 — the window
`CombatFx.GasBankFirst`/`GasBankLast` draws — carry mean alpha 0.167 to 0.183, the fullest of the whole sheet.

**3. The cards are there in the frame that was photographed.** `GasBankStills` now writes the capture frame's own
reading into `manifest.json`. At T1: `gas_cells` 775, `gas_cards` 774, `recipes` 1, `books_ready` true,
`draw_calls` 133, `gas_field_max` 36. At O240: 395 cells, 119 cards. (r2's 133-card reading came from an ordinary
play frame, which is why it proved nothing.)

**So the bank is drawn.** Against its plate, `gas-t1.jpg` changes **101,253 pixels, 11.0% of the frame**.

## Why the probe still reads zero

`probe.py`'s `gas_mask` keeps a pixel only if it changed from the plate AND its hue is in 45-95 (PIL, the chlorine
tint 0.74/0.80/0.34) at saturation >= 50 and value >= 30. Of the 101,253 changed pixels:

| bar | pixels that pass |
|---|---|
| value >= 30 | 101,219 |
| saturation >= 50 | 97,516 |
| **hue 45-95** | **193** |
| hue 95-160 (green-cyan to blue) | 92,713 |

The median hue of the gas pixels is 148 (PIL; about 209 degrees, a blue-cyan), and of the thickest one per cent,
127. Under the night light the bank arrives as a pale blue-cyan haze over a darker blue field. The hue window is the
only bar it fails, and it fails it by a mile.

## What is left

The bank's presence is already proven by subtraction from a plate at the same pose and the same sim tick — that is
what the plate is for — so the measure does not need the hue test at all. Changing `gas_mask` to measure the
*changed* mask, with the hue test kept only as a secondary reading, is a change to the MEASURE and to no look; it is
the next step (r2-2's plan, step 5). Growing, brightening or unsquashing the bank (the owner's option (b)) is still
not built and still not asked for.

## Rerunning it

```bash
# from the repo root, this checkout's editor closed; the pipeline's run_tests selects no [Explicit] test
python docs/evidence/ability-fx-captures-r2/sheetink.py \
  trench-warfare-3d/Assets/_Project/Resources/VFX/GasBank.png 8 4 30

TW_STILLS_DIR='C:\...\tw-gasbank' "/c/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" \
  -batchmode -projectPath "$(pwd -W)/trench-warfare-3d" -runTests -testPlatform EditMode \
  -testFilter GasBankCards -testResults Logs/cards.xml -logFile Logs/cards.log
python docs/evidence/ability-fx-captures-r2/sheetink.py --blobs <dir>/cards-gas.png <dir>/cards-plate.png

# the same line with -testFilter GasBankStills, once plain and once with TW_GAS_PLATE=1, then:
python docs/evidence/ability-fx-captures-r2/probe.py --pack <that folder>
python docs/evidence/ability-fx-captures-r2/probe.py --selftest   # the camera maths against a hand calculation
```

`probe.json` as it stands, from the run above, is **red on purpose**: it is the reading of a measure that cannot see
the bank it is pointed at.

| number | measured | the bar |
|---|---|---|
| `gas_t1_green_low_share` | 0.84 of 25 pixels — the hue window's leavings | >= 0.30 |
| `gas_t1_height_frac` | -0.0 (25 pixels is nothing to measure a height from) | <= 0.15 |
| `gas_o240_effect_area_frac` | 0.0275 | >= 0.03 |
| `pose_error_max` | 0.000 | < 0.50 |
