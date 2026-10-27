# ability-fx-captures-visual-review: the four sheets

Thirteen rounds of `ability-fx-captures-r2-2-*` argued about numbers and never showed the owner a picture. The
owner's answer on the Decide page to that chain was "show me visual results". This folder changes no production
file and shoots nothing new: it tiles the captures already committed on this lane
(`docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g2/`, commits `ac94229cee` and
`eef5ef5001`), plus the isolation cards and the previous (rejected) round, into four labelled contact sheets. The
pictures and the numbers in `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g2/README.md` are the
same build.

## The sheets

- **`sheet-t1.jpg`** — the game camera. Three runs, no-gas plate on the left and gas on the right. What to look
  at: does the gas read as a continuous green sheet over the trench, not a banded pattern.
- **`sheet-o240.jpg`** — the overview, the shot the owner called a striped rectangle. Same layout. What to look
  at: the gas should be one cloud with no dark mud rows cutting across it.
- **`sheet-cards.jpg`** — the four isolation stills the probe tests the gas signature against: plate, gas
  (accepted), smoke and puff (both rejected). What to look at: gas should look visually distinct from smoke/puff,
  which is what lets the probe tell them apart by number.
- **`sheet-before-after.jpg`** — the one sheet that shows the fix rather than stating it. Top row is the
  previous (rejected, striped) round's run1 at the same two poses (`t1=557`, `o240=249`); bottom row is this
  round's run1 at the identical poses. What to look at: the overview went from a striped rectangle to one cloud.

## The three runs, in numbers

| run | t1 tick | o240 tick | `gas_green_share` | `o240_dark_frac` | `bank_width_m` |
|---|---|---|---|---|---|
| run1 | 557 | 249 | 0.4828 | 0.0093 | 31.248 |
| run2 | 597 | 269 | 0.4431 | 0.0000 | 35.778 |
| run3 | 637 | 289 | 0.4478 | 0.0000 | 35.911 |

Rejected (striped) round, same poses, for the before/after row: `gas_green_share` 0.4357, `o240_dark_frac` 0.1415.

## What is still failing

`gas_green_share` is the one bar still failing. The owner's floor is 0.44 and all three runs clear it
(0.4828 / 0.4431 / 0.4478), but the nearest run (run2, 0.4431) clears it by only 0.0031 while the three runs'
own spread is 0.0397 — the owner's rule is that the bar must sit outside the spread of three captures of the
current build, and by that rule this build has not cleared it. Everything else he asked for is in: the overview
is one cloud (`sheet-o240.jpg`, `sheet-before-after.jpg`), the sheet is back to full width
(`bank_width_m` 31.248-35.911 against the previous round's 16.2-24.2), and `bank_patch_count` is far inside his
cap. Full detail, including the tried-and-rejected `GasBankThin` fallback, is in
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g1-g1-g2/README.md`.

## How this is checked

`sheet.py` builds the four jpgs and `sheet-manifest.json` (every tile's source path, sha1, sheet, cell and the
key it carries) from the table at the top of the module. `sheet.py --verify` rebuilds each sheet into a temp file,
byte-compares it with the committed jpg, and recomputes every source's sha1 against the manifest: the whole test
is that the sheets show the captures already committed on this lane, unaltered, and nothing else.
