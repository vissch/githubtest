# ability-fx-captures-r2-2-visual-review: the gas bank, as pictures, 2026-10-27

The owner's answer to "ability-fx-captures-r2-2: chain of fix units 7 deep, 26 units" was **"show me visual
results"**. This folder is those pictures: a fresh capture set on the **rebased** build (the lane sat 181 commits
behind, so the predecessors' "no reshoot" shortcut no longer held) and `sheet.py`, which draws the same stills the
probe measures.

**This lane changes no look.** Every number and every outline here comes from the build as it already stands; the
only new code is the drawing.

## The sheets

| file | what it is | what to look at |
|---|---|---|
| `sheets/contact.png` | **start here** — the three runs stacked, play zoom | the bank is a SHEET along the trench on all three frames, not one lucky capture |
| `sheets/run1-t1.png`, `run2-t1.png`, `run3-t1.png` | one run each, play zoom (T1) | three panels: the gas still, the no-gas plate, the plate difference boosted **x4** |
| `sheets/contact-o240.png`, `run<N>-o240.png` | the same for the 240 m overview | five separate calls 40 m apart, each its own patch |

**Why the boosted panel.** These are night stills. Raw, the gas reads almost black: open `run1/gas-t1.jpg` and
you see a dark field. The gas still minus its no-gas plate, times four, is what makes the bank visible to a
person — that panel is the whole point of the unit. The yellow box is the probe's **own** largest coherent patch
(`probe.bank_mask(..., blob=True)`), not a box drawn by hand, and the label under each row carries that run's
`bank_width_m` / `bank_rise_m` / `bank_aspect` / `gas_paint_px` straight from `probe.measure_t1`. `sheet.py`
imports every number from `probe.py` and derives none of its own, so the picture and `probe.json` cannot drift
apart.

**What a sheet should look like:** about 9 m of bank along the trench, rising 1.5-3.5 m (a man's head is 1.8 m),
so an aspect of roughly 3 to 6 — wide and low, lying on the ground, which is what the whole r2-2 chain was for.

| run | t1 `sim_tick` | `bank_width_m` | `bank_rise_m` | `bank_aspect` |
|---|---|---|---|---|
| run1 (+0 extra frames) | 557 | 9.071 | 1.845 | 4.918 |
| run2 (+30) | 597 | 9.223 | 3.443 | 2.679 |
| run3 (+60) | 637 | 9.341 | 1.575 | 5.930 |

## The probe's verdict on the rebased build: FAILED, and that is the honest result

`python probe.py` exits **1**. The bar values and the probe itself are copied unchanged from
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g2/` (commit `87c6d95beb`). **Nothing here was re-tuned.** The owner
asked for pictures, not a seventh bar.

**No value crossed a bar.** Every one of the eight keys still sits on the pass side of its own bar. What fails is
the probe's SPREAD rule — a bar must be cleared by at least the three runs' own spread — and the spread grew,
because run3 (+60 frames, tick 637) caught the bank at a lower, flatter moment on this build: `bank_rise_m`
1.575 where the landed set read 3.094. One wider run widens every spread at once.

| key | landed (`87c6d95beb`) runs | spread / margin | this build's runs | spread / margin | verdict |
|---|---|---|---|---|---|
| `bank_low_share` | 0.4371 / 0.2584 / 0.2853 | 0.1787 / 0.1884 | 0.4371 / 0.2585 / **0.4592** | 0.2007 / 0.1885 | FAILED (margin < spread) |
| `bank_height_frac` | 0.0799 / 0.1711 / 0.1608 | 0.0912 / 0.0989 | 0.0799 / 0.1711 / **0.0698** | 0.1013 / 0.0989 | FAILED (margin < spread) |
| `o240_area_frac` | 0.0130 / 0.0150 / 0.0153 | 0.0023 / 0.0030 | 0.0126 / 0.0148 / 0.0153 | 0.0027 / 0.0026 | FAILED (margin < spread) |
| `pose_error_max` | 0.0000 x3 | 0.0000 / 0.5000 | 0.0000 x3 | 0.0000 / 0.5000 | OK |
| `gas_paint_frac` | 0.0146 / 0.0151 / 0.0153 | 0.0007 / 0.0016 | 0.0146 / 0.0150 / 0.0142 | 0.0008 / 0.0012 | OK |
| `gas_green_share` | 0.1535 / 0.1426 / 0.1495 | 0.0109 / 0.0176 | 0.1537 / **0.1352** / 0.1503 | 0.0185 / 0.0102 | FAILED (margin < spread) |
| `bank_paint_share` | 0.1855 / 0.1622 / 0.1936 | 0.0314 / 0.0422 | 0.1849 / **0.1528** / 0.2006 | 0.0478 / 0.0328 | FAILED (margin < spread) |
| `bank_patch_count` | 10 / 13 / 7 | 6 / 12 | 11 / **16** / 6 | 10 / 9 | FAILED (margin < spread) |

The card and zebra rejections report NOT REJECTED for the same reason and no other: their distances are measured
in spreads, and they are unmoved in absolute terms (`cards-puff` is still 0.0174 under the floor, the zebra still
0.0128 over the ceiling) — only the divisor grew.

So: **the look is where it was** (`bank_width_m` 9.071 / 9.223 / 9.341 against the landed 9.071 / 9.223 / 9.351;
`gas_paint_px` 13,430 / 13,835 / 13,057 against 13,477 / 13,884 / 14,137 — the two measures a reviewer actually
looks at), and what the probe is telling us is that a three-capture spread is a fragile yardstick when one of the
three can land on a different phase of the cloud's life. Fixing that is a bar change, which this unit does not
make.

## Rerunning it

```bash
cd trench-warfare-3d && python Tools/editor_lock.py claim visual-review --minutes 60
# six passes, one after another (one editor per checkout): run1/run2/run3 x gas,plate
TW_STILLS_DIR=<scratch>/run1 TW_GAS_EXTRA_FRAMES=0 [TW_GAS_PLATE=1] \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode -projectPath <checkout>/trench-warfare-3d \
  -runTests -testPlatform EditMode -testFilter GasBankStills -testResults <xml> -logFile <log>
# ... the same with TW_GAS_EXTRA_FRAMES 30 and 60 (three different frames: the probe refuses three of one frame)
python probe.py --pack <scratch>/run1 --into run1      # and run2, run3
python probe.py            # the bars
python sheet.py --selftest # the pictures, then the assertions
```

No `-nographics`: the passes must render. `sheet.py --selftest` exits 0 — it checks each run's `gas_paint_px`
against 8,000-20,000 px (the landed README's 13,477-14,137, widened for a build that moved 181 commits) and that
every PNG it names was written.

## No `cards/` here

The isolation stills did not change; `probe.py`'s `CARDS_DIR` and `ZEBRA_DIR` are relative paths that still
resolve from this folder (`../ability-fx-captures-r2-2-g2-g1/cards`, `../ability-fx-captures-r2-2-g2`), so the
card and zebra rejection checks run exactly as they did there. The in-folder look report prints `no cards/`.

## Out of scope

Turning the sheet into a puff (a `CombatFx.Banks.cs` look change), a film of the gas (no harness: `EffectFilmStills`'
six effects do not include gas), re-tuning any bar, and landing the lane.
