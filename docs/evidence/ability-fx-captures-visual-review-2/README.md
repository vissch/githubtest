# ability-fx-captures-visual-review-2: the chlorine bank, rebased, 2026-10-27

The owner's word on the Decide page to "ability-fx-captures-r2-2: chain of fix units 9 deep, 26 units" was
**"show me visual results"**. This folder is that: three fresh captures of the lane **after it was rebased onto
the integration branch** (181 commits of integration are now under it), shot with the bars exactly as the last
fix unit left them. Nothing was re-tuned here — the pictures judge the rebase, not another round of bar fitting.

Build: lane commit `adbedb67c0`, rebased, 0 behind `origin/claude/trench-warfare-2d-3d-plan-idt7lf`.

## How they were shot

Six batch launches of `GasBankStills` (EditMode, `-batchmode`, no `-nographics`), one at a time:
`TW_GAS_EXTRA_FRAMES` 0, 30, 60, each run shot twice into the same `TW_STILLS_DIR` — once plain, once with
`TW_GAS_PLATE=1` for the empty plate. t1 `sim_tick` **557 / 597 / 637**, three different frames, as the probe
demands. (The sixth launch stalled at editor start on a stale lockfile; it was stopped, the lockfile removed, and
it ran clean on the retry. No still is from the stalled attempt.)

## The three runs

| key | run1 (t1 557) | run2 (t1 597) | run3 (t1 637) | bar |
|---|---|---|---|---|
| `bank_patch_count` | 1 | 2 | 1 | le 3 |
| `bank_height_frac` | 0.3845 | 0.3576 | 0.3484 | 0.313 - 0.421 |
| `bank_rise_m` | 8.602 | 8.024 | 7.808 | — |
| `bank_paint_share` | 0.9362 | 0.9266 | 0.9255 | ge 0.913 |
| `bank_low_share` | 0.0580 | 0.0683 | 0.0678 | ge 0.047 |
| `bank_aspect` | 0.784 | 0.905 | 0.387 | le 1.423 |
| `o240_area_frac` | 0.0212 | 0.0242 | 0.0251 | ge 0.018 |
| `gas_paint_frac` | 0.0182 | 0.0177 | 0.0172 | ge 0.016 |
| `gas_green_share` | 0.5344 | 0.5267 | 0.5310 | ge 0.517 |
| `pose_error_max` | 0.0000 | 0.0000 | 0.0000 | lt 0.5 |

`gas_cards` at t1 (manifest): 774 of `MaxCards` 1536, same as before the rebase.

## The verdict: `python probe.py` exits 1, on the margin rule only

Every key is inside its bar. Two keys fail the probe's **margin rule** — a value must clear its bar by at least
the runs' own spread — by a hair:

| key | value nearest the bar | bar | margin | spread needed | short by |
|---|---|---|---|---|---|
| `o240_area_frac` | 0.0212 (run1) | ge 0.0180 | 0.0032 | 0.0039 | 0.0007 |
| `bank_height_frac_lo` | 0.3484 (run3) | ge 0.3130 | 0.0354 | 0.0361 | 0.0007 |

Both bars were fitted to the pre-rebase runs at exactly 1.00x spread, so a change of 0.0004 in a measured value
flips them. Against the pre-rebase captures the picture is unmoved: `bank_height_frac` 0.3845 / 0.3576 / 0.3484
against 0.3848 / 0.3580 / 0.3490, `bank_paint_share` 0.9362 / 0.9266 / 0.9255 against 0.9392 / 0.9261 / 0.9277.
**The rebase did not change the gas look.** No bar and no code was touched here (the unit's word: show results,
not a tenth fix).

The old pale sheet still fails hard:
`python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g2` exits 1 with
`bank_patch_count` 5 / 7 / 4 and `bank_height_frac` 0.1118 / 0.2096 / 0.1375. `python probe.py --selftest`
exits 0. The card rejection still holds: `cards-smoke` and `cards-puff` are refused by 55x and 53x the spread.

## What to look at

`run1/gas-t1.jpg`, `run2/gas-t1.jpg`, `run3/gas-t1.jpg` — the standing bank at its thickest, one green mass
over the mud at night. `run*/gas-o240.jpg` — the same bank 300 ticks earlier, while it is still spreading.
`run*/*-plate.jpg` are the empty plates the probe subtracts.

Out of scope: the smoke bank, the `GasBank.png` sheet art, new `cards/` isolation stills (`CARDS_DIR` still
points at the older folder), a film, and any change to the bars or the gas look.
