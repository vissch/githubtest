# ability-fx-captures-r2-2: the pictures, 2026-10-25

The owner asked for visual results, not another bar. This folder is a **gallery**: the chlorine gas bank as the
build draws it now, reshot after the lane was rebased onto the integration branch at `de7402b2d4`. No code changed on
this leg and no bar was retuned.

## The pictures

| picture | what it shows |
|---|---|
| `run1/gas-t1.jpg` | the play zoom (T1, zoom 30) at night: the gas bank over the trench line |
| `run1/gas-t1-plate.jpg` | the same pose, same frame, **no gas** — hold it beside the one above to see what the bank paints |
| `run1/gas-o240.jpg` | the overview (O240): the whole corridor, five calls down the line |
| `run1/gas-o240-plate.jpg` | the overview with no gas |
| `cards/cards-gas.jpg` | one hand-packed gas card, a book on its own: the bank's building block |
| `cards/cards-smoke.jpg` | the same pose with the smoke card instead |
| `cards/cards-puff.jpg` | the same pose with the puff card instead |
| `cards/cards-plate.jpg` | the same pose with no card at all |

Each `.jpg` has its `.json` sidecar (pose, tick, draw calls); `run1/manifest.json` and `manifest-plate.json`
record what the sim took. The capture frame holds **774 gas cards** at T1 and 119 at the overview, so the bank is
drawn and reaches the capture camera.

## The numbers, reported and not barred

One frame set only: three frames exist to measure a spread, and no new bar is being set here. `probe.py` is
copied unchanged from `../ability-fx-captures-r2-2-g2-g1-g2-g2/` (same bars, same `GAS_DIR`/`GAS_COS`);
`python probe.py --calibrate` printed these.

| key | this reshoot (run1) | the landed bar | the landed g2-g2 run1 |
|---|---|---|---|
| `bank_low_share` | 0.4371 | ge 0.07 | 0.4371 |
| `bank_height_frac` | 0.0799 | le 0.27 | 0.0799 |
| `o240_area_frac` | 0.0126 | ge 0.010 | 0.0130 |
| `pose_error_max` | 0.0000 | lt 0.50 | 0.0000 |
| `gas_paint_frac` | 0.0146 | ge 0.0130 | 0.0146 |
| `bank_paint_share` | 0.1849 | ge 0.12 | 0.1855 |
| `bank_patch_count` | 11 | le 25 | 10 |
| `bank_width_m` / `bank_rise_m` / `bank_aspect` | 9.071 / 1.845 / 4.918 | unbarred | 9.071 / 1.845 / 4.918 |

**Reported, not barred.** The rebase pulled in upstream work on `CombatFx.Ground.cs` and `TacticalCamera.cs`, so
the reshoot was needed to be sure the look had not moved: it has not. The landed three-run probe
(`python docs/evidence/ability-fx-captures-r2-2-g2-g1-g2-g2/probe.py`) is still green on the rebased tree.

`bank_aspect` 4.9 is the same honest answer as before: the bank is **wide and low — a sheet along the trench,
not a puff**. That is what the pictures show, and it is the thing still waiting on the owner.

## Still yours to pick

The open row in `docs/reference/decisions.md` ("The gas bank: fewer, shallower, denser cards cannot hold
`bank_paint_share`", 2026-10-21), its three options unchanged:

- **(a)** keep the g2 build and lower nothing — the default taken so far; nothing was landed and
  `CombatFx.Banks.cs` is reverted.
- **(b)** drop `bank_paint_share` from the bars and bar the bank's shape instead (`bank_aspect`, 6.2 on the low
  build against 2.7-4.9 on g2).
- **(c)** redraw the bank as one mesh or a few large cards instead of one card per 4 m cell, which is a new look
  and so a concepts brief first.
