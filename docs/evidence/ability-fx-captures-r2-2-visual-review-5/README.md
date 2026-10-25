# ability-fx-captures-r2-2-visual-review-5: the fix chain, seen, 2026-10-25

The owner asked for one thing on the Decide page, answering "ability-fx-captures-r2-2: chain of fix units 8 deep,
26 units": **"show me visual results"**. This folder is that answer and nothing else. No bar was re-set, no measure
was written, no gas was changed.

**Look at `chain-sheet.jpg`.** Nine tiles, oldest build top-left to newest bottom-right, every one the same shot of
the same night corridor at the same moment (T1 pose, `sim_tick` 557). `chain-sheet-o240.jpg` is the same nine from
the overview. The number under each tile is that build's `bank_paint_share` — how much of the gas bank's own area
is actually painted, the key the owner's complaint was about.

What the sheet shows: **the chain worked, and it worked in the last two steps.** The first six tiles are a faint
blue-grey haze lying in stripes; the last two are one solid green mass sitting on the field. The share goes
0.08 -> 0.19 -> 0.20 -> **0.75**.

## One line per bank, from that bank's own README

| Tile | What that unit changed | paint share |
|---|---|---|
| `r2` | the first shoot: the bank IS in the picture; `probe.py`'s green test is what was broken | 0.0770 |
| `g2` | a gas measure three captures can agree on — measure only, build untouched | 0.0782 |
| `g2-g1` | **build moved** (`CombatFx.Banks.cs` `5c0c33620a`): wide overlapping cards at low alpha | 0.1855 |
| `g2-g1-g2` | three captures on three frames, a bar on the paint — measure only | 0.1855 |
| `g2-g1-g2-g2` | paint read by the DIRECTION colour lifts in, not hue — measure only | 0.1855 |
| `g2-g1-g2-g2-g2` | the test now refuses smoke and the puff card — measure only | 0.1855 |
| `...-g2-g1` | **build moved** (`3d7aa478ea`): one low bank, not a sheet of stripes | 0.2037 |
| `...-g1-g1` | **build moved** (`71ce0ccd73`): the gaps closed, every card one 1.72 m band | 0.7488 |
| `now` | this leg: the same build rebased onto integration (181 commits), freshly shot | 0.7496 |

**Read the middle of that table honestly:** four of the eight units changed only the MEASURE, so their tiles are
pixel-for-pixel the same picture. That is not a fault — those units were building a probe that could tell gas from
night field at all — but it is why "8 deep, 26 units" does not mean eight visible steps. Three units moved the
build. The last one did most of the visible work.

## What this leg actually did

- Rebased the lane onto the integration branch, 181 commits, including a hand merge in `CombatFx.Banks.cs`.
- Shot the rebased build once, in batch mode with the GPU: plate pass then gas pass, `GasBankStills`.
  `now/manifest.json`: `books_ready` true, `pose_error_m` 0, `calls_taken` 1 at T1 and 5 at O240, `gas_cards` 774.
- **The rebase did not change the look.** `now` reads 0.7496 against the pre-rebase build's 0.7488 on the same
  sim tick — the band values survived the merge whole.

## What this is not

- **No bar was re-set and no new probe was written.** Every number here was read off one
  `probe.py --against <bank>` per bank, run out of the `-g1-g1` folder. That command always exits 1 by design
  (an old build must fail the current bars), so the numbers come from its report, not from an exit code.
- `now/` holds one run, not three, so it cannot clear the spread rule and was never asked to. It is a picture.
- **The puff brief is still open.** Nothing here answers it.
- **This lane does not land.** It is pushed for the owner to look at.
