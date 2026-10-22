# Gas bank captures — three poses 60 frames apart (unit -g1-g1-g2-g1-g2)

The owner, 2026-10-22:

> The overview cloud-count ceiling of 15 clears the three runs 13, 13 and 11 by 2, and that spread is 2, so the
> bar sits on the spread. Those stills are one pose (frames 341, 356 and 371). Reshoot three captures far enough
> apart that the bank moves, and put the ceiling outside that spread.

**No production change in this unit.** The build is commit `093b0be1b4`'s (`GasCloudWiden` 5). What moved is the
captures and the bars.

## What was shot
`GasBankStills` twice per run into one temp dir (gas pass, then `TW_GAS_PLATE=1` plate pass), at
`TW_GAS_WAIT_FRAMES` 150 / 210 / 270 — the widest spacing inside the clamp 120..300 (`GasBankStills.MinWaitFrames`).

| run | wait | o240 frame | t1 frame | calls_taken | pose_error_m |
|---|---|---|---|---|---|
| run1 | 150 | 291 | 754 | 5 | 0 |
| run2 | 210 | 351 | 814 | 5 | 0 |
| run3 | 270 | 411 | 874 | 5 | 0 |

60 frames apart at both bands, against the 15 of the rejected set.

## The two new guards (the probe's only changes from -g1's)
- `MIN_FRAME_GAP = 45`: any pair of runs closer than 45 frames at `gas-t1` or `gas-o240` is **one pose** and the
  probe exits 1. The rejected set is 15 frames apart, so it fails by 30. These runs are 60 apart.
- `MOVE_OVERLAP_BAR = 0.7021` with `o240_green_mask` + `move_refusal`: the pairwise Jaccard of the three O240
  green masks proves the **bank** moved, not just the frame counter. The rejected one-pose set's worst pair is
  **0.7404**; this unit's worst pair is **0.6638**; the bar is midway. New runs OK, old set refused.

## The bars
Re-read from these three runs, each clearing by more than their own spread (`python probe.py` prints every line).
`green_height_frac` 0.15 (owner), the cloud-count floor 3 and `pose_error_max` 0.50 are carried unchanged.

## RED, and why — the owner's two bars
At 60-frame spacing the bank is at three stages of its life, so two owner-set bars no longer hold:

| bar | runs | spread | what it needs |
|---|---|---|---|
| `green_low_share` ge **0.30** (owner) | 0.2317 / 0.3473 / 0.3042 | 0.1156 | 0.1161 or lower |
| `o240_cloud_ceiling` lt **15** (owner) | 15 / 13 / 13 | 2 | 17 or higher |

run1 (the youngest bank, wait 150) is still rising, so under a third of its green sits below a man's head, and it
draws 15 counted clouds — exactly at the owner's ceiling. Lowering an owner bar, or raising the ceiling above
"under 15", is his call: `docs/reference/decisions.md`, "Open: waiting on the owner". Every other bar is green and
both new guards pass, so `python probe.py` exits 1 on those two lines alone.
