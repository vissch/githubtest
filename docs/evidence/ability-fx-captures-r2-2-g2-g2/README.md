# ability-fx-captures-r2-2-g2-g2: count the green, bar it outside three different frames, 2026-10-21

**`python probe.py` clears, exit 0.** Part 1 replaced the measure (green the gas ADDED over the plate, not
brightness) and added the frame guard, with no bars set. Part 2 (this leg) made `GasBankStills`' wait a knob, shot
three runs of this build at three different frames, and set `BAR_VALUES` from their spread.

## The owner's complaint

> What the probe calls the bank is not the stripes a player sees. The hue window is 25 px and the barred mask is
> 5956 of 63356 plate-difference pixels. `bank_area_frac` 0.0065 is not a bar. `bank_low_share`,
> `bank_height_frac` and `o240_area_frac` are 0.4267, 0.0950 and 0.0142 on all three runs, and those runs are
> frame 804 and frame 341. The o240 bar of 0.010 sits 0.0042 under that one number. Count the green in the frame,
> and put the bar outside the spread of three captures that are not the same frame.

Both faults were real. `-g2` called "the bank" any pixel that differed from the plate and was brighter than it —
a brightness test, no colour in it. And its three runs photographed the SAME frame (t1 804, o240 341), so their
spread was 0 by construction and every `margin >= spread` check was free.

## What is here

- `probe.py` — the measure. `greenness(r,g,b) = g - (r+b)/2`; a pixel is gas when its greenness is `GREEN_BAR`
  (8) or more ABOVE the SAME pixel's greenness in the no-gas plate, so the night field's own green never counts.
  T1 runs `largest_blob` on that green mask (one bank, one patch); O240 does not (five calls 40 m apart are five
  patches). The camera maths (`ray`, `ground_distance`, `height_at`, `selftest`) and `largest_blob` are carried
  from `-g2` unchanged. `gas_hue_window_px` and `hue_window_count()` — the 25 px fake — are gone.
- `run1/`, `run2/`, `run3/` — three runs of the four `GasBankStills` captures (`gas-t1`, `gas-t1-plate`,
  `gas-o240`, `gas-o240-plate`) plus their sidecars and `manifest.json` / `manifest-plate.json`. Same build, same
  pose, three different waits, so three different frames. Every manifest row: `books_ready` true, `gas_cards`
  774/769/763 (t1) and 119/123/131 (o240), `calls_taken` 1 (t1) / 5 (o240), `pose_error_m` 0.
- `cards/` — the four `GasBankCards` isolation stills, carried from `-g2`, for the unbarred look report only.
- `probe.json` — all three runs' numbers, frames, the look report and `BAR_VALUES`.

## Three different frames, and the guard

`GasBankStills.WaitFrames` became `DefaultWaitFrames` 200, overridable by SessionState `tw.gasbank.waitframes`
or `TW_GAS_WAIT_FRAMES`, clamped to 120..300 (the cloud lives ticks 60-300 after the call, 1.5 frames a tick).
Both passes of a run read the same value, so the plate keeps hitting the shot's frame; every manifest row now
carries `wait_frames`, so the evidence itself shows the three runs asked for three frames.

| run | `wait_frames` | t1 frame (tick) | o240 frame (tick) |
|---|---|---|---|
| `run1` | 200 | 805 (557) | 342 (249) |
| `run2` | 215 | 820 (567) | 357 (259) |
| `run3` | 230 | 835 (577) | 372 (269) |

Before any bar is looked at, `probe.py` reads the four sidecars' `frame` numbers of every run. Two runs sharing a
`gas-t1` frame, or a `gas-o240` frame, are not three captures: it prints `same frame: runN and runM both
photographed frame F` and exits 1. `--calibrate` prints the frames and does not refuse, so numbers can be read
while the rest of the captures are still being shot.

## The bars

Every bar is checked twice: all three runs on the pass side, AND the nearest run value clearing the bar by at
least the three runs' own spread (max - min). The two owner-decided bars are kept as they are — the green
readings clear them by far more than the spread.

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.30 | 1.0000, 1.0000, 1.0000 | 0.0000 | 0.7000 | owner-decided; every green pixel is under head height (`HEAD_M` 1.8). The bank lies on the ground: median rise 0.359 m. |
| `green_height_frac` | `le` | 0.15 | 0.0116, 0.0116, 0.0115 | 0.0001 | 0.1384 | owner-decided; the bank is 1.2% of the frame's height, not a column |
| `green_area_frac` | `ge` | 0.005 | 0.0055, 0.0056, 0.0055 | 0.0001 | 0.0005 | the round number under the spread-adjusted floor 0.0054 |
| `o240_green_area_frac` | `ge` | 0.008 | 0.0108, 0.0106, 0.0111 | 0.0005 | 0.0026 | 0.010 would have cleared by 0.0006 against a real spread of 0.0005 — too close to call, so the next round number down |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |

Nothing is barred on a plate-difference count any more. `green_px`, `plate_diff_px`, `blob_px`, `o240_green_px`
and `o240_plate_diff_px` are printed per run as named diagnostics and barred on nothing.

## What the green count sees that the hue window did not

The dropped 25 px hue window passed 25 of 63,356 plate-difference pixels. The green count passes **46,471 /
47,236 / 46,945** at T1 and **9,909 / 9,805 / 10,260** at O240 — tens of thousands of pixels the gas turned green
over the plate. `green_area_frac` is the T1 share AFTER `largest_blob` (5,109 / 5,146 / 5,080 px of a 1280x720
frame): the bank as one coherent patch, with the tracer flashes and infantry edges elsewhere in the frame left
out.

## The look report (unbarred)

From `cards/`: gas relief 0.0868 against the smoke card's 0.1933 (ratio 0.4490); both cards sit at ground level in
this isolation pose, so the rise ratio is 0/0 and says nothing. This is a look reading, not a bar — the gas card
is squashed by `CombatFx.GasBankSquash` 0.55 on purpose, and changing how the bank LOOKS is out of this unit's
scope.

## Status: done

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (the camera hand calculation, 0.34311 m).
