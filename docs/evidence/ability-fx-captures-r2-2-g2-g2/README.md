# ability-fx-captures-r2-2-g2-g2: count the green, bar it outside three different frames, 2026-10-21

Part 1 (this commit) rebuilt the measure as a **green count** and added a **frame guard**. No bars are set yet and
`python probe.py` exits 1 on purpose until part 2 shoots three captures at three different frames.

## The owner's complaint (about the `-g2` folder)

> What the probe calls the bank is not the stripes a player sees. The hue window is 25 px and the barred mask is
> 5956 of 63356 plate-difference pixels. `bank_area_frac` 0.0065 is not a bar. `bank_low_share`,
> `bank_height_frac` and `o240_area_frac` are 0.4267, 0.0950 and 0.0142 on all three runs, and those runs are
> frame 804 and frame 341. The o240 bar of 0.010 sits 0.0042 under that one number. Count the green in the frame,
> and put the bar outside the spread of three captures that are not the same frame.

Two faults, both real:

1. **`-g2` never measured green.** Its mask was "any pixel that differs from the plate AND is brighter than it" —
   a brightness test. The hue window it printed was a 25 px curiosity; nothing barred on the colour a player sees.
2. **Its three runs were one capture.** All three photographed `gas-t1` frame 804 and `gas-o240` frame 341, so
   their spread was 0 by construction and every `margin >= spread` check was free.

## What is here

- `probe.py` — the measure. `-g2`'s camera maths (`ray`, `ground_distance`, `height_at`, `selftest`) and its
  blob-finding (`largest_blob`) are carried over unchanged; the mask, the key names and the frame guard are new.
- `cards/` — the four `GasBankCards` isolation stills, carried from `-g2`, for the unbarred look report only.
- `run1/`, `run2/`, `run3/` — **part 2**: three captures of the current build at three different frames. `-g2`'s
  runs were deliberately NOT copied: they are the same frame, which is the fault.
- `probe.json` — every run's numbers, frames and bars, written by `python probe.py --calibrate` / `python probe.py`.

## The measure: green

`greenness(r, g, b) = g - (r + b) / 2` is how far a pixel leans green. A pixel counts as gas when its greenness is
at least `GREEN_BAR` (8) **above the same pixel's greenness in the no-gas plate**, so the night field's own green
does not count — only the green the gas added. That is the colour the player sees.

- **T1** runs `largest_blob` on the green mask: a bank is one patch.
- **O240** does not: five calls 40 m apart are five patches, and a blob there would keep one call.

On `-g2`'s own committed JPEGs this sees **46,473 green pixels at T1** and **9,913 at O240**, where the dropped hue
window saw 25. `changed()` survives only to print `plate_diff_px`; nothing is barred on it.

## The frame guard

`frames_of(run_dir)` reads `frame` from the four sidecars. Before any bar is looked at, two runs sharing a `gas-t1`
frame or a `gas-o240` frame are refused: `same frame: runN and runM both photographed frame F`, exit 1. The goal's
"three captures that are not the same frame" is enforced by the probe, not promised in prose.
`python probe.py --calibrate` prints the frames and does **not** refuse, so numbers can be read while the rest of
the captures are being shot.

## The bar: the spread rule

With three different-frame runs loaded, every bar is checked twice: all three runs must sit on the pass side, AND
the nearest run value must clear the bar by at least the three runs' own spread (max - min). A bar inside the run
values, or within one spread of them, is "the build cannot tell this from its own noise": the probe prints the
values, the spread and the margin for every bar and exits 1 rather than print a green it cannot support. Fewer than
three runs: exit 1, `three runs needed`.

## The keys

Barred (`BARS` in `probe.py`): `green_low_share` (ge), `green_height_frac` (le), `green_area_frac` (ge),
`o240_green_area_frac` (ge), `pose_error_max` (lt). `-g2`'s `bank_*` names are gone, and so are
`gas_hue_window_px` and `hue_window_count()`.

Diagnostics, **barred on nothing** and printed under that word: `green_px` (green before the blob),
`plate_diff_px` (the old plate-difference count), `blob_px` (after the blob), `o240_green_px`,
`o240_plate_diff_px`, the per-still `frame` numbers and the `cards/` look report (each card's rise in metres and
its relief, plus the gas/smoke ratios; never barred — the gas card is squashed by `CombatFx.GasBankSquash` 0.55 on
purpose). Every key this probe prints is either in `BARS` or named a diagnostic here, so "X is not a bar" cannot be
said of it again.

## The bars

`BAR_VALUES` is empty: part 2 fills this table from three captures at three different frames of the current build,
each bar's margin at or above the three runs' spread.

| key | cmp | bar | the three runs | margin | why |
|---|---|---|---|---|---|
| `green_low_share` | `ge` | — | — | — | owner-decided 0.30 (head height, `HEAD_M` 1.8) |
| `green_height_frac` | `le` | — | — | — | owner-decided 0.15 (r2's bar on this test) |
| `green_area_frac` | `ge` | — | — | — | to be set outside the spread |
| `o240_green_area_frac` | `ge` | — | — | — | to be set outside the spread |
| `pose_error_max` | `lt` | — | — | — | `GasBankStills.PoseBar` |

## Status: part 1 done, part 2 open

- `python probe.py --selftest` -> exit 0 (the hand calculation, 0.34311 m, unchanged).
- `python probe.py --calibrate` -> exit 0 once runs exist; with no runs it prints nothing to measure.
- `python probe.py` -> exit 1 until part 2's three different-frame runs and bars are in.
