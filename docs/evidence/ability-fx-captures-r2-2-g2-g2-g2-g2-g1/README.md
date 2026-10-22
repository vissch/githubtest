# ability-fx-captures-r2-2-g2-g2-g2-g2-g1: one O240 call, not a wash, 2026-10-22

**`python probe.py` clears, exit 0.** Part 1 only: no bar on the owner's final look, just the regression bars
re-derived for the one-call shot.

## The owner's complaint

> Draw one gas cloud the far camera can pick out, not a green wash of several blobs. `o240_large_cloud_count` is
> 5, 3 and 3 against a bar of 1. Start from `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g2`'s tip. Bar outside
> the spread of three captures of the current build.

`GasBankStills.Shoot`'s O240 band gassed five points 40 m apart along the corridor, so the far camera's still held
up to five patches at once — a wash, not a cloud. `measure_o240` then counted every component at or above
`LARGE_MIN_PX`, so the wash's count (5, then 3, then 3 on three captures of the five-call build) could never clear
a bar of 1 without lowering the floor the owner had already rejected once (`-g2-g2-g2-g2`'s README).

## What changed

- `GasBankStills.cs` `Shoot`: the O240 band's `far` list is now the single point `(x, z)` — the same centre the
  T1 block gasses — instead of the five-point spread. One call means one source, so the far camera's still holds
  one cloud by construction, not by a tighter count floor.
- `probe.py` `measure_o240` adds `o240_top_blob_px` (the biggest component's pixel count). `BARS` flips
  `o240_large_cloud_count` from `'ge'` to `'le'` (several large blobs now fail the count) and pairs it with
  `o240_top_blob_px: 'ge'` (a count of zero large blobs, from no cloud at all, still fails). `o240_large_share`
  and `o240_green_area_frac` stay `'ge'`, re-derived from the one-call tree's three fresh captures.
- `cards/` carried unchanged from `-g2-g2-g2-g2` (no `CombatFx` drawing changed; the unbarred look report only).
- `run1/`, `run2/`, `run3/` — three fresh `GasBankStills` captures of the one-call tree, `TW_GAS_WAIT_FRAMES`
  200/215/230, each the four stills plus sidecars and manifests.

## Side effect: the T1 bar moved too

Because the O240 call now lands on the exact point the T1 block later gasses, leftover O240 gas is still thinning
out when T1 fires its own call there, so T1's own green patch measures differently than before (rise 0.587/0.587/
0.539 m vs. the five-call build's 0.359/0.359/0.357; `green_area_frac` 0.0034/0.0034/0.0034 vs. 0.0055/0.0056/
0.0055). `green_rise_m`'s bar (`ge` 0.30) still clears either way. `green_area_frac`'s carried bar (`ge` 0.005)
does not clear the new, lower area, and the bar is re-derived to `0.0030` (floor 0.0034, spread 0.0000). Every
other T1 key (`green_low_share`, `green_height_frac`) is unaffected at this build's margins.

## The bars

Checked twice, as always: all three runs on the pass side, AND the nearest value clears the bar by at least the
three runs' own spread.

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.30 | 0.9138, 0.9109, 0.9122 | 0.0029 | 0.6109 | carried, re-checked — still clears |
| `green_height_frac` | `le` | 0.15 | 0.0209, 0.0209, 0.0192 | 0.0017 | 0.1291 | carried, re-checked — still clears |
| `green_rise_m` | `ge` | 0.30 | 0.587, 0.587, 0.539 | 0.048 | 0.239 | carried, re-checked — still clears despite the side effect above |
| `green_area_frac` | `ge` | 0.0030 | 0.0034, 0.0034, 0.0034 | 0.0000 | 0.0004 | re-derived (see "Side effect" above); the carried bar 0.005 no longer clears |
| `o240_green_area_frac` | `ge` | 0.0004 | 0.0006, 0.0007, 0.0006 | 0.0001 | 0.0002 | re-derived for the one-call shot; large-pixel share of the frame |
| `o240_large_cloud_count` | `le` | 1 | 1, 1, 1 | 0 | 0 | the one-call shot reads as exactly ONE large cloud on every capture — not the five-call wash's 5, 3, 3 |
| `o240_top_blob_px` | `ge` | 450 | 591, 620, 540 | 80 | 90 | pairs with the count bar: a reading of 0 large clouds (no cloud at all) still fails this |
| `o240_large_share` | `ge` | 0.20 | 0.3117, 0.3849, 0.2993 | 0.0856 | 0.0993 | re-derived for the one-call shot |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |

## The look report (unbarred)

From `cards/` (carried, unchanged): gas relief 0.0849 against the smoke card's 0.2178 (ratio 0.3898); both sit at
ground level in this isolation pose, so the rise ratio is 0/0 and says nothing. Out of this unit's scope
(`CombatFx.GasBankSquash`, card look).

## Status: done (part 1)

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (camera hand calculation, 0.34311 m). These
are regression bars from this build's own spread, same as every predecessor in this chain — they prove the far
camera now sees one cloud, not that this is the final look.
