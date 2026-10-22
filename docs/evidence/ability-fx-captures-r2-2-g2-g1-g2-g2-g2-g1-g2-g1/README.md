# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2-g1: every band counts, and the ceiling says the picture is striped, 2026-10-22

**`python probe.py` exits 1 here, on purpose.** Only the MEASURE moves in this folder (part 1 of the unit): the
key now counts the thin bands the owner can see, and the current build has 11 to 18 of them, so it fails. Fixing
the build to make one mass is part 2.

`CombatFx.Banks.cs` stays at `3d7aa478ea` (the g1 build). `run1`/`run2`/`run3` are the SAME three stills as
`../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g2/` (t1 `sim_tick` 557 / 597 / 637, twelve distinct hashes),
copied across rather than reshot, so the before/after table below shows the measure change alone with no
tick-to-tick noise mixed in.

## The owner's complaint

> At the game camera the three gas-t1 stills are still separate horizontal yellow-green bands with dark mud
> between them. The pass stopped counting thin edges (`bank_thin_patches` 10, 11 and 7) and set the patch ceiling
> at 10, so 4 to 7 thick patches pass. Count the thin bands with the thick ones and put that ceiling outside the
> spread of three captures of the current build.

## What the measure counts now

`gas_paint_stats`' `patch_count` is a size test again, and only a size test:

```python
patch_count = sum(1 for s in sizes if s >= PATCH_MIN_PX)
```

Every coherent patch of 200 px or more counts -- a thin card edge exactly as much as a card body. The predecessor
required a surviving eroded core (`PATCH_ERODE` 3, `PATCH_CORE_MIN_PX` 60) and so made the thin bands invisible to
the one key meant to catch them.

`PATCH_ERODE`, `PATCH_CORE_MIN_PX` and `patch_core_px` all stay, and `bank_thin_patches` keeps its line unchanged
(`s >= PATCH_MIN_PX and c < PATCH_CORE_MIN_PX`). Its meaning flips: it is now a **breakdown of**
`bank_patch_count` -- how many of the counted bands are thin -- not the patches the count dropped. 10 of run1's 15
bands are thin, 11 of run2's 18, 7 of run3's 11.

## The ceiling: 4

The carried stills read **15 / 18 / 11** under this measure. Those are the owner's own three numbers (his
complaint, and the predecessor README's "before" row at `e602950aba`).

The chain's rule is that a bar must clear the nearest run by at least the three runs' own spread. Here the bar
must **reject**, so it sits below them: `min - spread` = 11 - (18 - 11) = **4**. That is outside the spread of
three captures of the current build, below it, and it keeps the standing "ceiling under 11" demand with room.

Every other bar keeps its predecessor value (`bank_low_share` 0.15, `bank_height_frac` 0.31, `o240_area_frac`
0.014, `pose_error_max` 0.5, `gas_paint_frac` 0.0135, `gas_green_share` 0.183, `bank_paint_share` 0.15,
`bank_aspect` 4.891) and still clears these stills: none of them reads the patch count, so the carried pixels
could not move them.

## The zebra margin is back

`bank_patch_count`'s old second job -- rejecting the flat zebra sheet -- returns with the erosion gone.
`python probe.py --against ../ability-fx-captures-r2-2-g2` reads `bank_patch_count` **40** under this measure,
far over the ceiling of 4, and exits 1 (it always does). The predecessor had lost that margin: at `PATCH_ERODE` 3
the zebra read 6, inside its ceiling of 10, and only `bank_paint_share` (0.0782 against the 0.15 `ge` bar) was
rejecting it. Both keys reject it now.

## `selftest`

The same hand-worked construction as the predecessor, with the numbers this measure gives: a 40x40 block (1600 px,
eroded core 34x34 = 1156 px) plus a 3x100 fringe (300 px, core erodes to 0). Both are over `PATCH_MIN_PX`, so
`bank_patch_count` is **2**, and the fringe alone has no core, so `bank_thin_patches` is **1** -- a subset of the
count. The camera maths and the `WIDTH_BRIDGE_PX` span case are unchanged. `python probe.py --selftest` exits 0.

## Before and after

| key | before (predecessor's measure, these stills) | after (this measure, same stills) |
|---|---|---|
| `bank_patch_count` | 5 / 7 / 4 | **15 / 18 / 11** |
| `bank_patch_count` ceiling | 10 | **4** |
| `bank_thin_patches` | 10 / 11 / 7 (patches DROPPED by the count) | 10 / 11 / 7 (a SUBSET of the count) |
| the key's verdict on this build | OK | **FAILED** (margin -14) |
| zebra (`--against ...-g2`) `bank_patch_count` | 6 (inside the ceiling) | **40** (rejected by 36) |

Everything else in the run lines is byte-for-byte the predecessor's: `bank_low_share` 0.3743/0.2661/0.3238,
`bank_height_frac` 0.1118/0.2096/0.1375, `gas_paint_frac` 0.0154/0.0156/0.0148, `gas_green_share`
0.2296/0.2131/0.2357, `bank_paint_share` 0.2037/0.1886/0.2152, `bank_width_m` 8.986/8.963/8.405, `bank_aspect`
3.452/2.014/2.606.

## This build fails the key on purpose

The picture has 11 to 18 separate bands and the key now says so. That is the honest state after part 1: the
measure matches what the owner sees at the game camera. Part 2 changes `CombatFx.Banks.cs` so the bank is one
mass, reshoots three runs, and only then does `python probe.py` exit 0 with `bank_patch_count` at or under 4.

## No `cards/` here

Same as the predecessor: the isolation stills live in `../ability-fx-captures-r2-2-g2-g1/cards/` and did not
change. The in-folder look report is skipped (`no cards/`); the card rejection check (`gas_green_share`) reads
that folder directly and still passes (`cards-smoke` 0.0908 and `cards-puff` 0.1076 both `REJECTED`).

## Out of scope

The build itself (part 2), the gas colour constants (`GAS_DIR`/`GAS_COS`/`GREEN_LEAN`), reshooting `cards/`, the
smoke bank, the predecessor's folder, and landing this lane (the puff brief, `4101179359`, is still unanswered).
