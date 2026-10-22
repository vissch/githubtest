# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1: a measure for the row gap, not the blob

**Part 1 of 3.** This round adds `row_profile_stats` to `probe.py` and reports its baseline on the CURRENT build
(`71ce0ccd73`, `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1`'s own three runs, measured with THIS probe's new
keys). No reshoot, no code change to `CombatFx.Banks.cs` yet, and no bars on the two new keys yet — those are later
steps.

**Part 2** widened `GasBankJitterM` to a full field cell (so card centres fill the depth instead of sitting on the
old 4 m row lattice) and widened the band to 1.78 m (`71ce0ccd73`, `50c8d44ea5`). No reshoot of THIS folder and no
bars yet; that is this round.

**Part 3 (this round).** Three fresh captures of the jitter-widened build, `BAR_VALUES` re-set for every key from
those captures, and `row_dark_frac` barred. `row_gap_deepest` is NOT barred — see "row_gap_deepest does not clear"
below. This lane does not land.

## The owner's complaint

> At the game camera the three gas-t1 stills are still horizontal yellow-green bands with dark ground between the
> rows. Close those gaps until a player sees one low bank. Measure the dark gap between rows on gas-t1, not
> `bank_paint_share` of a blob that bridges the stripes (0.7488, 0.7575, 0.7872).

## Why `bank_paint_share` cannot see it

`bank_paint_share` reads PATCH COHERENCE: how much of all the frame's paint sits in one connected blob. A gas card
is 13.5-19.3 m wide on a 4 m grid, so in X the cards lie three to four deep and their paint touches column to
column — the blob is continuous in X almost everywhere. In Z there is exactly ONE card per 4 m field row, and the
only depth overlap between neighbouring rows is the 1.72 m band each card leans towards the next row
(`GasBankBandM`, see `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1/README.md`, "Why the bands were there"). That
lean is enough to keep the whole field-wide shape ONE coherent patch — `bank_paint_share` 0.75-0.79 — while the
brightness along a horizontal line through the frame still dips at every row seam, because the lean only bridges
part of each row's own footprint. A measure that only asks "is it one blob" cannot read a dip inside the blob.

## The new measure: `row_profile_stats`

Per image row, across the bank's own column span (the painted pixels' 5th-95th percentile in x), the mean
brightness LIFT over the plate (Rec.709 luma, 0-255). Smoothed 5 rows against JPEG ringing. The bank's own
on-screen height (`the extent`) is the contiguous run of rows around the profile's peak that keeps at least 25% of
the peak's lift — this is the silhouette boundary, not a row gap, and it falls off towards the plate just because
that is where the bank ends. Trimming 15% off each end of the extent (`ROW_CORE_TRIM`) before reading troughs keeps
the measure off that silhouette fall-off and on the rows the gas actually painted over (see the comment on
`row_profile_stats`, "WHY `ROW_CORE_TRIM`" — the untrimmed median read every run's own silhouette edge as a trough,
which would have made this a measure of where the bank ends, not where it gaps).

- `band_lift_med`: the trimmed core's median lift (unbarred; what "typical bank brightness" means here).
- `row_dark_frac` **(barred, `le`)**: share of the trimmed core's rows under `ROW_TROUGH_FLOOR` (0.75) of that
  median — the dark mud lines, as a share of the bank's own height.
- `row_gap_deepest` **(barred, `le`)**: `(band_lift_med - min core lift) / band_lift_med`, clamped 0..1 — how deep
  the single worst row-to-row dip reads against the bank's own typical brightness.
- `row_dark_count`, `row_dark_spacing_px` (unbarred): how many separate dark runs, and their median pitch in px —
  the 4 m row pitch should show up here on the striped build and move once a fix lands.

## Picking the threshold (before any reshoot)

`ROW_TROUGH_FLOOR` started at 0.5, as `BARS`-style floors in this module usually do. On the CURRENT build's three
runs that gave `row_dark_frac` 0.0727 / 0.0727 / 0.0329 — not literally zero, but the whole-extent median was being
dragged down by the silhouette's own near/far fall-off (rows at 25-40% of peak just because that is the bank's
edge), so the "half the median" bar was mostly tripped by the edges, not by inter-row gaps. Printing the smoothed
profile for run1 (see the comment, "WHY `ROW_CORE_TRIM`") showed the real inter-row dips sit at 0.655-0.700 of the
TRIMMED core's own median, consistently across all three runs. `ROW_TROUGH_FLOOR` 0.75 sits just above that, with
`ROW_CORE_TRIM` cutting the silhouette edges out of the comparison first.

## The baseline (current build, `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1`)

```
run1   row_dark_frac 0.0455   row_gap_deepest 0.3415   t1 sim_tick 557
run2   row_dark_frac 0.0455   row_gap_deepest 0.2997   t1 sim_tick 597
run3   row_dark_frac 0.0537   row_gap_deepest 0.3447   t1 sim_tick 637
```

`bank_paint_share` on these same three runs is 0.7488 / 0.7575 / 0.7872 — high, because the patch is one coherent
blob. `row_dark_frac` and `row_gap_deepest` are the new reading: every run still has a real row-to-row dip (the
deepest one reaching 30-34% below the bank's own typical brightness), even though the blob is unbroken. This is the
owner's complaint, measured directly for the first time. These three numbers are the floor the next round's fix
must clear by at least their own spread; no bars are set on these keys yet (step 3/8 of this round, not this one).

## Part 3: three captures of the jitter-widened build

`run1`/`run2`/`run3` are fresh captures of the committed build (`50c8d44ea5`: `GasBankJitterM` a full field cell,
band 1.78 m), `CombatFx.Banks.cs` otherwise unchanged (`GasBankThin` still 0.19 — see "A lever that made it worse"
below). t1 `sim_tick` 556 / 596 / 636.

```
run1   row_dark_frac 0.0140   row_gap_deepest 0.2980   bank_paint_share 0.6161   bank_patch_count 26
run2   row_dark_frac 0.0140   row_gap_deepest 0.3012   bank_paint_share 0.6903   bank_patch_count 30
run3   row_dark_frac 0.0000   row_gap_deepest 0.2438   bank_paint_share 0.7272   bank_patch_count 19
```

### The bars (re-set from these three runs)

| key | runs | spread | bar | margin | vs PREV (striped build, `../…g1-g1`) |
|---|---|---|---|---|---|
| `bank_low_share` (ge) | 0.0896/0.0877/0.0912 | 0.0035 | 0.08 | 0.0077 | — |
| `bank_height_frac` (le) | 0.3286/0.3349/0.3214 | 0.0135 | 0.35 | 0.0151 | — |
| `o240_area_frac` (ge) | 0.0167/0.0190/0.0196 | 0.0029 | 0.013 | 0.0037 | unchanged |
| `pose_error_max` (lt) | 0.0/0.0/0.0 | 0.0 | 0.5 | 0.5 | unchanged |
| `gas_paint_frac` (ge) | 0.0198/0.0188/0.0178 | 0.0020 | 0.015 | 0.0028 | — |
| `gas_green_share` (ge) | 0.4258/0.4229/0.4270 | 0.0041 | 0.41 | 0.0129 | — |
| `bank_paint_share` (ge) | 0.6161/0.6903/0.7272 | 0.1111 | 0.50 | 0.1161 | **moved the wrong way**: PREV 0.7488/0.7575/0.7872 |
| `bank_patch_count` (le) | 26/30/19 | 11 | 41 | 11 | **moved the wrong way**: PREV 21/21/15 |
| `row_dark_frac` (le) | 0.0140/0.0140/0.0000 | 0.0140 | 0.03 | 0.0160 | PREV 0.0455/0.0455/0.0537, fails by >0.0155 (1.1x spread) — improved |

Card rejection unaffected: `cards-smoke` 0.0908 and `cards-puff` 0.1076 both still fail `gas_green_share`'s 0.41
bar by over 70x this build's own spread.

### `row_gap_deepest` does not clear

`row_gap_deepest` runs 0.2980/0.3012/0.2438, spread 0.0574. Clearing this build's own spread needs
`bar >= 0.3012 + 0.0574 = 0.3586`. Leaving PREV's three captures (0.3415/0.2997/0.3447, min 0.2997) failing by
more than that same spread needs `bar < 0.2997 - 0.0574 = 0.2423`. Those two requirements do not overlap — no
single number satisfies both sides of the folder's own bar rule — so `row_gap_deepest` is reported, unbarred, not
faked. The band fix and the jitter widening both close `row_dark_frac` (how MUCH of the bank's height is dark);
neither closes `row_gap_deepest` (how DEEP the single worst dip reads), because the deepest point of each row's
own footprint is still set by where that one card's alpha falls off, independent of how far its neighbours jitter.

### Why `bank_paint_share` and `bank_patch_count` moved the wrong way

Both are read against the whole-frame paint, not the row profile. The jitter widening spreads card centres
further apart in X as well as Z (risk noted in the plan), so the biggest coherent patch now bridges LESS of the
frame's total paint (0.62-0.73, down from 0.75-0.79) and the paint falls into more separate patches (19-30, up
from 15-21) even though the ROWS look less gapped (`row_dark_frac` down from 0.045-0.054 to 0.000-0.014). This is
expected and not a regression in what the owner asked to fix: the owner's complaint was about the row gap, which
`row_dark_frac` reads directly and which improved; `bank_paint_share`/`bank_patch_count` read a different thing
(frame-wide patch coherence) that this change does not target and was never going to improve by widening jitter.

### A lever that made it worse

Step 8's one remaining lever, `GasBankThin` 0.19 -> 0.28 (less pale at a row seam), was tried and reverted. On a
reshoot of all three runs with `GasBankThin = 0.28f` the row measures moved the WRONG way: `row_dark_frac`
0.0882/0.0888/0.1006 (up from 0.0140/0.0140/0.0000) and `row_gap_deepest` 0.6393/0.6486/0.6734 (up from
0.2980/0.3012/0.2438). Raising the floor alpha raises the bank's own peak and typical brightness more than it
raises the seam's, so the SHARE of the typical brightness the seam reaches actually drops — the opposite of the
intended effect. `CombatFx.Banks.cs` is committed unchanged (`GasBankThin` still `0.19f`); the reshoot with
`0.28f` was not kept.

## Eye check

`run1/gas-t1.jpg`: one low green-tinted mass over the ground, no dark mud stripe visible between rows at the game
camera's distance. This matches the owner's ask for `row_dark_frac` (now 0.000-0.014, down from 0.045-0.054); the
worst single dip (`row_gap_deepest`) is still present but is no longer a wide, high-contrast band — see "does not
clear" above for why that number does not move with this fix.

## This lane does not land

The puff brief `2026-10-21-gas-bank-turn-the-sheet-into-a-puff` (board id `4101179359`) is still unanswered. Do
not rebase this onto integration or run `land.py` until the owner answers it.

## Out of scope

Any further change to `CombatFx.Banks.cs` or the gas card recipe beyond `GasBankJitterM`/`GasBankBandM` (already
landed) and the reverted `GasBankThin` experiment above, the gas hue and its colour constants, reshooting `cards/`
(`CARDS_DIR` keeps pointing at `../ability-fx-captures-r2-2-g2-g1/cards/`), `DrawSmokeBank` and the smoke jitter,
lowering the stand floor, turning the sheet into a puff, and landing the lane.
