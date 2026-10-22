# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1: a measure for the row gap, not the blob

**Part 1 of 3.** This round adds `row_profile_stats` to `probe.py` and reports its baseline on the CURRENT build
(`71ce0ccd73`, `../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1`'s own three runs, measured with THIS probe's new
keys). No reshoot, no code change to `CombatFx.Banks.cs` yet, and no bars on the two new keys yet — those are later
steps.

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

## Out of scope

Setting `BAR_VALUES['row_dark_frac']`/`['row_gap_deepest']` (needs a fix and a reshoot first), any change to
`CombatFx.Banks.cs` or the gas card recipe, the gas hue and its colour constants, reshooting `cards/`
(`CARDS_DIR` keeps pointing at `../ability-fx-captures-r2-2-g2-g1/cards/`), `DrawSmokeBank` and the smoke jitter,
lowering the stand floor, turning the sheet into a puff, and landing the lane.
