# ability-fx-captures-r2-2-g2-g2-g2-g2-g1-g2-g2: the gas card stands, and the O240 count bar comes off a 1-vs-2 tie, 2026-10-23

**`python probe.py` clears, exit 0.** No look change: the gas card already stands (it did since the -g2-g2 unit).
Two instrument faults made it read 0.000 m of rise and a 0.5505 relief ratio anyway, and the O240 blob-count bar sat
on a reading with zero margin. All three are fixed in `probe.py`.

## The owner's complaint

> cards-gas rise is 0, the same as the smoke card, so the rise ratio stays 0 and unbarred. The relief ratio 0.5505
> (bar 0.45, one cards pass, spread 0) passes that puddle. Make the gas card stand like the smoke card and bar its
> rise above 0. Put the relief ratio outside 0.5505, toward the smoke card's 0.2178. The far second blob is
> 238/83/136 px and o240_large_cloud_count is 1, 1, 1 with margin 0. Move that bar off the reading.

He is right about every number, wrong about the cause: the gas card was never a puddle. `CombatFx.GasBankStand`
and `GasBankSink` (set two units ago) already stand the card `CardWidth * 0.42` tall with its foot sunk 0.20 m
under the ground. The instrument was lying twice over.

## What was actually wrong

1. **The old `card_rise_m` measured every pixel against its own ground distance.** A pixel's height is read in
   the vertical plane straight below THAT pixel, so every pixel comes back standing on the ground it's over -
   0.000 m no matter how tall the card is drawn. `measure_t1` never had this bug: it takes each COLUMN's distance
   from its own FOOT pixel (the column's lowest masked row) and measures every pixel in that column against that
   one plane. The column-foot logic is now one shared function, `column_heights`, used by both `measure_t1` and
   the new `card_measure`.
2. **The cards used the GREEN mask** (`greener`/`green_mask`). The gas card IS green - a ~14.8k-pixel cloud - so
   its "mask" was never the card: it was ~1.4k px of green fringe bleeding past the card's edge, next to a grey
   smoke card the green mask barely sees at all. The two cards were never measuring the same thing. A plate-diff
   mask does no better: this scene flickers ±200 luma between passes with nothing drawn, so a luma-only diff blob
   runs to the frame bottom. What reads a green AND a grey card alike: **luma added over the plate** (`CARD_LUMA_BAR`
   6), confined to a centre `CARD_WINDOW` (x 0.25..0.75 of width, y < 0.95 of height) so the flicker at the frame
   edges never joins the card's blob, then `largest_blob`.

## The new reading (the committed tree, `cards/`)

| card | mask px | rise | top | relief | old (green-mask) relief |
|---|---|---|---|---|---|
| gas | 11320 | 2.224 m | **4.796 m** | 0.1257 | 0.1199 |
| smoke | 15719 | 3.598 m | 5.520 m | 0.0615 | 0.2178 |
| puff | 5099 | 1.541 m | 3.690 m | 0.1722 | 0.2175 |

`top_m` 4.796 m checks against the card's own drawn top, worked from the test's own constants, not from the probe:
`CardWidth * GasBankStand - GasBankSink` = `12 * 0.42 - 0.20` = **4.84 m**. The two agree to 4.4 cm - the metre
maths was always right; only the mask was wrong. `gas_smoke_relief_ratio` on the new mask is **2.0439**: the gas
card now reads a HIGHER relief than the smoke card (a cloud, not a flatter puddle), the opposite of the old
0.5505 the green mask reported.

## Three cards passes, not one

The old `cards/` was a single pass; the owner's bars need the same spread-and-margin check as run1/run2/run3, so
`GasBankCards` gained `TW_CARD_WAIT_FRAMES` (SessionState `tw.gasbank.cardwaitframes`, default 10, clamped 5..60)
for the per-card settle loop, and `cards2`/`cards3` are fresh passes at 14 and 18 frames (frames 111/115/119 -
first tried 10/25/40, which landed far enough apart in the card's life that the Erode-tagged smoke sheet visibly
faded between passes and swamped the relief reading in noise; closer wait values keep three distinct frames
without that drift).

| pass | wait frames | cards-gas frame | gas rise | gas relief | smoke relief | relief ratio |
|---|---|---|---|---|---|---|
| cards | 10 | 111 | 2.224 m | 0.1257 | 0.0615 | 2.0439 |
| cards2 | 14 | 115 | 3.192 m | 0.1958 | 0.0545 | 3.5927 |
| cards3 | 18 | 119 | 2.117 m | 0.1282 | 0.0537 | 2.3873 |

## The bars

| key | cmp | bar | the three passes | spread | margin | why |
|---|---|---|---|---|---|---|
| `cards_gas_rise_m` | `ge` | 1.00 | 2.224, 3.192, 2.117 | 1.075 | 1.117 | new: the owner's "above 0", with real room above it |
| `gas_smoke_relief_ratio` | `ge` | 0.45 | 2.0439, 3.5927, 2.3873 | 1.5488 | 1.5939 | re-derived off the luma-add masks: every pass reads the gas card as a REAL cloud (ratio > 2), clear of the old mask's 0.5505 puddle |
| `o240_top_blob_share` | `ge` | 0.65 | 0.7822, 0.8605, 0.8573 | 0.0783 | 0.1322 | new: `o240_top_blob_px / o240_green_px`. A second cloud of any size (the owner's 238/83/136 px second blobs included) costs this bar real room, where `o240_large_cloud_count` (now printed only) could only ever sit on a 1-vs-2 tie with zero margin |

Every bar carried from -g2-g2-g2-g2-g1-g2 (`green_low_share`, `green_height_frac`, `green_relief`, `green_rise_m`,
`green_area_frac`, `o240_green_area_frac`, `o240_large_share`, `o240_top_blob_px`, `pose_error_max`) is unchanged;
run1/run2/run3 and their numbers are carried unchanged too (no sim or presentation code touched for the T1/O240
band, only the cards instrument and the O240 bar shape).

## Status: done

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (camera hand calculation, 0.34311 m). The
look change the owner asked for ("make the gas card stand... bar its rise above 0... put the relief ratio
outside 0.5505") was already true of the build; this unit fixes the instrument that hid it and moves the O240
count bar off its zero-margin reading, with three fresh cards passes behind both new bars.
