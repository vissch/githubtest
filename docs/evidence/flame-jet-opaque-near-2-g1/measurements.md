# flame-jet-opaque-near-2-g1 - the measured baseline

The owner, 2026-10-24: "jet-t2-1, jet-t2-2 and jet-t2-3 are the same png. A pale straight cut runs through the
underside of the white core, one card meeting the next, and edge_spread 0 is that one frame counted three times.
Draw the near third as one sheet with no cut, shoot three ages of the pinned clock, and put the edge bar outside the
spread of three captures of the current build."

## The harness: three ages, not one age three times

`Tests/Stills/FlameJetStills.cs` pinned ONE age (1.5 s) for all three sessions, so the three captures rendered
byte-identical pngs and `edge_spread` was 0 by construction. It now shoots three ages of the same pinned clock -
1.3, 1.5 and 1.7 s, chosen by `TW_FLAME_CAP` (the plate keeps 1.5) - over a fixed 3 s burst, so every age clears
both valve ramps (0.10 s, 0.14 s) and every `left` (1.7 / 1.5 / 1.3 s) clears the play-out threshold (0.833 s here).
The valve is exactly 1 in all three; only the Perlin and `Mathf.Repeat` phases, which are functions of `PinnedNow`
alone, differ.

Three distinct pngs, measured (md5 of the baseline stills):

    b95996da79f0c619eb63f954f971b2c7  jet-t2-1.png   age 1.3 s
    7e73f1811f22c658283f9b0f55123dcd  jet-t2-2.png   age 1.5 s
    3cc29041d83d8ecdcaf4b88986174d00  jet-t2-3.png   age 1.7 s

`probe.py` now refuses to measure at all if any two of the three share a sha256 or differ by zero in every channel.

## The baseline: the current build, four sessions, 2026-10-24

`baseline/` holds the build at `3738dc6584` (no look change), one editor session per still
(`TW_FLAME_CAP` = 1, 2, 3, plate), same pose and same pinned clock.

| still | age | fire mask x | near px | tip px | near_edge_corr | seam_score | core_over_tip |
|---|---|---|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 624..1249 (626) | 34656 | 4419 | +0.0172 | 1.946 @ -18 deg | 1.961 |
| jet-t2-2.png | 1.5 s | 631..1218 (588) | 33460 | 7018 | +0.0065 | 2.037 @ -16 deg | 2.024 |
| jet-t2-3.png | 1.7 s | 624..1252 (629) | 34801 | 5051 | +0.0153 | 1.900 @ -90 deg | 2.003 |

Triples, and the minimum each bar has to sit outside:

    near_edge_corr   0.0172, 0.0065, 0.0153   min 0.0065
    seam_score       1.9463, 2.0369, 1.8997   min 1.8997
    core_over_tip    1.9610, 2.0237, 2.0029   min 1.9610

## The bars

Each is the midpoint between the baseline minimum and 0, rounded down, so a bar inside the current build's own
spread cannot be passed off as a pass:

| bar | value | why |
|---|---|---|
| `EDGE_BAR` | 0.0032 | half of 0.0065. Four decimals, not two: two would round it to 0.00 and no build could meet it. |
| `SEAM_BAR` | 0.94 | half of 1.8997, rounded down. The cut has to lose half its concentration. |
| `LUMA_BAR` | 1.6 | the owner's standing bar from the round before. The baseline already measures 1.96. |

`edge_spread` is gone: only identical frames could meet it.

`python docs/evidence/flame-jet-opaque-near-2-g1/probe.py --baseline` exits 0 on this evidence: the three frames are
distinct, and both new bars are below their baseline minimum. The full probe (no flag) also wants the new build's
three stills beside it, and the look change that earns them is the next step of this lane.

## The new build: the near third as ONE sheet, 2026-10-24

`Flamethrower.cs` now draws the hot band with exactly one `JetCore` card, and both chains start at
`FirstWarmLink` - the first link whose `u` is at or past `HotThird` (2 of the envelope's 4, 3 of the core's 9), so
no link and no `if (hot)` stagger card is laid inside the band. Eleven opaque cards became one: ten fewer per root.

Shot by the same harness, same pose, same pinned clock, one editor session per still. The plate (`bg-t2.png`) is
unchanged - it fires no burst, so no look change touches it.

Three distinct pngs again (md5 of the stills committed here, the round-2 sheet):

    410837f683334eda0163df82c7f42ecb  jet-t2-1.png   age 1.3 s
    7480de5a0d53953b5d56e7f96d8081f2  jet-t2-2.png   age 1.5 s
    394194ac76f941e844a7d995077a6adb  jet-t2-3.png   age 1.7 s
    80f6f58d24177de0eb7e4e9393765e2a  bg-t2.png      the plate

### Round 1 - the sheet as the plan specified it

| still | age | fire mask x | near px | near_edge_corr | seam_score | core_over_tip |
|---|---|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 728..1249 (522) | 39289 | +0.0678 | 6.993 @ +0 deg | 2.106 |
| jet-t2-2.png | 1.5 s | 705..1218 (514) | 32385 | +0.0598 | 4.378 @ +0 deg | 2.011 |
| jet-t2-3.png | 1.7 s | 712..1251 (540) | 35905 | +0.0819 | 4.583 @ +0 deg | 1.945 |

Two faults, both measured. The fire mask's left edge moved 624 -> 728 px: the one sheet left a HOLE at the nozzle
where the eleven cards used to overlap, and `near_edge_corr` rose 0.017 -> 0.082 with it - the plate's stakes
reading through. And `seam_score` went 1.90-2.04 -> 4.38-6.99, every one of the three at exactly 0 degrees.

### Round 2 - widened, lengthened and fully opaque (the plan's own first remedy)

1.30 of the band's length instead of 1.10, a quarter thicker, alpha 1.0. No second card: that is the cut coming back.

| still | age | fire mask x | near px | near_edge_corr | seam_score | core_over_tip |
|---|---|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 731..1249 (519) | 46507 | +0.0318 | 6.958 @ +0 deg | 2.206 |
| jet-t2-2.png | 1.5 s | 659..1218 (560) | 28561 | -0.0196 | 5.123 @ +0 deg | 2.069 |
| jet-t2-3.png | 1.7 s | 668..1251 (584) | 31925 | +0.0221 | 5.109 @ +0 deg | 1.936 |

The hole half closed: `near_edge_corr_max` 0.0819 -> 0.0318, still ten times EDGE_BAR 0.0032. `seam_score` did not
move at all: 6.958 against 6.993.

### What the number says

| bar | wanted | baseline (current build) | new build | verdict |
|---|---|---|---|---|
| `near_edge_corr_max` | <= 0.0032 | 0.0172 | **0.0318** | worse |
| `seam_score_max` | <= 0.94 | 2.0369 | **6.958** | far worse |
| `core_over_tip_luma_min` | >= 1.6 | 1.9610 | 1.936 | still over the bar |

`seam_score` landing at exactly 0 degrees in all six captures is the explanation. `Book.JetCore` is `_Solid`
(FlameJetCoreTests guards it), so the shader keeps the card's keyed fringe fully opaque and never softens it against
what is behind. A single card drawn along the aim therefore has a hard, perfectly horizontal top and bottom rim
running the whole length of the hot band - one straight line three times stronger than the card-to-card cut it was
drawn to remove. Width, length and alpha were tried and move it by 0.5%, because the rim IS the card.

So "draw the near third as one sheet with no cut" is built, measured and reported, and the measurement is that one
sheet trades a short internal cut for a long external one. The bars stay exactly where the baseline put them; the
probe exits 1. The next move is the owner's, and the two candidates are in the leg note: soften `JetCore`'s rim
(drop `_Solid` for this one card, or feather its alpha at the quad edge) so a lone sheet has no hard boundary, or
keep the eleven cards and attack the cut where it is - neither is a number this lane may choose on its own.

## Round 3 - the seam measured inside the mask, 2026-10-24

Round 2's `seam_score` could not tell a cut from an outline. All six captures peaked at exactly 0 degrees, the aim
axis, and width, length and alpha moved the score by 0.5% "because the rim IS the card". The owner's fault is a line
*through the underside of the white core*: internal. So `seam_score` now erodes the near third's mask by 13x13
(about 6 px in from the fire mask's own outline) and scores only inside that region. No png was re-shot: both
triples below are the same committed stills, measured again.

Prediction this run tested: the eleven-card baseline's cut is internal, so its eroded seam must stay well above 1.0
while the one sheet's falls. **Half of it held.** The baseline stayed above 1.0 - the metric still measures
something - but the one sheet's did not fall. It rose.

### The eleven cards (`baseline/`, the build at `3738dc6584`)

| still | age | near px | eroded seam_score | angle |
|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 34656 | 2.088 | +2 deg |
| jet-t2-2.png | 1.5 s | 33460 | 2.030 | -54 deg |
| jet-t2-3.png | 1.7 s | 34801 | 2.127 | -52 deg |

Min 2.0303. Two of the three now peak near -53 degrees, not on the aim axis: with the outline gone the metric finds
a different, internal line. That is the erosion doing its job.

### The one sheet (folder root, the build at `fd68de7cd4`)

| still | age | near px | eroded seam_score | angle |
|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 46507 | 11.146 | +0 deg |
| jet-t2-2.png | 1.5 s | 28561 | 2.489 | +70 deg |
| jet-t2-3.png | 1.7 s | 31925 | 6.842 | +0 deg |

Max 11.146, worse than round 2's own 6.958. Two of the three still peak at exactly 0 degrees, 6 px inside the fire.

### The new bar and its arithmetic

`SEAM_BAR` is re-derived by the rule already in use - half the baseline minimum, rounded down to two decimals -
because the metric under it changed: 2.0303 / 2 = 1.01515 -> **1.01**. `EDGE_BAR` 0.0032 and `LUMA_BAR` 1.6 do not
move. The probe's `bars_outside_baseline_spread` check still refuses a bar sitting inside the current build's own
spread, so the guard the owner asked for stays in force.

### Verdict

| bar | wanted | baseline (eleven cards) | one sheet | verdict |
|---|---|---|---|---|
| `seam_score_max` (internal) | <= 1.01 | 2.0303 (min) | **11.146** | far worse |
| `near_edge_corr_max` | <= 0.0032 | 0.0172 | **0.0318** | worse |
| `core_over_tip_luma_min` | >= 1.6 | 1.9610 | 1.936 | over the bar |

`python docs/evidence/flame-jet-opaque-near-2-g1/probe.py` exits 1; `--baseline` exits 0.

The round-2 reading was that the one sheet's straight line was only its own rim, and that a fairer, internal metric
would clear it. The internal metric says otherwise: a hard horizontal line along the aim axis is still there 6 px
inside the fire, and it is five times the eleven cards'. `Book.JetCore` is `_Solid`, so the shader keeps the card's
keyed fringe fully opaque; the resulting hard top and bottom boundary casts a Sobel ridge that reaches well inside
the mask, and one long card concentrates it where eleven short overlapping ones did not. The remedy is therefore
the opacity path, not the card count: soften `JetCore`'s rim so a lone sheet has no hard boundary at all. That is
the next leg's step 6, and it changes a look.

## Round 4 - the rim's ramp widened, and the nozzle centre pulled back, 2026-10-24

Round 3's conclusion named the opacity path as the remedy: "soften `JetCore`'s rim so a lone sheet has no hard
boundary at all". Two changes, shot together in one round of four sessions:

1. **`Flipbook_URP.shader:136`**, the `_Solid` branch: `saturate((tex.a - 0.18) / 0.22)` -> `(tex.a - 0.06) / 0.60`.
   At 0.18/0.22 the glyph is fully opaque by `tex.a` 0.40, so the drawing's fringe ends on a *level line* of the
   alpha channel - a step from clear to solid. Over 0.06..0.66 the same fringe fades across a band of pixels.
   `Solid = true` is on the `FireCore`/`JetCore` row alone (`FlameJetCoreTests.OnlyTheJetCoreIsSolid` guards it),
   so no other book moved.
2. **`Flamethrower.cs`**, the one sheet's centre: `HotThird * len * 0.50` -> `0.42`. The sheet is 1.30 of the band
   long, so from the middle it hung 0.15 of a band past both ends; at 0.42 it reaches 0.23 behind the mouth and
   still 1.07 past it, which keeps the hand-over to the first warm link covered.

The two were not measured apart. Each round costs four editor sessions, and round 3's number already said the
placement nudge can only move `near_edge_corr`, never the seam, so separating them buys no reading the seam column
does not already give.

Three distinct pngs, same harness, same pose, same pinned clock, one editor session per still. `bg-t2.png` came
back byte-identical to round 3's (it fires no burst), which is the harness's own repeatability check passing.

    8f481d39071d5d465a3da2d5efe9b0e4  jet-t2-1.png   age 1.3 s
    842ae2c998188730893ce67a291f6ea7  jet-t2-2.png   age 1.5 s
    21de1fb7d15bc671a86b61c92b9a862d  jet-t2-3.png   age 1.7 s
    80f6f58d24177de0eb7e4e9393765e2a  bg-t2.png      the plate, unchanged

| still | age | fire mask x | near px | near_edge_corr | eroded seam_score | core_over_tip |
|---|---|---|---|---|---|---|
| jet-t2-1.png | 1.3 s | 720..1249 (530) | 46558 | -0.0155 | 11.427 @ +0 deg | 2.203 |
| jet-t2-2.png | 1.5 s | 645..1218 (574) | 28063 | -0.0326 | 2.775 @ +4 deg | 2.086 |
| jet-t2-3.png | 1.7 s | 654..1251 (598) | 31154 | -0.0151 | 3.534 @ +2 deg | 1.954 |

### Verdict

| bar | wanted | baseline (eleven cards) | round 3 (one sheet) | round 4 | verdict |
|---|---|---|---|---|---|
| `near_edge_corr_max` | <= 0.0032 | 0.0172 | +0.0318 | **-0.0151** | **passes** |
| `core_over_tip_luma_min` | >= 1.6 | 1.9610 | 1.936 | **1.954** | passes |
| `seam_score_max` (internal) | <= 1.01 | 2.0303 (min) | 11.146 | **11.427** | **fails** |

`python docs/evidence/flame-jet-opaque-near-2-g1/probe.py` exits 1, on `seam_score_max` alone.

**The nozzle hole is closed and the edge bar is earned.** `near_edge_corr` went +0.0318 to -0.0151, past 0 and
below EDGE_BAR 0.0032: the near third no longer lets the plate's stakes read through it anywhere, and it is the
first build in this lane to clear that bar - the eleven-card baseline never did either (+0.0065 at best). The
previous leg's open question, "the jet's EDGE_BAR cannot be earned, near_edge_corr straddles zero in both builds",
is answered by measurement: it can, and this build earns it. `core_over_tip_luma` did not pay for it (1.936 ->
1.954), so the wider ramp cost the core none of its brightness over the tip.

**The seam did not move, and the ramp was its last named lever.** 11.146 -> 11.427, still at exactly 0 degrees in
capture 1. Two of the three improved a lot (6.842 -> 3.534 and 2.489 -> 2.775 is a wash, but round 2's 5.11/5.12
pair is gone), and the max is carried entirely by capture 1 at age 1.3 s - the capture whose near mask is 46.5k px
against 28-31k for the other two, half again as much fire in the band. So the ridge is not the keyed fringe's
contour: widening the ramp over three times its old width changed it by 2.5%. What is left at 0 degrees after the
fringe is softened is the card's own UV/geometry boundary - the sheet is drawn with `Fill = 0.90`, so the glyph
runs to within a tenth of the cell and is clipped by the quad, which no alpha ramp can feather.

Three remedies remain, and all three are the owner's call, not this lane's: inset the glyph inside its cell so the
quad clips nothing (a sheet change, `tw-vfx-sheets`); let the lone sheet keep the depth soften that `_Solid` skips,
which trades the opacity the owner asked for; or keep the eleven cards and attack the card-to-card cut where it is,
which is this lane's premise reversed. The lane's own scope says the first two change a look and the third is out
of scope, so the number is reported and the bars stay where the baseline put them.

## Round 5 — where the ridge is, in pixels (2026-10-24)

No still was re-shot and no look changed. `seamwhere.py` (new, look-only) reuses `probe.py`'s own `firemask`,
`edges`, `near_and_tip` and `seam_score` and asks one question: **which image row carries `seam_score`?**

**The prediction, written before the result** (round 4 blamed the glyph being clipped by its quad; that reading is
wrong on the code — `Flamethrower.cs:655` makes the quad *larger* than the ink and `Flipbook_URP.shader:105` samples
the whole cell, so nothing is clipped): capture 1's near mask of 46558 px against 28063 and 31154, in a window
shifted 70 px right, means `firemask`'s largest connected piece at age 1.3 s is the stream **with the burning ground
pool merged onto it**, and the ridge is the pool's straight horizontal top edge, not a cut through the white core.

**The prediction holds.**

| still | mask piece | near third | inner px | seam | the row carrying it | inner bottom y, right-hand buckets |
|---|---|---|---|---|---|---|
| jet-t2-1 | 100661 px, x 720..1249, y 208..579 | x 720..895, 46558 px | 36240 | 11.427 @ +0 deg | **image y 413** | 559, 561, 561, 563, 565, 567, 567, 570, 570 |
| jet-t2-2 | 98916 px, x 645..1218 | x 645..835, 28063 px | 13694 | 2.775 @ +4 deg | row 170/333 (0.51 down) | 551, 559, 561, 561, 563, 565 |
| jet-t2-3 | 104111 px, x 654..1251 | x 654..852, 31154 px | 17175 | 3.534 @ +2 deg | row 163/322 (0.51 down) | 553, 560, 561, 562, 564, 565, 567 |
| baseline 1 | 125384 px, x 624..1249 | x 624..831, 34656 px | 20826 | 2.088 @ +2 deg | row 170/349 (0.49 down) | 553, 560, 561, 563, 565 |
| baseline 2 | 127263 px, x 631..1218 | x 631..826, 33460 px | 19715 | 2.030 @ -54 deg | row 190/345 (0.55 down) | 553, 560, 561, 563, 565 |
| baseline 3 | 125311 px, x 624..1252 | x 624..832, 34801 px | 20962 | 2.127 @ -52 deg | row 188/351 (0.53 down) | 553, 560, 561, 563, 565 |

Two findings, both in the table:

1. **All six masks carry the ground band.** Every one of them reaches y 579 (near the picture's foot) and its inner
   mask's bottom is a near-constant 551..570 across the right-hand x buckets — a flat, horizontal floor. `probe.py`'s
   docstring promises the mask is "the stream, not the pool of burning fuel on the ground"; it is not. The pool is in
   every number this probe has ever printed, in both builds.
2. **Capture 1's ridge is not in the stream.** The row is image **y 413**, and at the near third's leftmost bucket
   (x 727..743) the inner mask runs y 357..408 — the stream's own body ends 5 px *above* the ridge. From the next
   bucket on the mask jumps down to 559. So y 413 is the horizontal line where the stream's underside meets the fire
   on the ground, not a cut through the white core. Captures 2 and 3 peak at +4 and +2 degrees halfway down an inner
   bbox whose bottom is that same floor.

### The attempted fix, and why it was reverted

A `stream_only(m)` was written into `near_and_tip`: the median y of the mask over its leftmost 10% of columns as the
nozzle spine, then column by column keep only the one vertical run containing the tracked y. Measured, it eats the
stream rather than the pool:

| still | near px (committed) | near px (stream_only) | mask x1 | seam |
|---|---|---|---|---|
| jet-t2-1 | 46558 | 18544 | 1041 (was 1249) | 7.057 |
| jet-t2-2 | 28063 | **2796** | 1041 (was 1218) | 0.000 (under the inner-px floor) |
| jet-t2-3 | 31154 | 15813 | 1251 | 3.234 |

A 6.6x spread where the plan's own check asks for 25%, the tip clipped at x 1041 in two of three, one capture with
nothing left to measure, and `core_over_tip_luma_min` down to 1.492 from 1.954 — the single-run walk snaps to the
wrong run at the stream's thin waist and abandons the far half of the jet. That is the risk the plan named for this
step, and its instruction: keep the old `near_and_tip`. `probe.py` is unchanged from round 4 and still exits 1 on
`seam_score_max` 11.427 alone. The bars did not move; no bar was re-derived, because the metric under it did not
change.

### What this leg settles, and what it does not

Settled, by measurement: **the number that fails the done-when is not measuring the owner's cut.** It is measuring
the top edge of the burning ground pool, which `firemask` merges into the stream and which `probe.py` claims it
excludes. The owner's cut may still be there — this says nothing about it either way — but 11.427 is not it.

Not settled: how to separate the stream from the pool. The one rule this leg could write for it is measurably worse
than no rule. Separating them honestly needs either a mask the renderer hands over (a per-book stencil or a capture
with `Book.Pool` suppressed, which is a harness change and a re-shoot) or a geometric rule stronger than a
column walk. Either is a new unit, not a change of bar. **The bar is not touched to make it pass.**
