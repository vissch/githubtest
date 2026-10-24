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
