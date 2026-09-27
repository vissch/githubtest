# C102: fx.columnHard, the toon cut on the earth column. Where it changes, and a blind 2-way against the default

Build 09f5c0d, the knob with default 0. Both runs are release player, scenario barrage, shot-tick 140, shot-frames 64,
`--no-hud`: runs 9/c102n-1 (no knob) and 9/c102h-1 (fx.columnHard=1).

**Old look.** c102n against the pre-patch build's 9/c101 had changed_frac 0.00000 at f16, f24, f40 and f60
(lander). The default is bit-identical to the old image.

## Where c102h differs from c102n

A pixel counts as changed when a channel moves by more than 8, the same threshold as `stilldiff`. The regions are the
largest connected parts of the changed mask (full-frame x0-x1, y0-y1).

| frames | changed_frac | where |
|---|---|---|
| f0-f7 | 0.0007-0.0019 | a right-side column at (1538-1707, 430-600) and the centre column's arcs at (800-930, 685-775) |
| f8-f13 | 0.0004-0.0008 | the centre column's arcs at (805-927, 660-740) |
| f14-f34 | 0.0002-0.0005 | the centre column (806-913, 610-715). From f23 there are also the left-edge column's tips at (0-56, 200-315), next to the top-left trench |
| f35-f49 | 0.0004-0.0018 | the centre and left columns, plus new arcs at (730-960, 855-985) |
| f50-f63 | 0.0017-0.0039 | a fresh centre burst's column, as arcs at (725-1001, 740-1020) |

Frames above 0.0005: f0-f7, f10-f13, f29, f35-f37, f40-f47 and f49-f63.

**It is the column.** Crops of f0, f12, f28, f44, f50 and f57 were compared side by side with the mask. Every changed
region is the orange-brown arcs of the Column book. With the cut on, their soft translucent falloff becomes a
hard-edged, more solid silhouette. Nothing else changes: smoke, bursts, flashes, tracers and men are the same.

**The change is small.** The column is small at the default size (fx.columnEarthSize 0.38), so the cut touches at
most 0.4% of the frame (f50, f57).

## Blind setup

The critic got six pairs, frames 12, 28, 43, 50, 57 and 60. Each pair had both full frames and a 2x crop of the
largest changed region. X and Y were shuffled per pair (seed 102009), and the critic was told the assignment was
independent per pair. It was a separate general-purpose agent. It was told only what to score, not what had
changed or which image was new.

Key (new, fx.columnHard=1 =): pair1 X, pair2 Y, pair3 Y, pair4 Y, pair5 Y, pair6 Y. The critic unshuffled it
correctly on its own from the change under test ("the solid build is X in pair 1 and Y in pairs 2 to 6"). That is
the effect being seen, not a leak.

| pair | frame | crop (full-frame x0, y0, x1, y1) |
|---|---|---|
| 1 | f12 | 714, 543, 1016, 845 |
| 2 | f28 | 0, 116, 157, 376 |
| 3 | f43 | 803, 775, 1063, 1035 |
| 4 | f50 | 801, 734, 1089, 1022 |
| 5 | f57 | 692, 635, 1090, 1033 |
| 6 | f60 | 620, 657, 1074, 1080 |

## Critic scores, unblinded (0-10)

| pair (frame) | earth old | earth new | men old | men new | weight old | weight new |
|---|---|---|---|---|---|---|
| 1 (f12) | 2 | 3 | 5 | 5 | 7 | 7 |
| 2 (f28) | 2 | 3 | 6 | 6 | 6 | 6 |
| 3 (f43) | 3 | 5 | 5 | 5 | 7 | 7 |
| 4 (f50) | 4 | 7 | 6 | 6 | 7 | 8 |
| 5 (f57) | 4 | 7 | 6 | 6 | 7 | 8 |
| 6 (f60) | 4 | 5.5 | 6 | 6 | 7 | 7 |
| **mean** | **3.17** | **5.08** | **5.67** | **5.67** | **6.83** | **7.17** |
| **delta** | | **+1.92** | | **0** | | **+0.33** |

**Readability holds (rule 6).** The men score was equal in all six pairs, and the critic found no difference outside
the column. In pair 2 the left-edge column's clod "sits just below the men and does not cover them".

**The column reads as earth.** It scored higher in all six pairs. The solid build "reads clearly as a fountain of
thrown dirt". The soft one "reads as orange flame or sparks", and in pair 3 as a hole in the smoke.

**Weight rose only +0.33**, against the card's +1. It was +1 in the two pairs with the most column on screen and
equal in the other four. The column is too small for a cut alone to carry the barrage.

**Risk named by the critic, not tested here.** The solid earth hides what is behind it. A burst landing on a trench
would hide the men under it more with the cut on. None of the six frames puts a column over men.

## Critic output (verbatim)

| pair | X col | Y col | X men | Y men | X weight | Y weight |
|---|---|---|---|---|---|---|
| 1 | 3 | 2 | 5 | 5 | 7 | 7 |
| 2 | 2 | 3 | 6 | 6 | 6 | 6 |
| 3 | 3 | 5 | 5 | 5 | 7 | 7 |
| 4 | 4 | 7 | 6 | 6 | 7 | 8 |
| 5 | 4 | 7 | 6 | 6 | 7 | 8 |
| 6 | 4 | 5.5 | 6 | 6 | 7 | 7 |

Pair 1 (f12)
- col: X has a small orange-tan blob at about (820-880, 680-720). It is faint but reads as dust or earth. In Y the same blob is paler and olive and merges into the green tracers, so it reads as haze. Neither is a column.
- men: the same in both. The left trench crowd (220-560, 450-1080) is readable under brown haze.
- weight: the same in both. Big flashes, a heavy smoke column and dense tracers.

Pair 2 (f28)
- col: both are mostly a white-orange glow, which reads as flame. Y adds a small opaque tan-brown clod at the crop's left edge. X has only a faint orange tint there.
- men: the same in both. The packed helmets are countable and the trench edge is clear. Y's clod sits just below the men and does not cover them.
- weight: the same in both.

Pair 3 (f43)
- col: in X the gap in the smoke is grey and see-through, with ground showing through and only an orange rim. It reads as a hole, not earth. In Y the same shape is opaque orange-brown and reads as a clod of earth in the smoke.
- men: the same in both. The left trench (200-560, 480-1080) stays legible.
- weight: the same in both.

Pair 4 (f50)
- col: X's spray at about (750-990, 790-1000) is a soft, see-through orange. The ground shows through, so it reads like a sparkler or flame fountain. Y's is solid brown with crisp arcs that hide the ground behind them. It reads clearly as a fountain of thrown dirt.
- men: the same in both. No men near this burst.
- weight: Y is a little higher. The solid brown mass adds bulk.

Pair 5 (f57)
- col: X's arcs at about (740-1000, 770-1000) are thin and see-through, so they read as flame jets. Y's are opaque brown with solid tapering stems, so they read as earth.
- men: the same in both. No men in the crop.
- weight: Y is slightly heavier.

Pair 6 (f60)
- col: both are thinner late arcs. X's are soft and semi-transparent. Y's are a bit thicker, more opaque and browner, so they read slightly more as earth. The gap is smaller than in pairs 4 and 5.
- men: the same in both.
- weight: the same in both.

General observations
- The builds differ only in how the earth thrown up by an impact is drawn: solid, hard-edged brown in one and soft, see-through orange in the other. There was no difference in tracers, smoke, flashes, lighting or soldiers.
- In pairs 1 and 2 the difference is marginal. The solid build is clearly better in pairs 3-5 and slightly better in pair 6.
- Neither build hides the men in these frames. The solid earth blocks everything behind it, so a burst landing on a trench would hide the men under it more in the solid build, but none of these six frames tests that. The dark smoke columns hide far more of the scene than either version of the earth.

## Decision

The earth score rose (+1.9), and readability held (0 in all six pairs), so rule 6 passes. The default-change patch is
`runs/9/C102-default.patch`: DefaultColumnHard 1, with the test expecting the new default and fx.columnHard=0 as the
old look. It compiles with occ.py. It is for the lander to land, then check that fx.columnHard=0 is bit-identical to
9/c102n, and that the no-knob run is bit-identical to 9/c102h.

The weight did not reach +1, because the column is too small to carry the barrage. The next step is C103: the
column's size and dark core. Any C103 still must put a column over men, the risk this critic named.
