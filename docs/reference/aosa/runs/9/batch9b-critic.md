# Batch 9b: C105 (vat.separation) and C106 (fx.smokeShear), measured contrast and blind pairs

Runs 9/n0 (defaults, bit-identical to b0 and c101), 9/n5 (vat.separation=1), 9/n6 (fx.smokeShear=1), 9/n56 (both):
release player, barrage, ShelledForest night, T1, shot-tick 140, 64 held frames, no HUD. Day: 9/n0d and 9/n5d
(WinterLine, tick 100, one frame, vat.separation off and on).

## Step 1: diff against n0

Pixels changed (> 8 levels in any channel) against n0, every 4th frame:

| | f0 | f16 | f24 | f40 | f56 | f60 |
|---|---|---|---|---|---|---|
| n5 | 0.061 | 0.059 | 0.055 | 0.064 | 0.049 | 0.046 |
| n6 | 0.267 | 0.311 | 0.353 | 0.410 | 0.470 | 0.437 |
| n56 | 0.308 | 0.351 | 0.389 | 0.446 | 0.499 | 0.465 |

Day n5d against n0d: 0.097. The n5 change sits only on the men, in both trenches and in the far crowds. The n6 change
is the smoke. The dark opaque cauliflower clouds go thin and see-through, and the orange earth-column arcs behind
them are left exposed.

## Step 1b: measured figure/ground contrast (C105)

**Segmentation.** Only the men change between n0 and n5, so the men mask is the pair's own diff: pixels changed by
more than 8 levels in any channel, closed 5x5, holes filled, opened 3x3. Both images use the same mask. The ground
is a ring 5-14 px outside the mask (mud, sandbags, trench wall). I checked the mask on overlays (LT f0, TR f40): it
covers the packed crowd, including men under smoke, and the ring sits on the mud and the sandbags.

Regions: night LT x0-600 y150-1080, TR x1100-1700 y100-450. Day LT x80-760 y100-1080, TR x1350-1920 y0-560.
Frames f0, f8, f16, f24, f40, f56.

**Figure/ground (the crowd against the mud around it).** dL* is the men's mean L* minus the ring's, and d' is the
luma separation of the two distributions.

| region, frame | dL* off -> on | d' off -> on |
|---|---|---|
| LT f0 | -2.11 -> +1.60 | 0.245 -> 0.162 |
| TR f0 | -4.10 -> -1.28 | 0.234 -> 0.071 |
| LT f8 | -2.24 -> +1.51 | 0.267 -> 0.155 |
| TR f8 | -2.54 -> +0.11 | 0.186 -> 0.008 |
| LT f16 | -4.89 -> -0.96 | 0.329 -> 0.061 |
| TR f16 | -6.60 -> -3.01 | 0.301 -> 0.135 |
| LT f24 | -3.41 -> +0.20 | 0.352 -> 0.019 |
| TR f24 | -3.54 -> -0.46 | 0.210 -> 0.027 |
| LT f40 | -1.38 -> +2.55 | 0.156 -> 0.252 |
| TR f40 | -2.67 -> +0.15 | 0.218 -> 0.012 |
| LT f56 | -1.49 -> +2.39 | 0.174 -> 0.241 |
| TR f56 | -13.51 -> -10.53 | 0.812 -> 0.625 |
| **night mean** | **abs 4.04 -> 2.06 (-49%)** | **0.290 -> 0.147 (-49%)** |
| day LT | -27.65 -> -20.41 | 1.280 -> 0.910 (-29%) |
| day TR | -20.55 -> -13.57 | 1.196 -> 0.759 (-37%) |

At night the men are a dark mass on slightly lighter mud. The separation lift, jitter and thinner ink raise the
men's mean onto the mud's, so the crowd as a mass stands out less in 12 of 14 region-frames (it rises only at LT
f40 and f56). This is the other session's bench trap, reproduced: the unit lift lowers figure/ground.

**Neighbour separation (men inside the crowd)**: connected components of pixels above the threshold inside the
mask, area 12-600 px. T is Otsu on n0's men, held fixed for both images. The boundary gradient is the mean Sobel
magnitude on the mask's 2 px edge.

| | blobs at fixed T, off -> on | blobs at own Otsu, on | boundary gradient | inner RMS luma |
|---|---|---|---|---|
| night LT (mean of 6) | 197 -> 382 | 255 (+30%) | 43.8 -> 56.6 (+29%) | 22.3 -> 27.7 |
| night TR (mean of 6) | 69 -> 102 | 89 (+29%) | 67.2 -> 74.4 (+11%) | 29.0 -> 31.3 |
| day LT | 718 -> 892 | 932 | 251 -> 197 (-21%) | 50.1 -> 55.0 |
| day TR | 227 -> 216 | 258 | 208 -> 143 (-31%) | 41.2 -> 45.2 |

At night, man-to-man separation rises: more helmet blobs, a harder crowd edge, more texture. The crowd-to-mud
contrast halves. At day, both the crowd-to-snow contrast and its edge fall.

## Step 2: blind setup

19 matched pairs, same frame, X and Y shuffled per pair (seed 90527), dealt across three separate general-purpose
critics (7, 6 and 6 pairs) so no critic saw one variant only. Each pair had both full frames, a 1:1 LEFT trench
X|Y sheet and a 2x TOP-RIGHT trench X|Y sheet. The critics were told nothing about knobs or cards. Each scored X and
Y 0-10 on barrage weight, men countable in the LEFT trench, men in the TOP-RIGHT trench, and smoke as real drifting
smoke (n/a for day).

Frames: n5 f0 f8 f16 f24 f40 f56; n6 f4 f16 f24 f36 f48 f60; n56 f8 f20 f28 f40 f52 f62; day pair.

## Key and scores (off = n0 / n0d, on = the knob)

| variant | frame | critic.pair | X is | weight off -> on | LT off -> on | TR off -> on | smoke off -> on |
|---|---|---|---|---|---|---|---|
| n5 | f0 | 1.1 | off | 6 -> 6 | 3 -> 5 | 2 -> 3 | 4 -> 4 |
| n5 | f8 | 2.3 | off | 6 -> 6 | 3 -> 5 | 2 -> 3 | 5 -> 5 |
| n5 | f16 | 3.1 | on | 7 -> 7 | 3 -> 4 | 2 -> 3 | 4 -> 4 |
| n5 | f24 | 3.6 | on | 7 -> 7 | 3 -> 4 | 2 -> 3 | 4 -> 4 |
| n5 | f40 | 1.4 | off | 7 -> 7 | 3 -> 5 | 2 -> 3 | 3 -> 3 |
| n5 | f56 | 3.3 | off | 7 -> 7 | 3 -> 5 | 2 -> 3 | 4 -> 4 |
| day | f0 | 3.5 | on | n/a | 4 -> 5 | 4 -> 6 | n/a |
| n6 | f4 | 1.5 | on | 7 -> 5 | 4 -> 4 | 2 -> 3 | 4 -> 3 |
| n6 | f16 | 2.1 | on | 7 -> 5 | 3 -> 4 | 2 -> 2 | 5 -> 3 |
| n6 | f24 | 2.2 | off | 7 -> 5 | 3 -> 4 | 3 -> 2 | 5 -> 3 |
| n6 | f36 | 1.6 | off | 7 -> 5 | 3 -> 4 | 3 -> 2 | 3 -> 3 |
| n6 | f48 | 1.2 | on | 7 -> 5 | 4 -> 4 | 2 -> 2 | 3 -> 2 |
| n6 | f60 | 1.7 | on | 7 -> 5 | 3 -> 4 | 2 -> 2 | 2 -> 3 |
| n56 | f8 | 2.6 | off | 6 -> 5 | 3 -> 6 | 2 -> 3 | 5 -> 3 |
| n56 | f20 | 2.5 | off | 7 -> 5 | 4 -> 5 | 2 -> 4 | 5 -> 3 |
| n56 | f28 | 3.4 | on | 6 -> 5 | 3 -> 4 | 2 -> 3 | 4 -> 3 |
| n56 | f40 | 3.2 | on | 6 -> 4 | 3 -> 4 | 2 -> 3 | 4 -> 3 |
| n56 | f52 | 1.3 | off | 7 -> 5 | 3 -> 5 | 2 -> 3 | 3 -> 4 |
| n56 | f62 | 2.4 | off | 7 -> 5 | 3 -> 5 | 1 -> 1 | 4 -> 3 |

Means, off -> on (delta; pairs up/down):

| | weight | men LT | men TR | smoke |
|---|---|---|---|---|
| n5 (6) | 6.67 -> 6.67 (0) | 3.00 -> 4.67 (+1.67; 6/0) | 2.00 -> 3.00 (+1.00; 6/0) | 4.00 -> 4.00 (0) |
| day (1) | n/a | 4 -> 5 (+1) | 4 -> 6 (+2) | n/a |
| n6 (6) | 7.00 -> 5.00 (-2.00; 0/6) | 3.33 -> 4.00 (+0.67; 4/0) | 2.33 -> 2.17 (-0.17; 1/2, min -1) | 3.67 -> 2.83 (-0.83; 1/4) |
| n56 (6) | 6.50 -> 4.83 (-1.67; 0/6) | 3.17 -> 4.83 (+1.67; 6/0) | 1.83 -> 2.83 (+1.00; 5/0) | 4.17 -> 3.17 (-1.00; 1/5) |

What the critics named:
- **n5 at night:** helmets lighter with more contrast, so the trench separates into men, where off is "a dark green
  mass" or "brown mush".
- **Day:** off's thick black outlines merge whole groups into single blobs, and on's squad is countable.
- **n6:** the thin smoke is "too pale to register as shellfire". The exposed orange plumes read as "rabbit-ear"
  stickers or a broken sprite, the weakest element on screen. The dark clouds read heavier, though still as flat
  cut-outs.

## Step 3: decision

- **C105, vat.separation=1: FAILS on the measured-contrast condition, though it passes blind.** Men rise in 7 of 7
  pairs (night LT +1.67, TR +1.00; day +1/+2), with weight and smoke unchanged. Neighbour separation rises at night:
  blobs +30% at their own threshold, crowd edge +11-29%. But the crowd-to-ground contrast falls, night mean abs dL*
  4.04 -> 2.06 and d' 0.290 -> 0.147 (-49%), falling in 12 of 14 region-frames. Day d' falls -29%/-37% and the edge
  -21%/-31%. The brief's condition (the measured contrast must not fall) is not met. Default change pending: reject.
  Next: split the lift from the rest. The moon lift and the loss of dark ink raise the men's mean onto the mud. Try
  the distance-thinned outline plus the per-man jitter with the lift at 0 (or a lift that darkens toward the mud's
  opposite), then re-measure figure/ground and neighbour separation on these frames.
- **C106, fx.smokeShear=1: FAILS.** Its target falls (smoke 3.67 -> 2.83, down in 4 of 6), and weight falls 2.0 in
  6 of 6 (limit 0.5). Men LT +0.67 and TR -0.17 (two pairs -1, within rule 6's limit) do not rescue it. Default
  change pending: reject. Thinning the clouds also exposes the column arcs, which read as stickers (C102/C103
  ground).
- **n56 as a pair: FAILS.** Weight -1.67 (0/6 up) and smoke -1.00. Its men gain (+1.67 / +1.00) is C105's alone.

## Follow-up C105s: the knob split (m-runs)

Runs 9/m0 (default; bit-identical to n0 and n0d: 0 of the pixels differ at f0 and f40), m1 (vat.separation=1,
vat.sepLift=0: outlines and symmetric jitter, no lift), m2 (as m1, plus vat.sepJitterDark=1: jitter darker only),
m3 (vat.sepOutline=1: outline thinning only). Day runs m0d, m2d and m3d. Same frames and regions as above. The men
mask is batch9b's n0/n5 mask, fixed for every variant so the numbers compare (m3's own diff covers only the outline
pixels). The blob threshold is Otsu on the default's men, as above. The table gives the mean d' (men vs the mud ring
around them), then blobs at the fixed T / blobs at each image's own Otsu / crowd-edge gradient.

| | night LT | night TR | day LT | day TR |
|---|---|---|---|---|
| default | 0.254, 197/197, 43.8 | 0.327, 69/69, 67.2 | 1.280, 718/718, 251 | 1.196, 227/227, 208 |
| n5 (C105) | 0.148 (-42%), 382/255, 56.6 | 0.146 (-55%), 102/88, 74.4 | 0.910 (-29%), 892/932, 197 | 0.759 (-37%), 216/258, 143 |
| m1 (no lift) | 0.069 (-73%), 277/249, 49.7 | 0.227 (-31%), 82/70, 69.8 | n/a | n/a |
| m2 (no lift, dark jitter) | 0.250 (-2%), 202/236, 44.1 | 0.319 (-2%), 64/60, 65.3 | 1.095 (-14%), 906/906, 191 | 0.895 (-25%), 237/265, 139 |
| m3 (outline only) | 0.063 (-75%), 271/246, 49.5 | 0.234 (-28%), 82/70, 69.6 | 1.025 (-20%), 895/899, 193 | 0.843 (-30%), 224/248, 140 |

Findings:
- **The lift was not the main cause.** m3, with the outline thinning alone, loses as much men-vs-mud as m1. The thin
  ink removes the dark rim that kept the crowd's mean below the mud. The lift even helped the left trench a little
  (n5 0.148 against m1 0.069).
- **m2's darker jitter pays the loss back at night** (d' -2% and -2%, abs dL* 2.59 -> 2.44 and 5.50 -> 5.36). Its
  separation gain is small, though: blobs at the fixed T go 197 -> 202 (LT) and 69 -> 64 (TR), and the crowd edge is
  flat.
- **By day every variant loses separation.** m2d is -14% and -25%, with the crowd edge -24% and -33%, because all
  of them thin the outline that carries the men against the snow.

**Blind critic: not run.** Every variant's measured separation falls: m1 and m3 by 28-75% at night, and m2 by 2% at
night and 14-25% at day. By the rule (skip the ones that fall), none goes to the critic.

**Decision: no variant passes, and no default is proposed.** C105 stays at default 0, pending reject. Next, and
untested: the jitter alone with the outline left as it is (vat.sepJitter=1, vat.sepJitterDark=1, vat.sepOutline=0,
vat.sepLift=0). m2 shows that the dark jitter holds men-vs-mud at night. Keeping the outline would keep the day's
contrast, and whether the jitter alone separates the helmets has to be measured.

## Follow-up C105s-2: m4, darker jitter only (outline and lift off)

Runs 9/m4 and m4d: vat.sepJitter=1, vat.sepJitterDark=1, vat.sepOutline=0, vat.sepLift=0. Against m0, 1.8-2.1% of the
pixels change (f0, f16, f40). Measured exactly as above (fixed n0/n5 mask, T = Otsu on the default's men):

| | d' off -> m4 | blobs at fixed T / own Otsu | crowd edge |
|---|---|---|---|
| night LT | 0.254 -> 0.425 (+67%, up in 6/6) | 197 -> 136 / 181 | 43.8 -> 39.7 |
| night TR | 0.327 -> 0.397 (+21%, up in 6/6) | 69 -> 54 / 51 | 67.2 -> 63.5 |
| day LT | 1.280 -> 1.343 (+5%) | 718 -> 735 | 251 -> 249 |
| day TR | 1.196 -> 1.245 (+4%) | 227 -> 238 / 232 | 208 -> 207 |

Men-vs-mud separation holds, and rises, by night and by day. But the helmet blobs fall at night (-31% LT, -22% TR):
darker men stand out from the mud and merge into each other. By the rule, it goes to the critic.

**Blind critic** (one foreground general-purpose agent, 6 night pairs f0 f8 f16 f24 f40 f56 plus the day pair,
X and Y shuffled, seed 4104). Key: m4 is Y in pairs 1, 2 and 4, and X in pairs 3, 5, 6 and 7 (pair 1 = f40,
2 = day, 3 = f16, 4 = f8, 5 = f24, 6 = f0, 7 = f56).

| | weight | men LT | men TR | smoke |
|---|---|---|---|---|
| night (6) | 6.50 -> 6.50 | 4.33 -> 3.33 (0 up, 6 down) | 2.00 -> 2.00 | 3.83 -> 3.83 |
| day | n/a | 6 -> 5 | 6 -> 6 | n/a |

The critic's words: m4's helmets are "darker with a grainy speckle, so the domes merge". The default's "smoother lit
domes separate better" and keep their top highlight.

**Decision: m4 fails rule 6.** The men fall 1 point in the left trench in 7 of 7 pairs, day included. No C105
variant passes. Over five variants, the one lever trades against the other. Lighter or thinner-inked men separate
from each other but sink into the mud (n5, m1, m3). Darker men stand out from the mud but merge (m4). m2 sits
between and gains neither. C105 is rejected at default 0. A further try would have to separate the men with
something other than their value, such as hue or a rim highlight on the helmet dome, and not by darkening or
thinning.
