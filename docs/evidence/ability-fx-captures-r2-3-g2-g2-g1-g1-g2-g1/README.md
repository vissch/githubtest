# The chlorine bank, round 3-g2-g2-g1-g1-g2-g1: one cloud, three sites, no fire in the side frame

The owner's reading of the last round: from the side and at zoom 30 the gas is **a carpet of flat green cards**,
not a cloud; the side band is **one site shot three times**, so its 1.61 spread is the cloud drifting, not three
places; and the **wreck fires are still in the side frame** (195 / 293 / 193 px). This round draws one body a
player can read, shoots three different sites again, and clears the fire out.

Step 1 of 4 is done: the measure. **No still of this round has been captured yet**, so `python probe.py` in this
folder exits 1 on missing files. That is expected until step 4 packs the new captures.

## What the measure now says that g2's could not

| Reading | What it is | Direction |
|---|---|---|
| `side_fill_frac` | blob pixels / the area under the silhouette top, per column from that column's own top row to its own bottom row | a **floor**: separate flat cards leave holes between them, one body fills its own hull |
| `side_top_scallops` | bumps in the column-top profile, smoothed over 15 px, counted when both saddles beside them stand >= 8 px above | a **ceiling**: one card is one bump, so a carpet counts many |
| `side_fire_box` | `[row0, row1, col0, col1, patches]` of the fire mask | locates the leftover fire instead of guessing at it |
| `side_site_spread` | max/min of `side_face_px` across the three **seeds within one run** (g2's `side_face_spread`, renamed) | a ceiling, now once more three different sites |
| `side_repeat_spread` | the worst, over the three sites, of max/min across the three **runs** (`--runs a b c`) | a ceiling: the cloud's own phase drift |

The split is the point. g2 shot one site on every seed, so its 1.83 never measured site noise at all - it
measured repeat noise and called it a spread. Both numbers now come out of the same nine shots.

## Before: the current build (g2's three captures), read by this probe

Nine side shots, three seeds per run, all at ONE site (x 56 / z 142) - that is what this round changes.

| | run1 (wait 200) | run2 (wait 215) | run3 (wait 230) |
|---|---|---|---|
| `side_face_px`, three seeds | 68979 / 111292 / 100423 | 60368 / 94968 / 96959 | 66647 / 98566 / 118311 |
| `side_fill_frac`, three seeds | 0.5743 / 0.6675 / 0.7322 | 0.6019 / 0.7153 / 0.7375 | 0.5843 / 0.7266 / 0.6849 |
| `side_top_scallops`, three seeds | 9 / 10 / 10 | 8 / 8 / 9 | 7 / 7 / 10 |
| `side_fire_px` (worst seed) | 293 | 101 | 96 |
| `side_fire_box` (worst seed) | rows 297-313, cols 837-958, 10 patches | rows 220-313, cols 834-1011, 11 | rows 220-307, cols 624-923, 19 |
| `side_site_spread` | 1.6134 | 1.6061 | 1.7752 |
| `side_outside_frac` (worst seed) | 0.2949 | 0.2863 | 0.2872 |

`--runs` over the three: `side_repeat_spread` per site **1.1426 / 1.1719 / 1.2202**, worst **1.2202**.

So, in numbers, the owner's two complaints:

- **The carpet.** The silhouette top carries **7 to 10 bumps** on every single shot, and the body fills only
  **0.57-0.74** of its own hull. A cloud a player reads as one thing is a handful of bumps and a high fill;
  this is a row of cards.
- **The spread was never a site spread.** Repeat noise alone is 1.14-1.22x. g2's 1.83 bar sat on top of that,
  so it had room for 50 % of drift that nothing in the capture could produce. It carries over here onto
  `side_repeat_spread`, which is what it actually measured, and onto nothing else.
- **The fire.** 8 to 19 separate patches, in a band on the right of the frame (columns 620-1145, rows 220-313),
  worst 293 px.

## The bars today

| Bar | g2 | here | Why |
|---|---|---|---|
| `side_site_spread` | - (was `side_face_spread` <= 1.83) | **<= 3.03** | the owner's own number. It BITES g1's three-site capture (3.0308 misses it). Three sites return, so a one-site bar cannot be carried onto them |
| `side_repeat_spread` | - | **<= 1.83** | g2's bar, unchanged, moved onto the number it measured |
| `side_outside_frac`, `side_fire_px`, `side_face_px`, `t1_changed_frac`, `texture_var_drop`, `o240_hue_dist_deg` | 0.31 / 393 / 48000 / 0.08 / 0.40 / 108.0 | unchanged | no bar is ever widened here |
| `side_fill_frac`, `side_top_scallops` | - | **reported, not barred** | a bar is only ever derived from a capture, and this round's capture does not exist yet. Step 4 sets them from the new stills |

`side_face_px` is kept as it stands for now; the plan warns it may have to FALL when the cards get fewer and
softer, and if it does, step 4 re-derives it by the unchanged rule and says so here.

## Reproducing

```
python probe.py --selftest        # hand calculations, including the new fill, scallop and fire-box cases
python probe.py --dir ../ability-fx-captures-r2-3-g2-g2-g1-g1-g2 --out /dev/null          # the before table
python probe.py --runs ../ability-fx-captures-r2-3-g2-g2-g1-g1-g2 ../ability-fx-captures-r2-3-g2-g2-g1-g1-g2/run2 ../ability-fx-captures-r2-3-g2-g2-g1-g1-g2/run3
python probe.py                   # this folder -> exit 1 until step 4 packs the stills
```

The plan predicted the `--dir` line above would exit **1** at step 1. It exits **0**, and honestly so: the two
readings that bite the carpet (`side_fill_frac`, `side_top_scallops`) are reported and not yet barred, because
no capture of this round exists to derive a bar from. It must exit 1 once step 4 sets them.
