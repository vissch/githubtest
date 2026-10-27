# The chlorine bank, round 3-g1-g2: a side view, and a ceiling on the flat green fill

The owner's words (2026-10-20), on g1's result: "texture_var_drop is 0.70 against a bar of 0.40, which rewards the
flat green fill, and gas_t1_height_m 8.3-9.9 has no side view a player can read as a cloud. Shoot each seed from
the side at zoom 30 and fail a bank whose blocks are flatter than the ground."

Two new bars answer that, and g1's four (`t1_changed_frac`, `texture_var_drop` as a floor, `o240_hue_dist_deg`,
`t1_gap_frac`) stay exactly as they were, as regression bars.

## The side view

`GasBankStills` now takes a THIRD still of every seed: the same cloud, the same zoom (30), the same block - so the
pass timeline, the seed patches and the plate's frame-for-frame match are untouched - but at pitch 8 instead of
25. 8 degrees is `TacticalCamera.PitchMin`, the lowest a player can tilt, and `CaptureRig` clamps a shot to it
anyway, so this is the flattest look at the bank the game ever gives. Stems `gas-side-s0/1/2` and their plates:
18 stills a pass, not 12. Both stills sit inside the existing 520-frame block pad (a block spends about 250), and
there is no second gas call and no second drain.

## The side measure, corrected (2026-10-21)

**The first side bar was an artifact.** `side_relief_ratio` divided the bank's top relief by the relief of the
side blob's BOTTOM edge, and both were read at one fixed plane - the seed's own, 77.4 m out. The bottom pixels of
the blob are not the bank's foot at 77 m: at pitch 8 with a 25 deg vertical FOV the frame's bottom ray leaves
about 20.5 deg below the horizon and meets the ground some 29 m from the camera. Read at 77 m instead, it lands
10.87 - 77.4*tan(20.5 deg) - 2.77 = -20.8 m. That is why the first baseline's `side_base_m` was **-15.27 /
-7.93 / -13.69 m** for a bank that lies ON the ground, why its "ground relief" was 3.5-11.3 m, and why
`SIDE_BAR` came out at 1.8128. Geometry, not terrain.

What the measure does now:

- **the denominator is not a pixel reading at all.** `GasBankStills` samples the render ground at the centre of
  every gas cell at or over the 0.8 floor the bank's own `Refresh` uses, and writes `ground_relief_m` (p90 - p10,
  `relief`'s own formula) and `ground_cells` into the still's manifest row. The gassed cells ARE the bank's
  footprint, so nothing about it is guessed.
- **the blob is trimmed to the bank.** A pixel whose own per-pixel `ground_distance` lands more than
  `SIDE_PLANE_WINDOW_M` (10 m) from the seed plane is dropped - which is what the frame's bottom rows are. Well
  under the horizon the per-pixel route is sound, and that is exactly where the trim is applied. 10 m, not the
  plan's 25 m: a pixel 25 m short of the plane reads about 2.6 m under the ground, and the baseline's median base
  came back at -4.5 m against the 1.5 m sanity bar. The trim leaves 32k-65k px and 850-1280 columns of bank.
- **the numerator is the TOP edge only**, each column turned into a height in the seed's own vertical plane (at
  8 degrees a metre of distance error is about 0.01 m of height).
- **`side_base_m` is a sanity row, and `main` fails on it:** the bank's foot lies on the ground, so its median
  must sit within 1.5 m of 0. It now reads -1.37 / -1.41 / -1.18 m.

`--selftest` gained the side still's own hand calculation: s2's sidecar (cam_y 10.87328, pitch 8, ground_y 2.7679)
at its 77.37 m plane, centre pixel, is 10.87328 - 77.37*tan(8 deg) - 2.7679 = **-2.770 m** - the fixed plane aims
under the ground at this pitch, which is why only p90 - p10 is ever read off it, never an absolute height - and the
vectorised distance grid the trim uses is checked against the scalar `ground_distance` at three pixels.

## The drop ceiling

`texture_var_drop` keeps its 0.40 floor and gains a ceiling, `DROP_CEILING`, checked `max(got) <= DROP_CEILING`.
A drop is `1 - var_gas/var_plate`, so a ceiling is a floor on how much of the ground texture is still read through
the bank. It is the same number the owner says rewards the flat green fill, barred the other way round, so the
reading cannot be won by painting the frame green.

## Both bars, from this round's own baseline

The control is the current build plus the side still and the probe - no look change: `baseline/` and
`baseline.json`, re-shot on 2026-10-21 because the manifest rows gained `ground_relief_m` and the old stills
cannot be re-measured without it. The family's formulas, and never widened afterwards:

    SIDE_BAR     = baseline_max + max(0.25, spread)      higher is better
    DROP_CEILING = baseline_min - max(0.05, spread)       lower is better (a ceiling)

| Seed | `side_relief_ratio` | `side_bank_relief_m` | `ground_relief_m` (cells) | `side_base_m` | `texture_var_drop` |
|---|---|---|---|---|---|
| s0 | 0.0214 | 0.042 | 1.961 (772) | -1.374 | 0.7210 |
| s1 | 0.0277 | 0.048 | 1.750 (678) | -1.413 | 0.7237 |
| s2 | 0.3321 | 0.896 | 2.698 (550) | -1.181 | 0.6922 |

`side_relief_ratio`: spread = 0.3321 - 0.0214 = 0.3107, margin = max(0.25, 0.3107) = 0.3107,
**`SIDE_BAR` = 0.3321 + 0.3107 = `0.6428`** (the old 1.8128 is gone with the artifact).

`texture_var_drop`: spread = 0.7237 - 0.6921 = 0.0316, margin = max(0.05, 0.0316) = 0.05,
**`DROP_CEILING` = 0.6921 - 0.05 = `0.6421`** - unchanged, and the re-shoot reproduced it to four decimals
(0.7210 / 0.7237 / 0.6922), which is the frame-for-frame match of the two passes measured.

Every seed's ground relief is 1.75-2.70 m, over the 0.5 m under which the ratio stops being trustworthy, so the
bar is set rather than left unset (the round's own guard). The corrected numbers say the owner's complaint far
more plainly than the artifact did: the bank's top edge rises and falls 0.04-0.90 m where the ground under it
rises and falls 1.75-2.70 m - 2 to 33 per cent as much. The bar now asks for 1.26-1.73 m of top-edge relief,
which is of the order of the 1.75 m of gas a card draws, where the artifact's bar asked for 13-20 m.

The baseline run is the control and exits 1: it misses both bars it sets, meets all four of g1's, and its
`side_base_m` passes the sanity check.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure: g1's four bars (regression, unchanged) plus `side_relief_ratio` and `DROP_CEILING`. `--selftest`, `--pack`, `--dir`, `--out` unchanged in shape |
| `baseline/` + `baseline.json` | three seeds, three bands, of the build **before** this round's fix (`f427286f0d`) - the control both new bars are set against |

The root of this folder holds the 18 "after" stills of the committed ridge look (2026-10-27, see below);
`probe.py` with no arguments measures them and exits 1.

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass, 9 stills
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, the other 9
python docs/evidence/ability-fx-captures-r2-3-g1-g2/probe.py --pack <dir>
```

## The fix, step by step

| Step | Change | `side_relief_ratio` worst | `texture_var_drop` worst | Note |
|---|---|---|---|---|
| baseline (artifact) | first side still, bottom-edge denominator | 0.3252 (s2) | 0.7237 (s1) | `side_base_m` -15.3 m: the measure, not the look |
| part 1, 2026-10-21 | the measure corrected, the look untouched | 0.0214 (s0) | 0.7237 (s1) | `SIDE_BAR` re-set to 0.6428, `side_base_m` within 1.5 m |
| step 6, `GasBankLift` floor 0.6 | a per-card height factor 0.6-1.0 off the card's hash, all three tiers | 0.0236 (s1) | 0.6093 (s1) | the drop lands INSIDE 0.40..0.6421, but `t1_gap_frac` goes to 0.44-0.45 over g1's 0.3559 |
| step 6, floor 0.8 | the same, lowering cards less | 0.0296 (s0) | 0.6686 (s1) | the gap comes back to 0.26-0.37, the drop goes back over its ceiling |
| step 7, `GasBankThin` 0.95 -> 0.90 | at floor 0.8 | 0.0248 (s1) | 0.6792 (s1) | the alpha lever moves the drop the WRONG way: three overlapping tiers set the coverage, not one card's alpha |
| landed | **nothing** - the look is reverted | 0.0214 (s0) | 0.7237 (s1) | the baseline's own numbers; `decisions.md` option (a) stands |
| round 2, `GasBankRidge` 44 m / floor 0.55 | a world-space sine lowers whole regions of both merged tiers | 0.0505 (s0) | 0.6505 (s1) | the best of the four: s1's top edge rose ten-fold (0.0277 -> 0.2517), `t1_gap_frac` 0.3919 |
| round 2, wave 56 m | the same, a longer wave | 0.0226 (s1) | 0.6629 (s1) | worse: the bank sits on one flank of the sine and is lowered evenly |
| round 2, wave 64 m | the same, longer still | 0.0229 (s1) | 0.6790 (s1) | worse again, and the drop climbs back toward the baseline |
| round 2, wave 64 m / floor 0.45 | the same, cards lowered further | 0.0226 (s1) | 0.7262 (s1) | no gain; `t1_gap_frac` 0.3958, the worst of the four |
| landed, round 2 | the ridge itself (44 m, floor 0.55), **shot as evidence only** | 0.0505 (s0) | 0.6505 (s1) | an eighth of the 0.6428 bar; option (a) stands, see `decisions.md` |

## Why part 2 landed no look change (2026-10-21)

Three shoots under the man rule, in the table above. Two findings close the round:

- **`texture_var_drop` and `t1_gap_frac` are one lever seen twice.** Lowering cards shows more ground (the drop
  falls, which the owner asked for) and opens holes (the gap rises, which g1 bars). They move strictly against each
  other and roughly linearly; the gap bar is met near a lift floor of 0.83, where the drop is about 0.677, over its
  0.6421 ceiling by 0.035. No setting meets both. `GasBankThin`, step 7's lever, moves the drop the wrong way.
- **`side_relief_ratio` is out of reach of card height.** At pitch 8 the camera is 10.9 m up and the gas tops out at
  1.75 m a card, so the side still is a green blanket seen from above, and its top edge is a MAX over heavily
  overlapping cards - a per-card height spread is smoothed away before it reaches the silhouette (0.02-0.35 against
  the 0.6428 bar at every setting tried). An undulating edge needs a bank that stands up (option (b), against the
  man rule) or a spatially correlated height, which is a new design.

So the round ends where part 1 left it: a sound measure, two bars set from the control, and the open owner row.

## Why round 2 landed no passing look (2026-10-27)

The one design part 2 named as untried is now built and shot: `GasBankRidge(x, z)`, a fixed world-space sine that
lowers a MERGED gas card to as little as `GasBankRidgeFloor` of its height by WHERE it stands, so a max over
overlapping cards cannot average the variation away the way a per-card spread was averaged away. It works in kind
and fails in size. s1's top edge went 0.0277 to 0.2517 at a 44 m wave - the ridge does reach the silhouette - but
the worst seed of the best setting is 0.0505 against the 0.6428 bar, and both longer waves and a deeper floor make
it worse: a wave much longer than the bank's own footprint puts the whole bank on one flank of the sine, lowering it
evenly, which relieves nothing. Lowering cards also re-opens part 2's counter-move - every setting put
`t1_gap_frac` at 0.38-0.40 over g1's 0.3559 bar, with the drop still over its 0.6421 ceiling.

All three levers of the plan are now spent, and the arithmetic says why: the bar asks 1.26-1.73 m of top-edge
relief from a bank whose every card tops out at 1.75 m, seen from 10.9 m above, where the top edge is a max over
cards overlapping several deep. Only a bank that stands taller - the owner's option (b), against the man rule - can
give it. The ridge is committed (it costs nothing and is a real improvement in kind), the bars and the measure are
untouched, and the open row carries the numbers.

The root of this folder now holds the 18 stills of the ridge at 44 m / floor 0.55 - the committed look - with both
manifests, the sidecars and `probe.json`. `probe.py` with no arguments exits 1 on them: three of its seven rows are
missed, and the row the owner must answer is why.
