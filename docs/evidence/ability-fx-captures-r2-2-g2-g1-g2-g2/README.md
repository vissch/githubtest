# ability-fx-captures-r2-2-g2-g1-g2-g2: paint by direction, not hue, 2026-10-21

**`python probe.py` clears, exit 0.** The build is unchanged from g2 (`CombatFx.Banks.cs`, commit `5c0c33620a`,
wide overlapping cards at low alpha). What changed here is only the measure: the mask that decides "is this pixel
gas paint" now reads the DIRECTION a pixel's colour lifts in, not its hue.

## The owner's complaint

> `gas_hue_window_px` is 1, 3 and 0 while `mask_px_raw` is 46702, 50755 and 47337. `paint_frac`'s bar of 0.022 sits
> 0.0047 under run1's 0.0267 and the spread is 0.0041. gas-t1 is still a sheet across the trench, not a puff.

## Why hue cannot see the gas

On the isolation still (`../ability-fx-captures-r2-2-g2-g1/cards/cards-gas.jpg` against `cards-plate.jpg`, the
only thing that differs from the plate), the gas card paints 35,016 px. Its **composite hue median is 156 PIL**
(≈220°, blue) and its green lean (`dG - max(dR,dB)`) runs p10/p50/p90 = **-18 / -8 / +4** — it does not even lean
green on average. At night the gas composites to the night field *brightened*, not to green: the hue of the pixel
is the night's, and even the lift itself does not point towards green. This is why `gas_hue_window_px` (a 45-95
PIL hue window) read 1 / 3 / 0 on this build's three runs while 46,702-50,755 pixels actually differ from the
plate: the window is reading the wrong thing, not failing to find enough gas.

## The signature

`gas_signature(cards_dir)` (`python probe.py --signature`) takes `cards-gas.jpg`'s own paint (plate diff >= 20 RGB
and Rec.709 luma lift >= 2, the same 35,016 px) and averages each pixel's own lift vector `(dR, dG, dB)`,
normalised, into one direction:

```
GAS_DIR = (0.4874, 0.5865, 0.6469)
GAS_COS = 0.9455   (the cosine that keeps 80% of the 35,016 px inside the cone)
```

These are hard-coded in `probe.py`, never read from the cards at bar time (a look change to the cards must not
silently redefine what counts as gas). Even though the *composite* colour is the night's, the card's own lift is
coherent enough in direction — the night pixels it composites over vary, but the gas's own tint pushes every one
of them roughly the same way in RGB space.

## Specificity: does the cone catch anything else?

`--signature` also measures what share of `cards-smoke.jpg`'s and `cards-puff.jpg`'s own paint falls inside the
same cone:

| card | paint px | cone share |
|---|---|---|
| `cards-gas` | 35,016 | 0.8000 (by construction) |
| `cards-smoke` | 41,007 | 0.7139 |
| `cards-puff` | 24,539 | 0.7425 |

The cone is **not gas-exclusive** — smoke and puff score nearly as high. Direction coherence still cuts a real
kind of noise (infantry motion and JPEG block noise lift in scattered, inconsistent directions pixel to pixel;
all three translucent cards push broadly the same way), but this is not a colour test that can tell "gas" from
"any translucent lit card at night" — the bank has no colour of its own once the night field is this dark. That
is the honest answer to the owner's hue numbers: there is no hue, and no small set of RGB directions, unique to
gas at night; only patch coherence (below) still separates the bank from a zebra of separate cards.

## The mask

Two masks, computed from the same plate-difference-and-brighter pixels, used for different things:

- **The metre mask (`bank_mask`), unchanged from g1/g2**: plate difference >= `T1_DIFF_BAR` (20 RGB), kept only
  inside the single largest coherent patch, and brighter than the plate. Still backs `bank_low_share`,
  `bank_height_frac`, `bank_rise_m` — comparable across g1/g2/g2-g2.
- **The gas-direction count (`gas_paint_stats`), new**: of the SAME whole-frame plate-difference-and-brighter
  paint (no patch restriction), count only the pixels whose own lift vector sits within `GAS_COS` of `GAS_DIR`.
  `gas_paint_px`/`gas_paint_frac` are this count. Per run: 13,477 / 13,884 / 14,137 px of 13,477-14,137 — far fewer
  than the 46,702-50,755 raw plate-difference pixels, and unlike the hue window, not near zero.

**A per-pixel cone test on the PATCH itself was tried first and failed.** Filtering the single coherent bank
patch pixel-by-pixel by `GAS_COS` (the natural reading of "recompute `bank_paint_share`/`bank_patch_count` on this
mask") fragmented it: a translucent card has its own interior shading, so plenty of pixels inside the one true
patch lift in directions outside the cone even though the composite still reads as gas (the same fact `bank_mask`'s
own comment records for the luma lift). At `BLOB_DILATE` 1 this collapsed `bank_paint_share` to 0.0125-0.0157 —
indistinguishable from the zebra's 0.0157 — and no dilation setting tried (2 through 20) recovered a bar that both
cleared this build's spread AND left the zebra failing by more: either the fragments stayed too small, or enough
dilation to bridge them also bridged the zebra's separate cards into one blob. **Decision, this leg**:
`bank_paint_share`/`bank_patch_count` stay computed on the untouched plate-difference-and-brighter patch
(g2's own measure, which already separates this build from the zebra on patch coherence alone — see "The bars"),
and the new gas-direction count is reported as the separate `gas_paint_px`/`gas_paint_frac` pair, a floor under
the build, not a patch-shape test.

`composite_hue_median` and `gas_lift_dir` are printed per run, unbarred: `composite_hue_median` 152-153 PIL (still
blue-leaning, matching the isolation still's 156) and `gas_lift_dir` ≈ (22, 22, 15) — this is the row that explains
the owner's 1/3/0, in place of the dropped `gas_hue_window_px`.

## The bars

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.07 | 0.4371 / 0.2584 / 0.2853 | 0.1787 | 0.1884 | unchanged from g2 |
| `bank_height_frac` | `le` | 0.27 | 0.0799 / 0.1711 / 0.1608 | 0.0912 | 0.0989 | unchanged from g2 |
| `o240_area_frac` | `ge` | 0.010 | 0.0130 / 0.0150 / 0.0153 | 0.0023 | 0.0030 | unchanged from g2 |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |
| `gas_paint_frac` | `ge` | 0.0130 | 0.0146 / 0.0151 / 0.0153 | 0.0007 | 0.0016 | 2.3x the spread (owner asked for 2x after `paint_frac`'s 1.1x) |
| `bank_paint_share` | `ge` | 0.12 | 0.1855 / 0.1622 / 0.1936 | 0.0314 | 0.0422 | unchanged from g2; zebra 0.0782 fails by 0.0378 |
| `bank_patch_count` | `le` | 25 | 10 / 13 / 7 | 6 | 12 | unchanged from g2; zebra 40 fails by 15 |

`gas_paint_frac`'s bar is set so its margin (0.0016) clears **2.3x** its spread (0.0007), directly answering "the
bar sits 0.0047 under run1 while the spread is 0.0041" (1.1x) — the owner's complaint about g2's `paint_frac`.
Like g2's `paint_frac`, it is a floor under this build only: the zebra paints *more* of the frame in direction
terms too (`--against` reads 0.0178 on the zebra vs 0.0146-0.0153 here), the same "fainter than the zebra" fact
g2 already reported, now read through direction instead of plain difference.

## No reshoot

`git diff --stat d1480a099b HEAD -- trench-warfare-3d/Assets` is empty: the build has not moved since g2's three
captures were shot, so `run1`/`run2`/`run3` here are copied byte-for-byte from
`../ability-fx-captures-r2-2-g2-g1-g2/`, still t1 `sim_tick` 557 / 597 / 637, still twelve distinct hashes. No
`cards/` here either — the isolation stills did not change, they stay in
`../ability-fx-captures-r2-2-g2-g1/cards/`, and the look report is skipped (`no cards/`).

## The sheet, as a number

`bank_width_m` (the biggest metre-mask patch's width, kept only where it also lifts towards `GAS_DIR`, same
ground-projection maths as `bank_rise_m`) and `bank_aspect` (`bank_width_m / bank_rise_m`), unbarred — a baseline
for whatever build next turns the sheet into a puff:

| run | `bank_width_m` | `bank_rise_m` | `bank_aspect` |
|---|---|---|---|
| run1 (tick 557) | 9.071 | 1.845 | 4.918 |
| run2 (tick 597) | 9.223 | 3.443 | 2.679 |
| run3 (tick 637) | 9.351 | 3.094 | 3.022 |

The width barely moves (9.07-9.35 m) while the rise triples as the gas ages (1.8-3.4 m): the bank gets taller but
not narrower, which is "a sheet across the trench, not a puff" as a number. A puff should show `bank_width_m`
shrinking towards `bank_rise_m` (aspect nearer 1) as it billows up instead of spreading along the trench line.

## The knob

The offset knob is `TW_GAS_EXTRA_FRAMES` (env var) or SessionState key `tw.gasbank.extra`
(`GasBankStills.cs`), 0/30/60 across the three runs — unchanged since g2. g2's own README calls it
`TW_GAS_OFFSET_FRAMES`, which does not exist in the code; that is g2's README's mistake, not fixed here (out of
scope: this lane does not touch the landed g2 README).

## Out of scope

Turning the sheet into a puff is a change to `CombatFx.Banks.cs` (the card layout/spread), which this lane does
not make — `bank_width_m`/`bank_aspect` above are the baseline a future build is measured against, not a fix.
