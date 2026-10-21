# The chlorine bank, round 3-g2-g2-g1-g1: the tank is out of the picture, not just out of the mask

g1 took the scenery out of the side MASK with two gates (a hue band and the bank's own column of air) and put the
two passes on the same sim tick. It did not take it out of the PICTURE: `gas-side-s1.jpg` was still a burning tank
wreck filling the right half of the frame, with a green carpet behind it. A reader who opens the evidence sees a
tank; the number says bank. This round shoots the side band with the movable drawers switched off, so the still
and its plate hold no hull, no man and no debris piece at all.

Nothing in `probe.py` changed but one paragraph of its header. The measure, its gates and its four bars are g1's,
unchanged: a capture whose side band holds LESS than g1's has to clear the same bar.

## What changed in the harness (`GasBankStills.cs`)

| Change | Why | What it is worth |
|---|---|---|
| **The movable drawers are off for the side block**, in both passes: `enabled = false` on the scene's `TankRenderer`, `VATRenderer` and `DebrisRenderer` from the block's first frame until the still is written (and a belt at the end of `Shoot`). All three draw from `LateUpdate`, so the component switch is enough and no production file changes. | The hull in `gas-side-s1` was never the bank, and a picture that has to be argued away is not evidence. | The three side stills are the bank and the static field, nothing that moves. `gas-side-s1.jpg` is now a green carpet behind wire pickets - no tank. |
| It is done **unconditionally of `plate`**. | Both passes then skip the same drawing on the same frames, so the plate still subtracts and the tick parity g1 won is kept. | `side_tick_skew` **0 / 0 / 0**, as in g1; `gas_cells_before` on the side rows 0 / 0 / 0. |
| **The row says so**: `drawers_off` (true on the side rows) and `hulls_near`, alive `UnitFlags.Vehicle` units within 80 m of the patch, counted off `Host.Local.World` just before the shot. | The evidence should state that hulls were about and are not in the picture, rather than leave it to be noticed. | `drawers_off` **true / true / true**; `hulls_near` **0 / 0 / 0** - see "What is still not framed the same": the thing in g1's s1 was a WRECK, which is dead and so not counted here. |

### Why not aim the lens away from the tank

Because the hull is dynamic. From a seed's gas call the gas pass's world and the plate pass's world diverge (gas
kills men), so any choice of pose that reacts to the world can land differently in the two passes - and a plate
from a different pose is no control at all. Not drawing a thing is identical in both passes, by construction.

## What the drawers were actually worth

| Side reading | g1 | this round | What it says |
|---|---|---|---|
| `side_mask_px` (ungated) | 189398 / 139794 / 294688 | 186645 / **226173** / 294677 | seed 1's ungated mask grew by 86000 px: the wreck was STANDING IN FRONT of changed ground, and with it gone the plate subtraction sees what was behind it |
| `side_cut_frac` | 0.2656 / **0.4141** / 0.2985 | 0.2705 / **0.3029** / 0.2985 | seed 1's gates had been throwing away 41 % of the mask, most of it the wreck's own pixels. They now throw away as much as the other two seeds |
| `side_blob_px` | 125271 / 67924 / 201617 | 122074 / 67925 / 201610 | unchanged |
| `side_face_px` (the bar) | 70304 / 37666 / 114148 | **68982 / 37663 / 114148** | unchanged to within 2 % |

**The honest reading:** the two gates had already done their job - the wreck was never inside the barred number.
What this round buys is that the picture and the number now agree, and that seed 1's gates no longer have to cut
away two fifths of the mask to get there. It does **not** close the spread: `side_face_px` still runs 37663 to
114148, 3.03x, and it always was the banks' own size (`side_blob_px` 122074 / 67925 / 201610 on three different
patches), not a tank.

## The bars: unchanged

Every seed clears g1's bars, so by the rule "never lower a bar below what this capture supports" and "a bar only
moves up", they stay where they are. The face bar's re-derivation on this capture would give
`floor(0.8 * 37663 / 1000) * 1000` = **30000**, the same number.

| Reading | Seeds (s0 / s1 / s2) | Min | Rule | Bar |
|---|---|---|---|---|
| `t1_changed_frac` | 0.2261 / 0.2249 / 0.2754 | 0.2249 | the owner's, unchanged | **>= 0.08** |
| `texture_var_drop` | 0.5638 / 0.7428 / 0.6094 | 0.5638 | the owner's, unchanged | **>= 0.40** |
| `o240_hue_dist_deg` | 133.92 / 137.67 / 122.85 | 122.85 | g1's, unchanged | **>= 108.0** |
| `side_face_px` | 68982 / 37663 / 114148 | 37663 | `floor(0.8 * min / 1000) * 1000` = 30000 | **>= 30000** |

## Final numbers (`probe.json`, this build)

| Reading | Bar | s0 | s1 | s2 |
|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.2261 | 0.2249 | 0.2754 |
| `texture_var_drop` | >= 0.40 | 0.5638 | 0.7428 | 0.6094 |
| `o240_hue_dist_deg` | >= 108.0 | 133.92 | 137.67 | 122.85 |
| `side_face_px` | >= 30000 | 68982 | 37663 | 114148 |

`python probe.py` exits 0 and prints `OK`; `python probe.py --selftest` exits 0.

### Reported, not barred

| Row | s0 | s1 | s2 |
|---|---|---|---|
| `gas_side_height_m` (median column's silhouette top less its footprint bias) | 1.648 | 2.329 | 2.419 |
| `side_top_m` / `side_bias_m` | 3.427 / 1.779 | 4.237 / 1.908 | 4.364 / 1.945 |
| `gas_t1_height_m` | 3.881 | 6.047 | 5.518 |
| `side_face_frac` / `side_face_col_px` | 0.5651 / 51 | 0.5545 / 97 | 0.5662 / 100 |
| `side_mask_px` / `side_box_px` / `side_cut_frac` | 186645 / 700291 / 0.2705 | 226173 / 700291 / 0.3029 | 294677 / 700291 / 0.2985 |
| `side_tick_skew` | 0 | 0 | 0 |
| `o240_bank_hue_deg` / `o240_grass_hue_deg` | 87.7 / 221.6 | 85.9 / 223.5 | 98.5 / 221.3 |
| `t1_luma_delta` | 0.0958 | 0.1146 | 0.1049 |
| `pose_error_m` | 0.0 on all nine stills of both passes | | |

## What is still not framed the same

1. **Flames stay, and they are visible.** Fire is a `CombatFx` card, not a `TankRenderer` draw, so switching the
   drawers off does not switch it off - and it must not, because the same `CombatFx` draws the gas. There is a
   burning wreck's fire at the bottom right of `gas-side-s1.jpg`. It is outside the barred number: the hue gate
   cuts fire at ~23 deg against the bank's yellow-green, which the selftest checks by hand, and the patch box cuts
   anything outside the bank's own column of air. `side_cut_frac` 0.2705 / 0.3029 / 0.2985 is what the two gates
   removed between them.
2. **`hulls_near` reads 0 on all three seeds**, yet g1's s1 held a tank. The tank was a WRECK: a wreck is a dead
   unit, so `UnitFlags.Alive` is clear and this count does not see it. The count is still worth having (it says
   living armour was not about), but it is not the proof that the hull is gone - the picture and
   `side_mask_px` 139794 -> 226173 are.
3. **The static scenery is still in front of the bank.** `ClearView`'s ladder runs out on this map exactly as it
   did in g1: the log reads `71 open patches, 71 of them kept at view width 0 m (0 = no corridor check, none of
   the widths yielded 3 seeds); seeds 48 m apart`. So wire pickets, a bunker mouth and duckboards stand between
   the side lens and the bank on seed 1. They subtract out (same tick, same pose, and they do not move), but they
   are in the picture.
4. **The three side stills are still three different banks**, 37663 to 114148 face pixels on three patches. One
   framing, three cloud sizes - which is what three seeds are for, and why the bar is set at 20 % under the
   smallest rather than near the middle.

## What is here

| File | What it is |
|---|---|
| `probe.py` | the measure, g1's but for one header paragraph. `--selftest` checks the camera maths, the texture/hue readings, `footprint_bias_m`, the face count, `project`, `gas_hue_gate` and `patch_box` against hand calculations; no arguments measures the stills beside it, and refuses a side still more than `SIDE_SKEW_TICKS` from its plate |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls, and on the side rows `drawers_off` and `hulls_near` |
