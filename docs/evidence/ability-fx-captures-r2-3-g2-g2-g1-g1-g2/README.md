# The chlorine bank, round 3-g2-g2-g1-g1-g2: the side band is ONE site, shot three times

g1 shot the side band at three different patches and called the result one bank. The pictures disagreed:
`side_face_px` read 70304 / 37666 / 114148, a 3.03x spread, on three ground heights (1.47 / 1.21 / 2.77 m) with
different pickets and a bunker in front. The owner's demand was to put that spread bar under 3.03.

The manifests already said the CLOUD was identical on all three seeds: `gas_cells` 66, `gas_field_max` 35.82,
`gas_cards` 88 / 87 / 88. So the 3.03x was the FRAMING, not the gas. This round shoots the side band at **one
site**, `x` 56 / `z` 142, ground 1.4684 m, on every seed and in both passes - and clears the wreck's fire out of
the frame, which `TankRenderer`'s drawer switch could never do (the fire is a `Flamethrower` pool/pyre, drawn from
`CombatFx.Update`, and the side block now empties it every frame).

## The capture: three runs, nine side shots

Three full captures of the same build, with `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 - the only knob that moves the
cloud's phase - packed as this folder (run 1), `run2/` and `run3/`. Every bar below is derived from all nine side
shots and from nothing else.

| | run1 (wait 200) | run2 (wait 215) | run3 (wait 230) |
|---|---|---|---|
| `side_face_px`, three seeds | 68979 / 111292 / 100423 | 60368 / 94968 / 96959 | 66647 / 98566 / 118311 |
| `side_face_spread` | **1.6134** | **1.6061** | **1.7752** |
| `side_outside_frac` (worst seed) | 0.2949 | 0.2863 | 0.2872 |
| `side_fire_px` (worst seed) | 293 | 101 | 96 |
| `gas_cells` on the side rows | 66 / 66 / 66 | 72 / 72 / 72 | 77 / 77 / 77 |

The side rows of `manifest.json` and `manifest-plate.json` share one `x`/`z` and one `sim_tick` per pair
(217 / 2239 / 4261 in run 1), read `drawers_off` true, `gas_cells_before` 0 and `fires_near` 0.

## The trade, said plainly

**The side band's spread bar is now a repeat-noise bar, not a three-patch bar.** One site means the number can
no longer be moved by a patch's ground height or by what stands in front of it, so what is left in it is the
cloud's own drift between phases: 1.61-1.78x over nine shots. That is the honest reading of what the bar now
guards. It is NOT evidence that the bank looks the same on three different pieces of ground - **the three-patch
evidence lives in the T1 and O240 bands**, which still shoot three sites and still clear their own bars
(`t1_changed_frac` 0.2225 at the worst of nine, `o240_hue_dist_deg` 120.26 against 108.0).

## The fire

`side_fire_px` is the new reading: the plate-differenced mask's pixels whose hue is fire's (`FIRE_HUE_DEG` 25 +/-
`FIRE_HUE_TOL` 25, saturation at least 0.25). Plate-differencing takes the corridor's static lamps out; the band
cannot reach the gas's yellow-green, which `GAS_HUE_TOL`'s own selftest case shows.

- the previous capture (g1): **4864 px**.
- this capture: **195 / 293 / 193**, **43 / 66 / 101**, **51 / 96 / 52** - worst 293 px of 921600, 0.03 %.

So the fire is **down by 94 % but not gone**. The bar is the worst plus 100 = **393**, which holds it where this
build leaves it and fails a round that lets a pyre back in. It does not claim a clean frame.

## The bars

| Bar | g1 | now | How it is derived |
|---|---|---|---|
| `side_face_spread` | <= 3.55 | **<= 1.83** | worst run spread 1.7752 + 0.05, rounded up. Under the owner's 3.03 |
| `side_fire_px` | - | **<= 393** | worst of nine 293 + 100 |
| `side_outside_frac` | <= 0.40 | **<= 0.31** | worst of nine 0.2949 + 0.01, rounded up. A ceiling only tightens |
| `side_face_px` | >= 30000 | **>= 48000** | `floor(0.8 * min(nine) / 1000) * 1000`, min 60368 |
| `t1_changed_frac`, `texture_var_drop`, `o240_hue_dist_deg` | 0.08 / 0.40 / 108.0 | unchanged | no drawing changed; the nine shots clear them with room, and raising them would police noise |

No bar was widened. Every one of them **bites the previous capture**: g1's stills fail this probe on
`side_face_px` (37663), `side_outside_frac` (0.3812), `side_face_spread` (3.0308) and `side_fire_px` (4864) -
a bar that passes both captures is no bar.

## Reproducing

```
python probe.py                    # this folder (run 1)   -> OK
python probe.py --dir run2         # -> OK
python probe.py --dir run3         # -> OK
python probe.py --selftest         # hand calculations, including the 2x2 fire case (answer: 1 pixel)
python probe.py --dir ../ability-fx-captures-r2-3-g2-g2-g1-g1 --out /dev/null   # -> exit 1, as it must
```

The stills come from `GasBankStills` (EditMode), six launches: gas and plate passes at each of the three waits,
with `TW_STILLS_DIR`, `TW_GAS_WAIT_FRAMES` and `TW_GAS_PLATE`.
