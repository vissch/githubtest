# ability-fx-captures-r2-2: the close gas puff, pictures for the owner

"Show me visual results." These are fresh captures of the rebased lane
(`lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g1`, 19 commits on top of the integration branch), shot in
batch mode on 2026-10-27 with `GasBankStills` and `GasBankCards`. Nothing was retuned to make them: they show
the build as it stands.

## The pictures

| File | What it shows |
|---|---|
| `review1/gas-t1.jpg` | the close camera (zoom 30) on the chlorine bank — the complaint's frame |
| `review1/gas-o240.jpg` | the overview (240 m out), the merged-out blanket |
| `review1/gas-t1-plate.jpg`, `review1/gas-o240-plate.jpg` | the same two frames with no gas: the plates the probe subtracts |
| `cards/cards-gaspuff.jpg` | the close puff's card drawn alone |
| `cards/cards-gas.jpg`, `cards/cards-smoke.jpg`, `cards/cards-puff.jpg` | the far gas card, the smoke card, the burst puff card, each alone |
| `cards/cards-plate.jpg` | that pose with no card |

Each `.jpg` has a `.json` sidecar with its camera pose and the frame it was photographed on.

## The build these came from

`Assets/_Project/Presentation/Camera/CombatFx.Banks.cs`: `GasPuffBlock` 1 (one card per cell, so the close cards
touch and the green is ONE blob), `GasPuffStand` 1.2, `GasPuffRim` 0.2 (a card's height is scaled by
`Lerp(rim, 1, thickness)`, so the thin skirt lies low and the thick core stands), `GasPuffFirst..Last` 12-16,
`GasBankSquash` 0.55 for the overview (untouched, so O240 does not move).

Shot with `TW_GAS_WAIT_FRAMES=200`; the plate pass is a second launch into the same directory.
Frames reported by the sidecars: t1 **804** (plate 796), o240 **341** (plate 301). Pose error 0.00 m on all four.

## The numbers (`probe.json`, from `probe.py`)

Diagnostics of this build, not a new bar:

```
green_low_share 0.1659   green_height_frac 0.1880   green_rise_m 5.126   green_area_frac 0.1914
t1_blob_share 0.6366     t1_cloud_count 41          blob_px 176384       green_px 277073
o240_green_area_frac 0.0094   o240_cloud_count 15   o240_cloud_share 0.8608
```

The forest is gone: `t1_blob_share` was **0.0803** on the rejected build (106 separate bushes) and reads **0.6366**
here — most of the green is in one patch.

Card look report (`cards/`, unbarred): gaspuff relief 0.1004 on 36287 px against the smoke card's 0.1690 on 999 px
(relief ratio 0.59).

## Why no verdict

**The bars are left unjudged on purpose.** Whether `green_low_share` 0.30 or the standing puff gives way is the
owner's open question ("The close gas puff: the 0.30 low-gas floor against one standing puff", 2026-10-22, in
`docs/reference/decisions.md`). This folder shows what the current build looks like; it does not answer that row
and no knob was moved.

`probe.py --calibrate` needs three same-build runs named `run1/run2/run3` for its barred mode; only one capture set
was shot here, so `probe.json` holds this one set's numbers under `review1`.
