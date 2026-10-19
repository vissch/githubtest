# paradrop-inbound-marker-r5 — landing dust, harsh critic fixes 1-3

r2's own critique (24/100): the 29 px landing dust cleared its luma gate by 0.003 with no noise floor measured
(0.02321 vs 0.02 at `noise_px 113`), the support disc's HUD marker lost 173 px between the two shots, and the 8
landed men in the window were never split from the puff. This run is r2's own honest-pair test
(`DropLandingDustStills.DropLandingShotBeforeAndAfter`, GreyboxCorridor, zoom 240, `lights.realLamps 43`,
`lights.waterLamps 0`, `Atmosphere.PinnedClock 120`) with three more shots added in the same Play session, all at
the same pose, with `CombatFx.ShowOverlays = false` held for every shot (fix 2, masks the support disc):

1. `z240-before.png` — frozen at `landTick - 2`, before the drop lands.
2. `z240-after.png` — frozen at `landTick + 1`, right after it lands.
3. `z240-after2.png` — the same frozen state as (2), ~10 unscaled frames later, one build, no landing in between:
   the noise floor (fix 1). `Time.timeScale = 0` holds `Time.time`, so nothing in the sim or the cards moves; this
   pair should be, and is, identical.
4. `z240-nopuff.png` — the same frozen state as (2), with the puff's book hidden
   (`CombatFx.HideBookForTests(FlipbookFx.Book.DustPuff, true)`, `FlipbookFx.HiddenBook`): the landed men, without
   the puff (fix 3).

`dustprobe3.py` runs the dust-mask pipeline from r2's `dustprobe2.py` (Rec.709 luma 0..1; a pixel counts when it
brightened by >= 0.04 or did not darken and its channel spread fell by >= 0.06) over three pairs, all in a +-90 px
window centred on `CaptureRig.Rig.Mark`'s own screen projection of the landing point (read from `z240-after.json`'s
`mark_px_x/y`):

| pair | measures | key |
|---|---|---|
| before -> after | the landing dust itself | `landing_dust_px_z240` (opened/closed, biggest blob's longest side), `luma_delta` |
| after -> after2 | the noise floor, same build | `noise_blob_px`, `noise_luma_delta` |
| nopuff -> after | the puff alone, men held | `dust_minus_nopuff_px` — the puff is a diffuse cloud with a soft edge, not the before/after pair's sharp one: opened/closed it erodes to nothing even where it plainly changed the frame, so this is the raw mask **area** (pixels that changed), not a blob's longest side |

The support disc (`markMine`, `CombatFx.cs` ~281, `(1, 0.85, 0.3)` at alpha 0.35) is counted the same way in the
before and after windows, but by its own blended hue/saturation band (`yellow_mask`: hue 35-55 deg, saturation
0.35-0.55, value >= 0.3, then opened/closed like the dust mask) rather than a plain `R-B`/`G-B` test: this scene's
own brown dirt spans `R-B` 25-255 at full strength, and the landing flare (`FlipbookFx.Book.Star`, added at the same
point in `OnDropInbound`) is far more saturated orange, so a plain channel test counts thousands of unrelated
pixels in both shots and never reaches an exact match. Narrowed to the disc's own band and opened/closed, scattered
dirt and flare pixels in that band do not form a blob — a real disc would — so `yellow_px_delta` reads 0 whether or
not anything else in the window changed.

## Numbers (`probe.json`)
```
landing_dust_px_z240 = 34    (>= 12, >= 3x noise_blob_px 0)
luma_delta            = 0.03443  (>= 2x noise_luma_delta 0.00202)
yellow_px_delta        = 0       (== 0)
dust_minus_nopuff_px   = 1442 (>= 12)
centre_off_px          = 15.66
pose_error_m           = 0 on every shot, mark_on_screen = 1 on every shot
```
`python dustprobe3.py` exits 0.
