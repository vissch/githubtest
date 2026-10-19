# paradrop-inbound-marker-r2: an honest landing-dust measurement

r1's dust probe measured a camp lamp going dark (151,87,33 -> 34,34,42) ~570 px from the drop, in a
hand-found window: `NightLights.RealLamps` rotates the water lamps' real light every frame, so one of r1's two
"identical" renders had a different lamp lit. This fix re-shoots a held-clock zoom-240 pair either side of the
landing tick, with every lamp pinned real and the weather clock pinned (never released), and measures only the
window around CaptureRig's own screen projection of the landing point.

## How the pair was shot

`Assets/_Project/Tests/Stills/DropLandingDustStills.cs` (`[Explicit]`, run by name, with a graphics device):

```
Unity.exe -batchmode -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
          -testFilter TW.Tests.DropLandingDustStills \
          -testResults <out.xml> -logFile <out.log>
```

`TW_DROPDUST_DIR` picks where the stills go (default `docs/evidence/paradrop-inbound-marker-r2` at the repo
root, resolved from `Application.dataPath`).

In order:
1. Opens `GreyboxCorridor`, seats every `SimHost.FactionA` to Brass (ParaDrop is Brass's alone,
   `FactionRoster.MayCall`), enters Play.
2. Pins `Knobs["lights.realLamps"] = 43` (every lamp keeps its real light: none rotates out between the two
   shots) and `Knobs["lights.waterLamps"] = 0`, and `Atmosphere.PinnedClock = 120f` — set once and never
   released, so `Atmosphere.RainNow` (a function of the pinned clock alone) is identical in both shots.
3. Finds real droppable ground by scanning with `OffMapAbilitySystem.CanLand(player 0, pos)` itself (not a
   hand-picked point): not water, not a trench, short of the enemy's rear line by `ParaDropKeepOut`.
4. Sets `CaptureRig.Rig.Mark` to that point (`RenderGround.Sample` for its height) and issues a `SupportFire`
   command for `OffMapAbilityId.ParaDrop` there.
5. Listens for the sim's own `SimEventType.DropInbound` event near that point: its `Tick` is the command's real
   processing tick (`InputDelayTicks` later than the tick it was issued on), not a guess from `Time.time`. The
   landing tick is that `+ WarmupTicks` (100, `OffMapAbilities.cs`'s `AbilityStats` for ParaDrop).
6. Drives `Time.timeScale` 1 to let the world tick, 0 to freeze it: polls to `landTick - 2`, freezes, shoots
   `z240-before.png`; polls to `landTick + 1` (so `DropLanded` has been pumped and the puff drawn), freezes,
   shoots `z240-after.png`. Both at zoom 240, yaw 21, pitch 25 — the pose `FarGrowTests.DropLandingDustIsSeenAt240`
   checks the puff is readable at.

Both sidecars' `pose_error_m` are 0, `rain` is equal (0.20069 both), and `mark_on_screen` is 1.

## The probe

`dustprobe2.py` reads `mark_px_x/y` from `z240-after.json`, takes a +-90 px window around it, and for each pixel
computes Rec.709 luma 0..1 (CaptureRig's own scale) and the channel spread (max-min). A pixel counts as dust
only when it got brighter (`luma_after - luma_before >= 0.04`) or it did not darken and its spread fell by 0.06
or more (dust is grey and washes colour out) — a darkening lamp and the newly landed men (also dark, but not
losing colour) both fail this test. 3x3 opening then closing, connected-component labelling,
`landing_dust_px_z240 = max(w, h)` of the biggest component's bounding box, `centre_off_px` = distance from the
centroid of every counted pixel in the window to the mark, `luma_delta` = the mean `after - before` luma over
those pixels.

Measured: `landing_dust_px_z240` 29, `centre_off_px` 17.95, `luma_delta` 0.02321 — passes `px >= 12`,
`centre_off_px <= 30`, `luma_delta >= 0.02`.
