# flame-jet-tip-smoke: measurements

Harsh critic r1 (41/100), fix 2 on flame-jet-core (owner, 2026-10-19): "no dark smoke past the jet's tip (jet-t2
before/after: orange cards only) and no probe key for it; also the black outline strokes round every card make the
jet read as a texture."

## Captures
Both stills: GreyboxCorridor (night look), `CaptureRig.Shot` at focus (100, 104), zoom 16 (T2), yaw 0, pitch 25,
RNG seeded `UnityEngine.Random.InitState(20250925)`, camera framed screen-right of the jet's nozzle at (100, *, 104)
so the stream's aim is screen-right and its tip is near the frame's centre.

- `plate-t2.png` / `.json`: the pose with no jet ever fired this session. `pose_error_m = 0`.
- `jet-t2.png` / `.json`: the identical pose, 1.0 s after `Flamethrower.Active.Burst(...)` with the same seed, so the
  stream and its tip smoke have had several `NextLight` beats (`LightEvery = 0.09f`) to lay cards. `pose_error_m = 0`.

## probe.py
`tip_x` is the rightmost column of the jet still's fire mask (flamecheck's own rule: the same mask both tools call
fire). The 200 px window right of it is compared pixel-for-pixel against the plate; a pixel counts if the jet still
is at least 15/255 (Rec.601) darker there than the empty plate.

```
stream tip at x=1098   window x 1099..1299 (200 px wide)
smoke_px_past_tip = 30221   (want >= 3000)
```
10x the bar: the tip-smoke plume is comfortably readable at T2, not a marginal pass.

## Before the fix
Before this change the stream laid one `Book.Smoke` card at `far`, 0.15 alpha, tinted by `CombatFx.ApplyTints`
every scene (so it can never be soot) - against this same plate it reads as orange card bleed, not dark smoke,
which is the critic's complaint. `TipSmokeCards = 3` `MineSmoke` cards at 0.62 alpha replace it (never add: the
budget rule).

## The ink ring
`Flipbook_URP.shader`'s `_Contour` darkened a ring round every fire card's silhouette by default
`(0.10, 0.26, 0.85)`, because no C# code ever set `_InkColor`/`_Contour` and the shader's own default drew it on
every book, including `Jet`, `Core` and `Head`. Those three now build their material with `_Contour.z = 0`
(`Sheet.NoInk`), so the stream's own cards carry no outline; the stills above show the stream's silhouette without
the dark stroke that made it read as a flat painted texture.
