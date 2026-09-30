# To every lane: the owner's night look, on `lane/show/night-look` (not landed)

The owner, 2026-09-29: the night look is the palette of their colour edit and the effects vocabulary of their anime edit
(the `decisions.md` row of 2026-09-29, carried by `lane/show/vehicle-weight`). This lane is its first part: lighter distance, warm light pools, wet
mud that sparkles by the fires. Worktree `githubtest-night-look`, based on integration `b406f70`. **Waiting for the
owner's word to land.**

**Every look is behind a knob that draws the old night at 0**, as `fx.deathAbsurd` is. **The owner set all three on
(2026-09-29, `decisions.md`).** A capture or test that wants the old night sets `look.lift`, `look.pools` and `look.wet` to 0.

| Surface | Change |
|---|---|
| Replay format | none: presentation only. |
| Knobs | `look.lift`, `look.liftReach`, `look.liftMist`, `look.liftBank`, `look.pools`, `look.poolReach`, `look.firePools`, `look.poolsThroughHaze`, `look.hazeLight`, `look.inkFade`, `look.grade`, `look.gradeSat`, `look.gradeDark`, `look.puddleSky`, `look.waterDim`, `look.poolSoft`, `look.poolShoulder`, `look.horizonFires`, `look.warmFlare`, `look.rainCurtain`, `look.moreFires`, `look.glowHue`, `look.glowFade`, `look.propRim`, `look.poolUnblue`, `look.poolAmber`, `look.poolVary`, `look.moonSheen`, `look.wet` (`tasks.md`: Terrain view, weather, night, biomes). |
| `Atmosphere` | now `partial` (`Atmosphere.NightLook.cs`). Hook lines in `LateUpdate` only: the fog colour, its end, the mist and bank colours go through `Lifted` / `LiftedFogEnd`, which return today's values at 0. New global `_TWWetLook`, cleared in `OnDestroy`. The fog's distances moved into `FogDistances(camera)`: called from `LateUpdate` as before, and while lifted also as the main camera begins to render, so `CaptureRig` stills are fogged for the view they frame (they were fogged for the gameplay view). |
| Grade | `Atmosphere.BuildGrade` keeps its `ColorAdjustments` and `ShadowsMidtonesHighlights`; `GradeLook()` (LateUpdate) re-tints the shadows toward umber at night when a knob moves. |
| `NightLights` | now `partial` (`NightLights.Pools.cs`): `PushPools()` in `Update`, `ClearPools()` in `OnDestroy`; a `beginCameraRendering` hook sets `_TWPools`, `_TWPoolTint`, `_TWPoolCount` (0 unless `look.pools` > 0). |
| `SceneHooks` | new `FirePool(at, colour, strength, reach, life)` (0 life: this frame only), set by `NightLights`, cleared in `Reset`. `TankRenderer.Burning` calls it once a frame per burning hull or wreck (one line); `NightLights`' `FireLight` hook passes a flamethrower's fires on to it. |
| `VAT_URP.shader`, `Tank_URP.shader` | include `TWLightPools.hlsl`; one block each behind `_TWPoolCount > 0`: the pools, and a warm rim on the edge turned to a fire (`TWPoolsOnFigure`). Wreck lanes editing `Tank_URP` round the lamp lines: expect a small conflict. |
| `InkLines_URP.shader` | the ink fades with the linear distance fog too (`_TWInkFogFade`, `look.inkFade`), one line. |
| `Water_URP.shader` | one line: the lit body times `1 - _TWWaterDim` (a global, 0 by day; `look.waterDim` at night). |
| `RainCurtain_URP.shader` | while lifted, `_TWCurtain` sets the curtains' colour (1.15 x the haze, was 1.9) and alpha (`look.rainCurtain`). |
| `Glow_URP.shader` | `_TWGlowHue` (`look.glowHue`): the core keeps its hue as the haze thickens; the haze is passed to the fragment. |
| `Toon_URP.shader` | includes `TWLightPools.hlsl`; two added lines behind `_TWPoolCount > 0` / `_TWWetLook > 0`, and the puddle mirror's weight times `1 - 0.55 * _TWWetLook`. |
| Editor | `LookLab` (knobs set from `unity command eval`). |
| UI | `ComicWords` (rare comic sound words on the HUD's markers layer, built beside `DeathMarks` in `SelectionController`; `fx.comicWords`, `fx.comicGap`); `.hud-comic` in `BattleHud.uss`. |

`lane/show/vehicle-weight` also makes `NightLights` partial (`NightLights.Machines.cs`): whoever lands second keeps both
partial files, a one-word conflict at most. If your lane edits `Toon_URP.shader` round the gloss block or the local
lights, expect a small conflict there.

Delete this note once the lane has landed and every lane has rebased over it.
