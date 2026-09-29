# To every lane: the owner's night look, on `lane/show/night-look` (not landed)

The owner, 2026-09-29: the night look is the palette of their colour edit and the effects vocabulary of their anime edit
(the `decisions.md` row of 2026-09-29, carried by `lane/show/vehicle-weight`). This lane is its first part: lighter distance, warm light pools, wet
mud that sparkles by the fires. Worktree `githubtest-night-look`, based on integration `b406f70`. **Waiting for the
owner's word to land.**

**Every look is behind a knob that draws today's night at 0**, as `fx.deathAbsurd` is. The owner picks the defaults from
the captures.

| Surface | Change |
|---|---|
| Replay format | none: presentation only. |
| Knobs | `look.lift`, `look.liftReach`, `look.liftMist`, `look.liftBank`, `look.pools`, `look.poolReach`, `look.wet` (`tasks.md`: Terrain view, weather, night, biomes). |
| `Atmosphere` | now `partial` (`Atmosphere.NightLook.cs`). Hook lines in `LateUpdate` only: the fog colour, its end, the mist and bank colours go through `Lifted` / `LiftedFogEnd`, which return today's values at 0. New global `_TWWetLook`, cleared in `OnDestroy`. |
| `NightLights` | now `partial` (`NightLights.Pools.cs`): `PushPools()` in `Update`, `ClearPools()` in `OnDestroy`; a `beginCameraRendering` hook sets `_TWPools`, `_TWPoolTint`, `_TWPoolCount` (0 unless `look.pools` > 0). |
| `Toon_URP.shader` | includes `TWLightPools.hlsl`; two added lines behind `_TWPoolCount > 0` / `_TWWetLook > 0`, and the puddle mirror's weight times `1 - 0.55 * _TWWetLook`. |
| Editor | `LookLab` (knobs set from `unity command eval`). |

`lane/show/vehicle-weight` also makes `NightLights` partial (`NightLights.Machines.cs`): whoever lands second keeps both
partial files, a one-word conflict at most. If your lane edits `Toon_URP.shader` round the gloss block or the local
lights, expect a small conflict there.

Delete this note once the lane has landed and every lane has rebased over it.
