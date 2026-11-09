# CelFx: the procedural cel effect kit

The six H3 effects (lasers, bullet impacts, explosion, fire bolts, flamethrower) are built from this kit. No flipbooks:
every shape is a mesh or a field in a shader, banded into flat cel values with a white HDR core and dark ink.

Files: `CelFx.cs` (Play, LOD), `CelFxRecipe.cs` (the data format), `CelFxRenderer.cs` (GPU pools), `CelKind.cs` (the
extension point and the built-in kinds), `CelPalette.cs`, `CelRecord.cs`, `CelDemo.cs` (a worked recipe).
Shaders: `Shaders/Cel/CelFx.hlsl` (shared functions) and `CelStreak_URP`, `CelLobe_URP`, `CelGlow_URP`.
Tests: `Tests/Cel/CelFxTests.cs`. Still: `Editor/Cel/CelFxStill.cs`.

## Play an effect

```csharp
CelFx.Play(recipe, origin, dir, target, seed);   // origin: muzzle/burst, dir: facing, target: the hit
```
It spawns every layer at once (records born later wait on the GPU), so an effect costs CPU only when played, 0 B, and
nothing per frame. The same seed plays the same effect. The renderer makes itself on first use (`CelFx.Renderer`).
Chunks need a debris owner to lend `CelFx.Chunks` (DebrisRenderer.Burst); lights go through `SceneHooks.Flash`.

## Write a recipe

A recipe is a list of `CelLayer`s. Put yours in `Cel/<Effect>/<Effect>Recipe.cs` as a `static readonly CelFxRecipe`
(the tests find every static recipe and validate it). See `CelDemo.cs`.

| field | meaning |
|---|---|
| `Kind` | `"Streak"`, `"Lobe"`, `"Glow"`, an added kind's `Name`, `"Chunks"` (DebrisRenderer) or `"Light"` (NightLights) |
| `Start`, `Duration`, `TimeJitter` | first birth (s after Play), births spread evenly over Duration, +- jitter |
| `Life`, `LifeJitter` | seconds each record lives |
| `Close`, `Mid`, `Far` | counts per LOD tier (must not rise going out) |
| `Anchor`, `Offset` | Origin or Target, plus an offset in the effect frame (x right, y up, z along dir) |
| `Shape`, `ShapeSize` | Point, Sphere (x radius), Shell, Disc (level; x outer, y inner), Box, Line (origin to target) |
| `Aim`, `Speed`, `SpeedJitter`, `Spread`, `UpBias`, `Arrive` | None, Dir, Radial, Target (Speed 0: arrive at Life x Arrive), Up, Back |
| `Gravity`, `Drag`, `Buoyancy`, `Spin` | motion, integrated in the vertex shader |
| `Size` (CelCurve), `SizeJitter`, `Stretch` | size from/to/pow over the life; Stretch = streak trail seconds or lobe squash |
| `Erode` (CelCurve), `Feather` | erosion threshold over the life (0 whole, 1 gone): things vanish in steps, never by alpha |
| `Palette`, `CoolTo`, `CoolFrom` | palette names; a lobe can cool into a second palette (fire to smoke) |
| `Core`, `Opacity`, `Ink` | HDR multiplier of the core band (3-6 blooms), additive opacity, ink width in px at Close |
| `Extra` | the kind's own four numbers (below) |

`Extra` per kind: **Lobe** (lump freq [1.3], lump amp [0.14], light up-bias [0 = eye], cool start), **Glow** (mode 0
ground pool / 1 ground ring / 2 flash facing the eye / 3 ring facing the eye, falloff power or ring half-width, star
points, star sharpness), **Streak** (tail taper 0 pointed..1 square, min length m, flicker 0..1), **Chunks**
(DebrisRenderer.Piece, burn), **Light** (reach m).

Palettes: `CelPalette.Add(name, c0, c1, c2, c3, core, coreAt, bands, ink)` - four colours dark to light as they look
(sRGB hex from the reference frame), the core colour (x the layer's `Core`), the band value above which the core
shows, how many of the four bands to use (2-4, the lightest), the ink colour. Built-ins: white, fire, fire_hot,
smoke_tan, smoke_black, dust, spark, ember, laser_green, laser_blue, glow_warm. Register yours from your recipe
class's static constructor; ids never move.

## Add a primitive kind (Bolt, Beam, Spike burst, Cone, ...)

One shader + one class, no edit to the kit:
1. `Shaders/Cel/Cel<Kind>_URP.shader`, named `TW/Cel/<Kind>`. Include URP `Core.hlsl` then `Assets/_Project/Shaders/Cel/CelFx.hlsl`;
   read the record with `CelFetch(svInstanceID)` (it uses `GetIndirectInstanceID_Base`), collapse dead records with
   `CelAlive`, use `CelPosition/CelVelocity/CelCurve`, colour with `CelRamp`, ink with `CelInk`, cut with `CelErode`.
   Document your `Size.w` and `Extra` in its header.
2. A class anywhere in `Presentation/VFX/Cel/<Effect>/`:
   ```csharp
   public sealed class BoltKind : CelKind
   {
       public override string Name => "Bolt";
       public override Shader FindShader() => Shader.Find("TW/Cel/Bolt");   // write the literal (build check reads it)
       public override Mesh BuildMesh() => CelMeshes.Strip(8);              // or Quad01, Icosphere, your own
       public override int Capacity => 256;
   }
   ```
   The renderer finds every `CelKind` in the TW assemblies when it starts; built-ins keep ids 0-2, the rest follow in
   name order. Recipes use it by `Kind = "Bolt"`.
3. A keeper material `Resources/ShaderKeep/KeepCel<Kind>.mat` for your shader (copy `KeepCelStreak.mat`, put your
   shader's GUID in `m_Shader`), or a player build has no shader. `CelFxTests.Every_Cel_Shader_Is_Found_And_Kept_For_A_Build` fails until you do.
Kit changes you need (a new shared function, a record field) go to the kit owner via `stage/kit-requests.md`.

## LOD rules

- The tier is the camera zoom (`ViewEye.Zoom`; bands T3 7.5, T2 16, T1 30, O120): **Close** to 16, **Mid** at 30,
  **Far** from 120, linear between (`CelFx.LodBlend`: 0, 1, 2). A layer's count is lerped between its tiers, then
  scaled by `FxQuality` (Low 0.5, Medium 0.75, High 1, Epic 1.25).
- **Close** = full detail; **Mid** = fewer pieces, no secondary arcs; **Far** = one or two primitives (a flash Glow, a
  line Streak, a pool Glow). Counts and detail never rise going out (`Close >= Mid >= Far`; tested).
- Ink thins going out (`_CelLod.y`, x1 Close, x0.75 Mid, x0.5 Far); use `CelLodInk(px)` in a shader.
- `CelFx.ZoomOverride` pins the zoom for a capture; `CelFx.Clock` pins the clock.

## Budget rules

- One indirect draw per **kind** with a live record, through `FrameBudget.DrawIndirect`, however many effects or
  records: an effect whose kinds are already drawn this frame adds no draws. Lobe draws two passes (body + ink hull).
- Pools are rings with caps (Streak 2048, Lobe 768, Glow 512, x FxQuality chunks; knob `cel.capacityScale`): past
  the cap the oldest record is overwritten. 0 B allocated per spawn, per play and per frame (tested).
- Lobes are opaque cutout (no overdraw: use them for smoke walls); Streak and Glow are additive.
- Keep the frame under 300 draws and 13 ms GPU (the barrage is already at 374, so replace draws, do not add kinds
  lightly: prefer a mode in `Extra` over a new kind when the mesh is the same).

## Render a still

`Unity.exe -batchmode -projectPath <wt>/trench-warfare-3d -executeMethod TW.Editor.CelFxStill.Batch -celOut <dir>`
(with graphics, never `-nographics`) renders the demo at 8 moments into `cel_sheet.png`, `cel_peak.png` and
`cel_still.json` (draws per moment). For your recipe, call `CelFxStill.Sheet(recipe, dir, zoom)` from your own
executeMethod. Chunks and lights need Play and are not in the still.
