# Painted battlefield environment

The owner's new north star is [battlefield-northstar.jpeg](reference/battlefield-northstar.jpeg).
Judge assets at a 25 degree vertical field of view, 25 degree pitch, zoom 30, yaw 291 degrees.
The reference establishes shape, pigment and density; it does not authorize changes to simulation terrain or cover.

## Art decisions

- Keep grey-brown earth in a narrow value range, pale cool water, warm canvas and timber, dark trench recesses.
- Use actual duckboard panels and inset revetment, irregular earthen lips, and rounded shaded sandbags.
- Anchor each trench with sparse timber supply recesses and broken concrete shelters; crates belong in clusters.
- Mix forked, straight, shattered and fallen tree silhouettes. Leave quiet ground between clusters.
- Preserve painted contours without turning the entire ground into high-contrast camouflage.
- Cross-light the forms. Apply cast shadows after toon quantization so contact shadows survive the light bands.

## Reusable generation contract

`BattlefieldProps.BuildFieldKit` creates metre-scale modules with ground pivots. `DressTrench` places anchors
at proportional map-width positions, rejects water and occupied/link/trench/wire footprints, and reads the heightfield.
These are decorative shelters, not new garrisons, obstacles or cover. Their placement must never write MapData.
The supplied generator uses straight trench rows; a future curved layout needs a presentation path/tangent adapter
instead of copying the current z-min/z-max placement rule.

Variation is stable integer-hash selection rather than Unity random state. Reuse meshes and materials; add silhouette
variants before increasing density. `Combine` preserves source pigment and bakes vertical shading into vertex colors.
Terrain painting uses a presentation-only domain-warped noise mask; simulation noise and terrain remain unchanged.

## Budget and verification

- Prop instances are grouped in 32 m spatial pages (maximum 1023 matrices), with transformed mesh bounds and a
  8 m culling margin per side for nearby shadow casters. No per-prop GameObjects or per-frame list rebuilding.
- Standard-view environment sample: 819 submitted prop instances, 112,749 base mesh vertices, 106 instanced submissions.
  These counters exclude outline/shadow passes, terrain and soldiers; they are not GPU timing claims.
- Ground pigment is 8 texels/metre: approximately 7 MiB with mipmaps for the 90 x 240 m map. The painted skirt uses
  3 texels/metre and fades into haze. Crater repaints refresh mipmaps only when changed.
- New procedural meshes/materials and terrain textures are released when their owner is destroyed.
- Soldier meshes and the renderer's 1.5M vertex limit are unchanged. GTX 1050 timing still needs measurement on
  that hardware; captures here use an RTX 4070 Laptop GPU.

`AgentScripts/VisualCapture.cs` stages a fixed review camera and writes `Temp/VisualReview/standard.png` by rendering
Camera.main. Run `VisualCapture.Stage`, allow a frame for spatial culling, then `VisualCapture.Standard` via
`unity command run_script`. Staging pauses the simulation and disables tactical input for that Play session only.
Stop Play before refreshing assets. Run `python validate.py`, then the complete EditMode/PlayMode suites with
the editor closed before committing.

## Remaining visual limits

This is a procedural art pass, not a reproduction of the illustration. The simulation still supplies straight
trench geometry, while hand-painted wood grain, more asymmetric ruin silhouettes and authored character animation
would be subsequent art work. The shelters do not add gameplay interiors. The HUD remains IMGUI.
