---
paths:
  - "trench-warfare-3d/Tools/*split*.py"
  - "trench-warfare-3d/Tools/battle*.py"
  - "trench-warfare-3d/Tools/*rig*.py"
  - "trench-warfare-3d/Tools/fbxscan.py"
  - "trench-warfare-3d/Assets/_Project/Resources/Vehicles/**"
  - "trench-warfare-3d/Assets/_Project/Editor/TankImport.cs"
---

# Model tools and battle FBXs: traps (checked 2026-10-14)

- **Do not round-trip a battle FBX through Blender's scene to change its meshes.** Import puts the axis turn on
  the root and export adds a node turn: nested parts come back moved (a hull by 4 m). Parse the file with
  Blender's `io_scene_fbx.parse_fbx`, replace only the Geometry arrays, write with `encode_bin`. Prove the writer
  first: a parse-and-write copy must be byte-identical. In `encode_bin`, type `C` is a char and `B` a bool; mixed
  up, Blender reads the file and Unity says "File is corrupted".
- **Blender reading a file back says nothing about Unity.** Let Unity import it and read the Editor log.
- **Export with `bake_space_transform` maps Blender -Y to Unity -Z** whatever the axis settings say. Check facing
  by where the barrel's vertices sit, not by bounds.
- **Every part is parented to the Body.** `TankModel` walks the tree from the root part; a part under the FBX's
  own empty is never drawn, without an error.
- **Two exports of the same model differ in bytes.** Compare by shape (counts, bounds, distinct per-corner rows).
- A colour Mask or Tread UV set that `TankImport` reads goes stale when only UV0 is rewritten: carry every layer.
