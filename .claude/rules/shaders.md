---
paths:
  - "trench-warfare-3d/Assets/_Project/Shaders/**"
---

# Shaders: traps (checked 2026-10-14)

- **Indirect draws index with `GetIndirectInstanceID_Base`**, never `GetIndirectInstanceID`. The plain one is a
  per-draw id that starts at 0 on D3D, so every pool after the first reads the first pool's records (nine debris
  kinds drew as clods; snipers and the fallen read riflemen's data). `Debris_URP.shader` and `VAT_URP.shader` are
  the pattern.
- **A value a shader needs must also be on the MaterialPropertyBlock.** `new Material(m)` drops undeclared
  properties and a shader reload in Play resets material values: without `_PosMin` / `_PosSize` on the block, men
  draw as dots.
- **Judge a change to a shared look in the battle, not on a bench.** Shoot `GreyboxCorridor` off and on with
  `CaptureRig.Hold` then `CaptureRig.Shot` and compare `contrast_median`; the same build shot twice differs by
  about 0.016. A unit-shader lift raised the playground's number and lowered the battle's.
- The float4 house chunk mask in `Toon_URP.shader` is a seam (`CLAUDE.md`): its own commit, first.
