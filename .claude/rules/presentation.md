---
paths:
  - "trench-warfare-3d/Assets/_Project/Presentation/**"
  - "trench-warfare-3d/Assets/_Project/Editor/**"
  - "trench-warfare-3d/Assets/_Project/UI/**"
---

# Presentation and editor code: traps (checked 2026-10-14)

- **`Mathf.SmoothStep(from, to, t)` interpolates; it is not GLSL `smoothstep(edge0, edge1, x)`.** Written as the
  GLSL one it held every fire light at a fifth of its peak for weeks. The edge form is
  `Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(a, b, x))`. When a fade or light looks weaker than its constants
  say, read the live value before retuning.
- **Writing both worlds:** only through `SimCommand` or `SimHost.WriteWorlds(...)`; the Local and Peer worlds can
  be a tick apart between frames, and a write to one logs DESYNC.
- **A green gate does not run `OnGUI` or press a button.** After a SHOW change, play it through its real path: the
  real button, the seat's delay, far models drawn close.
- **Stop Play before a recompile or a build.** A domain reload in Play leaves `SimHost.Local` null and the console
  fills with NullReferenceExceptions: that is the reload, not a bug.
- **Captures:** let the sim step before `CaptureRig.Hold` (held at tick 0, vehicles draw at the origin); a flyer or
  a tall tank falls out of a centred frame, so pull back and check `Camera.main.WorldToViewportPoint` before
  calling an effect missing; black smoke on a dark hull at night cannot be seen.
