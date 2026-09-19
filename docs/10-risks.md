# Risks and mitigations

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

| Risk | Mitigation |
|---|---|
| Burst float non-determinism across CPU architectures | Phase 0 gate; `SimMath` swap to fixed-point; lockstep matchmaking by architecture as fallback |
| Flow field cost with many goal groups | Cap goal groups (≤ 8 per team), time-slice, recompute only on cost change |
| VAT memory for many archetypes × factions | Share skeleton/animation set across factions (uniform = material/texture swap), RGBAHalf, atlas per archetype |
| Gas/smoke diffusion cost | 4 m grid (75 × 200 = 15k cells), one Burst job, half precision |
| Scope for 1–2 people | Slice = British vs German only; other factions are data; missions reuse one trench kit and three maps |
| Lockstep stalls on packet loss | Input delay 3 ticks, resend window, stall UI, N3 reconnect |
| Unity 6 / Entities 1.3 breaking changes | Pin exact versions in manifest; Entities used only in presentation |
