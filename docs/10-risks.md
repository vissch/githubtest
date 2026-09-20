# Risks and mitigations

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

| Risk | Mitigation |
|---|---|
| Burst float non-determinism across CPU architectures | Windows x64 only (decision 2026-09-20); same-build lockstep; no fixed-point fallback exists |
| Flow field cost with many goal groups | Cap goal groups (≤ 8 per team), time-slice, recompute only on cost change |
| VAT memory for many archetypes × factions | Share skeleton/animation set across factions (uniform = material/texture swap), RGBAHalf, atlas per archetype |
| Gas/smoke diffusion cost | 4 m grid (75 × 200 = 15k cells), one Burst job, half precision |
| Scope for 1–2 people | Slice = British vs German only; other factions are data; missions reuse one trench kit and three maps |
| Lockstep stalls on packet loss | Input delay 3 ticks, resend window, stall UI, N3 reconnect |
| Unity 6 package drift | Pin exact versions and commit `packages-lock.json`; Entities dropped (unused, and its pin did not exist on the registry) |
| Art pipeline has no owner | One of the two developers owns C4 with a week budget; one shared humanoid rig across factions; VAT baker targets that rig |
| 3D is not more fun than 2D | M1.5 fun gate at ~week 8 with capsules, before any VAT/terrain/VFX investment |
| Scope (~80 dev-weeks to Mission 3 for two people) | Cuts in [11-plan-review](11-plan-review.md) §4; nothing beyond M1.5 is scheduled until it passes |
