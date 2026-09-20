# Handoff: Trench Warfare 1917 → 3D

Branch `claude/trench-warfare-2d-3d-plan-idt7lf` holds the roadmap, its 2026-09-20 review, and a Unity 6000.0.50f1
project with Phase 0 implemented and a true, tested baseline (P0.5).

## 1. Where things are
| Path | Content |
|---|---|
| `docs/11-plan-review.md` | **Plan of record.** Owner decisions, what was wrong with the first skeleton, re-cut milestones, Unity skill map, open follow-ups |
| `docs/PLAN.md`, `docs/00-…10-*.md` | The original plan; catalogs (units, abilities, missions) still authoritative, architecture/milestones partly superseded by 11 |
| `trench-warfare-3d/` | Unity project: 18 assembly definitions, Phase 0 code, stubs for every later phase, tests, generated `ProjectSettings/` and `.meta` files |
| `trench-warfare-3d/validate.py` | Offline checks (JSON, assembly graph, sim purity, phase headers). Not a compile check |
| `.github/workflows/unity.yml` | CI from `unity ci init`; needs the secrets named in its header before it goes green |
| `github-test1/` | Unrelated older test project. Do not touch |

## 2. State of the work
- **M1 is done.** A1 is implemented: `FlowFieldManager` (goals, lazy time-sliced fields, tracked mode), garrison stop,
  trench orders (`TrenchOrdersSystem`, initial), stance/terrain speed, `VehicleKinematicsSystem`. The M1 acceptance run
  (`M1_TwoThousandUnits_AdvanceAcrossCorridor_StayInSync`) holds 2,000 units hash-identical over lossy loopback for
  5,000 ticks at 0.31 ms/tick. In the greybox scene: 1-5 deploy, Space `>>`, Backspace `↩`, L lock, F3 cycles the
  flow-field view; `SimHost.StressUnits` deploys N per side and sends both over the top.
- The project compiles under 6000.0.50f1 and `unity test` passes EditMode and PlayMode. Entities / Entities Graphics
  were removed; the replay format is v2 (hash content changed again at M1, see docs/02 change log).
- Decisions that shape everything after this: Windows x64 only; online multiplayer deferred until after Mission 3;
  two developers, one can do art; unit density (300 vs 2,000) decided at the **M1.5 fun gate**.
- Unity tooling on the workstation: Unity CLI 1.0.0-beta, the `unity@unity-agent-plugin` Claude Code plugin, and the
  Unity MCP server registered for Claude Code (`unity mcp configure claude-code`).

## 3. Next steps in order (docs/11 §5)
1. ~~M1~~ done.
2. **M1.5 fun gate**: A2 core (target acquisition, direct fire, near-miss suppression, fire-step assignment along the
   garrisoned trench, hold-fire), A3 core (sector control so a `>>` into the enemy trench captures it and flips
   ownership; deploy queue / logistics can wait), one HE barrage with crater stamp, one gas cloud — capsules and
   IMGUI only. Playtest with both developers. Nothing below is scheduled until it passes.
3. M2 look (B3 sized by the gate, B6, B2, C2 infantry) → M3 Mission 1 → M4 Mission 2 → M5 Mission 3 + perf → M6
   online (if required) → M7 meta.

Every stub carries a `// Phase: <id>` header. Never change the three contracts in `docs/02-contracts.md` without
bumping `ReplayRecorder.FormatVersion` and logging it in that file's change log.

## 4. Before every commit
```powershell
cd trench-warfare-3d
python validate.py
unity test . --mode EditMode --timeout 600
unity test . --mode PlayMode --timeout 600 -- -nographics
```
Commit `.meta` files with their assets and `Packages/packages-lock.json` with `manifest.json`. Push to the same branch.

## 5. Prompt for the next agent
> Continue the Trench Warfare 3D project on branch `claude/trench-warfare-2d-3d-plan-idt7lf`. Read `HANDOFF.md`,
> then `docs/11-plan-review.md`, then `docs/01-phases-and-dependencies.md` for the systems catalog. Work M1 then the
> M1.5 fun gate as listed in HANDOFF §3, keeping the determinism rules in `docs/03` and the frozen contracts in
> `docs/02`. Use the `unity-cli` skill for tests and editor control; run `validate.py` and `unity test` before each
> commit and push to the same branch.
