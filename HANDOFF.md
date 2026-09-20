# Handoff: Trench Warfare 1917 → 3D

This branch (`claude/trench-warfare-2d-3d-plan-idt7lf`) holds the approved roadmap and a Unity 6 project skeleton with
Phase 0 implemented. It was produced in a sandbox that could not push to GitHub. Everything below is what the next agent
needs to continue.

## 1. Where things are
| Path | Content |
|---|---|
| `docs/PLAN.md` | The approved plan as a single document (architecture, contracts, all units, abilities, three missions, phase graph, schedules) |
| `docs/00-…10-*.md` | The same plan split into working documents; `docs/01` is the phase and dependency map |
| `trench-warfare-3d/` | Unity 6000.0 LTS project: 18 assembly definitions, Phase 0 code, stubs for every later phase, tests |
| `trench-warfare-3d/validate.py` | Offline validation (JSON, assembly graph, sim purity, phase headers). Run it before every commit |
| `github-test1/` | Unrelated older test project. Do not touch |

## 2. State of the work
- Commit `ec43c27` + this handoff commit contain everything. No uncommitted changes.
- The C# has **not** been compiled: the sandbox had no Unity. First task for the next agent with Unity access: open the
  project, fix any compile errors, run `TW → Build Bootstrap Scenes`, then run EditMode and PlayMode tests.
- Package versions in `Packages/manifest.json` are known-good pins; let the Package Manager resolve newer patches if it asks.
- The Entities and Entities Graphics packages are installed but no assembly references them yet (B3 adds them).

## 3. Next steps in order (from docs/01)
1. Compile and run the tests (see above). Fix before anything else.
2. Milestone M1: finish A1 (goal-group `FlowFieldManager`, trench garrison stop, vehicle kinematics) and C1 map tool greybox.
3. Then A2 (fire, suppression, cover), A3 (units, orders, economy, sector control), B3 (VAT), B6 (UI) → M2.
4. Then A4/A5/A6 + B2/B5 + C3 → M3 (Mission 1 playable).
Every stub carries a `// Phase: <id>` header naming its phase and contract dependencies. Never change the three contracts in
`docs/02-contracts.md` without bumping the replay format version.

## 4. Getting the code into GitHub
The sandbox received HTTP 403 from GitHub: Claude had no write access to `vissch/githubtest` for the organization.
Either of these fixes it before the next agent starts:
- An org admin installs the Claude GitHub App on the repository: https://github.com/apps/claude/installations/select_target
- Or reconnect GitHub in claude.ai settings: https://claude.ai/customize/connectors?auth_start=github&auth_start_force=1

Then, with the handoff files:
```bash
# Option A: git bundle (exact commits, keeps history)
git clone https://github.com/vissch/githubtest
cd githubtest
git fetch ../trench-warfare-handoff.bundle claude/trench-warfare-2d-3d-plan-idt7lf:claude/trench-warfare-2d-3d-plan-idt7lf
git checkout claude/trench-warfare-2d-3d-plan-idt7lf
git push -u origin claude/trench-warfare-2d-3d-plan-idt7lf

# Option B: tarball (no git history needed)
git clone https://github.com/vissch/githubtest && cd githubtest
git checkout -b claude/trench-warfare-2d-3d-plan-idt7lf
tar -xzf ../trench-warfare-handoff.tar.gz
git add -A && git commit -m "Add 3D transition roadmap docs and Unity 6 project skeleton"
git push -u origin claude/trench-warfare-2d-3d-plan-idt7lf
```
Open a pull request against `main` only if the owner asks for one.

## 5. Prompt for the next agent
> Continue the Trench Warfare 3D project on branch `claude/trench-warfare-2d-3d-plan-idt7lf`. Read `HANDOFF.md`, then
> `docs/PLAN.md` and `docs/01-phases-and-dependencies.md`. First compile the Unity project and make the tests pass, then
> work the phases in the order in section 3 of HANDOFF.md, keeping the determinism rules in `docs/03` and the frozen
> contracts in `docs/02`. Run `trench-warfare-3d/validate.py` before each commit and push to the same branch.
