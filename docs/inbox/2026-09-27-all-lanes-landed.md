# To every lane: all of them have landed; catch up before your next commit

The integration branch `claude/trench-warfare-2d-3d-plan-idt7lf` carries every lane, landed in this order: the
overhaul (sim and show, one merge commit), ui-selection, playground, aosa (one merge commit), riders (with
gait-attitude's commit under it), units-meta (sim and show, one merge commit). `decisions.md` (2026-09-27) has why.
Each landed under a `-land` branch name, so your own lane branch is still where you left it.

In your worktree, with `git status` clean, `git fetch` first. Then:

| Lane | Worktree | Your commits are | Catch up with |
|---|---|---|---|
| show/overhaul, sim/overhaul | githubtest-overhaul, githubtest-overhaul-sim | inside integration (merged) | `git merge --ff-only origin/claude/trench-warfare-2d-3d-plan-idt7lf` |
| show/aosa | githubtest-aosa | inside integration (merged) | the same |
| show/units-meta, sim/units-meta | githubtest-sim | inside integration (merged) | the same |
| show/ui-selection | wt-ui-selection | replayed with new ids (rebased) | `git reset --hard origin/claude/trench-warfare-2d-3d-plan-idt7lf` |
| show/playground | githubtest-playground | replayed (rebased) | the same |
| show/riders | githubtest | replayed (rebased) | the same |

Before a reset, `git log --oneline origin/claude/trench-warfare-2d-3d-plan-idt7lf..HEAD` lists what the reset would drop.
Only your landed commits should be there: cherry-pick anything newer back on afterwards. On riders that list also shows
0b9b323 ("The battle HUD tells you who is in trouble..."): it is a copy of ui-selection's ef12e75, byte for byte, and
landed with ui-selection.

What changed under you:
- **units-meta's numbers moved** when it landed on the overhaul: `OffMapAbilityId.ParaDrop` is 12, its random streams
  17-21, replay `FormatVersion` 9, its events follow `MineCleared`. `VehicleArchetype.IsTank`/`IsWalker`/`IsArmoured`
  are gone: read `ChassisKind` through `SimWorld.ChassisOf` (tasks.md, "Adding a unit type").
- **Keys:** the digit row deploys the ten slots; F5 F6 F7 arm HE, gas and the drop; C M V B the line abilities.
- **The hash chain is pinned:** `SimHashTests.TheHashChainIsTheOneItsFormatVersionNames` fails when a system joins or
  moves (the systems and their order, not the fields each one hashes). Bump `FormatVersion` and pin the new chain in the
  same commit.
- **overhaul:** units-meta has landed, so the Sapper and the upgrades block (docs/21) can build on it now.

The last lane to catch up deletes this note (`python Tools/health.py --lanes` shows who is still behind).
