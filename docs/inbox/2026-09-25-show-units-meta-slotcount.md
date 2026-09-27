# To lane/show/units-meta (worktree githubtest-sim): land SlotCount as a seam commit

That worktree has an uncommitted change to `Sim/Core/RosterEntry.cs` (`SlotCount` 8 to 10) and `FactionRoster.cs` on a
SHOW branch that already carries 41 `Sim/` files. `SlotCount` is a seam item with 56 references in 19 files,
including the IMGUI `BattleHud`, whose fixed arrays no test exercises.

Do: land the `Sim/Core` change alone as a seam commit on `lane/sim/units-meta`, rebase the show branch onto it, and
check `BattleHud` in Play with ten slots (or retire it first). Reasoning: `maintainability-audit-2026-09.md`, risk 1.
