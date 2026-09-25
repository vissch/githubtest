# Inbox: notes from one session to another

Cross-session messages on this machine often expire unread (a session's user has to approve them). Anything
another session must know goes here too: dated, addressed by lane or by file, and short. **Read it when you land.
Delete a note once it is done or no longer true**, so this file stays a list of things still pending.

A fact about the code that is always true (an invariant) does not belong here. Put it in the code as a comment,
in the `tasks.md` row's Trap line, or in a test.

---

- **2026-09-25, to `lane/show/units-meta` (worktree `githubtest-sim`).** That worktree has an uncommitted change to
  `Sim/Core/RosterEntry.cs` (`SlotCount` 8 → 10) and FactionRoster.cs (a file only that branch has) on a SHOW branch, and the branch
  already carries 41 `Sim/` files. `SlotCount` is a seam item with 56 references in 19 files, including the IMGUI
  `BattleHud`, whose fixed arrays no test exercises. Land the `Sim/Core` change alone as a seam commit on
  `lane/sim/units-meta`, rebase the show branch onto it, and check `BattleHud` in Play with ten slots (or retire it).
  Full reasoning: `maintainability-audit-2026-09.md`, risk 1.
- **2026-09-25, to `lane/show/aosa`.** `lane/show/maint-2026-09` splits `Presentation/Camera/CombatFx.cs` into four
  partial files (no behaviour change) and moves `CameraShake` to its own file. Your branch has a ~20-line edit to
  `CombatFx.cs`: on rebase, re-apply it to whichever partial now holds that code (`tasks.md`, Combat effects).
  Tools/aosa/land.ps1 (on your branch) silently reverts `TW-URP.asset` and the URP global settings; an intended settings change
  will be lost without a message.
- **2026-09-25, to every lane cut before 9c1ec32** (`lane/sim/units-meta`, `lane/show/units-meta`, `lane/show/aosa`).
  Your branch has no `CLAUDE.md`, no `gate.ps1` and none of `docs/reference/`. `git pull --rebase` onto
  `claude/trench-warfare-2d-3d-plan-idt7lf` once this pass lands; `validate.py` will then check your docs too.
