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
- **2026-09-27, to `lane/show/aosa`.** `CombatFx.cs` was split into partials (`CombatFx.Ground.cs`, `CombatFx.Chunks.cs`,
  `CombatFx.Ambient.cs`, `CombatFx.Bodies.cs`, and `CameraShake.cs`). A trial merge of your branch conflicts there, and git cannot
  follow your 97 changed lines into the moved code. Resolve `CombatFx.cs` to the split side, then run
  `python Tools/port_split.py` (`workflow.md`, section 8): in the trial it placed 17 of your 20 edits. The other 3 are
  real conflicts with the integration branch and need a person: `MaxMarks` is now 900 in `CombatFx.Ground.cs`, and the
  Column and Burst `books.Add` calls in `Explode` gained arguments (`flight * lean`). Also:
  - your new `public static bool ShowOverlays` will fail `StaticLifecycleTests` until it resets when Play ends
    (`SceneStatics.Register`) or is added to that test's `Explained` list with a reason;
  - `UI/Selection.meta` is an add/add conflict (two GUIDs for one folder, nothing references it): take either;
  - `FlipbookFx.cs`, `NightLights.cs`, `PropDestruction.cs` and `Flipbook_URP.shader` conflict with the flamethrower
    commits (60714dd), not with this pass.
  Tools/aosa/land.ps1 (on your branch) silently reverts `TW-URP.asset` and the URP global settings; an intended
  settings change will be lost without a message.
- **2026-09-27, to `lane/show/ui-selection` and the main clone's uncommitted selection work.** `UnitPicker.cs`
  conflicts: you and `lane/show/maint-2026-09` both fixed the 1.5 figure scale. Take the maint side
  (`FigureScale = FigureMetrics.UnitScale`, one constant shared with `VATRenderer`); the value is the same 1.125.
- **2026-09-27, to `lane/show/units-meta`.** `TankCapture.cs` conflicts: both branches replaced the archetype chain in
  `Spawn`. Take yours, which reads your new roster model; drop the maint version. Your one-line `CombatFx.cs` edit
  moves across the split with `Tools/port_split.py`. `UI/Selection.meta`: take either GUID.
- **2026-09-25, to every lane cut before 9c1ec32** (`lane/sim/units-meta`, `lane/show/units-meta`, `lane/show/aosa`).
  Your branch has no `CLAUDE.md`, no `gate.ps1` and none of `docs/reference/`. `git pull --rebase` onto
  `claude/trench-warfare-2d-3d-plan-idt7lf` once this pass lands; `validate.py` will then check your docs too.
