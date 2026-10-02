# The EditMode tests are split into module folders (2026-10-02)

From `lane/show/test-modules`. This note applies from the day that lane is on the integration branch.

**What changed.** `Tests/EditMode/` became five folders, one assembly each: `Tests/Sim/`, `Tests/Match/`,
`Tests/Show/`, `Tests/UI/`, `Tests/Project/`. Every test file only moved: same content, same namespace `TW.Tests`,
same GUID, so `--filter TW.Tests.<Class>` works as before. The generated list at the bottom of `tasks.md` says which
folder holds which class.

**When you rebase onto it:**
1. Your edits to existing tests follow the move by themselves. No conflict is expected.
2. A test file your lane ADDED arrives in `Tests/EditMode/`, which is kept as a landing folder. It still compiles
   there. `python validate.py` then fails with one line per such file, naming its module folder and the two
   `git mv` commands (the file and its `.meta`). Run them, then `python Tools/codemap.py`.
3. From then on a new test goes in the folder of the highest assembly it needs. `Tests/Sim/` references only
   `TW.Sim.*`, `TW.Net` and `TW.Data`; a sim test that needs `LockstepSession` or `ScriptedEnemy` goes in
   `Tests/Match/`.

**The gate.** `gate.ps1 -EditOnly` now skips the sim and match tests when nothing your lane changed can reach them
(they are nearly all of the run time), and says so on its first line. `-EditOnly -All` is the old behaviour,
`-Module Sim,Match` runs exactly what you name, `-EditOnly -Plan` only says what would run. The full gate is
unchanged and is still the only run `Tools/land.py` accepts. The rule and its table: `workflow.md`, section 5.

**validate.py** runs one file per check from Tools/checks/. `python validate.py --only <name>` runs one while you
fix it; `--list` names them.

Delete this note from your lane once you have rebased and moved your tests.
