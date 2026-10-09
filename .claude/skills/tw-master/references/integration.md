> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

Path keys: R = repo root, P = trench-warfare-3d/Assets/_Project, T = trench-warfare-3d/Tools, D = docs.

## E. MASTER (integrate and land)

### `CLAUDE.md` (`R\CLAUDE.md`, 109 lines against a cap of 110)
- **Lanes** (:39-54): work it out from the branch, `lane/sim/*` or `lane/show/*`; any other branch means stop and ask.
  - SIM owns `Sim/**`, `Net/**`, `Data/**`.
  - SHOW owns `Presentation`, `UI`, `Editor`, `Perf`, `Resources`, `Art`, `Shaders`, `Settings`, `Scenes`.
  - "The folder decides the lane". SHOW changes the sim only via `SimCommand` or `SimHost.WriteWorlds(...)`.
  - `Tools/**` and `gate.ps1` are shared: change them in their own commits.
- **Integration** (:69-82):
  - Lanes branch off `claude/trench-warfare-2d-3d-plan-idt7lf` and rebase; no lane-to-lane merges and no merge commits on integration.
  - Rebase conflicts: outside your lane, `git checkout --ours -- <file>`. For shared files, merge by hand; in `decisions.md` keep every row. For a gen block, `python Tools/codemap.py`. For split files, `python Tools/port_split.py <old file> --rebase`.
  - Land only when the owner says so: rebase onto origin's integration branch, run the full gate on that exact commit, then `python Tools/land.py`.
- **Owner decision procedure** (:80-81): "commit it alone (only `decisions.md`), cherry-pick it onto a `lane/show/decision-<date>-<topic>` off origin's integration branch, `land.py`, then delete it here and on origin."
- **Gate** (:84-92): `powershell -NoProfile -ExecutionPolicy Bypass -File gate.ps1 [-EditOnly]` from the repo root. SIM changes also need the determinism, replay and hash tests; SHOW changes need a look in Play.
- **Asking and remembering** (:94-102):
  - Ask the owner in the chat (in Claude Code: AskUserQuestion) with one decision per question, written into `decisions.md` in the same turn.
  - With nobody to ask: add it to the open questions with a default, and do not land on it.
  - Inbox: `docs/inbox/<date>-<to>-<topic>.md` (`<to>` like `show-aosa`, or `all`). The receiver deletes it.
  - `agent-memory.md` is capped at 150 lines (currently 139).
- **Merge-commit exceptions** by the owner (decisions.md:67-70): the overhaul, aosa and units-meta each landed as one merge commit.

### `land.py` (`T\land.py`)
- Usage: `python Tools/land.py [--dry-run] [--carry-sim "<why>"]`. Exit 0 landed (or would), 1 refused, 2 push failed (:4-6, :26).
- **Refusals:**
  - not a `lane/sim|show/*` branch (:74-76);
  - uncommitted changes (:77-79);
  - `origin/<lane>` moved on fetch; the lease is read *before* the fetch (:83-89);
  - HEAD does not contain `origin/<integration>`: rebase and gate again (:93-94);
  - a SHOW lane with files under `Sim/`, `Net/` or `Data/` (diffed `--no-renames`) without `--carry-sim` (:103-109).
- **Code lane vs code-free lane:**
  - Code means anything under `trench-warfare-3d/` outside `Tools/`, or `gate.ps1`, or any file whose name a test quotes, such as `envatlas.py` or `17-ui-art-spec.md` (`is_code` :51-53, `names_tests_read` :56-63).
  - A code lane needs `tw-gate-green` to equal `HEAD^{tree}` (:112-120).
  - A docs or Tools-only lane runs `validate.py`, plus `Tools/selftest.py` when `Tools/` changed (:121-128).
- **Push:** `git push --atomic origin HEAD:refs/heads/<integration> --force-with-lease=refs/heads/<lane>:<lease> HEAD:refs/heads/<lane>` (:130-131). The local integration ref moves only after success (:140).
- **The marker:** `gate.ps1` writes `<tree> <timestamp>` to `git rev-parse --path-format=absolute --git-path tw-gate-green`, which is per worktree. It writes only on a full green run and only if the tree before equals the tree after (`gate.ps1:111`, :127-135).

### `health.py` (`T\health.py`)
- `python Tools/health.py [--compile] [--lanes]`, about 15 s. Exit 0 all good, 1 something to fix (:5-7, :27-28).
- **Lines** (:16-25): lock, memory (commit headroom: under 6 GB LOW, under 10 GB no new editor; :152-155), editor (compile failed?), validate, branch (lane, ahead/behind, files outside the lane; `Tests/` are exempt, :202-205), inbox.
- **Inbox:** notes on your branch, on integration and on other lanes' origin refs. The recipient is the longest matching lane name; "FOR YOU" marks yours, and it flags notes addressed to no existing lane (:60-87).
- **`--lanes`:** every worktree's branch, last commit, ahead/behind and uncommitted count, plus a `git merge-tree` trial merge naming the conflicting files (:98-130).

### `scorecard.py` (`T\scorecard.py`)
- `python Tools/scorecard.py [--selftest] [--history FILE [--accept]]` (:4-11).
- A regressed run never becomes the baseline. A missing or `-1` metric counts as a regression (:112-133).
- **Metrics** (:32-40):
  - lower is better: codemap_errors, mandatory_read_tokens, unrouted_files, files_over_1000_lines, largest_file_lines, scenehooks_refs/files, statics_explained, selftest_failed, editmode_failed, playmode_failed;
  - higher is better: validate_ok, selftest_cases, editmode_tests, playmode_tests;
  - info only: codemap_seconds, claude_md_lines, tasks_md_tokens, workflow_md_tokens, pending_until_tags, inbox_notes, cs_files, cs_lines, stub_systems.
- Test counts come from `test-results-<mode>.xml`, with their age in hours (:88-97).

### Requirements for new files
- **`validate.py`** (`R\trench-warfare-3d\validate.py`):
  - asmdef JSON; unknown references; `TW.Sim.*` references only `TW.Sim.*`; no cycles (:18-39);
  - no `UnityEngine`, `Mathf.`, `Time.deltaTime` or `System.Random` under `Sim/` (:41-46);
  - **the first line of every .cs starts `// Phase:`** (:51-55);
  - balanced braces (:57-61);
  - every `using TW.X;` names a namespace that exists (:66-76);
  - `AudioManager` `m_Volume` equals 1 (:81-87);
  - runs `codemap.py --check` (:92-100).
- **`codemap.py --check`** (`T\codemap.py:17-27`, `check()` :516-590) fails on:
  - a stale `<!-- gen:NAME -->` block;
  - a folder two levels under `_Project` with no `PURPOSE` line (:57, :528-531);
  - a command-line arg, pref or env var with no `FLAG_EFFECT` line (:111, :532-533);
  - a `STATIC_SWITCHES` entry no longer declared (:130, :534-536);
  - a backticked cited path, or `File.cs:NN`, that does not exist (:481-513);
  - **a test class that tasks.md never names outside gen blocks** (:539-543);
  - **a production .cs named on no agent page** (CLAUDE, tasks, workflow, pipelines, code-map, feature-flags) (:545-552);
  - an `(until "<subject>" lands)` whose commit has landed (:554-571);
  - a `Tools/*.py` or `tw` subcommand in neither pipelines.md nor workflow.md (:572-579);
  - `agent-memory.md` over 150 lines or `CLAUDE.md` over 110 (:51-52, :581-590).
  - Regenerate with `python Tools/codemap.py`.
- **`selftest.py`** (`T\selftest.py:1-16`): run it after changing any tool. It breaks a throwaway worktree on purpose (codemap cases, port_split cases, health `--lanes`, scorecard, land cases); about a minute. `validate.py` alone does not run it.
- **`port_split.py`** (`T\port_split.py:1-33`): `python Tools/port_split.py FILE --rebase|--merge|--from BASE --to REV [--dry-run] [--into F]`. Hunks it cannot place go to `FILE.port.rej`. Exit 1 when anything went to `.rej`; then compile, `git add`, `git rebase --continue`.

### Inbox (`D\inbox\README.md:1-11`)
- One note per file. The receiver deletes it in its own commit. A permanent fact goes to a comment, a tasks.md Trap line or a test instead.
- **Current notes:**
  - `2026-09-26-show-ui-selection-overhaul-ability-targeting.md`
  - `2026-09-27-all-lanes-landed.md`: every lane landed; catch-up table; ParaDrop 12, FormatVersion 9, chain pinned.
  - `2026-09-28-all-pipeline-tooling.md`: aosa tools default to the checkout they run from; `TW_PROJECT`, `TW_LIB` and `AOSA_FALKIT` override.

### Maintainability audit (`D\reference\maintainability-audit-2026-09.md`)
- **Top risks** (:14-28):
  1. seam change on the wrong lane
  2. statics survive Play (inventoried, not fixed)
  3. SceneHooks is a global bus
  4. files over 1,000 lines (CombatFx, TankRenderer…)
  5. determinism checked only against itself (R5 golden hash)
  6. per-frame full-slot loops
  7. machine paths in tooling
  8. repo weight (LFS, github-test1, Linux CI)
- **Backlog** (:74-98): R4 retire BattleHud, R2 SceneHooks → services, R1 split Presentation/Camera, R3 split TankRenderer, R5 golden hash, R6 static resets, R7 hot paths, R8 owner hygiene, R9 record live matches, R10 CI for validate/selftest, R11 CombatFx for real.
- **Re-audit triggers** (:100-104): a file past 1,000 lines, a new `Explained` static, a new SceneHooks member, or `validate.py` edited to skip a rule.
