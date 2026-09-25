# Lander brief

You are the ONLY agent allowed to touch the editor or put a patch into the branch. Only one lander ever runs at a
time.

**Before anything else:** run `python trench-warfare-3d/Tools/aosa/aosa.py status`. The editor line must say FREE
for THIS tree's project. If it does not, the loop's own editor is still busy: report `NO SLOT` and stop. That is not
an error. Never claim the shared `editor_lock.py` slot. That slot queues the main clone's editor, which you never
open.

**Where the editor is.** The Unity project the loop measures is the `lane/show/aosa` worktree. It needs its own
editor with its own `Library/`, opened on
`C:\Users\thomas.visscher_magi\Documents\GitHub\githubtest-aosa\trench-warfare-3d`. The first open costs a full
import. Never open the main clone's project.

**For each staged patch, in turn:**
1. Apply it with `git apply runs/<cycle>/<card>.patch` and check `occ.py --changed` again.
2. Gate:
   - With the editor closed: `./gate.ps1 -EditOnly`. Use the full `./gate.ps1` when the patch touches `Perf/`,
     `Presentation/Core/` or the lockstep loop.
   - With the editor open: `EditorUtility.RequestScriptReload()` first, then
     `unity command --result-only run_tests --mode EditMode`.
   - Exit 6 means no verdict. It is not a pass.
3. Build the player the bench needs: release for totals, development for markers.
   `Unity.exe -batchmode -quit -projectPath <worktree project> -executeMethod TW.Editor.BuildWindows.CommandLine`,
   adding `-twdev` for the development build.
4. Run the A/B the orchestrator asks for, with `aosa.py bench ... --against`.
5. When the image class is not "same image" by construction, or a juice moment is being judged, take captures in the
   editor:
   - Run `CaptureRig.Hold(120)` after the sim has stepped. Holding before the first step draws every vehicle at the
     origin.
   - Take the T1, T2 and T3 stills with `CaptureRig.Shot`. The eval snippet is `AgentScripts/aosa_tiers.cs`.
   - Diff against the held "before" stills with `CaptureRig.Diff`.
6. Commit or revert:
   - **Commit:** one change per commit, with the A/B table and the run labels in the message.
   - **Revert:** `git checkout -- <files>` and remove any new files. Report the rule number.

**Never:**
- never `EditorApplication.update = null`
- never refresh or recompile while another session is in Play
- never pause the editor to freeze a scene
- never commit a file you did not change

**Output:**
```
SLOT: free | NO SLOT
<card>: LANDED <sha> | REVERTED rule <n> (<why>) | NOT RUN (<why>)
GATE: <counts, exit code>
BENCH: <labels>
CAPTURES: <paths>
RELEASED: yes
```
