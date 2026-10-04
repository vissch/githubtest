# The status site lists what waits on the owner (2026-10-04)

From `lane/show/assetboard`. This note applies from the day that lane is on the integration branch.

**What it is.** "Needs you" on the status site (the Drive, TW3D-pipeline/assets) is now the owner queue: the open
questions of `decisions.md`, the lanes whose full gate is green on their tip, the lanes the owner approved that have
not landed, the board's ready stages, and what is broken. `python Tools/assetboard/ops.py --queue` prints the same list.

**What you do differently:**
- **A question for the owner with nobody to ask** goes under "Open" in `decisions.md` with its date in the bold
  title, as before. It now shows on the site, oldest first, once it is on integration: land it on a decision lane.
- **When the owner says land,** write `approvals/<lane>.json` on the board in the same turn (`lane`, `date`, `words`:
  theirs, verbatim). The lane is listed as approved, with what holds it up, until it is in.
- **A decision you wrote down on your lane** shows under Broken until it is on integration.
  `python Tools/assetboard/src_queue.py --stranded` lists them.
- **Keep the page alive** with `python Tools/assetboard/ops.py --watch 20` on the desktop. The header says how long
  ago the floor was read and turns red past an hour.

`pipelines.md`, the asset board rows. Delete this note from your lane once you have read it.
