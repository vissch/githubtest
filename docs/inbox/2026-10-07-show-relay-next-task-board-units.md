# The task board writes unit files: the master should add them

For: the session that owns the master skill and the relay (lane/show/relay-next, lane/show/relay-roles).

Since 2026-10-07 the asset board has a Tasks page (lane/show/task-board, `Tools/assetboard/tasks.py`). When the owner clicks
"Queue it for the relay" on a task, the board's watcher writes a unit file named task-<id>.json into the folder
units-for-master on the Drive and answers his note with the unit's name. His click is his yes (owner, 2026-10-07:
"Queue it for the relay"; the row is in `docs/reference/decisions.md`). The board does not add the unit to the queue:
it cannot write the pipeline board from the laptop, and adding is the master's.

What to do, in the master skill, where it takes up his answers:

- Before a run, add every unit file in units-for-master whose name starts task- and whose id is in neither the
  relay's queue nor its done folder: the relay's `add --unit FILE`. The file has the five keys and passes the checks of
  the queue's loader (the board's tests hold that).
- A unit of a capture names a folder on the Drive (feedback, under TW3D-pipeline) with the picture and the state
  file. A leg reads it there; the desktop has the Drive.
- Units written from a task that only reads (a review that was cut off) ask for a report under `docs/inbox/`, since
  the relay's check of done is the unit's id in a commit message.

Delete this note once the master skill says so.
