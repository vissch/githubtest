# This leg: EXECUTE the plan
Your leg card holds the plan. Do the steps under "Steps", in order. If the heading says "part N of M", those are the
only steps for this leg.
- Before the first edit of a step, read what the step needs in one answer: the files it names, side by side. The
  plan names them, so you need not search for them one at a time.
- Do not redesign. If a step is wrong or cannot be done, stop, say which step and why, and end BLOCKED.
- Run the plan's checks. A check that fails is a result: report it as failed, do not hide it.
- When your card asks for note.md and you cannot finish: under `## Decisions` goes every choice that is already
  made (the owner's, a brief's, a decisions.md row, or yours in this leg), one line each with where it was decided.
  The next leg works by them and does not ask again.
- Commit only with the edit gate green for that exact tree, then push your lane. Leave nothing uncommitted.
- Never start the full gate: it is the master's, before a landing. Yours is the edit gate, and `leg gate wait`.
- A tool change: run only the tests you added or changed, by name. Never run `Tools/toolcheck.py` or all of
  `test_relay.py` by hand, even when the card or goal says so: they take over 30 minutes here, and the edit gate
  runs toolcheck for a tool lane once. The goal's done_when is checked by the runner after the leg.
- Never end your turn while a gate or a test run you started is still going: a leg that ends then counts as failed.
  Wait for its verdict, then finish.
- A PlayMode test is run with `leg play <its class or full name>`: it starts the run for this checkout, waits, and
  prints total, passed and failed with each failed test. It is what a goal's long `Unity.exe -runTests` line asks
  for: never start Unity for a test by hand.
- Your leg card names the gate and close-out commands. `leg gate start` runs the edit gate detached, `leg gate wait`
  waits for it (call it again while it says RUNNING), `leg finish` commits and pushes, `leg done` checks nothing is left.
