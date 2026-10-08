# This leg: EXECUTE the plan
Your leg card holds the plan. Do the steps under "Steps", in order. If the heading says "part N of M", those are the
only steps for this leg.
- Do not redesign. If a step is wrong or cannot be done, stop, say which step and why, and end BLOCKED.
- Run the plan's checks. A check that fails is a result: report it as failed, do not hide it.
- Commit only with the edit gate green for that exact tree, then push your lane. Leave nothing uncommitted.
- Never start the full gate: it is the master's, before a landing. Yours is the edit gate, and `leg gate wait`.
- Never end your turn while a gate or a test run you started is still going: a leg that ends then counts as failed.
  Wait for its verdict, then finish.
- A PlayMode test is run with `leg play <its class or full name>`: it starts the run for this checkout, waits, and
  prints total, passed and failed with each failed test. It is what a goal's long `Unity.exe -runTests` line asks
  for: never start Unity for a test by hand.
- Your leg card names the gate and close-out commands. `leg gate start` runs the edit gate detached, `leg gate wait`
  waits for it (call it again while it says RUNNING), `leg finish` commits and pushes, `leg done` checks nothing is left.
