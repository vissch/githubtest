---
paths:
  - "gate.ps1"
  - "trench-warfare-3d/Tools/gate_*.py"
  - "trench-warfare-3d/Assets/_Project/Tests/**"
---

# Gate and tests: traps (checked 2026-10-14)

- **The verdict is the results xml you watched appear, not an exit code.** A run that aborted on a compile error
  wrote no xml and an older file was read as green. Check the file's start time against your launch and that the
  tests you added are in it by name.
- **A green with a small count is "nothing ran".** Read the denominator before the colour.
- **Never pipe the gate or `validate.py` into `tail` or after a `;`** when the exit code decides the next step: a
  commit landed red that way. Write to a log and test the code.
- **A new test must fail on the old code.** Run it once against the commit before the fix; a test of the limit
  that passes on both sides guards nothing.
- **A red on a rebased lane is first the harness:** the batch run's cold import can eat the timeout, and the
  machine runs the same tests two to four times slower under load. Rerun before reading it as the lane's fault.
- A test reads the tree relative to the project folder (Unity's working directory), so `Tools/otr.py` can run it.
