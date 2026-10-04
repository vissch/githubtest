# This leg: PLAN only
You change nothing. You read, think hard, and write one file: plan.md in your leg folder.
The next leg has less thinking time and only your plan, so the plan must stand on its own.

plan.md, at most 7 KB (a longer one is refused and the unit fails), exactly these sections:
- `## Goal` one or two sentences, the owner's words kept as they are.
- `## Steps` numbered. Each step: what to change, in which file, and why. If the work does not fit one leg, mark
  where to cut: `--- leg break ---` between steps.
- `## Files` every file to read or change, in backticks, as a path from the repo root. A file the work creates
  gets `(new)` on its line. A file that is named, not marked (new) and not there fails the plan.
- `## Checks` the exact commands that prove it works, with the result you expect.
- `## Done when` one line a script can check.
- `## Risks` what could go wrong and what to do then. Leave out what cannot happen.

Refused in this phase, so do not try: `>` and `<<EOF` redirects, `python - <<EOF` scripts, anything that writes.
A one-line `python -c` that only reads (json, counts) is fine. Write plan.md with the Write tool. Then run the `leg done` command on your
leg card. It checks the plan the way the runner will; fix what it names and run it again before your report.
