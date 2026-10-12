# This leg: PLAN only
You change nothing. You read, think hard, and write one file: plan.md in your leg folder.
The next leg has less thinking time and only your plan, so the plan must stand on its own.

plan.md: aim for 1,000 words, never over 1,400 (the check counts bytes and refuses a longer one). Read first, then
write the whole plan once. Every call after that costs your whole context again: a plan that is good enough is done,
do not polish it. Exactly these sections:
- `## Goal` one or two sentences, the owner's words kept as they are.
- `## Steps` numbered. Each step: what to change, in which file, and why. If the work does not fit one leg, mark
  where to cut: `--- leg break ---` between steps. Cut so that each part is one whole behaviour that passes
  its own check, inside this lane's folders (CLAUDE.md, "Lanes"), not one layer of several behaviours. A seam
  change (CLAUDE.md, "The seam") is its own part, first.
- `## Files` every file to read or change, in backticks, as a path from the repo root. A file the work creates
  gets `(new)` on its line. A file that is named, not marked (new) and not there fails the plan.
- `## Checks` the exact commands that prove it works, with the result you expect. A new test names where its
  expected value comes from: a worked example, a number in the goal, or a run of a build known to be right (name
  the commit). A value worked out the way the code works it out proves nothing.
- `## Done when` one line a script can check.
- `## Difficulty` one word, `easy`, `medium` or `hard`, then one sentence why. It picks the model that does the
  work. hard: it touches the seam, the sim's rules or the replay format, or more than five files, or no test can
  prove it. easy: one file, and the check is one command. Everything else is medium. In doubt take the harder.
- `## Risks` what could go wrong and what to do then. Leave out what cannot happen. When a reader could take
  something for part of this unit that is not, end with one line `Out of scope:` naming it.

Refused in this phase, so do not try: `>` and `<<EOF` redirects, `python - <<EOF` scripts, anything that writes.
A one-line `python -c` that only reads (json, counts) is fine. Write plan.md with the Write tool. Then run the `leg done` command on your
leg card. It checks the plan the way the runner will and prints its size; fix all it names in one edit and run it
again before your report.
