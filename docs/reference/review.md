# Code review: how it is done, and how a fix is checked

For whoever runs a review of this repo or fixes its findings. Not a page to read on landing.

## A review

The method of the first review (2026-10-04, 376 findings, the report is on the Drive beside the handoffs):

1. **Pin the tree.** Fetch, note the integration tip, and read from a detached worktree at that commit. Nothing is
   changed and no Unity is opened in it.
2. **Cut it into units** of about 3,000 lines by folder or module: the guards first (the gate, the checks,
   `Tools/land.py`), then sim, the seam and the sim tests, show, UI and editor, the other tools.
3. **One read-only reviewer per unit, two at a time.** Each gets its file list, the rules the code is held to
   (`docs/03-determinism-rules.md`, the seam in `CLAUDE.md`, `docs/05-performance-budgets.md`), the places to look
   hardest, and the finding format. It reads every file of its unit in full and lists what it read.
4. **The lead re-reads every bug finding at its line.** Not confirmed means dropped, or kept as `unsure` with what
   would settle it. "Read in code" is said as such: it is not "seen in Play".
5. **One report**: what is sound, the P1 findings, what only a played game can settle, the decisions only the
   owner can make, then every finding by unit.

Severity: **P0** a desync, lost work, or a tool that passes or lands a bad tree. **P1** wrong behaviour a player or
an agent hits. **P2** an edge case, fragile code, a test that cannot fail. **P3** cleanup.

A finding is one header line and an indented body:

```
U1 | P1 | bug | Sim/Match/SeaLanding.cs:94,200 | CONFIRMED
  What is wrong and how it fails: the input, then the wrong result.
  Fix: one line. Size S/M/L. Lane sim.
```
Ids are a letter or two for the unit and a number. They must be unique across the report: a fix names its finding
by id in square brackets, in a commit message and on its test.

## A fix

Findings are fixed as units on the relay, a few related findings each, on one lane per kind: a sim lane (every
hash change bumps the replay version, so two sim lanes would collide), a show lane, a tools lane. The unit's goal
names each finding with its line in the report and these rules: fix exactly these; check each against the code as
it is now; **every bug gets a test that fails on the old code and passes on the fix**, tagged with the id in a
comment (`// [U1] ...`) or in its title; where no test can be written the commit message says
`[U1] no test: why`; every id stands in square brackets in a commit message; no landing.

A test the edit gate does not run (PlayMode) the fixer runs itself in batch mode by name, and reports the counts.
A finding only a played game could settle gets a PlayMode test that walks the real path (the menu, the key, the
button), not a hand-built view.

## The check after each unit

The relay passes a fix unit when every id is in a commit message. That is not "fixed": of the first three units,
two had a fix that did not hold and one commit gave nine files a UTF-8 BOM. Two checks follow every unit, in this
order.

**1. The script.**
```bash
python Tools/review/fixcheck.py --tree <fixcheck's own checkout> --base <commit before the unit> --head <its last commit> \
    --lane lane/sim/<x> --ids U1 U4 --unit rv-02-fleet-fires
```
It answers: is every id named and tagged on a test (or excused in a commit); is each tagged test **red on the old
code and green on the fix**; did a file gain a BOM; is a file outside the lane. The old code is the tree at the
fix with every file that is not a test put back, so the new tests meet the code as it was.

| It prints | Means |
|---|---|
| `PROVED` | red on the old code with a failure that names the id, green on the fix |
| `RED` | red on the old code and green on the fix, but the failure there does not name the id. A unit fixes several things at once, so the test may be red for another fix's reason: read what it said (the record keeps it). Give the assert a message that holds the id, like `"[U1] the fleet never fired"`, and it reads as proved |
| `WEAK` | not shown red on the old code: the test did not run there (the old code does not compile with it, or its runner stopped before it). Not proof of the bug; the `note` lines say what the runner printed |
| `NO TEST` | a commit says why there is none |
| `UNCHECKED` | its tests need Unity and none was given, or a tagged test was not found by its name. Never a pass |
| `FAIL` | green on the old code too (the test cannot fail), red on the fix, no test and no reason, or no commit names it |

A follow-up unit that only makes an earlier fix's test able to fail has nothing to be red against: that fix is
already in its base. It says so in a commit message, `[N2b] fix in base: <the commit of the fix>`, and the script
puts the files of that commit back as well before the old run.

Exit 0 PASS, 1 FAIL, 2 UNCHECKED. The tree must be a checkout nobody else uses, with "fixcheck" in its folder name:
the script moves its HEAD and resets it. Python tests run anywhere; Unity tests run in batch mode by name, so that
checkout needs a Library of its own. `python Tools/review/test_fixcheck.py` is its tests: each verdict shown both
ways on a small repo with a stand-in for Unity.

**2. A second reader.** A fresh read-only agent per unit, given the unit's goal and its commits and told to be
sceptical. The script cannot see a fix that is wrong where no test looks; the reader can. The lead re-reads every
FAIL at its line, then queues a follow-up unit named after the first with a `b` (`rv-03b-...`), so it runs next.

Only then is a unit counted as fixed. Before a lane of fixes is called landable it also passes the full gate: the
relay's gate skips the Long tests and PlayMode.
