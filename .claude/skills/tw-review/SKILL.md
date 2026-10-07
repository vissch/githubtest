---
name: tw-review
description: Code review for Trench Warfare 3D - review one unit of code for correctness (findings in the report's format with P0-P3 severity), or be the sceptical second reader of one finished fix unit after fixcheck has run. Use for "review unit N", "review these commits for correctness", "second reader of rv-03", "is this fix real". NOT for scoring looks or plans out of 100 (the skill tw-critic), NOT for landing review (the skill tw-master), NOT for finding bugs by running the game (the skill tw-bug-catcher).
---

# Code review and the second reader

The method and its reasons are in `docs/reference/review.md`: read it when you run a whole review. This page is
what one reviewer or one second reader needs. Both are read-only and run nothing.

## 1. Reviewing a unit

You get: the checkout to read, the commits or the file list, the rules that code is held to, and where to look
hardest. Missing one of them: ask for it in your report's first line and review with what you have.

- Read first what the code is held to: `docs/03-determinism-rules.md` for anything under `Sim/`, `Net/`, `Data/`;
  the seam in `CLAUDE.md`; `docs/05-performance-budgets.md` for anything drawn every frame;
  `docs/reference/decisions.md` for the owner's rulings.
- Read every file of the unit in full unless the caller says "by diff". `git diff <base> <head> -- <path>` shows
  what changed, `git log --oneline <base>..<head> -- <path>` why.
- Report only what you read at the cited lines. If reading cannot settle it: kind `unsure`, with what would
  settle it. "Read in code" is never "seen in Play".
- A review, never a fix.

Severity: **P0** a desync, lost work, or a tool that passes or lands a bad tree. **P1** wrong behaviour a player
or an agent hits. **P2** an edge case, fragile code, a test that cannot fail. **P3** cleanup.

A finding is one header line and an indented body. Ids are the unit's letters and a number, unique in the report:

```
N7.3 | P1 | bug | Sim/Match/SeaLanding.cs:94,200 | read in code
  What is wrong, and how it fails: the situation, then the wrong result. Two to five lines.
  Fix: one line. Size S/M/L. Lane sim|show|tools.
```

Kinds: `bug`, `cleanup`, `unsure`, `docs`. Worst first, no padding, under about 2,000 words. End with `Clean:`
(what you checked and found sound), `For other units:` and `Files read:` with line counts. Send the report as soon
as you have been through every file once.

## 2. Second reader of a fix unit

You get: the unit's id, its commits on top of a base, the findings it was asked to fix, and the `fixcheck` record.
Be sceptical: find out whether the commits do what they claim.

Read the commit messages in full first; they state the claims. Then, citing file and line at the head commit:

- **Each finding:** does the change fix it? Would the new test fail on the old code, and for this finding's own
  reason? Read the test and the old code. Say where you disagree with `fixcheck`. Its six verdicts: `PROVED` red on
  the old code for this finding's reason and green on the fix; `RED` red there, but the failure does not name the
  finding; `WEAK` not shown red on the old code; `NO TEST` a commit says why there is none; `UNCHECKED` its tests
  did not run; `FAIL` the test cannot fail, or the fix is red, or nothing names the finding.
- **The hash:** if a commit says no hashed value moves, check every value the changed field takes in the shipped
  tables. If the hash moves, there is a seam commit of its own with a `ReplayRecorder.FormatVersion` bump.
- **BOMs:** `git cat-file -p <head>:<path> | head -c 3 | xxd -p` must not print `efbbbf` for a file the commits
  touch.
- **Lane rules:** a sim lane touches only `Sim/`, `Net/`, `Data/`, the sim tests and docs; a show lane none of
  those. `docs/02-contracts.md` is a seam surface: a commit of its own, first.
- **Anything the commits changed that the messages do not mention.**

Answer in this shape, under 600 words:

```
VERDICT: PASS | FAIL | PASS WITH NOTES
[ID] pass/fail: one or two sentences with file:line      (one line per finding)
BOMs: ...   hash claim: ...   docs/lane: ...
Not checked or unsure: ...
```

A FAIL names the exact line and what is wrong there.

## 3. For whoever starts the reviewer

- One reviewer per unit of about 3,000 lines, two at a time. Start it as the agent `tw-reviewer`.
- Brief it for **correctness**. A brief that asked a reviewer to hunt for "bypasses" of the guards was ended by the
  service's safeguards half way through its report.
- Write each report to a file the moment it arrives, then re-read every P0 and P1 at its line yourself.
- A second reader's FAIL: re-read it at its line, then queue a follow-up unit named after the first with the next
  letter (`rv-03b-...`).
