# The decisions stranded on lanes are on the integration branch (2026-10-04)

From `lane/show/decision-2026-10-04-stranded`. This note applies from the day that lane is on the integration branch.

**What changed.** 36 rows and open bullets of `docs/reference/decisions.md` that existed only on unlanded lanes are
now on integration, word for word, each in its section by date. Five rows of 2026-10-04 are new (Process: the
critique's two fixes, the Long tier, what a required check is, what may land without asking, the board stays), and
three open questions (pushing `lane/show/pipe-vfx`, the balance sweep's bands, a standing critic). The open bullet on
a CI check is answered and gone; "whether CI builds Windows" is answered in the same row.

**The row is ahead of its code.** A row landed here says what the owner decided. What it describes is still on the
lane that wrote it until that lane lands:

| Rows from | Lane |
|---|---|
| melee, the 20 % reach cut, crabs pounce, the Bullfrog | `lane/sim/bullfrog` (and `lane/sim/melee-v23`) |
| the VFX pass: blood, per-class looks, the sheets, smoke and fire | `lane/show/pipe-vfx` (not pushed: this desktop only) |
| men under fire, machine reach, the night look answers | `lane/show/nightly-v2` |
| the Proving Ground level, the six models' names, the crabs' feet | `lane/show/proving-ground` |
| the asset board | `lane/show/assetboard` |
| no constant bombardment | `lane/show/no-bombardment` |
| snipers and the trench mouth | `lane/sim/match-flow` |
| the absurd deaths are on | `lane/show/deaths-absurd` |
| the troops' bake, Unity on the owner's desktop, every Playground unit in the battle | the two `decision-2026-09-28-*` lanes, deleted now |

**When you rebase onto it.** `decisions.md` will conflict where your lane wrote one of these rows. Keep one copy of
each row, yours if you changed its wording since, and every other row from both sides.

**Finding them next time.** `python Tools/assetboard/src_queue.py --stranded` (on `lane/show/assetboard` until it
lands) lists what a lane wrote down and integration lacks. A decision still lands ahead of its lane, on a
`lane/show/decision-<date>-<topic>` lane (`CLAUDE.md`, Integration).

Delete this note from your lane once you have rebased.
