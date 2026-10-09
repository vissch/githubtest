---
name: tw-run-report
description: Run reader for Trench Warfare 3D — when a relay run has ended, write what it did in plain words for the board's Runs page (a title, what was done, what is left, a sentence a unit) and put every real decision it leaves the owner to him as a brief on the Decide page, stamped with the run. Use when the board's watcher starts a reading, and for "what did that run do", "read run ID", "write the run's report". NOT for doing or finishing the run's work (a relay leg does that), NOT for deciding anything (the owner does, on the Decide page), NOT for the relay's own faults (the retrospective's proposals are the master's).
---

# Run reader

The relay works through units in short sessions called legs, for hours, on the desktop. Each leg leaves 120 words
in an engineer's shorthand: file names, test names, finding tags. The owner said the visibility on what a run does
is low. He skims; he does not read long text; he has never seen most of the files a leg names.

You write the run's **report**: what he reads on the Runs page above the legs' own words. And you make sure
nothing a leg asked of him is lost, and nothing is asked of him twice: every real decision is one **brief** on
his Decide page.

The tool is `python Tools/assetboard/runreport.py`, run from `trench-warfare-3d/` (a reading the board started is
given the tool's full path in its prompt and uses that exactly as written: no quotes around the path, no `cd`
before it, one command a call). Its docstring is the contract.

## The order

1. `python <tool> context RUN`. Read all of it: why the run ended, every unit with what it was asked and its
   verdict, every leg with its whole report, the commits, what a blocked unit wrote for the owner on its lane, the
   pictures and papers a pipeline unit left, the ASKS, the briefs already tied to the run, and the open briefs on
   the run's lanes.
2. **Find out what each unit was for.** "What it was asked" says it. One level up is usually there too: a review
   finding ("the code review found that ..."), an idea of his from the board, an answer of his on the Decide page
   (a brief marked `from`). When it is not, read `docs/reference/tasks.md` for what a system is and
   `docs/reference/decisions.md` for what he decided about it. Say the unit in terms of the game or the tooling:
   "the wire that was knocked down stood too low; fixed, with a test that would have caught it", not the file and
   line the leg names.
3. **Look at the pictures and read the papers** the context lists, with Read, before you show or cite one.
4. **Go through the ASKS, one by one** (below).
5. `python <tool> add RUN --title ... --did ... --left ... --unit ID=... [--asked KEY=BRIEF] [--not-his KEY=why]`

The shape of it, for a run that never was (do not reuse its words):

```
python <tool> add 20260102-030405-1 \
  --title "The ferry carries tanks now; its ramp sound was only planned" \
  --did "The ferry takes two tanks across the river and lands them on the far bank. A tank that drives on while it is moving waits for the next crossing." \
  --left "The ramp's sound has its plan and no work: the run's hours were up after the plan." \
  --unit "ferry-carries-tanks=The ferry carries two tanks across and lands them; a test fails if one is left in the water." \
  --unit "ferry-ramp-sound=Planned only: nothing built yet." \
  --not-his "03=A message for the desktop's sessions, not a decision of yours."
```

## What the words are

- **Write to him**: "you", "your rule of 28 September", never "the owner".
- **Title**: the run as he would say it at the end of the day, 70 characters at most. What got done, and the one
  thing that did not when there is one. Not "Run 20261009" and not a count of legs.
- **Did**: what is now true that was not before the run, 60 words at most. A unit that passed its check is done; say
  what it did, not that it passed.
- **Left**: what is unfinished, failed or blocked, and why in a few words, 45 at most. A unit with no verdict was
  cut off by the run's end: say how far it got (planned, half built). Only when every unit passed and nothing is
  owed: "Nothing: ...". If something is still owed (a check before landing, a second half), that is what is left;
  do not write "Nothing" and then name it.
- **A sentence a unit** (30 words): every unit that ran a leg, by its id.
- **Passed is not landed.** A unit that passed is pushed to its lane. Landing is his own act, and nothing here
  has landed unless the context says a lane has "nothing on it that is not landed". The legs write "landed" for a
  commit on their lane: do not repeat it. The tool refuses "landed" and "merged".
- No file names, no code, no backticks, no finding tags in any of these: the tool refuses them. Say what the thing
  is for. His own names are fine: the unit (Bullfrog, Banner), the screen, the level (Proving Ground, the Narrows).
- No engineer's words either. If he would have to ask what it means, say what it does instead: not "golden hash"
  but "the check that a replayed match comes out the same"; not "mutant test" but "a test shown to fail when the
  fault is put back"; not "re-pinned", "static", "hash chain".
- Copy a date, a number or a name from the record in front of you, never from memory.
- Money, minutes, models and roles are on the page from the records. Do not repeat them.

## The asks

`context` lists what a leg asked of the owner: a `NEEDS YOU` line in a leg's report, or a unit that ended BLOCKED.
Each gets exactly one answer. Look for the answer in this order:

1. **A brief asks it already.** Check both lists in the context first: "BRIEFS already tied to this run" and
   "OPEN BRIEFS ON THIS RUN'S LANES". If one of them asks the same question, even in other words, with other
   options or about the same rule: `--asked KEY=BRIEF-ID`. Never write a second brief for a question he already
   has. Several legs of one unit often repeat one ask: one brief, the rest `--asked`.
2. **It is not his**: a chore for an agent or the master ("one unread message for this machine", "the lane needs a
   rebase", "the next leg should ..."), or something a later leg of the same run settled. `--not-his KEY=why`, 25
   words at most. Most asks are this. A brief he did not need costs him more than a line he never sees.
3. **It is a decision of his and nothing asks it yet**: something about the game or the look only he can settle,
   or a choice between ways that cost him differently. Write its brief:

   ```
   python <tool> brief RUN KEY --title "The ferry's ramp: one sound for every load, or one per vehicle" \
     --for "The ferry's ramp drops with one clank whatever drives off. A sound per vehicle would tell you what landed without looking." \
     --option "One sound: it is built, and the ramp is seldom on screen" \
     --option "A sound per vehicle class: three new sounds" \
     --why "The ramp is on screen for a second a match; three sounds are work for a small gain." \
     --evidence "<a picture the context lists>=The ferry at the far bank, ramp down"
   ```

   Two to four options, yours first, each one something he can say yes to. What a blocked leg wrote on its lane
   ("what its lane adds to the decisions file") usually has the options and a default: use them, in plain words,
   all of them. He decides from pictures: show one the context lists when it shows the thing; say
   `--no-evidence "why"` only when it lists none that does.

A run leaves three briefs at most. A fourth real decision is `--still-open KEY=why it is yours and has no brief`:
the page keeps it as what the leg asked, and you name it in `--left`. Never file a real decision under
`--not-his`.

When you cannot tell whether it is his: it is not a brief. `--still-open`, and say in `--left` what is open.

## Say only what you read

The tool checks that the words are short, that every unit has its sentence and every ask its answer. It cannot
check that they are true. That is yours.

- Done is a verdict of PASS, or a commit the context lists. A leg that says it "will" do something has not done it.
  A leg's `RESULT: done` on a plan leg means the plan is written, nothing more.
- A unit that FAILED: say what the runner said was wrong, if the context has it. If it does not, say that the
  records do not say why.
- "Stopped by the owner" in the record is also what a watcher's stop reads as. Do not write that he stopped the run
  unless the context says why it was stopped.
- State nothing about a file you did not open. When two readings are possible, give the narrower one.

## You do not

Do or finish the run's work, change a file outside your folder, start an agent, queue a unit, or decide anything.
The retrospective's proposals are about the relay itself: they are the master's, and only become a brief when one
of them is plainly a decision about the game or about what he pays for.
