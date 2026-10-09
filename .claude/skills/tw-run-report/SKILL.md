---
name: tw-run-report
description: Run reader for Trench Warfare 3D — when a relay run has ended, write what it did in plain words for the board's Runs page (a title, what was done, what is left, a sentence a unit) and put every real decision it leaves the owner to him as a brief on the Decide page, stamped with the run. Use when the board's watcher starts a reading, and for "what did that run do", "read run ID", "write the run's report". NOT for doing or finishing the run's work (a relay leg does that), NOT for deciding anything (the owner does, on the Decide page), NOT for the relay's own faults (the retrospective's proposals are the master's).
---

# Run reader

The relay works through units in short sessions called legs, for hours, on the desktop. Each leg leaves 120 words
in an engineer's shorthand: file names, test names, finding tags. The owner said the visibility on what a run does
is low. He skims; he does not read long text; he has never seen most of the files a leg names.

You write the run's **report**: what he reads on the Runs page above the legs' own words. And you make sure
nothing a leg asked of him is lost: every real decision becomes a **brief** on his Decide page.

The tool is `python Tools/assetboard/runreport.py`, run from `trench-warfare-3d/` (a reading the board started is
given the tool's full path in its prompt and uses that, one command a call). Its docstring is the contract.

## The order

1. `python <tool> context RUN`. Read all of it: why the run ended, every unit with what it was asked and its
   verdict, every leg with its whole report, the commits, what a blocked unit wrote for the owner on its lane, the
   pictures and papers a pipeline unit left, the ASKS, and the briefs already tied to the run.
2. **Find out what each unit was for.** "What it was asked" says it. One level up is usually there too: a review
   finding ("the code review found that ..."), an idea of his from the board, an answer of his on the Decide page
   (a brief marked `from`). When it is not, read `docs/reference/tasks.md` for what a system is and
   `docs/reference/decisions.md` for what he decided about it. Say the unit in terms of the game or the tooling:
   "the wire frames that were knocked down stood too low; fixed, with a test that would have caught it", not
   "`BattlefieldComposer.cs:326` lift is now .15f".
3. **Look at the pictures and read the papers** the context lists, with Read, before you show or cite one.
4. **Go through the ASKS, one by one** (below).
5. `python <tool> add RUN --title ... --did ... --left ... --unit ID=... [--asked KEY=BRIEF] [--not-his KEY=why]`

```
python <tool> add 20261009-185051-11300 \
  --title "Three review fixes passed; the golden-match fix was only planned" \
  --did "Fixed three faults the second code review found in earlier fixes: the test level can field any ten units again, knocked-down wire stands at the right height, and two screen tests now really run in the nightly check." \
  --left "The fourth, a match-replay check that ran with no soldiers in it, has its plan and no code: the run was stopped after the plan." \
  --unit "rv-c1b-proving-ground-any-ten=The test level takes any ten units again; a test now fails if that breaks." \
  --unit "rv-c2b-wire-frames-height-and-test=Knocked-down wire is back at its height, with two tests that fail on the old code." \
  --unit "rv-12b-hud-tests-that-run-in-batch=Two screen tests that proved nothing in the nightly check now run there." \
  --unit "rv-10b-golden-match-with-men=Planned only: no code yet." \
  --not-his "01=An unread message for the desktop's sessions, not a decision of his."
```

## What the words are

- **Title**: the run as he would say it at the end of the day, 70 characters at most. What got done, and the one
  thing that did not when there is one. Not "Run 20261009" and not a count of legs.
- **Did**: what is now true that was not before the run, 60 words at most. A unit that passed its check is done; say
  what it did, not that it passed.
- **Left**: what is unfinished, failed or blocked, and why in a few words, 45 at most. A unit with no verdict was
  cut off by the run's end: say how far it got (planned, half built). When everything passed: "Nothing: ...".
- **A sentence a unit** (30 words): every unit that ran a leg, by its id.
- No file names, no code, no backticks, no finding tags in any of these: the tool refuses them. Say what the thing
  is for. His own names are fine: the unit (Bullfrog, Banner), the screen, the level (Proving Ground, the Narrows).
- Money, minutes, models and roles are on the page from the records. Do not repeat them.

## The asks

`context` lists what a leg asked of the owner: a `NEEDS YOU` line in a leg's report, or a unit that ended BLOCKED.
Each gets exactly one answer:

- **It is a decision of his**: something about the game or the look only he can settle, or a choice between ways
  that cost him differently. Write its brief:

  ```
  python <tool> brief RUN KEY --title "The Banner has no shield to move: add one, or move another part" \
    --for "The recut Banner was to raise a shield when it fires. Its model has no plate, so nothing moves there yet." \
    --option "Leave it: the top turns and the guns recoil, which is enough" \
    --option "Add a plate to the model and animate it" \
    --why "The Banner already reads as firing; a new plate is new art for a small gain." \
    --no-evidence "The unit left no picture of the Banner."
  ```

  Two to four options, yours first, each one something he can say yes to. What a blocked leg wrote on its lane
  ("what its lane adds to the decisions file") usually has the options and a default: use them, in plain words. Show
  a picture when the context lists one that shows the thing (`--evidence PATH=caption`); else say why not. Three
  briefs a run at most: if a run leaves more, the rest goes in `--left` and to the asks as `--not-his "KEY=more
  than three: named in what is left"`.
- **A brief asks it already** (the context's list of briefs, or the same ask from an earlier leg of this run that
  you just wrote a brief for): `--asked KEY=BRIEF-ID`. Several legs of one unit often repeat one ask: one brief,
  the rest `--asked`.
- **It is not his**: a chore for an agent or the master ("one unread message for this machine", "the lane needs a
  rebase", "the next leg should ..."), or something a later leg of the same run settled. `--not-his KEY=why`, 25
  words at most. Most asks are this. A brief he did not need costs him more than a line he never sees.

When you cannot tell whether it is his: it is not a brief. Say in `--left` what is open and that nobody has
decided whose it is.

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
