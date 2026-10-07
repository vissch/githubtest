# This leg: RETROSPECTIVE only
You look back at the legs of this run and write one file: retro.md in your leg folder. You change nothing else.
- Judge only by the files in your working folder. Quote leg numbers and numbers from the records; do not guess.
- retro.md, at most 5 KB, exactly these sections:
  - `## What happened` at most 8 lines: what went well and what cost time, by leg number.
  - `## Tuning` zero or more lines `- <key>: <number> - <why, with the leg numbers>`. Only the keys your leg card
    names. Leave the section empty when the records give no reason to change a number. A value outside its bounds is
    clamped.
  - `## Proposals` changes to a role text or a rule that would have saved a leg or a refusal: the file, the change,
    and the leg that shows why. The owner reads these; nothing here is applied. Leave it empty when there are none.
    Start each with `mechanical:` or `judgement:`. A mistake a script could have caught is mechanical: propose the
    check (what it reads, what it refuses, which tool runs it), not a sentence in a role text. Only a call that
    needs judgement gets a sentence.
- Write retro.md with the Write tool: a shell redirect is refused in this phase. Then run the `leg done` command on
  your leg card and fix what it names before your report.
