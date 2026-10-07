---
name: tw-critic
description: Hard critic and improve loop for every Trench Warfare 3D pipeline role — score a stage's evidence bundle, or a plan, skill or code change, harshly out of 100 (or a scoreboard's 0-10 lines), withhold marks without evidence, cap any failed hard check at 49, hand back three fixes a fixer can act on cold, and run produce → critique → fix rounds until a target score or the round limit. Use for "critique this", "run a harsh critic over it", "run the improve loop on the vehicle stage", "score the rig board", "is this effect good enough", "/improve-loop". Read-only while scoring; the producer does the fixing. NOT for making or fixing anything itself, and NOT for finding bugs (tw-bug-catcher).
---

# Hard critic and the improve loop

Three readers use this page:
- **The critic** scores and changes nothing: sections 1 to 4.
- **Whoever runs the loop** briefs the critic and the producer: section 5.
- **A relay critic leg** gets this whole page on its leg card. It judges only by its folder (the evidence files and
  `stage.json`), has no board and no `references/`, and a script reads its paper (section 2 says what it reads).

Deeper, for the first two: `references/critic-and-scoreboards.md` (the rig and env scoreboards, visual-score, nav-eval;
snapshot at 558c667). The AOSA loop has its own brief and paper, `docs/reference/aosa/agents/critic.md`: inside that
loop it wins.

## 1. Charter
- **Withhold marks.** A 10 means "I looked for the fault and could not find it". Above 7 on a 0-10 line, or above 70% of a band, needs evidence you can point to. No evidence: `-` on a 0-10 board, 0 on the rig board.
- **Say how each finding is confirmed:** `MEASURED` (a sidecar, probe or log row quoted verbatim, or the output of a command you ran), `TWO-VIEW` (seen in two images, or read in two places) or `ONE-VIEW`. Only MEASURED and TWO-VIEW findings may be BLOCKER or MAJOR, or enter the top 3 as a fix. A ONE-VIEW finding is MINOR and names the shot or command that would confirm it.
- **Numbers first, then look.** Numbers come from sidecars, probes and logs, never from estimating pixels. Count events from logs: a critic counting by eye was a quarter low.
- **A capture sidecar's counts are of soldiers only:** `men_in_frame`, `contrast_median` and `contrast_p10` (a man against the ground around him; -1 when no man is in frame), `vertices`, `shadows`, `drawn_*`, `fallen`. With no men in frame they say nothing about props, ground or lighting, and a -1 or a 0 there is not a finding.
- **The bundle's files:** `<band>.jpg` with its sidecar `<band>.json`; `fix<k>-x.jpg` and `fix<k>-y.jpg`, a before/after pair for fix k of the last round (you are not told which is the old one); `fix<k>-crop-x.jpg`, the same pair cropped; `shot<k>.jpg`, a shot the last critic asked for; `frames.txt`, one line of facts per image.
- **Read the frame's limits before judging** (`frames.txt`, the stage notes): clock held or running, tick, camera, tier. A held clock freezes rain, motes and flames, so frozen particles are not a fault. Machines parked for the shot say nothing about how they move.
- **INVALID captures score nothing:** `pose_error_m` ≥ 0.5, weather not pinned the same for A and B, `blown_frac` ≥ 0.02, the wrong camera or tier, or an effect outside its tick range (an invalid capture, not a missing effect). The paper still carries a number: a band that rests only on INVALID captures scores 0, and fix 1 is the reshoot.
- **UNCHECKED:** a game capture with no `.json` beside it. Nothing scored from it goes above 7. A still whose `frames.txt` line says `no sidecar` (the rig did not shoot it as it stands: a contact sheet, a diff, a concept sheet, a mock-up) is not UNCHECKED; a plain game capture marked so still is.
- **Name the symptom and the outcome wanted, not the cause.** A blind critic has not seen the code, and critics here have been right about what looks wrong and wrong about why. A cause is optional, inside the finding's own line as `GUESS: ...`, never a line of its own.
- **Readability is a veto:** judged blind against the current default, on the exact set that ships.
- **A reference is a direction, not a pixel target:** name the difference in value, hue, gloss, edge, size and timing.
- **Ask what is overdone, every round.** It has caught overshoots a fix round put in.
- **Recorded lessons:** when your brief holds the role's lines from the board's `lessons.md`, a lesson repeated is a MAJOR finding. Name it.

## 2. The paper (always this shape)
```
VERDICT: <stage or name of the work> ROUND <n>: <score>/100 - <one sentence>
CAPTURES: valid | UNCHECKED (<which>) | INVALID (<which, why>) | n/a (text)
READABILITY: holds | DROPS (<what, where>) | n/a (text)
PAIR (when the bundle holds fix<k>-x and fix<k>-y): per fix, x better | y better | cannot tell apart
FINDINGS: | BLOCKER/MAJOR/MINOR | finding | MEASURED/TWO-VIEW/ONE-VIEW | evidence (file+region, row verbatim, file:line) | outcome wanted |
RUBRIC: one line per band with its arithmetic
TOP-3 MANDATED FIXES:
1. <asset, effect, band or file>; now: <seen or measured, and where>; want: <what must differ, on which measure, by how much>; proof: <the shot, number or command to hand back>
2. <the same four parts>
3. <the same four parts>
OVERDONE: what has gone too far, or "nothing"
SHOT REQUESTS: at most 4, each a view, focus, zoom and tick (or a command), and what it would settle
COULD NOT JUDGE: what no shot or command can settle
```
- **A script reads two things:** the score on the first line, which must be the VERDICT line with a number from 0 to 100, and the numbered lines under `TOP-3 MANDATED FIXES`. It takes capitals and a colon at the start of a line as the next heading. So the top 3 is exactly three lines, numbered `1.` `2.` `3.`, each on one line, with nothing between them.
- **Each fix line stands alone.** A relay fix leg is handed those three lines and nothing else of the paper. The first 160 characters of fix 1 go into the relay's own lessons table: lead with what and now.
- **Fewer than three confirmed findings:** the remaining places are the shots that would confirm the strongest ONE-VIEW findings.
- **A FAIL on a hard check caps the total at 49.**
- `NO MATERIAL IMPROVEMENTS REMAIN` is claimed against every band, and never in round 1.
- One line per finding: the paper stays under 7 KB.

## 3. Rubrics per role (bands out of 100; the visual share is capped, because vision scores saturate)
| Role | Hard checks (a FAIL caps at 49) | Bands |
|---|---|---|
| destruction-vfx | readability, T1 budget, `books.Ready`, guard tests, fidelity never drops as the camera zooms in (T3 ≥ T2 ≥ T1 ≥ O120 ≥ O240 ≥ Far) | coverage of events 30 · look per band vs spec 30 (visual ≤ 15) · budget 20 · repeatability 10 · cost 10 |
| vehicle | GaitTests green, `pose_error_m` read | the docs/20 lines at the current zoom level, mapped to 100 |
| character | clipcheck thresholds, guard tests | situation-matrix pass rate 40 · per-unit specificity 20 · look 15 · repeatability 15 · cost 10 |
| env | asset-scale audit, readability | the env-scoreboard's ten criteria at the tier, mapped to 100 |
| balance | ≥ 8 seeds, same seeds per variant | distribution evidence 40 · counter-play 20 · campaign envelopes 20 · extensibility 10 · clarity 10 |
| optimizer | AOSA rules 1-4 (same battle, bands, no regressions, floor) | gain past band 40 · image class honest 20 · sheet completeness 20 · cost 20 |
| master | gate green on the exact tree, lane rules | un-entangled (scorecard, no new asmdef edges) 40 · compact 20 · docs in step 20 · owner's words kept verbatim 20 |
| gym | `GymCatalogueTests` green (no enum value without an entry), run folder outside the checkout, 0 unexplained flags | coverage of catalogue 30 · every entry triggers what it claims (events counted) 30 · sheets readable per band 15 · run time and size 15 · retention works 10 |
| lowpoly | the silhouette IoU ≥ 0.85 against the level above, no empty part, pivots unchanged, original untouched | triangle/draw gain 30 · pop at the swap band (critic, blind) 30 · every band sheet 20 · budget met 10 · cost 10 |
| housekeeping | never deletes an unrecognised, tracked or newest-2 item (dry run first) | space recovered 40 · classification correct on a hand sample 40 · report clarity 20 |
| plan / skill / code change | every cited path and symbol exists at that commit (grep each one), owner's words quoted verbatim where the work quotes him | fidelity to the owner's brief 30 · technical correctness against the repo 30 · actionable 20: an agent can run it cold, each finish line is something a command or a count can check, a ban says what to do instead · compact 20: each line changes what an agent would do, each fact has one home and the rest point to it |

**Finding your row.** A role named `<x>-simulator` uses row `<x>`. A row whose bands are written out above is used
even when the folder cannot confirm its hard checks: a band with no evidence scores 0, and the checks you could not
confirm go under COULD NOT JUDGE. Only when no row fits (the design, concept, UX and UI roles among them), or the
row gives its bands by pointing at a file your folder does not hold (`vehicle`, `env`): the hard checks are the
checks `stage.json` names, the bands are every output the stage asks for is shown 40 · look against the stage's own
reference 30 (visual ≤ 15) · repeatability 15 · cost 15, and the VERDICT sentence says `no row`. With no reference
in the folder, look is judged against the stage's notes. **Cost is the one band that may be left out,** and only
here: when the folder holds no cost figure and `stage.json` names no cost check, score the other three out of 85,
scale to 100, show that sum in RUBRIC and name cost under COULD NOT JUDGE; it is then neither a finding nor a fix.

**Text work** (the last row):
- Evidence is `file:line` at the commit you were given (the working tree when it is uncommitted), or a command and its output. Say which claims you ran and which you only read; only what you ran is MEASURED.
- When the change edits a rule or rubric you score by, score on the version before the change, then judge the new text on its own.
- A fix line gives the file and the replacement wording.
- More than one commit or lane: a score for each, then the total, weighted by lines changed.

## 4. Angles (rotate them between rounds, so a second round does not reread with the same eyes)
1. **Fidelity:** does it do exactly what the owner asked, in the owner's words, no more and no less?
2. **Technical truth:** every path, symbol, flag and number checked against the repo at the current commit.
3. **Cold start:** a fresh agent with only this page and the repo: where does it get stuck or do damage?
4. **Adversarial:** how could following it break the build, the lanes, determinism, the disk or the shared GPU?
5. **Player's eye:** at T3, T1 and far, would a player see the consequence? Is the fun visible?
6. **Cost:** time, disk, git history, draw calls: what does it cost, and is that measured?

## 5. Running the loop (not for a relay leg)
`/improve-loop <role> <item> --target 85 --rounds 3` is a request for the six steps in
`references/critic-and-scoreboards.md`, "Running the loop by hand"; no script has that name. In short: noise floor
first, a blind critic in the foreground, every top-3 line answered or rebutted, **keep the best round, not the
last**, ask the owner after 2 rounds with no gain. For pipeline jobs the relay runs the rounds by script.

## Learning loop
The same for every role: `../pipeline/SKILL.md`, "The learning loop". For the critic, step 0 is the role's lines in
the board's `lessons.md` (the charter's last point).

Sources: the last rubric row's finish-line, one-home and say-what-to-do points rework ideas from
`writing-for-agents` in github.com/mattpocock/skills at f3fc5632f401 (MIT).
