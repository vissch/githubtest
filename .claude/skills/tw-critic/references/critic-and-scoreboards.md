> Snapshot read at integration 558c667 (2026-09-28). file:line citations drift: re-grep the symbol before editing. The code wins over this page.

Path keys: R = repo root, P = trench-warfare-3d/Assets/_Project, T = trench-warfare-3d/Tools, D = docs.

## D. HARD CRITIC

### The brief (`D\reference\aosa\agents\critic.md`, 41 lines)
- **Default:** "By default you withhold marks. A 10 means 'I looked for the fault and could not find it'. Any score above 7 needs evidence that you can point to in the image … A line you have no image evidence for is scored `-`, never guessed." (:3-5)
- **Input** (:7-15):
  - the T1, T2 and T3 stills, contact sheets and the bench `shot=`, each with its `.json` sidecar (luma percentiles, blown and black fractions, men in frame, figure/ground contrast, pose error);
  - the "before" captures for an A/B;
  - the references `docs/reference/battlefield-northstar.jpeg`, `battlefield-night.jpeg`, `battlefield-night-game.jpeg` and any `runs/<cycle>/refs/*.png`;
  - `env-scoreboard.md` (the ten criteria), `JUICE.md`, and the last ten `owner_verdict`s ("Weight them above your own taste").
- **INVALID capture rules** (:17-22): `pose_error_m` < 0.5; the weather pinned so A and B share the sky; `blown_frac` < 0.02. "A bad capture is reported as INVALID with its reason, and gets no scores."
- **Method** (:23-30):
  - Score every visible env criterion and moment, each with a file and pixel region.
  - For an A/B, say better, same or worse per criterion, and whether A and B are distinguishable at all (that decides the "indistinguishable" class).
  - "Readability, always: … Any drop is a veto."
  - A reference is a direction: name differences in value, hue, gloss, edge, size and timing, never "make it more like the reference".
- **Output format** (:32-41):
```
CAPTURES: valid|INVALID (<which, why>)
READABILITY: holds|DROPS (<what, where>)
SCORES (tier, biome):
| # | criterion or moment | before | after | evidence (file, region) |
AB: distinguishable yes|no; better on <#>, worse on <#>
DELTAS (concrete, max 5):
- <what to change, measured in the image …>
```
- **Operating lessons:**
  - Run critics in the foreground (LESSONS.md:110-114).
  - Judge readability blind against the current default, on the exact set that ships (README rule 6; LESSONS.md:123-141).
  - Count events from `aosa.py shots`, not by eye (LESSONS.md:9).
  - Measure figure/ground (`contrast_median`, `contrast_p10`, man-vs-man) before critiquing a shared-look shader (LESSONS.md:129-135).
  - Check that the evidence can contain the effect: tick range, extreme knob (LESSONS.md:143-159).

### Scoreboards
**`D\20-rig-scoreboard.md`** (the walkers)
- **Rules** (:1-17):
  - Each line is scored 0-10 each cycle.
  - "The camera starts far out and only comes closer when every line on the board is **8 or better**". A score is valid only at its zoom.
  - A fix is applied only if the gait tests stay green; nothing is committed ("the owner commits").
  - "A line nobody has rendered evidence for scores 0, not 'unknown'" (note the difference: critic.md uses `-`).
- **Zoom levels** (:21-35): 1 Tactical 14× height at 35°; 2 Engagement 7× at 22°; 3 Close 3.5× at 14°; 4 Macro 1.8× at 9°. The lens is chosen so the machine fills 72% of the frame.
- **Board** (:39-86): Walk W1-W6, Terrain T1-T5, Damage D1-D6, Guns G1-G5.
- **Standing constraints** (:177-185).
- The closing summary is at :1851+ (51 cycles, nothing committed).

**`D\reference\env-scoreboard.md`** (the environment)
- The zoom ladder T1, T2, T3 (:8-18).
- **The bar** (:20-21): "every line is at least **8** and the tier totals at least **85 / 100**".
- Three biome boards: NightMud, Winter, Coast. A change made for one must be checked against the other two (:23-25).
- **Ten criteria** (:27-40): 1 Silhouette and read, 2 Ground surface, 3 Liquid, 4 Trench kit and structures, 5 Props, clutter and falloff, 6 The men, 7 Light and shadow, 8 Colour and depth, 9 Effects and motion, 10 Cohesion.
- **Standing constraints** (:42-62): readability never drops; the budget never rises (`FrameBudget.DrawCalls` and `Vertices` at the standard view; T2 and T3 detail behind `_TWClose` / `SceneHooks.CloseUp`); sim untouched; "Measured, not asserted".
- Board state: every line is still `-` (:74-120).

**`D\reference\visual-score.md`** (the older day and night log)
- Eight criteria scored 0-10 against the northstar (:8-17). 0 = nothing of the target, 5 = the same idea, 8 = the same style, 10 = indistinguishable.
- "A round that lowers the total, or lowers line 8 [Readability] at all, is reverted" (:19-20).
- Round 14: render-to-texture captures bypass post-processing (:40).

**`D\reference\nav-eval.md`** (testing the docs)
- Method (:7-19):
  1. held-out commits written as user requests;
  2. the answer key out of the repo;
  3. a detached worktree, isolation by instruction;
  4. a control run with the docs deleted;
  5. blind grading on recall and precision, tests, and lane and seam rules, with the spread reported.
- Prompt template :21-30. Results :46-52.

**`workflow.md`** (:187-213)
- "Do not judge a picture by eye first. Read the numbers, then look." (:189)
- Use `luma_mean` / `luma_p95` from the CaptureRig JSON.
- Capture "before" on the old code with the same pose and `CaptureRig.Hold()`, and the unchanged build twice to know the noise.
- `shotstats.py` uses 0-255 Rec.601; CaptureRig uses 0-1 Rec.709 (blown above 0.90). "Never compare a number from one with a number from the other" (:206-208).
- `contrast_median` 0.4 means findable and 0.1 means mud (:210-213).
- Repeatable captures: `stepabs`, a seeded Random, `shot_tick=N shot_hud=0` (:262-268).
