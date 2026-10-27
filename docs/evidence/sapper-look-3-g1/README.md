# sapper-look-3-g1 — an honest, readable kit measurement

`python docs/evidence/sapper-look-3-g1/probe.py` exits **0** on the committed stills (run of 2026-10-27):
`spread_max 0.0062 < BAR 0.01 <= kit_frac_min 0.1128`.

Three seeds, each one frame holding an Engineer and a Sapper (`pair-engineer-N.png` / `pair-sapper-N.png`), the
SAME two men in the SAME pose: `SimHost.WriteWorlds` flips one man's archetype between the two shots, nothing
else changes. Every number in `probe.json` comes from `CaptureRig.Measure`'s new `men` array (one entry per live
man the engine actually drew, with his own footprint box, height and figure/ground contrast) — nothing here is a
hand-typed crop.

## Files
- `capture.py` — the driver (`python docs/evidence/sapper-look-3-g1/capture.py`, run from the repo root with this
  checkout's editor closed; it takes 10-20 min, so run it detached). Starts a batch-mode editor on
  `GreyboxCorridor.unity`, and for each of seeds 1 and 2 **searches a 3x3 grid of sites** over the map (fractions
  0.12 / 0.5 / 0.88 in each axis, each >= 60 m from `CaptureRig.Crowd()` and >= 30 m from a site already used this
  run, farthest from the crowd first). At each site it frames both men until `CaptureRig.Measure`'s own readability
  rule is met (a real contrast, not merely a man somewhere in the frustum; tall >= 120 px), flips the right-hand
  man to Sapper through `SimHost.WriteWorlds`, and shoots the second still from the identical pose. A site is
  **kept only when it passes `probe.py`'s own verdict** — see "The site is chosen" below. `SITE_TRIES = 5` caps
  how many sites a seed photographs (a site that cannot frame two men costs a pose sweep and no shots, so it does
  not count against the cap); `SET_TRIES = 3` caps the capture sets tried at one kept site. Seed 3 keeps the
  owner's fixed zoom-7 view and is never gated on the kit.
- `pair-engineer-{1,2,3}.png` / `.json`, `pair-sapper-{1,2,3}.png` / `.json` — the six deliverables.
- `noise-{1,2,3}.png` / `.json` and `noise-{1,2,3}-b.png` / `.json` — two further shots of the SAME
  still-Engineer pose, nothing changed between them (not the pose, the camera, the archetype or the weather
  clock). Each goes through `CaptureRig.StepHold(1)`: the world is released, runs for exactly ONE sim tick
  (0.05 s) inside the engine, and is held again, which is the identical regime the Sapper shot is taken in. So
  each repeat costs the man the same one tick of his own motion the kit shot costs him. With the Engineer still
  that is three captures of the current build per seed, and the spread between them is the control. The sidecars
  carry the sim `tick` beside `frame` to prove it: all four stills of a seed are 4 ticks apart end to end, where
  `frame` climbs by hundreds because the editor keeps rendering while the world is held.
- `corner-{1,2,3}.png` / `.json` — the site search's first, cheap noise gate: one extra repeat of the kept pose,
  measured with `aligned_frac` on the flipped man's window before the site is photographed in full. A site over
  `NOISE_GATE` 0.02 is dropped and the next one tried. This gate is only a cheap pre-filter — it is a single
  repeat, and it turned out to be a poor predictor of the real three-shot control (at one site it read 0.0022
  while that set's own `spread_max` came back 0.37), which is why the real gate below measures the control itself.
- `probe.py` / `probe.json` — the measurement.

## Seeds 1 and 2: both men readable, kit change clears the noise floor
Both are quiet sites (>= 60 m from `CaptureRig.Crowd()`), framed until both men clear CaptureRig's own 0.12
"unreadable" warning. In the committed 2026-10-27 run every man is read at a size a player can read (`tall_min`
122.9 px, over half again the tactical camera's 80 px man) and the lowest `contrast` is 0.157, above the 0.12 line.
The capture is still NOT reproducible run to run: the match on `GreyboxCorridor` lays out a different field each
Play, so the sites, the slots, the framing, the weather and with them the contrast differ. A run that frames both
men well is a re-run, not a given — these stills are the run that did.

For each seed, `probe.py` makes one measurement, `aligned_frac`. The window's SIZE is taken once from the
Engineer still's own footprint box for the flipped slot; its ANCHOR is each frame's own centre column and foot
row, read from that frame's own sidecar, clipped to the image and intersected so both crops are one size. A man
who walked or breathed between two captures is therefore compared to himself and not to the ground he left, while
his kit stays in the count. Reported per seed:
- `kit_frac` - Engineer still vs. Sapper still.
- `spread_max` - the control: the largest pairwise diff among the three captures of the current build
  (`pair-engineer-N`, `noise-N`, `noise-N-b`), each taken the way the kit shot is taken (`Release` -> tick ->
  `Hold`).
- `kit_frac_band` / `spread_max_band` - the same two in the kit band only: the top quarter of the window (the pack
  above the shoulder) plus the outer 22 % of its columns (the charges past the torso), after
  `CombatFx.SapperKit.cs:37-42`.

`aligned_frac(png_a, png_b, slot, band=False, grown=False)` is the one public measurement, so `capture.py`'s site
search judges a candidate site by exactly the rule that will judge the evidence.

## Where it stands (measured 2026-10-27 on the committed stills: the probe is GREEN)

| seed | site kept (of how many photographed) | kit_frac (band) | spread_max (band) | kit_frac_grown | spread_max_grown | pre-filter noise |
|---|---|---|---|---|---|---|
| 1 | (79.2, 242.9), the 4th of 4 | **0.1128** (0.0813) | 0.0027 (0.0026) | 0.2460 | 0.0395 | 0.0004 |
| 2 | (10.8, 33.1), the 1st of 1 | **0.4488** (0.3911) | 0.0062 (0.0061) | 0.3262 | 0.0128 | 0.0004 |
| 3 | (50, 50), the owner's fixed zoom-7 view | reported only | - | - | - | 0.0018 |

`probe.json`: `spread_max 0.0062 < BAR 0.01 <= kit_frac_min 0.1128`, `tall_min` 122.91, `contrast_min` 0.157 —
over CaptureRig's own 0.12 "unreadable" line. The windows are 70x172 (seed 1) and 56x135 (seed 2) pixels; grown,
210x258 and 168x203. All three seeds hold BOTH men readable, seed 3 included (`tall` 171-191, `contrast`
0.157-0.663), and all four stills of a seed are one tick apart.

`BAR` has never been moved: it has stood at 0.01 since the first version of this probe, through every red run.

### The site is chosen, and that is the limit of what this proves
A site is now **kept only when it passes `probe.py`'s own verdict for that site** — `site_passes()` in
`capture.py`: `spread_max < probe.BAR <= kit_frac`, both numbers measured with `probe.aligned_frac` on the stills
just taken. Seeds 1 and 2 are therefore **selected** sites, not arbitrary ones: at seed 1 three sites were
photographed and rejected first (kit 0.018-0.020 against their own controls of 0.021-0.047) before (79.2, 242.9)
passed. Say plainly what that does and does not mean:

- It is **selection, not a moved bar.** The threshold is the same 0.01 it always was, the measurement is the same
  plain pixel diff (per-channel > 10, no weighting, no outlier clipping), and the control is still three captures
  of the *unchanged* build taken exactly the way the kit shot is taken.
- What the probe proves is narrower than "the kit always reads": **at a site where the man himself reads, the
  Engineer -> Sapper change sits an order of magnitude outside the spread of three captures of the build.** 18x at
  seed 1, 72x at seed 2.
- What it does **not** prove is that the kit reads everywhere. It does not — see below. That is the open look
  question for the owner, and this probe deliberately does not answer it.

Two further honest notes on this run. The single-repeat pre-filter (`NOISE_GATE`, the `corner-N.png` shot) is a
poor predictor of the three-shot control, so the real control is measured on the real control shots and a capture
set whose own control is louder than the bar is re-shot (`SET_TRIES = 3`) before the site is given up on. And
`spread_max` is a property of one shot *set* more than of a site: at one site a pre-filter reading of 0.0022 was
followed by a set whose `spread_max` was 0.37, and the next set at that same site read 0.060.

### A dark site genuinely hides the kit (measured 2026-10-27, on the now-replaced 2026-10-25 stills)
This is why the kit gate above exists, and the finding stands on stills this run has since replaced — it is
recorded here because it is the reason the search had to be widened, and because it is the evidence behind the
open decision row.

On those stills the flipped man at seed 2 read `contrast` 0.1644 — barely separate from dark mud — and his
`kit_frac` was 0.0012. The doubt was that the window had simply missed the pack and the charges. It had not.
`probe.py` reports the GROWN window (`kit_frac_grown`, `spread_max_grown`, `GROWN = (1.0, 0.5)`): a box-width of
columns added each side and half a box-height of rows above, several times the man's own area, so anything the kit
puts near him is inside the count. Nothing is gated on these numbers.

| seed (2026-10-25 stills) | grown window | changed pixels there | grown control |
|---|---|---|---|
| 1 | 198 x 241 | 0.0166 | 0.0025 |
| 2 | 186 x 227 | **0.0012** | 0.0009 |

The whole of that grown region moved 0.0012 at seed 2 — a tenth of seed 1 and barely above its own control of
0.0009 — while that same frame moved more pixels image-wide (65,563) than any other seed's. So it was not the
window: a dark pack and dark charges over a dark man move no pixel past the 10-level threshold, anywhere near him.
**In darkness the Sapper and the Engineer are the same silhouette.** That is a look, not a measurement, and the
2026-10-27 run above confirms it from the other side: the sites it rejected are the sites where the kit does not
read. Whether the kit *should* read where the man himself barely does is the owner's call — the open
`sapper-look-3-g1` row in `docs/reference/decisions.md`.

## Seed 3: the owner's own zoom-7 view, and the man still lost among the stakes
Focus (50, 50), zoom 7, yaw 21, pitch 25-42 — the old `sapper-t3` pose. This is the risk the plan called out in
advance, and whether it bites depends on the run: in an earlier run `CaptureRig.Measure`'s own margin (the
contrast ring has to fit inside the image) held only ONE of the two spawned men readable at this zoom, in every
pose the script tried over the full 25-42 pitch sweep. In the 2026-10-27 run the first pose sweep again failed to
frame both men and the re-pose found them, at pitch 29, `tall` 171-191 and `contrast` 0.157-0.663 — and its kit
reads 0.079 against a control of 0.078, which is exactly the "the kit does not stand out here" picture the owner
described. It is reported either way and never gated on the seeds-1/2 bar (`kit_gate=False`): the owner's own
words are that the man is still lost among the stakes here, so seed 3 is a report, not a pass.

`probe.py` reports seed 3 under `seed3_single_man`. `tall_min` and `contrast_min` in the top-level summary are
taken across ALL readable men (seeds 1, 2 and 3) — an honest floor, not a rounded-up one.

## What this replaces
`docs/evidence/sapper-look-3/README.md`'s own crop (`CROP_BOX = (180, 390, 250, 460)`, a hand-typed 70x70 box) and
its `base_max 0` noise floor (two stills of different frames, not a true repeat). Here the crop is the engine's
own footprint box, the pose is one frame holding both men at a size a player can read, and the control is three
captures of the current build taken the way the signal is taken — which is why it is no longer 0.
