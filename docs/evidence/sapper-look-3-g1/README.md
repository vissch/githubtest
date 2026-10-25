# sapper-look-3-g1 — an honest, readable kit measurement

Three seeds, each one frame holding an Engineer and a Sapper (`pair-engineer-N.png` / `pair-sapper-N.png`), the
SAME two men in the SAME pose: `SimHost.WriteWorlds` flips one man's archetype between the two shots, nothing
else changes. Every number in `probe.json` comes from `CaptureRig.Measure`'s new `men` array (one entry per live
man the engine actually drew, with his own footprint box, height and figure/ground contrast) — nothing here is a
hand-typed crop.

## Files
- `capture.py` — the driver (`python docs/evidence/sapper-look-3-g1/capture.py`, run from the repo root with this
  checkout's editor closed). Starts a batch-mode editor on `GreyboxCorridor.unity`, picks two quiet sites plus the
  owner's own zoom-7 view, frames both men until `CaptureRig.Measure`'s own readability rule is met (a real
  contrast, not merely a man somewhere in the frustum; tall >= 120 px), flips the right-hand man to Sapper through
  `SimHost.WriteWorlds`, and shoots the second still from the identical pose.
- `pair-engineer-{1,2,3}.png` / `.json`, `pair-sapper-{1,2,3}.png` / `.json` — the six deliverables.
- `noise-{1,2,3}.png` / `.json` and `noise-{1,2,3}-b.png` / `.json` — two further shots of the SAME
  still-Engineer pose, nothing changed between them (not the pose, the camera, the archetype or the weather
  clock). Each goes through a full `Release` -> 0.25 s wait -> `Hold` -> shot cycle, the identical regime the
  Sapper shot is taken in, so each repeat costs the man the same one tick of his own motion the kit shot costs
  him. With the Engineer still that is three captures of the current build per seed, and the spread between them
  is the control.
- `probe.py` / `probe.json` — the measurement.

## Seeds 1 and 2: both men readable, kit change clears the noise floor
Both are quiet sites (>= 60 m from `CaptureRig.Crowd()`), framed until both men clear CaptureRig's own 0.12
"unreadable" warning. Every man is read at a size a player can read (`tall_min` 126 px in the 2026-10-25 run,
50%+ past the tactical camera's 80 px man), but the capture is NOT reproducible run to run: the match on
`GreyboxCorridor` lays out a different field each Play, so the two quiet sites, the slots and the framing differ,
and with them the contrast. In the 2026-10-25 run the far man of seed 2 came back at `contrast` 0.0136, under
CaptureRig's own 0.12 line, so that check is red too. A run that frames both men well is a re-run, not a given.

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

`aligned_frac(png_a, png_b, slot, band=False)` is the one public measurement, so `capture.py`'s corner search can
judge a candidate site by exactly the rule that will judge the evidence.

## Where it stands (measured 2026-10-25 on the committed stills, and why this probe is still red)
The 0.0 floor an earlier version of this page reported was not a floor at all: both its captures sat inside ONE
unbroken `CaptureRig.Hold`, so they were pixel-identical by construction - the same empty zero the owner threw
out. Taken honestly, and now measured in the aligned window:

| seed | kit_frac (band) | spread_max (band) |
|---|---|---|
| 1 | 0.0040 (0.0070) | 0.0220 (0.0378) |
| 2 | no pair: `pair-sapper-2.png` holds no readable man at all | - |
| 3 | reported only (one man readable at zoom 7) | - |

Aligning the window took the walking man's translation out of the control: the whole-box numbers on this same
evidence were 0.11-0.61. What is left is the released time between two repeat shots. Each repeat is its own
`unity.exe cmd eval` subprocess, so `Release` -> `sleep 0.25` -> `Hold` runs the world and every `_Time`-driven
shader for seconds, not a tick - the animated rain-streak highlight behind the man. `BAR` has NOT been moved to
meet either number: shrinking that window to one tick inside the engine, and giving the corner search its own
noise gate, is the rest of this work.

## Seed 3: the owner's own zoom-7 view, and the man still lost among the stakes
Focus (50, 50), zoom 7, yaw 21, pitch 25-42 — the old `sapper-t3` pose. This is the risk the plan called out in
advance, and whether it bites depends on the run: in an earlier run `CaptureRig.Measure`'s own margin (the
contrast ring has to fit inside the image) held only ONE of the two spawned men readable at this zoom, in every
pose the script tried over the full 25-42 pitch sweep. In the 2026-10-25 run both men came back readable and the
flip was found, at `contrast` 0.139-1.81. It is reported either way and never gated on the seeds-1/2 bar: the
owner's own words are that the man is still lost among the stakes here, so seed 3 is a report, not a pass.

`probe.py` reports seed 3 under `seed3_single_man`. `tall_min` and `contrast_min` in the top-level summary are
taken across ALL readable men (seeds 1, 2 and 3) — an honest floor, not a rounded-up one.

## What this replaces
`docs/evidence/sapper-look-3/README.md`'s own crop (`CROP_BOX = (180, 390, 250, 460)`, a hand-typed 70x70 box) and
its `base_max 0` noise floor (two stills of different frames, not a true repeat). Here the crop is the engine's
own footprint box, the pose is one frame holding both men at a size a player can read, and the control is three
captures of the current build taken the way the signal is taken — which is why it is no longer 0.
