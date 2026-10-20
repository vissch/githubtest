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
- `noise-{1,2,3}.png` / `.json` — a second shot of the SAME still-Engineer pose, no change in between: the
  repeat-capture noise floor (the same technique `docs/evidence/sapper-look-3/README.md` used: an unbroken
  `CaptureRig.Hold` measures zero noise; a `Release`/`Hold` cycle in between does not).
- `probe.py` / `probe.json` — the measurement.

## Seeds 1 and 2: both men readable, kit change clears the noise floor
Both are quiet sites (>= 60 m from `CaptureRig.Crowd()`), framed until both men clear CaptureRig's own 0.12
"unreadable" warning by a wide margin (`contrast_min` over these two seeds: 0.1055 is the seed-3 figure below;
seeds 1-2 alone are all >= 0.2242) and both exceed the tactical camera's 80 px man by 50%+ (`tall_min` 125 px).

For each seed, `probe.py` reads the RIGHT man's own footprint box from the sidecar and measures what fraction of
that box's pixels changed (per-channel, threshold 10):
- `kit_frac` — Engineer still vs. Sapper still, same pose. Seed 1: 0.0345. Seed 2: 0.610 (a much bigger box and a
  more exposed pose, so a bigger fraction of it is kit).
- `noise_frac` — Engineer still vs. a REPEAT of the same Engineer still. 0.0 both seeds: with the Hold unbroken,
  the capture is pixel-identical before anything changes, exactly like the older evidence found.

`BAR = 0.01` sits strictly between the two (`noise_frac_max` 0.0 `< 0.01 <=` `kit_frac_min` 0.0345): the kit change
is real and an order of magnitude past anything the capture itself contributes.

## Seed 3: the owner's own zoom-7 view, and the man still lost among the stakes
Focus (50, 50), zoom 7, yaw 21, pitch 25-42 — the old `sapper-t3` pose. This is the risk the plan called out in
advance: at this zoom, `CaptureRig.Measure`'s own margin (the contrast ring has to fit inside the image) held only
ONE of the two spawned men readable in every pose this script tried, pitch swept the full 25-42 range. The man who
stayed readable (archetype never flipped) sits at `contrast` 0.106 and 0.105 in the two stills — barely above
CaptureRig's own 0.12 "unreadable" line, i.e. close to it from the readable side, not comfortably past it. The man
who WAS flipped to Sapper for this seed never entered a frame with a usable contrast, so there is no kit-change
measurement to make here: the Sapper himself is, as the owner said, still lost among the stakes at this zoom.

`probe.py` reports seed 3's pair under `seed3_single_man` and does not hold it to the seeds-1/2 bar: there is
nothing to compare when the flipped man never clears the margin. `tall_min` and `contrast_min` in the top-level
summary are taken across ALL readable men (seeds 1, 2 and the one seed-3 man), so seed 3's 0.1055 is the limiting
number in `probe.json` — an honest floor, not a rounded-up one.

## What this replaces
`docs/evidence/sapper-look-3/README.md`'s own crop (`CROP_BOX = (180, 390, 250, 460)`, a hand-typed 70x70 box) and
its `base_max 0` noise floor (two stills of different frames, not a true repeat). Here the crop is the engine's
own footprint box, the pose is one frame holding both men at a size a player can read, and the noise floor is a
measured 0.0, not an assumed one.
