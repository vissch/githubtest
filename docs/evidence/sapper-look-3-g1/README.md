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

For each seed, `probe.py` reads the RIGHT man's own footprint box from the sidecar and measures what fraction of
that box's pixels changed (per-channel, threshold 10):
- `kit_frac` — Engineer still vs. Sapper still, same pose.
- the control — the spread between the three Engineer captures of the same pose, measured in the same box.

## The control swamps the signal (measured 2026-10-25, and the reason this probe is red)
The 0.0 floor the earlier version of this page reported was not a floor at all: both its captures sat inside ONE
unbroken `CaptureRig.Hold`, so they were pixel-identical by construction — the same empty zero the owner threw
out, and an easier control than the thing it judges. Taken the way the signal is taken (a `Release` -> tick ->
`Hold` per repeat), the control is **larger than the kit change**:

| seed | kit_frac (Engineer vs Sapper) | spread_max (3 Engineer captures) |
|---|---|---|
| 1 | 0.000 | 0.613 |
| 2 | 0.057 | 0.111 |
| 3 | 0.126 | 0.107 |

The cause is the man himself: a Repair-archetype Engineer walks on his own orders, and in the one tick each
repeat costs him he translates far enough inside his own footprint box that the whole-box diff measures his
motion, not his kit. `probe.py` is therefore red on this evidence, and `BAR` has NOT been moved to make it green.
The next step is the plan's own fallback: crop both numbers to the kit band (`CombatFx.SapperKit.cs:37-42` — the
top quarter of the box, where the pack sits above the shoulder, plus the outer ~22 % of the columns, where the
charges sit past the torso), where the kit is and the walking legs are not, and report whole-box and kit-band
side by side.

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
