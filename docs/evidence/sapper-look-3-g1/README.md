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
  clock). Each goes through `CaptureRig.StepHold(1)`: the world is released, runs for exactly ONE sim tick
  (0.05 s) inside the engine, and is held again, which is the identical regime the Sapper shot is taken in. So
  each repeat costs the man the same one tick of his own motion the kit shot costs him. With the Engineer still
  that is three captures of the current build per seed, and the spread between them is the control. The sidecars
  carry the sim `tick` beside `frame` to prove it: all four stills of a seed are 4 ticks apart end to end, where
  `frame` climbs by hundreds because the editor keeps rendering while the world is held.
- `corner-{1,2,3}.png` / `.json` — the corner search's own noise gate: one extra repeat of the kept pose,
  measured with `aligned_frac` on the flipped man's window before the corner is accepted. A corner over 0.02 is
  dropped and the next one tried; if none passes, the quietest tried is re-posed and every corner's number is
  printed.
- `probe.py` / `probe.json` — the measurement.

## Seeds 1 and 2: both men readable, kit change clears the noise floor
Both are quiet sites (>= 60 m from `CaptureRig.Crowd()`), framed until both men clear CaptureRig's own 0.12
"unreadable" warning, and each kept corner also has to clear the search's own noise gate (0.02 on a repeat shot of
the same pose). In the committed 2026-10-25 run every man is read at a size a player can read (`tall_min` 135.7 px,
70 % past the tactical camera's 80 px man) and the lowest `contrast` is 0.1618, above the 0.12 line. The capture is
still NOT reproducible run to run: the match on `GreyboxCorridor` lays out a different field each Play, so the two
quiet sites, the slots and the framing differ, and with them the contrast. A run that frames both men well is a
re-run, not a given — these stills are the run that did.

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
out. The 0.11-0.61 control that replaced it was not a measurement of the build either: it was released time. Each
repeat was its own `unity.exe cmd eval` subprocess, so `Release` -> `sleep 0.25` -> `Hold` ran the world and every
`_Time`-driven shader for SECONDS - the animated rain-streak highlight behind the man. `CaptureRig.StepHold`
bounds that window to one tick inside the engine, and the control falls by 20-100x:

| seed | kit_frac (band) | spread_max (band) | corner noise |
|---|---|---|---|
| 1 | 0.0355 (0.0323) | 0.0066 (0.0063) | 0.00009 |
| 2 | 0.0012 (0.0020) | 0.0000 (0.0000) | 0.0000 |
| 3 | reported only (the owner's zoom-7 view) | - | 0.0006 |

All three seeds now hold BOTH men readable — including seed 3, the zoom-7 view among the stakes where the man was
lost — at `tall` 135-202 px and `contrast` 0.16-1.33, and all four stills of a seed are one tick apart.

The probe is still red, and on a different thing than before: the control no longer swamps the signal (0.0066
against 0.0355 at seed 1), but the kit change itself reads near zero at seed 2 (0.0012), so no single `BAR` sits
above every control and below every `kit_frac`. `BAR` has NOT been moved to meet a number.

### The window is not the reason (measured 2026-10-27 on these same stills)
The one remaining doubt was that the window had simply missed the pack and the charges at seed 2. It had not.
`probe.py` now also reports the GROWN window (`kit_frac_grown`, `spread_max_grown`, `GROWN = (1.0, 0.5)`): a
box-width of columns added each side and half a box-height of rows above, several times the man's own area, so
anything the kit puts near him is inside the count. Nothing is gated on these numbers.

| seed | grown window | changed pixels there | grown control |
|---|---|---|---|
| 1 | 198 x 241 | 0.0166 | 0.0025 |
| 2 | 186 x 227 | **0.0012** | 0.0009 |
| 3 | (reported only) | 0.0217, from a static-box count of the same stills | - |

At seed 2 the whole of that grown region moves 0.0012 — a tenth of seed 1 and barely above its own control —
while the rest of that same frame moved more than any other seed's (65,563 changed pixels image-wide). The flipped
man there reads `contrast` 0.1644: a man barely separate from dark mud. A dark pack and dark charges over a dark
man move no pixel past the 10-level threshold, anywhere near him. So at a dark site the Sapper and the Engineer
are the same silhouette, and that is the look, not the measurement: the window is ruled out as the cause.
Whether the kit should read in darkness where the man himself does not is a look question for the owner — the open
`sapper-look-3-g1` row in `docs/reference/decisions.md`.

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
