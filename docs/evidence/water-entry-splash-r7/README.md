# water-entry-splash-r7 — the evidence, and what it proves

Harsh critic r1 scored r6's water entry 56/100 on two counts, and this round answers both in pixels:

- **the ring adds nothing** — r6's `ring_only_px` was 0 and the ring's largest luma move 0.0191, just under the 0.02
  threshold, so the ring's own contribution to the landing frame could not be told from nothing;
- **the crown is blue-white on blue water and nearly gone at +0.3 s** — and a count of changed pixels at luma 0.02
  proves *change*, not that a player sees it.

So the crown is now measured as a **value step over the water it stands in**: the p90 luma of the splash region
minus the p90 of the same region, at the same frozen instant, with the splash lifted out. The owner's bars:

| key | bar | what it means |
|---|---|---|
| `ring_only_px` | >= 300 | changed pixels the ring alone accounts for, the **min** of land + 0.05 s and land + 0.3 s |
| `crown_contrast_land` | >= 0.15 | the crown's value step over the water at land + 0.05 s |
| `crown_contrast_post` | >= 0.09 | the same a third of a second after the entry |

## How to read this folder

`probe.py` measures the stills and writes `probe.json`; every number there is measured, never typed. Its own
docstring is the long account: the camera, the window, the crown box, the threshold and why each moment has its own
`-nosplash` reference instead of a plate taken earlier.

```bash
python docs/evidence/water-entry-splash-r7/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
python docs/evidence/water-entry-splash-r7/probe.py --fixed run-4 run-5 run-6   # the fix; exit 0 when it clears
```

- `run-N/` — one capture run, staged by `trench-warfare-3d/AgentScripts/water_entry_fling.cs` and rendered by
  `CaptureRig` in Play in GreyboxCorridor at night, T1 (zoom 30, yaw 21, pitch 25), 1600 x 900, rain 0, both clocks
  pinned, the sim frozen, `pose_error_m` 0. Nine stills with a `.json` sidecar each, plus `timings.txt` (the
  achieved game-time delta of every frozen moment) and `stage.log`. `plate.png`, the ford with no throw at all,
  is kept in `run-1` only; the other runs would only repeat it.
- `land-run-N.png` (baseline) and `land-fixed-run-N.png` (the fix) — a copy of each run's landing frame at the top
  level, so the key frame of every run can be opened without going into a folder.
- `probe.json` — the baseline block (`*_base_max`, `n_base_captures`) and, once the fix is captured, the fixed
  block (the bar keys, `n_captures`, `pass`) and a `runs` block with every single run's own numbers.

## The baseline: the CURRENT build, captured three times before anything changed

The owner's rule for this round. `*_base_max` is the **max** over the three runs, so the old build gets its best
showing; the fixed build's keys are the **min** over its three runs, its worst. Each bar sits above the baseline
spread, so clearing it cannot be capture noise.

| key | run-1 | run-2 | run-3 | `_base_max` | bar |
|---|---|---|---|---|---|
| `ring_only_px` | 0 | 0 | 0 | 0 | 300 |
| `crown_contrast_land` | 0.0320 | 0.0320 | 0.0317 | 0.0320 | 0.15 |
| `crown_contrast_post` | 0.0235 | 0.0239 | 0.0289 | 0.0289 | 0.09 |

The three runs agree to within 0.005, and they reproduce r6's committed stills (0.0335 and 0.0237), so the gap to
the bars — about 4.5x on the crown, everything on the ring — is the effect's, not the capture's. The water's own p90
in the crown box is 0.313: that is the foam the crown has to step above.

## The fix, captured three times

`run-6`, `run-9` and `run-10`, each in its OWN Play session (see below). Every key is the MIN over the three.

| key | run-6 | run-9 | run-10 | taken | `_base_max` | bar |
|---|---|---|---|---|---|---|
| `ring_only_px` | 1292 | 1292 | 1289 | 1289 | 0 | 300 |
| `crown_contrast_land` | 0.2961 | 0.2966 | 0.2958 | 0.2958 | 0.0320 | 0.15 |
| `crown_contrast_post` | 0.2092 | 0.2081 | 0.2080 | 0.2080 | 0.0289 | 0.09 |

`probe.py --fixed run-6 run-9 run-10` exits 0: every bar is cleared with about four times the baseline's best, and
r6's own guards hold too (`noise_px` 2, `ring_px_land` 3128 against a floor of 1000, `ring_px_post` 4135 against
600, the ring's centroid 0.068 m off its mark).

## What the pixels taught this round, and what they overturned

**A value step in the ALBEDO is not a value step on SCREEN at night.** The first fixed build did exactly what the
plan asked — the crest lifted the water's albedo by 0.26, the trough cut it to 0.62 — and the capture came back at
`ring_only_px` 0 with a largest move of 0.0196: the same "the ring adds nothing" the critic wrote. Night water is a
dark albedo times a dim moon times `1 - _TWWaterDim`, so an albedo step of 0.26 arrives as 0.02. The step now goes
on the LIT colour, in `TWRingValueStep`, after the lighting and the dim (`Water_URP.shader`). Same two weights, a
different place in the pipeline, and `ring_only_px` went from 0 to 1289.

**The crown's `Lit = 0.35` made it DARKER, not brighter.** The reasoning was sound — `Flipbook_URP` draws a card as
`_Tint * lerp(ink, lit, _Lit)` and at night `lit` is the moon's dim colour — but the drawing's own ink is the DARK
half of this card, so inking more of it measured `crown_contrast_land` at -0.0187: the crown was darker than the
water it stood in. The `Crown` row keeps its opened window (`Low` 0.22) and no `Lit` override; the value is carried
by the tint `ApplyTints` paints from `WaterEntry.CrownTint`.

**One Play session, one capture run.** `run-7` and `run-8` were shot in the same Play session as `run-6` and
measured `noise_px` ~1030 against `run-6`'s 1. The staging script puts the scene back, but not far enough for a
second capture to repeat the first to the pixel. Each of the three committed fixed runs was staged in a Play
session of its own, and its noise floor is 2 px because of it; the baseline's three measured 2, 1 and 9.
