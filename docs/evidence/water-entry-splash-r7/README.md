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
- `land-run-N.png` — a copy of each run's landing frame at the top level, so the key frame of every run can be
  opened without going into a folder.
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
