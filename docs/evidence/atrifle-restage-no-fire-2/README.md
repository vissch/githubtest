# atrifle-restage-no-fire-2

The owner's answer to "The AT rifleman's readability shot: clear the fire, or move the camera in" was
"Re-stage with no fire in the frame" (`docs/reference/decisions.md`, 2026-10-27).

## Staging

Same camera as `atrifle-look-r3-g1` (one lens, the prone AT rifleman and the kneeling Sniper five metres apart),
but all three fire knobs are at 0: no brazier, no small torch, nothing burning in frame.

## Pictures to look at

- `pair-1.png`, `pair-2.png`, `pair-3.png` — both men in one frame, one camera, three live poses each.

## The numbers (from the committed `probe.json`)

- `flames_in_frame 0` in all fifteen sidecars (`*-[123].json`) — no flame anywhere in frame.
- `silhouette_diff: 0.6019` against the 0.6 bar (the worst of the three live-pose readings:
  `silhouette_diff_readings: [0.7715, 0.6019, 0.6701]`).
- The firelit `atrifle-look-r3-g1` set (before the re-stage) read `0.7655` on the same measure.
- `problems: []`, verdict: "the prone AT rifleman reads as his own man".

## Re-check

```
python docs/evidence/atrifle-restage-no-fire-2/probe.py
```

Rewrites `probe.json` from the same committed stills; `git status --short` should stay empty afterwards.

## Why `-2`

The folder keeps its `-2` name because it is the second staging of the `r3-g1` camera, not the first
`atrifle-restage-no-fire` attempt — there was no first folder under that exact name.
