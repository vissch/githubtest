# C104 split: blind critic for the tracer shape (fx.tracerShape) under the new smoke order

All runs come from one build: release player, scenario barrage, ShelledForest night, tick 140, 64 frames, `--no-hud`.
`<label>-1.png` is f0, then `<label>-1.fN.png`.

| run | knobs | tracers |
|---|---|---|
| s0 | none (today's default) | old order (over the smoke), old wide halo |
| t1 | fx.tracerInSmoke=1, fx.tracerShape=0 | new order (under the smoke), old wide halo |
| t2 | fx.tracerInSmoke=1, fx.tracerShape=0.5 | new order, halfway shape |
| s3 | fx.tracerInSmoke=1 (shape 1) | new order, full slim shape |

Sanity checks: s0 matches b0, and s3 matches b3, with changed_frac 0 at f0, f15 and f40.

## Where the variants differ from s0

A pixel counts as changed when any channel moves by more than 8. Every frame differs, and the changes are nested:
t1 is always smaller than t2, and t2 smaller than s3.

| frames | t1 | t2 | s3 | notes |
|---|---|---|---|---|
| f0-f5 | 0.016-0.019 | 0.022-0.033 | 0.025-0.040 | the opening volleys |
| f10-f17 (peak f15) | 0.025-0.040 | 0.037-0.057 | 0.043-0.066 | the centre fans (300-1470, 180-1080) |
| f30-f41 | 0.002-0.008 | 0.004-0.010 | 0.004-0.011 | a lull |
| f50-f56 | 0.017-0.032 | 0.026-0.037 | 0.030-0.040 | the late volley |

Checked by eye on the f12 and f53 crops:
- **t1** keeps the wide green and red beams, but the near black smoke cloud now cuts them off. They no longer cross
  the smoke.
- **t2** has a thinner core with a narrow coloured glow.
- **s3** is a bare white needle with a small coloured head.

## Blind setup

There were two general-purpose critics, told nothing about knobs, cards or the hypothesis. Each got 9 matched pairs
against s0: 3 frames x 3 variants, 18 pairs in all, 6 per variant. Each pair had both full frames plus a side-by-side
crop. X and Y were drawn at random per pair (seed 1041), and each critic was told the draw was independent. Each
image was scored 0-10 on barrage weight, men readability (naming the trench) and tracers read as bullets in depth.
Each critic also ranked the four runs on three 4-way sheets (f10, f26, f50), with the letters shuffled per sheet.

Both critics judged the men on the left trench (about x 0-600, y 120-1080).

## Key and scores (weight, men, tracers; old = s0, new = the variant)

| critic, pair | variant | frame | crop (x0, y0, x1, y1) | new | old w m t | new w m t |
|---|---|---|---|---|---|---|
| A 5 | t1 | 3 | 250, 150, 1400, 700 | Y | 6 3 2 | 5 3 4 |
| B 2 | t1 | 12 | 300, 250, 1400, 1000 | Y | 7 4 3 | 6 4 6 |
| A 7 | t1 | 15 | 650, 250, 1500, 1000 | X | 6 4 2 | 6 4 4 |
| B 7 | t1 | 24 | 350, 250, 1350, 1080 | Y | 6 3 3 | 6 3 5 |
| A 6 | t1 | 44 | 750, 300, 1500, 1080 | Y | 6 4 2 | 6 4 4 |
| B 1 | t1 | 53 | 550, 280, 1300, 950 | Y | 6 4 3 | 6 4 5 |
| A 4 | t2 | 3 | 250, 150, 1400, 700 | X | 6 3 2 | 5 3 5 |
| B 8 | t2 | 12 | 300, 250, 1400, 1000 | X | 7 4 3 | 6 4 6 |
| A 3 | t2 | 15 | 650, 250, 1500, 1000 | X | 6 4 2 | 6 4 6 |
| B 6 | t2 | 24 | 350, 250, 1350, 1080 | X | 6 3 3 | 5 3 6 |
| A 9 | t2 | 44 | 750, 300, 1500, 1080 | Y | 6 4 2 | 5 4 6 |
| B 5 | t2 | 53 | 550, 280, 1300, 950 | Y | 6 4 3 | 5 4 6 |
| A 8 | s3 | 3 | 250, 150, 1400, 700 | X | 6 3 2 | 5 3 5 |
| B 3 | s3 | 12 | 300, 250, 1400, 1000 | X | 7 4 3 | 6 4 6 |
| A 1 | s3 | 15 | 650, 250, 1500, 1000 | X | 6 4 2 | 6 4 5 |
| B 4 | s3 | 24 | 350, 250, 1350, 1080 | Y | 6 3 3 | 5 3 6 |
| A 2 | s3 | 44 | 750, 300, 1500, 1080 | Y | 6 4 2 | 5 4 5 |
| B 9 | s3 | 53 | 550, 280, 1300, 950 | X | 6 4 3 | 5 4 6 |

### Mean change against s0 (6 pairs each)

| variant | weight | men | tracers | weight down in | men down in |
|---|---|---|---|---|---|
| t1 (shape 0) | **-0.33** | 0.00 | +2.17 | 2 of 6 | 0 |
| t2 (shape 0.5) | -0.83 | 0.00 | **+3.33** | 5 of 6 | 0 |
| s3 (shape 1) | -0.83 | 0.00 | +3.00 | 5 of 6 | 0 |

### 4-way rankings (2 critics x 3 sheets; 1 = best)

| sheet | letters A B C D | weight, best to worst | tracers, best to worst |
|---|---|---|---|
| A f10 | s0 s3 t1 t2 | s0 t1 t2 s3 | t2 s3 t1 s0 |
| A f26 | t2 s3 t1 s0 | s0 t1 t2 s3 | t2 s3 t1 s0 |
| A f50 | t2 t1 s0 s3 | s0 t1 t2 s3 | t2 t1 s3 s0 |
| B f10 | s0 s3 t2 t1 | s0 t1 t2 s3 | t2 s3 t1 s0 |
| B f26 | s0 s3 t2 t1 | s0 t1 t2 s3 | t2 s3 t1 s0 |
| B f50 | t2 s0 t1 s3 | s0 t1 t2 s3 | t2 s3 t1 s0 |

The mean ranks:
- weight: s0 1.0, t1 2.0, t2 3.0, s3 4.0 (the same order in all 6 sheets)
- tracers: t2 1.0, s3 2.17, t1 2.83, s0 4.0

### Critic notes (condensed)

- Both critics named whether a tracer is drawn over the near smoke or hidden by it as the biggest depth cue. When a
  tracer is drawn over the smoke it reads as a flat sticker (tracers 2-3). When the smoke hides it, the tracer sits
  in the scene (+2 or more).
- Width is the trade-off. The wide halo feels busiest and heaviest (about +1 weight) but reads as lasers or
  lightsabers. The thin core reads as bullets but thins out the firefight.
- Both critics picked the thin bright core with a narrow coloured glow (t2) as the best tracer. The bare white needle
  (s3) loses the green/red colour, is faint and scratchy at 1080p, and ranked last for weight.
- The men never changed within a pair. Both critics scored them 3-4, as a dense blob of helmets, partly under smoke.
- For the old wide halo with the new order (t1), critic A said it still looks beam-like but better because the cloud
  cuts it off. Critic B said the same: the tracers stop at the smoke edge but keep a wide glowing band.

## Decision (rule 6: best tracers whose weight drop is at most 0.5 and whose men never drop)

- **t1 qualifies:** weight -0.33, men 0 in every pair, tracers +2.17.
- **t2 and s3 fail on weight** (-0.83 each, down in 5 of 6 pairs), although t2 has the best tracers of all (+3.33,
  and ranked first in 6 of 6 sheets).

**Default change pending: fx.tracerInSmoke=1, fx.tracerShape=0.** This is the under-smoke order with the old wide
halo. Most of the depth gain comes from the draw order alone.

To take the halfway shape (t2) as well, it needs about +0.5 of barrage weight back. The critics' notes say the lost
weight is glow area and colour, not the tracer count. A candidate is to raise the halo brightness or the saturation
of the coloured glow at shape 0.5, then rerun the matched pairs against t1. The full slim shape (s3) also loses its
colour, so it should not be the target.
