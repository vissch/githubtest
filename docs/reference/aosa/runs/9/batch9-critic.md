# Batch 9: blind pair critics for C99 (pair), C103 and C104 against today's defaults

All runs are from one build: release player, scenario barrage, ShelledForest night, T1, `--no-hud`. `<label>-1.png` is
f0, then `<label>-1.fN.png`.

| run | knobs | frames |
|---|---|---|
| b0 | none (today's defaults) | 64 from tick 140 |
| b1 | fx.smokeSoft=0.6, fx.burstGlow=0.5 (the C99 pair) | 64 from tick 140 |
| b2 | fx.columnSoil=1 (C103) | 64 from tick 140 |
| b3 | fx.tracerInSmoke=1 (C104) | 64 from tick 140 |
| b4 | all four | 64 from tick 140 |
| b0t / b4t | none / all four | 32 from tick 180 |

**b0 is the old image.** b0 against 9/c101 has changed_frac 0 at f0, f16 and f40.

## Where each variant differs from its baseline

A pixel counts as changed when any channel moves by more than 8 (`stilldiff`'s threshold). Every frame of every
variant differs. The regions below are full-frame (x0-x1, y0-y1).

| variant | changed_frac | what changed (checked by eye on crops) |
|---|---|---|
| b1 | 0.007-0.039, peak f26 | The smoke is lighter over the ground (luma +10 over the smoke). The burst clouds are dimmer while lit: f15-f27 at the left burst (0-200, 250-560) and the top-right burst (1240-1450, 0-190), and f46-f59 at the f44 top-right impact (1370-1640, 120-390), where luma falls by 5-25. The same split as C99's c99s and c99g. |
| b2 | 0.021-0.034, peak f51 | The orange see-through column arcs are gone everywhere. Over men they are gone, not replaced: at f0 in the top-right trench (1380-1780, 380-800) the old arcs over the helmets vanish (the cap over men). Elsewhere the dark soil column is hard to tell from the dark smoke. The clods are fewer, bigger and clumped: the left burst f12-f49 (0-180, 400-630), the centre burst f50-f63 (700-1020, 740-1020), and the top-right impact f54-f63 (1380-1580, 230-435), where the clods fly over the top-right trench's sandbags and men. |
| b3 | 0.004-0.066, peak f15 | The tracers. The neon halo ribbons become thin white streaks with a short coloured head. Changed pixels are much darker (luma about 115 -> 57). Where smoke lies over a tracer, the tracer is dimmed or hidden. The biggest change is f10-f17 over the centre (620-1470, 330-1080). |
| b4 | 0.036-0.102, peak f15 | The union of b1, b2 and b3. |
| b4t | 0.036-0.059 | The same union later in the barrage. The column over the top-right trench's heap of men and debris at f0-f7 and f17-f27 (1250-1660, 0-390), which is C102's s180 case: the old orange arcs over the men are gone. The smoke is thinner at (1440-1830, 590-930), which has no men in it. |

**Columns over or right in front of men** (the case where C102 failed):
- b0/b2 and b0/b4 f0 at the top-right trench (1380-1780, 380-800), where the old arcs lie over the helmets.
- b0/b2 and b0/b4 f56, and b0/b2 f60, at the top-right trench (1250-1700, 100-500). Here the new clods fly over the
  sandbags and men.
- b0/b2 f20 and f28 at the left burst beside the top-left trench (0-450, 250-700).
- b0t/b4t f0, f20 and f26 at the top-right trench (1250-1750, 0-420).

## Blind setup

There were two separate general-purpose agents, told nothing about knobs, cards or hypotheses. Each pair had both full
frames plus a side-by-side crop of the changed region (up to 2x). X and Y were drawn at random per pair (seed 9091),
and each critic was told the draw was independent per pair. Per image, each critic scored four things, 0-10:
- barrage weight
- the men near the bursts, naming the region
- how well the earth reads as thrown earth
- how well the tracers read as bullets in depth

Critic A got the b1, b2 and b4t pairs (17). Critic B got the b3 and b4 pairs (12).

Key (new = the variant's image):

| critic, pair | variant | frame | crop (x0, y0, x1, y1) | new |
|---|---|---|---|---|
| A 1 | b2 | 60 | 1250, 100, 1700, 500 | X |
| A 2 | b2 | 56 | 1250, 100, 1700, 500 | X |
| A 3 | b1 | 57 | 1300, 80, 1700, 450 | Y |
| A 4 | b2 | 0 | 1380, 380, 1780, 800 | X |
| A 5 | b1 | 36 | 0, 300, 500, 700 | Y |
| A 6 | b1 | 50 | 1300, 100, 1700, 450 | Y |
| A 7 | b4t | 26 | 1250, 0, 1750, 420 | Y |
| A 8 | b1 | 20 | 0, 250, 500, 650 | Y |
| A 9 | b1 | 8 | 1250, 450, 1800, 850 | X |
| A 10 | b2 | 50 | 650, 650, 1100, 1080 | Y |
| A 11 | b4t | 10 | 700, 400, 1450, 1080 | X |
| A 12 | b1 | 26 | 1150, 0, 1600, 300 | X |
| A 13 | b2 | 20 | 0, 250, 450, 700 | X |
| A 14 | b4t | 0 | 1250, 0, 1750, 420 | Y |
| A 15 | b4t | 20 | 1250, 0, 1750, 420 | Y |
| A 16 | b4t | 30 | 0, 200, 750, 650 | Y |
| A 17 | b2 | 28 | 0, 250, 450, 700 | Y |
| B 1 | b4 | 14 | 0, 250, 700, 900 | X |
| B 2 | b4 | 20 | 0, 250, 500, 650 | Y |
| B 3 | b4 | 0 | 1300, 380, 1780, 800 | Y |
| B 4 | b3 | 12 | 300, 250, 1400, 1000 | Y |
| B 5 | b4 | 25 | 650, 450, 1350, 1080 | X |
| B 6 | b4 | 52 | 600, 400, 1250, 1080 | X |
| B 7 | b3 | 53 | 550, 280, 1300, 950 | Y |
| B 8 | b3 | 24 | 350, 250, 1350, 1080 | Y |
| B 9 | b3 | 15 | 650, 250, 1500, 1000 | Y |
| B 10 | b3 | 44 | 750, 300, 1500, 1080 | X |
| B 11 | b4 | 56 | 1250, 100, 1700, 500 | X |
| B 12 | b3 | 3 | 250, 150, 1400, 700 | X |

## Critic B (as relayed by the orchestrator: weight, men, earth, tracers)

| pair | X | Y |
|---|---|---|
| 1 | 6 6 5 7 | 8 5 5 4 |
| 2 | 8 5 6 4 | 6 6 5 7 |
| 3 | 8 5 4 4 | 7 6 5 7 |
| 4 | 9 5 5 3 | 7 6 5 7 |
| 5 | 6 6 5 7 | 8 6 6 4 |
| 6 | 6 6 6 7 | 8 5 4 3 |
| 7 | 8 5 4 3 | 6 6 4 8 |
| 8 | 8 6 5 4 | 6 6 5 7 |
| 9 | 9 5 5 3 | 7 6 5 7 |
| 10 | 6 6 5 7 | 7 6 5 5 |
| 11 | 6 6 6 7 | 8 5 4 4 |
| 12 | 6 6 5 7 | 7 6 5 5 |

The men were judged on the left trench (0-550, 150-800), and in pair 3 on the top-right trench's right end.

Critic B saw two builds. The first had thick see-through tracer beams, many small separate clods, and orange loop
ribbons (an orange slab at the left burst). The second had thin tracers with a bright core and a small coloured tip,
fewer, bigger, clumped clods, and a dark red-brown dome at the left burst. It called the first build Y in pairs 1, 5,
6, 10, 11 and 12, and X in the rest, which matches the key in 12 of 12. That is the change being seen, not a leak.

Its notes:
- Pair 7: the thin tracers pass behind the black cloud and reappear in its gaps. This was the best read of depth.
- Pairs 1 and 9: the thick bars cut across the men, at the left trench's edge and at the top-right trench's mouth.
- The orange loops read as flame or liquid, not soil. The dark dome at the left burst was the best heave of dirt,
  though the old build's small separate clods read slightly better as sprayed soil.
- No earth plume hides the men. The smoke does.

### Unblinded (old -> new)

| pair | variant | frame | weight | men | earth | tracers |
|---|---|---|---|---|---|---|
| B 4 | b3 | 12 | 9 -> 7 | 5 -> 6 | 5 -> 5 | 3 -> 7 |
| B 7 | b3 | 53 | 8 -> 6 | 5 -> 6 | 4 -> 4 | 3 -> 8 |
| B 8 | b3 | 24 | 8 -> 6 | 6 -> 6 | 5 -> 5 | 4 -> 7 |
| B 9 | b3 | 15 | 9 -> 7 | 5 -> 6 | 5 -> 5 | 3 -> 7 |
| B 10 | b3 | 44 | 7 -> 6 | 6 -> 6 | 5 -> 5 | 5 -> 7 |
| B 12 | b3 | 3 | 7 -> 6 | 6 -> 6 | 5 -> 5 | 5 -> 7 |
| B 1 | b4 | 14 | 8 -> 6 | 5 -> 6 | 5 -> 5 | 4 -> 7 |
| B 2 | b4 | 20 | 8 -> 6 | 5 -> 6 | 6 -> 5 | 4 -> 7 |
| B 3 | b4 | 0 (column over men) | 8 -> 7 | 5 -> 6 | 4 -> 5 | 4 -> 7 |
| B 5 | b4 | 25 | 8 -> 6 | 6 -> 6 | 6 -> 5 | 4 -> 7 |
| B 6 | b4 | 52 | 8 -> 6 | 5 -> 6 | 4 -> 6 | 3 -> 7 |
| B 11 | b4 | 56 (column over men) | 8 -> 6 | 5 -> 6 | 4 -> 6 | 4 -> 7 |

| variant | weight | men | earth | tracers |
|---|---|---|---|---|
| b3 (6) | 8.00 -> 6.33 (-1.67; 6 down) | 5.50 -> 6.00 (+0.50; 3 up, 0 down) | 4.83 -> 4.83 (0) | **3.83 -> 7.17 (+3.33; 6 up)** |
| b4 (6) | 8.00 -> 6.17 (-1.83; 6 down) | 5.17 -> 6.00 (+0.83; 5 up, 0 down) | 4.83 -> 5.33 (+0.50; 3 up, 2 down) | 3.83 -> 7.00 (+3.17; 6 up) |

## Critic A (as relayed by the orchestrator: weight, men, earth, tracers; "-" = not judged)

A rate limit stopped the critic's image viewer, so it scored pairs 1-3 and 13-17 from the crop only. It scored pairs
4-12 from the full frames plus the crop. It noticed that some files were shared: pair06_X is pair10_X (both are b0
f50) and pair08_X is pair13_Y (both are b0 f20). Both are baseline frames used twice, which is consistent with the key.

| pair | X | Y |
|---|---|---|
| 1 | 7 5 6 - | 7 5 7 - |
| 2 | 7 5 6 - | 7 5 7 - |
| 3 | 7 5 6 - | 7 5 6 - |
| 4 | 7 5 5 6 | 7 4 4 6 |
| 5 | 7 4 5 5 | 7 4 5 5 |
| 6 | 8 4 4 6 | 8 4 4 6 |
| 7 | 7 5 4 4 | 6 5 5 7 |
| 8 | 8 5 5 5 | 8 5 5 6 |
| 9 | 7 5 4 6 | 7 5 4 6 |
| 10 | 8 4 3 6 | 8 4 6 6 |
| 11 | 6 5 4 7 | 8 5 4 4 |
| 12 | 8 5 5 6 | 7 4 5 6 |
| 13 | 8 5 5 - | 8 5 6 - |
| 14 | - 3 3 - | - 3 4 - |
| 15 | - 3 3 - | - 3 4 - |
| 16 | - 5 4 4 | - 5 4 7 |
| 17 | 7 5 6 - | 7 5 5 - |

Its notes, in short:
- Pairs 1-2, the top-right burst: X throws a clumped column of clods. Y throws more, smaller clods in a wider ring,
  which read as flung soil.
- Pair 3: nearly identical.
- Pair 4, the top-right trench: Y adds see-through orange arc streaks at about (1600-1700, 470-560) over the helmets.
  They read as flame or spray and partly hide the men.
- Pair 6: a wash.
- Pair 7, the biggest tracer difference: X has thick see-through green bars painted on top. Y has thin needles with
  bright heads and fading tails that dim in the smoke, and they read as bullets in depth. Y is lighter in weight.
  X also has orange streaks over the top-right debris, where Y has a brown dust heave.
- Pair 10, the clearest earth win: at the bottom burst X has orange flame arcs and Y has dark clods flying (soil).
- Pair 11 is the reverse of pair 7: X's thin tracers fade into the smoke, while Y's thick bars are stickers but heavier.
- Pair 12: Y's smoke is lighter, flatter and more opaque. It dims the flash and slightly veils the men at the top left
  (0-300, 230-330). X's is darker and see-through, with a hotter flash.
- Pair 13: X has one large merged black blob, Y many small separate clods (better soil).
- Pairs 14-15: X has orange arcs over the top-right debris and men, Y has none.
- Pair 16: X has thick green bars, Y thin white streaks with better depth.
- Pair 17: Y's clods merge into a blob, slightly worse than X's even small clods.

Its general notes:
- The see-through orange arc fountain always reads as flame or spray, never as soil, and over a trench it partly
  hides the men.
- Where the clods differ, many small separate ones read as earth, and a few merged blobs read as silhouettes.
- The thick tracer bars are heavier but look painted on. The needles read as bullets and cost some weight.

### Unblinded (old -> new)

| pair | variant | frame | weight | men | earth | tracers |
|---|---|---|---|---|---|---|
| A 3 | b1 | 57 | 7 -> 7 | 5 -> 5 | 6 -> 6 | - |
| A 5 | b1 | 36 | 7 -> 7 | 4 -> 4 | 5 -> 5 | 5 -> 5 |
| A 6 | b1 | 50 | 8 -> 8 | 4 -> 4 | 4 -> 4 | 6 -> 6 |
| A 8 | b1 | 20 | 8 -> 8 | 5 -> 5 | 5 -> 5 | 5 -> 6 |
| A 9 | b1 | 8 | 7 -> 7 | 5 -> 5 | 4 -> 4 | 6 -> 6 |
| A 12 | b1 | 26 | 7 -> 8 | 4 -> 5 | 5 -> 5 | 6 -> 6 |
| A 1 | b2 | 60 (column over men) | 7 -> 7 | 5 -> 5 | 7 -> 6 | - |
| A 2 | b2 | 56 (column over men) | 7 -> 7 | 5 -> 5 | 7 -> 6 | - |
| A 4 | b2 | 0 (column over men) | 7 -> 7 | 4 -> 5 | 4 -> 5 | 6 -> 6 |
| A 10 | b2 | 50 | 8 -> 8 | 4 -> 4 | 3 -> 6 | 6 -> 6 |
| A 13 | b2 | 20 (beside the trench) | 8 -> 8 | 5 -> 5 | 6 -> 5 | - |
| A 17 | b2 | 28 (beside the trench) | 7 -> 7 | 5 -> 5 | 6 -> 5 | - |
| A 7 | b4t | 26 (column over men) | 7 -> 6 | 5 -> 5 | 4 -> 5 | 4 -> 7 |
| A 11 | b4t | 10 | 8 -> 6 | 5 -> 5 | 4 -> 4 | 4 -> 7 |
| A 14 | b4t | 0 (column over men) | - | 3 -> 3 | 3 -> 4 | - |
| A 15 | b4t | 20 (column over men) | - | 3 -> 3 | 3 -> 4 | - |
| A 16 | b4t | 30 | - | 5 -> 5 | 4 -> 4 | 4 -> 7 |

## Means, old -> new

| variant | weight | men | earth | tracers |
|---|---|---|---|---|
| b1, the C99 pair (A, 6) | 7.33 -> 7.50 (+0.17; 1 up, 0 down) | 4.50 -> 4.67 (+0.17; 1 up, 0 down) | 4.83 -> 4.83 (0) | 5.60 -> 5.80 (+0.20; n 5) |
| b2, C103 (A, 6) | 7.33 -> 7.33 (0) | 4.67 -> 4.83 (+0.17; 1 up, 0 down) | 5.50 -> 5.50 (0; 2 up, 4 down) | 6.00 -> 6.00 (n 2) |
| b3, C104 (B, 6) | 8.00 -> 6.33 (-1.67; 6 down) | 5.50 -> 6.00 (+0.50; 3 up, 0 down) | 4.83 -> 4.83 (0) | 3.83 -> 7.17 (+3.33; 6 up) |
| b4, all four (B, 6) | 8.00 -> 6.17 (-1.83; 6 down) | 5.17 -> 6.00 (+0.83; 5 up, 0 down) | 4.83 -> 5.33 (+0.50; 3 up, 2 down) | 3.83 -> 7.00 (+3.17; 6 up) |
| b4t, all four late (A, 5) | 7.50 -> 6.00 (-1.50; n 2, 2 down) | 4.20 -> 4.20 (0; 0 down) | 3.60 -> 4.20 (+0.60; 3 up) | 4.00 -> 7.00 (+3.00; n 3, 3 up) |

## Columns over the men (rule 6)

There were 10 pairs where a column stands over or right beside men:
- b2: f0, f20, f28, f56, f60
- b4: f0, f56
- b4t: f0, f20, f26

The men were 3 up, 7 equal and **0 down**. The C103 cap does what C102 lacked. The old see-through orange arcs over
the top-right helmets are gone, and nothing opaque replaces them there: b2 f0 went 4 -> 5, and b4 f0 and f56 went 5 -> 6.
In the b4t heap at the top-right trench (f0, f20), the men stay at 3, where they are buried by debris and smoke in
both builds.

## Decision

- **C99 pair (b1): passes. Land fx.smokeSoft=0.6 with fx.burstGlow=0.5** (a0046). The men were never lower and the
  weight was never lower: the glow pays back smokeSoft's -0.5 weight (a0040). The gain is small, from one pair of six
  (f26: men 4 -> 5, weight 7 -> 8).
- **C103 (b2): fails its target line. Do not land fx.columnSoil** (a0047). The weight is flat (7.33 both). Earth is
  flat on average, 2 up and 4 down. It rose where the orange flame arcs vanished (f50 3 -> 6, f0 4 -> 5). It fell
  where the clods merge into big blobs (f20, f28, f56, f60). The dark soil column is not seen against the dark
  smoke. Rule 6 holds (0 down over the men). What to keep for the next attempt: the cap and the removal of the arcs.
  What to fix: small separate clods thrown in a ring, and a column value that separates it from the smoke.
- **C104 (b3): passes the rule. Land fx.tracerInSmoke=1** (a0048). Tracers +3.33 in 6 of 6, and the men were never
  lower (+0.50). It is a trade: weight fell -1.67 in 6 of 6, and in b4t by -1.50. Both critics said the neon bars
  read heavier but painted on, and cut across the men. a0044's split shows fx.tracerShape=0 keeps the draw order
  and most of the depth at weight -0.33. So land it with shape 0, not with this run's full shape.
- **b4 (all four) does not pass as a whole.** The weight drops 6 of 6 (-1.83), and C103 inside it fails its own line.
- **Default set: fx.smokeSoft=0.6, fx.burstGlow=0.5, fx.tracerInSmoke=1 (fx.tracerShape=0), fx.columnSoil stays 0.**
  This exact combination was not shot. b4, which adds columnSoil to it, never lowered the men in any pair, so the
  set is not expected to either. The lander's check shot should confirm the weight.
