# C101: blind critic, barrage weight and left-trench readability on today's defaults

Run 9/c101 (release player, scenario barrage, ShelledForest night, shot-tick 140, 64 held frames: f0 = `c101-1.png`,
f1-f63, `--no-hud`). `hash_start` 252EA3E1DA8F7314, `hash_end` C8D53F7E42C71459. Knobs: none set (today's defaults,
before C102's default change).

## Evidence check (by eye and by diff, before the critic)

- **The left-edge shell lands at f12**, at about (0-200, 380-640): a white-hot fireball with black chunks and a flash
  (f11 has none). Consecutive-frame change jumps from 0.43 (f11) to 0.53 (f12).
- **What rises from f14-f16 is the burst's cream cloud**, at about (0-260, 330-520), not a distinct earth column. At the
  current column defaults (fx.columnEarth 0.22, fx.columnEarthSize 0.38, C98 reverted) the column barely clears the
  cloud, as runs 8/c98-critic.md found. The cloud turns brown by f28-f36, and from f40 a dark brown smoke mass covers
  (0-480, 250-700), over the top-left and left trench.
- **A second impact lands at the top-right trench at f44**, at about (1420-1570, 200-340) (change 0.40 -> 0.57).
- So f12-f60 holds the left impact from 0 to 0.8 s and the top-right one from 0 to 0.27 s.

## Blind setup

A separate general-purpose agent got 7 shots, f12, f20, f28, f36, f44, f52 and f60, renamed shot1-shot7 in order, each
at full resolution plus a 1:1 crop of the left side (x 0-700, y 80-1080). It was told nothing about knobs, cards or
hypotheses, only to score barrage weight and left-trench readability (0-10) per shot and to rank three elements
(the earth column above each impact, the smoke's tearing and softness, tracers drawn over the smoke) from weakest.

## Result

The critic's final report went to the orchestrator, which relayed this summary; the per-shot table was not relayed.

| | mean |
|---|---|
| barrage weight | **5.43** |
| left-trench readability | **4.00** |

Weakest first:
1. **(a) the earth column.** No column above any impact, only thin translucent orange-brown arcs.
2. (c) tracers drawn over the smoke.
3. (b) the smoke's tearing and softness.

## Decision

Weight 5.43 is below 8 and the left trench 4.0 is below 5, so C53 does not close. The weakest named is the earth
column, which is C102's (toon cut on the column, measured a0038) and C103's (column size, dark core, rim) ground.
Next: tracers inside the smoke (in C103's rest).
