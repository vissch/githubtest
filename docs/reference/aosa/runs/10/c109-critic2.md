# C109, second blind critic: p4c vs p0 (cycle 10)

Verdict: **land** p4c (fx.columnPlay=0.4, fx.columnCap=1). It passes rule 6 with a second, independent critic.

## Setup

- Runs: the existing runs/10 captures only, with no new render. p0 = today, p4c = candidate (64 frames from tick 140); p0t = today, p4ct = candidate (32 frames from tick 180).
- Per-frame diff (any channel > 12). p4c differs at f0-12 near the right-centre sandbag trench, (1517-1707, 425-747). It also differs at f13-63 across the centre and left, touching the left trench from f21 and the top-right trench from f51. p4ct differs in every late frame, in both the left and the top-right regions.
- Pairs were picked by how much the diff overlaps each trench band: 6 from the main window (f0, f9, f32, f44, f56, f63) and 3 from the late window (f0, f18, f29).
- One foreground critic scored the pairs. It was not told what changed, and saw full frames (1280x720) plus a full-resolution crop of the area that differs.
- X/Y was shuffled with seed 1092, 4/4 balanced over pairs 1-8. Pair 9 (main f0) was sent to the same critic afterwards: my first trench band stopped at y 440 and missed the right-centre trench at f0.
- Independence: the critic knew nothing about the change. Before scoring, I (the orchestrator) saw only the one-line a0054 summary in attempts.jsonl, which I read to get the next id. I read c108.md and c109.md only after all scores were in.

## Key and scores (today -> candidate)

| pair | frame | X | trench named | earth | men | weight |
|---|---|---|---|---|---|---|
| 1 | p0/p4c f9 | today | right | 5 -> 5 | 5 -> 5 | 5 -> 5 |
| 2 | p0/p4c f32 | candidate | left | 5 -> 5 | 5 -> 5 | 6 -> 6 |
| 3 | p0/p4c f44 | today | left | 4 -> 5 | 5 -> 5 | 7 -> 7 |
| 4 | p0/p4c f56 | candidate | left | 3 -> 5 | 5 -> 5 | 6 -> 6 |
| 5 | p0/p4c f63 | candidate | left | 3 -> 5 | 4 -> 4 | 5 -> 5 |
| 6 | p0t/p4ct f0 | today | top-right | 4 -> 5 | 2 -> 2 | 6 -> 6 |
| 7 | p0t/p4ct f18 | today | top-left | 3 -> 3 | 2 -> 2 | 7 -> 7 |
| 8 | p0t/p4ct f29 | candidate | top-left | 2 -> 2 | 2 -> 2 | 7 -> 7 |
| 9 | p0/p4c f0 | candidate | right | 3 -> 5 | 5 -> 5 | 6 -> 6 |
| **mean (9)** | | | | **3.56 -> 4.44 (+0.89; 5 up, 0 down)** | **3.89 -> 3.89 (0; 0 down)** | **6.11 -> 6.11 (0)** |
| mean (pairs 1-8) | | | | 3.63 -> 4.38 (+0.75) | 3.75 -> 3.75 (0) | 6.13 -> 6.13 (0) |

## What the critic called wrong

The critic's complaints, with each one assigned to its side after unblinding:

- Today only: "orange crown splashes / arcing fountain jets read as liquid, sparkler or flame, not soil" (pairs 3, 4, 5, 6, 9), a vertical orange streak through the smoke hole (pair 3), embers in a cloud (pair 2) and a faint orange lick near the top-left trench (pair 8).
- Candidate only: nothing. It had no pop, no cut-off, nothing missing and no hard edge. In pair 3 the critic said the streak was "gone (the hole shows ground)", with no artefact.
- Both sides: flat opaque smoke blobs; a small orange cap on the bottom-centre cloud (pair 9); and the lilac/blue "plasma or water" column over the centre burst in the late window (pairs 7 and 8). That column is the worst earth cue left, and it is unchanged by this knob.

## Rule 6

| check | result |
|---|---|
| earth or men up on average | earth +0.89 |
| men never lower by more than 1 in a pair | no pair lower |
| men not lower on average | 0 |
| weight >= -0.5 | 0 |
| no pop or cut artefact | none called out on the candidate |

Result: **pass**.

## Compared with the first critic (c109.md, 6 p4c pairs)

| critic | earth | men | weight |
|---|---|---|---|
| first | +1.67 (6 of 6 up) | +0.17 (the top-right trench at f0, 4 -> 5) | -0.17 (f50, 6 -> 5) |
| second | +0.89 (5 of 9 up; the 4 flat pairs are the f9/f32 frames, where the arcs are small, and the late f18/f29 frames under the unchanged centre column) | 0 (at f0 it saw no change in the men, 5 -> 5) | 0 (f56 held at 6, and the second critic did not score f50) |

The two agree in direction and on every rule 6 check. Earth rises wherever the orange arcs were in view, and it never falls. The second critic finds no men gain and no weight cost, so p4c's small plus and minus in c109.md are within one critic's noise.

Default to land: fx.columnPlay=0.4, fx.columnCap=1 (fx.columnBurstLit stays 1, inert). Next target: the lilac/blue centre column in the late window.
