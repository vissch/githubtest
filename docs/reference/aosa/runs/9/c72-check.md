# C72 check: the per-shot log against the C71 blind count

Run `c72s` (runs/9/c72s-1.json): the same bench as `c71f` (`--player --shot-tick 100 --shot-frames 32 --no-hud`, window
tick 1800, still at tick 1900), on the build with C72's patch (`runs/9/C72.patch`; gate EditMode 356/356, PlayMode
15/15). Its f1, f12, f24 and f31 stilldiff 0.00000 against c71f's: the log does not change the image. Read with
`python Tools/aosa/aosa.py shots c72s --table`; the comparison script is below.

Log: 399 shots logged from tick 1890 (0 dropped), 183 first drawn on held frames 1-31, 50 in flight at frame 0, 9 whole
ticks (1899-1907) inside the frames. Frame 0 has no births. That is the stagger, not a gap: tick 1899's events arrive on
frame 0, and every shot with a delay above 0 is first drawn on a later frame, so a tick's shots land on the 4 frames
after the frame they arrive on.

## The critic's lines, mapped by start px

A birth is put in the critic's region by its screen start (top-left px): team 0 with start y > 1080 is BOT (shooters
below the edge), team 0 with y < 600 is LTU, the rest of team 0 is LTL; team 1 with y < 220 is TOP, the rest is RT.

| critic line | log lines (births) | log births f1-31 | critic f1-31 | per-frame r | max frame share log / critic |
|---|---|---|---|---|---|
| LTU | 0:t0.0 (9), 0:t0.1 (23), 0:t0.2 (49) | 81 | 50 | 0.95 | 0.14 / 0.14 |
| LTL | 0:t0.4 (12), 0:t0.5 on screen (10), 0:t0.3 (2) | 24 | 35 | 0.89 | 0.12 / 0.14 |
| BOT | 0:t0.5 below the edge (28), 0:t0.6 (13) | 41 | 22 | 0.39 | 0.17 / 0.14 |
| RT | 1:o1.3 (15), 1:t1.2 (7), 1:o1.4 (5), 1:o2.3 (3), 1:o2.4 (3), 1:t1.3 (1) | 34 | 26 | 0.35 | 0.09 / 0.12 |
| TOP | 1:t1.1 (3) | 3 | 3 | 1.00 | 0.67 / 0.67 |
| all | 14 lines | 183 | 136 | | |

Frame 0, what is drawn (the critic's "visible" row): log LTU 19, LTL 2, BOT 9, RT 20, TOP 0 (50); critic 14, 7, 3, 14, 0 (38).

Where the critic was right:
- **TOP is exact.** Frames 20 (2) and 29 (1), as it said, at high confidence.
- **LTU's timing is right.** Per-frame r = 0.95, and the volley at frames 26-28 is real (log 10, 11, 6 against its 7, 7, 4).
- **Its single-frame shares hold.** No line with 10 or more births puts more than 0.23 of its births on one frame
  (log: 0:t0.6 0.23, 0:t0.2 0.18, 0:t0.5 0.18). C62 (a0037) stands.
- **Frame 0's red bundle is RT's.** RT has 20 tracers in flight at frame 0.

Where it was wrong:
- **Its totals were low by a quarter.** It counted 136 against 183 births, although it called the totals "fairly
  reliable". LTU was low by 38% (50 against 81), mostly in bundles: frame 2 has 10 births and it counted 4.
- **It split LTL and BOT at the wrong place.** Tracers from 0:t0.5, whose shooters stand just below the edge, enter at
  x 280-700. It counted many of them as LTL (35 against 24) and dated them late, which is why BOT has r = 0.39. LTL
  plus BOT is 65 in the log and 57 in its count.
- **RT is outside its range.** It gave 20 to 30, and the log has 34. RT's timing also does not match (r = 0.35), which
  fits the trunk, stump and lamp occlusion it named.

## C63's rule: a line with more than half of one tick's births on one frame

The C63 rule asks for line-ticks with enough births. In ticks 1899-1907 there are 10 line-ticks with 4 or more births:

| line, tick | births | most on one frame | share | chance of a share this high under uniform random delays |
|---|---|---|---|---|
| 0:t0.2, 1903 | 4 | 3 | 0.75 | 0.25 |
| 0:t0.5, 1902 | 14 | 7 | 0.50 | 0.29 (7 or more) |
| 0:t0.1, 1907 | 8 | 4 | 0.50 | 0.57 |
| 0:t0.2, 1899 / 1907 | 20 / 22 | 7 / 9 | 0.35 / 0.41 | |
| others (4-8 births) | | | 0.38-0.50 | |

The chance column is a Monte Carlo of i.i.d. uniform delays over one tick, 3.2 held frames a tick, with the frame grid
at a random phase. That is what C63's per-shot hash would give.

Only one line-tick is above 0.5, and it has 4 births, where uniform random delays do that a quarter of the time. Each
line's median per-tick share equals the uniform-random chance level (0.41-0.50). Across all lines, a whole tick's
largest one-frame share is 0.33-0.58, where before C22 it was 1.0. **C63 stays closed**: the golden-ratio stagger
already spreads each line as well as a uniform hash would, so the hash has nothing to win.

## C45

**C45 closes on C72.** The log gives each shot's frame, tick, side, line and screen px exactly, with 0 dropped and the
image unchanged, which is the "high" confidence C45 asked for. This check also measured the blind count's error:
totals low by a quarter, and lines at the screen edge mislabelled.

## Method

This is `c72_cmp.py` from the author's scratchpad. It loads c72s-1.json, maps each birth to a region with the start-px
rule above, and parses the critic's per-frame table from `c71-critic.md`. It then compares the two per frame (totals,
|diff| and Pearson r). The chance levels come from a 20,000-trial Monte Carlo with seed 7.
