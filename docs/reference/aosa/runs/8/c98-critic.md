# C98: blind critic, old column against the landed default

Closes a0032's missing measured delta. Old: runs 8/col64-1 (fx.columnEarth 0.22, fx.columnEarthSize 0.38, build
dd6bef2). New: runs 8/dcol-1 (0.12 / 0.7, build 14ccd6f plus the C98 and C67 patches, committed as 96c1247 and
5378697). Both runs used scenario barrage, shot-tick 140, `--no-hud` and the release player. They share `hash_start`
252EA3E1DA8F7314 and `hash_end` C8D53F7E42C71459, so the battle is the same. The only commits between the two builds
are instrument and tool changes (f5ab2d2, c858f5c, 8a32744, 14ccd6f), and C67 changes only the HUD, which is hidden.

**Frame counts differ.** col64 took 64 frames (main plus f1-f63) and dcol took 32 (main plus f1-f31). Matched pairs
therefore exist only for frames 0-31.

## Evidence check (by eye, before the critic)

- **Left-edge burst.** A shell lands at about (0-250, 350-650) at f12 in both sets. Its flash, fireball and chunks
  are pixel-identical.
  - New: a brown column rises from its top-left starting at f16. It stands about 300 px tall by f20-f24 and is still
    up at f31. It is partly cut off by the screen's left edge.
  - Old: in f12-f31 the column shows only as small brown tips above the smoke ball's rim, at about (0-30, 200-290)
    from f26. By f36-f44 there is still no column taller than the smoke ball.
  - So a column is rising in both sets, but the old one barely clears the smoke.
- **Centre burst.** An earlier burst sits at about (750-1100, 750-1000), from before tick 140. In f0-f16 of the new
  set its column is in its late phase over the centre, at about (680-1030, 0-800). In the old set the same column
  shows only as small orange-brown licks at about (780-830, 470-520).
- **Timing limit.** C98's spec asks for frames 30-60 after an impact. dcol ends at f31, which is 19 frames (about
  0.3 s) after the left impact. The late phase is seen only on the centre burst (f0-f16), whose impact came before the
  window, so its exact age is not known.
- **Top-right burst.** A burst at f12 near (1300-1500, 50-250) shows no column in either set within f12-f31.

## Blind setup

The critic got seven pairs: frames 0, 8, 16, 20, 24, 28 and 31, at full resolution, re-saved under neutral names.
X and Y were shuffled per pair (seed 98032), and the critic was told the assignment was independent per pair. It
was a separate general-purpose agent and was never told which image was new.

Key (new build =): pair1 Y, pair2 Y, pair3 Y, pair4 Y, pair5 X, pair6 X, pair7 X.

The critic could see which side had a column. That is the change under test, not a leak.

## Critic scores, unblinded (0-10)

| pair (frame) | col old | col new | men old | men new | overall old | overall new |
|---|---|---|---|---|---|---|
| 1 (f0)  | 1 | 4 | 6 | 5 | 5 | 5 |
| 2 (f8)  | 1 | 5 | 6 | 6 | 5 | 6 |
| 3 (f16) | 1 | 3 | 6 | 6 | 7 | 6 |
| 4 (f20) | 1 | 4 | 6 | 5 | 7 | 6 |
| 5 (f24) | 1 | 4 | 6 | 5 | 6 | 6 |
| 6 (f28) | 1 | 3 | 6 | 5 | 6 | 5 |
| 7 (f31) | 1 | 2 | 6 | 5 | 5 | 4 |
| **mean** | **1.0** | **3.6** | **6.0** | **5.3** | **5.9** | **5.4** |
| **delta** | | **+2.6** | | **-0.7** | | **-0.4** |

**Readability drops.** The new column scored lower on the men in 5 of the 7 pairs and never higher. The critic's
regions:

- The left column lies over the leftmost men of the top-left trench, at (0-230, 100-300), in f20-f31.
- A brown tint over part of the right trench and crater, at (1540-1800, 80-260), mutes the men in f0.
- In f28 the centre tail covers the large dead tree, at (700-770, 240-500).

The mean drop (0.7) is inside a0032's band of 1, but the direction is the same in every pair that differs. Owner
rule 6 says readability never drops, so this is flagged for the owner. Nothing was reverted here.

**Column quality.** The new column scored higher, at 3.6 against 1.0, but it still does not read as thrown earth.
The critic read it as a smooth, flat, translucent brown or orange sheet: flame in its early frames, then smoke
ribbons. In late frames it breaks into "tendrils, hooks and ear-like lobes... like a tail or creature". What would
read as earth is "a darker, opaque core, a jagged or clumpy silhouette and visible debris chunks, one that falls off
quickly rather than curling".

## Critic output (verbatim)

The critic viewed the images downscaled and did not crop them, so detail under about 20 px may be missed.

| pair | X col | Y col | X men | Y men | X overall | Y overall |
|---|---|---|---|---|---|---|
| 1 (f0) | 1 | 4 | 6 | 5 | 5 | 5 |
| 2 (f8) | 1 | 5 | 6 | 6 | 5 | 6 |
| 3 (f16) | 1 | 3 | 6 | 6 | 7 | 6 |
| 4 (f20) | 1 | 4 | 6 | 5 | 7 | 6 |
| 5 (f24) | 4 | 1 | 5 | 6 | 6 | 6 |
| 6 (f28) | 3 | 1 | 5 | 6 | 5 | 6 |
| 7 (f31) | 2 | 1 | 5 | 6 | 4 | 5 |

Pair 1
- X col (800-910, 710-780; 1550-1700, 440-500): small orange licks peek above the bottom smoke ball and the right crater. There is no column.
- Y col (680-1030, 0-1080; 1480-1610, 510-650; 1540-1800, 80-260): a huge smooth brown-orange translucent sheet rises from the bottom burst to the top of the frame. It reads as a flame or smoke veil, not dirt, with no chunks and no crisp edge.
- X men (1480-1900, 80-640): the right trench's men are dark but countable. The left trench (150-560, 400-1080) is buried in grey smoke, identical in both.
- Y men (1540-1800, 80-260 and 1480-1610, 510-650): a brown tint lies over part of the right trench and crater and mutes the men. The left trench is identical to X.
- X overall: busy tracer fire and dark smoke balls, flat, with no vertical punch.
- Y overall (680-1030, 0-1080): the column adds height, but it is a blurry smear through the tracers rather than an impact.

Pair 2
- X col (640-1100, 150-760): above the bottom smoke ball there are only tracers and trees. There is no column.
- Y col (700-1010, 170-760): a tall brown column with a split, arched top comes out of the smoke ball. It reads most like a column of material of all the pairs, but the fill is flat and it has no debris texture.
- X men / Y men (1500-1900, 100-600; 0-560, 300-1080): the right trench is readable and the left is smoked over. The two are identical, and the column crosses only no-man's-land.
- X overall (600-1100, 150-1080): a smoke ball with green tracers, generic.
- Y overall (700-1010, 170-760): a readable "big hit" silhouette, though soft.

Pair 3
- X col (0-170, 280-640): the left impact is a bright fireball with a white cloud and black chunks. There is no column.
- Y col (0-70, 280-360; 700-1000, 100-700): a small orange cone above the left fireball reads as flame. The centre has thin wavy brown tendrils, like smoke trails or hair.
- X men / Y men (150-560, 440-800): the fireball lights the left trench men and they are countable. The two are identical.
- X overall (0-170, 280-640 plus 1260-1420, 40-260): two bright bursts and dense tracers make a strong frame.
- Y overall (700-1000, 100-700): the same bursts, plus brown tendrils that clutter the centre.

Pair 4
- X col (0-170, 280-650): fireball and white cloud, with no column.
- Y col (0-75, 120-330; 700-1000, 80-700): an orange-brown teardrop spike off the left burst reads as a flame tongue. The centre has wavy tendrils.
- X men (0-230, 110-300): the top-left trench's men are visible.
- Y men (0-75, 120-330): the spike overlays the leftmost men of the top-left trench.
- X overall (0-420, 280-650): a bright left burst with a blast haze, a good moment.
- Y overall: extra brown shapes that read as smoke or flame dilute the burst.

Pair 5
- X col (0-90, 0-290; 700-1000, 60-700; 740-850, 60-200): a big brown-orange plume rises off the left burst. The centre tendril ends in a curled hook like a tail, a soft ribbon rather than dirt.
- Y col (0-170, 280-640; 640-1100, 600-1080): fireball and smoke ball only, with no column.
- X men (0-230, 100-300): the left plume covers the leftmost part of the top-left trench.
- Y men (0-230, 100-300): the top-left trench is clear.
- X overall: some added height, but the curly shapes look odd.
- Y overall (0-420, 280-650): a clean fireball and haze, a bit flat.

Pair 6
- X col (0-100, 0-260; 700-850, 40-640; 670-1000, 520-640; 620-700, 880-1080): a brown curved strip at the left. In the centre a tail and two ear-like lobes give it a creature-like silhouette, not earth.
- Y col (600-1150, 600-1080): a plain dark smoke ball with no column.
- X men (0-230, 100-280; 700-770, 240-500): the top-left trench is overlaid, and the tail covers the large dead tree.
- Y men: clear.
- X overall (670-1000, 40-640): the lobes and tail are distracting and look unintended.
- Y overall (0-420, 280-650): the left fireball carries the frame, cleanly.

Pair 7
- X col (0-110, 0-260; 690-830, 30-560; 660-1000, 500-660; 620-720, 880-1080): late brown ribbons and furry lobes read as fur or smoke, not dirt.
- Y col (815-850, 625-660): a tiny orange fleck, effectively nothing.
- X men (0-230, 100-300): the left ribbon lies over the top-left trench.
- Y men: clear.
- X overall (600-1150, 30-1080): brown ribbons muddy an already dark, fading frame.
- Y overall (0-420, 280-650): a fading fireball, quiet but clean.

General observations
- **Column build:** it draws the column as one smooth, flat, semi-transparent brown or orange shape with no chunks, clods, grain or hard silhouette. It reads as a flame sheet early (pair 1, and the left spikes in pairs 3-4) and as wavy smoke ribbons later.
- **Late frames (pairs 5-7):** the column breaks into tendrils, hooks and ear-like lobes, the least earth-like and most distracting element.
- **No-column build:** it shows at most small orange flecks, with no sense of dirt thrown. The fireballs and tracers carry the barrage.
- **Readability of the men:** the biggest problem in both builds is the grey smoke over the left trench (150-600, 400-1080), identical in every pair. The columns add a smaller one, a translucent brown veil over the top-left and right trenches.
- **What would read as earth:** a darker, opaque core, a jagged or clumpy silhouette, visible debris chunks, and a quick fall-off rather than a curl.
