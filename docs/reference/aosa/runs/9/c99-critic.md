# C99: blind pair critic, burst glow and smoke softness against today's defaults

Baseline run 9/c101 (today's defaults). Variants, same build, same flags (barrage, ShelledForest night, shot-tick 140,
64 held frames, `--no-hud`), all with `hash_start` 252EA3E1DA8F7314 and `hash_end` C8D53F7E42C71459:

- **c99g**: fx.burstGlow=0.5 (half the glow on the burst's cloud).
- **c99s**: fx.smokeSoft=0.6 (default 0.3).
- **c99f**: fx.smokeNightFire=0.15 (default 0.35).

## Where each variant differs from c101 (every frame f0-f63)

A pixel counts as changed when any channel moves more than 8 (`aosa.py stilldiff`'s threshold).

- **c99g** differs only while a burst cloud is lit:
  - f12-f27, up to 3.0% at f25, in (0-1434, 0-543): the top-left and top-right bursts.
  - f46-f59, up to 1.8% at f57, in (1387-1612, 148-363): the f44 impact at the top-right trench.
  - f28-f29 and f60-f61 move by 8 levels or less, and f0-f11 and f30-f45 are identical.
  - Changed pixels are darker (luma -23 to -53): the cream cloud is dimmer.
- **c99s** differs on every frame: 0.7-2.0% of pixels above 8 levels (7-12% at any level), spread over the smoke
  across the frame. Changed pixels are lighter (luma +16 to +34): the smoke's base is thinner over the ground and the men.
- **c99f** changes 7-13% of pixels on most frames, but by at most 18 levels, and on most frames by only 4-11. Above 8
  levels it is 0.0001 at f24 and 0 at f40. **It is no visible lever here**, so it was not critic-tested.

## Blind setup

13 matched pairs went to one separate general-purpose agent, which was told nothing of knobs, cards or hypotheses:
- 7 c99g pairs, on frames where it differs above threshold: f16, f20, f24, f27, f48, f52, f56.
- 6 c99s pairs, spread over the window: f12, f20, f28, f40, f48, f56.

The pair order was shuffled, and X/Y was assigned at random per pair (seed 99091). Each pair came as two full frames
plus a sheet of 1:1 crops of the C99 regions for X and for Y:
- TR, the top-right trench (1150-1600, 120-380)
- FL, the far-left flank (0-320, 320-920)
- LL, the lower-left trench under the smoke (300-680, 760-1080)

Per image it scored men TR, FL, LL, men overall and barrage weight (0-10).

Key (variant = new):

| pair | variant | frame | new |
|---|---|---|---|
| 1 | c99g | 16 | X |
| 2 | c99s | 48 | X |
| 3 | c99g | 56 | X |
| 4 | c99g | 48 | Y |
| 5 | c99s | 56 | Y |
| 6 | c99g | 27 | Y |
| 7 | c99g | 20 | X |
| 8 | c99s | 28 | Y |
| 9 | c99s | 12 | X |
| 10 | c99g | 52 | X |
| 11 | c99g | 24 | X |
| 12 | c99s | 40 | X |
| 13 | c99s | 20 | Y |

## Critic scores (as relayed by the orchestrator: TR, FL, LL, men, weight)

| pair | X | Y |
|---|---|---|
| 1 | 5 4 3 4 7 | 5 4 3 4 6 |
| 2 | 5 3 7 6 6 | 5 3 6 5 7 |
| 3 | 4 3 7 5 7 | 4 3 7 5 7 |
| 4 | 5 3 7 5 6 | 5 3 7 5 7 |
| 5 | 4 3 6 5 7 | 4 4 7 6 6 |
| 6 | 5 3 6 5 7 | 5 3 6 5 7 |
| 7 | 4 4 3 4 7 | 4 4 3 4 6 |
| 8 | 5 3 5 5 6 | 5 3 6 5 6 |
| 9 | 4 4 4 4 7 | 4 3 4 4 7 |
| 10 | 4 3 7 5 7 | 4 3 7 5 6 |
| 11 | 5 3 5 5 7 | 6 3 5 5 7 |
| 12 | 4 4 7 6 6 | 4 3 6 5 7 |
| 13 | 4 3 3 4 7 | 4 4 3 4 7 |

The critic's reading of the differences: pairs 1, 3, 4, 6, 7, 10 and 11 differ only in the burst puff's brightness,
and the darker, less lit puff reads heavier (about +1), with the men unaffected except pair 11. Pairs 2, 5, 8, 9, 12 and
13 differ in the lingering smoke's density: thinner smoke shows the men better (about +1 on men in LL and FL), and
denser smoke adds a little weight. Its diff images were scratch and were removed.

## Unblinded (old default -> variant, mean over the pairs)

| variant | men TR | men FL | men LL | men overall | weight |
|---|---|---|---|---|---|
| c99g (7) | 4.71 -> 4.57 (-0.14; 1 down) | 3.29 -> 3.29 | 5.43 -> 5.43 | **4.71 -> 4.71 (0)** | **6.43 -> 7.00 (+0.57; 4 up, 0 down)** |
| c99s (6) | 4.33 -> 4.33 | 3.00 -> 3.67 (+0.67; 4 up, 0 down) | 5.00 -> 5.67 (+0.67; 4 up, 0 down) | **4.67 -> 5.17 (+0.50; 3 up, 0 down)** | **6.83 -> 6.33 (-0.50; 3 down, 0 up)** |

## Decision

Pass rule: readability rises, and weight does not drop by more than 0.5 (rule 6 is hard).

- **c99s passes, at the limit.** The men rise by +0.5 overall and +0.67 in FL and LL, and are never lower in any pair.
  Weight drops by exactly 0.5, which is not more than 0.5. `runs/9/C99.patch` makes 0.6 the default:
  - `FlipbookFx.DefaultSoft` goes from 0.3 to 0.6, and the KnobsTests expectation from "0.3" to "0.6".
  - OldSoft stays 0, the look before C52. fx.smokeSoft=0.3 draws the previous default.
  - It was made on lane/show/aosa 1541ab3 plus C102-default.patch and C72.patch, and compiled with `occ.py --changed`.
  - The weight cost sits at the rule's edge, so a later weight card should re-score it.
- **c99g fails C99.** The men do not rise (TR -0.14). But it raises weight by +0.57 (4 up, 0 down) with the men flat,
  so it is a weight lever for C53/C103, not a readability one.
- **c99f did nothing visible.** It was not tested.
