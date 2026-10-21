# The chlorine bank, round 3-g2-g2-g1-g2: one bank per BAND, and what the T1 frame really shows

The owner (2026-10-21): "Draw one soft gas bank a player can see, not a carpet of flat green pads. gas-t1-s0,
gas-t1-s1, gas-t1-s2 and the overhead are tiled rectangles, and `texture_var_drop` passes because those pads are
flat."

He is right, and the fault is two faults that are fixed apart. **This capture fixes the first one only** - the
one that made the T1 frame hold six banks instead of one. The second, the drawing, is untouched here and is the
next leg's work. The stills are reshot on this build; nothing is copied from g1.

## Fault 1, the capture: the drain ran once a seed, not once a block

g1's `manifest.json` reads, per seed: `gas_cells_before` **0** on the side row, **153** on o240 and **725** on T1,
with **1046** gas cards in the T1 frame. The drain sat at the END of a seed, so only the seed's first block (side)
started on an empty field. o240 then held the side block's bank; T1 held that one, the o240 block's five, and its
own - six banks, drifted together, which is the "carpet" in the owner's words.

`DrainFrames` now runs before **every** block. The loop is unconditional, as every wait in this harness is: a
block one pass skips shifts the plate's sim tick and the probe then refuses the pair (`side_tick_bad`). o240
keeps its five calls - its five banks are the far band's subject - and loses only the older blocks' leavings.

**Not before the very first block of the run.** The first run of this change put a drain there too and
`gas-side-s0` came back with `gas_cells_before` **157**: there are no leavings at tick 0, so that drain is only
1000 ticks of live battle, in which the AI's own chlorine can land on the field. The condition is on the SEED
(`s > 0`), never on the pass, so both passes still run frame for frame.

**The test's own timeout.** Nine drain blocks take the capture past NUnit's default 180 s - the plate pass died
on "Timeout value of 180000 ms was exceeded" at 223 s, with its stills half written. `[Timeout(900000)]`, five
times the measured run, so a slower machine or a colder asset database still finishes.

| Reading | g1 | here |
|---|---|---|
| `gas_cells_before`, worst row | **725** (T1) | **0**, every row of both manifests |
| `gas_cells` at the T1 shot | 818 / 679 / 562 | **66 / 68 / 66** |
| `gas_cards` at the T1 shot | 1046 / 853 / 718 | **90 / 92 / 90** |
| `draw_calls`, worst row | 139 | **137** (ceiling 300) |
| `side_tick_skew` | 0 / 0 / 0 | 0 / 0 / 0 |
| sim tick, gas vs plate | - | identical on all nine stills: 217, 1589, 2898, 4239, 5611, 6920, 8261, 9633, 10942 |

Both passes, same build, on the tip of this lane:

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2-g2-g1-g2/probe.py --pack <dir>
```

`Unity.exe` is the EDITOR, `C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe`. The `unity.exe` at
`%LOCALAPPDATA%/unity/bin/` is the CLI that talks to a running editor and answers `-batchmode` with
"error: unknown option".

## Fault 2, the drawing: still there, and now visible on its own

`gas-t1-s0.jpg` is one bank now, and it is still **a stack of flat opaque olive pads with hard rims**. That is
`GasBankThin` = 0.95: the erode threshold `(1 - alpha) * (0.45 + 0.55 * edge)` is near zero, so every card draws
its sheet's whole alpha blob at full opacity. One card per 4 m cell plus one per 2x2 block puts two such pads on
every patch of ground. Nothing here touches it; it is the next leg's step.

This is also why `texture_var_drop` is no evidence of a good look: `t1_block_var_gas` 0.000757 / 0.001686 /
0.001152 against `t1_block_var_plate` 0.002805 / 0.006891 / 0.003987. The gas side is near zero **because the
pads are flat**, which is exactly the fault, so the bar rewards it. Stated, not barred: moving that bar is the
drawing leg's job, with a reading that cannot be passed by a flat pad.

## The bars

`probe.py` is g1's, copied unchanged. Three of the four pass on all three seeds; one misses.

| Reading | Bar | s0 | s1 | s2 | |
|---|---|---|---|---|---|
| `t1_changed_frac` | >= 0.08 | 0.0854 | 0.1086 | 0.1220 | OK |
| `texture_var_drop` | >= 0.40 | 0.7300 | 0.7554 | 0.7110 | OK |
| `o240_hue_dist_deg` | >= 108.0 | 135.54 | 138.53 | 126.39 | OK |
| `side_face_px` | >= 30000 | 70300 | **28560** | 116466 | **MISSED on s1** |

**`side_face_px` on seed 1 fell from 37666 to 28560, and the bar does not move.** A bar only ever moves up here;
a reading that falls is a finding, never a lower bar. The finding:

- **The drawing did not change** - this leg changed no presentation code at all - so the fall is not the bank
  getting smaller. The side block's conditions are also unchanged: it was the first block after a drain in g1 and
  it still is, with `gas_cells_before` 0 both times.
- **What changed is the sim tick the side still is taken at.** Six extra drain blocks push the whole timeline
  along: seed 1's side still was tick 4239 here against g1's much earlier tick. The battlefield behind the bank
  is a different battlefield, and the probe's two gates cut a different share of it: `side_cut_frac` 0.4141 ->
  **0.4292**, `side_mask_px` 139794 -> **115185**. Seed 1 is the seed with the burning wreck, and it is the seed
  that moves.
- **So 30000 was set from one capture's spread, and the spread is wider than one capture showed.** The honest
  reading is that `side_face_px` has capture-to-capture noise of at least 24 % on this seed, which the g1 README
  could not know from a single run. The bar stays at 30000 and this capture MISSES it.

`t1_changed_frac` did fall too, as the plan predicted it would once the T1 frame holds one bank instead of six
(0.2249 -> 0.0854 at worst), and it still clears the owner's 0.08 - by 0.0054 on s0. That is thin, and it is
thin for an honest reason: one bank covers less of the frame than six. Settling the bank longer or shooting T1
nearer the patch would both lift it; neither was done here, because the drawing leg will change this number
again and the bar should be re-derived once, after it.

## What is here

| File | What it is |
|---|---|
| `probe.py` | g1's measure plus the two readings below (`t1_rim_px`, `t1_gas_cards`). `--selftest` exits 0; no arguments measures the stills beside it |
| `probe.json` | the readings of this run, one value per seed, with the bars |
| `gas-t1-s*`, `gas-o240-s*`, `gas-side-s*` | three seeds, three bands, each with its `-plate` (same pose, same sim tick) and its `.json` sidecar |
| `manifest.json`, `manifest-plate.json` | one row per still: patch, sim tick, pose error, gas field before and after the call, cards drawn, draw calls |

Reported, not barred: `side_tick_skew` (0 / 0 / 0), `side_cut_frac` (0.2658 / 0.4292 / 0.2964), `side_blob_px`
(125267 / 51315 / 203356), `gas_side_height_m` (1.648 / 2.168 / 2.416 m), `o240_bank_hue_deg` (86.5 / 85.0 /
94.7) against `o240_grass_hue_deg` (222.1 / 223.5 / 221.1), `o240_core_px` (6683 / 6420 / 5298), and
`gas_t1_height_m` (2.454 / 5.105 / 4.464) against `t1_footprint_bias_m` (3.552 / 3.794 / 3.727). Pose error is
0.0000 m on all eighteen stills.

## Fault 2, the measure: a reading that catches a flat pad

`texture_var_drop` cannot find the fault the owner named, because it **rewards** it: a flat pad flattens the
ground under it, so the flatter and harder the pad, the better that number reads. Both builds pass it (0.56-0.74).
Two readings are added so the drawing leg has something that bites. Their bars are deliberately **UNSET** here
(the probe prints them and gates nothing); they are derived in the drawing leg, from three captures of the build
that has the new drawing, by the same rule as every bar above.

- **`t1_rim_px`** - how soft the bank's edge is, in pixels. From every boundary pixel of the T1 blob whose
  outside neighbour lies left or right, walk outward along the row up to 24 px and count the steps until the
  plate difference falls under 0.25 x the median difference inside the blob; the median of those walks is the
  reading, `t1_rim_p90_px` sits beside it. A hard-edged pad reads 1-2 px (only JPEG softens it); a bank that
  erodes into wisps reads high single figures. Hand-checked both ways in `--selftest`: a 20 px rectangle with a
  10 px linear ramp reads **8 px** (the walk ends where 0.20 x (1 - i/10) < 0.05, so i = 8) and the same
  rectangle with no ramp reads **1 px**.
- **`t1_gas_cards`** - the cards the harness drew for the T1 still, read from its `manifest.json` row. This bar
  runs the other way (`<=`, an upper limit): a carpet is more cards, one bank is fewer. A missing field reads as
  a number no upper bar can pass, so it is never a free pass.

### The old build against the new readings

`python probe.py --dir ../ability-fx-captures-r2-3-g2-g2-g1 --out <outside this folder>`, so g1's folder is left
untouched. The before numbers come from the stills committed at `1dd0a2750c`, not from the code that reads them.

| Reading | g1, the carpet (s0 / s1 / s2) | here, one bank per block | What it says |
|---|---|---|---|
| `t1_gas_cards` | **1046 / 853 / 718** | **90 / 92 / 90** | the capture fix is visible in one number: 11x fewer cards |
| `t1_rim_px` | 5 / 2 / 3 | 3 / 2 / 3 | **unchanged, and it should be**: the drawing is untouched here |
| `t1_rim_p90_px` | 24 / 17 / 23 | 16 / 15 / 20 | the widest edges are the drifted leavings g1 still held |
| `texture_var_drop` | 0.56 / 0.74 / 0.61 | 0.73 / 0.76 / 0.71 | passes on both: the reading that cannot see the fault |

So the two readings do different jobs, and only one of them has fired yet:

- `t1_gas_cards` separates the two builds outright, and would have refused g1 at any bar between 93 and 718.
- **`t1_rim_px` does not separate them, because neither build has a soft edge.** Both read 2-3 px at worst, which
  is the hard pad of the selftest's second case. That is the honest state of this folder: the capture is fixed,
  the drawing is not, and the rim reading is the thing that will say when it is. It is proven to work by hand
  calculation (8 px against 1 px), not by a difference between these two builds.

### `side_face_px` still misses

Unchanged from the section above, and not touched by this leg: seed 1 reads 28560 against the bar 30000, so
`python probe.py` exits **1**. That bar is capture noise on the one seed with a burning wreck, and the drawing
leg reshoots all three seeds; it is settled there, never by widening the bar.
