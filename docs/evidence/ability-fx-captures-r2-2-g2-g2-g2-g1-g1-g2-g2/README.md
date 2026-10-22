# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2: one close cloud with a soft edge, 2026-10-22

`python probe.py` exits 0 on this build's own three captures (`run1/`, `run2/`, `run3/`, T1 frames 804 / 819 / 834)
and the one `cards/` set. Every bar is printed with its spread and its margin.

## The owner's complaint

> At the close camera the gas is still separate flat green tree-cards with hard edges, and the gas card is one
> flat puddle with rise 0. Draw one cloud with a soft edge and a height a player can see. The area floor 0.0135
> sits 0.00006 under three readings that only span 0.00003, so move that floor outside this build. Start from
> `lane/show/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2`'s tip. Bar outside the spread of three captures of the
> current build.

## What changed

- **One cloud, not a field of trees.** `CombatFx.GasPuffWide` (new, 2.0) widens the close card over the 12 m gap
  between cards (`GasPuffBlock` 3 x `FieldCellSize` 4), so the cards overlap into one mass. `GasPuffStand` came
  down 1.2 -> 0.50 in the same sweep row, so the drawn height holds at about 7 m.
- **The soft edge is the overlap, not the sheet row.** Dropping `Erode` from the `GasPuff` `Sheets` row - the
  plan's first knob for the hard edge - was measured and **refused**: see the edge table below.
- **The card rise reads for real** (part 1's probe fix) and the `cards-gaspuff` card is now packed as `CombatFx`
  draws it at `BankStep` 1 (`GasPuffWide` wide, `GasPuffStand` of that width tall), so the two knobs move the
  isolation card exactly as they move the field.
- **`BurstRecipeTests.TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat`**: its close half is reversed. It
  asserted a close card does NOT scale with the block ("gaps are what makes a puff read"); it now asserts the card
  is **wider than the gap** (15.4 m against 12 m) and still stands over a 1.8 m man (7.7 m, under 10). The
  overview half is untouched.

## The edge: dropping Erode was refused, with numbers

One gas + plate pass per row at `TW_GAS_WAIT_FRAMES` 200, `GasPuffWide` 1.0 / `GasPuffStand` 1.2 (the old size), so
only the sheet row moves:

| GasPuff row | T1 `green_px` | `blob_px` | `green_height_frac` | `green_low_share` | `green_rise_m` | `green_soft_share` |
|---|---|---|---|---|---|---|
| `Erode`, `Low` 0.17 (old build) | 156037 | 12528 | 0.0950 | 0.3462 | 2.575 | 0.1230 |
| `Erode`, `Low` 0.10 | 158151 | 12550 | 0.0950 | 0.3448 | 2.575 | 0.1223 |
| no `Erode`, `Low` 0.17 | 19780 | 725 | 0.0066 | 1.0000 | 0.221 | 0.1821 |
| no `Erode`, `Low` 0.10 | 20438 | 744 | 0.0065 | 1.0000 | 0.219 | 0.1640 |
| no `Erode`, `Low` 0.03 | 20791 | 770 | 0.0065 | 0.9942 | 0.219 | 0.1701 |

`Erode` is not only the edge on this row: it is what carries the card's alpha. Without it the close gas all but
vanishes - 8x fewer green pixels, and what is left is faint speckle that falls apart into a 725 px blob, so the
height, rise and area bars collapse. The ink ramp (`Low` 0.17 -> 0.10 -> 0.03) moves none of it. So `Erode` stays,
and the soft faint band comes from the overlap instead: `green_soft_share` 0.1230 -> 0.1639, outside both builds'
spreads.

## The width sweep (`GasPuffWide` / `GasPuffStand`, `Erode` kept, wait 200)

| wide / stand | `height_frac` (0.08-0.15) | `low_share` (>= 0.30) | `rise_m` (>= 2.4) | `top_share` | `cloud_count` | verdict |
|---|---|---|---|---|---|---|
| 1.0 / 1.2 | 0.0950 | 0.3462 | 2.575 | 0.0803 | 39 | the old build: 39 lone cards |
| 1.8 / 0.67 | 0.1281 | 0.1937 | 3.964 | 0.3739 | 34 | merged; `low_share` floor broken |
| 1.9 / 0.55 | 0.0872 | 0.2954 | 2.765 | 0.2879 | 39 | a hair under the floor |
| **2.0 / 0.50** | **0.0836** | **0.3045** | **2.653** | **0.2908** | **38** | **kept** |
| 2.1 / 0.47 | 0.0851 | 0.3018 | 2.697 | 0.2904 | 39 | the same look, less room |
| 2.2 / 0.50 | 0.1300 | 0.1961 | 3.686 | 0.4306 | 35 | one mass, floor broken |
| 2.2 / 0.55 | 0.1433 | 0.1836 | 4.064 | 0.4147 | 36 | floor broken |
| 2.4 / 0.50 | 0.1320 | 0.1869 | 3.746 | 0.4244 | 35 | floor broken |
| 2.4 / 0.42 | 0.1148 | 0.2114 | 3.254 | 0.4407 | 35 | floor broken |
| 2.4 / 0.35 | 0.0666 | 0.3413 | 2.151 | 0.2050 | 48 | under the height floor and the rise bar |
| 2.0 / 0.45 | 0.0634 | 0.3475 | 2.141 | 0.2037 | 42 | under the height floor |
| 1.8 / 0.45 | 0.0626 | 0.5181 | 1.690 | 0.0928 | 46 | barely merged |
| 1.8 / 0.35 | 0.0471 | 0.6781 | 1.270 | 0.1041 | 37 | a puddle again |
| 2.8 / 0.38 | 0.1434 | 0.1784 | 4.056 | 0.4236 | 35 | floor broken |
| 3.0 / 0.40 | 0.1776 | 0.1470 | 5.149 | 0.5964 | 23 | over the 0.15 height ceiling |

**The merge and the owner's `green_low_share` floor pull against each other.** Once the cards merge into one mass
it reaches back into the field, and the far cards' pixels sit high on screen, so the share of green under a man's
head falls to about 0.19. Every row that merges fully (`top_share` over 0.4) breaks the 0.30 floor the owner set
when the gas was a puddle. `2.0 / 0.50` is where both hold: the widest merge that floor allows. **A fuller single
cloud needs the owner to move `green_low_share`**, and that question is in `docs/reference/decisions.md`.

## The bars, from these three captures

| bar | cmp | bar | run1 / run2 / run3 | spread | margin | note |
|---|---|---|---|---|---|---|
| `green_low_share` | ge | 0.30 | 0.3045 / 0.3048 / 0.3034 | 0.0014 | 0.0034 | owner-decided, carried |
| `green_height_frac` | le | 0.15 | 0.0836 / 0.0833 / 0.0841 | 0.0008 | 0.0659 | owner-decided ceiling, carried |
| `green_height_floor` | ge | 0.08 | the same three | 0.0008 | 0.0033 | the floor, carried |
| `green_rise_m` | ge | 2.4 | 2.653 / 2.643 / 2.669 | 0.026 | 0.243 | carried |
| `green_area_frac` | ge | **0.05** | 0.07577 / 0.07591 / 0.07606 | 0.00029 | 0.02577 | **moved out of the old build** |
| `t1_top_share` | ge | **0.28** | 0.2908 / 0.2910 / 0.2901 | 0.0009 | 0.0101 | new |
| `green_soft_share` | ge | **0.14** | 0.1639 / 0.1645 / 0.1637 | 0.0008 | 0.0237 | new |
| `o240_green_area_frac` | ge | 0.0001 | 0.0010 / 0.0008 / 0.0006 | 0.0004 | 0.0005 | overview, untouched |
| `o240_cloud_count` | ge | 3 | 11 / 9 / 7 | 4 | 4 | overview, untouched |
| `o240_cloud_ceiling` | lt | 15 | 11 / 9 / 7 | 4 | 4 | overview, untouched |
| `o240_cloud_share` | ge | 0.25 | 0.4777 / 0.4384 / 0.3676 | 0.1101 | 0.1176 | overview, untouched |
| `pose_error_max` | lt | 0.50 | 0 / 0 / 0 | 0 | 0.50 | `GasBankStills.PoseBar` |

Every margin is at or over the spread. The overview rows read exactly the parent's numbers: nothing outside
`DrawGasBank`'s `step == 1` branch moved.

### The area floor, as the owner asked

The old floor 0.0135 sat 0.00006 under readings spanning 0.00003. One merged cloud covers 5.6x the screen
(0.0136 -> 0.0758), so the floor moves to **0.05**: 0.0258 over the new minimum, 89x their spread, and the old
build's own 0.01359 / 0.01358 / 0.01356 fails it outright.

### The three new keys, before and after

| key | old build (the parent's runs) | this build | barred |
|---|---|---|---|
| `t1_top_share` | 0.0803 / 0.0800 / 0.0797 | 0.2908 / 0.2910 / 0.2901 | yes, `ge` 0.28 |
| `green_soft_share` | 0.1230 / 0.1181 / 0.1182 | 0.1639 / 0.1645 / 0.1637 | yes, `ge` 0.14 |
| `t1_cloud_count` | 39 / 44 / 42 | 38 / 41 / 38 | **no** |

`t1_cloud_count` is **not** barred: the two spreads overlap, so the merge did not move it. It is still measured and
printed. (The count stays near 40 at every width in the sweep: the mask's fringe specks are counted too, and they
multiply as the cards grow.) Never bar a measure the change did not move.

## The card check

`cards/` is one capture, so there is no spread to bar against and the look report stays unbarred - except for one
refusal, on the card this unit draws:

| card | rise_m | relief | mask_px |
|---|---|---|---|
| `cards-gas` | 0.655 | 0.0839 | 5914 |
| `cards-smoke` | 0.217 | 0.0888 | 126 |
| `cards-puff` | 0.216 | 0.1138 | 157 |
| `cards-gaspuff` | **4.259** | 0.1131 | 59304 |

`probe.py` exits 1 when `cards-gaspuff` reads under `HEAD_M` 1.8 m - the owner's "rise 0" and "a height a player
can see", in the probe's own constant. `cards-gas` is **not** barred: it is the merged-out overview card, which
`GasBankSquash` keeps deliberately under a man's head (`BurstRecipeTests` asserts exactly that), and the overview
is out of this unit's scope.

## What is here

- `probe.py` - part 1's probe with the bars set, `t1_cloud_count` demoted to a diagnostic, and the
  `cards-gaspuff` rise refusal.
- `run1/`, `run2/`, `run3/` - this build's `GasBankStills` captures, sidecars and manifests, at
  `TW_GAS_WAIT_FRAMES` 200 / 215 / 230 (T1 frames 804 / 819 / 834, O240 341 / 356 / 371), `pose_error_m` 0.
- `cards/` - the four isolation cards and their plate, one `GasBankCards` launch.
- `probe.json` - every key of every run, the look report and the bars.

## The capture recipe

Unchanged from the parent. Per run, this checkout's editor closed, two batch passes into the SAME directory at the
SAME wait (the second with `TW_GAS_PLATE=1`), never two at once against this checkout:

```bash
TW_STILLS_DIR=%TEMP%/tw-gasbank-g2g2-run2 TW_GAS_WAIT_FRAMES=215 \
python Tools/pipeline/run_detached.py start gas-g2g2-run2 --timeout 2400 -- \
  "C:/Program Files/Unity/Hub/Editor/6000.0.50f1/Editor/Unity.exe" -batchmode \
  -projectPath <checkout>/trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults <out>/stills.xml -logFile <out>/stills.log
python docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2/probe.py --pack %TEMP%/tw-gasbank-g2g2-run2 --into run2
```

Runs 1 and 3 are the same at 200 and 230; `cards/` is one launch at `-testFilter GasBankCards`, no wait knob and no
plate pass (the test shoots its own plate).

## Commands

```bash
cd docs/evidence/ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2
python probe.py                 # the three runs, the cards, every bar: exit 0
python probe.py --selftest      # the five hand cases
python probe.py --measure run1  # one directory, every key, no bars
```
