# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2-g2: the band scales with the merge step

Three captures of the band-scales-with-the-step build, and the re-bar they asked for. **The look change worked and
the bar cannot be set**: `bank_patch_count` fell from 13/19/21 to 8/7/12, but its run-to-run spread of 5 puts the
honest ceiling at 17, over the owner's "at or under 12". No bar is loosened, re-tuned or fitted by dropping a run,
so `BAR_VALUES` is still the previous round's and `python probe.py` exits 1 on this folder's own evidence.

## The complaint

> At 240 m the bank is a flat striped lime patch on the trench, and the three overview stills are one pose (sim
> ticks 248, 268 and 328). bank_patch_count is 13/19/21 and the ceiling of 29 equals max plus the spread of 8.
> Join the stripes into one cloud a player can read at that zoom, and reshoot the three captures at least 60
> frames apart. Bar bank_patch_count at or under 12.

## The lever (shipped on this lane in `bfa6ac75f4`)

`GasBankBandFor(step)` scales the painted band with the merge step, so the overview's doubled row rise no longer
outruns the band; and `GasBankStandFloor = GasBankFullThick / 2` (2.0) draws the thinnest standing card at the
midpoint between haze and the bank's core opacity, instead of at 0.8, barely over the probe's visibility bar,
where it broke into blotches. The captures in `run1`/`run2`/`run3` are of that build.

## Did the stripes join?

**Yes, by the count the owner named.**

| key | this round | previous round's captures | what it says |
|---|---|---|---|
| `bank_patch_count` | **8 / 7 / 12** | 13 / 19 / 21 | the painted stripes read as one patch, not many cards |
| `o240_area_frac` | 0.0311 / 0.0318 / 0.0350 | 0.0246 / 0.0254 / 0.0301 | the 240 m cloud covers MORE of the frame, not less |
| `o240_mask_px_patch` | 40139 / 39271 / 43200 | 36047 / 33043 / 40468 | the largest patch grew with the area |
| `bank_paint_share` | 0.9518 / 0.9643 / 0.9175 | 0.9528 / 0.9351 / 0.9005 | and over 0.91 of the paint is in that one patch |

Fewer patches, a bigger patch, and the same share of paint inside it: the join is real, not a mask that stopped
seeing the fringe.

## The frames

o240 frames **342 / 417 / 492** — 75 apart, clearing the owner's 60. t1 sim ticks **557 / 657 / 757**, o240 sim
ticks 249 / 299 / 349: three poses, not one. `pose_error_m` 0.0000 on all twelve stills, every ability call
`(taken)`. The 60 is now the probe's business: `one_frame_fault` reads each still's `frame` from the manifests and
refuses the evidence when two runs' `gas-o240` frames are less than `FRAME_GAP_MIN` = 60 apart, with the owner's
sentence quoted in its docstring. Before this round it was an eyeball on `--calibrate`.

## The re-bar this round refuses

The rule has not moved: a `le` bar is max + spread, a `ge` bar is min − spread, rounded away from the runs. Run it
on these three captures and two bars come out wrong:

| key | runs | spread | the rule gives | verdict |
|---|---|---|---|---|
| `bank_patch_count` | 8 / 7 / 12 | 5 | `le 17` | **REFUSED**: 17 breaks the owner's ceiling of 12 |
| `gas_green_share` | 0.5743 / 0.5363 / 0.6459 | 0.1096 | `ge 0.4267` | **REFUSED**: 0.4267 is under his 0.44 floor |
| `bank_low_share` | 0.0582 / 0.0763 / 0.0663 | 0.0181 | `ge 0.040` | would hold |
| `bank_height_frac` | 0.4282 / 0.3391 / 0.3479 | 0.0891 | `le 0.52` | would hold |
| `o240_area_frac` | 0.0311 / 0.0318 / 0.0350 | 0.0039 | `ge 0.027` | would hold |
| `gas_paint_frac` | 0.0266 / 0.0218 / 0.0190 | 0.0076 | `ge 0.011` | would hold |
| `bank_paint_share` | 0.9518 / 0.9643 / 0.9175 | 0.0468 | `ge 0.87` | would hold |
| `row_dark_frac` | 0.2045 / 0.1728 / 0.0314 | 0.1731 | `le 0.38` | would hold |
| `row_gap_deepest` | 0.5456 / 0.6114 / 0.2909 | 0.3205 | `le 0.94` | would hold |

A bar at 12 would sit INSIDE this build's own scatter (run3 reads 12 exactly), so it would go red on a rerun of
the identical build; a bar at 17 is honest and breaks the owner's number. The two instructions cannot both hold
on this build. Setting the seven that would hold while the owner's key stays at the previous round's 29 would
leave the file half re-barred and still exit 1, so **nothing moved**: every value in `BAR_VALUES` is the previous
round's, as it was in part 1, and the file's comments say exactly that.

## What moved the wrong way

- **The scatter did not shrink with the join.** `bank_patch_count` 7 to 12 is a spread of 5 on a mean of 9: run3's
  cloud is older (o240 sim tick 349) and its fringe has begun to break up again, which is the whole of the
  problem. The lever joined the stripes; it did not make the join tick-independent.
- **`gas_green_share`'s floor fell for the first time** (`min − spread` 0.4459 last round, 0.4267 now). The spread
  widened, not the level: the lean itself rose (0.5363 at worst against 0.4688 two rounds ago).
- **`row_gap_deepest` got much looser**, 0.74 to a would-be 0.94, driven by run3's 0.2909 against run1/2's
  0.55/0.61. Same cause: run3 is a different age of cloud, not a different build.

The next lever is the one part 1 already named: `GasBankLoad` as `max(own, mean)`, which LIFTS the fringe with its
neighbourhood instead of trimming it, so a late tick's thinning edge keeps its cards; or a wider card at the merge
step via `GasBankWidth`. Either aims at the SPREAD, which is now what blocks the owner's 12, not the level.

## An eye check

- `run1/gas-o240.jpg`: at 240 m the gas is ONE lime body lying over the middle of the trench strip, roughly half
  its length. The stripes are gone — the patch reads as a single shape at a glance, which is what the owner
  asked for. It is still FLAT: a painted area on the ground, with a hard straight edge at its left end and no
  height or soft fringe. "Join the stripes" is answered; "a cloud" is only half answered.
- `run1/gas-t1.jpg`: at the game camera the bank is a green sheet over the mud, not a wall. The dark mud shows
  through in broken horizontal lanes across the middle of the frame (`row_dark_frac` 0.2045, `row_gap_deepest`
  0.5456) — the row-pitch lever, out of scope here and still open.

## Checks

- `python probe.py --calibrate` -> exit 0; three runs, three different t1 sim ticks, o240 frames 75 apart, and the
  frames printed per still beside the ticks.
- `python probe.py` -> **exit 1**, `bank_patch_count` and six other keys FAILED against the previous round's bars,
  both isolation cards REJECTED (`cards-smoke` 0.0908 at 3.19x the spread, `cards-puff` 0.1076 at 3.03x). This is
  the honest state of the folder, not a pass.
- `python probe.py --selftest` -> exit 0 (the camera maths, untouched).
- `python probe.py --against ../ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g1-g1-g1-g2-g1-g2` -> exit 1, printing the
  previous round's 13 / 19 / 21.

## This lane does not land

The puff brief `4101179359` is still unanswered. This is evidence, not a landing.

Out of scope here: any further change to `CombatFx.Banks.cs`, the row-pitch lever for `row_gap_deepest`,
reshooting `cards/`, the smoke bank, turning the sheet into a puff, and landing the lane.
