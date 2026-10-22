# ability-fx-captures-r2-2-g2-g2-g2-g1-g1-g2-g2-g1-g1: merge the bank LOW, 2026-10-22

**Part 1 of 2, and it ends BLOCKED on the owner.** The skirt was built and fitted by capture, three candidates,
and **none of them holds the owner's `green_low_share` >= 0.30 floor**, so no bar is set from them, the three
keeper captures were not shot, and `python probe.py` does not exit 0 here. What this part did prove is the
reason, and it closes the last SHOW knob: *the merge is not bought by how tall a card is drawn.*

## The owner's complaint

> At the close camera the chlorine bank is still about 24 separate flat green cards with mud in the gaps (gas-t1
> run1, run2 and run3, frames 804, 819 and 834, fifteen frames apart, so one pose). `t1_visible_clouds` is 24, 24
> and 23 and the ceiling of 26 sits just above that frozen spread, so the probe exits 0 while a player does not
> see one cloud. Reshoot three captures at least 60 frames apart. One bank has to read as one soft mass. Keep the
> `green_low_share` floor of 0.30. A field of cards must fail.

## The frame guard (done, and it is real)

`probe.py`'s `same_frame_refusal` is now a GAP refusal: any two runs whose `gas-t1` or `gas-o240` frames are
closer than `FRAME_GAP_MIN = 60` fail with a printed line, and the identical-frame case keeps its own message.
`FRAME_GAP_MIN` is written into `probe.json`'s `bars` block. The parent's own 804 / 819 / 834 - 15 frames apart -
fails it, which is exactly the owner's point: those were one pose photographed three times. `GasBankStills` clamps
the wait to 120..300 frames, so waits 170 / 235 / 300 are the three gaps of 65 that satisfy it.

Selftest case (g) checks it on hand-made frame maps, no image at all: 800 / 815 / 830 refused (15 and 15 apart),
800 / 865 / 930 passed (65 apart), 800 / 800 / 930 refused as identical.

## The skirt: built, fitted by capture, and refused by the floor

Width (the grandparent) and density (the parent) both merge the bank only by piling green HIGH, which breaks the
0.30 floor. So this part tried the opposite: a SECOND card per cell in the same `gasCards` list and the same book
(no new draw call) - wide, flat and ground-hugging, bridging the `GasPuffBlock` 3 x `FieldCellSize` 4 = 12 m lane
that splits the mask with pixels that sit under a man's head.

One `GasBankStills` gas + plate pass each at `TW_GAS_WAIT_FRAMES` 170, measured with this folder's `probe.py`.
Bars carried from the parent in brackets. The merge wanted `t1_visible_clouds` 1-3 and `t1_top_share` over 0.80.

| `GasPuffSkirtWide` / `SkirtStand` | skirt drawn | `t1_visible_clouds` | `t1_cloud_count` | `t1_top_share` | `green_low_share` [>= 0.30] | `green_height_frac` [0.08-0.15] | `green_rise_m` [>= 2.4] | `green_soft_share` [>= 0.14] | `green_area_frac` | `green_px` |
|---|---|---|---|---|---|---|---|---|---|---|
| no skirt (the parent, kept) | - | 24 | 38 | 0.2908 | 0.3045 | 0.0836 | 2.653 | 0.1448 | 0.0758 | ~240k |
| 1.6 / 0.17 | 14-23 m wide, 2.4-3.9 m tall | 17 | 40 | 0.4734 | **0.1915** | **0.1584** | 4.449 | 0.1411 | 0.1637 | 318702 |
| 2.2 / 0.11 | 20-28 m, 2.2-3.1 m | 12 | 41 | 0.6177 | **0.2100** | **0.2031** | 5.489 | **0.1330** | 0.2285 | 340858 |
| 2.2 / 0.07, KEPT of the three | 20-28 m, 1.4-2.0 m | 16 | 52 | 0.5368 | **0.2482** | 0.1492 | 4.030 | 0.1676 | 0.1713 | 294111 |

1.6 / 0.17 is the plan's candidate A, 2.2 / 0.11 its candidate B. 2.2 / 0.07 is this leg's own: a skirt whose
own top is **under a 1.8 m head everywhere**, which is the strictest form of the plan's idea. It is the only row
that holds the 0.15 height ceiling and it comes closest to the floor - and it still reads 0.2482 against 0.30,
with 16 visible clouds against the 1-3 the merge needs.

### Why, and why this is the end of the SHOW knobs

**The merge is not bought by how tall a card is drawn.** Once the cards merge, ONE connected mass spans the
field; perspective then puts the far cells' pixels high in the frame, so `green_low_share` (the share of green
under a 1.8 m head) falls and `green_height_frac` (the largest blob's screen height) rises *whatever shape the
card has*. The skirt at stand 0.07 is physically under a man's head at every cell and the floor still breaks,
which rules out the whole family: width, density, the parent's re-fit and now height. Candidate C (1.3 / 0.24,
taller and narrower) was not shot: it fails the merge in the direction already measured twice, and the floor
breaks harder the taller the skirt is drawn.

### Cards and draws

T1, from the skirt build's `manifest.json`: the skirt doubles `gas_cards` into the same list and the same
`DrawPacked` call, so draw calls do not move. The budget was never the question here; the floor was.

## What is in the tree, and what is not

- **In:** the skirt (`CombatFx.GasPuffSkirtWide` 2.2 / `GasPuffSkirtStand` 0.07, with the table above on the
  constants), its two guard asserts in `BurstRecipeTests.TheCloseGasPuffStands_AndTheMergedOutSheetStillLiesFlat`
  (every skirt bridges its 12 m lane; the skirt stands lower than the card over it), this `probe.py` with the gap
  refusal, and the measured line under the open row in `docs/reference/decisions.md`.
- **Not in:** any bar from a skirt capture, and the three keeper captures. `BAR_VALUES` is the parent's, carried
  byte for byte - including `t1_cloud_ceiling` `lt` 26 and `green_low_share` >= 0.30 - so `python probe.py` on
  three skirt captures would FAIL the floor. **A bar is never set from a capture that breaks the owner's own.**

## The decision, and the default taken

It is his standing open row, "The close gas: one cloud, or gas that still pools under a man's head?" (option (a)
keep the 0.30 floor, option (b) drop it to about 0.15). The default taken here is (a) again: the floor is his, it
is not lowered, and the skirted build is **not landed**. Only (b) can take the close bank to one cloud.

## Checks run

- `python probe.py --selftest` -> exit 0, cases (a) to (g); (g) is new.
- `python probe.py --measure <each fit capture>` -> the table above.
- `python Tools/tw run run_tests -- --mode EditMode --filter TW.Tests.BurstRecipeTests` -> exit 0.
- `python probe.py` -> exit 1, "three runs needed": the keeper captures are part 2's and were not shot, because
  no fitted pair earned them.
