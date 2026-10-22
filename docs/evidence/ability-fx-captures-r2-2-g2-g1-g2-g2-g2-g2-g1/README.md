# ability-fx-captures-r2-2-g2-g1-g2-g2-g2-g2-g1: the new puff shot three times, bars NOT set, 2026-10-22

**`python probe.py` FAILS (exit 1).** The three captures of the new build are here and honest; the bars could not
be re-set, because the green-lean window that this probe's verdict rests on is no longer feasible. The reason and
the owner's choice are in `docs/reference/decisions.md`, "Open: waiting on the owner" (2026-10-22, the close gas
puff and the puff card). `probe.py` is the predecessor's, copied byte-for-byte: `GAS_DIR`, `GAS_COS`, `GREEN_LEAN`,
`CARDS_DIR`, `ZEBRA_DIR` and both rejection checks are untouched, and `BAR_VALUES` still holds the old build's bars.

## What changed in the build

Commit `d391a911a9`: at the close camera the bank is one low puff per 4 m cell (`FlipbookFx.Book.GasPuff`, the
`SmokeBank` drawing tinted chlorine by `ApplyTints`, `GasPuffStand` 0.26, `GasPuffWidth` 5-7 m), not the sheet of
pale green stripes. `CombatFx.Banks.cs` draws `Book.GasPuff` at step 1 and `Book.GasBank` beyond, so draw calls do
not rise.

## The owner's complaint

> Draw one low gas puff at the close camera, not a sheet of pale green stripes, and reshoot three ticks.

## The three captures

Six batch passes of `GasBankStills` (gas and plate x `TW_GAS_EXTRA_FRAMES` 0 / 30 / 60), each with its own
`TW_STILLS_DIR`, all `Passed`, all `pose_error_m` 0.000. The t1 stills land on three different sim ticks, so
`one_frame_fault` accepts them:

| run | t1 `sim_tick` | o240 `sim_tick` | t1 gas cards | t1 gas cells |
|---|---|---|---|---|
| run1 | 557 | 249 | 774 | 775 |
| run2 | 597 | 269 | 743 | 743 |
| run3 | 637 | 289 | 709 | 709 |

## Every key, measured (old bars, for the record)

| key | cmp | old bar | run1 / run2 / run3 | spread | verdict |
|---|---|---|---|---|---|
| `bank_low_share` | `ge` | 0.07 | 0.4712 / 0.2869 / 0.4409 | 0.1843 | OK |
| `bank_height_frac` | `le` | 0.27 | 0.0668 / 0.1880 / 0.0732 | 0.1212 | FAILED (margin 0.0820 < spread) |
| `o240_area_frac` | `ge` | 0.010 | 0.0131 / 0.0150 / 0.0153 | 0.0022 | OK |
| `pose_error_max` | `lt` | 0.50 | 0.0000 x3 | 0.0000 | OK |
| `gas_paint_frac` | `ge` | 0.0130 | 0.0160 / 0.0162 / 0.0153 | 0.0009 | OK |
| `gas_green_share` | `band` | 0.1250-0.2318 | 0.1403 / 0.1245 / 0.1206 | 0.0197 | FAILED |
| `bank_paint_share` | `ge` | 0.12 | 0.1344 / 0.1218 / 0.1401 | 0.0183 | FAILED (margin 0.0018 < spread) |
| `bank_patch_count` | `le` | 25 | 23 / 22 / 17 | 6 | FAILED (margin 2 < spread) |

The look itself reads right: the puff is LOWER than the sheet was (`bank_rise_m` 1.559 / 3.604 / 1.673 against the
sheet's 1.845 / 3.443 / 3.094) and `bank_low_share` is up (0.4712 / 0.2869 / 0.4409 against 0.4371 / 0.2584 /
0.2853). Three of the four failures above are only a bar that has not been re-set to the fresh spread; they would
clear at `bank_height_frac <= 0.3092`, `bank_paint_share >= 0.1035` and `bank_patch_count <= 29`. Those three were
NOT written in, because the fourth cannot be fixed the same way and a half-set table would read as a pass in
waiting.

## Why `gas_green_share` cannot be re-set

A bar must clear the nearest run by at least the runs' own spread, and the floor must leave `cards-smoke` and
`cards-puff` failing by that same spread. With spread 0.0197, the floor has to be at or above
`cards-puff + spread = 0.1273` and at or below `min(run) - spread = 0.1009`. There is no such number: it needs
0.0394 of room between the puff card and the nearest run, and there is 0.0130.

Scanning the lean does not open it (`scan.py` of the leg folder, every `t` from 0.04 to 0.44 in steps of 0.02 over
the same masks):

| `GREEN_LEAN` | runs | spread | `cards-puff` | floor needs |
|---|---|---|---|---|
| 0.12 | 0.2245 / 0.1973 / 0.1987 | 0.0272 | 0.1673 | [0.1945, 0.1701] — empty |
| 0.16 (today) | 0.1403 / 0.1245 / 0.1206 | 0.0197 | 0.1076 | [0.1273, 0.1009] — empty |
| 0.20 | 0.0890 / 0.0794 / 0.0736 | 0.0154 | 0.0711 | [0.0865, 0.0582] — empty |
| 0.30 | 0.0290 / 0.0284 / 0.0224 | 0.0066 | 0.0317 | the runs are now BELOW the card |

The ceiling is squeezed the same way: it must sit at or above `max(run, cards-gas) + spread = 0.2387` and at or
below `min(zebra) - spread = 0.2249`.

The cause is the look change itself, and it is not a flaw in the look: the new close card borrows the smoke
mound's drawing, so its paint leans green about as much as a smoke or puff card does. The old window was measured
when the close frame drew `Book.GasBank`'s own ink. The isolation cards in `CARDS_DIR`
(`../ability-fx-captures-r2-2-g2-g1/cards/`) are all from that older build, and the plan put a fresh
`cards-gaspuff` out of scope — which is exactly the thing the window now needs.

## What is here

`run1`, `run2`, `run3`: `gas-t1`, `gas-t1-plate`, `gas-o240`, `gas-o240-plate` as JPGs under 400 KB with their
sidecars and both manifests, all written by `probe.py --pack`. `probe.json` is this run's measurements. No
`cards/` (the in-folder look report prints `no cards/`); the card REJECTION check reads `CARDS_DIR` directly.

## Not done on this leg

Step 9 (re-set every bar) and step 10 (cite this probe in `docs/reference/tasks.md` line 142) are undone: citing a
red probe as the guard of that task, and setting four of eight bars, would both be worse than leaving them. The
old path is still cited there.
