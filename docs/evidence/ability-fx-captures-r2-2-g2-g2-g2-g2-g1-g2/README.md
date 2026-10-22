# ability-fx-captures-r2-2-g2-g2-g2-g2-g1-g2: the close gas is a standing cloud, 2026-10-22

**`python probe.py` clears, exit 0.** Part 1 of 2: the in-game close band is the smoke cloud's drawing in green
and its height is barred from BELOW. `cards/` is still the old gas card, so the gas/smoke ratios stay unbarred.

## The owner's complaint

> The close shot is still a sheet of pale green stripes. `green_height_frac` is 0.0209, 0.0209 and 0.0192, and a
> ceiling of 0.15 clears that sheet. `cards-gas` is a flat puddle, rise 0, relief 0.0849, while `cards-smoke` is
> a standing cloud, relief 0.2178. `green_area_frac` was lowered from 0.005 to 0.0030 and all three runs read
> 0.0034, spread 0. Draw the close gas as that smoke cloud, with a rim and a rise, and put the height floor
> outside the stripe sheet.

He is right twice over. The gas sheet draws a flat puddle, and the probe's height bar was a **ceiling**: a pale
stain 2 % of the frame tall passed `le 0.15` exactly as easily as a real cloud would, so the bar could not tell a
cloud from a sheet, and the look was free to stay flat.

## What changed

- **`FlipbookFx.cs`, the gas row.** It keeps its **ordinal** and all of its green — `Tint`, `Lit 0.75`,
  `High 0.60`, `Low 0.17`, `Mood`, `Erode`, `Deep`, `Bank` — and only its drawing changes: `Name = "SmokeBank"`,
  `Frames = 22`, `Ink = (0.03, 0.87)` (SmokeBank's measured window). A new `Book` entry was deliberately **not**
  added: `Sheet.Name` is just which PNG to load, the Gas book already reused `Puff.png` that way, and a new
  ordinal would drag in FlipbookOrdinalTests, KnobsTests' Deep list and a tint row for no gain in the look.
- **`CombatFx.Banks.cs`.** `GasBankFirst/Last` 17..28 → **12..20.5** (the smoke sheet has 22 frames, so the old
  window ran off the end). New `GasBankStand = 0.42` and `GasBankSink = 0.20`: the **close** card (`step == 1`)
  is drawn `width * GasBankStand` tall with its bottom edge at `Ground - GasBankSink`, so a 7 m card stands
  2.94 m — over a man's head, and because the new drawing fills its cell that is the cloud's real height, not a
  third of it. From the overview (`step == 2`, zoom ≥ `BankMergeZoom`) **nothing changed**: still the flat
  `GasBankSquash` blanket at `Ground - 0.35`, so the one-cloud O240 result that landed last unit stands.
- **`FlipbookOrdinalTests.cs`.** `EveryBookFromJetOn_DrawsTheSheetNamedAfterIt` asks every row to draw the PNG
  named after its book, and it already carries one named exception (`Book.MineSmoke` draws `WreckSmoke`). The gas
  row gets the second one, in the same shape and with the reason in its message.
- **`BurstRecipeTests.cs`.** `TheGasBankLiesOnTheGround_NotOverAMansHead` encoded the rule the owner has now
  reversed. Renamed `TheCloseGasStandsAsACloud_TheOverviewBankStillLies`: close, `7 * GasBankStand` is above
  1.8 m and under 7 m (a cloud, not a wall); from the overview `7 * GasBankSquash / 3` is still under 1.8 m and
  over 0.6 m, the sentence that test already proved.
- **`probe.py`.** `green_height_frac` flips `le` → **`ge`**, with the floor set outside the stripe sheet's
  0.0192-0.0209. `measure_t1` gains `green_relief` (stdev of the masked pixels' luma over their mean — the same
  four lines `card_rise_m` already used, so T1 and `cards/` read on one scale) and `green_rim_ratio` (mean luma
  of mask pixels with an off-mask 4-neighbour over the mean of the rest). `green_relief` is **barred** as a
  floor; `green_rim_ratio` is a printed diagnostic, **barred on nothing**, because which way this art's rim
  falls is not predictable and a bar on it would be a guess. The camera maths, `greenness`, `green_mask`,
  `components`, `measure_o240`, the frame guard and `spread_check` are untouched.
- **`run1/`, `run2/`, `run3/`** — three fresh `GasBankStills` captures of the committed tree,
  `TW_GAS_WAIT_FRAMES` 200/215/230, each the four stills with sidecars and manifests.

## `GasBankStand` was measured, not picked

The plan opened at 0.9. Three calibration shots say that is a wall:

| `GasBankStand` | `green_rise_m` | `green_height_frac` | `green_low_share` (floor 0.30) | `green_relief` |
|---|---|---|---|---|
| 0.90 | 7.673 | 0.3567 | **0.1124** — a wall | 0.1989 |
| 0.55 | 4.543 | 0.1671 | **0.1882** — a wall | 0.1656 |
| 0.30 | 0.549 | **0.0166** — under the old stripe sheet | 0.9206 | 0.0941 |
| **0.42** | 1.709 | 0.0570 | 0.3522 | 0.1168 |

0.42 is the only one of the four that clears both ends: its mass stays low enough for the owner-decided
`green_low_share` floor of 0.30, and it rises nearly three times the stripe sheet's height.

## The bars

Checked twice, as always: all three runs on the pass side, AND the nearest value clears the bar by at least the
three runs' own spread. Every carried bar was re-derived — the art changed, so every key moved.

| key | cmp | bar | the three runs | spread | margin | why |
|---|---|---|---|---|---|---|
| `green_low_share` | `ge` | 0.30 | 0.3522, 0.3451, 0.3493 | 0.0071 | 0.0451 | owner-decided, and it now bites: the flat sheet was saturated at 1.0000, the cloud reads 0.35 |
| `green_height_frac` | `ge` | 0.04 | 0.0570, 0.0552, 0.0586 | 0.0034 | 0.0152 | **turned round into a floor**, set clear of the stripe sheet's 0.0192-0.0209 |
| `green_relief` | `ge` | 0.10 | 0.1168, 0.1202, 0.1167 | 0.0035 | 0.0167 | **new**: above the old gas puddle's 0.0849, on the way to the smoke card's 0.2178 |
| `green_rise_m` | `ge` | 1.50 | 1.709, 1.669, 1.756 | 0.087 | 0.169 | re-derived: the old bar 0.30 was set against a 0.359 m sheet |
| `green_area_frac` | `ge` | 0.07 | 0.0872, 0.0931, 0.0867 | 0.0064 | 0.0167 | re-derived: a cloud fills the frame where the sheet barely stained it (0.0034) |
| `o240_green_area_frac` | `ge` | 0.0025 | 0.0036, 0.0037, 0.0042 | 0.0006 | 0.0011 | re-derived: the T1 call's gas drifts into the far shot too, so the new art moves this |
| `o240_large_cloud_count` | `le` | 1 | 1, 1, 1 | 0 | 0 | the far camera still sees exactly ONE cloud |
| `o240_large_share` | `ge` | 0.70 | 0.7822, 0.8605, 0.8573 | 0.0783 | 0.0822 | re-derived: the cloud IS the green now, not a patch inside a wash |
| `o240_top_blob_px` | `ge` | 2800 | 3348, 3436, 3840 | 492 | 548 | re-derived; pairs with the count bar, so zero clouds still fails |
| `pose_error_max` | `lt` | 0.50 | 0.0000, 0.0000, 0.0000 | 0.0000 | 0.5000 | `GasBankStills.PoseBar` |

## The look report (unbarred)

| reading | value | note |
|---|---|---|
| `green_rim_ratio` (T1, in game) | 0.9018, 0.9007, 0.9033 | under 1: the cloud's silhouette is its **dark** rim, the lit mass sits inside it. Reported, never barred |
| `cards-gas` relief | 0.0849 | `cards/` is **carried from `-g1`** and still shows the OLD gas card: `GasBankCards` hand-packs its own card and was not reshot |
| `cards-smoke` relief | 0.2178 | |
| gas/smoke relief ratio | 0.3898 | stale for the same reason, so it is **not barred** — reshooting `cards/` and barring that ratio is part 2 |

The in-game `green_relief` (0.1168) is the live reading of the new art and is the one under a bar.

## Status: done (part 1)

`python probe.py` exits 0, `OK`. `python probe.py --selftest` exits 0 (camera hand calculation, 0.34311 m).
`TW.Tests.BurstRecipeTests` 19/19 green with the renamed gas test. These are regression bars from this build's own
spread, as in every predecessor of this chain: they prove the close gas now stands as a cloud with a rise and a
dark rim, not that `cards/` has caught up with it.
