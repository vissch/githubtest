# ability-fx-captures-visual-review-3: the gas chain, drawn together, 2026-10-27

The owner asked to "show me visual results" of the ability-FX fix chain. These are **the stills the chain already
committed**, put on three sheets. Nothing here is re-measured and nothing is re-rendered: every number beside a
panel comes from that step's own `probe.json`, with that step's own bar.

| file | what it shows |
|---|---|
| `chain-t1.jpg` | the close band, one panel per step: each folder's `run1/gas-t1.jpg` (the oldest, `r2`, has no `run1/`, so its own `gas-t1.jpg`) |
| `chain-o240.jpg` | the far band, the same eleven steps, from `gas-o240.jpg` |
| `diff-now.jpg` | the build as it stands: the newest folder's `run1/gas-t1-plate.jpg` (gas off), `run1/gas-t1.jpg` (gas on), and `\|gas - plate\| x 4` clipped to 255 |

Panels are in chain order (shortest folder name first, so a fix unit never comes before its parent). Under each
panel: `green_low_share`, `t1_visible_clouds`, `o240_visible_clouds`, and the bar guarding it where that step had
one (`o240_visible_clouds` is barred by `o240_visible_floor`). A step whose probe never measured a key says
"not measured yet" - an old key is never read as a new one.

Rebuild them:

```bash
python docs/evidence/ability-fx-captures-visual-review-3/sheet.py   # read-only, exits 0
```

## The chain, one line per panel

1. `r2` - the chlorine bank, 2026-10-19. No bank in the frame at all.
2. `g2` - a gas measure three captures can agree on, 2026-10-20.
3. `g2-g2` - count the green, bar it outside three different frames, 2026-10-21.
4. `g2-g2-g2` - bar the rise, count O240's clouds, 2026-10-21.
5. `g2-g2-g2-g1` - a floor under the height, and the puff cut to fit it, 2026-10-22.
6. `g2-g2-g2-g1-g1` - fewer, taller close puffs instead of a sheet, 2026-10-22.
7. `g2-g2-g2-g1-g1-g2` - overview clouds with gaps, a count ceiling, real digits, 2026-10-22.
8. `g2-g2-g2-g1-g1-g2-g2` - one close cloud with a soft edge, 2026-10-22.
9. `g2-g2-g2-g1-g1-g2-g2-g1` - one bank is one cloud, 2026-10-22.
10. `g2-g2-g2-g1-g1-g2-g2-g1-g2` - a cloud at the overview a player can see, 2026-10-22.
11. `g2-g2-g2-g1-g1-g2-g2-g1-g2-g1` - the overview cloud, barred, 2026-10-23.

## Where the build stands

Quoted from the newest folder's README:

- **`o240_visible_floor` >= 6 is green at 9 / 10 / 10**, where the pre-widening build read 0 / 0 / 0. Panel 11 of
  `chain-o240.jpg` is that frame: green patches a player can see at the overview.
- **`green_low_share` >= 0.30 still FAILS at 0.2343 / 0.3045 / 0.3067.** It is an owner-decided bar and was not
  moved. The question is open in `docs/reference/decisions.md` ("The close gas: one cloud, or gas that still pools
  under a man's head?"). Its caption on the sheet is printed in red.

## Nothing was reshot

The project was free, but the newest folder's recipe is a three-capture set - six batch-editor runs (a GAS and a
PLATE pass at waits 140 / 200 / 260), hours of wall clock, more than one relay leg has. No `reshot/` folder, and
the committed stills above are the deliverable either way: they are the chain's own renders of the code this lane
carries.
