# ability-fx-captures-r2-3-g2-g2-g1-g1-g1-g2 — no fire and no chimney smoke

The owner's word on the last round: "remove the fire and smoke from the chimneys and the feet". This round
changes the HARNESS only, not the look of the gas and not a single bar. Three seeds, both passes, on this lane's
tip; every bar of `...-g1-g1-g1` still stands, is still checked, and still passes unmoved.

```bash
TW_STILLS_DIR=<dir> Unity.exe -batchmode -projectPath trench-warfare-3d -runTests -testPlatform EditMode \
  -testFilter GasBankStills -testResults Logs/g.xml -logFile Logs/g.log      # the gas pass
TW_GAS_PLATE=1 TW_STILLS_DIR=<dir> ...                                       # the plate pass, same script, no gas
python docs/evidence/ability-fx-captures-r2-3-g2-g2-g1-g1-g1-g2/probe.py --pack <dir>
```

## What the owner was looking at

The gas stills are shot in the night corridor (`GreyboxCorridor`, `PinnedClock = 30`), so `NightLights` built
exactly the two things he named:

- **the chimneys** — `NightLights` fills `SceneHooks.SmokeSources` from each composed dugout site, and
  `CombatFx.Ambient` puffs a smoke chunk at each one every 0.45 s;
- **the feet** — `flameFeet` / `flameShapes` (fires on broken trees and stumps, ten more from `MoreFires`, the
  torch wicks) become one `TW/Flame` mesh, plus a fire glow card, a point light and a fire pool.

## What changed

Both are suppressed **in the harness, not in production** — the same rule the side band's `drawers_off` already
follows, and for the same reason: the suppression must be identical in the gas pass and the plate pass, or the
plate stops subtracting.

| Change | Where |
|---|---|
| `TW_KNOBS` zeroes `maxFires`, `maxTorches` and `moreFires` **before Play**, so the knobs are read in the fresh domain | `GasBankStills.cs` |
| `SceneHooks.SmokeSources` cleared at the shot, so `CombatFx.Ambient` has nothing to puff from | `GasBankStills.cs` |
| The built `Flames` child hidden as a belt, in case a knob did not take | `GasBankStills.cs` |
| Both asserted per still; every manifest row carries `fires_off` and `smoke_sources` | `GasBankStills.cs` |
| `probe.py` copied **unchanged** from `...-g1-g1-g1` — no measure change, no bar moved | this folder |

## The stills: the point of the round

All 18 rows of `manifest.json` + `manifest-plate.json` read `smoke_sources: 0` and `fires_off: true`.

```
python -c "import json;rows=json.load(open('manifest.json'))['stills'];print(all(r['smoke_sources']==0 and r['fires_off'] for r in rows))"  -> True
```
`gas-side-s0.jpg` is the clearest look: the broken trees and stumps across the whole field carry **no flame and
no glow card**, and **no smoke column rises from any dugout**. The sky keeps its haze, which is fog, not smoke.

**Honest about what is still lit:** warm light still falls on the sandbag parapet and the far horizon. That is
the lanterns and the trench lamps, and the horizon fires behind the fog bank — out of scope by the plan, because
the owner named the chimneys and the feet. If he wants those off too, that is the next round's word.

## Every bar, this capture

```
t1_changed_frac    want >= 0.08    worst 0.2094    OK
texture_var_drop   want >= 0.4     worst 0.4474    OK
o240_hue_dist_deg  want >= 108.0   worst 133.44    OK
side_face_px       want >= 30000   worst 34233     OK
side_crown_m       want >= 1.9     worst 2.493     OK
side_dome_ratio    want >= 0.88    worst 1.2736    OK
texture_var_drop   want <= 0.7     worst 0.6338    OK
side_outside_frac  want <= 0.4     read  0.2823    OK
side_face_spread   want <= 3.55    read  3.1645    OK
```

Per seed:

| reading | s0 | s1 | s2 |
|---|---|---|---|
| `t1_changed_frac` | 0.2094 | 0.2429 | 0.2410 |
| `texture_var_drop` | 0.5014 | 0.6338 | 0.4474 |
| `o240_hue_dist_deg` | 134.93 | 137.90 | 133.44 |
| `side_face_px` | 95654 | 34233 | 108330 |
| `side_crown_m` | 2.572 | 2.493 | 2.639 |
| `side_dome_ratio` | 1.5346 | 1.2736 | 1.4590 |
| `side_height_m` | 1.983 | 1.714 | 2.250 |
| `pose_err_m` | 0.0000 | 0.0000 | 0.0000 |

## The risk the plan named: did a bar shift?

The fire lights and the glow cards lit the field, so the field is darker without them. The plate loses them too,
so the subtraction bars should hold — and they do. Against the last round, with the same `probe.py`:

| reading | `...-g1-g1-g1` (fires on) | this build (fires off) |
|---|---|---|
| `texture_var_drop` worst | 0.6472 | 0.6338 |
| `texture_var_drop` sparsest | 0.4302 | **0.4474** |
| `o240_hue_dist_deg` worst | 133.98 | 133.44 |
| `side_crown_m` worst | 2.496 | 2.493 |
| `side_dome_ratio` worst | 1.2726 | 1.2736 |
| `side_face_px` worst | 34245 | 34233 |
| `side_outside_frac` | 0.2539 | 0.2823 |
| `side_face_spread` | 3.1969 | 3.1645 |

**Nothing moved that matters.** The two absolute-colour bars the plan flagged both held: `o240_hue_dist_deg`
drifts 0.54 deg (still 25 deg clear of its 108 floor), and `texture_var_drop` actually gained headroom at both
ends — the sparsest seed rose 0.4302 -> 0.4474 off the 0.40 floor, the thickest fell 0.6472 -> 0.6338 off the
0.70 ceiling. `side_outside_frac` rose 0.2539 -> 0.2823, well inside its 0.40 ceiling; that is the darker field
shifting the silhouette cut a little, not the bank changing. **No bar was widened.**

## What is still not answered

- **The owner has not seen these stills.** Whether the chimneys and the feet are now quiet enough is his call,
  and it is the only thing that closes this round.
- The lanterns, the trench lamps and the horizon fires are untouched, by the plan. They are the obvious next
  thing he may name.
- The suppression lives in the harness. Production still builds every fire and every smoke source; if the owner
  wants them gone from the GAME, that is a different change on a different lane.
