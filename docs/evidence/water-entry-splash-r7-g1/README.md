# water entry splash, round 7 g1 — body, crown, fallen foam

The owner read r7's stills and said: "The entry is a brighter copy of the old wisp. run-6/land.png, run-9/land.png
and run-10/land.png are one blue-white column, the same shape as the dim column in run-1/land.png, and pre.png
shows no body entering the creek. Shoot a splash a player can name at zoom 30 at night, body then crown then
fallen foam, three times. Bar outside the spread of three captures of the current build."

He is right on every count, and all three are now measured rather than argued:

| What he said | Measured on r7's committed stills (commit 0fb59286ce) |
|---|---|
| one blue-white column | the crown mask is 3.4–3.5 m wide by 5.9 m tall, aspect 0.58 |
| the same shape as the dim column | r7's own baseline run-1 reads 0.32 at the same moment |
| nothing has fallen back | at +0.3 s only 29 % of the mask is within 0.5 m of the water, and its top is still 5.77 m up |
| no body entering the creek | `pre.png` minus `pre-nosplash.png` was exactly ZERO pixels: at land − 0.2 s the man is still over the dry bank and the ring and cards are still queued |

## The three beats, and the bars

The moments are now his three beats, and each one is photographed with the references that let it be measured at
that same frozen instant (`AgentScripts/water_entry_fling.cs`):

| Beat | Moment | Stills | The key it proves |
|---|---|---|---|
| body | land − 0.05 s | `body.png`, `body-nobody.png`, `body-nosplash.png` | `contact_px` — is the water already breaking where he goes in; `body_px` — the man himself reads, over the creek |
| crown | land + 0.05 s | `crown.png`, `crown-noring.png`, `crown-nosplash.png` | `crown_aspect` — wider than tall, a crown instead of a column |
| foam | land + 0.3 s | `foam.png`, `foam-noring.png`, `foam-nosplash.png` | `foam_flat`, `foam_top_m` — the foam has fallen back onto the water |

| Bar | Owner's bar | Baseline, worst of three captures of the current build |
|---|---|---|
| `contact_px` | ≥ 250 | 0, in all three (structural: the ring and the cards are queued for the landing instant) |
| `crown_aspect` | ≥ 1.0 | 0.596 (0.579 / 0.584 / 0.596) |
| `foam_flat` | ≥ 0.70 | 0.294 (0.291 / 0.293 / 0.294) |
| `foam_top_m` | ≤ 2.5 m | 5.77 m, in all three |

Carried from r7 as GUARDS, which the current build already clears because they are what r7 won — the fixed build
must still meet them, and the baseline is not asked to sit below them: `ring_only_px` ≥ 300 (baseline 1287),
`crown_contrast_land` ≥ 0.15 (0.2935) and `crown_contrast_post` ≥ 0.09 (0.2092).

## How it is shot and measured

`probe.py`'s own docstring is the full account: the staging, why each moment carries its own `-nosplash` reference
instead of an earlier plate, the four measuring regions and why the shape keys need a box taller than r6's 6 m
window (in that window the splash is clipped at its 3 m top and every run's height reads 3.02 m, baseline and
fixed alike, so an aspect taken there is the window's shape and not the splash's). The luma threshold is r5's
0.02, never raised. The noise floor is a second live throw with the same seed, from a scene put back, not a
re-render of a frozen frame; one Play session per run, because two runs in one session measured `noise_px` ≈ 1030
against 1.

```
python docs/evidence/water-entry-splash-r7-g1/probe.py --base  run-1 run-2 run-3   # the unchanged build, first
python docs/evidence/water-entry-splash-r7-g1/probe.py --fixed run-4 run-5 run-6   # the fix
python docs/evidence/water-entry-splash-r7-g1/probe.py                             # re-measure the committed runs
```

## Where this stands

`run-1`, `run-2` and `run-3` are three captures of the UNCHANGED build, shot before anything moved, and the
baseline block in `probe.json` is green: every bar of this round sits outside their spread
(`base_below_bars: true`). The fix itself — the art and code change that turns the column into a crown with foam
that falls back — and its three captures `run-4`, `run-5`, `run-6` are the next step. Until they exist, the
no-argument run of `probe.py` reports that `probe.json` names no `fixed_runs` yet and exits 2.
