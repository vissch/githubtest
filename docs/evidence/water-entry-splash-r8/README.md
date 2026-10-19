# water-entry-splash-r8 — the baseline round

Harsh critic r1 scored r7's water entry **61/100**. This folder is r8's answer, and this page is the first half of
it: the **unchanged build, photographed three times, measured, and written down before a single line of the look
was touched**. That order is the owner's rule for the round, and it is the only thing that makes the numbers later
on this page mean anything — a bar set after seeing the result is not a bar.

## What the critic said, and what the unchanged build actually scores

| The critic's claim | The number | Three baseline runs | The bar | Verdict |
|---|---|---|---|---|
| the ring is a **filled disc**, not a ring | `centre_rim_ratio` | 1.004, 1.008, 1.000 | ≤ 0.60 | **true — work to do** |
| the ring **does not grow** between land and +0.3 s | `ring_growth` | 1.448, 1.448, 1.448 | ≥ 1.50 | **true — work to do** |
| **only T1 was shot** | `bands_monotone` | false, false, false | true | **true — work to do** |
| the crown is **~60 px**, too small | `crown_height_m` | 3.33, 3.33, 3.33 m | ≥ 1.30 | **rebutted** |
| the crown has **no fall-back shown** | `crown_fall_ratio_06` | 0.081, 0.089, 0.095 | ≤ 0.50 | **rebutted** |
| the crown **will not read far out** | `o120_crown_contrast` | 0.325, 0.328, 0.321 | ≥ 0.09 | **rebutted** |

Three of the six were right and three were not, and the three that were not are left **exactly as they are**. No bar
was moved to make them pass and no change was invented to satisfy a claim the build already answers. The crown
r7 built genuinely stands about 3.3 m out of the water, genuinely still reads at 120 m zoom, and is genuinely 92%
gone by land + 0.6 s. That is what the critic was told, with numbers.

The three that were right are about **the ring, and only the ring**, and they say the same thing three ways: what
the shader draws under the body today is a grey ellipse of roughly constant radius with no hole in it.
`base-land.jpg` and `base/run-1/post.jpg` show it plainly — the disc under the crown has a faintly brighter edge
and a middle exactly as bright as the edge (centre luma 0.3291 against rim luma 0.3277 at land + 0.05 s).

## The band ladder, and the thing it exposes

`crown_contrast` at the five gym bands, off **one** frozen land + 0.05 s world (run 1; runs 2 and 3 agree to
within 0.007, which is `band_tol`):

| T3 (7.5) | T2 (16) | T1 (30) | O120 | O240 |
|---|---|---|---|---|
| 0.183 | 0.251 | 0.294 | 0.325 | 0.374 |

The ladder runs **the wrong way**. The owner's standing rule is that a look never loses detail as the camera comes
in, and this one loses a third of its contrast between O240 and T3. The reason is not that the splash is dimmer
close up — it is that close up the camera resolves it, and a card that is a solid white blob at 240 m turns out to
be a soft, half-transparent smear at 7.5 m, so the brightest tenth of the same patch of world is lower. A thing
that holds up only until you can see it is the definition of what the close bands are for. `bands_monotone` is in
the pass condition and it is false on every baseline run; fixing it means giving the crown something to be at T3,
not making the far bands worse.

## How to read this folder

| | |
|---|---|
| `probe.py` | every measurement, and the reasoning for each one, in its docstring |
| `probe.json` | the numbers; `runs.base` holds all three runs in full, nothing summarised away |
| `frames.txt` | which still is which, and the achieved moment of every freeze |
| `base-*.jpg` | run 1 at the six moments and bands, for the eye |
| `base/run-N/` | all ten kept stills per run, each with its own sidecar, plus `timings.txt` and `stage.log` |

**The JPGs are for the eye only.** Every number was read off the PNGs, which live in the git-ignored
`trench-warfare-3d/Captures/water-entry-r8/run-N/`: JPEG chroma subsampling moves luma by more than the 0.02
threshold the probe counts with, so measuring a JPG would measure the compressor.

## How the stills were made

`trench-warfare-3d/AgentScripts/water_entry_fling_r8.cs`, in Play in `GreyboxCorridor` at night, `CaptureRig`
rendering. A body is flung off the dry bank at (12, 128) into the knee-deep ford at (15, 130) through the two
shipped lines `CombatFx.Deaths.cs` runs for a flung body, so what is photographed is the shipped path and not a
staged pose. `fly.y` is bisected until the flight is **exactly 43 frames** at 1/60 s, and every freeze is scheduled
on game time from the landing instant, so all four moments land on their spec to the frame: `timings.txt` reports
−0.200 / +0.050 / +0.300 / +0.600 in all three runs, typed by nobody.

The sky and water clocks are pinned, rain is off and the sim is frozen, so nothing in the picture moves but the
body and its splash. Each moment is photographed three times from the same frozen world — lit, with the ring slots
parked, and with the water entry lifted out entirely — which gives the ring mask and the card mask separately. The
five bands are shot from that **one** freeze, so the only thing that differs between them is the camera.

**Capture noise is measured, not assumed:** after the four moments, the scene is put back to before the throw, the
RNG is re-seeded with the same seed and the identical throw is made again. The difference between the two landing
frames is the splash's own repeatability — **1, 9 and 1** changed pixels across the three runs. Play is stopped and
restarted between runs and never reused; r7 learned that the hard way, measuring 1,030 against 2 when it was.

## What r8 does next

Fix the ring, and only the ring: a hollow centre that opens outward. Then three runs of the changed build through
`probe.py --fixed`, which must clear all five owner bars **and** the ladder. The bars for the three true claims sit
outside the baseline's spread by construction (0.60 against a measured 1.000–1.008; 1.50 against a measured
1.4483 on all three runs), so clearing them cannot happen by luck.
