# 18 — Biomes: the lava field and the winter mountain

Two new battlefields beside the night mud field, specified from the owner's reference art
(`Lava_planet_trench_warfare_2K`, `Snowy_mountain_winter_trench_war…_2K`, both 2026-09-22).

**What the references are.** Both are generated images built over one real screenshot of the game — the same HUD,
the same silver count of 3,559, the same "Men 0 Enemy 130", the same `DEFEAT: your HQ has fallen`, and the same
compile error along the bottom edge (garbled in the winter one, which is how you can tell). So they are **target
looks, not captures**. Nothing in them is evidence that anything already works. They are a brief.

**The governing constraint.** Same trench layout, same composer output, same 3,000 men, same low-spec target. A
biome must be a **profile**, not a fork: if adding the third one means touching the rendering path again, the
design is wrong. Everything below is chosen with that test in mind.

---

## What the two references actually demand

### Lava

Read the image for where the light comes from, because that is the whole problem. **The key light is under the
floor.** The lava is the brightest thing in frame and it is below everything, so every man, sandbag, duckboard and
burnt stump is lit from beneath and silhouetted against its own ground. The night look's moon is a top light; this
is its exact inverse, and nothing in the scene may keep a top-lit look or it will read as a mud field with an
orange filter over it.

| # | What the reference shows | What it needs technically |
|---|---|---|
| L1 | Lava as a flowing liquid: bright yellow-white cores, darker crust plates drifting on it | An emissive liquid surface with scrolling flow, a crust mask, and a hot/cool ramp. The water surface is the nearest existing thing; it is not the same thing, because water reflects and lava emits |
| L2 | Black basalt ground with glowing cracks between plates | A crack mask on the terrain driving emission. Cracks must be *between* crust cells, not painted noise, or it reads as glowing dirt |
| L3 | Everything rim-lit from below in orange | A bottom-up light term: an upward-facing ambient the opposite way round from the sky term, tinted by the lava, and stronger near hot ground |
| L4 | Magenta fog filling the scene, and it is BRIGHT — lit from within | Fog colour cannot be a flat tint. It has to be brighter low down where the lava lights it, which is a height-dependent fog colour, not just a height-dependent density |
| L5 | Charred timber spikes everywhere, black with glowing edges | Prop tint to near-black plus an edge/fresnel emission. Cheap: reuse the same emission term as L3 |
| L6 | Steam and smoke plumes, pale pink, rising | Existing smoke chunks, re-tinted and biased upward. Nearly free |
| L7 | Rising embers | Vertex-shader motes, same mechanism as the planned moths and ash. Free per mote |
| L8 | Heat shimmer over hot ground | The expensive one. Full-screen refraction is a pass; the cheap 80% is a vertex wobble on distant geometry, or a UV distortion in the fog |
| L9 | Erupting volcano and lightning on the horizon | Skirt/horizon art plus an occasional light flash. The horizon is already a painted skirt, so this is paint plus one flashing light |
| L10 | Duckboards crossing lava on bridges | Composer/placement, not rendering. Lava must be a *hazard the layout respects* |

### Winter

The winter reference is the opposite problem: the light is **flat and everywhere**. Overcast, high key, low
contrast, desaturated to almost monochrome blue-grey. The danger here is not silhouette, it is mush — an image
where everything is the same value and nothing reads.

| # | What the reference shows | What it needs technically |
|---|---|---|
| W1 | Snow on every up-facing surface: sandbag tops, duckboard slats, timber, stone | A snow term driven by world normal Y, applied in the shared shading path so terrain, props and men all get it from one rule. This is the single most important item and it is nearly free |
| W2 | Frozen shell holes: pale, translucent, cracked ice | The liquid surface again, third variant. Ice is water with a crust and no flow |
| W3 | Snow drifts — the ground is sculpted into wind-carved ridges | Terrain height modulation, or a drift term in the ground shading. Height is better and more expensive |
| W4 | Blizzard: dense wind-driven snow, streaking | Particles with a wind bias. The motes mechanism again |
| W5 | Distance disappears fast in white-blue fog | Fog profile: much denser than night, cold tint, and it should *brighten* with distance rather than darken |
| W6 | Real mountains on the horizon, with shape and depth | **This breaks the current horizon.** The skirt is a painted flat surface; mountains need silhouette against the sky. Either a height-displaced skirt or a separate distant range |
| W7 | Icicles on dugout lips, bunting, structures | Kit modules, placed on downward edges |
| W8 | Men's breath at head height | Already planned in the close-up work |
| W9 | Footprints, which snow shows far more than mud does | Already planned; winter raises its value sharply |
| W10 | Ice glare — the few bright speculars in an otherwise flat image | The only high-contrast element available. Whatever the ice does with light is what stops the image being mush |

---

## Where the AAA actually comes from, inside the budget

The important observation, and the reason both biomes are affordable:

**W1 and L2/L3 are arithmetic on data the fragment shader already has.** World normal and world position are
already interpolated for every pixel of every surface. Snow coverage is `saturate(normalWS.y)` shaped by a curve
and a noise break-up; lava emission is a mask times a colour. Neither needs a new texture, a new draw call, a new
buffer, or any CPU work per object. Wrapped in `if (_TWSnow > 0)` and `if (_TWHeat > 0)` — uniform branches, free
when false — they cost the night look **nothing at all**.

That is the whole trick: the two biomes that look most different from the base game are the two that can be built
almost entirely out of terms rather than assets. The expensive items are the short list, and they are all optional:

| Genuinely expensive | Cheap 80% version |
|---|---|
| L8 heat shimmer as a refraction pass | UV distortion inside the existing fog, no extra pass |
| W3 snow drifts as real displaced height | A drift term in ground shading; no silhouette, but reads at distance |
| W6 mountains with true silhouette | Height-displaced skirt mesh, which the skirt already is — it just needs amplitude |
| L1/W2 a third liquid shader | One liquid shader with a mode, since lava, water and ice differ in ramp and flow, not in structure |

## Open questions for the owner

1. **Do these replace Mission 1's setting or sit beside it?** A biome that is only a re-skin is cheap; a biome
   that changes how the map plays (lava as a hazard, snow slowing movement) is a sim change and touches
   determinism.
2. **Should lava hurt?** L10 assumes the layout respects it. If lava is only scenery, it will look wrong the
   first time a man walks across it.
3. **Does snow accumulate during a match, or is it a fixed state?** Accumulating snow is a beautiful idea and a
   sim/determinism question, not a rendering one.

Answers pending. Until then, everything here is built as presentation-only, with the sim untouched.

## What was built, 2026-09-23 02:40 to 07:10

### The spine

A battlefield is a `BiomeProfile` (`Presentation/Terrain/BiomeProfile.cs`): three entries, everything that differs
between one field and another as named data. The rule it is written to keep is that **a fourth biome is a fourth
entry and nothing else**. `Atmosphere.ApplyNight` and its nineteen hand-assigned literals are gone.

`SceneMood.Night` was the trap. It is read at twenty-odd sites in `CombatFx`, `TankRenderer` and
`AnimationController`, always as a binary, and a third biome does not break those loudly — it silently takes the
*daylight* branch. The bool keeps the only meaning it ever had, **is it dark**, and lava answers yes: it is lit
from below by its own floor, which is the case those glow branches were tuned for. Everything else that used to
ride on "night" is a field.

### The terms, all in the shared header, all behind uniform branches

| Term | What it does | Cost |
|---|---|---|
| `TWSnowAmount` | Coverage by world normal, domain-warped, with a hard wandering edge and a frost floor | Free |
| `TWHemisphere` | The shaded half split into sky-from-above and bounce-from-below | Free |
| `TWGroundBounce` | What the floor throws up onto what stands on it | Free |
| `TWMolten` | Two octaves; where the ground is molten at all | Cheap, terrain only |
| `TWPlateEdge` / `TWHeatGlow` | Crust plates, seams that widen toward the pools, a three-stage cooling ramp | Cheap, terrain only |
| `TWHeatCrust` | Albedo toward black between plates — adding glow without this is glowing dirt | Free |
| `TWWorldPaint` | Multiply, then a **pull** toward the biome's hue | Free |

`TWHemisphere` is the one worth singling out. It is a quality fix for winter's flat overcast light **and** the
entire key light of the lava field. One function, opposite signs, one parameter.

### Measured, with `scratchpad/hot.py`, on the ground half of the standard view

| | Lava | Winter | Concept target |
|---|---|---|---|
| Hot area | **31.4%** (was 1.0%) | — | 34.6% |
| Median value | 0.18 | 0.50 | 0.47 |
| Warm pixels | — | **7.6%** (was 22.4%) | 0.2% |
| Inside the biome hue | — | **90.8%**, 97.1% at trench level | ~100% |

### What the captures taught that reading the code did not

- **The lava field was figure-ground inverted.** Three rounds of tuning produced 1% hot area against a 34.6%
  target. That is a different model, not a different constant: I had built cold crust with hairline seams, and the
  reference is molten rock with cold plates standing in it. Turning the glow up against a 30× area gap produced
  neon piping, twice.
- **Absolute world height is the wrong instrument, twice.** Gating the glow above `MoltenLevel` deleted every
  crack on ground above 0.6 m — most of a battlefield with a parapet. The ground bounce had the same fault.
- **A measured colour is not an emission colour.** The reference's crack body is what it shows *after* fog, grade
  and bloom. Pasted in as emission and pushed through Saturation +22 and Bloom 1.35 it came out magenta.
- **A multiply cannot change hue.** Brown mud times cold grey is brown, so winter's trench interiors stayed warm.
- **Snow that rides on the rain amount brings the rain's wetness with it**, because that number is also `_TWWet.z`.
- **A square quad is confetti.** Snow needs a 7:1 dash along its fall.

### Not built, and therefore not true to the references

No embers, heat shimmer or volcano on lava. No ice: winter's shell holes still render as water. No mountains on
the winter horizon — the skirt is flat painted ground, and this is the one item that needs real geometry. No snow
volume (no vertex displacement), so a parapet is still a row of discrete bags with white tops rather than a
continuous pillow. No contact shadows anywhere, which winter needs most because a bright uniform field hides
nothing. The blizzard falls nearly vertically.

### Two single-field findings outside the biomes

- `m_ShadowDistance: 220` with one cascade at 2048 is ~11 cm per texel, which is why nothing in this game appears
  to have a contact shadow. They are on; they are spread too thin to resolve a sandbag.
- The renderer is plain Forward with `AdditionalLightsPerObjectLimit: 8`, while `NightLights` creates ~60 lights.
  A large terrain chunk gets eight, chosen per *object*. `_FORWARD_PLUS` is already in the shader pragmas and
  `LIGHT_LOOP_BEGIN` already handles it, so the open Forward+ decision is one asset field.

## Where it stands at 08:20, and what to do next

Eight commits, every one gated green (EditMode 191/191, PlayMode 13/13). Measured on the ground half of the
standard view with `scratchpad/hot.py`:

| | Lava | Winter | Concept |
|---|---|---|---|
| Hot area | 29.7% (from 1.0%) | — | 34.6% |
| Median value | 0.16 | 0.50 | 0.47 |
| Warm pixels | — | 7.6% (from 22.4%) | 0.2% |
| Inside the biome hue | — | 91.0% | ~100% |

### The honest remaining holes, in the order I would take them

1. **Lava at trench level is still 0.9% hot.** Adding a third noise octave helped and did not close it. The
   player spends most of their time at this camera, and standing in a trench on a lava planet you see almost no
   lava. This is the single biggest gap between what is built and what was asked for.
2. **The median is 0.16 against 0.47.** The histogram is bimodal — near-black crust against pools that clip —
   where the reference carries its mass in the mid-tones. More brightness will not fix it; more mid-value area
   will.
3. **Winter's remaining warm is the MEN, and 7.6% was the wrong camera.** Corrected below; the wire and the
   duckboards are real and second-order. Smoke was fixed separately (it now takes `SmokeTint`).
4. **Ice.** Winter's shell holes still render as water: a reflective blue puddle on a snowfield.
5. **No contact shadows.** Winter needs them most, because a bright uniform field hides nothing. Note the
   shadow-distance finding above first — it may be most of the answer and costs one field.
6. **Mountains.** The only item on either list that needs real geometry rather than a term.

### A lesson worth keeping, because it cost real time twice

**A metric with no opposing metric will be gamed.** The single-hue number climbed to 97% at trench level while
the picture went monochrome, and I only caught it because a critique looked at the image rather than the number.
`hot.py` reports hot area, warm fraction, in-hue fraction *and* median value together for exactly this reason,
and a value-spread metric should join them before anyone tunes winter further.

## Correction, and the measurement that forced it: 7.6% was the camera that shows the fewest men

The table above reports winter's warm fraction from the standard view. Measured again with `hot.py` across the
whole capture set (`biomeshot.sh Winter`), at one lens and one field:

| Frame | What is in it | Warm |
|---|---|---|
| `close_ground` | super zoom, bare ground, no men | **1.31%** |
| `far` | zoom 60 over open field | 1.02% |
| `close_field` | super zoom, wire and craters | 4.20% |
| `wide` | the standard view, zoom 30, ~6 men | 7.78% |
| `mid` | zoom 16, ~6 men | **9.52%** |
| `close_trench` | super zoom, one man at point-blank | 36.27% |

**Read that last row carefully, and do not quote it.** 36% is one soldier filling the frame at a metre, which is
a camera placement and not a statistic. It is in the table for one reason: it identifies the surface. The slab
that fills that frame samples `(110, 109, 85)`, and a man in the `mid` frame samples `(109, 107, 86)`. They are
the same object.

The row that matters is the pair at the top and the middle. Bare ground at the super zoom is **1.31%** warm and
open field at zoom 60 is **1.02%**; put six men in frame and it is **7.78–9.52%**. Rust wire and tan duckboards
cannot be the explanation the list above gave, because the frames without men in them are already at the
reference. Looking at `mid` rather than at the number settles it: the snow, the sandbags, the duckboards, the
revetment planks, the wire and the stumps are all cold — `(114, 128, 152)` — and every soldier is olive. The men
are not darker than the field, they are the same VALUE at a different hue, which is precisely what reads as cut
out of another picture.

### Why the men were the one thing the biome could not reach

`TWWorldPaint` — the pull that turns the baked mud palette to basalt or to old snow — was called in exactly one
place in the project, `Toon_URP.shader:161`. That covers the ground, the props, and the **fallen**, who are drawn
with `TW/Toon` (`CombatFx.Painted`). It did not cover the living, who are drawn with `VAT_URP`. So on the winter
field a corpse was dragged 80% toward cold grey while the man standing over him kept Flanders brown, and the same
cloth rendered as two different hues depending on whether its owner was alive.

The header that declares `_TWWorldTint` says this itself, three lines above the declaration
(`TWAtmosphere.hlsl:30-34`): these globals live in the shared header "because snow that lands on the terrain and
the sandbags but not on the men ... is worse than no snow, because the eye reads the men as cut out of a different
picture". `_TWWorldTint` was the one term in that block doing the thing the comment forbids.

**What was changed:** the mud caked on boots and shins now takes the same paint the ground takes. That mud is the
field's material, not the man's kit, and it was a hard-coded night-mud brown on all three biomes.

**What was deliberately not changed:** his uniform. Khaki pulled 80% toward blue-grey is the mush this file's own
warnings are about, and what troops wear on a winter field is a design decision. **Owner's call: do winter troops
get a greatcoat — a second baked cloth palette per biome — or does the khaki stay?** Until that is answered,
winter's warm fraction at trench level cannot reach the reference's 0.2%, and it should not be tuned toward it by
any other means.

### A second bug, on lava: every lantern faded to snow

`TWLocalLights.hlsl:48` desaturates the far half of a lamp pool toward "the biome's own colour", and the value it
used for that was `_TWSnowColor`. That field is pushed on every biome — the night field's own default is
`(0.90, 0.93, 0.97)` — and it only means anything where snow lies. The branch is live wherever `LampScale < 1`,
which is winter **and lava**, and `BiomeProfile.Lava()` never assigns a snow colour. So on a planet whose entire
design is orange light from below, every lantern's outer pool was fading toward snow white. It now asks whether
there is snow (`_TWSnow.x`) and otherwise takes the field's own tint, normalised by its luma so it desaturates
without darkening.

Underneath it: `Atmosphere.PushBiome` sets seven shader globals and `ClearBiome` put five of them back.
`_TWSnowColor` and `_TWHeatColor` survived a change of field, which was safe only because their consumers happen
to be guarded by the two that *were* cleared. `EveryGlobalABiomePushesIsAGlobalABiomeClears` now holds that line.

### And a limit of the cycle-5 guard, stated rather than papered over

`EveryFieldOnTheProfileIsReadBySomething` would not have caught any of this. It asserts a field's name is
mentioned somewhere else in the tree, and `WorldTint` is mentioned — in `Atmosphere`, which uploads it faithfully
to a uniform that one shader out of twelve reads. "Declared and never mentioned" and "read here and not there"
are different failures, and no source scan sees the second. The instrument that did see it was a capture with men
in it, measured against a capture without.
