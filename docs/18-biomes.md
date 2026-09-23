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

## Status

Specified 2026-09-23 02:40. Nothing implemented yet. This document is the brief; the implementation notes and the
measured costs will be appended as each item lands.
