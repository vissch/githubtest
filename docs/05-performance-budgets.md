# Performance budgets

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.

Target: **60 fps (16.6 ms) at 1920×1080** on a GTX 1050 / Intel Iris Xe with a low-IPC quad-core CPU, with
2,000 live infantry slots, 20 vehicles, 40 emplacements, an active barrage and one gas cloud.

## CPU budget per sim tick (20 Hz, so the tick is spread over ~3 frames)
| System | Budget | Notes |
|---|---|---|
| Command apply + mission triggers | 0.10 ms | trivial |
| Flow field recompute (time-sliced) | 0.60 ms | ≤ 8 goal groups per team, 150×400 grid, Dijkstra in Burst, double-buffered |
| Spatial hash rebuild | 0.30 ms | `NativeParallelMultiHashMap`, 2,000 inserts |
| Target acquisition (⅓ of slots) | 0.40 ms | heightfield raycast ≤ 80 m, ≤ 8 candidates per unit |
| Direct + indirect fire, blasts | 0.50 ms | |
| Suppression, stance, garrison | 0.20 ms | |
| Gas/smoke diffusion | 0.30 ms | 75×200 cells, two `half` grids |
| Deformation + cost update | 0.20 ms | amortised, ≤ 4 stamps per tick |
| Separation + movement | 0.60 ms | parallel job, 9-bucket neighbour query |
| Sector control, death, hash | 0.30 ms | xxHash64 over ~1.5 MB |
| **Total** | **≤ 3.6 ms** | hard ceiling 4 ms; measured with the Unity Profiler `SimWorld.Step` marker |

## CPU budget per render frame
| Item | Budget |
|---|---|
| Pose interpolation job + `GraphicsBuffer` upload | 0.4 ms |
| Event pump (VFX/audio/UI routing) | 0.2 ms |
| Terrain dirty-chunk mesh upload | 0.3 ms amortised |
| Ragdoll physics (≤ 15 active) | 1.0 ms |
| UI | 0.5 ms |
| Camera, input, misc MonoBehaviours | 0.3 ms |
| **Main thread total (excl. render submit)** | **≤ 3 ms** |

## GPU budget (GTX 1050, 1080p)
| Pass | Budget |
|---|---|
| Terrain chunks (≤ 60 visible, displacement shader) | 2.0 ms |
| VAT infantry (≤ 3 indirect draws per archetype, ≤ 8 archetypes visible) | 3.5 ms |
| Vehicles, props, trench kit (BRG instancing) | 1.5 ms |
| Shadows (one cascade, 2048) | 2.0 ms |
| Volumetric gas/smoke (half-res raymarch) | 1.5 ms |
| VFX, decals, post (no TAA, FXAA only) | 2.0 ms |
| UI | 0.5 ms |
| **Total** | **≤ 13 ms** |

## Memory
| Item | Budget |
|---|---|
| SimWorld state (2,048 slots × ~96 B + grids) | ≤ 4 MB |
| Flow fields (8 goals × 2 layers × 60k cells × 2 B) | ≤ 2 MB |
| VAT atlases (RGBAHalf, 8 archetypes) | ≤ 256 MB VRAM |
| Terrain heightfield texture (R16, 300×800) | 0.5 MB |
| Corpse instances | ≤ 1,000 before oldest-first fade |
| Managed allocations during play | **0 B/frame** (verified with Profiler GC Alloc column) |

`SimConfig.MaxSlots = 2048` is a **combined** cap (infantry + vehicles + emplacements). The target above (2,000 + 20 + 40)
does not fit; bump to 4096 when A3 lands vehicles and emplacements. Whether 2,000 infantry is a requirement at all is
decided at the M1.5 fun gate.

## Draw calls
- Infantry: ≤ 3 `RenderMeshIndirect` calls per archetype per LOD tier.
- Terrain: 1 draw per visible chunk, shared material.
- Total target: **< 300 draw calls** per frame.

## Anti-patterns that are banned in this project
| Banned | Use instead |
|---|---|
| `MonoBehaviour.Update` per unit | jobs over `SimWorld` arrays; presentation entities |
| `NavMeshAgent` per unit | flow fields + spatial hash separation |
| `SkinnedMeshRenderer` per infantry unit | VAT + indirect instancing (hero pool excepted) |
| Colliders/rigidbodies on infantry | spatial hash queries |
| `Physics.Raycast` / `OverlapSphere` in sim | `HeightfieldRaycast`, hash-bucket AoE |
| Unbounded ragdoll instantiation | 15-slot pool + corpse baking |
| Runtime mesh slicing for gore | ellipsoid clip in shader |
| `Instantiate`/`Destroy` in combat | pools allocated at scene load |
| LINQ, boxing, `string` building in per-frame code | pre-allocated buffers |

## Profiling procedure (every milestone)
The procedure that used to stand here named a menu (`TW → Debug → Stress 2000`) and a marker (`SimWorld.Step`) that
never existed. Since 2026-09-23 both halves are real (details in the last section of this file):
1. **One benchmark, two places.** `TW.Perf.PerfBench` runs a fixed battle — the stress preset at `stress` riflemen a
   side, fast-forwarded to `settle_ticks`, paused there while `warm` frames render, then `ticks` sim ticks at 1x from
   the standard view held over the middle of the army, shake off, weather pinned — and writes a json report.
   Editor: `CaptureRig.Bench("stress=1500 settle_ticks=1800 ticks=400 out=...")` with GreyboxCorridor open.
   Player: `TrenchWarfare.exe -twbench "stress=1500 settle_ticks=1800 ticks=400 quality=5 out=..."` from
   `TW/Build/Windows Bench` (`Editor/BuildWindows.cs`). Two reports with the same `hash_start` measured the same fight.
2. **Markers.** `TW.Sim.Step`, `TW.Sim.Sys.<System>` (one per sim system), `TW.Sim.Hash`, `TW.Host.*`, `TW.Anim.*`,
   `TW.Events.*` (`Sim/Core/PerfMarkers.cs`); the report divides each marker's window total by the ticks it covered
   (`per_tick_ms`). They compile out of a release player: attribute with a development build, compare totals only
   release to release.
3. **Allocation.** `TW.Perf.AllocProbe` counts managed allocations exactly. Never `GC.GetAllocatedBytesForCurrentThread`:
   under Unity's Boehm GC it reads 0 whatever the code does.
4. Frame Debugger for draw calls and indirect batches, as before.

## First measurement (2026-09-22, commit with `Editor/CaptureRig.cs`)

Until this date nothing in the project had ever opened a `ProfilerRecorder`, so every budget above was unverified.
`TW.Editor.CaptureRig.Profile(path, frames)` samples the recorders over N frames and writes them as json.

| Recorder | p50 | p95 | p99 | Budget | Over |
|---|---|---|---|---|---|
| Main Thread | 2.53 ms | 8.05 ms | 13.77 ms | 3 ms | yes, at p95 |
| GPU Frame Time | 9.09 ms | 16.17 ms | 20.63 ms | 13 ms | yes, at p95 |
| SetPass Calls | 763 | 768 | 769 | — | — |
| Draw Calls | 936 | 941 | 942 | < 300 | yes, 3x |
| GC Allocated / frame | 29.6 KB | 34.1 KB | 59.8 KB | 0 B | yes |

**Read these as a starting point, not a verdict.** They were taken in the editor, not a player build; the editor adds
its own draw calls and managed allocation, and this machine is far above the GTX 1050 target. The run had about ten
men alive, not the 2,000-man stress preset — which makes 936 draw calls the most alarming line, because a nearly
empty field should be nowhere near the ceiling. The honest next steps are the same measurement in a build, then with
the stress preset, and only then any optimisation work.

## Second measurement (2026-09-22, the same evening, with an army on the field)

The first measurement above had about ten men alive, and said so. `CaptureRig.Stress(unitsPerSide, path, frames)`
now sets the preset in the scene, enters play mode, waits for both sides to deploy and go over the top, profiles,
writes the file and puts the scene back — across the domain reload in the middle, which is why the request is held
in `SessionState`. (The procedure above refers to a menu `TW → Debug → Stress 2000`; no such menu has ever existed.)

**1,996 men alive, 200 frames, same editor, same machine.**

| Recorder | 10 men | 2,000 men | Budget |
|---|---|---|---|
| Main Thread p50 | 2.53 ms | **13.76 ms** | 3 ms |
| Main Thread p95 | 8.05 ms | **44.23 ms** | 3 ms |
| Main Thread p99 | 13.77 ms | **123.28 ms** | — |
| GPU p50 | 9.09 ms | 10.83 ms | 13 ms |
| GPU p95 | 16.17 ms | 16.52 ms | 13 ms |
| SetPass | 763 | 761 | — |
| Draw Calls | 936 | **931** | < 300 |
| GC / frame | 29.6 KB | **428 KB** | 0 B |

Three things fall out of this, and they point optimisation somewhere quite different from where it was heading.

**The men are free in draw calls.** Two hundred times as many soldiers cost *five fewer* draw calls. The instanced
VAT path does what it was built to do. The 930 draw calls are the world — terrain chunks, props, kit — plus whatever
the editor adds, and no amount of work on the units will move that number. The alarm raised by the first measurement
was pointed at the wrong thing.

**The GPU is nearly fine.** 10.8 ms p50 with an army out, against a 13 ms budget, and it barely moved from the empty
field. Whatever is wrong here is not the shading.

**The main thread and the garbage are the wall.** 2.53 → 13.76 ms p50 and 29.6 KB → 428 KB per frame, both scaling
with the number of men: roughly 200 bytes allocated per man per frame, which at 60 fps is 25 MB/s of garbage and a
collection every few seconds. The budget says 0 B/frame in play. This is the first hard lead the project has ever
had on where the frame goes, and it is a CPU-side lead, not a rendering one.

Still true of both measurements: the editor is not a player build, and this machine is far above the GTX 1050
target. A build measurement is the next honest step, and the absolute numbers should not be quoted as the game's
performance until one exists. The *ratios* — men free in draw calls, garbage scaling per man — survive the caveat.

## First optimisation from a measurement rather than a hunch (2026-09-22)

The stress profile said the units were free in draw calls, so the 930 had to be the world. `BattlefieldProps`
reported **271 draw calls for 1,706 instances** — six instances a call. The cause: a batch keeps its instances in
32 m spatial *pages* so they can be frustum-culled one at a time, and `Render` was then submitting one
`RenderMeshInstanced` **per page**. The page is the right unit to decide what the camera can see and the wrong unit
to hand to the GPU.

Pages are still culled individually; the survivors are now copied into one reused 1023-matrix buffer and submitted
once per module (again for each further 1023, which is the instancing limit).

| Measured, same scene, ~12 men | Before | After |
|---|---|---|
| `BattlefieldProps.DrawCalls` | 271 | **76** |
| Draw calls, whole frame | 949 | **368** |
| SetPass calls | 770 | **313** |
| GPU p50 / p95 | 8.39 / 13.07 ms | **3.07 / 5.15 ms** |
| Main thread p95 / p99 | 7.91 / 16.06 ms | **4.64 / 7.68 ms** |
| GC per frame | 30.6 KB | 29.5 KB (unchanged, as expected) |

The GPU is now inside its 13 ms budget with room to spare, and the frame is no longer dominated by submission.
Draw calls are still over the 300 ceiling, but by 23% rather than 216%. Nothing was removed from the world to get
this: a capture at the standard view and at the super zoom shows the same trenches, bags, duckboards, wire and
lanterns as before.

### The same change, with the army on the field

| Measured, 1,996 men | Before | After |
|---|---|---|
| Main thread p50 / p95 / p99 | 13.76 / 44.23 / 123.28 ms | **2.52 / 16.14 / 22.60 ms** |
| GPU p50 / p95 | 10.83 / 16.52 ms | **1.90 / 3.12 ms** |
| SetPass | 761 | **321** |
| Draw calls | 931 | **374** |
| GC per frame p50 / p95 | 428 KB / 833 KB | **29.5 KB** / 429 KB |

With two thousand men on the field the main thread is now inside its 3 ms budget at the median, where it was four
times over it, and the 123 ms worst frame is down to 23 ms. The GPU costs under 2 ms. Draw calls are 23% over the
ceiling instead of 210%.

**A correction.** The previous section read 29.6 KB at ten men against 428 KB at two thousand and called it roughly
200 bytes of garbage per man per frame. That reading does not survive this measurement. Allocation here is bimodal:
after the change the median frame allocates 29.5 KB and the 95th percentile allocates 429 KB, so the 400 KB is a
periodic lump on *some* frames rather than a per-man cost on every one. Submitting 271 instanced batches a frame
instead of 76 was evidently making that lump land far more often. What causes the lump is still unknown and is the
largest outstanding budget problem; it wants its own measurement, not another theory.

## Texture memory, measured for the first time (2026-09-22)

"VAT atlases ≤ 256 MB" has been a line in this document since it was written and had never been added up.
`TW.Editor.CaptureRig.Textures(path)` now lists every texture in memory, largest first, with what
`Profiler.GetRuntimeMemorySizeLong` says it costs, plus the distinct material and shader count for the prop kit.

### What it found

| | Bytes |
|---|---|
| `InfantryVatPositions`, 917x3398, RGBA64, no mips | 48.7 MB each |
| `InfantryVatNormals`, 917x3398, RGBA32, no mips | 24.3 MB each |
| Kit base maps, six imported sheets + five pigments | 34.9 MB |
| `TankAtlas_LOD0`, 2048², DXT1 | 5.5 MB |
| `Painted horizon`, 846x1404, RGBA32, 11 mips | 12.4 MB |

The VAT atlases are the whole story: about 144 MB against the 256 MB budget, an order of magnitude more than
anything else. Two things about them want investigating by whoever owns `VATRenderer`. The reported size is about
twice what the format and dimensions imply, which is the signature of a readable texture keeping a CPU copy beside
the GPU one; and four position atlases were resident where there should have been two, which suggests they
accumulate across play-mode entries rather than being released.

### The imported sheets, combined

Six 2048² sheets were being loaded, one per Tripo set, and bound separately. `Tools/envatlas.py` now packs them
halved into one 4096x2048 sheet as a 4 x 2 grid of 1024 cells, two of them spare. No shader change was needed:
`TW/Toon (URP)` already transforms its UVs by `_BaseMap_ST`, so `BattlefieldKit.Imported` sets the base map once
and picks the set with a texture scale and offset.

| Kit base maps | Before | After |
|---|---|---|
| Distinct textures | 11 | 6 |
| Memory | 34.93 MB | 13.59 MB |

**Power of two, or it is not worth doing.** The sheet was first cut 3072 wide. Unity's default NPOT rule rounded it
up to 4096 and resampled it; told to keep 3072, the block compressor refused it and returned 25 MB of uncompressed
RGB24 — worse than the six sheets it replaced. All three versions looked identical on screen, because the cell
rects are fractions either way. Only the measurement told them apart.

### Still not measured

The 80 kit modules hold 80 distinct materials, one per module, which is the obvious explanation for SetPass calls
sitting at 321 against 374 draw calls. Sharing materials needs the per-module colour, sway and gloss to move off
the material, so it is its own piece of work with its own measurement.

### ...and the painted surfaces, combined too (same day)

The five procedural pigments were greyscale stored in RGB24 — three bytes to say one thing — in five separate
textures, each costing its own binding. They are now the layers of one R8 `Texture2DArray`, set globally as
`_PigmentSheet`, with the layer chosen per material by the `_Pigment` float (`-1` keeps the old `_BaseMap` path,
which the ground needs because its alpha carries water depth).

An array rather than an atlas: these tile across a plank or a wall, and a tile packed into the corner of an atlas
bleeds into its neighbours the moment it wraps. That is the opposite of the imported sheets, whose UVs are clamped
inside 0..1 and which therefore pack into cells perfectly well. Same word, "combine", two different techniques,
and picking the wrong one for either would have looked like a bug rather than a cost.

| Prop kit base maps | Start | After the env atlas | After the pigment sheet |
|---|---|---|---|
| Distinct textures | 11 | 6 | **1** |
| Memory | 34.93 MB | 13.59 MB | **10.67 MB** |

Eighty modules now bind one base map between them, plus one global sheet of about 0.7 MB.

Because a painted surface now costs a float rather than a texture and a binding, three new ones were worth adding
where there had been none at all: **Rust** (pitted patches with the rust running down from them) on the helmet,
mess tin, ammunition tin, bucket, hanging tins, wire tins, grave marker and spent cases, all of which were flat
colour; **Stone** (fracture lines with a chipped lip) on rubble; and **Sacking** (a warp and weft you can count) on
sandbags and trench bags, which were using the smoother seamed Canvas.

Still open: 80 modules hold 80 materials. Now that a painted module binds no texture of its own, the only things
keeping those materials apart are `_BaseColor`, `_Pigment`, `_Sway`, `_Gloss` and `_OutlineWidth` — all floats and
a colour. Moving them to a per-module property block is the obvious next experiment against the 321 SetPass calls,
and it is an experiment, not a certainty: a property block may cost a state change of its own.

## The periodic allocation, located but not yet named (2026-09-23)

The largest remaining budget problem was a GC spike nobody could explain: allocation is bimodal, so the mean says
nothing. Measured twice tonight with 1,997 men, the second run confirming the first:

| gc bytes per frame | p50 | p95 | p99 |
|---|---|---|---|
| 2,000-man stress | 33,560 | 441,456 | 499,986 |

**It is not per-man and it is not per-frame. It is once per sim tick.** The editor profiler's frame hierarchy puts
it inside `SimHost.Update`'s subtree, in 106 frames of 300, averaging 145,890 bytes across all frames — about
403 KB on each frame where it appears. The mean gap between those frames is 2.8, and the sim ticks at 20 Hz, so at
this frame rate "every 2.8 frames" and "every tick" are the same statement. That also explains the shape: a frame
that carries a tick pays it, a frame that does not carries nothing, and the average of the two is a number that
describes no frame that ever happened.

The rest of the frame, for scale: `BattleHud.OnGUI` allocates 13,302 bytes in **every** frame (IMGUI string
interpolation, mine, and the largest steady cost), and `CombatFx.Update` 1,136 bytes.

### What is not yet known, and why the obvious answer is not in this document

Callstack resolution pointed at one line, `LockstepDriver.cs:54`, with 3,146 allocations a frame. That line is not
recorded here as the cause, because it cannot be corroborated and the arithmetic does not support it: the driver
steps at most eight ticks a frame, the loopback transport drains its inbox on every receive, and there is no path
by which that line runs three thousand times. Two independent attempts each returned **exactly one** distinct
callstack — the first because samples merged by name and one representative stack was resolved for the whole
merged item, the second out of 40,899 samples, which is the signature of the same artefact rather than agreement.

Two instruments returning the same wrong answer is not corroboration when they share a mechanism. What is solid is
the subtree; what is not is the line. The subtree is enough to act on and the line is not, so the line waits.

### The instrument that will settle it

`Tests/EditMode/TickAllocationTests.cs` drives the drivers, the presenter, the animation controller and the event
pump directly and measures `GC.GetAllocatedBytesForCurrentThread()` around each, after a warm-up long enough that
one-time capacity growth is not mistaken for a leak. It is deterministic, needs no editor, runs in the gate in a
second, attributes the cost to one of four callers by construction rather than by callstack resolution, and stays
afterwards as the regression test that keeps a fixed tick from silently un-fixing itself.

The general lesson, which is the same one the HUD taught from the other end: an instrument answers the question it
was pointed at. `gc_bytes_per_frame` was pointed at "how much", answered it correctly for a day, and was read as if
it had answered "where".

### The VAT atlases: one of the two questions answered (2026-09-23)

The measurement above left two things for "whoever owns `VATRenderer`". One had its answer in our own source.

**Four atlases where there should be two: they were never given back.** Every atlas texture is created with
`HideFlags.HideAndDontSave` (`VatCodec.Decode`, `ProceduralSoldier.Build`). That flag exempts an object from
*every* cleanup Unity does on its own: leaving Play mode, unloading the scene and `Resources.UnloadUnusedAssets`
all walk past it. Nothing in `Presentation/Units` destroyed a `Texture2D` anywhere — `VATRenderer.OnDestroy` freed
its `GraphicsBuffer`s and `NativeArray`s and stopped there. So every `Start` decoded a fresh set and the previous
one stayed resident until the editor was quit.

One resident set is 70.2 MB, and that is the whole of the reported ~144 MB:

| | | |
|---|---|---|
| Soldier | 917 verts × 3,398 frames, 111 rows | 35.7 MB |
| Sniper | 887 verts × 3,398 frames, 111 rows | 34.5 MB |
| **One set** | positions RGBA64 (8 B/texel) + normals RGBA32 (4) | **70.2 MB** |

`VatAsset.Release()` now destroys them and `VATRenderer.OnDestroy` calls it. Two traps it has to avoid, both of
which would be worse than the leak: a decoded figure **borrows** its mesh from whatever supplied the bytes, and for
a baked figure that is the `Resources` `.asset` on disk, so destroying it would empty the file — hence
`VatAsset.OwnsMesh`, set only by `ProceduralSoldier.Build`. And a figure whose bake is missing borrows the figure
before it, so the same `VatAsset` sits in several slots and teardown de-duplicates by reference.

**The atlases were also all named the same thing.** `VatAssetData.ToAsset` hardcoded `"InfantryVat"`, so the
Soldier's textures and the Sniper's had identical names and a memory listing could not tell them apart. That
ambiguity is most of why "four where there should be two" took a day to read. They are now named after the figure.

**The doubling is still open, and the hypothesis recorded above is wrong.** "The signature of a readable texture
keeping a CPU copy beside the GPU one" is refuted by the source: `VatCodec.Decode` already passes
`makeNoLongerReadable`, at `pos.Apply(false, !keepReadable)` with `keepReadable` false. The likeliest remaining
explanation is that it is an **editor-only** artefact — the editor keeps a copy it can re-upload after a graphics
device reset, whatever the flag says — in which case it does not exist in a build at all. That is a guess. The
honest next step is the one this document has been asking for since the frame-budget section: *measure a build*.

`Tests/EditMode/VatAtlasMemoryTests.cs` guards the lifetime and holds one resident set to 128 MB, and prints the
unexplained number rather than asserting on it, so an open question cannot go green.

**Not yet covered by a test:** that `VATRenderer.OnDestroy` actually calls `Release`. The tests cover `Release`
itself; deleting the call from `OnDestroy` would leave them green. That wants a PlayMode test which stands up a real
renderer, records its atlases and destroys it — next piece of work on this.

### The doubling is systematic, and it is not a readable copy (2026-09-23)

The other half of the atlas question turns out not to be about the atlases at all. Three textures, measured against
what their format and dimensions imply:

| | Format arithmetic | `GetRuntimeMemorySizeLong` | Ratio |
|---|---|---|---|
| `InfantryVatPositions`, 917×3398, RGBA64, no mips | 23.77 MB | 48.7 MB | **2.05** |
| `InfantryVatNormals`, 917×3398, RGBA32, no mips | 11.89 MB | 24.3 MB | **2.04** |
| `Painted horizon`, 846×1404, RGBA32, 11 mips | 6.04 MB | 12.4 MB | **2.05** |

Two different formats, two unrelated creation paths (`VatCodec.Decode` and `GreyboxTerrainView.BuildSkirt`), one
mipped and two not — and the same constant. That rules out anything format-specific, content-specific or
mip-related, and it means this was never a VAT problem.

It also finally disposes of the hypothesis recorded above. "A readable texture keeping a CPU copy beside the GPU
one" cannot be it: **all three already pass `makeNoLongerReadable`** — `pos.Apply(false, !keepReadable)` in
`VatCodec.Decode` with `keepReadable` false, and `texture.Apply(…, true)` in `BuildSkirt`. Whatever the second copy
is, asking for it to be dropped does not drop it.

What is left is that the **editor** keeps a copy it can re-upload after a graphics device reset, whatever the flag
says, in which case it does not exist in a player build; or that `GetRuntimeMemorySizeLong` counts system and video
memory together and reports both. Either way every texture number in this document is likely **twice the shipped
cost**, and none of them has ever been measured anywhere but the editor.

**So the next piece of work on memory is a build measurement, and it is worth more than any further optimisation.**
Until it happens, the honest reading of the table at the top of this section is "editor figures, probably 2× high".

### The painted horizon: DXT5 (2026-09-23)

12.4 MB for a surface that is mostly drawn beyond the fog made it the third largest texture in the game. It is now
block compressed, 4× smaller.

**Not DXT1, despite it being half the size again.** The alpha is not spare: `Coast` drives it down to 0.45 on wet
sand, and `TW/Toon` reads `half gloss = max(_Gloss, 1.0 - base.a)` with `base.a < 0.45` meaning standing water. So
alpha here is a continuous wetness that crosses a hard threshold, and DXT1's one bit of it would replace the whole
beach with a single water/not-water edge. DXT5 keeps eight interpolated bits.

**The grid had to be rounded first.** DXT works in 4×4 blocks and 846 is not a multiple of four, so the compressor
would have declined it — *silently*, leaving the texture at full size while the code read as though it had worked.
That is the same failure this document already records losing an afternoon to with the 3072-wide kit sheet, which
came back as 25 MB of uncompressed RGB24 and looked identical on screen. The skirt now rounds up to 848 and derives
its world mapping from the rounded size rather than assuming three texels a metre.

`Tests/EditMode/PaintedHorizonCompressionTests.cs` sweeps alpha through the 0.45 threshold, compresses, and asserts
that no texel changes side away from the crossing — the one thing that could go wrong on screen. Measured: the
largest alpha error across the sweep is **0.002**, and no texel changes side. DXT5's alpha block is very nearly
lossless on a smooth gradient, which is what a beach is, so the wetness gradient survives intact.

The refusal is not folklore either. A 6×8 texture logs `has dimensions (6 x 8) which are not multiples of 4.
Compress will not work.` and comes back still RGBA32 — an error in the log, nothing thrown, and the calling code
reading as though it had saved three quarters of the memory. That is the whole case for `Round4`.

## The perf pass: instruments that see, and what they saw first (2026-09-23)

The owner asked for the game to run faster "without losing fidelity". Before changing anything, two instruments this
file had been relying on turned out not to exist or not to work.

**The allocation instrument was blind.** Every allocation finding above — "five suspects measured at zero", the
per-event lookups "free", `AnimationController.Tick` cleared at 0 B — was measured with
`GC.GetAllocatedBytesForCurrentThread()`. In this editor it reads **0 bytes for a 1 MB array**, for 10,000
`new object()` and for 2,000 formatted strings. It is a stub under Unity's Boehm collector. The section above that
called one callstack "not corroborated" was right to distrust it and wrong about why: the tool that "refuted" it could
not see anything. `TW.Perf.AllocProbe` counts `GC.Alloc` samples on the calling thread instead (what
`Is.Not.AllocatingGCMemory()` does inside); `AllocProbeSanityTests` pins it to known answers (nothing = 0, a 1 MB array
= 1, 10,000 objects = 10,000, a formatted string ≥ 1) so it cannot silently go blind the same way.

**The periodic lump, named.** Re-measured with the probe, a tick with 300 men out made **1,518 allocations, all in
`AnimationController.Tick`**: every `Start(...)` call built its `why` string ("moves at 1.7 m/s: WalkRifle ...") for
every man, and `Start` only reads it for the one man the trace follows. The calls now pass
`(i == FollowSlot ? ... : null)`; the tick makes **0**. The trace for the followed man is word for word what it was.

**A benchmark that measures the same battle twice.** `PerfBench` (procedure above). Editor, RTX 4070 laptop, 1,500 a
side, ticks 1800–2200, standard view, same `hash_start` (F03581A2ADD8FFBA) in both columns:

| | before | after the `why` strings |
|---|---|---|
| Main thread p50 / p95 / p99 | 7.7 / 38.5 / 84.8 ms | 7.4 / 34.7 / 50.9 ms |
| GC per frame p50 / p95 | 11 KB / **461 KB** | 11 KB / **12 KB** |
| Allocations per frame p95 | 10,609 | 244 |
| GC collections in the window | 3 | 1 |
| `TW.Anim.Tick` per tick | 3.51 ms | 1.15 ms |
| GPU p50 / p95 | 3.6 / 8.4 ms | 3.8 / 6.8 ms |
| Draw calls / SetPass | 359 / 257 | 359 / 258 |

**Where the tick goes** (the first per-system numbers this project has had; both lockstep worlds, so per world is half):

| per tick | ms |
|---|---|
| `TW.Sim.Step` (all systems, both worlds) | 21.3 |
| of which `TrenchGarrisonSystem` | **15.7** |
| `TargetAcquisitionSystem` | 2.1 |
| `TW.Sim.Hash` | 1.4 |
| `MovementSystem` | 1.3 |

Two things fall out. The garrison system is three quarters of the sim: every man in a full trench with no post re-ran
the nearest-post search (five passes a kind, two kinds, every cell of the trench) on every tick, and with an army out
that is most of the men. And single player steps the sim twice (the loopback peer), so every one of these numbers is
paid twice. Both are the next two changes. Also measured: at ~1,800 men in view the unit vertex count (1.65 M) is
over `LodTiers.VertexBudget`, so **the men's shadows are already off** in this battle — GPU savings buy fidelity back.

Still editor numbers. The Windows build path exists (`Editor/BuildWindows.cs`); its first measurement is next.

### One world in single player (same day)

Owner decision: single player stops simulating the match twice; the two-world cross-check stays as an opt-in
**determinism canary** (`SimHost.DeterminismCanary`, `-twCanary`, on for the whole PlayMode gate). The enemy's seat
became a `CommandSeat`, which sends player 1's orders on the loopback with no world behind it; the scripted enemy
(`ScriptedEnemy`, out of SimHost) reads the player's world; the world skips its full-state hash when nobody compares it
(`SimWorld.HashInterval = 0`); the tick loop lives in `LockstepSession`, which a test can drive.

Two faults in the old loop surfaced on the way, both of which made the enemy's orders depend on network timing:
the scripted peer re-ran at the same tick whenever the loopback stalled it (a second deploy; a barrage *and* gas on
the support tick), and the stress preset timed the PLAYER's deploys by the PEER's tick. The first benchmark of the
one-world build measured a different battle from the canary (different `hash_start`), which is how the second was
found; `SinglePlayerEquivalenceTests` now plays one match three ways (one world, zero-lag canary, lossy canary, stress
preset included) and holds all three hash-identical every tick.

Same battle, `hash_start` 252EA3E1DA8F7314 in both columns (editor, 1,500 a side, ticks 1800–2200):

| | canary (two worlds, as every session ran) | one world |
|---|---|---|
| `TW.Sim.Step` per tick | 21.4 ms | **10.3 ms** |
| `TW.Host.Update` per tick | 25.7 ms | 15.9 ms |
| Main thread p50 / p95 / p99 | 8.8 / 33.8 / 61.2 ms | **6.2 / 21.7 / 31.9 ms** |
| Frames over 33 ms (of ~1,300) | 64+ (the cap) | 20 |
| Mean fps (editor) | 64.7 | 93.2 |

`TrenchGarrisonSystem` is now 8.0 of the 10.3 ms left in a tick, and is next.

### The garrison search that could never succeed (same day)

`TrenchGarrisonSystem` gave every garrisoned man without a post a nearest-post search on every tick: `Nearest` walks
every cell of the trench up to five times (four spacing rungs), and when that fails it does it again for the other post
kind. With an army out most men sit in FULL trenches, so the search walked the whole trench ten times per man per tick
to return -1. The system now keeps, per trench and post kind, a count of the free posts it lists (an upper bound, built
after the holders are known and counted down as posts are taken) and skips `Nearest` for a kind whose count is 0.
`Nearest` has no side effects and returns -1 whenever no post of that kind in that trench is free, so the results are
the same by construction; the bench confirms it on the full battle (same `hash_start`, same survivors at tick 2200).

Same battle, one world (editor, 1,500 a side, ticks 1800–2200):

| | before | after |
|---|---|---|
| `TrenchGarrisonSystem` per tick | 7.98 ms | **0.15 ms** |
| `TW.Sim.Step` per tick | 10.3 ms | **2.35 ms** (budget 3.6) |
| Main thread p50 / p95 / p99 | 6.2 / 21.7 / 31.9 ms | **5.7 / 12.8 / 18.7 ms** |
| Frames over 33 ms | 20 | 6 |
| `Stress_ThreeThousandUnits_…` test runtime | 76 s | 10.4 s |

From the first baseline of this pass to here, on the editor stress battle: main thread p95 38.5 → 12.8 ms, p99
84.8 → 18.7 ms, sim per tick 21.8 (two worlds) → 2.35 ms, GC p95 461 KB → 12 KB a frame. What is left above the 3 ms
main-thread budget is presentation (`TW.Anim.Advance` is now the largest single marker at ~1.5 ms a frame) and the
crater frames, which are next.

### The crater frame, first half: the terrain (same day)

With presentation markers in (one per Update/LateUpdate, sections inside the terrain), the 64–85 ms hitches read as
three whole-map jobs landing in the frame a shell does: the terrain's chunk rebuild (27.4 ms worst frame in the
editor), the props' Compose (25.6 ms) and the terrain's hollow rescan (11.0 ms).

- **Chunks** are re-read row by row under a 2 ms budget (`GreyboxTerrainView.ChunkBudgetMs`) instead of two whole chunks a
  frame, and uploaded when their last row is read. A vertex's normal samples the surface 0.25 m either side of it, and
  those points are its neighbours' too, so each is sampled once: half the ground samples. Every position involved is a
  multiple of 0.25 m, exact in float, so the values are the same floats; checked in Play over all 27 chunks, 102,663
  vertices: 0 positions and 0 normals differ from the old per-vertex formula. Worst chunk frame **27.4 → 2.2 ms**.
- **One heavy job a frame** (`HeavyWork.TryClaim`): the hollow rescan and the props' Compose no longer stack; a refused
  job stays pending for the next frame. Chunks wait while the hollows are pending (they read the pools), and so does
  Compose, which read them too and could before run on the old ones in the same frame.

Worst frame on the stress battle 84.9 → 60.6 ms, p99 22.6 → 19.2 ms. Left: Compose itself (28.5 ms on the live field:
the composer's Build 21.5 ms — Landmarks 6.1, Margins 3.6, Litter 2.3 — and ~7 ms applying 7,112 instances). It
places against the current ground (a crater can move an MG nest), so it cannot be cached; it is to be spread over
frames, which waits on the village work in BattlefieldComposer being committed.

## The first Windows player, and what it was really measuring (2026-09-23)

`TW/Build/Windows Bench` built the first Windows player (release 208 s, 185 MB; development ~2 min, 240 MB). Its
first report agreed with the editor's sim to the bit (same `hash_start` at tick 1800), which is good news for lockstep
from editor to player. The rest of it was measuring a broken game, and nothing in the project could have said so:

- **Six shaders the game finds by name were not in the build.** `Shader.Find` only finds what a build contains: a
  shader a shipped material uses, or one on GraphicsSettings' Always Included list. TW/Debris, TW/Sea, TW/Tank and
  TW/TankDisc were on neither (no debris, no sea, box tanks), nor were URP Unlit and Lit (no tracers, sparks, smoke,
  gas or bodies). `new Material(null)` threw, and CombatFx and Ocean threw again every frame: 8,500 exceptions in a
  22 s benchmark, **256 KB and 435 allocations of garbage a frame, 91 collections** in the window. The editor finds
  every shader in the project, so no test, capture or Play session had ever seen any of it. The four TW shaders are
  now always included like the other thirteen; URP Unlit and Lit are kept by three materials in `Resources/ShaderKeep`
  set up the way the code sets up its own (instanced; transparent-surface keyword), rather than always including every
  variant of URP Lit. `ShaderInclusionTests` scans the source for `Shader.Find` names and fails on any not in a build.
- **The main menu stayed drawn over every match.** `ShellRouter.Pop` cleared a screen's Root before removing it from the
  panel, so no popped screen ever left it. Pressing Play in the battle scene never pushes the menu, so the editor never
  showed it; a player boots to the menu, and after Skirmish the menu sat over the whole battle. (The fault would also
  have left a resumed pause menu on screen.) Found by the benchmark's own screenshot (`shot=`), fixed by swapping the
  two lines; `ShellRouterPlayTests` fails on the old order and passes on the new.

Development player, same battle, broken → fixed:

| | broken build | fixed |
|---|---|---|
| GC per frame p50 | 256 KB (435 allocations) | **1.3 KB (16 allocations)** |
| Collections in the 22 s window | 91 | **1** |
| Main thread p50 / p95 / p99 | 6.1 / 11.4 / 19.1 ms | **5.4 / 9.0 / 12.6 ms** |
| GPU p50 | 4.2 ms | 4.7 ms (it now draws debris, sea, tanks and effects) |
| Draw calls / SetPass | 309 / 213 | 330 / 236 |

The bench now launches the match from the main menu the way the Skirmish button does (`MatchLaunch.Start`), so the
player is measured in the state a player is in, and it saves a screenshot of the held view during warm-up.

### The crater frame, second half: the props' composition (same day)

`BattlefieldComposer.Build` is now `BuildSteps`, one generator a step (`Build` runs them all, unchanged for any
caller), and `BattlefieldProps` stages a recomposition a step a frame into a list and rebuilds its batches from it in a
frame of its own, each step a `HeavyWork` turn. What is drawn is always a whole composition; a crater mid-way only
re-dirties, so the next starts when this one ends and the props converge on the ground as it is. The first
composition, and one after `Restyle`, is whole, so the field is dressed from the first frame; the hamlets and rear
structures are still placed once, on the first layout.

Checked by fingerprinting every prop instance (module, and every matrix bit for bit) on the benchmark battle held at
tick 1802 (world hash 170A83FEB9BD6AA0): the old Compose, the new whole Compose and the new spread one all give
**7,198 instances, 1B7C3EF7BF477CFF**.

Worst props frame **27.3 → 7.0 ms**; worst frame of the window 60.6 → 45.9 ms. What remains in the crater frames is
the terrain's hollow rescan (~11 ms, whole-map and order-dependent) and a sim tick (Blast/Deformation), now each in a
frame of their own.

**A measuring caveat learned here.** Two runs of the same build an hour apart, same battle, same draw counts
(363 draws, 1.42 M triangles), differed by 1.3 ms of GPU and 0.4 ms of main thread: the laptop after three player
builds, not the code (the per-system markers were unchanged). Compare runs taken back to back, or interleaved.
