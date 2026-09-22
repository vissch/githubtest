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
1. Load `GreyboxCorridor` (M1) or the mission map, spawn the stress preset (`TW → Debug → Stress 2000`).
2. Capture 600 frames with the Unity Profiler (Deep Profile off), export the `SimWorld.Step`, `SimPresenter`,
   `EventPump` and `Render` marker averages to `docs/perf/<milestone>-<machine>.md`.
3. Frame Debugger: count draw calls, confirm indirect batches.
4. Memory Profiler: confirm 0 B/frame managed allocation in play.

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
