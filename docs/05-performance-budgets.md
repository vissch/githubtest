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
