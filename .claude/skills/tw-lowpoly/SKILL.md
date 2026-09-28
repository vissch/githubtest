---
name: tw-lowpoly
description: Low-poly rebuilds for Trench Warfare 3D, made agentically in Blender 5.0 on the desktop — give every model at least one lower-polygon form (sometimes several LOD levels), check each against the original from every side, prove it at every zoom band, and propose the swap with a before/after sheet. Knows the repo's Blender split scripts and their LOD flags (tanksplit, tank3split, mechsplit, jeepsplit, frogrig, housesplit, envsplit), battlelineup.py's pop metric, TankModel's LOD0/1 switch, the VAT far figure, BattlefieldKit modules, the house chunk-mask seam, the budgets and the traps that already bit. Use for "make low-poly versions", "LOD the houses", "the Maw needs a far LOD", "rebuild the props lower", "which models are over budget". NOT for perf measurement alone (tw-optimizer) or new art.
---

# Low-poly rebuilds (Blender, agentic)

**The owner's words, verbatim:** "The current models are generally OK. But we need to make lower poly models. We will
do this agenticly rebuild them in blender. Each model needs atleast their lowpoly form. Sometimes multiple levels of
minimalization."

**Coverage rule:** every model gets at least one lower form, or a written exemption the owner approved. The inventory
has a "low form: Y / N / exempt" column. The work is done at 100 %, not when the far view looks fine. Deep reference:
`references/lod-lessons.md`: every script's LOD flags, measured numbers, and traps. Tool docs: `docs/reference/pipelines.md`
"Blender splits". Budgets: `docs/22-asset-playground.md` "LOD distances"; `decisions.md` (units, vehicles).

Blender: `"C:\Program Files\Blender Foundation\Blender 5.0\blender.exe" -b --factory-startup -P <script> -- <args>`,
desktop only, through `run_detached.py` for anything over a few minutes.

## The inventory (build it first, keep it on the board)
One row per model: family, file, triangles and vertices per existing LOD, where it is drawn (which bands, how many at
once at T1), low form (Y/N/exempt) and the target. `lodmake.py --inventory` writes it once that tool exists (**to build**).
Until then, a Blender pass over the FBXs does the same, counting `len(mesh.polygons)` after triangulating. Today:

| Family | Files | LODs today | Target |
|---|---|---|---|
| Battle machines (11) | `Resources/Vehicles/<Name>/<Name>_LOD0,1.fbx` | LOD0 + LOD1 (LOD1 is the far form) | Every LOD1 ≤ 1,500 tris (the far cap, `docs/22` ~191). A LOD0 over the 5k vehicle budget gets a mid form when a band shows the pop (IoU < 0.85) |
| Houses | `Resources/Env/Houses` (83 chunk FBXs) | none: every intact house draws all its chunks | **Intact-house LOD1** (one merged, decimated mesh) plus a far LOD2 block. Chunks swap in on the first hit. Chunk LODs are the owner's question Q-B4 |
| Ruins | `Resources/Env/Ruins/Chunks` (220) | none | owner's question Q-B4 |
| Env props | `Resources/Env/<Set>` (Fence, Military, Plants, Siege, Stones, Weapons, Wood) | none; small ones are culled past `BattlefieldKit.SmallReach` (55 m) | LOD1 for every prop, and a far LOD where overview/far shows it |
| Infantry | VAT Soldier / Sniper + the 264-vertex far box | 2 tiers | A mid figure is the **owner's call** (Q-B3): a bake rebakes both figures, about 38 MB of history |
| Playground figures and tanks | `Playground/Art/...` | 3-4 LODs already | none |

## The loop per model (at most 3 rounds, then BLOCKED with what source is needed)
1. **Predict** the triangle target and the band where the swap happens, before you touch anything. Use
   `docs/22`'s screen-height cuts: vehicle LOD1 above 0.22, LOD2 beyond ~145 m; figure LOD2 out to ~167 m.
2. **Make it** in the cheapest way that holds:
   - a Tripo lower model of the same object, when the owner has one: split by the **same rules** (`TW_LOD2=tripo`);
   - LOD0 decimated **part by part**: `mechsplit`/`jeepsplit` `TW_LOD2=derive` with `TW_LOD2_TRIS` and `TW_PIECE_TRIS`
     floors, or `tank3split`/`frogrig` `TW_DERIVE`. `tanksplit` takes a full and a far FBX and has no flags;
   - a new generic pass, `Tools/lodmake.py` (to build: one mesh in, LOD1/LOD2 out by triangle target, symmetry-aware
     collapse, UVs kept, a silhouette check). It goes in a Tools-only commit and is named in `pipelines.md`.
3. **Check it offline:** `battlelineup.py` pop metric, four sides at the same ortho framing:
   - silhouette IoU ≥ 0.85 against the level above (frog LOD3 measured 0.82 at 219 tris, 0.85 at 300);
   - block colour difference;
   - no part empty at any LOD;
   - pivots and sockets unchanged.
4. **Check it in the game:**
   - the gym, or the Playground `lod` / `lodfit` / `lodpop` / `lodtint` commands, at the band where it swaps and the
     band above;
   - then a before/after sheet by `tw-optimizer`: images per band, `CaptureRig.Diff`, draws, vertices, triangles.
5. **Propose** the swap with the sheet. The swap is applied only on the owner's word. **Candidates live only in the run
   folder** (`%LOCALAPPDATA%\TrenchWarfare\runs\<job>`, with a `KEEP.txt` naming what it waits on, so housekeeping
   leaves it alone). Approved files are then copied into `Resources/` beside the
   original (`<Name>_LOD2.fbx`) and committed. **The original is never overwritten.**
6. **Critic round** (`tw-critic` lowpoly rubric, rotating angles). Keep the best round, not the last.
7. **Learn:** record the numbers that worked (tris, IoU, the flags) once, in the board's
   `lessons.md`. A flaw that recurs becomes a proposed checklist line, which the owner approves.

## Contracts that bite
- **The house chunk mask is a seam** (`Toon_URP` float4, 96 chunks, `HouseKit.ChunkMask`). A merged intact-house LOD
  carries no chunk ids. So it draws **only while the house is untouched**, and the first `PropChanged` swaps to the
  chunks. The switch is SHOW code in `BattlefieldKit`/`PropDestruction`. It never changes the mask layout.
- **`VehicleSize` is baked into the meshes** (a seam). Export at the same scale; `battlelineup.py` shows the lineup.
- **The env atlas grid is a seam** (`BattlefieldKit` ↔ `Tools/envatlas.py`). A LOD keeps the same UV tiles.
- **Unity makes an LODGroup by itself** for meshes named `*_LOD0..n` inside one FBX, and that group decides what draws.
  Any loader must remove it (as `UnitRig.Build` does) or the files must be named otherwise. `TankModel` loads
  `<Name>_LOD0/1` as separate files.
- **Tripo LODs are not the same object decimated.** Match parts by region, never by island, and check where each part
  sits.
- **Decimating LOD0 hard tears it**: the jeep at 13-20 % tore into spikes, and the frog's head broke at 24 %. Derive
  LOD1 from LOD0, then LOD2 from Tripo's own lower model or from LOD1 (`TW_LOD2_FROM_LOD1`).
- Binaries go straight into git (no LFS). Commit only the LODs the owner approved, never the candidates. Candidates
  live in the run folder (`tw-housekeeping`).

## Hand-offs
- Measuring and the before/after sheet: `tw-optimizer`.
- Proving a model at every band: `tw-gym`.
- Scoring the look: `tw-critic` (lowpoly rubric).
- A mid infantry figure, or a LOD that changes the mask, atlas or `VehicleSize`: ask the owner.
