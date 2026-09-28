---
name: tw-lowpoly
description: Low-poly rebuilds for Trench Warfare 3D, made agentically in Blender 5.0 on the desktop — give every model at least one lower-polygon form (sometimes several LOD levels), check each against the original from every side, prove it at every zoom band, and propose the swap with a before/after sheet. Knows the repo's Blender split scripts and their LOD flags (tanksplit, tank3split, mechsplit, jeepsplit, frogrig, housesplit, envsplit), battlelineup.py's pop metric, TankModel's LOD0/1 switch, the VAT far figure, BattlefieldKit modules, the house chunk-mask seam, the budgets and the traps that already bit. Use for "make low-poly versions", "LOD the houses", "the Maw needs a far LOD", "rebuild the props lower", "which models are over budget". NOT for perf measurement alone (tw-optimizer) or new art.
---

# Low-poly rebuilds (Blender, agentic)

**The owner's words:** "The current models are generally OK, but we need lower-poly models, rebuilt agentically in
Blender. Each model needs at least its low-poly form; sometimes multiple levels." Deep reference:
`references/lod-lessons.md`: every script's LOD flags, measured numbers, and traps. Tool docs: `docs/reference/pipelines.md`
"Blender splits". Budgets: `docs/22-asset-playground.md` "LOD distances"; `decisions.md` (units, vehicles).

Blender: `"C:\Program Files\Blender Foundation\Blender 5.0\blender.exe" -b --factory-startup -P <script> -- <args>`,
desktop only, through `run_detached.py` for anything over a few minutes.

## The inventory (build it first, keep it on the board)
One row per model: family, file, triangles and vertices per existing LOD, where it is drawn (which bands, how many at
once at T1), and the target. `lodmake.py --inventory` writes it once that tool exists. Until then, a Blender pass over
the FBXs does the same. Today:

| Family | Files | LODs today | Target |
|---|---|---|---|
| Battle machines (11) | `Resources/Vehicles/<Name>/<Name>_LOD0,1.fbx` | LOD0 + LOD1 (LOD1 is the far form) | Check every LOD1 against the far cap (≤ 1,500 tris). LOD0 over 5k gets a mid LOD only when a band shows the pop |
| Houses | `Resources/Env/Houses` (83 chunk FBXs), `Ruins/Chunks` (220) | none: every intact house draws all its chunks | **Intact-house LOD1** (one merged, decimated mesh) plus a far LOD2 block. Chunks swap in on the first hit |
| Env props | `Resources/Env/<Set>` (Fence, Military, Plants, Siege, Stones, Weapons, Wood) | none; small ones are culled past `BattlefieldKit.SmallReach` (55 m) | LOD1 for each prop drawn past 55 m, and a far LOD where overview/far shows it |
| Infantry | VAT Soldier / Sniper + the 264-vertex far box | 2 tiers | A mid figure is the **owner's call**: it costs a VAT bake, about 20 MB of history |
| Playground figures and tanks | `Playground/Art/...` | 3-4 LODs already | none |

## The loop per model (at most 3 rounds, then BLOCKED with what source is needed)
1. **Predict** the triangle target and the band where the swap happens, before you touch anything. Use
   `docs/22`'s screen-height cuts: vehicle LOD1 above 0.22, LOD2 beyond ~145 m; figure LOD2 out to ~167 m.
2. **Make it** in the cheapest way that holds:
   - a Tripo lower model of the same object, when the owner has one: split by the **same rules** (`TW_LOD2=tripo`);
   - LOD0 decimated **part by part** (`TW_LOD2=derive`, `TW_PIECE_TRIS` floors);
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
5. **Propose** the swap with the sheet. The swap is applied only on the owner's word. **Never overwrite the original**:
   new files beside it, `<Name>_LOD2.fbx`.
6. **Learn:** record the numbers that worked (tris, IoU, the flags) in `references/lod-lessons.md` and in the board's
   `lessons/lowpoly.md`.

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
