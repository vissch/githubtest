# LOD lessons and flags (snapshot at `c43b73f`, 2026-09-28)

Sources: `docs/reference/pipelines.md` "Blender splits"; `docs/22-asset-playground.md` "LOD distances" and "Traps
found here"; the script headers. **The scripts win over this page.**

## Script flags that make lower forms
| Script | LOD flags | Notes |
|---|---|---|
| `tanksplit.py` | a full FBX and a far FBX → `<Tank>_LOD0,1.fbx` | Maw, Tusk |
| `tank3split.py` | three Tripo LODs → `_LOD0..2`; `TW_DERIVE`, `TW_SCALE` | the SAME named parts at every LOD; fails if a part is empty |
| `mechsplit.py` | `TW_LOD2=derive\|tripo`, `TW_LOD2_TRIS`, `TW_PIECE_TRIS`, `TW_BATTLE=1` (the battle's LOD0/1, where LOD1 is the far form at the LOD2 budget), `TW_KIND=hover\|halftrack` | LOD1/2 derived part by part to Tripo's counts |
| `jeepsplit.py` | `TW_LOD2=tripo\|derive`, `TW_LOD2_TRIS` | LOD1 = sqrt(t0 × LOD2) triangles; LOD2 from Tripo's lower model because LOD0 at 13-20 % tore into spikes |
| `frogrig.py` | `TW_DERIVE` (12, the owner's default), `TW_LOD3_TRIS` (300), `TW_LOD2_FROM_LOD1`, `TW_LOD3_KEEP`, `TW_DERIVE_SYM` | skinned; weights transferred from LOD0 |
| `housesplit.py` | `TW_CUT`, `TW_SMALL`, `TW_MINTRIS`, `TW_FLOORS`, `TW_ONE`+`TW_KEEP` | chunks, not LODs. Houses use `TW_PIVOT=chunk TW_CUT=2.4 TW_SMALL=1.4` |
| `envsplit.py` | none | props at full detail |
| `battlelineup.py` | `-- <outdir> <Name>...`, `TW_ENGINE=workbench` (reads the `TW_BATTLE=1` FBXs mechsplit writes) | pop.json: the worst side's silhouette IoU and block colour, LOD0 vs LOD1 |

## Measured numbers
- Frog LOD3: 219 tris → IoU 0.82; 300 → 0.85; 380 → 0.87 (300 ships).
- Skimmer 7,098 / 1,391 tris; Salvo 7,863 / 1,830 (LOD0 / LOD1). Both LOD0s are over the 3-5k vehicle budget.
- Figure LOD1 is 1,924 vertices, over the 1,200-1,500 crowd budget; LOD2 is 963.

## Budgets
| Model | Budget |
|---|---|
| Unit | 1,200-1,500 vertices, 2,000 max |
| Far unit (beyond 170 m) | 250-400 |
| Vehicle | 3,000-5,000 tris |
| Far vehicle | ≤ 1,500 tris ("the far LOD's cap is 1,500", `docs/22` ~191) |
| VAT vertices on screen | `vat.vertexBudget` 1.5 M |
| Draw calls | < 300 (374 measured in the VFX catalogue's barrage frame), so a LOD that merges draws is worth more than one that only cuts triangles |

## Screen-height cuts (docs/22)
| Model | LOD0 | LOD1 | LOD2 | LOD3 |
|---|---|---|---|---|
| Figure | > 0.20 (~22 m) | > 0.08 (~63 m) | > 0.03 (~167 m) | beyond |
| Vehicle | > 0.70 (~45 m) | > 0.22 (~78 m) | beyond ~145 m | — |

The picker moves one level at a time with a 10 % margin at each cut. A two-level jump once left a vehicle at LOD0 at
240 m.
