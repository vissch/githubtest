# Asset scale audit

Not yet generated. This file is written by `TW/Audit/Asset Scale` (`Editor/AssetScaleAudit.cs`), from an open
editor or with the editor closed:

```
Unity.exe -batchmode -quit -projectPath <checkout>/trench-warfare-3d -executeMethod TW.Editor.AssetScaleAudit.Run -logFile audit.log
```

It composes the real battlefield of seed 1917 with no scene, styles every instance as the game draws it, and lists
every module against the soldier (1 SU = `FigureMetrics.HeightM`, 2.0 m): class, bounds, mesh size, look, placed
instances, drawn size, standard-view ratio, how many the clamp touched, verdict. The first run waits for a machine
with enough free memory to open an editor (2026-09-26: under 1 GB free); `AssetScaleTests` checks the looks and
the hand edits without it. Design: `docs/21-overhaul-2026-09.md` phase 1, `docs/13-environment-system-rebuild.md`.

## Interim: the procedural kit as built, computed by hand (2026-09-26, critique rounds 2-3)

Until the audit runs, every `kit/*` row that enforces was computed from its builder in `BattlefieldKit.cs` at the
scale the kit builds it (the gate test `AssetScaleTests.The_Kit_Pieces_Stand_In_Bounds_As_Built` judges the same
thing on the real meshes and is the verdict that counts). 1 SU = 2.0 m; the axis is the row's.

| Module | As built | Row | Holds |
|---|---|---|---|
| kit/helmet (shrunk 2026-09-26: brim 0.43 → 0.31 m) | 0.147 SU | 0.13-0.18 | yes |
| kit/boots (a pair laid out) | 0.27 | 0.20-0.30 | yes (row widened) |
| kit/ammoTin (with its spilled rounds) | 0.31 | 0.22-0.32 | yes (row widened) |
| kit/messKit (height) | 0.05 | 0.04-0.08 | yes (row corrected: was 0.10-0.22, drawn double) |
| kit/hangingTins | 0.21 | 0.10-0.26 | yes (row widened) |
| kit/supplies (the braced crate) | 0.525 | 0.30-0.55 | yes (row widened) |
| kit/shellCases (three cases, a group) | 0.32 | 0.28-0.45 | yes (row corrected twice: the first hand pass composed the rotations in the wrong order) |
| kit/bucket | 0.15 | 0.10-0.22 | yes |
| kit/wireTins | 0.12 | 0.10-0.22 | yes |
| kit/lantern | 0.75 | 0.60-0.90 | yes |
| kit/spade | 0.40 | 0.25-0.45 | yes (row raised: it stood 2 mm under the old ceiling) |
| kit/leanRifle | 0.62 | 0.50-0.70 | yes |
| kit/graveMarker | 0.64 | 0.50-0.70 | yes |
| kit/signBoard | 0.70 | 0.60-0.85 | yes |
| kit/looseBoards | 0.95 | 0.40-1.10 | yes |
| kit/knifeRest | 0.67 | 0.50-0.85 | yes |
| kit/ladder | 1.12 | 1.00-1.40 | yes |
| kit/duckboards | 1.00 | 0.90-1.10 | yes |
| kit/sandbags | 0.29 | 0.25-0.45 | yes |
| kit/dugout | 1.11 | 1.00-1.80 | yes |
| kit/bunker | 1.39-1.43 | 1.00-1.80 | yes |
| kit/roof (the mound, skirt to crest) | 1.55-1.69 | 1.00-1.80 | yes |
| kit/TrenchWalls, TrenchBags, TrenchFloors (+ damaged) | per axis by TrenchKit | report only | not clamped |

The numbers were re-derived on 2026-09-26 by a replication of the builders' geometry (`Blob`, `Taper`, `Sack`,
`WornBox`, the cube, `Combine`'s TRS with the rotations composed Z, X, Y about the world axes) in critique round 3;
a first hand pass had the shell cases at 0.455 SU by composing them in the wrong order. The first gate run replaces
this table with the measured one.
