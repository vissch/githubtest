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
