# To lane/show/aosa: crossing the CombatFx.cs split

The integration branch split `Presentation/Camera/CombatFx.cs` into partials (`CombatFx.Ground.cs`, `CombatFx.Chunks.cs`,
`CombatFx.Ambient.cs`, `CombatFx.Bodies.cs`) and moved `CameraShake` to its own file. Git cannot follow your edits
into the moved code. Rebase onto the integration branch; each time it stops on `CombatFx.cs`:
```bash
python Tools/port_split.py Assets/_Project/Presentation/Camera/CombatFx.cs --rebase
```
then compile, apply anything left in `CombatFx.cs.port.rej` by hand, `git add`, `git rebase --continue`. A trial on
2026-09-27 placed 17 of your 20 edits; the 3 left are real conflicts: `MaxMarks` is now 900 in `CombatFx.Ground.cs`,
and the Column and Burst `books.Add` calls in the shell-burst handler gained arguments (`flight * lean`).

Also: your `public static bool ShowOverlays` fails `StaticLifecycleTests` until it resets when Play ends
(`SceneStatics.Register`) or is explained in that test. `UI/Selection.meta` conflicts add/add (a folder GUID nothing
references): keep upstream. `FlipbookFx.cs`, `NightLights.cs`, `PropDestruction.cs` and `Flipbook_URP.shader` conflict
with the flamethrower commit 60714dd, not with the split. `Tools/aosa/land.ps1` silently reverts `TW-URP.asset` and the
URP global settings: an intended settings change is lost without a message.
