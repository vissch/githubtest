# PerfMarkers.Names now carries the per-system names

From `lane/sim/found-put-each-system-s-profiler-marker-name-in-pe-20261017-0`
(commit tagged `[found-put-each-system-s-profiler-marker-name-in-pe-20261017-0]`).

`PerfMarkers.ForSystem` (Sim/Core/PerfMarkers.cs) now adds its marker name to `PerfMarkers.Names` itself, the first
time any world registers that system type. `PerfBench.cs:544` (`foreach (var s in host.Local.World.Systems) Marker("TW.Sim.Sys." + s.GetType().Name);`)
is now redundant and should be deleted — it is SHOW's line, so this lane did not touch it.

Until it is deleted, every `TW.Sim.Sys.*` key appears twice in a bench report's `series` and `per_tick_ms` objects
(same marker, same values — `PerfBench.New` does not de-duplicate, and JSON keeps the last write, so nothing breaks,
but the duplication is unnecessary now).

Delete this note once `Perf/PerfBench.cs:544` is removed.
