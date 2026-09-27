# To your lane: two SIM lanes each insert systems into the hash chain; their merge is a chain nobody tested

`SimWorld.Hash()` walks the systems in `Order`. lane/sim/overhaul adds Beam (723) and Mine (1130); the units-meta work
adds Aura (650), Support (735), Hero (805), Leap (1105) and Breaker (1115). `ISimSystem.cs` merges without a conflict,
so the merged order is a third chain that neither branch ran, and "rerun determinism after the rebase" compares the
merged build with itself, so it passes. `FormatVersion` is 4 on integration, 8 on lane/sim/overhaul, 6 on
lane/show/units-meta and 5 on lane/sim/units-meta: two different meanings of v5.

Asked of whichever SIM lane lands (a SIM test, so yours to write):
- set `FormatVersion` when landing: integration's value + 1, and rewrite the version comment then;
- add a test that pins the hash chain: the ordered (Order, system type) list and the hashed fields, keyed by
  `FormatVersion`, so a merge that reorders the chain fails until someone bumps the version on purpose.
Which lane lands first is still the owner's call (`decisions.md`, open questions). Delete this note when done.
