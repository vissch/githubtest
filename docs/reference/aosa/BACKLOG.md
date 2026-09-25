# AOSA backlog

The format and rules are in `README.md` under "Cards". Only `aosa.py age` edits the age and idle columns. The
predicted deltas below are cycle-0 guesses from code reading, and `aosa.py learn` recalibrates them per class. Every
metric is a `tw-perf/1` series at p95 from the player release bench, unless it names another source.

Seeded 2026-09-25 from these sources:
- the perf plan's open items (`~/.claude/plans/check-the-current-unity-generic-leaf.md` Phases 2-4, each re-checked in
  code on this date)
- docs/05
- env-scoreboard "known going in"
- the rig loop's parked patches (docs/20 closing summary)
- the agent-memory open critique items

| id | class | tier | metric: now -> target | predicted delta | evidence | size | owner | age | idle | status |
|---|---|---|---|---|---|---|---|---|---|---|
| C01 | instrument | T1 | noise band: unknown -> 3x MAD per metric, release + dev | n/a | no interleaved repeats exist; docs/05 saw 1.3 ms GPU drift with no code change | S | loop | 0 | 0 | ready |
| C02 | instrument | T1 | player build: 47d8c52 -> HEAD of lane/show/aosa | n/a | Builds/WinBench/build-info.json is 20+ commits stale | S | loop | 0 | 0 | ready |
| C03 | instrument | T1/T2/T3 | env-scoreboard T1 NightMud column: all "-" -> scored from captures | n/a | three rounds without a capture (env-scoreboard log rows 1-3) | M | loop | 0 | 0 | blocked:editor |
| C04 | shader | T1 | gpu_ms: ? -> -0.2 | -0.2 | Ink Lines copies the HDR colour buffer only to lerp (`TW-Renderer.asset fetchColorBuffer: 1`); the mathematically equal blend needs no copy (perf plan 3.2) | S | loop | 0 | 0 | ready |
| C05 | cull | T1 | gpu_ms: ? -> -0.3 | -0.3 | tanks, wrecks, discs and flames submit with `Everywhere` 5 km bounds (`TankRenderer.cs:135,1146,1156,1220`), so they are never culled (perf plan 3.3) | M | loop | 0 | 0 | ready |
| C06 | cull | T1 | gpu_ms: ? -> -0.1 | -0.1 | DebrisRenderer draws each ring's full capacity, not its live range (perf plan 3.3) | S | loop | 0 | 0 | ready |
| C07 | shader | T1 | gpu_ms: ? -> -0.1 | -0.1 | rain streaks with alpha 0 are still rasterised (`Rain_URP.shader:73`); collapse them in the vertex shader (perf plan 3.4) | S | loop | 0 | 0 | ready |
| C08 | shader | T1 | gpu_ms: ? -> -0.1 | -0.1 | water loops all 16 rings per pixel; pack live rings and loop to a count (perf plan 3.5; `_TWRingCount` does not exist yet) | S | loop | 0 | 0 | ready |
| C09 | cache | T1 | main_ms: ? -> -0.1 | -0.1 | `HudBridge.UseToolkitHud` reads PlayerPrefs every call and DebugOverlay and TestPanel call it every frame (perf plan 2.5) | S | loop | 0 | 0 | ready |
| C10 | cap | T1 | main_ms: ? -> -0.05 | -0.05 | `Physics.simulationMode` is still Auto and PhysX is unused (perf plan 2.5) | S | loop | 0 | 0 | ready |
| C11 | cache | T1 | main_ms: ? -> -0.2 | -0.2 | FlipbookFx `RemoveAt(0)` at the cap and `RemoveAt(i)` in the draw loop are O(n) shifts on 1,536 cards; use a ring buffer and bucket by book (perf plan 2.4) | M | loop | 0 | 0 | ready |
| C12 | cache | T1 | main_ms: ? -> -0.1 | -0.1 | the same `RemoveAt(0)` pattern in VATRenderer fallen (`:331,392`) and in CombatFx rests, marks and bodies (`:337,343,575`) | M | loop | 0 | 0 | ready |
| C13 | cache | T1 | per_tick TW.Terrain.*: ? -> -30% | -0.3 | the ground colour map Apply rebuilds mips on the CPU per dirty frame (`GreyboxTerrainView.cs:568`); apply once the tile queue drains, or at most every 100 ms (perf plan 2.3) | M | loop | 0 | 0 | ready |
| C14 | instrument | T1 | the crater frame's worst ms: 45.9 -> named | n/a | docs/05: what remains in crater frames is the hollow rescan (~11 ms) and a sim tick; measure it with `scenario=barrage` before changing anything | S | loop | 0 | 0 | ready |
| C15 | budget-sweep | T1 | vat_shadows_on: 0 at 3,000 men -> on for more men | +1 | `LodTiers.VertexBudget` 1.5 M turns the men's shadows off in the stress battle; the clip removal freed 1.5 ms of opaque pass (docs/05, 2026-09-24). Sweep `vat.vertexBudget` 1.5/2.0/2.5 M | S | loop | 0 | 0 | ready |
| C16 | budget-sweep | T1 | gpu_ms per metre of `vat.lodDistance`: unknown -> knobs.json slope | n/a | perf plan 4.5 (far model at ~90 m instead of 170 m); sweep 90/130/170 first, and change it only with a critic pass | S | loop | 0 | 0 | ready |
| C17 | shader | T1 | shadow sharpness at equal gpu_ms | n/a | shadow distance is a fixed 220 m; fit it each frame to the visible ground plus a margin (perf plan 4.2). This is an "indistinguishable" class change and needs a critic | M | loop | 0 | 0 | ready |
| C18 | shader | T1 | memory: -12 MB per figure; texture fetches halved | n/a | VAT normals can pack into the position texture's constant alpha (`VatCodec.cs:109,180`; perf plan 3.6) | L | loop | 0 | 0 | ready |
| C19 | batch | T1 | setpass: ~236 -> -60 | -60 | 80 kit modules hold 80 materials that differ only by floats (docs/05 "Still open"); an experiment, since a property block may cost a state change of its own | L | loop | 0 | 0 | ready |
| C20 | juice | T1 | critic moment J01 (shell burst, T1): - -> 8 | +2 | explosions batch B (sky flash, shock ring, foliage bend, per-weapon recipes) is designed and waits on the owner's go (ASK.md A03) | L | owner | 0 | 0 | parked:ASK A03 |
| C21 | juice | T2 | critic moment J02 (cook-off): - -> 8 | +3 | the cook-off fireball flames draw for ONE frame, because TankRenderer's flames list is cleared every frame (agent-memory, blast reactions round) | S | loop | 0 | 0 | ready |
| C22 | juice | T1 | critic moment J03 (rifle volley legibility at T1): - -> 8 | +1 | env-scoreboard known going in: criterion 9 | M | loop | 0 | 0 | ready |
| C23 | art | T3 | critic env criterion 1 at T3: - -> 8 | +1 | the flipbook drawings' ink outlines do not follow zoom (env-scoreboard known going in) | M | loop | 0 | 0 | ready |
| C24 | art | T1 | critic env criterion 3 (Winter), ice luma spread 0.061 -> ~0.16: - -> 8 | +0.1 | winter ice is flatter than the snow beside it and has no glare (env-scoreboard known going in) | M | loop | 0 | 0 | ready |
| C25 | art | T1 | critic env criteria 4 and 5 (Coast): - -> 8 | +2 | the coast has no obstacles, no wire and no defender's trench on the sand (env-scoreboard); this is a map-design call, so the owner decides placement | L | owner | 0 | 0 | parked:ASK A04 |
| C26 | juice | T2 | critic rig W5 (lean into a turn): 6 -> 8 | +1 | parked patch: `wantRoll += yawRate * pace * k`, k ~0.06 (docs/20 closing summary, change 1) | S | loop | 0 | 0 | ready |
| C27 | juice | T2 | critic rig W3 (carriage reads as carried): 5 -> 7 | +1 | parked patch: stop filtering a walker's carriage twice (docs/20 closing summary, change 2) | S | loop | 0 | 0 | ready |
| C28 | juice | T2 | critic rig D1 (detached leg pops to full length): 2 -> 4 | +1 | `TankRenderer:770-785` rebuilds a detached part with `Vector3.one` scale (docs/20 A19) | S | loop | 0 | 0 | ready |
| C29 | art | T1 | critic env criterion 6 (men read at zoom 60, not white ghosts): - -> 8 | +1 | agent-memory: VAT albedo ~0.35, team band on the helmet, ground disc (queued by claude-68); check the current state first | M | loop | 0 | 0 | ready |
| C30 | instrument | T1 | FrameBudget in the bench JSON: absent -> sampled | n/a | env-scoreboard round 1: "PerfBench does not sample FrameBudget yet" (built in this branch; the card closes when a bench shows it) | S | loop | 0 | 0 | ready |
| C31 | instrument | T1 | FrameBudget coverage: 5 of 9 submitters -> 9 of 9 | n/a | on this branch VATRenderer, TankRenderer, BattlefieldProps and PropDestruction still call Graphics.* directly (commit 3c54769 routed them only in the shared working tree, uncommitted by their owners); until they land, frame_budget_* reads LOW and rule 7 cannot see those four | S | loop | 0 | 0 | ready |
