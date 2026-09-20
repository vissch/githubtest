# Plan review and re-cut (2026-09-20)

> Part of the Trench Warfare 3D roadmap. See [README](../README.md) for the index.
> This document records what was wrong with the handed-off plan and skeleton, the decisions the owner made in
> response, and the re-cut milestones. Where it conflicts with `PLAN.md` or docs 00–10, **this document wins**;
> the older docs carry a banner pointing here where they were superseded.

## 1. Decisions taken by the owner

| Question | Decision | Consequence |
|---|---|---|
| Ship platforms | **Windows x64 only** | Determinism = Burst `FloatMode.Strict` on x64. No ARM64 gate, no fixed-point fallback. Online play, when it comes, is x64-only. |
| Online multiplayer | **Keep the door open** | Lockstep sim, replays and `LoopbackTransport` (N1) stay. Transport, hash exchange, late join (N2, N3) move **after** the single-player slice. |
| Team and art | **2 developers, one can do art** | Two-track schedule stands. `C4 art` gets an owner and a week count instead of "ongoing". |
| Unit density | **Decide at the fun gate (M1.5)** | VAT + impostors + `RenderMeshIndirect` (B3) is not scheduled until a playtest shows 2,000 units matter. If ~300–500 is enough, B3 shrinks to GPU-instanced skinned meshes. |

## 2. The baseline was not true — fixed in P0.5

The handoff said "Phase 0 implemented, compilable stubs, known-good pins". None of that held on a machine with Unity.

| # | Claim | Reality | Fix (commit "P0.5") |
|---|---|---|---|
| 1 | Compilable | `MovementSystem` declared a field `Hash` and a method `Hash(ulong)` → CS0102. `TW.Sim.Nav` failed, and every other assembly references it, so **nothing compiled**. | Field renamed `Spatial`. |
| 2 | Known-good pins | `com.unity.entities.graphics@1.3.14` does not exist on the registry; Package Manager aborted the whole import. | Entities and Entities Graphics removed (see §3). |
| 3 | Press Play → capsules flow | `ProjectSettings/` held only `ProjectVersion.txt`: render pipeline was Built-in, Active Input Handling was legacy, so URP shaders fell back and `Keyboard.current` was null. | `TW → Setup Project` now also switches input handling; `BootstrapSceneBuilder.SetupAll` runs it all from batch mode; real `ProjectSettings/` committed. |
| 4 | — | PlayMode test asmdef did not reference `TW.Sim.Nav` although `MatchSim` exposes `MovementSystem` (CS0012). | References added. |
| 5 | — | `SliceDefinitions.Create()` created folders on disk and called `AssetDatabase.CreateAsset` before an import. | `AssetDatabase.Refresh()` added. |
| 6 | — | No `.meta` files; first open generates ~120 GUIDs. | Generated once and committed before any parallel work. |
| 7 | `validate.py` "passes" | It walked `Library/PackageCache` and only checked braces, so it passed with a compile error. | Restricted to `Assets/`; the real gate is `unity test` (§6). |
| 8 | docs/03: xxHash64, LUT sin/cos | Code is FNV‑1a 64 and a polynomial sin/cos. | Docs corrected. |

## 3. Architecture changes

**Entities and Entities Graphics are out.** No assembly referenced them, no code used them, the VAT path was
`Graphics.RenderMeshIndirect` anyway, and `BatchRendererGroup` works without Entities. For a two-person team they
were a compile-time and versioning tax with no consumer — and they broke the import. Vehicles, props and trench
kit render through `BatchRendererGroup` (or `RenderMeshInstanced` until profiling says otherwise). Re-add
Entities only when a measured need appears.

**Floats, not fixed-point, and say so.** `SimMath.Fixed` was a `const Shift` and two converters; every sim array
is `NativeArray<float3>`. The "local swap" promised in docs/03 was a rewrite of `Sim/`. With Windows x64 as the
only platform the decision is: Burst `FloatMode.Strict` + `FloatPrecision.Standard`, same-build determinism,
same-architecture matchmaking if online ever ships. The cross-platform gate is retired; the same-machine gate
(`TW → Determinism → Write Platform Report`, compared across two builds/runs) stays.

**Replay header v2.** The only versioning was the magic `TWR1`; changing a unit's HP in data would silently
desync every stored replay. The header now carries `FormatVersion` (2), `MapHash` and `DataHash`. Logged in
docs/02 as the contract change it is.

**`MaxSlots = 2048` is a combined cap** (infantry + vehicles + emplacements). The perf target of "2,000 infantry +
20 vehicles + 40 emplacements" does not fit; bump to 4096 when A3 lands vehicles and emplacements.

## 4. Scope: what the slice keeps, what waits for the fun gate

Everything the 2D game has is kept: tug-of-war axis, five slots, `>>` / roster / lock / `↩`, silver economy,
off-map support. The plan then added ~30 new systems. The slice proves the **core 3D loop** first:

> garrison + fire-step + suppression/pinning + `>>` `↩` + HE barrage with craters + chlorine gas sinking into trenches.

| Cut or deferred | Was | Now |
|---|---|---|
| B4 gore (ellipsoid-clipped wounds, ragdoll → corpse baking) | M4 | Cut from the slice. VAT death frame + fade. |
| B3 four render tiers + 8-direction impostors | M2 | Decide the camera's max zoom-out first; a 300 m corridor at high-angle iso rarely shows > 150 m. Two tiers. |
| B5 volumetric gas raymarch (1.5 ms on a GTX 1050) | M3 | Particle fog volumes first. |
| A5 grab-bag (three "parts" across three milestones) | A5 | Three named phases: **A5a** artillery + gas/smoke fields, **A5b** armour + vehicles + creeping barrage + Bangalore, **A5c** mines, fascine, AA, flamethrower. |
| A2 / A3 overlap ("trench garrison" listed under both) | — | Garrison + fire-step belong to **A2**; stances, orders, economy, capture to **A3**. |
| N2 / N3 transport, late join, spectator | before M6 | After Mission 3 (owner decision). |
| C1 map authoring editor window | M1 | Parameterised generator (extend `GreyboxMapGenerator`) for the three maps. Build the window only if a level designer joins. |
| A7 modes and meta | M7 | Unchanged, post-slice. |

## 5. Re-cut milestones

| Milestone | Composed of | Acceptance |
|---|---|---|
| **P0.5 Baseline is true** | §2 fixes, Entities out, metas + ProjectSettings committed, `unity test` green (EditMode + PlayMode), CI workflow committed | `unity test --mode EditMode` and `--mode PlayMode` exit 0 on a fresh clone; `validate.py` OK |
| **M1 Greybox corridor** | A1 (goal-group `FlowFieldManager`, garrison stop, vehicle kinematics), B1 done, N1 done, greybox generator | 2,000 capsules flow through trench links; two loopback sims with 120 ms fake latency stay hash-identical for 5,000 ticks; sim ≤ 3 ms/tick |
| **M1.5 Fun gate** *(new)* | A2 core (LoS, direct fire, near-miss suppression, garrison/fire-step), A3 core (deploy, silver, `>>` / `↩`, objective capture), A5a-lite (one HE barrage with crater stamp, one gas cloud); capsules + IMGUI only | Two trench lines, riflemen + MG, one scripted enemy wave. **Playtest with both developers: is trench-to-trench assault better in 3D than in 2D?** Decide unit density and whether VAT is needed. Nothing below is scheduled until this passes. |
| **M2 Look** | B3 (as sized by M1.5), B6 UI, B2 terrain mesh + trench kit, C2 British/German infantry | Riflemen visibly garrison and fire from the fire-step; `>>` / `↩` from the UI; ≤ 3 draw calls per archetype if VAT |
| **M3 Mission 1** | A4 craters/wire/mud, A5a full, A6, B5 (particles), C3 Ypres | Mission 1 completable on Normal |
| **M4 Mission 2** | A5b, B4-lite (VAT death + fade), C1 Somme, C2 Mark IV | Mission 2 completable; tank crosses trench, bogs, crushes wire |
| **M5 Mission 3 + perf** *(was M6)* | A5c, B7 audio, C2 full, C3 Cambrai, profiling pass | Three missions Normal/Hard, 60 fps GTX 1050 @ 1080p, sim ≤ 4 ms/tick, 0 B/frame GC |
| **M6 Online** *(was M5; only if a launch requirement)* | N2, N3 | Two clients over a real network, desync detected and dumped |
| **M7 Modes + meta** | A7 | Survival, Operations, campaign shell, upgrades, skins |

### Estimate (honest)

Sizes in docs/01 are S/M/L with no conversion. Using S = 1, M = 3, L = 6 developer-weeks:

| Track | Dev-weeks |
|---|---|
| A sim (A1 L, A2 L, A3 L, A4 M, A5a/b/c 3×L, A6 M) | ~42 |
| B presentation (B1 S done, B2 M, B3 L→M after fun gate, B5 M, B6 M, B7 S) | ~14–17 |
| C tools/content (three maps via generator 3×S, C2 M, C3 3×S, C4 art **~12 with an owner**) | ~21 |
| N (N1 done; N2, N3 deferred) | 0 in slice |
| **Total to M5** | **~80 dev-weeks → ~10 months for two people with no slip; plan on 12–15** |

The first proof that the 3D bet is right (M1.5) lands at roughly week 8 instead of week 20+.

## 6. Process rules added

1. **Every A-phase ships a determinism test**: a `DeterminismReplayTests` case with that system registered, plus a
   `HashTests` case that the new arrays are in `SimWorld.Hash()`. Added to the docs/03 checklist.
2. **`unity test` is the gate, not `validate.py`.** Before every commit:
   ```powershell
   python validate.py
   unity test . --mode EditMode --timeout 600
   unity test . --mode PlayMode --timeout 600 -- -nographics
   ```
   Exit 8 = a test failed (fix it); exit 6 = the run never produced a verdict (compile error, licence).
3. **CI**: `.github/workflows/unity.yml`, generated by `unity ci init`, runs tests and a Linux player build on
   every push. It needs repository secrets — see the comment block at the top of the file. Until they exist the job
   fails at "Activate the Unity license"; that is expected.
4. **Commit `.meta` files with their assets, and `Packages/packages-lock.json` with `manifest.json`.**
5. **Fresh clone setup** is one command: `unity run . -- -nographics -executeMethod TW.Editor.BootstrapSceneBuilder.SetupAll`.

## 7. Unity agent plugin — which skill for which phase

The `unity@unity-agent-plugin` Claude Code plugin is installed on the workstation, and the `unity` CLI
(1.0.0-beta) with it. The Unity MCP server is registered for Claude Code (`unity mcp configure claude-code`), so
an agent can drive the open editor once `com.unity.pipeline` is in the project.

| Skill | Use it for | Phase |
|---|---|---|
| `unity-cli` | Headless setup, `unity test`, `unity run`, driving a live editor (`unity command …`), `unity ci init` | P0.5, every commit |
| `unity-package-management` | Any package add/remove — via the `Client` API script, never by hand-editing `manifest.json` (the way P0.5 did it once) | when needed |
| `ui-uitk` | B6 UI in UI Toolkit (UXML/USS) instead of uGUI; `com.unity.ugui` stays only for the IMGUI debug overlay | B6 |
| `urp-postprocessing` | Volume framework for the grey/mud palette, FXAA, no TAA | B2, B5 |
| `validate-urp-render-graph-renderer-feature` | Review any `ScriptableRendererFeature` (terrain displacement, VAT, gas volumes) against the Render Graph API | B2, B3, B5 |
| `shader-graph-create-custom-node` | Wrap the VAT sampling HLSL as a Shader Graph node so artists can build materials on it | B3 |
| `physics-3d-collision` | B4-lite ragdoll layers/collision matrix (cosmetic only, never read by sim) | B4 |
| `audio-setup-mixers`, `optimize-audio` | B7 mixer routing and import settings | B7 |
| `setup-multiplayer-services` | N2: Lobby + Relay instead of a hand-rolled handshake and NAT traversal | M6 |
| `localization` | Campaign shell strings and dialogue tables | A7 / C3 |
| `build-live-game` | Cloud save / economy for gold and upgrades, if the meta goes online | A7 |
| `generate-editor-search-query` | Asset/scene lookups in the editor during any phase | any |
| **Not applicable** | `initialize-ai-navigation` (NavMesh is banned in docs/05 — flow fields only), `2d-*`, `tilemap-*`, `sprite-*`, `optimize-web`, `implement-in-app-purchases`, `levelplay-unity-integration` (premium Windows title) | — |

## 8. Open follow-ups

- Wire `MapHash` into `ReplayRecorder` at the call sites (`LockstepDriver`, tests) and `DataHash` when C2 bakes
  tables into the sim.
- `SeparationJob` carries a `[ReadOnly] SpatialHash` struct inside an `IJobParallelFor`; the Jobs Debugger may
  object to the nested container attribute. Check on the first PlayMode run with the debugger on.
- Bump `SimConfig.MaxSlots` when vehicles/emplacements land (A3).
- Decide camera max zoom-out (B1) before sizing B3 render tiers.
- Add `com.unity.pipeline` (`unity pipeline install`) so agents can drive the editor; mirror its skill with
  `unity skill install claude-code --local`.
