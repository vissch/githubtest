# AOSA: questions for the owner, and proposals for the SIM lane

The loop cannot decide these, and it does not spend cycles on them. Each item has the number that motivates it.
When one is answered, write the answer and the date under it. The next cycle turns the answer into a card.

## Open

**A01. Merge policy for `lane/show/aosa`.** The loop commits each gated change here, one change per commit, with its
A/B table. Do you want to review these commit by commit, or as a batch every N cycles?

**A02. Ranking of the three measures.** Main thread, GPU and a critic score can pull against each other, and the loop
needs to know which one wins. The default is: main-thread p95 first, then GPU p95, then the T1 critic score. The
reason is docs/05, which shows main thread p95 as the budget still exceeded, at 9.0 ms in the development player
against a 3 ms budget.

**A03. Explosions batch B and C.** Batch B is the sky flash, the shock ring and foliage bend, and per-weapon recipes.
Batch C is the scar layer, smouldering craters, haze and wire pieces. Both are designed in
`~/.claude/plans/for-the-trench-warfare-lovely-cerf.md` and wait on your go after the batch A report. The juice
director ranks the shell burst at T1 as the first moment to improve, and card C20 is parked on this question.

**A04. Coast obstacles.** The coast has no wire, no obstacles and no defender's trench on the sand. Placing them is a
map-design call, and card C25 is parked on it.

**A05. Ruins placement.** The ruins set is cut, imported and tested, and nothing places it (agent-memory 2026-09-24).
Where should the ruins go?

**A06. Redoubt's leg count.** Its profile says 6 legs and its model has 4. Damage lames the wrong leg, and two legs
can be shot off with no visible effect (docs/20 A17). The fix is either `Legs = 4`, which is a balance change on the
SIM lane, or a re-split of the model.

**A08. The loop's background editors and your other sessions (cycle 3; editor gates paused until you answer).**
Two ways the loop's batch editors reached your processes:
- The `unity test` editors wrote the shared `%LOCALAPPDATA%/Unity/Editor/Editor.log`, which is your own editor's
  log. This is fixed: `land.ps1` now gives them their own `-logFile`.
- Every Unity editor starts the `com.unity.pipeline` server, and the loop's batch editors on the worktree do too.
  Requests from outside the loop reached them during gates: a `capture_game_view` failed with "No GPU available
  (batchmode/headless)", and `/api/exec` requests timed out while a long test held the main thread. Both
  projects' folders are named `trench-warfare-3d`. Those calls were probably another session's MCP tool calls,
  meant for your editor, landing in the loop's headless one for the few minutes a gate runs.
Turning the server off for the worktree needs an `EditorPipelineManager` asset with AutoStart off under
`Assets/`, which would reach your editor if merged. Nothing the loop may edit alone turns it off. Options:
(a) the loop gates only when you say the editor is free, or on a schedule; (b) add that asset on the loop's
branch only and never merge it; (c) keep gating and accept the occasional misrouted call.
The loop's recommendation is (b). Until you answer, the loop runs only player benches, offline compiles and
patch authors. Waiting on the gate: C48, the missing player effects (runs/3/C48.patch, EditMode 328/328), and
the C13 default of 100 ms.

**A07. Shadows on the men at the standard view (cycle 3, C15).** Raising `vat.vertexBudget` from 1.5 M to 3.4 M
turns the men's shadows on at 1,500 a side. The sweep ran on the release player, interleaved against the current
build, 2 runs per value (runs/3 vb3400000, vb4000000 against base4):
- VAT vertices double: 1.6 M -> 3.2 M. That breaks your rule that standard-view vertices never rise.
- GPU p95 goes from 3.08 to about 3.4 ms (+0.3). The main thread and draw calls do not move.
- At night the look barely changes. 1.4% of pixels differ, mostly slightly darker ground among the men in the
  trench (runs/3/sh34-1.png against runs/2/v1-1.png, both held-clock stills of the same frame).
The loop's recommendation: not at night at T1. It could be worth a look as a daylight-only or T2/T3-only setting,
and the knob already allows either. Do you want the vertices rule relaxed for this, and if so, for which weather
or tier?

## SIM-lane proposals (the loop measured it; the SIM lane decides)

Each proposal is sent to the SIM lane as a seam request and is never edited here.

- **S01 (cycle 1). `TargetAcquisitionSystem` runs 0.80 ms per tick against a 0.40 ms line.** It is the largest sim
  system at 45% of `TW.Sim.Step`'s 1.79 ms (dev player, runs/1/devbase-1). Check that the one-third slot stagger and
  the 8-candidate cap actually hold. (Cycle 4: there is no candidate cap. It keeps the 3 nearest but walks
  everyone within 130 m, so the cost grows with the square of a crowd; see S04.) Sim.Step itself is inside its 3.6 ms ceiling.
- **S02 (cycle 1). `FlowFieldManager` peaks at 3.07 ms on its worst barrage tick, against its 0.60 ms time-sliced
  line.** Craters trigger recomputes (runs/1/devbarrage-1). It is not a hitch carrier on its own.
- **S03 (cycle 3). `LockstepLoopbackTests.Stress_ThreeThousandUnits_AdvanceAcrossCorridor_StayInSync` runs about
  10.5 s in one frame.** On a loaded machine the `unity test` CLI's own `/api/exec` request times out after 5 s. It
  logs an error, and the test fails on that log after its assertions have passed. This happened in 4 of the last 6
  gate runs (cycles 2 and 3), on SHOW-only changes the test cannot execute. Proposal: yield a frame every N ticks,
  or `LogAssert.Expect` that one message. `land.ps1` meanwhile accepts it, with a note in its log, when it is the
  only failure twice in a row.
- **S04 (cycle 4, from the owner's question: cap an army at its trenches' capacity?).** This was measured by running the
  whole sim headless on Unity's Mono. The counts at tick ~1800 match the player's still: P0 alive 1,462 against
  1,471, and the "1415 MEN" label.
  - **The crush.** At 1,500 a side, 1,409 of P0's men stand in its rear trench, which has 79 posts, and 1,330 of
    them have no post. On average each has 37.9 men within 2 m, about 3.8 per m2. A side's trenches hold 154 or 157
    posts in all.
  - **What it costs.** It is 60-70% of `TW.Sim.Step`: TargetAcquisition walks every unit within 130 m with no
    candidate cap, so its cost grows with the square of a crowd. It also costs about 1 M VAT vertices.
  - **Normal matches never reach it.** The economy buys 60-90 men a side in 15 minutes (docs/08), and docs/12 calls
    3,000 alive "a crowded rear-trench stress scene, not a normal match".
  - **Variants, SIM-lane, all behind a SimConfig switch that is off for tests and the ceiling bench:**
    - (a) Refuse a deploy when the trenches are full. Sim -1.6 ms a tick, VAT 0.26 M, and the men's shadows come
      back. The 3,000 ceiling becomes unreachable.
    - (b) Hold the overflow as an off-map reserve counter that walks on as posts free up. Sim -1.1 to -1.3 ms a
      tick, about -1 M vertices, and the shadows come back. It is a new rule, and the HUD needs a reserve counter.
    - (c) A lower stress or default cap, for example 750 a side. Sim -1.0 ms a tick, but the shadows stay off, and
      it overrides the owner's 3,000 decision.
  - **Recommendation:** keep the 3,000 ceiling. If a rule is wanted, (b). The first step does not touch the game:
    the stress preset (ScriptedEnemy, SHOW lane) sends P0's rear garrison forward as well, so the bench measures a
    battle, not a crush. That needs a new baseline.
  - **The ceiling test.** LockstepLoopbackTests asserts 3,000 alive and must opt out of any cap, so S03's frame
    yield is still needed.
  - Runs: scratchpad trenchcap/ (s_base, s_cut1, s_cap154/308/750).

## Answered

(none yet)
