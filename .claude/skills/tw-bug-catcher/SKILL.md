---
name: tw-bug-catcher
description: Bug catcher for Trench Warfare 3D — patrol every simulator's results and the merged build for glitches, errors, desyncs and broken behaviour, separate real failures from the known false reds, and file one bug card per signature with an exact repro. Knows the gate's verdict rules, the determinism/hash/replay tests, the canary, SimProbe, DeterminismPlatformReport, otr.py limits and the bug-reproduction recipe. Use for "patrol for bugs", "is this test failure real", "the game desynced", "check the build for broken stuff". It never fixes: it routes. NOT for performance (tw-optimizer) or looks (tw-critic).
---

# Bug catcher

Deep reference: `references/determinism-and-verdicts.md` (tests, canary, probes, gate verdicts, false reds; snapshot
at 558c667). Repo pages: workflow.md sections 5 (false reds) and "reproducing a bug"; docs/03-determinism-rules.md.

## When it runs
1. After every stage result: sweep the stage's logs, results XML and probe numbers.
2. After every landing: the full gate on integration, plus a determinism pass.
3. **Patrol** (desktop, detached, no model in the loop): round-robin over every item's bench scenarios with fresh seeds, derived from the patrol id and a counter, within a time budget. It yields the batch slot between scenarios. The agent reads the findings afterwards.

## Is it real? (check in this order)
1. **The verdict source.** The results XML decides, never Unity's exit code. A run with no tests, or nothing passed, is **no verdict** (gate exit 6), not a pass. `gate.ps1` deletes old XML first, so a stale green copy cannot answer.
2. **Known false reds:**
   - statics surviving a Play session (run `RequestScriptReload`, then rerun);
   - `.unity-pipeline-port` sharing violations or `Failed to handle /api/exec request` (the gate reruns once);
   - the first run after a Burst job edit running managed code;
   - a stale `Library/BurstCache` (NullReference / IndexOutOfRange inside jobs: delete it with the editor closed);
   - `No graphic device is available` under `-nographics`;
   - the 3,000-unit loopback stress test's 10.5 s timeout.

   Rerun once. Only a second red is a finding.
3. **Which runner.** `otr.py` (Mono, stand-in engine) is a first filter: a red there is not a bug until the real gate agrees, and its hashes never compare with Burst ones.
4. **An audit that reads all green:** feed it an input that must fail, and prove it still can.

## Determinism
- **Tests:**
  - `SimHashTests` pins the hash chain for `FormatVersion` 9; a system joining or moving fails it.
  - `DeterminismReplayTests`, `HashIntervalTests`, `SinglePlayerEquivalenceTests` (one world vs canary vs canary under latency, jitter and loss).
  - `BattlefieldLockstepTests` (`FirstDifference` names the first differing array or system).
- **Live:** `-twCanary` / `SimHost.DeterminismCanary` runs a peer world. `LockstepSession.Desync` / `DesyncTick` and a `DESYNC at tick` log line mark a split. Pause at that tick and diff `SimProbe.Arrays()` with `SimProbe.Arrays(true)` (only 8 arrays; the test's `FirstDifference` is broader).
- **Cross-machine:** `-executeMethod TW.Editor.DeterminismPlatformReport.Write` writes `DeterminismReport-<platform>-<arch>.txt` into the project folder (copy it out before a second run). Desktop and laptop matched byte for byte at 2ad4973, but that covers deploys only.

## Sim fault or drawing fault?
`Tools/tw eval 'return TW.Editor.SimProbe.Unit(<slot>);'` compares sim state with drawn state. If the sim has the man in a trench and he is drawn on the parapet, it's SHOW. If the sim itself has him on the surface, it's SIM.
A real red in the other lane's code is theirs: file a card and an inbox note, and never patch their files.

## The bug card (board `findings/BUG-<station>-<date>-<slug>.md`, one per signature, repeats add to a count)
1. Severity, and **provenance**: gate XML / an in-editor run after reload / otr / read in code (not confirmed in Play).
2. Symptom, expected vs actual, and the assertion message verbatim.
3. Lane and owner file (from `SimProbe.Unit`, the tasks.md row, and the guarding test).
4. **Repro:**
   - the `SimProbe.Match()` line (seed, battlefield seed, ground, bombardment, stress, tick, mission);
   - the command script with ticks and a ±N tick sweep (order timing depends on frame rate);
   - canary and latency settings; the commit; the station.
5. For a desync: `DesyncTick`, `FirstDifference`, the `Arrays` diff, and the FormatVersion or chain impact.
6. A discriminating test that fails on the old code, where one can be written.
7. Routing: the owning stage's feedback request, or an inbox note `docs/inbox/<date>-<lane>-<topic>.md`.

**Never fix.** The card goes to the master, who turns it into a feedback request for the stage that owns the file.
