# Canary desync check can't tell "no hash" from "same hash"

Found while looking at the sim/canary seam (look-code-sim-core-20261018).

`LockstepSession.Compare` reads `theirs[k] == w.LastHash` as agreement. `SimWorld.Step` sets `LastHash = 0`
on every tick `HashInterval` skips (`SimWorld.HashesTick`, next to `HashInterval`). Two worlds that have
diverged still both report `LastHash == 0` on a skipped tick, so `Compare` would call that agreement. The
canary is safe today only by accident: its constructor branch never touches `HashInterval`, and
`LockstepDriver` only guards the interval when a recorder is attached. Raising `HashInterval` for the canary
(to make it cheaper) would silently break this.

## The fix (one line, in `Compare`)

After `mine[k] = w.LastHash; mineAt[k] = tick;` add:
```csharp
if (!w.HashesTick(tick)) { mineAt[k] = uint.MaxValue; return; }
```
This both skips the comparison on a tick with no real hash, and stops that slot being read as a hash later.
`SimWorld.HashesTick(uint tick)` is the SIM-side predicate added for this (`SimWorld.cs`, next to
`HashInterval`): true exactly on the ticks that compute into `LastHash`.

Also set the canary constructor branch's `Local.World.HashInterval = Peer.World.HashInterval = 1`, so the
canary never runs sparse in the first place — belt and suspenders with the `Compare` fix above.

## The test this needs (SHOW's, not done here)

In `Assets/_Project/Tests/Match/SinglePlayerEquivalenceTests.cs`'s module (`TW.Tests.Match` already
references `TW.Presentation.Core`): build a canary pair with `HashInterval = 10`, write one world's `Hp[0]`,
step both, and expect `Desync`. It's red today only if the constructor keeps the interval the test sets
(i.e. before you add the `HashInterval = 1` line above — write the test first, confirm red, then fix).

SIM side (`SimWorld.HashesTick`, and `HashIntervalTests.AWorldThatSkipsAHashReportsNoHashNotAnAgreeingHash`
proving the trap) is done on `lane/sim/found-make-the-canary-desync-check-distinguish-no-20261018-0`.

Delete this note when done.
