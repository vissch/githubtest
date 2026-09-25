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

## SIM-lane proposals (the loop measured it; the SIM lane decides)

Each proposal is sent to the SIM lane as a seam request and is never edited here.

- **S01 (cycle 1). `TargetAcquisitionSystem` runs 0.80 ms per tick against a 0.40 ms line.** It is the largest sim
  system at 45% of `TW.Sim.Step`'s 1.79 ms (dev player, runs/1/devbase-1). Check that the one-third slot stagger and
  the 8-candidate cap actually hold. Sim.Step itself is inside its 3.6 ms ceiling.
- **S02 (cycle 1). `FlowFieldManager` peaks at 3.07 ms on its worst barrage tick, against its 0.60 ms time-sliced
  line.** Craters trigger recomputes (runs/1/devbarrage-1). It is not a hitch carrier on its own.

## Answered

(none yet)
