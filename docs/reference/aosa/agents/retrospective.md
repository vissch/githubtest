# Retrospective brief (every 10 cycles)

You judge the loop, not the game. Read only what the loop wrote:
- `python trench-warfare-3d/Tools/aosa/aosa.py retro`
- `LEDGER.md`
- `attempts.jsonl`
- `priors.json`
- `knobs.json`
- `BACKLOG.md`
- `JUICE.md`
- `LESSONS.md`
- `README.md`

**Answer these questions, each with the attempt ids that support the answer.**
1. **What paid?** Give hit rate and median gain per class, and the cost per landed change. Which class should get
   more of the WIP, and which less?
2. **What stalled?** List the cards with idle of 3 or more, and the cycles where nothing landed and why: no slot, a
   premise was false, a rule reverted it, or the result was void. If "no slot" is more than half of all cycles, say
   so plainly. The owner's answer might be a nightly window or the loop's own clone.
3. **What does the loop keep getting wrong?** Name any revert reason that appears twice or more, and any class whose
   calibration falls outside [0.5, 2]. Propose one rule or instrument for each.
4. **Is the image budget spent well?** Compare the juice share's hit rate (a critic delta after a reference) with
   the tier share's. Propose a new split if they differ by more than 2x.
5. **Does the critic agree with the owner?** Compare the last `owner_verdict` entries with the critic's calls on
   the same commits.
6. **Cadence and WIP.** Should the 20-minute cadence or the WIP cap of 3 change? Answer from how long cycles
   actually took.

**Output:** a patch to `README.md` (rules), `BACKLOG.md` (ordering, splits, parks) and `budget.json` (the split),
each change citing its evidence, plus a 10-line summary for the owner. The orchestrator commits it as "AOSA
retrospective, cycles N-M".
