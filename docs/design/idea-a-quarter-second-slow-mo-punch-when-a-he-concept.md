# A quarter-second slow-mo punch when a heap fountains — concepts

The idea, accepted by the owner on 2026-10-08: *"When a stacked heap of bodies erupts, time pinches to a brief
slow-mo punch under a quarter second, then snaps back to full speed. A hit of impact, not a pause."*

What is being picked is **when** the clock pinches, and **for whom** — not how anything looks. All five pages are
the same heap, drawn from the game's own throw table, at the standard view (fov 25, pitch 25, zoom 30: 25.98 px
per metre across the view and up, 10.98 px into it), with a standing rifleman 2.50 m = 65 px beside it for scale.
Nothing is rendered: every page is drawn with PIL by the generator beside them, because this station has no
window.

## The pages

Board folder `evidence/idea-a-quarter-second-slow-mo-punch-when-a-he/concept/` (round 2, 2026-10-10):
`shown.jpg` (the five pages as one sheet, 1:1), `sheet.png`, the five page PNGs, `greyscale-check.jpg`,
`gen.py` (draws them), `measure.py` + `probes.json` + `luminance.txt` (probes them back out of the sheet's own
pixels), `frames.txt`, `notes.md`.

**The letters moved this round.** He said on 2026-10-09 "do it means the recommanded. which is usually A", so the
recommendation — which is also the option he already picked — is page A now. Round 1's letters: A was the
hit-stop, B the hang at the top, C this pick, D only the men.

## How a still shows slow-mo

Every strip is sampled at equal **wall-clock** steps of 1/60 s, never at equal world time. Where the clock is
pinched, the same metres of flight carry more samples, so the figures bunch up; where it runs free they are
evenly spread. Each page prints a ruler of real seconds against shown seconds with the pinch shaded, and two
numbers: the world seconds slowed, and the **added wall-clock** — the real time the pinch costs the watcher.

## The four

| | What it is | Added wall-clock | What it costs to build | What it says |
|---|---|---|---|---|
| **A, pinch on the lift-off** (his pick, put first) | 0.25x for 0.20 s of real time from the bang + 0.28 s, then a 0.08 s ramp back | 0.180 s | Least: one window on the fountain event, one curve, one ramp | The heap coming apart is the moment, and it is still the bang's own beat |
| **B, hit-stop on the bang** | A dead stop, 0.12 s from the bang + 0.04 s, no ramp | 0.120 s | Least, and the simplest curve of all: one hold, no ramp | The hardest punch — of a frame in which nobody is flying yet |
| **C, hang at the top** | 0.30x for 0.22 s from the bang + 1.09 s, then a 0.06 s ramp back | 0.175 s | More: the window has to wait for the men to near their apex, so it needs the throw's own timing, not the bang's | The clearest read of five bodies in the air — a tableau, not a hit |
| **D, only the men slow** | The battle's clock untouched; each man plays his own first 0.24 s of flight at 0.40x | 0.000 s to the battle (0.162 s to each man's own flight) | Most: every thrown body needs its own clock, and nothing shared can drive it | Nothing stops, so there is no beat for the eye — but it is the only one a lockstep peer could run |

Page E is the control: what ships today, no pinch, same throw table and the same 1/60 s sampling, so the only
difference on it is the even spacing. The pick is made against it.

**Why A is first.** It is the only one that slows the heap *while it is coming apart* and is still close enough
to the bang to read as the hit — and it is the one he picked on 2026-10-09. B freezes a frame with nobody in the
air. C lands a second after the bang. D has no beat because the battle carries on around the men. A's 0.180 s of
added wall-clock is inside the idea's own bar ("under a quarter second") and well inside the owner's wider one
("less then half a second is ok").

## Three things the build stage needs to know

1. **A pinch needs Unity's `Time.timeScale`, not `MatchClock`.** `Presentation/Core/SimHost.cs` advances the sim
   by `Time.deltaTime * TimeScale`, but thrown bodies, dirt and bursts time on `Time.time`, so a dip in
   `MatchClock` alone would slow the sim and *not* the flying men. `Presentation/Core/MatchClock.cs` owns
   `SimHost.TimeScale`, clamps `SetSpeed` at 0.25, and has no short dip.
2. **`Storm.cs` writes `Time.timeScale` too.** `Presentation/Terrain/Storm.cs` holds it at 0 for `FreezeSeconds`
   on a lightning strike and restores what it saved; a pinch writing the same field can undo that freeze, or be
   undone by it. Whoever builds the pinch has to decide who wins, in one place.
3. **It is single player only.** Storm's freeze says it in as many words — "a lockstep peer cannot stop the sim" —
   and a pinch carries exactly the same limit, because it slows the sim with the rest of the world. Option D is
   the only one of the four that does not, which is D's one argument.

Nothing is built until the build stage runs: no clock change, no event hook, no sim change. The pick is already
made (2026-10-09, page A).
