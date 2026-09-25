# AOSA lessons

Read this first every cycle. Each line is something a measurement disproved, or a reason a change was reverted that
no rule predicted. It names the attempt it came from (`sNNN` for seeds from before the loop). Newest go at the top
of their section. When the same lesson appears twice, the retrospective turns it into a rule in README.md.

## About instruments

- **An instrument answers only the question it was pointed at.** `gc_bytes_per_frame` answered "how much" and was
  read as if it said "where". Before trusting a number, say what the instrument cannot see. (docs/05, s003)
- **`GC.GetAllocatedBytesForCurrentThread` reads 0 under Unity's Boehm GC, whatever the code allocates.** Use
  `TW.Perf.AllocProbe`. (docs/05)
- **The first budget counter could not see six of the nine things that draw, including every close-tier effect.** A
  counter that misses a submitter rewards the loop for adding cost there. Grep for every draw API
  (`RenderMeshInstanced`, `RenderMesh`, `RenderMeshIndirect`, `DrawMesh*`) before trusting `FrameBudget`.
  (env-scoreboard rounds 1-2)
- **Only the player build could see that six shaders were missing.** The editor finds every shader. Look at the
  bench's `shot=` still before trusting any player number. (s009)
- **Two runs of the same build an hour apart differed by 1.3 ms of GPU.** The laptop drifts. Interleave A and B and
  never compare runs taken far apart. (docs/05; this is README rule 2)
- **Render-to-texture captures bypass post-processing.** A grade change is invisible in them. Judge post effects from
  a real Game view capture. (visual-score round 14)
- **Pausing the editor stops `Update`.** Per-frame `RenderMeshInstanced` submissions vanish from both frames of a
  paused A/B diff, and so do queued `CaptureRig` shots. Hold time with `CaptureRig.Hold`, never with the editor's
  pause. (agent-memory)
- **A value read from an editor eval is not evidence about the shipped path.** The editor re-fetches vertex data that
  a player does not, so a test passed while the game was wrong. When the numbers and a screenshot disagree, the
  screenshot is right. (agent-memory, twice in one day)
- **A single-instance `RenderMeshInstanced` ignores instanced properties.** Test with 4 or more instances before
  concluding a per-instance property is broken. (agent-memory)

## About searching and attributing

- **Grep the DECLARATION, not a guessed usage.** `Stance[` missed `StanceOf`, and a class may live in a file with
  another name. Search for `class <Name>`. (agent-memory)
- **Change the input instead of searching for the cause.** Neutralise the suspect (for example, set a size factor to
  1 so the values equal HEAD). Byte-identical failure output proves the path never reads it. (agent-memory)
- **Port the arithmetic, leave the geometry in Unity.** The Blender harness got axes wrong twice and origins wrong
  twice, and overstated a slide 4 to 15 times. Only the decision-only port survived its own checks. (s012)

## About editor contention

- **`occ.py` on one assembly compiles against the main clone's OLD dlls for everything else.** A new symbol in a
  dependency then reads as "does not exist". Use `occ.py --changed`, or name every changed assembly in dependency
  order. (cycle 0 build)
- **The editor was unavailable for 46 of the rig loop's 51 cycles.** A loop that needs the editor for every step
  stalls. Mode P (player plus offline compile) must always have work. (docs/20 closing)
- **A `;` or a pipe swallows a failed claim.** Use `editor_lock.py claim ... || exit 1` inside the landing script
  itself. (agent-memory)
- **Statics survive leaving Play, so an EditMode red in a live editor may be false.** Call `RequestScriptReload`
  first, and re-run any red in a fresh domain before reporting it. (agent-memory)
- **`EditorApplication.update = null` kills every session's eval bridge.** Remove only your own delegate.
  (agent-memory)

## About changes

- **A lower number in one place can be a trade.** The toe fix improved every slide, and Pincer's belly clearance went
  from +0.33 to -0.15 m. Log a trade as a trade. (s011)
- **A constant tuned against one machine sinks another.** LEAN 0.30 was fine for four walkers and sank Banner
  0.46 m. Test every archetype, not the one you were looking at. (s010)
- **Power of two, or it is not worth doing.** A 3072 sheet came back as 25 MB of uncompressed RGB24 and looked
  identical on screen. Measure memory after every texture change, because the picture will not show a failed
  compression. (s002)
