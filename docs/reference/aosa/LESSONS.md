# AOSA lessons

Read this first every cycle. Each line is something a measurement disproved, or a reason a change was reverted that
no rule predicted. It names the attempt it came from (`sNNN` for seeds from before the loop). Newest go at the top
of their section. When the same lesson appears twice, the retrospective turns it into a rule in README.md.

## About instruments

- **A blind count off held frames was a quarter low, although it called its totals reliable.** C72's shot log counted 183 births on frames 1-31 where the C71 critic counted 136. LTU was 38% low, mostly in bundles (10 births on frame 2, counted as 4). Tracers from shooters just below the screen were split between the wrong lines (LTL and BOT). Its timing on unoccluded lines did hold (r 0.95). Count events from a log, and use a blind critic only for how they look. (C72, a0041; runs/9/c72-check.md)
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

- **One loaded run can veto a clear win under the spread band.** C34 won every interleaved pair: Hollows max
  11-18 ms against 22-52 ms, with no overlap. One baseline run on a busy machine (d2-4: main p50 15.0, Hollows max
  52) stretched `max(3*MAD, spread)` to 30 ms, and rule 3 failed. Adding runs cannot fix it, because the spread keeps
  the outlier. Two things follow. Predict a card on a sum or a p99, not on `max`, which is one frame's extreme.
  And if this happens a second time, the retrospective should propose voiding a pair whose main_ms p50 is off
  its label's median by more than the band (a load detector), just as rule 1 voids a run read at the wrong tick.
  (C34, cycle 1; runs 1/k1d-1..4, d2-1..4)
- **The held clock repeats the image exactly, and the draw counts only to about 0.25%.** Two held-clock runs of
  one build and one knob set gave bit-identical stills, but their draw sums differed by 955 of 395k (runs 2/v1-1,
  v1-2). Held-clock p50/p95/max of setpass and draws do line up. So judge count cards there, and give their sums
  a 0.3% band. (cycle 2)
- **A slow frame can read hash_end past its tick.** d2-3 read it at tick 2206, not 2200, and `compare` voided the
  set until a rerun. Card C41. (cycle 1)
- **On this machine the release player's main-thread p95 is noise-bound.** The band from 3 runs is 4.2 ms, while GPU
  p95 is 0.14 ms. Other sessions' editors share the CPU. Judge main-thread cards on the development build's
  per-marker `per_tick_ms` and on p50, never on release p95 alone. Judge GPU cards on release p95. (cycle 0,
  runs 0/base-1..3)
- **The release player cannot see GC.** `GC Allocated In Frame` is unavailable in a release player, so `gc_bytes`
  is absent. Measure allocation cards on the development build. (cycle 0)
- **The seeded reports are not repeats of each other.** `learn` excludes `runs/seed/` from the noise bands. It
  once grouped two unrelated player runs as one. (cycle 0)

- **The held clock made the battlefield repeat, and the HUD did not.** After C33's held clock, three runs shot the
  same held frame at the same `time`, and every differing pixel (3%) was HUD. The HUD animates on real time, and it
  follows the owner's pointer over the background player window: one run showed the MG card hovered. A pixel diff
  uses `--shot-tick N --no-hud`. The critic may still get the HUD still. (C33, cycle 1; the cycle 0 stills differed
  on 7.5-10% before the clock was held)

## About searching and attributing

- **Grep the DECLARATION, not a guessed usage.** `Stance[` missed `StanceOf`, and a class may live in a file with
  another name. Search for `class <Name>`. (agent-memory)
- **Change the input instead of searching for the cause.** Neutralise the suspect (for example, set a size factor to
  1 so the values equal HEAD). Byte-identical failure output proves the path never reads it. (agent-memory)
- **Port the arithmetic, leave the geometry in Unity.** The Blender harness got axes wrong twice and origins wrong
  twice, and overstated a slide 4 to 15 times. Only the decision-only port survived its own checks. (s012)

## About editor contention

- **A batch editor is still an editor to everything else on the machine.** Without `-logFile`, `unity test`
  editors write the shared default `Editor.log`. Every editor also starts the pipeline server that MCP clients
  talk to, and a `capture_game_view` meant for someone else failed inside the loop's headless test editor. "Run in
  the background" has to include those shared channels, not only windows and priority. (cycle 3, A08)

- **A player build rewrites URP's shader-prefilter fields.** It touches `TW-URP.asset`,
  `UniversalRenderPipelineGlobalSettings.asset` and `ProjectSettings/GraphicsSettings.asset`. That is build churn,
  not a change. Revert all three with `git checkout --` after every build and never commit them. (cycle 0 build)
- **Seed a new tree's `Library/` from an idle clone of the same Unity version.** Robocopy took 34 s for 1.8 GB, and
  the gate then ran in 4 minutes. Never copy from a Library whose editor is open. (cycle 0 build)
- **Two `occ.py` runs at once corrupted each other.** Every tree wrote its dlls to one `%TEMP%/tw-occ`. With two
  patch authors compiling in parallel, one run failed on "being used by another process" and then reported
  phantom missing members, read from the other tree's dlls. `occ.py` now writes to `tw-occ-<hash of the tree>`.
  Parallel authors need per-tree scratch for everything they write. (cycle 5)
- **Git Bash rewrites a Windows tool's `/FLAG` into a path.** `robocopy A B /MIR` from bash became `C:/Program Files/Git/MIR`, and robocopy returned 16. The build snapshot was silently missing for cycle 5. Set `MSYS_NO_PATHCONV=1` for any Windows tool called from bash, and check its return code (robocopy: below 8 is ok). (cycle 5)
- **A shell one-liner that quotes through three shells can do the opposite of what it says.** The cycle 8 snapshot line (bash -> PowerShell -> robocopy) lost `$b` and escaped its own quote, so it ran `robocopy Builds\ Builds\@f5ab2d2 /MIR`: a mirror of the folder into itself. It made a hidden 1.3 GB copy (robocopy marks such a copy hidden and system), then sat on robocopy's default million retries for an hour, and the landing behind it never started. Snapshots are now `aosa.py snapshot` (Python, checks what it copied); `status` names any folder in `Builds/` that is neither a player nor a `kind@label` snapshot. Never call robocopy without `/R:1 /W:1`, and give any background chain a step log that is written *before* each step, so a hang shows where it is. (cycle 8)
- **`occ.py` on one assembly compiles against the main clone's OLD dlls for everything else.** A new symbol in a
  dependency then reads as "does not exist". Use `occ.py --changed`, or name every changed assembly in dependency
  order. (cycle 0 build)
- **The editor was unavailable for 46 of the rig loop's 51 cycles.** A loop that needs the editor for every step
  stalls. Mode P (player plus offline compile) must always have work. (docs/20 closing)
- **On a loaded machine the PlayMode stress test can fail on the CLI's own request, not on the code.**
  `LockstepLoopbackTests.Stress_ThreeThousandUnits...` holds the main thread for more than 5 s. The `unity test` CLI's
  `/api/exec` request times out and logs an error, and the test fails on that unexpected log. The gate that day took
  6.5 min for EditMode, not 1 min. Rerun once. Only a second red is a finding. (C22, cycle 2)
  It came back in cycle 3 (C40+C13), this time alongside `MatchClockTests` logging "No graphic device is available to
  initialize the view" under `-nographics`. Two occurrences made it a rule (README self-learning 7): `land.ps1`
  reruns PlayMode once, but only when every failure matches one of those two environment signatures, and it says
  so in its log. Any other failure stops the land as before.
- **A `;` or a pipe swallows a failed claim.** Use `editor_lock.py claim ... || exit 1` inside the landing script
  itself. (agent-memory)
- **Statics survive leaving Play, so an EditMode red in a live editor may be false.** Call `RequestScriptReload`
  first, and re-run any red in a fresh domain before reporting it. (agent-memory)
- **`EditorApplication.update = null` kills every session's eval bridge.** Remove only your own delegate.
  (agent-memory)

## About running the loop

- **A subagent's background child reports to the orchestrator, not to the subagent.** In cycle 9 three agents
  launched their blind critic with the Agent tool in the background and then waited for a notification that went to
  the orchestrator instead, so each stalled until the orchestrator relayed the result by hand. Brief every agent
  that runs a critic (or any child it must read) to launch it with `run_in_background: false`. (a0039, a0040,
  a0046-a0048: runs 9/c101-critic.md, c99-critic.md, batch9-critic.md hold the relayed scores; cycle 9)

- **The loop followed the most interesting card and stopped grooming.** For cycles 4-7 each cycle chased the newest
  critic finding (smoke, overlays, shadows). By cycle 8, `pick` listed 20 cards past the split threshold (idle 4-8)
  that no one had split, although README says idle 3 means split. The rule exists so that work which stands still
  gets decided. Start every cycle with `pick`, and split whatever it marks SPLIT before starting new work. (cycle 8)

## About changes

- **Parts that pass alone can fail together.** The C99 pair (fx.smokeSoft 0.6 + fx.burstGlow 0.5) passed blind
  alone (a0046: weight +0.17, men +0.17, 0 down), and C104's order with the old shape passed alone (a0044: weight
  -0.33, men 0, tracers +2.17). The default set of both lost barrage weight -1.00, lower in 8 of 8 pairs (a0050,
  runs 9/d9k-critic.md), and nothing landed. Rule 6's blind check is on the exact set that will ship, against the
  current default, never on its parts. (a0044, a0046, a0050, cycle 9)

- **A critic and a measurement can disagree; for a shared-look change, measure first.** C105's moon lift and
  outline thinning raised the blind men score on the left trench 3.00 -> 4.67 (a0042), but the measured men-vs-mud
  separation d' halved, 0.290 -> 0.147: the critic saw men separate from each other and missed them sinking into
  the mud. The split put it on the outline thinning (a0045), and the dark-jitter variant held the mud contrast but
  merged the men, 4.33 -> 3.33 in 7 of 7 pairs (a0049). For a shader change to a look every man shares, measure
  figure/ground (man vs ground and man vs man) in the real bench frames before the critic, and treat a fall in
  either as a rule 6 failure. (a0042, a0045, a0049, cycle 9)

- **A sweep's winner is not a pass.** The cycle 8 column sweep asked a blind critic which of four sizes read best as a
  column, and 0.7 won. Nobody asked whether 0.7 read worse than the 0.38 already there. After landing, a blind pair
  test against the old default scored the column +2.6 as earth and the men near the burst 6.0 -> 5.3 (lower in 5 of 7
  pairs, never higher): the bigger column covers the trenches. Reverted. Rule 6 now says readability is judged blind
  against the current default before the commit. (a0032, cycle 9)

- **Third time: the J01 barrage stills were shot after the last shell.** The barrage is called at the window's open,
  warms up 80 ticks and drops its shells over 120, so every burst falls in ticks ~82-200. The evidence tick was 300.
  Two blind critics then reported "no flash, no earth column", and card C54 was written for a missing effect that
  draws fine at tick 140. With "no still showed a shot" and C49, this lesson has now come up three times. The
  retrospective must make it a rule: an image card names the tick range its event occupies, taken from the code
  (warm-up, spread, lifetime), and its evidence is shot inside that range. (C54, a0022; runs 6/b54a, b54b, b54c)
- **The bench view could not show C49's effect: the evidence was blind again.** The fake crowd shadow darkens open
  ground under men. In the stress preset every man stands packed in the rear trench, where the men cover the ground
  themselves, and the zoom-16 view frames an empty no-man's land. Knob on against off changed 0.002% and 0% of
  pixels, which read at first like a broken map. One extreme-knob run (strength 1, radius 1.5, stretch 3) changed
  4.6%, in the upper right: the map worked, and the view had nothing for it to act on. A sparse battle (stress=150)
  then showed why the design missed. The men stand on duckboards, which are painted kit and not the ground material
  the shade was gated to, so the fake could not have shown in any battle. This is the second time, after "no bench
  still ever showed a shot", so it is a rule candidate for the retrospective. Before judging an image card, show
  the effect at an extreme knob value in the evidence view. If it does not show there, change the view, not the
  verdict. And before writing a card for "under the men", check what they actually stand on. (C49, a0017; runs
  4/n49z, s49z, x49z, q0c, q1c)
- **A per-system cost read from code as "grows with the square of the crowd" was flat per man.** S04's diagnosis
  put 60-70% of Sim.Step on the crush, because TargetAcquisition walks everyone within 130 m. Spreading the army
  cut Sim.Step 39%, but per living man it did not move (0.82 against 0.87 µs a tick per 1,000). The 130 m search
  sees the same men at 2 m or at 10 m spacing, and the spread battle had fewer men alive. Normalise a sim cost by
  the men alive before crediting a scenario change with it. (S04d, a0018; runs 4/sp1, dv0)

- **A cause inferred from code, with no render, was wrong. The patch's own knob disproved it in one player run.**
  The C42 author reasoned that the muzzle flare was hidden by depth and the soft edge, and built a fix that pulls
  it toward the eye. On held-clock stills, pull 1.2 against 0 was bit-identical, and so were scale 6, a 4096-card
  pool and a 200 m reach: the flare is not drawn at all. The author had named the discriminating test. The lander
  runs that test before any critic or perf A/B, and a patch whose cause is inferred must ship with one.
  (C42, cycle 3)

- **Mono runs float arithmetic at double precision unless the code narrows it.** C35's `Unorm8` had SetPixel's
  formula, `(int)(v*255 + .5)`, and still differed on 515 edge texels: 0.503921568 x 255 is 128.5 in float and
  128.4999... in double. An explicit `(float)` cast on each step fixed it. The oracle test, which runs the old
  SetPixel loop beside the new code, caught this. The author's fallback guess, Mathf.Round, would also have been
  wrong, because SetPixel takes a half up and Mathf.Round takes it to even. A byte-exact port needs an oracle
  test against the engine, not a formula. (C35, cycle 1)

- **A marker name is not a diagnosis.** C34 blamed `TW.Terrain.Hollows` on its overlap loop, but a Mono port of the
  code showed the loop cost 0.3 ms. 70% of the time was drainage, which ran under the same marker. Before a
  patch, time the parts inside the marker. The port on Unity's own `mono-bdwgc.exe` ran without the editor. (C34,
  cycle 1)
- **A card seeded from memory can be stale.** C21 said the cook-off flames draw for one frame, but commit 1ede261
  had already fixed that. Re-check a seeded card's premise in code before a patch author starts on it. (a0006)
- **No bench still ever showed a shot.** `shot=` was taken while the battle was paused, before the window opened.
  An instrument can be blind to exactly the thing a card is about. Check that the evidence CAN contain the effect
  before scoring it. (juice director, cycle 1)
- **A lower number in one place can be a trade.** The toe fix improved every slide, and Pincer's belly clearance went
  from +0.33 to -0.15 m. Log a trade as a trade. (s011)
- **A constant tuned against one machine sinks another.** LEAN 0.30 was fine for four walkers and sank Banner
  0.46 m. Test every archetype, not the one you were looking at. (s010)
- **Power of two, or it is not worth doing.** A 3072 sheet came back as 25 MB of uncompressed RGB24 and looked
  identical on screen. Measure memory after every texture change, because the picture will not show a failed
  compression. (s002)
