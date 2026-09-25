# AOSA ledger

One row per cycle. `aosa.py ledger add` writes the rows, and the detail of every attempt is in `attempts.jsonl`
under the same cycle number.

| cycle | utc | mode | cards | attempts | result | gate | commit | usd | note |
|---|---|---|---|---|---|---|---|---|---|
| 0 | 2026-09-25T12:22Z | B | C01,C02,C14,C16,C30 | a0001,a0002,a0003,a0004,a0005 | L0 R0 V0 M5 | validate OK, EditMode 309/309, PlayMode 15/15 | abc6f3a | 0.00 | Phase 3 start: Library seeded from the SIM worktree, gate green (EditMode 309/309, PlayMode 15/15), players built from abc6f3a, 3x release baseline (hash_start 252EA3E1DA8F7314 = hash_end-stable), first bands (gpu p95 0.14, main p95 4.2 ms), lodDistance sweep, barrage doubles main p50; player stills not repeatable (C33) |
| 1 | 2026-09-25T13:47Z | B | C14,C21,C22,C31,C33,C34,C35 | a0006,a0007,a0008,a0009,a0010 | L4 R0 V0 M2 | validate OK, EditMode 320/320, PlayMode 15/15 | 5932df5,2c4f3f3,ab92f8e,143f90d | 0.00 | Landed C31 (every draw through FrameBudget), C33 (bit-identical held-clock stills with --shot-tick --no-hud; unblocks C22), C34 (Hollows sum -89 ms, max -15 ms under barrage), C35 (Repaint p50 2.78 -> 2.03 ms). C21 premise stale, C14 measured. C34 needed a second A/B set: one loaded baseline run stretched the spread band (lesson). Real-time setpass.p95 +1 disproved on the held clock (identical per-frame). C22 patch staged (shot stagger, knob fx.shotStagger). New cards C40, C41. All runs in the background at below-normal priority |
