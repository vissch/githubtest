---
paths:
  - "trench-warfare-3d/Assets/_Project/Sim/**"
---

# Sim: traps (checked 2026-10-14)

- **Every job carries `[BurstCompile(CompileSynchronously = true, ...)]`** (`docs/03-determinism-rules.md`). Without
  it the first run uses managed code and the determinism tests fail. All 25 job files have it; keep it on a new one.
- **After a big change to a job struct, close the editor and delete `Library/BurstCache`.** A stale cache gave
  NullReference and IndexOutOfRange inside Burst jobs that the source did not explain.
- **Search for the declaration, not a guessed use.** The stance array is `StanceOf`, written in
  `MovementSystem.cs`; a grep for `Stance[` found nothing and "never written" was reported to the owner. A class
  can live in a file of another name: search `class <Name>`, never `<Name>.cs`.
- **A new field on `SimWorld` goes at the end of `Hash()` with a format bump** (the seam in `CLAUDE.md`). After
  every rebase rerun the determinism, replay and hash tests: two appended lines merge cleanly and hash differently.
- A red from the offline runner (`Tools/otr.py`) is not a sim bug until the real gate agrees: its engine is a
  stand-in.
