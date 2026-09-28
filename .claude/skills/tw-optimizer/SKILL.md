---
name: tw-optimizer
description: Optimizer for Trench Warfare 3D — look at the merged build, find what costs frame time, reduce it without changing the look, and produce before/after comparison sheets (numbers + images per zoom band), including where models should be swapped for lower-polygon versions. Built on the repo's AOSA loop (aosa.py bench/compare/learn/pick, acceptance rules 1-10, noise bands, seven agent briefs), PerfBench, perfcmp.py, FrameBudget and the LOD derive tools. Use for "optimize the build", "why is the barrage slow", "compare before/after", "which models need fewer polygons". NOT for balance (tw-balance-sim) or new art (the sim roles).
---

# Optimizer (the AOSA loop, wrapped)

**Contract:** `docs/reference/aosa/README.md`. Read its cycle, the acceptance rules and "what the loop never does"
before anything. Deep reference: `references/aosa-and-perf.md` (every aosa.py subcommand, BenchOptions key,
tw-perf/1 field, budget; snapshot at 558c667). Agent briefs: `docs/reference/aosa/agents/*.md`.

## Where it runs
The desktop, in its own worktree, with no visible editor: players run minimised, at below-normal priority, from `-batchmode` builds. Never claim the shared editor slot (`%LOCALAPPDATA%\TrenchWarfare\editor-slot.json`). Never compare release numbers with development numbers.
The RTX 5090 is not the target: frame time here is a proxy. The target is a GTX 1050 at 1080p with 2,000 infantry. Judge by counters (draw calls, SetPass, vertices, GC bytes, per-tick ms). The laptop's low-end bench is the nearest real frame time.

## The measures
| Tool | Command |
|---|---|
| status (the mode it may run: P player / E editor / B rebuild) | `python Tools/aosa/aosa.py status` |
| bench, interleaved A/B, ≥ 3 repeats | `python Tools/aosa/aosa.py bench <label> --player --against <base> --repeats 3 [--scenario barrage\|armour\|vfx] [--knobs k=v]` |
| verdict (rules 1-4) | `python Tools/aosa/aosa.py compare <label> <base> --json` (exit 0 pass, 1 fail, 2 refused) |
| learn noise bands and priors | `python Tools/aosa/aosa.py learn`, then `pick --n 3` |
| player build | `Unity.exe -batchmode -quit -projectPath <wt> -executeMethod TW.Editor.BuildWindows.CommandLine [-twdev] -logFile <f>`. `build-info.json` records `git_sha` and `dirty_files` |
| one-off comparison | `python Tools/perfcmp.py before.json after.json`. It refuses a different `hash_start` or build kind but **not** a different commit: check `git_sha` yourself, and `hash_end` for a presentation-only change |
| image identity | `CaptureRig.Diff` on held T1/T2/T3: "same image" means `changed_frac` < 0.001 |

**Acceptance, in short:**
1. Same battle (`hash_start`, plus `hash_end` for presentation-only changes).
2. At least 3 interleaved repeats past the noise band.
3. The target improves and no other p95 rises past its band.
4. Fidelity floor (`alive_end`, VAT vertices, shadows).
5. The image class is honest: same / indistinguishable (critic) / juice.
6. Readability never drops.
7. The T1 budget never rises unpaid.
8. The gate passes.
9. The prediction is written **before** measuring, or the attempt is void.

The owner's fidelity bar: an "indistinguishable" change needs before/after captures and a critic, **in the editor and in a Windows player**.

## The comparison sheet (the owner asked for this)
For each change or model swap, per band (the gym's six: T3, T2, T1, O120, O240, Far):
- **Before and after images:** the same pose, held clock, `shot_tick`, HUD off. Add the `CaptureRig.Diff` image and `changed_frac`.
- **A numbers table:** main/gpu p50 and p95, draw calls, SetPass, vertices (note that `FrameBudget.Vertices` excludes the indirect-drawn men), GC bytes, per-tick ms, each with its noise band and verdict.
- **What was changed**, in one line, with the commit.

Write it to the board: `evidence/<item>/optimizer/sheet.md` plus JPGs.

## Polygon reduction and model swaps
- **Budgets** (decisions.md): units 1,200-1,500 vertices (max 2,000), far model 250-400 beyond 170 m; vehicles 3,000-5,000. The VAT vertex budget knob is `vat.vertexBudget` (1.5 M).
- **Find candidates:** meshes over budget, LODs that pop (`lodpop` IoU in the Playground), props drawn at far zoom with near detail.
- **Reduce** (details and the tearing trap: `../tw-lowpoly/SKILL.md`): LODs derive from LOD0 (`TW_DERIVE` 12 in `tank3split.py` / `frogrig.py`, owner decision). The static-mesh decimator pattern is in `C:\Users\PC\Documents\unity\EMTD-marketing\Video_1_Project\EnvBuilder\tools\decimate_buildings.py` (static meshes only).
- **Never overwrite the original.** A swap is a proposal with its comparison sheet, applied only after the owner approves.
- **Making the lower models** is `tw-lowpoly`'s job (Blender, inventory, per-model loop). This role measures them and
  makes the sheet.

## Never (from the contract)
- Edit `Sim/`, `Net/`, `Data/`, asmdefs, `ProjectSettings/` or `Packages/`. A sim cost becomes an ASK item for the SIM lane.
- Close a card on an argument, trust `max` over sums/p99, or trust a number over a screenshot that disagrees.
- Set `EditorApplication.update = null`.

## Learning loop
Brief 2 §B5, the same for every role: see `../pipeline/SKILL.md`, "The learning loop". This role's lessons file is in
that section's table.
