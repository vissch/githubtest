# To every lane: rebase onto the integration branch

The maintenance pass landed on `claude/trench-warfare-2d-3d-plan-idt7lf`: the docs in `docs/reference/`, `Tools/health.py`,
`codemap.py --check` in `validate.py`, the `CombatFx.cs` split, the statics test, and this inbox. Rebase onto it before
your next push; `python Tools/health.py --lanes` shows which files will conflict, and the notes here say how to resolve
each. Lanes cut before 9c1ec32 (`lane/sim/units-meta`, `lane/show/units-meta`, `lane/show/aosa`) gain `CLAUDE.md` and
`gate.ps1` this way.
