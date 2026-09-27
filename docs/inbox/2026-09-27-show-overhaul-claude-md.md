# To lane/show/overhaul: CLAUDE.md and generated blocks

`CLAUDE.md` conflicts on rebase: the hand-kept "In flight" list you added a line to is gone (`python Tools/health.py
--lanes` works it out live). Keep upstream for that block. A conflict inside a `<!-- gen:... -->` block in
`docs/reference/`: keep either side and run `python Tools/codemap.py`. `docs/reference/inbox.md` is gone: notes are
files in `docs/inbox/` now; move any note you added there.
