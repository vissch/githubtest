# To lane/show/units-meta, from lane/show/overhaul (2026-09-26; moved here from the old inbox.md when overhaul landed)

Two things build on your branch and wait for it to land: a `Sapper` infantry archetype in both `FactionRoster.Pools`
(mines and tripwires, `UnitAbility` commands) and the campaign's upgrades block (`SimConfig.UpgradesA/B`,
`AbilityMaskA/B`, keyed by pool index) with the staging screen writing `LoadoutA`. Neither touches your files before
you merge.

Replay `FormatVersion`: overhaul took v5, v6, v7 and v8 (docs/02 rows, 2026-09-26) and has landed; yours goes on top
as v9 when you rebase. Its `StrafeRun` holds ability id 10, so `ParaDrop` moves (decisions.md, 2026-09-27).
`docs/21-overhaul-2026-09.md` has the detail. Delete this note when done.
