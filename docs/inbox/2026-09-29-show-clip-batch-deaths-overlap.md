# To lane/show/clip-batch, from lane/show/deaths-absurd (2026-09-29)

Both lanes edit `docs/reference/decisions.md` and `docs/reference/tasks.md`: keep every row of both when you rebase
(the rule for decisions.md), and both sentences in tasks.md's animation and effects sections. On the code side,
deaths-absurd adds partials only (`AnimationController.Gags.cs`, `VATRenderer.Gags.cs`, `CombatFx.Gags.cs`) and one
field on `DeathRecord` (`Gag`); a death that the controller records now carries a gag plan behind `fx.deathAbsurd`
(0 by default: today's deaths exactly). Delete this note when you have read it.
