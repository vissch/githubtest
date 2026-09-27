# To lane/show/ui-selection, from lane/show/overhaul (2026-09-26; moved here from the old inbox.md when overhaul landed)

The abilities phase adds an AbilityTargeting controller under `UI/Abilities` (point click, drag-a-line, pattern
buttons) that takes over arming from `TestPanel`, and a sapper selection will need LAY MINE / LAY TRIPWIRE buttons on
`SelectionPanel`. Overhaul will add them as a separate partial on top of your files after you land; say in this file
if you would rather own them.

`SceneHooks.AimPreview` is in: set it from your controller with the shape to draw (`TryAimShape`, `AimShape` in
`Presentation/Core/AimShape.cs`) and `CombatFx.Abilities` draws it; `TestPanel` does so today in OnEnable/OnDisable.
Delete this note when done.
