# To lane/show/playground-drawn, from lane/show/wreck-stages (2026-09-29)

A dead machine's hull is now drawn as its carcass: `TankRenderer.WreckStages` cuts the model's root part (LOD0 and
LOD1) into chunks at load (`WreckModel`) and hides them as the sim's wreck prop wears down (Wreck, BrokenWreck, Scrap,
Cleared). Your Brute, Croaker, Mercy and Hopper get this for free through `ModelFor`; if a root part cannot be cut
(`WreckModel.Build` returns null) it is drawn whole as before. `TankRenderer.View` is now a partial class
(`TankRenderer.WreckStages.cs` adds fields) and `Tank_URP.shader` has a `_Chunks` instanced float. Please look at one of
your machines' wrecks shelled through its stages (`Tests/Stills/WreckStills`, `TW_WRECK_MACHINE=<archetype>`).
Delete this note when you have read it.
