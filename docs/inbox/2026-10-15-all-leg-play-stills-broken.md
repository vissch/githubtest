# `leg play` cannot run a Stills test

`Tools/relay/legcmd.py`'s `play_command` hardcodes `--mode PlayMode -- -nographics`. Every `TW.Tests.Stills`
test (and likely `TW.Tests.EditMode`/`.Match`/etc UnityTests that enter Play from an EditMode asmdef) is
`includePlatforms: ["Editor"]`, i.e. EditMode-only, and the header comment on `DeathStills.cs` says it needs a
graphics device (no `-nographics`). `leg play EveryDeathGagIsPhotographed` reports RED "no verdict ... no test
ran" (testMode PlayMode excludes it); running it anyway with `-nographics` fails every still with
`RenderTexture.Create failed`.

Worked around for `punch-quarter-second` (2026-10-15) by calling `unity.exe test . --mode EditMode --filter
<name> --output <path> --timeout 600` directly (no `-nographics`), same CLI `Tools/tw` pins. `TW_RELAY_PLAY`
can't help: a leg is refused for touching the relay's own env var.

Fix belongs in `Tools/relay/legcmd.py` (shared, its own commit) — e.g. detect the test's assembly mode, or add
a `--edit-mode`/graphics flag to `leg play`. Not done here: out of lane and out of this unit's scope.
