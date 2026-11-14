# Final CelFx Windows delivery - 2026-11-14

Owner-selected final set: green_laser R15, blue_laser c52, bullet_impacts r05, explosion candidate74, fire_bolts candidate32, flamethrower candidate59, orbital_laser r26, plus the recorded artillery, cyan_beam and neon_acid commits. Exact source/evidence origins are in celfx-final-selection.json.

All ten effects compile into the Windows player, with ShaderKeep materials and linker preservation for reflective kind/callback discovery. Start the player with `-twvfx green_laser` to open the runtime preview and select any effect. The preview uses a neutral stage, not the calibrated reference beach. Normal battle routing is still the game's existing routing; replacing live battle effects is a separate pending owner choice. The reference sequences contain whole volleys and must not be replayed wholesale for every bullet event.

Shared integration preserves explosion delayed births, target-frame payloads, LOD replacement groups and transient light cards; flamethrower emitter preparation; orbital lighting lifecycle hooks; and fire-bolt persistent light cards. Blue-laser near branches and its distant thread share a LOD replacement group without changing their counts. Palette capacity rises from64 to128 so the complete library's96 custom entries plus built-ins fit. The later orbital shadow prototype is excluded.

Validation: 228 CelFx/Project native tests passed after integration fixes. The broader native precommit gate ran1589 tests:1557 passed,0 failed,32 skipped (Long tests excluded). The documentation/validation selftests passed51/51; gate/gate-scope selftests passed53/53 after the fake full-run fixture was updated to include the new CelFx test assembly. Windows release build succeeded at363MB with0 errors and33 warnings. Target-device GPU time remains unmeasured. These facts do not claim exact visual identity to the references.

A native player smoke test exercises each effect for7.5 seconds after playback begins. Logs and build evidence live outside the repository under C:/Users/PC/Documents/celfx/final_build_20261114. Final build and commit hashes are recorded in that delivery report.
