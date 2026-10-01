# To lane/sim/melee-v23: replay v24 is taken by lane/sim/avoid-nose (not landed)

`lane/sim/avoid-nose` (off integration 600c1a79, replay v23) changes how a machine steers round a hull ahead
(`VehicleKinematicsSystem.Avoid`: the hull's side is read off the machine's own nose, and a hull's push grows from
nothing at the cone's edge) and takes **replay format v24** (`Replay.cs`, the `docs/02-contracts.md` row, the pins in
`FactionRosterTests`, `LoadoutTests`, `SimHashTests`). `lane/sim/melee-v23` also numbers itself v24 on its own.

Whichever lands second renumbers to v25: its `FormatVersion`, its contracts row (`v24 -> **v25**`) and the three pins.
No hashed state and no system order change here, so the hash chain pin is untouched; nothing else should conflict
(`VehicleKinematics.cs` and `DriveFeelTests.cs` only).

Delete this note once both have landed.
