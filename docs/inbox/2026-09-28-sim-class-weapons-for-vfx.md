# To the SIM lane (whichever takes it): class weapons the VFX pass needs from the sim

From `lane/show/pipe-vfx`, 2026-09-28. The owner decided every unit class gets its own VFX ("think of grenades, laser
weapons, mortars") and chose to request the missing weapons from SIM (decisions.md, 2026-09-28). SHOW is building every
look that exists now; these three need sim units or events first, and SHOW will not touch `Sim/**`.

1. **Thrown grenades.** `Sim/Units/Grenades.cs` is a stub (throws NotImplemented). Today the only grenade is the infantry
   close-assault bundle on a vehicle (`Shot` with scalar 1, `DirectFire.cs:203`). Wanted: a grenade a class (or riflemen)
   throws at men or trenches. For the picture: an event at the THROW (thrower slot, target point, flight seconds) and the
   burst as an `Explosion` with its own `Dir.y` BlastShape (not 0, the shell's), so the burst can be a grenade's, not a shell's.
2. **A laser / beam unit class.** No unit carries a beam weapon; only the off-map `Beam` ability (11) exists. Wanted: the unit
   and its weapon, firing as `Shot` (or its own event) with a way to tell a beam from a round: the shooter's archetype is
   enough for SHOW (it reads `w.Archetype[e.A]`), but a continuous beam needs start/stop or a duration.
3. **A flamethrower unit.** The show side is built (`Presentation/Camera/Flamethrower.cs`: jet, pools, torches, pyres),
   driven today only by the debug TestPanel. Wanted: the unit, its stream as events (start/stop, aim, reach) and
   `BlastShape.Incendiary` for its fireball (declared, no producer).
4. **Flight time for the indirect guns** (added 2026-09-29). The Kettle's mortar and the Salvo's rockets burst the tick
   after `VehicleFired` (TankGunnery adds the Impact at once), so the picture's arc onto the burst has to be over in 0.16 s,
   and from the tactical camera a 0.16 s arc along the depth axis reads as a vertical line (critique lin6). Wanted: the
   Impact queued N ticks after the shot (about 0.5 s for the Kettle, 0.35 s for the Salvo), with the flight seconds on
   `VehicleFired` (its `Scalar` is AP/HE today; a new field or event is fine). SHOW then flies a visible shell or rocket.

Nothing here is urgent for SHOW's current work (small arms per class, mortar arc, rockets, tank calibres, mines, the bundle
drawn as a thrown grenade). Reply with a note to `show-pipe-vfx` when any of these lands, with the event fields. Delete this
note when done.
