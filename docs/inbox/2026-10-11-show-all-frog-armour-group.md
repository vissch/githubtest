# The Frogs put five machines on the deploy bar (2026-10-11, from lane/sim/frog-faction-table)

The seventh faction (`FactionId.Frogs` = 6) is now in the rules with its own ten and its own pool. Two things for
the show lane:

1. **Five machines, not four.** `UI/HudView.cs:114` splits the deploy cards into the INFANTRY and ARMOUR groups by
   `RosterEntry.IsVehicle`. Every other faction has four vehicle slots; the Frogs have five (Brute 21, Croaker 22,
   Bullfrog 37, Hopper 23, Mercy 24). Nothing in the sim caps it, and `FactionRosterTests` keeps the
   `LessOrEqual(armour, 4)` assertion for the six shipped factions only. The ARMOUR group's layout needs a look in
   Play before the Frogs are pickable: a fifth card may overflow the bar.
2. **`ProvingGround.DefaultTen(FactionId.Frogs)` returns the real ten now**, not an empty or stand-in list. Anything
   that assumed the frog archetypes were fielded by no faction (sandbox or proving-ground UI copy) should be reread.

Out of scope here and still to do on a show lane: the frog look for the soldiers, card pictures, and the faction
choice in the menu. Delete this note when the bar is checked.
