# The Chorus: a frog card that pins a whole trench

Design page of 2026-10-08 for the item `idea-the-chorus-a-frog-card-that-pins-a-whole` (stage game-design). Every "already" below names the file it was read in; paths under `trench-warfare-3d/Assets/_Project/`.

## 1. The rule
One support card, no aiming. Press it and the frogs in your front trench breathe in for a moment, then croak together, again and again. While they croak, every enemy on foot in the strip in front of that trench, as wide as the trench and out past the trench opposite, keeps his head down: men in the open lie flat and neither move nor shoot, the garrison opposite stops firing. It hurts nobody. It needs at least four frogs alive in that trench, and it stops the moment fewer are left or the trench is lost.

## 2. What the player sees and does
1. The card sits with the support cards; it is lit only while four frogs man the front trench.
2. He sends his wave over the top and keeps the choir at home (the cost: those frogs are not in the attack).
3. He presses the card. The frogs' throats swell for a second and a half: the enemy can see it coming.
4. Sixteen croaks, two a second. Each one is a warm ring rolling out over no man's land from the trench.
5. Where a ring passes, enemy men drop flat; the muzzle flashes along the trench opposite go out.
6. His wave runs the last stretch into a silent trench and fights it out with bombs and hand to hand.
7. The croaking ends; about seven seconds later the garrison's heads come up again.

## 3. Numbers (all placeholder; the balance simulator sweeps them)
- **Cost 100 silver** (placeholder). Dearer than the smoke screen (60), cheaper than chlorine (120) and the HE barrage (150), all in `Sim/Match/OffMapAbilities.cs`: it kills nobody but silences a whole trench. 100 s of income at `SilverPerSecond = 1` (`Sim/Core/SimConfig.cs`).
- **Cooldown 1,800 ticks = 90 s** (placeholder), the strafe run's. Tick rate 20 (`SimConfig.cs`).
- **Warm-up 30 ticks = 1.5 s** (placeholder): shorter than the smoke's 40, because the tell is on his own trench.
- **16 pulses, one every 10 ticks = 8 s of croaking** (placeholder). A pulse sets suppression to 100, the meter's top (`Sim/Combat/Suppression.cs`); it decays 8 a second, so between pulses a man stays above Pinned (85, `Sim/Core/StanceRules.cs`).
- **After the last pulse**: a man in the open is up again in 1.9 s (100 to 85) and off his belly in 5 s (to 60); a garrison fires again in 7.5 s (to `GarrisonFireSuppressionLimit` 40, `Sim/Combat/CombatTables.cs`). So the trench opposite is silent about 15.5 s.
- **What 15.5 s buys**: about 70 m at a sprint (rifleman 3.0 m/s x 1.5 for Sprint, `RosterEntry.cs`, `StanceRules.cs`). The playtest map has 200 m of no man's land (`Sim/Terrain/GreyboxMapGenerator.cs`), so on it the wave must be out before the card is played; "croak, then go" holds only on a field under about 70 m. This is the first thing to sweep.
- **Reach 220 m forward of the trench** (placeholder): the 200 m of no man's land plus the trench opposite.
- **Choir of 4 frogs at least** (placeholder): 100 silver of men (Frog costs 25, `Sim/Match/UnitDefinitions.cs`) held back.

## 4. Edge cases
1. **In a trench**: a garrisoned man never takes the Pinned stance (`Sim/Nav/MovementSystem.cs`: garrison is decided before suppression); he goes silent through the 40 limit in `Sim/Combat/TargetAcquisition.cs`. Show must draw him flat from the pulse event, the sim's stance will not say it.
2. **Map edge**: the strip is cut at the map. **Frogs die mid-chorus**: every pulse counts the living frogs in the trench that are themselves under 85 suppression; under four, that pulse and the rest do nothing. No refund.
3. **The trench is taken mid-chorus**: a pulse whose trench is no longer the caller's does nothing.
4. **Both sides croak on the same tick**: count both choirs first, then apply both pulses, or player 0 would silence player 1's choir by command order.
5. **Officers and the Death Battalion**: held at 84 by the aura and by `NeverPinned` (`Suppression.cs`, `Sim/Combat/Aura.cs`). In the open they keep moving and shooting; in a trench they are still silent (84 is over 40).
6. **Machines and men in the air** are untouched. A pinned man who is struck fights back (`MovementSystem.cs`).
7. **3,000 units**: one pass over the units per pulse (count and strip test), two passes a second, no pair loop; one event per pulse, not one per man.
8. **Replay**: no dice, whole ticks, and the pulses wait in `OffMapAbilitySystem.Scheduled`, which is already hashed.
9. **Frogs against frogs**: enemy frogs are pinned like anyone, or the card is dead in the main faction's mirror.

## 5. What it needs
- Sim: `OffMapAbilityId.Chorus = 13` and its stats in `OffMapAbilities.cs` (12, ParaDrop, is the last id; `AbilitySlots` is 16), target mode None (in the enum; used so far only by ReinforcementSurge in `Editor/SliceDefinitions.cs`). **Ability id seam.**
- Sim: the call is refused, silver kept, when `FlowFieldManager.FrontTrench` holds fewer than four frogs (archetype 25).
- Sim: `PayloadKind.Croak = 5`, appended; sixteen payloads at the call, the trench id carried in a field `ScheduledPayload` already has. A new event appended to `SimEventType` (the pulse: trench, player, the strip, the frogs croaking). `AbilityFired` marks the breath in.
- Sim: bit 13 in `FactionRoster.AbilityMask` (`Sim/Core/FactionRoster.cs`), per question 1.
- Sim: **replay version 37 to 38**, with the chain pinned in `Tests/Sim/SimHashTests.cs`. No archetype id, no header or file layout change. Another lane may take 13 or 38 first: take the next free at landing.
- Sim tests: the garrison opposite fires nothing during the chorus; three frogs refuse it; killing the choir stops it.
- Show (concepts first, `docs/reference/goals.md`): the frog's croak pose and throat sac; the ring over the ground; the trench opposite going dark. `Clip.PinnedLoop` is in `Sim/Core/UnitPose.cs`; whether the frog figure has that clip: not found.
- Show: the card, by the checklist "Adding a support ability" in `docs/reference/tasks.md` (HudView, HudLayout, KeyMap, HudText, SkinSpec, TestPanel, CombatFx). A croak sound: no audio system was looked for; not found.

## 6. What it must not break
- **"The six support cards stay shared, and a card the player's faction cannot call is hidden"** (decisions.md, 2026-10-06, not built). The Chorus is a seventh shared-or-not card: question 1.
- **The battle bar** draws at 0.91 on 16:9 with seven support cards and `HudLayoutTests` holds 0.9 (decisions.md, 2026-09-27; the seven are in `UI/HudView.cs`). An eighth card needs a hidden one or a new fit.
- **There is no frog faction in the sim**: `FactionId` has six and none is frogs (`Sim/Core/Faction.cs`); archetype 25 is in no pool (`FactionRoster.cs`; decisions.md, 2026-09-28). "The frog faction is the main faction" is decided and not built (decisions.md, 2026-10-06). So "frog-only" is carried by the rule itself: no frogs in the trench, no chorus.
- **The hero moment** scores pinned men (`Sim/Combat/HeroSystem.cs`): a chorus raises the enemy's chance of one. Leave it; it is a fair answer.
- **No constant noise** (goals.md): one card, one trench, 90 s apart.
- "The sniper stays the tenth card" is in the idea's own check (`idea.json`); a row for it in decisions.md: not found. "In the measured matches nobody goes over the top" is the idea's why-now; the measurement itself: not found.

Cut from the design: damage; a longer chorus for more frogs; frogs holding their fire while they croak; a trench he picks; enemy frogs croaking back; frog machines joining in.

## 7. Open to the owner
1. **Who gets the card before the frog faction exists?** Default: every faction may call it and the card shows only when the player's loadout has the frog; it narrows to the frog faction when that is built. (Other answer: wait for the faction.)
2. **Do the frog machines (Croaker, Bullfrog) count as voices in the choir?** Default: no, infantry frogs only; the picture is a trench of small frogs.
