// Phase: A2 look (2026-09-29) — the drawn bomb (CombatFx.Grenades.cs). It flies on the sim's clock from the thrower's
// hand to where the sim sets it off, CombatTables.GrenadeFlightTicks after the throw: so it must come down on the burst's
// point at the burst's time, and lob as a thrown bomb does (a man's height or two, not the hundred metres the first cut
// reached when its arc ran on the wall clock at a tenth of the speed).
using NUnit.Framework;
using UnityEngine;
using TW.Sim.Combat;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class GrenadeLookTests
    {
        const float Tick = 0.05f;

        [Test]
        public void TheDrawnBomb_ComesDownWhereAndWhenTheSimSetsItOff()
        {
            var hand = new Vector3(10f, 1.7f, 20f);
            foreach (float metres in new[] { CombatTables.GrenadeMin, 9f, 15f, CombatTables.GrenadeRange })
            {
                var to = new Vector3(10f + metres * 0.6f, -0.9f, 20f + metres * 0.8f);   // into a trench, below the hand
                float air = CombatTables.GrenadeFlightTicks(metres, Tick) * Tick;
                var v = CombatFx.BombVelocity(hand, to, air);
                Assert.Less(Vector3.Distance(CombatFx.BombAt(hand, v, air), to), 0.01f, $"{metres} m: lands on the burst");
                Assert.Less(Vector3.Distance(CombatFx.BombAt(hand, v, 0f), hand), 1e-5f, "leaves from the hand");
            }
        }

        [Test]
        public void TheDrawnBomb_LobsAsAThrownBombDoes()
        {
            var hand = new Vector3(0f, 1.7f, 0f);
            foreach (float metres in new[] { CombatTables.GrenadeMin, 9f, 15f, CombatTables.GrenadeRange })
            {
                var to = new Vector3(0f, 0f, metres);
                float air = CombatTables.GrenadeFlightTicks(metres, Tick) * Tick;
                var v = CombatFx.BombVelocity(hand, to, air);
                float top = 0f;
                for (int k = 0; k <= 50; k++) top = Mathf.Max(top, CombatFx.BombAt(hand, v, air * k / 50f).y);
                Assert.That(top - hand.y, Is.InRange(0f, 2.5f), $"{metres} m: its top over the hand");
                Assert.Greater(v.z, 0f, "it goes toward the mark");
            }
        }
    }
}
