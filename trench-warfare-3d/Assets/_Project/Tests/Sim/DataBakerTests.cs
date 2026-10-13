// Phase: review fixes (2026-10-13) — [S10] DataBaker's campaign Faction -> sim FactionId mapping and
// ApplyUpgrades' ignored dmgTier/accTier.
using NUnit.Framework;
using UnityEngine;
using TW.Sim.Combat;
using TW.Data;

namespace TW.Tests
{
    public class DataBakerTests
    {
        [Test]   // [S10]
        public void BakeMission_MapsCampaignFactionThroughFactionMap()
        {
            var playerFaction = ScriptableObject.CreateInstance<FactionDefinition>();
            playerFaction.Faction = Faction.German;
            var mission = ScriptableObject.CreateInstance<MissionDefinition>();
            mission.PlayerFaction = playerFaction;

            var script = DataBaker.BakeMission(mission, Unity.Collections.Allocator.Temp);
            try
            {
                Assert.AreEqual((byte)FactionMap.ToSim(Faction.German), script.PlayerFaction,
                    "[S10] BakeMission must map through FactionMap.ToSim, not cast the campaign Faction byte directly");
                Assert.AreEqual(3, script.PlayerFaction, "[S10] FactionId.German is 3 by FactionMap's table");
            }
            finally { script.Dispose(); }

            Object.DestroyImmediate(playerFaction);
            Object.DestroyImmediate(mission);
        }

        [Test]   // [S10]
        public void ApplyUpgrades_AppliesDamageAndAccuracyTiers()
        {
            var stats = default(TW.Sim.Units.UnitStats);
            var weapon = new WeaponStats { Damage = 25f, Accuracy = 0.5f };

            DataBaker.ApplyUpgrades(stats, weapon, hpTier: 0, dmgTier: 2, accTier: 1, out var upgraded);

            Assert.AreEqual(35f, upgraded.Damage, 1e-4f, "[S10] ApplyUpgrades must apply +5 per dmgTier (25 + 5*2 = 35)");
            Assert.AreEqual(0.55f, upgraded.Accuracy, 1e-4f, "[S10] ApplyUpgrades must apply accTier to Accuracy, not ignore it");
        }
    }
}
