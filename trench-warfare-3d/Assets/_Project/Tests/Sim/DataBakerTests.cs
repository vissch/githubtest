// Phase: review fixes (2026-10-13) — [S10] DataBaker's campaign Faction -> sim FactionId mapping and
// ApplyUpgrades' ignored dmgTier/accTier.
using System.Text.RegularExpressions;
using NUnit.Framework;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Sim.Combat;
using TW.Data;

namespace TW.Tests
{
    public class DataBakerTests
    {
        [Test]   // [found-a-mission-with-an-unset-faction-fields-facti-20261016-0]
        public void BakeMission_UnsetFaction_BakesFactionMapDefaultAndWarns()
        {
            var mission = ScriptableObject.CreateInstance<MissionDefinition>();
            mission.Title = "Unset";
            mission.PlayerFaction = null;
            mission.EnemyFaction = null;

            LogAssert.Expect(LogType.Warning, new Regex("PlayerFaction"));
            LogAssert.Expect(LogType.Warning, new Regex("EnemyFaction"));

            var script = DataBaker.BakeMission(mission, Unity.Collections.Allocator.Temp);
            try
            {
                Assert.AreEqual(2, script.PlayerFaction,
                    "[found-a-mission-with-an-unset-faction-fields-facti-20261016-0] an unset PlayerFaction must bake FactionId.British (2), FactionMap's own default, not Iron (0)");
                Assert.AreEqual(2, script.EnemyFaction,
                    "[found-a-mission-with-an-unset-faction-fields-facti-20261016-0] an unset EnemyFaction must bake FactionId.British (2), FactionMap's own default, not Iron (0)");
            }
            finally { script.Dispose(); }

            Object.DestroyImmediate(mission);
        }

        [Test]   // [S10] [S3.2]
        public void BakeMission_MapsCampaignFactionThroughFactionMap()
        {
            var playerFaction = ScriptableObject.CreateInstance<FactionDefinition>();
            playerFaction.Faction = Faction.German;
            var enemyFaction = ScriptableObject.CreateInstance<FactionDefinition>();
            enemyFaction.Faction = Faction.French;
            var mission = ScriptableObject.CreateInstance<MissionDefinition>();
            mission.PlayerFaction = playerFaction;
            mission.EnemyFaction = enemyFaction;

            var script = DataBaker.BakeMission(mission, Unity.Collections.Allocator.Temp);
            try
            {
                Assert.AreEqual((byte)FactionMap.ToSim(Faction.German), script.PlayerFaction,
                    "[S10] BakeMission must map through FactionMap.ToSim, not cast the campaign Faction byte directly");
                Assert.AreEqual(3, script.PlayerFaction, "[S10] FactionId.German is 3 by FactionMap's table");
                Assert.AreEqual((byte)FactionMap.ToSim(Faction.French), script.EnemyFaction,
                    "[S3.2] BakeMission must map EnemyFaction through FactionMap.ToSim too, not cast the campaign Faction byte directly");
                Assert.AreEqual(4, script.EnemyFaction, "[S3.2] FactionId.French is 4 by FactionMap's table");
            }
            finally { script.Dispose(); }

            Object.DestroyImmediate(playerFaction);
            Object.DestroyImmediate(enemyFaction);
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
