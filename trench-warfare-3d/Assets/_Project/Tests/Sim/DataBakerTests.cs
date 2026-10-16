// Phase: review fixes (2026-10-13) — [S10] DataBaker's campaign Faction -> sim FactionId mapping.
using NUnit.Framework;
using UnityEngine;
using TW.Data;

namespace TW.Tests
{
    public class DataBakerTests
    {
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
    }
}
