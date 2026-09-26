// Phase: B6 / docs/21 phase 6 (implemented) — the player's campaign: which faction, the gold, the missions won, how far
// each Home Front building is built and which upgrade lines were bought on it, the abilities unlocked. One
// serialisable tree with a Version, like GameSettings; JsonUtility, so unknown fields are ignored and missing ones
// take their defaults. Migrate clamps what an older or hand-edited file may carry.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation
{
    [Serializable]
    public sealed class CampaignProfile
    {
        public const int CurrentVersion = 1;
        public const int MaxStage = 3, MaxTier = 6;
        public const int StartingGold = 60;

        public int Version = CurrentVersion;
        /// <summary>0 Iron, 1 Brass.</summary>
        public byte Faction;
        public int Gold = StartingGold;
        /// <summary>"node/index" for every mission won (a replayed mission pays nothing).</summary>
        public List<string> CompletedMissions = new List<string>();

        [Serializable] public sealed class BuildingState { public byte Faction; public string Id = ""; public int Stage; }
        [Serializable] public sealed class LineState { public byte Faction; public string Building = ""; public int Line; public int Tier; }
        public List<BuildingState> Buildings = new List<BuildingState>();
        public List<LineState> Lines = new List<LineState>();
        /// <summary>A bit per OffMapAbilityId the player has unlocked at the Signals Station.</summary>
        public uint UnlockedAbilities;
        public string LastNode = "";
        public float HeroPity;

        public static string MissionKey(string node, int index) => node + "/" + index;
        public bool IsComplete(string node, int index) => CompletedMissions.Contains(MissionKey(node, index));
        /// <summary>Records a win. False when it was already recorded (nothing is paid twice).</summary>
        public bool Complete(string node, int index)
        {
            string key = MissionKey(node, index);
            if (CompletedMissions.Contains(key)) return false;
            CompletedMissions.Add(key);
            return true;
        }

        public int StageOf(byte faction, string id)
        {
            foreach (var b in Buildings) if (b.Faction == faction && b.Id == id) return b.Stage;
            return 0;
        }

        public void SetStage(byte faction, string id, int stage)
        {
            stage = Mathf.Clamp(stage, 0, MaxStage);
            foreach (var b in Buildings) if (b.Faction == faction && b.Id == id) { b.Stage = stage; return; }
            Buildings.Add(new BuildingState { Faction = faction, Id = id, Stage = stage });
        }

        public int TierOf(byte faction, string building, int line)
        {
            foreach (var l in Lines) if (l.Faction == faction && l.Building == building && l.Line == line) return l.Tier;
            return 0;
        }

        public void SetTier(byte faction, string building, int line, int tier)
        {
            tier = Mathf.Clamp(tier, 0, MaxTier);
            foreach (var l in Lines) if (l.Faction == faction && l.Building == building && l.Line == line) { l.Tier = tier; return; }
            Lines.Add(new LineState { Faction = faction, Building = building, Line = line, Tier = tier });
        }

        /// <summary>Clamp what an older or hand-edited file may carry, and fill what it lacks.</summary>
        public void Migrate()
        {
            CompletedMissions ??= new List<string>();
            Buildings ??= new List<BuildingState>();
            Lines ??= new List<LineState>();
            if (Gold < 0) Gold = 0;
            if (Faction > 1) Faction = 0;
            foreach (var b in Buildings) { b.Id ??= ""; b.Stage = Mathf.Clamp(b.Stage, 0, MaxStage); }
            foreach (var l in Lines) { l.Building ??= ""; l.Tier = Mathf.Clamp(l.Tier, 0, MaxTier); }
            LastNode ??= "";
            if (HeroPity < 0f || float.IsNaN(HeroPity)) HeroPity = 0f;
            Version = CurrentVersion;
        }

        public CampaignProfile Clone()
        {
            var c = JsonUtility.FromJson<CampaignProfile>(JsonUtility.ToJson(this));
            c.Migrate();
            return c;
        }

        public string ToJson() => JsonUtility.ToJson(this, true);

        public static CampaignProfile FromJson(string json)
        {
            var p = JsonUtility.FromJson<CampaignProfile>(json) ?? new CampaignProfile();
            p.Migrate();
            return p;
        }
    }
}
