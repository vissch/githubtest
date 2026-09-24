// Phase: B6 (implemented) — one entry on the mission select: a map, its knobs, and the difficulties it offers.
// A light sibling of the A6 MissionDefinition (factions, waves, triggers), which nothing steps yet: this only holds
// what SimHost reads today. ToRequest starts from the scene's own defaults so the default card is bit-identical to
// pressing Play in GreyboxCorridor (MissionCatalogTests holds ShelledWood1917 to seed 1917 and today's knobs).
using System;
using UnityEngine;
using TW.Presentation;

namespace TW.UI
{
    [CreateAssetMenu(menuName = "TW/Mission Card", fileName = "Mission")]
    public sealed class MissionCard : ScriptableObject
    {
        public string Id = "shelled-wood-1917";
        public string Title = "SHELLED WOOD, 1917";
        public string Subtitle = "THE COAST";
        [TextArea(3, 8)] public string Description = "A wood shelled to stumps, a river with fords and one bridge, and the sea beyond the enemy line, where your reinforcements land.";
        public Texture2D Thumbnail;

        [Header("Map")]
        public bool GeneratedBattlefield = true;
        public bool PlaytestMap = true;           // used when GeneratedBattlefield is off
        public uint BattlefieldSeed = 1917;
        [Tooltip("Which battlefield. The shelled wood is the zero value, so cards made before this are unchanged.")]
        public Ground Ground = TW.Presentation.Ground.ShelledForest;
        public bool AllowSeedEdit = true;
        public float Bombardment = 8f;
        public string FrontLine = "90 x 240 M";

        [Header("Match")]
        public uint MatchSeed = 0xC0FFEE;
        public int StartingSilver = 300;
        public float SilverPerSecond = 2f;

        [Serializable]
        public sealed class Difficulty
        {
            public string Name = "NORMAL";
            public string Blurb = "The enemy deploys every two seconds and attacks at eight men.";
            public int PeerDeployEveryTicks = 40;
            public bool PeerAttacks = true;
            public int PeerAttackGarrison = 8;
            public bool PeerDeploysTanks = false;
            public bool PeerUsesSupport = true;
            public int PeerSupportReserve = 180;
        }

        public Difficulty[] Difficulties =
        {
            new Difficulty { Name = "EASY", Blurb = "A slow enemy that attacks late and fields no armour or support.", PeerDeployEveryTicks = 60, PeerAttacks = true, PeerAttackGarrison = 12, PeerDeploysTanks = false, PeerUsesSupport = false, PeerSupportReserve = 400 },
            new Difficulty { Name = "NORMAL", Blurb = "Today's opponent: deploys every two seconds, attacks at eight men, shells you when it can.", PeerDeployEveryTicks = 40, PeerAttacks = true, PeerAttackGarrison = 8, PeerDeploysTanks = false, PeerUsesSupport = true, PeerSupportReserve = 180 },
            new Difficulty { Name = "HARD", Blurb = "Fast deploys, early attacks, tanks whenever it can afford them.", PeerDeployEveryTicks = 28, PeerAttacks = true, PeerAttackGarrison = 6, PeerDeploysTanks = true, PeerUsesSupport = true, PeerSupportReserve = 120 },
        };

        public int DefaultDifficulty = 1;

        public MatchLaunch.Request ToRequest(int difficulty, uint? seedOverride = null)
        {
            var d = Difficulties[Mathf.Clamp(difficulty, 0, Difficulties.Length - 1)];
            return new MatchLaunch.Request
            {
                MissionId = Id, Title = Title, Difficulty = d.Name,
                MatchSeed = MatchSeed, StartingSilver = StartingSilver, SilverPerSecond = SilverPerSecond,
                GeneratedBattlefield = GeneratedBattlefield, PlaytestMap = PlaytestMap,
                BattlefieldSeed = AllowSeedEdit && seedOverride.HasValue ? seedOverride.Value : BattlefieldSeed,
                Ground = Ground,
                Bombardment = Bombardment, ScriptedPeer = true,
                PeerDeployEveryTicks = Mathf.Max(1, d.PeerDeployEveryTicks), PeerAttacks = d.PeerAttacks, PeerAttackGarrison = d.PeerAttackGarrison,
                PeerDeploysTanks = d.PeerDeploysTanks, PeerUsesSupport = d.PeerUsesSupport, PeerSupportReserve = d.PeerSupportReserve,
            };
        }
    }
}
