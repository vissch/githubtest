// Phase: B6 / docs/21 phase 6 (implemented) — staging: the mission (its picture for the seed and ground, its text and
// numbers, the difficulty tabs), the battalion (the roster's eight slots as cutouts with the tiers bought for them,
// read-only: the roster swap lands with the ten-slot roster of units-meta), the support abilities the Home Front has
// unlocked (up to two picks), and DEPLOY: the mission's request with the depot's silver and income, the factions
// and the ability mask, the campaign session begun, the profile saved, the battle loaded.
using System.Collections.Generic;
using Unity.Collections;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;

namespace TW.UI
{
    public sealed class StagingScreen : ShellScreen
    {
        public const string TreePath = "Shell/Staging";
        public static readonly string[] RequiredNames = { "staging-screen", "mission-title", "mission-subtitle", "info-image", "info-body", "info-stats", "difficulty-tabs", "difficulty-blurb", "slot-row", "unit-info", "ability-row", "ability-note", "btn-homefront", "btn-back", "btn-deploy" };
        public const int MaxAbilityPicks = 2, DefaultDifficulty = 1;
        public const string RosterNote = "THE FIXED EIGHT. ROSTER SWAPS LAND WITH THE TEN-SLOT ROSTER.";
        public const string AbilityNote = "PICK UP TO TWO. THE PICKS REACH THE SIM WITH THE UPGRADE SEAM; UNTIL THEN EVERY UNLOCK IS ON THE HUD.";
        public const string NothingUnlocked = "ONLY THE BARRAGE AND THE GAS: THE SIGNALS STATION UNLOCKS THE REST.";
        public override bool HidesHud => true;

        readonly CampaignProfile given;
        CampaignProfile profile; bool persist;
        readonly string nodeId; readonly int index;
        CampaignNode node; CampaignMission mission;
        int difficulty = DefaultDifficulty; uint picks; Texture2D thumb;
        readonly List<Button> diffTabs = new List<Button>(), tiles = new List<Button>(), abilityButtons = new List<Button>();
        readonly List<OffMapAbilityId> offered = new List<OffMapAbilityId>();
        public uint Picks => picks;
        public int Difficulty => difficulty;
        public byte Faction => (byte)Mathf.Clamp(profile != null ? profile.Faction : 0, 0, 1);

        public StagingScreen(CampaignProfile profile, string nodeId, int index) { given = profile; this.nodeId = nodeId; this.index = index; }

        public override VisualTreeAsset Tree(ShellAssets a) => Resources.Load<VisualTreeAsset>(TreePath);

        protected override void OnBind()
        {
            profile = given ?? ProfileStore.Current; persist = given == null;
            node = CampaignGraph.Find(nodeId);
            mission = node != null && index >= 0 && index < node.Missions.Length ? node.Missions[index] : null;
            Btn("btn-back", () => Router?.Pop());
            Btn("btn-homefront", () => Router?.Push(new HomeFrontScreen(given)));
            Btn("btn-deploy", Deploy);
            if (mission == null) { Root.Q<Button>("btn-deploy")?.SetEnabled(false); SetText("mission-title", "NO SUCH MISSION"); return; }
            SetText("mission-title", mission.Title);
            SetText("mission-subtitle", mission.Subtitle + "  ·  MISSION " + (index + 1) + " OF " + node.Missions.Length);
            SetText("info-body", mission.Description);
            var img = Root.Q("info-image");
            if (img != null)
            {
                thumb = MapThumbnail.Render(mission.Seed, mission.Ground);
                img.style.backgroundImage = thumb != null ? new StyleBackground(thumb) : new StyleBackground(StyleKeyword.None);
            }
            var tabs = Root.Q("difficulty-tabs");
            diffTabs.Clear();
            if (tabs != null)
            {
                tabs.Clear();
                for (int i = 0; i < CampaignDifficulty.Standard.Length; i++)
                {
                    int idx = i; var b = new Button { name = "tab-difficulty-" + i, text = CampaignDifficulty.Standard[i].Name }; b.AddToClassList("tw-tab"); b.focusable = false;
                    b.clicked += () => SetDifficulty(idx);
                    tabs.Add(b); diffTabs.Add(b);
                }
            }
            BuildRoster();
            BuildAbilities();
            SetDifficulty(difficulty);
            Root.Q<Button>("btn-deploy")?.SetEnabled(CampaignGraph.CanFight(node, index, profile));
        }

        protected override void OnUnbind() { if (thumb != null) { Discard(thumb); thumb = null; } }

        public void SetDifficulty(int d)
        {
            difficulty = Mathf.Clamp(d, 0, CampaignDifficulty.Standard.Length - 1);
            for (int i = 0; i < diffTabs.Count; i++) diffTabs[i].EnableInClassList("tw-tab--active", i == difficulty);
            SetText("difficulty-blurb", CampaignDifficulty.Standard[difficulty].Blurb);
            RefreshStats();
        }

        void RefreshStats()
        {
            var d = CampaignDifficulty.Standard[difficulty];
            float every = d.PeerDeployEveryTicks / 20f;
            int silver = FactionBuildings.StartingSilverBonus(profile, Faction); float income = FactionBuildings.IncomeBonus(profile, Faction);
            bool won = profile.IsComplete(node.Id, index);
            SetText("info-stats",
                $"FRONT {mission.FrontLine}\nSEED {mission.Seed}\nBOMBARDMENT {mission.Bombardment:0}/MIN\nENEMY DEPLOYS EVERY {every:0.#} S\n" +
                $"ENEMY ATTACKS AT {(d.PeerAttacks ? d.PeerAttackGarrison + "+ MEN" : "NEVER")}\nENEMY TANKS: {(d.PeerDeploysTanks ? "YES" : "NO")}\nENEMY SUPPORT FIRE: {(d.PeerUsesSupport ? "YES" : "NO")}\n" +
                $"WAR CHEST +{silver} SILVER\nSUPPLY +{income:0.00}/S\n" +
                (won ? "ALREADY WON: NO GOLD AGAIN" : $"REWARD {CampaignGraph.Reward(node, index)} GOLD"));
        }

        void BuildRoster()
        {
            var row = Root.Q("slot-row");
            tiles.Clear();
            if (row == null) return;
            row.Clear();
            var roster = new NativeArray<RosterEntry>(RosterEntry.SlotCount * 2, Allocator.Temp);
            RosterEntry.FillDefault(roster, 0); RosterEntry.FillDefault(roster, RosterEntry.SlotCount);
            int offset = Faction * RosterEntry.SlotCount;
            for (int s = 0; s < RosterEntry.SlotCount; s++)
            {
                var e = roster[offset + s];
                var tile = new Button { name = "slot-" + s }; tile.AddToClassList("tw-list-row"); tile.AddToClassList("campaign-slot"); tile.focusable = false;
                var art = new VisualElement { pickingMode = PickingMode.Ignore }; art.AddToClassList("campaign-slot__art");
                var tex = UnitArt.Full(UnitArt.NameOf(e.Archetype)); if (tex != null) art.style.backgroundImage = new StyleBackground(tex);
                var name = new Label(HudText.Name(e.Archetype).ToUpperInvariant()) { pickingMode = PickingMode.Ignore }; name.AddToClassList("campaign-slot__name");
                var tier = new Label(TierText(e.Archetype)) { pickingMode = PickingMode.Ignore }; tier.AddToClassList("tw-caption"); tier.AddToClassList("campaign-slot__tier");
                tile.Add(art); tile.Add(name); tile.Add(tier);
                byte a = e.Archetype; tile.clicked += () => SelectUnit(a, tile);
                row.Add(tile); tiles.Add(tile);
            }
            roster.Dispose();
            SetText("unit-info", RosterNote);
        }

        string TierText(byte archetype)
        {
            int h = FactionBuildings.UnitTier(profile, Faction, archetype, UnitTrack.Health), d = FactionBuildings.UnitTier(profile, Faction, archetype, UnitTrack.Damage), a = FactionBuildings.UnitTier(profile, Faction, archetype, UnitTrack.Accuracy);
            return h + d + a == 0 ? "NO TIERS" : $"HP {h}  DMG {d}  ACC {a}";
        }

        void SelectUnit(byte archetype, Button tile)
        {
            foreach (var t in tiles) t.EnableInClassList("tw-list-row--selected", t == tile);
            SetText("unit-info", HudText.Name(archetype).ToUpperInvariant() + "  ·  " + TierText(archetype) + "\n" + HudText.Tip(archetype));
        }

        void BuildAbilities()
        {
            var row = Root.Q("ability-row");
            abilityButtons.Clear(); offered.Clear(); picks = 0;
            if (row == null) return;
            row.Clear();
            uint mask = FactionBuildings.AbilityMask(profile, Faction);
            foreach (var id in HudView.SupportAbilities)
            {
                if ((mask & (1u << (int)id)) == 0) continue;
                var b = new Button { name = "ability-" + id, text = HudText.Support(id).Name }; b.AddToClassList("tw-btn"); b.AddToClassList("tw-btn--small"); b.AddToClassList("campaign-ability"); b.focusable = false;
                var pick = id; b.clicked += () => Toggle(pick);
                row.Add(b); abilityButtons.Add(b); offered.Add(id);
            }
            SetText("ability-note", offered.Count > 2 ? AbilityNote : NothingUnlocked);
            RefreshAbilities();
        }

        public void Toggle(OffMapAbilityId id)
        {
            uint bit = 1u << (int)id;
            if ((picks & bit) != 0) picks &= ~bit;
            else if (Count(picks) < MaxAbilityPicks) picks |= bit;
            RefreshAbilities();
        }

        static int Count(uint bits) { int n = 0; while (bits != 0) { n += (int)(bits & 1u); bits >>= 1; } return n; }

        void RefreshAbilities()
        {
            for (int i = 0; i < abilityButtons.Count; i++) abilityButtons[i].EnableInClassList("tw-btn--primary", (picks & (1u << (int)offered[i])) != 0);
        }

        /// <summary>The request DEPLOY launches: the mission at the difficulty, the depot's bonuses, the factions, the abilities.</summary>
        public MatchLaunch.Request BuildRequest()
        {
            if (mission == null) return null;
            var r = mission.Build(CampaignGraph.MissionId(node, index), difficulty);
            FactionBuildings.ApplyTo(r, profile, Faction);
            r.FactionA = Faction; r.FactionB = node.EnemyFaction;
            r.AbilityMaskA = picks != 0 ? picks : FactionBuildings.AbilityMask(profile, Faction);
            r.AbilityMaskB = FactionBuildings.AbilityMask(new CampaignProfile(), node.EnemyFaction);
            return r;
        }

        void Deploy()
        {
            var r = BuildRequest();
            if (r == null || !CampaignGraph.CanFight(node, index, profile)) return;
            CampaignSession.Begin(node.Id, index, Faction);
            profile.LastNode = node.Id;
            if (persist) ProfileStore.Save(profile);
            Router?.StartMission(r);
        }
    }
}
