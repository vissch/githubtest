// Phase: B6 (implemented) — SECTOR SELECT: the missions on the left, the chosen one's plate on the right, DEPLOY.
// Rows come from the MissionCatalog in ShellAssets; the info panel shows the card's text, a rendered map thumbnail
// for the current seed, the difficulty tabs with their blurb, and the seed field (when the card allows it). DEPLOY
// builds a MatchLaunch.Request and loads the battle.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class MissionSelectScreen : ShellScreen
    {
        public static readonly string[] RequiredNames = { "mission-list", "info-panel", "info-title", "info-subtitle", "info-image", "info-body", "info-stats", "difficulty-tabs", "difficulty-blurb", "seed-row", "seed-field", "btn-seed-random", "btn-deploy", "btn-back" };
        public override bool HidesHud => true;

        readonly List<Button> rows = new List<Button>();
        readonly List<Button> diffTabs = new List<Button>();
        MissionCard card; int difficulty; uint seed;
        Texture2D thumb;

        public override VisualTreeAsset Tree(ShellAssets a) => a?.MissionSelect;

        protected override void OnBind()
        {
            var catalog = Router != null && Router.Assets != null ? Router.Assets.Catalog : null;
            var list = Root.Q("mission-list");
            rows.Clear();
            if (catalog != null && list != null)
            {
                for (int i = 0; i < catalog.Cards.Length; i++)
                {
                    var c = catalog.Cards[i]; if (c == null) continue;
                    var b = new Button { name = "row-" + c.Id }; b.AddToClassList("tw-list-row"); b.focusable = false;
                    var t = new Label(c.Title); t.AddToClassList("tw-list-row__title"); t.pickingMode = PickingMode.Ignore; b.Add(t);
                    var s = new Label(c.Subtitle); s.AddToClassList("tw-list-row__sub"); s.pickingMode = PickingMode.Ignore; b.Add(s);
                    var card = c; b.clicked += () => Select(card);
                    list.Add(b); rows.Add(b);
                }
            }
            var seedField = Root.Q<IntegerField>("seed-field");
            seedField?.RegisterValueChangedCallback(e => { seed = (uint)Mathf.Max(0, e.newValue); RefreshThumb(); RefreshStats(); });
            Btn("btn-seed-random", () => { seed = (uint)Random.Range(1, 99999); seedField?.SetValueWithoutNotify((int)seed); RefreshThumb(); RefreshStats(); });
            Btn("btn-deploy", Deploy);
            Btn("btn-back", () => Router?.Pop());
            if (catalog != null && catalog.Cards.Length > 0 && catalog.Cards[0] != null) Select(catalog.Cards[0]);
            else Root.Q<Button>("btn-deploy")?.SetEnabled(false);
        }

        protected override void OnUnbind() { if (thumb != null) { Discard(thumb); thumb = null; } }

        void Select(MissionCard c)
        {
            card = c; difficulty = Mathf.Clamp(c.DefaultDifficulty, 0, c.Difficulties.Length - 1); seed = c.BattlefieldSeed;
            foreach (var r in rows) r.EnableInClassList("tw-list-row--selected", r.name == "row-" + c.Id);
            SetText("info-title", c.Title); SetText("info-subtitle", c.Subtitle); SetText("info-body", c.Description);
            var tabs = Root.Q("difficulty-tabs");
            if (tabs != null)
            {
                tabs.Clear(); diffTabs.Clear();
                for (int i = 0; i < c.Difficulties.Length; i++)
                {
                    int idx = i; var b = new Button { name = "tab-difficulty-" + i, text = c.Difficulties[i].Name }; b.AddToClassList("tw-tab"); b.focusable = false;
                    b.clicked += () => { difficulty = idx; RefreshDifficulty(); RefreshStats(); };
                    tabs.Add(b); diffTabs.Add(b);
                }
            }
            var seedRow = Root.Q("seed-row"); seedRow?.EnableInClassList("tw-hidden", !c.AllowSeedEdit);
            Root.Q<IntegerField>("seed-field")?.SetValueWithoutNotify((int)seed);
            RefreshDifficulty(); RefreshStats(); RefreshThumb();
            Root.Q<Button>("btn-deploy")?.SetEnabled(true);
        }

        void RefreshDifficulty()
        {
            for (int i = 0; i < diffTabs.Count; i++) diffTabs[i].EnableInClassList("tw-tab--active", i == difficulty);
            SetText("difficulty-blurb", card != null && difficulty < card.Difficulties.Length ? card.Difficulties[difficulty].Blurb : "");
        }

        void RefreshStats()
        {
            if (card == null) return;
            var d = card.Difficulties[Mathf.Clamp(difficulty, 0, card.Difficulties.Length - 1)];
            float every = d.PeerDeployEveryTicks / 20f;
            SetText("info-stats",
                $"FRONT {card.FrontLine}\nSEED {seed}\nBOMBARDMENT {card.Bombardment:0}/MIN\nENEMY DEPLOYS EVERY {every:0.#} S\n" +
                $"ENEMY ATTACKS AT {(d.PeerAttacks ? d.PeerAttackGarrison + "+ MEN" : "NEVER")}\nENEMY TANKS: {(d.PeerDeploysTanks ? "YES" : "NO")}\nENEMY SUPPORT FIRE: {(d.PeerUsesSupport ? "YES" : "NO")}");
        }

        void RefreshThumb()
        {
            var img = Root.Q("info-image");
            if (img == null || card == null) return;
            if (card.Thumbnail != null) { img.style.backgroundImage = new StyleBackground(card.Thumbnail); return; }
            if (thumb != null) { Discard(thumb); thumb = null; }
            thumb = card.GeneratedBattlefield ? MapThumbnail.Render(seed) : null;
            img.style.backgroundImage = thumb != null ? new StyleBackground(thumb) : new StyleBackground(StyleKeyword.None);
        }

        void Deploy()
        {
            if (card == null) return;
            Router?.StartMission(card.ToRequest(difficulty, seed));
        }
    }
}
