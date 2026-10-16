// Phase: B6 (implemented) — the unit artwork is complete and wired: every unit either side can field has its cutout
// (the armoury shows it), everyone with a face has all six condition faces (the speaker strip and the battlefield
// reports use them), the art imports with straight alpha, and the speaker strip flaps, settles, queues and clears.
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class UnitArtTests
    {
        [Test]
        public void EveryDeployableUnitHasItsCutout()
        {
            foreach (byte a in ArmouryScreen.Units())
                Assert.That(UnitArt.Full(UnitArt.NameOf(a)), Is.Not.Null, $"archetype {a} ({UnitArt.NameOf(a)}) has no Resources/UnitArt cutout");
            Assert.That(UnitArt.Full(UnitArt.Narrator), Is.Not.Null, "the Sergeant (narrator) has no cutout");
        }

        [Test]
        public void EveryFaceHasAllSixStates()
        {
            foreach (var face in UnitArt.Faces)
                for (int m = 0; m < UnitArt.StateNames.Length; m++)
                    Assert.That(Resources.Load<Texture2D>(UnitArt.StatePath(face, (Mood)m)), Is.Not.Null, $"{face} has no '{UnitArt.StateNames[m]}' face");
        }

        [Test]
        public void TheArtImportsWithStraightAlphaAndNoMips()
        {
            int textures = 0;
            foreach (var guid in AssetDatabase.FindAssets("t:Texture2D", new[] { "Assets/_Project/UI/Resources/UnitArt" }))
            {
                textures++;
                var path = AssetDatabase.GUIDToAssetPath(guid);
                var imp = (TextureImporter)AssetImporter.GetAtPath(path);
                Assert.That(imp.alphaIsTransparency, Is.True, path);
                Assert.That(imp.mipmapEnabled, Is.False, path);
                var tex = AssetDatabase.LoadAssetAtPath<Texture2D>(path);
                Assert.That(tex.width % 4 == 0 && tex.height % 4 == 0, $"{path} is {tex.width}x{tex.height}: block compression needs multiples of 4");
            }
            // [TU11] FindAssets returning nothing (a moved or renamed folder) used to pass this test in silence.
            Assert.That(textures, Is.GreaterThan(0), "[TU11] no texture at all under Assets/_Project/UI/Resources/UnitArt: the import settings of nothing were checked");
        }

        static HudDialogue Strip(out VisualElement root)
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>("Assets/_Project/UI/Resources/Hud/BattleHud.uxml");
            root = tree.Instantiate();
            return new HudDialogue(root);
        }

        [Test]
        public void TheStripTalksThenSettlesThenClears()
        {
            var d = Strip(out var root);
            var strip = root.Q("dialogue"); var portrait = root.Q("dialogue-portrait");
            Assert.That(strip.style.display.value, Is.EqualTo(DisplayStyle.None), "hidden until someone speaks");
            d.Say("Rifleman", "RIFLE", "Rifles up!", Mood.Cheering, 2f);
            d.Tick(0.01f);
            Assert.That(d.Showing, Is.True); Assert.That(strip.style.display.value, Is.EqualTo(DisplayStyle.Flex));
            Assert.That(root.Q<Label>("dialogue-text").text, Is.EqualTo("Rifles up!"));
            var talking = portrait.style.backgroundImage.value.texture;
            Assert.That(talking, Is.SameAs(UnitArt.State("Rifleman", Mood.Talking)), "a line starts on the talking face");
            d.Tick(1.5f);   // past the talking time
            Assert.That(portrait.style.backgroundImage.value.texture, Is.SameAs(UnitArt.State("Rifleman", Mood.Cheering)), "then settles on the line's mood");
            d.Tick(1f);
            Assert.That(d.Showing, Is.False); Assert.That(strip.style.display.value, Is.EqualTo(DisplayStyle.None));
        }

        [Test]
        public void AHurtReportCarriesAStatusChipAndOnlyCriticalKeylinesThePortrait()
        {
            var d = Strip(out var root);
            var strip = root.Q("dialogue"); var status = root.Q<Label>("dialogue-status");
            d.Say("Rifleman", "RIFLE", "Five down.", Mood.Wounded, 1f);
            d.Tick(0.01f);
            Assert.That(status.text, Is.EqualTo("WOUNDED"));
            Assert.That(strip.ClassListContains("is-hurt"), Is.True); Assert.That(strip.ClassListContains("is-critical"), Is.False);
            d.Tick(1f);
            d.Say("Sergeant", "SERGEANT", "Hold the line.", Mood.Neutral, 1f);
            d.Tick(0.01f);
            Assert.That(strip.ClassListContains("is-hurt"), Is.False, "an ordinary line drops the chip");
            d.Tick(1f);
            d.Say("Rifleman", "RIFLE", "Fifteen down.", Mood.Critical, 1f);
            d.Tick(0.01f);
            Assert.That(status.text, Is.EqualTo("CRITICAL")); Assert.That(strip.ClassListContains("is-critical"), Is.True);
        }

/// <summary>[TU10] The old test filled `ours` from the same roster `all` is built from, so the subset assert held by
        /// construction and could not fail. The claim in the name is about the roster halves: slots 0..SlotCount-1 are
        /// Iron's, SlotCount..2n-1 Brass's, so our side owns Iron's four machines and none of Brass's, while the armoury
        /// still lists all eight (the enemy's carry an ENEMY chip). The expected machines come from
        /// FactionBuildingsTests.Unit_Tracks_Name_The_Factions_Own_Machines, not from ArmouryScreen.</summary>
        [Test]
        public void TheArmouryKnowsWhichUnitsAreOurs()
        {
            var ours = new System.Collections.Generic.HashSet<byte>();
            var all = ArmouryScreen.Units(null, ours);
            byte[] iron = { VehicleArchetype.Maw, VehicleArchetype.Pincer, VehicleArchetype.Banner, VehicleArchetype.Breaker };
            byte[] brass = { VehicleArchetype.Tusk, VehicleArchetype.Kettle, VehicleArchetype.Censer, VehicleArchetype.Redoubt };
            foreach (var a in iron) Assert.That(ours.Contains(a), Is.True, $"[TU10] our own machine {a} is not on our side of the roster");
            foreach (var a in brass) Assert.That(ours.Contains(a), Is.False, $"[TU10] the enemy's machine {a} counts as ours");
            foreach (var a in iron) Assert.That(all, Does.Contain(a), $"[TU10] the armoury does not list our machine {a}");
            foreach (var a in brass) Assert.That(all, Does.Contain(a), $"[TU10] the armoury does not list the enemy's machine {a} (it shows it with an ENEMY chip)");
        }

        [Test]
        public void LinesQueueAndAFloodKeepsTheNewest()
        {
            var d = Strip(out _);
            for (int i = 0; i < 10; i++) d.Say("Sergeant", "SERGEANT", "line " + i);
            Assert.That(d.Queued, Is.EqualTo((int)HudDialogue.MaxQueue));
            d.Tick(0.01f);
            Assert.That(d.CurrentText, Is.EqualTo("line 6"), "the oldest were dropped, the rest play in order");
        }

        [Test]
        public void TheSergeantOpensWithTipsAndSpeaksToTrenchesAndTheEnd()
        {
            var d = Strip(out _);
            using (var c = new HudCommentary(null, d, tips: true))
            {
                Assert.That(d.Queued, Is.EqualTo(2), "two opening tips");
                c.OnEvent(new SimEvent { Type = SimEventType.TrenchCaptured, A = 3, B = 0 });
                c.OnEvent(new SimEvent { Type = SimEventType.MatchEnded, A = 1 });
                Assert.That(d.Queued, Is.EqualTo(4));
                c.OnEvent(new SimEvent { Type = SimEventType.Death, A = 5 });   // no world: not ours, silent
                Assert.That(d.Queued, Is.EqualTo(4));
            }
        }

        /// <summary>[I8] a death reaches the HUD a frame late: at 2x-8x the dead man's slot may already hold a new man,
        /// and then his type must not be charged with the loss.</summary>
        [Test]
        public void ALossInARefilledSlotIsNotChargedToTheNewMan()
        {
            var cfg = SimConfig.Default;
            using var w = new SimWorld(cfg, new SimConfig.WorldInit
            {
                SpawnA = new Unity.Mathematics.float3(60f, 0f, 10f),
                SpawnB = new Unity.Mathematics.float3(60f, 0f, 200f),
            });
            int dead = w.Spawn(0, 3, new Unity.Mathematics.float3(1f, 0f, 1f), 10f, 1f, false);
            int live = w.Spawn(0, 2, new Unity.Mathematics.float3(2f, 0f, 2f), 10f, 1f, false);
            w.Flags[dead] &= ~(uint)UnitFlags.Alive;    // he fell; nobody has taken his slot yet

            Assert.That(HudCommentary.OurLoss(w, dead, out byte fell), Is.True, "[I8] a fallen man of ours is a loss of ours");
            Assert.That(fell, Is.EqualTo((byte)3), "[I8] the loss was counted against the wrong type");
            Assert.That(HudCommentary.OurLoss(w, live, out _), Is.False,
                "[I8] the loss was counted against the live man now in the slot");
        }
    }
}
