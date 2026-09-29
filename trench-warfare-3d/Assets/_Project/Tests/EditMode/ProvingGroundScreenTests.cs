// Phase: tooling (2026-09-28) — the Proving Ground's two screens: each UXML has the parts its controller queries and
// binds with no router or panel, every button wears a skin class, the launch screen picks units into the ten being
// picked and builds the request slot for slot, and the panel over a director of its own puts units on the field, builds
// a wave and sends it. Needs the editor (AssetDatabase, UI Toolkit): the director itself is ProvingGroundTests'.
using System.Collections.Generic;
using NUnit.Framework;
using Unity.Collections;
using UnityEditor;
using UnityEngine.UIElements;
using TW.Net;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class ProvingGroundScreenTests
    {
        const string Folder = "Assets/_Project/UI/Resources/Shell/";

        static VisualElement Instantiate(string name)
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>(Folder + name + ".uxml");
            Assert.That(tree, Is.Not.Null, $"{Folder}{name}.uxml did not load");
            return tree.Instantiate();
        }

        static void Skinned(VisualElement root, string uxml) =>
            root.Query<Button>().ForEach(b => Assert.That(b.ClassListContains("tw-btn") || b.ClassListContains("tw-tab") || b.ClassListContains("tw-list-row"), $"{uxml}: button '{b.name}' has no skin class"));

        sealed class Seat : ICommandSink
        {
            public int Player => 1;
            public readonly List<SimCommand> Issued = new List<SimCommand>();
            public void Issue(SimCommand c) { c.Player = 1; Issued.Add(c); }
        }

        [Test]
        public void TheLaunchScreenHasItsPartsAndATileForEveryUnitAndIdea()
        {
            var root = Instantiate("ProvingGround");
            foreach (var n in ProvingGroundLaunchScreen.RequiredNames) Assert.That(root.Q(n), Is.Not.Null, $"ProvingGround.uxml has no element named '{n}'");
            var screen = new ProvingGroundLaunchScreen(fresh: true);
            Assert.DoesNotThrow(() => screen.Bind(root, null));
            try
            {
                Skinned(root, "ProvingGround");
                foreach (var u in ProvingGround.Catalogue())
                {
                    var tile = root.Q<Button>("tile-" + u.Archetype);
                    Assert.That(tile, Is.Not.Null, $"no tile for {u.Name}");
                    Assert.That(tile.parent.name, Is.EqualTo(u.Machine ? "machine-grid" : "infantry-grid"), u.Name);
                    Assert.That(tile.Q<Label>(className: "pg-chip").text, Is.EqualTo(ProvingGround.StatusNames[(int)u.Status]), u.Name);
                }
                for (int i = 0; i < ProvingGround.Ideas.Length; i++) Assert.That(root.Q<Button>("idea-" + i), Is.Not.Null, ProvingGround.Ideas[i].Name);
                Assert.That(root.Q("ten-ours").childCount, Is.EqualTo(RosterEntry.SlotCount));
                Assert.That(root.Q("ten-theirs").childCount, Is.EqualTo(RosterEntry.SlotCount));
                Assert.That(root.Q<Button>("tab-ground-0").ClassListContains("tw-tab--active"), Is.True, "the shelled wood to start");
            }
            finally { screen.Unbind(); }
        }

        [Test]
        public void ATileGoesIntoTheTenBeingPickedAndTheRequestKeepsEverySlot()
        {
            var root = Instantiate("ProvingGround");
            var screen = new ProvingGroundLaunchScreen(fresh: true);
            screen.Bind(root, null);
            try
            {
                var iron = ProvingGround.FrogTen(); var brass = ProvingGround.DefaultTen(FactionId.Brass);
                Assert.That(screen.Ours, Is.EqualTo(iron)); Assert.That(screen.Theirs, Is.EqualTo(brass));
                Assert.That(screen.Pick(VehicleArchetype.Brute), Is.False, "a full ten takes no more");
                Assert.That(root.Q<Label>("ten-note").text, Is.EqualTo(ProvingGroundLaunchScreen.FullNote));

                screen.ClearSlot(0, 3);
                Assert.That(screen.Pick(VehicleArchetype.Brute), Is.True);
                Assert.That(screen.Ours[3], Is.EqualTo(VehicleArchetype.Brute), "the first empty slot");
                Assert.That(root.Q<Button>("slot-0-3").Q<Label>("name").text, Is.EqualTo("BRUTE"));
                Assert.That(screen.Pick(50), Is.False, "an id nothing defines");

                screen.SetSide(1); screen.Clear();
                Assert.That(screen.Pick(InfantryArchetype.Flamethrower), Is.True);
                Assert.That(screen.Pick(VehicleArchetype.A7V), Is.True);
                Assert.That(screen.Theirs[0], Is.EqualTo(InfantryArchetype.Flamethrower)); Assert.That(screen.Theirs[1], Is.EqualTo(VehicleArchetype.A7V));
                Assert.That(screen.Theirs[2], Is.EqualTo(ProvingGroundLaunchScreen.Empty));
                Assert.That(screen.Ours[3], Is.EqualTo(VehicleArchetype.Brute), "the other ten is untouched");

                screen.SetGround(2); screen.SetAi(3);
                var r = screen.BuildRequest();
                Assert.That(r.ProvingGround, Is.True); Assert.That(r.Endless, Is.True);
                Assert.That(r.Ground, Is.EqualTo(Ground.Landing)); Assert.That(r.Difficulty, Is.EqualTo("HARD"));
                Assert.That(r.LoadoutA.Length, Is.EqualTo(RosterEntry.SlotCount)); Assert.That(r.LoadoutB.Length, Is.EqualTo(RosterEntry.SlotCount));
                Assert.That(r.LoadoutA[3], Is.EqualTo(VehicleArchetype.Brute));
                Assert.That(r.LoadoutB[0], Is.EqualTo(InfantryArchetype.Flamethrower)); Assert.That(r.LoadoutB[1], Is.EqualTo(VehicleArchetype.A7V));
                for (int s = 2; s < RosterEntry.SlotCount; s++) Assert.That(r.LoadoutB[s], Is.EqualTo(brass[s]), $"empty slot {s} takes the faction's unit, in its own slot");

                screen.Defaults();
                Assert.That(screen.Theirs, Is.EqualTo(brass));
            }
            finally { screen.Unbind(); }
        }

        [Test]
        public void TheNextVisitOpensOnWhatWasChosen()
        {
            var first = new ProvingGroundLaunchScreen(fresh: true);
            first.Bind(Instantiate("ProvingGround"), null); first.Unbind();
            var a = new ProvingGroundLaunchScreen();
            a.Bind(Instantiate("ProvingGround"), null);
            a.SetSide(0); a.Clear(); a.Pick(VehicleArchetype.Croaker); a.SetGround(1);
            a.Unbind();
            var b = new ProvingGroundLaunchScreen();
            b.Bind(Instantiate("ProvingGround"), null);
            try
            {
                Assert.That(b.Ours[0], Is.EqualTo(VehicleArchetype.Croaker));
                Assert.That(b.BuildRequest().Ground, Is.EqualTo(Ground.WinterLine));
            }
            finally { b.SetSide(0); b.Defaults(); b.SetGround(0); b.Unbind(); }
        }

        /// <summary>A card takes its picture from a USS class named after the portrait. The pictures of the six units of
        /// 2026-09-25, the Breaker and the drop were baked and their classes never written, so their cards were blank;
        /// the Sapper and the Mercy borrow two of them.</summary>
        [Test]
        public void EveryPortraitTheSkinBakesHasItsClass()
        {
            string uss = System.IO.File.ReadAllText(System.IO.Path.Combine(UnityEngine.Application.dataPath, "_Project/UI/Skin/dustfront.components.uss"));
            foreach (var n in SkinSpec.PortraitNames)
                StringAssert.Contains(".tw-portrait-" + n + " { background-image: url(\"Portraits/" + n + ".png\"); }", uss, $"no class for the portrait {n}: its card is blank");
            foreach (var u in ProvingGround.Catalogue())
                StringAssert.Contains(".tw-portrait-" + UnitLook.PortraitName(u.Archetype) + " {", uss, $"{u.Name}'s card is blank");
        }

        [Test]
        public void WhatTheScreensKeepIsForgottenWhenTheSessionEnds()
        {
            var a = new ProvingGroundLaunchScreen();
            a.Bind(Instantiate("ProvingGround"), null);
            a.SetSide(0); a.Clear(); a.Pick(VehicleArchetype.Mercy); a.Unbind();
            var panel = new ProvingGroundPanel();
            panel.Bind(Instantiate("ProvingGroundPanel"), null);
            panel.AddToWave(InfantryArchetype.Frog); panel.Unbind();
            Assert.That(new ProvingGroundPanel().Custom.Units, Is.GreaterThan(0), "setup: your wave outlives the panel (a restart)");

            SceneStatics.ResetSession();
            Assert.That(new ProvingGroundPanel().Custom.Empty, Is.True, "your wave is emptied");
            var b = new ProvingGroundLaunchScreen();
            b.Bind(Instantiate("ProvingGround"), null);
            try { Assert.That(b.Ours, Is.EqualTo(ProvingGround.FrogTen()), "the tens are the defaults again (ours: the frogs, play test)"); }
            finally { b.Unbind(); }
        }

        [Test]
        public void ThePanelHasItsPartsAndBindsWithNoMatch()
        {
            var root = Instantiate("ProvingGroundPanel");
            foreach (var n in ProvingGroundPanel.RequiredNames) Assert.That(root.Q(n), Is.Not.Null, $"ProvingGroundPanel.uxml has no element named '{n}'");
            var panel = new ProvingGroundPanel();
            Assert.DoesNotThrow(() => panel.Bind(root, null));
            try
            {
                Skinned(root, "ProvingGroundPanel");
                Assert.That(panel.Modal, Is.False, "the match plays on under it");
                Assert.That(panel.HidesHud, Is.False); Assert.That(panel.Overlay, Is.True);
                foreach (var u in ProvingGround.Catalogue())
                {
                    Assert.That(root.Q("unit-" + u.Archetype), Is.Not.Null, $"no row for {u.Name}");
                    Assert.That(root.Q<Button>("btn-ours-" + u.Archetype), Is.Not.Null); Assert.That(root.Q<Button>("btn-theirs-" + u.Archetype), Is.Not.Null);
                }
                for (int i = 0; i < ProvingGround.Ideas.Length; i++) Assert.That(root.Q("idea-" + i).Q<Button>(), Is.Null, "an idea has no button");
                Assert.That(root.Q<ScrollView>("wave-list").contentContainer.childCount, Is.EqualTo(ProvingGround.Presets().Count));
                Assert.DoesNotThrow(() => { panel.Spawn(0, InfantryArchetype.Rifle); panel.Send(panel.Custom); panel.SetAi(2); panel.SetSpeed(0); panel.Tick(); }, "no match: nothing happens");

                Assert.That(root.Q("page-units").ClassListContains("pg-page--hidden"), Is.False);
                panel.Show(1);
                Assert.That(root.Q("page-units").ClassListContains("pg-page--hidden"), Is.True);
                Assert.That(root.Q("page-waves").ClassListContains("pg-page--hidden"), Is.False);
                panel.Fold(true);
                Assert.That(root.Q("pg-dock").ClassListContains("pg-dock--collapsed"), Is.True);
            }
            finally { panel.Custom.Squads.Clear(); panel.Unbind(); }
        }

        [Test]
        public void ThePanelPutsUnitsOnTheFieldAndSendsYourOwnWave()
        {
            var cfg = SimConfig.Default; cfg.Endless = true;
            using var m = MatchSim.CreatePlaytest(cfg);
            var seat = new Seat();
            var director = new ProvingGround(a => { a(m); return true; }, () => m, () => seat);
            var root = Instantiate("ProvingGroundPanel");
            var panel = new ProvingGroundPanel(director);
            panel.Bind(root, null);
            try
            {
                panel.Custom.Squads.Clear();
                int Alive(int team, int archetype)
                {
                    int n = 0; var w = m.World;
                    for (int i = 0; i < w.HighWater; i++) if (w.IsAlive(i) && w.Team[i] == team && w.Archetype[i] == archetype) n++;
                    return n;
                }
                Assert.That(panel.Count, Is.EqualTo(5), "x5 to start");
                Assert.That(panel.Spawn(0, VehicleArchetype.Hopper), Is.EqualTo(5));
                Assert.That(Alive(0, VehicleArchetype.Hopper), Is.EqualTo(5));
                Assert.That(root.Q<Label>("status").text, Does.Contain("5 HOPPER FOR US"));

                Assert.That(root.Q<Button>("btn-custom-send").enabledSelf, Is.False, "an empty wave cannot be sent");
                panel.AddToWave(InfantryArchetype.Sapper); panel.AddToWave(InfantryArchetype.Sapper); panel.AddToWave(VehicleArchetype.Whippet);
                Assert.That(panel.Custom.Units, Is.EqualTo(15));
                Assert.That(root.Q<Label>("custom-text").text, Does.Contain("10 SAPPER")); Assert.That(root.Q<Label>("custom-text").text, Does.Contain("5 WHIPPET"));
                Assert.That(root.Q<Button>("btn-custom-send").enabledSelf, Is.True);
                Assert.That(panel.Send(panel.Custom), Is.EqualTo(15));
                Assert.That(Alive(1, InfantryArchetype.Sapper), Is.EqualTo(10)); Assert.That(Alive(1, VehicleArchetype.Whippet), Is.EqualTo(5));

                panel.Repeat(panel.Custom);
                Assert.That(director.Scheduled, Is.Not.Null);
                Assert.That(director.EveryTicks, Is.EqualTo(UnityEngine.Mathf.RoundToInt(ProvingGroundPanel.Everys[panel.EveryIndex] / m.World.Config.TickSeconds)), "30 s of sim time");
                Assert.That(director.EveryTicks, Is.EqualTo(600));
                panel.Tick();
                Assert.That(root.Q<Label>("timer-text").text, Does.StartWith("YOUR WAVE EVERY 30 S"));
                Assert.That(root.Q<Button>("btn-timer-stop").enabledSelf, Is.True);
            }
            finally { panel.Custom.Squads.Clear(); panel.Unbind(); }
        }

        /// <summary>
        /// The sim says who burns (UnitAlight): a jet drawn for the sim's shot, and the fuel it leaves on the ground,
        /// light nobody by themselves. The same stream from the debug panel does, which is what shows the test can fail.
        /// Found on 2026-09-28: the jet was held back and its fuel was not.
        /// </summary>
        [Test]
        public void AJetTheSimFiredAndTheFuelItLeavesLightNobody()
        {
            var books = new TW.Presentation.Tactical.FlipbookFx();
            try
            {
                Assume.That(books.Ready, "the flipbook shader and sheets load in the editor");
                int caught = 0;
                var fire = new TW.Presentation.Tactical.Flamethrower { Catch = (at, radius, seconds) => caught++ };
                System.Func<int, UnityEngine.Vector3> drawn = slot => UnityEngine.Vector3.zero;
                System.Func<float, float, float> ground = (x, z) => 0f;
                float t0 = UnityEngine.Time.time;

                // two seconds of the sim's stream (a jet ends by the frame's clock: it is given two seconds), four more of its fuel
                fire.Burst(3, new UnityEngine.Vector3(0f, 1f, 0f), UnityEngine.Vector3.forward, UnityEngine.Vector3.zero, 2f, sim: true);
                for (float t = 0f; t < 6f; t += 0.02f) { fire.SimNow = t; fire.Update(t0 + t, null, books, drawn, ground); }
                Assert.That(fire.Jets, Is.EqualTo(0), "the stream is over");
                Assert.That(fire.Pools, Is.GreaterThan(0), "the stream left fuel burning on the ground");
                Assert.That(fire.SimPools, Is.EqualTo(fire.Pools), "and all of it is the sim's");
                Assert.That(caught, Is.EqualTo(0), "neither the stream nor its fuel lit anybody");

                // the debug panel's stream over the same ground: it catches, and the fuel it feeds catches again
                fire.Burst(4, new UnityEngine.Vector3(0f, 1f, 0f), UnityEngine.Vector3.forward, UnityEngine.Vector3.zero, 60f);
                for (float t = 6f; t < 8f; t += 0.02f) { fire.SimNow = t; fire.Update(t0 + t, null, books, drawn, ground); }
                Assert.That(caught, Is.GreaterThan(0), "a stream that is not the sim's sets alight what it sweeps");
                Assert.That(fire.SimPools, Is.LessThan(fire.Pools), "and the fuel it fed is no longer the sim's alone");
            }
            finally
            {
                // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode (ColumnLightTests)
                var flags = System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance;
                var mats = (UnityEngine.Material[])typeof(TW.Presentation.Tactical.FlipbookFx).GetField("mats", flags).GetValue(books);
                if (mats != null) foreach (var m in mats) if (m != null) UnityEngine.Object.DestroyImmediate(m);
                var quad = (UnityEngine.Mesh)typeof(TW.Presentation.Tactical.FlipbookFx).GetField("quad", flags).GetValue(books);
                if (quad != null) UnityEngine.Object.DestroyImmediate(quad);
            }
        }

        /// <summary>A jet the sim fired is held by its slot and marked as the sim's.</summary>
        [Test]
        public void AJetTheSimFiredIsHeldByItsSlotAndMarkedAsTheSims()
        {
            var fire = new TW.Presentation.Tactical.Flamethrower();
            fire.Burst(3, new UnityEngine.Vector3(0f, 1f, 0f), UnityEngine.Vector3.forward, UnityEngine.Vector3.zero, 0.45f, sim: true);
            Assert.That(fire.Jets, Is.EqualTo(1)); Assert.That(fire.SimJets, Is.EqualTo(1));
            fire.Burst(3, new UnityEngine.Vector3(0f, 1f, 0f), UnityEngine.Vector3.forward, UnityEngine.Vector3.zero, 0.45f, sim: true);
            Assert.That(fire.Jets, Is.EqualTo(1), "a second shot lengthens his stream");
            fire.Burst(4, new UnityEngine.Vector3(5f, 1f, 0f), UnityEngine.Vector3.forward, UnityEngine.Vector3.zero);
            Assert.That(fire.Jets, Is.EqualTo(2)); Assert.That(fire.SimJets, Is.EqualTo(1), "the debug panel's burst is not the sim's");
        }
    }
}
