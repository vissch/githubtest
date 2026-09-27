// Phase: B6 (implemented) — the selection rules, without a scene: a handle is (slot, generation) so a reused slot is not
// the man who died in it; toggles, groups and pruning; the picker takes the unit the cursor is most centrally on and the
// nearer on a tie, the box takes only our units, the double-click only that type and side on the screen; the hexagon
// the markers are cut from is a hexagon.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.UI;

namespace TW.Tests
{
    public class SelectionTests
    {
        static UnitHandle H(int slot, ushort gen = 1) => new UnitHandle(slot, gen);

        [Test]
        public void AReusedSlotIsNotTheSameUnit()
        {
            Assert.That(H(5, 1), Is.EqualTo(H(5, 1)));
            Assert.That(H(5, 1), Is.Not.EqualTo(H(5, 2)), "the new man in a dead man's slot is someone else");
            var m = new SelectionModel();
            m.Set(new[] { H(5, 1), H(6, 1) });
            m.Prune(h => !(h.Slot == 5 && h.Gen == 1));   // slot 5 died and was reused
            Assert.That(m.Count, Is.EqualTo(1)); Assert.That(m.Contains(H(6, 1)), Is.True);
        }

        [Test]
        public void ToggleAddsThenRemovesAndDuplicatesAreIgnored()
        {
            var m = new SelectionModel();
            m.Toggle(H(1)); Assert.That(m.Contains(H(1)), Is.True);
            m.Toggle(H(1)); Assert.That(m.Count, Is.EqualTo(0));
            m.Set(new[] { H(2), H(2), H(3) });
            Assert.That(m.Count, Is.EqualTo(2));
            int v = m.Version; m.Add(H(3)); Assert.That(m.Version, Is.EqualTo(v), "adding what is already selected changes nothing");
        }

        [Test]
        public void GroupsStoreAndRecallAndLoseTheDead()
        {
            var m = new SelectionModel();
            m.Set(new[] { H(1), H(2), H(3) });
            m.Assign(0);
            m.Clear();
            Assert.That(m.Recall(0), Is.True); Assert.That(m.Count, Is.EqualTo(3));
            Assert.That(m.Recall(4), Is.False, "an empty group changes nothing");
            Assert.That(m.Count, Is.EqualTo(3));
            m.Prune(h => h.Slot != 2);
            Assert.That(m.GroupSize(0), Is.EqualTo(2), "the dead leave the group too");
            Assert.That(SelectionModel.KeyDigit(0), Is.EqualTo(1)); Assert.That(SelectionModel.KeyDigit(9), Is.EqualTo(0));
        }

        static ScreenUnit U(int slot, float x, float y, float r, float depth, bool ours = true, byte archetype = 0) =>
            new ScreenUnit { Slot = slot, Gen = 1, Screen = new Vector2(x, y), RadiusPx = r, Depth = depth, Ours = ours, Archetype = archetype };

        [Test]
        public void TheClickTakesTheUnitItIsMostCentrallyOnThenTheNearer()
        {
            var us = new List<ScreenUnit> { U(0, 100, 100, 20, 50), U(1, 110, 100, 20, 30), U(2, 400, 400, 20, 10) };
            Assert.That(UnitPicker.Nearest(us, new Vector2(101, 100)), Is.EqualTo(0), "the cursor is on 0's centre");
            Assert.That(UnitPicker.Nearest(us, new Vector2(105, 100)), Is.EqualTo(1), "equally central: the nearer to the camera");
            Assert.That(UnitPicker.Nearest(us, new Vector2(250, 250)), Is.EqualTo(-1), "empty ground");
        }

        [Test]
        public void TheBoxTakesOnlyOurUnitsInsideItFromAnyCorner()
        {
            var us = new List<ScreenUnit> { U(0, 10, 10, 10, 5), U(1, 50, 50, 10, 5, ours: false), U(2, 90, 90, 10, 5), U(3, 200, 200, 10, 5) };
            var into = new List<int>();
            UnitPicker.InRect(us, new Vector2(100, 0), new Vector2(0, 100), true, into);
            Assert.That(into, Is.EquivalentTo(new[] { 0, 2 }));
        }

        [Test]
        public void TheDoubleClickTakesThatTypeAndSideOnTheScreenOnly()
        {
            var us = new List<ScreenUnit> { U(0, 10, 10, 10, 5, true, 2), U(1, 50, 50, 10, 5, true, 2), U(2, 60, 60, 10, 5, false, 2), U(3, 70, 70, 10, 5, true, 1), U(4, 5000, 50, 10, 5, true, 2) };
            var into = new List<int>();
            UnitPicker.SameTypeOnScreen(us, 2, true, 1920, 1080, into);
            Assert.That(into, Is.EquivalentTo(new[] { 0, 1 }));
        }

        [Test]
        public void AKnotUnderTheCursorIsOneClumpOfOneSideNearestFirst()
        {
            var us = new List<ScreenUnit>
            {
                U(0, 100, 100, 10, 50), U(1, 110, 104, 10, 40), U(2, 92, 96, 10, 60),   // a knot of ours
                U(3, 104, 98, 10, 30, ours: false),                                     // an enemy in it
                U(4, 160, 100, 10, 20),                                                 // one of ours further off
            };
            var into = new List<int>();
            Assert.That(UnitPicker.Clump(us, new Vector2(101, 100), into), Is.EqualTo(0));
            Assert.That(into[0], Is.EqualTo(0), "the nearest leads");
            Assert.That(into, Is.EquivalentTo(new[] { 0, 1, 2 }), "ours in reach, not the enemy, not the man 60 px off");
            Assert.That(UnitPicker.Clump(us, new Vector2(400, 400), into), Is.EqualTo(-1)); Assert.That(into, Is.Empty);
            var big = new List<ScreenUnit> { U(0, 100, 100, 60, 10), U(1, 170, 100, 60, 10) };   // zoomed in: two big men side by side
            UnitPicker.Clump(big, new Vector2(100, 100), into);
            Assert.That(into, Is.EquivalentTo(new[] { 0 }), "a man is not clumped with a neighbour the cursor is not on");
        }

        [Test]
        public void TheStateReadsTheWorstThingFirst()
        {
            uint alive = (uint)TW.Sim.UnitFlags.Alive, trench = alive | (uint)TW.Sim.UnitFlags.InTrench;
            Assert.That(UnitStatus.Of(trench, TW.Sim.Stance.FireStep, 0f, false, 0f), Is.EqualTo(UnitState.InTrench));
            Assert.That(UnitStatus.Of(trench, TW.Sim.Stance.FireStep, 0f, true, 0f), Is.EqualTo(UnitState.Engaging));
            Assert.That(UnitStatus.Of(trench | (uint)TW.Sim.UnitFlags.HoldFire, TW.Sim.Stance.FireStep, 0f, true, 0f), Is.EqualTo(UnitState.HoldingFire));
            Assert.That(UnitStatus.Of(trench | (uint)TW.Sim.UnitFlags.Burning, TW.Sim.Stance.Pinned, 90f, true, 0f), Is.EqualTo(UnitState.Burning));
            Assert.That(UnitStatus.Of(trench, TW.Sim.Stance.Crouch, 90f, false, 0f), Is.EqualTo(UnitState.Pinned));
            Assert.That(UnitStatus.Of(alive | (uint)TW.Sim.UnitFlags.Exposed, TW.Sim.Stance.Standing, 0f, false, 4f), Is.EqualTo(UnitState.Advancing));
            Assert.That(UnitStatus.Of(alive, TW.Sim.Stance.Standing, 0f, false, 0f), Is.EqualTo(UnitState.Waiting));
            uint tank = alive | (uint)TW.Sim.UnitFlags.Vehicle;
            Assert.That(UnitStatus.Of(tank, TW.Sim.Stance.Standing, 99f, false, 0f), Is.EqualTo(UnitState.Waiting), "a tank is never pinned");
            Assert.That(UnitStatus.Of(tank | (uint)TW.Sim.UnitFlags.KnockedOut | (uint)TW.Sim.UnitFlags.Burning, TW.Sim.Stance.Standing, 0f, false, 0f), Is.EqualTo(UnitState.KnockedOut));
            Assert.That(UnitStatus.Tone(UnitState.Pinned), Is.EqualTo(StateTone.Alarm));
            Assert.That(UnitStatus.Tone(UnitState.Suppressed), Is.EqualTo(StateTone.Warn));
            Assert.That(UnitStatus.Tone(UnitState.InTrench), Is.EqualTo(StateTone.Normal));
        }

        [Test]
        public void AClumpSummaryGivesTheTwoCommonestStatesWithCounts()
        {
            var c = new int[UnitStatus.StateCount];
            c[(int)UnitState.InTrench] = 5; c[(int)UnitState.Pinned] = 2; c[(int)UnitState.Waiting] = 1;
            Assert.That(UnitStatus.Summary(c, out var tone), Is.EqualTo("5 IN TRENCH  ·  2 PINNED"));
            Assert.That(tone, Is.EqualTo(StateTone.Alarm), "the worst tone of anyone in the knot");
            System.Array.Clear(c, 0, c.Length); c[(int)UnitState.Moving] = 4;
            Assert.That(UnitStatus.Summary(c, out tone), Is.EqualTo("MOVING")); Assert.That(tone, Is.EqualTo(StateTone.Normal));
            System.Array.Clear(c, 0, c.Length); c[(int)UnitState.Waiting] = 3; c[(int)UnitState.Burning] = 3;
            Assert.That(UnitStatus.Summary(c, out tone), Does.StartWith("3 BURNING"), "a tie goes to the worse");
        }

        static ScreenUnit At(float x, float z, bool ours) => new ScreenUnit { World = new Vector3(x, 1f, z), Ours = ours };

        [Test]
        public void AStrikeCountsTheEnemyInItsCircleAndOursInItsReach()
        {
            var us = new List<ScreenUnit>
            {
                At(10, 0, false), At(0, 24, false), At(0, 30, false),   // enemy: in, in, outside the 25 m circle
                At(-20, 0, true), At(0, -32, true), At(40, 0, true),    // ours: in, in the reach (25 + 8), beyond it
            };
            var enemy = new List<ScreenUnit>(); var ours = new List<ScreenUnit>();
            AimReadout.Tally(us, Vector2.zero, 25f, 33f, enemy, ours);
            Assert.That(enemy.Count, Is.EqualTo(2));
            Assert.That(ours.Count, Is.EqualTo(2), "a man a shell's blast outside the circle is still in reach");
        }

        [Test]
        public void TheReachIsTheCirclePlusOneShellAndGasReachesFurtherThanItsReticle()
        {
            Assert.That(AimReadout.Radii(TW.Sim.Match.OffMapAbilityId.HeBarrage, out float hit, out float reach), Is.True);
            Assert.That(TW.Sim.Match.OffMapAbilitySystem.TryGetStats((int)TW.Sim.Match.OffMapAbilityId.HeBarrage, out var he), Is.True);
            Assert.That(hit, Is.EqualTo(he.Radius)); Assert.That(reach, Is.EqualTo(he.Radius + he.ShellRadius));
            Assert.That(AimReadout.Radii(TW.Sim.Match.OffMapAbilityId.ChlorineGas, out hit, out reach), Is.True);
            Assert.That(reach, Is.GreaterThan(hit), "gas drifts: ours are warned further out");
            Assert.That(AimReadout.Radii(TW.Sim.Match.OffMapAbilityId.None, out _, out _), Is.False);
        }

        [Test]
        public void ATrenchBadgeTurnsShakenAtAThirdAndPinnedAtHalf()
        {
            Assert.That(GarrisonStats.Grade(0, 0, 0), Is.EqualTo(GarrisonStats.Morale.Steady), "an empty trench is not in trouble");
            Assert.That(GarrisonStats.Grade(12, 1, 2), Is.EqualTo(GarrisonStats.Morale.Steady));
            Assert.That(GarrisonStats.Grade(12, 1, 3), Is.EqualTo(GarrisonStats.Morale.Shaken));
            Assert.That(GarrisonStats.Grade(12, 6, 0), Is.EqualTo(GarrisonStats.Morale.Pinned));
        }

        [Test]
        public void AGroupCutDownQuicklyIsHitAndOneBledSlowlyIsNot()
        {
            var a = new GroupAlerts();
            a.Observe(0, 1, 12, 0, 0f);                                  // assigned: twelve men
            a.Observe(0, 1, 11, 0, 1f); a.Observe(0, 1, 10, 0, 20f);     // one, then another much later
            Assert.That(a.Of(0, 20f), Is.EqualTo(GroupAlerts.Alert.None), "two losses 19 s apart are attrition, not an alarm");
            a.Observe(0, 1, 8, 0, 22f); a.Observe(0, 1, 7, 0, 23f);     // three inside the window: a quarter of twelve
            Assert.That(a.Of(0, 23f), Is.EqualTo(GroupAlerts.Alert.Hit));
            Assert.That(a.Of(0, 23f + GroupAlerts.HitShowSeconds + 0.1f), Is.EqualTo(GroupAlerts.Alert.None), "the flash ends");
        }

        [Test]
        public void HalfPinnedIsPinnedAWipedGroupIsLostAndReassigningStartsAgain()
        {
            var a = new GroupAlerts();
            a.Observe(3, 1, 6, 3, 0f); a.Observe(3, 1, 6, 3, 1f);
            Assert.That(a.Of(3, 1f), Is.EqualTo(GroupAlerts.Alert.Pinned));
            a.Observe(3, 1, 0, 0, 2f);
            Assert.That(a.Of(3, 2f), Is.EqualTo(GroupAlerts.Alert.Lost), "lost outranks hit");
            Assert.That(a.Of(3, 2f + GroupAlerts.LostShowSeconds + 0.1f), Is.EqualTo(GroupAlerts.Alert.None), "then the chip goes");
            a.Observe(3, 2, 4, 0, 3f);                                   // Ctrl+4 on new men
            Assert.That(a.Of(3, 3f), Is.EqualTo(GroupAlerts.Alert.None)); Assert.That(a.Strength(3), Is.EqualTo(4));
            Assert.That(new GroupAlerts().Of(5, 0f), Is.EqualTo(GroupAlerts.Alert.None), "a group never assigned says nothing");
            var m = new SelectionModel(); int s0 = m.GroupStamp(2); m.Assign(2);
            Assert.That(m.GroupStamp(2), Is.Not.EqualTo(s0), "assigning is what tells the alerts to start again");
        }

        [Test]
        public void ASelectionIsScopedToATrenchOnlyWhenEveryManGarrisonsIt()
        {
            int t = -1, m = 0;
            Assert.That(TrenchScope.Step(ref t, ref m, true, true, false, 2, 0), Is.True);   // a rifleman in trench 2
            Assert.That(TrenchScope.Step(ref t, ref m, true, true, false, 2, 2), Is.True);   // an MG in trench 2
            Assert.That(t, Is.EqualTo(2)); Assert.That(m, Is.EqualTo((1 << 0) | (1 << 2)));
            int t2 = t, m2 = m;
            Assert.That(TrenchScope.Step(ref t2, ref m2, true, true, false, 3, 0), Is.False, "a man in another trench breaks it");
            t2 = t; m2 = m;
            Assert.That(TrenchScope.Step(ref t2, ref m2, true, true, false, -1, 0), Is.False, "a man out in the open breaks it");
            t2 = t; m2 = m;
            Assert.That(TrenchScope.Step(ref t2, ref m2, true, false, false, 2, 0), Is.False, "an enemy breaks it");
            t2 = t; m2 = m;
            Assert.That(TrenchScope.Step(ref t2, ref m2, true, true, true, 2, 5), Is.False, "a vehicle breaks it: they never advance by type");
        }

        [Test]
        public void ASelectionThatCoversEveryCategoryPresentIsAPlainAdvance()
        {
            int present = (1 << 0) | (1 << 1) | (1 << 2);
            Assert.That(TrenchScope.Effective(1 << 2, present), Is.EqualTo(1 << 2), "only the MGs");
            Assert.That(TrenchScope.Effective(present, present), Is.EqualTo(0), "everyone: the ordinary over the top");
            Assert.That(TrenchScope.Effective(present | (1 << 10), present), Is.EqualTo(0), "a category no longer present does not matter");
        }

        [Test]
        public void OursInReachAreNamedByTypeMostFirst()
        {
            var c = new int[256];
            c[0] = 1; c[2] = 2;                                           // one rifleman, two MGs
            string mg = TW.UI.HudText.Name(2).ToUpperInvariant(), rifle = TW.UI.HudText.Name(0).ToUpperInvariant();
            Assert.That(AimReadout.Breakdown(c, 3), Is.EqualTo("2 " + mg + ", 1 " + rifle));
            c[1] = 1; c[10] = 1;                                          // and an assault man and a sniper: four types
            Assert.That(AimReadout.Breakdown(c, 3), Does.EndWith(" +1 MORE"), "three named, the rest counted");
            Assert.That(AimReadout.Breakdown(new int[256], 3), Is.Empty);
        }

        [Test]
        public void DeathsCloseTogetherShareOneCountedSkull()
        {
            var root = new UnityEngine.UIElements.VisualElement();
            root.Add(new UnityEngine.UIElements.VisualElement { name = "markers-layer" });
            var d = new DeathMarks(root, null, v => v);
            d.Add(new Vector3(0, 0, 0), true, 0f); d.Add(new Vector3(2, 0, 1), true, 0.3f); d.Add(new Vector3(1, 0, 0), true, 0.6f);
            Assert.That(d.Count, Is.EqualTo(1)); Assert.That(d.CountOf(0), Is.EqualTo(3), "three of ours fell together: one skull, x3");
            d.Add(new Vector3(1, 0, 0), false, 0.7f);
            Assert.That(d.Count, Is.EqualTo(2), "an enemy death gets its own skull, whatever it is next to");
            d.Add(new Vector3(40, 0, 0), true, 0.8f);
            Assert.That(d.Count, Is.EqualTo(3), "a death far off gets its own");
            d.Add(new Vector3(0, 0, 0), true, 0.8f + DeathMarks.MergeSeconds + 0.5f);
            Assert.That(d.Count, Is.EqualTo(4), "a death after the moment has passed gets its own");
            d.Dispose();
        }

        [Test]
        public void TheSkullHasDarkSocketsInsideASolidHead()
        {
            Assert.That(DeathMarks.HeadSdf(0f, 0.2f), Is.LessThan(0f), "the cranium");
            Assert.That(DeathMarks.HeadSdf(0f, -0.25f), Is.LessThan(0f), "the jaw");
            Assert.That(DeathMarks.HeadSdf(0.45f, 0.45f), Is.GreaterThan(0f), "a corner is outside");
            Assert.That(DeathMarks.HolesSdf(0.13f, -0.005f), Is.LessThan(0f), "an eye socket");
            Assert.That(DeathMarks.HeadSdf(0.13f, -0.005f), Is.LessThan(0f), "which is inside the head, so it is drawn dark, not cut out");
        }

        [Test]
        public void TheLogoSpinsItsCogInThenTurnsSlowlyAndItsWordsLand()
        {
            Assert.That(GameLogo.GearAngle(0f), Is.EqualTo(GameLogo.GearFromDeg).Within(1e-3f), "the cog starts wound back");
            Assert.That(GameLogo.GearAngle(GameLogo.GearSpinIn), Is.EqualTo(0f).Within(1e-3f), "and arrives where the art has it");
            Assert.That(GameLogo.GearAngle(GameLogo.GearSpinIn + 10f), Is.EqualTo(10f * GameLogo.GearIdleDegPerSecond).Within(1e-3f), "then turns on slowly");
            GameLogo.Pose(0f, out _, out float fa0, out _, out float ta0, out _, out float ba0, out _);
            Assert.That(fa0, Is.EqualTo(0f)); Assert.That(ta0, Is.EqualTo(0f)); Assert.That(ba0, Is.EqualTo(0f));
            GameLogo.Pose(3f, out float fy, out float fa, out float ts, out float ta, out float bs, out float ba, out _);
            Assert.That(fa, Is.EqualTo(1f)); Assert.That(ta, Is.EqualTo(1f)); Assert.That(ba, Is.EqualTo(1f));
            Assert.That(fy, Is.EqualTo(0f).Within(1e-4f)); Assert.That(ts, Is.EqualTo(1f).Within(1e-4f)); Assert.That(bs, Is.EqualTo(1f).Within(1e-4f));
            GameLogo.Pose(GameLogo.TopAt + GameLogo.WordIn * 0.5f, out _, out _, out _, out _, out float bsEarly, out float baEarly, out _);
            Assert.That(baEarly, Is.EqualTo(0f), "TOADSTEEL lands after DIESELFRONT");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt - 0.01f, GameLogo.GlintAt, 12), Is.EqualTo(-1), "no glint before its time");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt, GameLogo.GlintAt, 12), Is.EqualTo(0));
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + GameLogo.GlintSweep - 0.001f, GameLogo.GlintAt, 12), Is.EqualTo(11), "the sweep ends on the last frame");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + 2f, GameLogo.GlintAt, 12), Is.EqualTo(-1), "and rests between sweeps");
            Assert.That(GameLogo.GlintFrame(GameLogo.GlintAt + GameLogo.GlintPeriod, GameLogo.GlintAt, 12), Is.EqualTo(0), "then comes round again");
            Assert.That(GameLogo.Punch(-0.1f), Is.EqualTo(0f)); Assert.That(GameLogo.Punch(GameLogo.PunchSeconds * 0.5f), Is.EqualTo(GameLogo.PunchScale).Within(1e-4f));
            Assert.That(GameLogo.Punch(GameLogo.PunchSeconds + 0.01f), Is.EqualTo(0f), "a landing punches once");
            Assert.That(GameLogo.Kick(3f), Is.LessThan(0.01f), "the cog's kick settles");
        }

        [Test]
        public void TheLogoShipsWithEveryLayerItsLayoutNames()
        {
            var json = Resources.Load<TextAsset>(GameLogo.Folder + "/layout");
            Assert.That(json, Is.Not.Null, "Resources/Logo/layout.json");
            foreach (var n in new[] { "shadow", "glint_top_0", "glint_bottom_11", "window", "cog_top", "gear", "plate", "side_left", "side_right", "cog_left", "cog_right", "word_top", "word_bottom", "logo_full", "wordmark" })
            {
                Assert.That(json.text.Contains("\"" + n + "\"") || n == "logo_full" || n == "wordmark" || n.StartsWith("glint"), Is.True, $"layout names {n}");
                Assert.That(Resources.Load<Texture2D>(GameLogo.Folder + "/" + n), Is.Not.Null, $"Resources/Logo/{n}.png");
            }
        }

        [Test]
        public void MenGrowWithTheZoomAsTheyAreDrawn()
        {
            Assert.That(UnitPicker.Grow(10f), Is.EqualTo(1f));
            Assert.That(UnitPicker.Grow(48f), Is.EqualTo(2f).Within(1e-4f));
            Assert.That(UnitPicker.Grow(1000f), Is.EqualTo(UnitPicker.MaxGrow));
        }

        [Test]
        public void TheMarkerHexagonIsAHexagon()
        {
            const float r = 0.4f, apothem = r * 0.8660254f;
            Assert.That(SelectionMarkers.HexSdf(r, 0f, r), Is.EqualTo(0f).Within(1e-3f), "a vertex on the x axis (flat top)");
            Assert.That(SelectionMarkers.HexSdf(0f, apothem, r), Is.EqualTo(0f).Within(1e-3f), "the top edge at the apothem");
            Assert.That(SelectionMarkers.HexSdf(0f, 0f, r), Is.LessThan(0f), "the centre is inside");
            Assert.That(SelectionMarkers.HexSdf(0f, r, r), Is.GreaterThan(0f), "above the flat top is outside");
        }
    }
}
