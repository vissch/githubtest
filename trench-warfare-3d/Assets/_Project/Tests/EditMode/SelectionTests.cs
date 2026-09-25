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
        public void MenGrowWithTheZoomAsTheyAreDrawn()
        {
            Assert.That(UnitPicker.Grow(10f), Is.EqualTo(1f));
            Assert.That(UnitPicker.Grow(48f), Is.EqualTo(2f).Within(1e-4f));
            Assert.That(UnitPicker.Grow(1000f), Is.EqualTo(UnitPicker.MaxGrow));
        }

        [Test]
        public void ThePickerSizesMenAsTheRendererDrawsThem()
        {
            // The picker once kept its own 1.5 after the renderer went to 1.125, and every pick radius was a third
            // too big. Both now read FigureMetrics; this catches a renderer default set anywhere else.
            var go = new GameObject("vat-defaults");
            try
            {
                var vat = go.AddComponent<TW.Presentation.Units.VATRenderer>();
                Assert.That(UnitPicker.FigureScale, Is.EqualTo(vat.UnitScale), "pick radius scale vs drawn scale");
                Assert.That(UnitPicker.GrowFromZoom, Is.EqualTo(vat.GrowFromZoom));
                Assert.That(UnitPicker.MaxGrow, Is.EqualTo(vat.MaxGrow));
            }
            finally { Object.DestroyImmediate(go); }
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
