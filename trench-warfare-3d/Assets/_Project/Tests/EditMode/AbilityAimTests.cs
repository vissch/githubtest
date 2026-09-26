// Phase: B6 / docs/21 phase 5 (implemented) — aiming an off-map ability: a point ability fires on the press; a line
// ability is press-drag-release and packs its heading, pattern and length into the command; Shift snaps the heading;
// a short drag runs the ability's own length straight up the field; Tab cycles only the patterns the ability offers;
// the shape drawn is a disc for a point and a corridor for a line; the readout counts a corridor as a capsule; the
// HUD has a card, a key and stats for every support ability. No input, no scene.
using System.Collections.Generic;
using NUnit.Framework;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.UI;

namespace TW.Tests
{
    public class AbilityAimTests
    {
        static readonly Vector3 At = new Vector3(60f, 0f, 200f);

        [Test]
        public void APointAbilityFiresOnThePress()
        {
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.HeBarrage);
            Assert.IsFalse(aim.IsLine);
            Assert.IsTrue(aim.Press(new Vector3(100f, 0f, 200f), 5, 0, out var cmd), "a click is the target");
            Assert.AreEqual(CommandType.SupportFire, cmd.Type); Assert.AreEqual((int)OffMapAbilityId.HeBarrage, cmd.A); Assert.AreEqual(0, cmd.B, "the plain ability: B = 0");
            Assert.AreEqual(100f, cmd.Pos.x); Assert.AreEqual(200f, cmd.Pos.z); Assert.AreEqual(5u, cmd.Tick);
            Assert.AreEqual(OffMapAbilityId.None, aim.Armed, "and the aim is over");
        }

        [Test]
        public void ALineAbilityIsPressDragRelease()
        {
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.StrafeRun);
            Assert.IsTrue(aim.IsLine);
            Assert.IsFalse(aim.Press(At, 7, 0, out _), "the press starts the drag, nothing fires");
            Assert.IsTrue(aim.Dragging);
            aim.Drag(At + new Vector3(20f, 0f, 0f));
            Assert.IsTrue(aim.Release(At + new Vector3(40f, 0f, 0f), 7, 0, out var cmd));
            AbilityArgs.Unpack(cmd.B, out int heading, out int pattern, out int length);
            Assert.AreEqual(90, heading, "dragged along +X"); Assert.AreEqual(0, pattern); Assert.AreEqual(40, length, "as far as the drag");
            Assert.AreEqual(At.x, cmd.Pos.x); Assert.AreEqual(At.z, cmd.Pos.z);
            Assert.AreEqual((int)OffMapAbilityId.StrafeRun, cmd.A);
            Assert.AreEqual(OffMapAbilityId.None, aim.Armed); Assert.IsFalse(aim.Dragging);
            Assert.IsFalse(aim.Release(At, 8, 0, out _), "nothing to release");
        }

        [Test]
        public void ShiftSnapsTheHeadingToFifteenDegrees()
        {
            Assert.AreEqual(18, AbilityAim.HeadingOf(Vector3.zero, new Vector3(10f, 0f, 30f), false));
            Assert.AreEqual(15, AbilityAim.HeadingOf(Vector3.zero, new Vector3(10f, 0f, 30f), true));
            Assert.AreEqual(270, AbilityAim.HeadingOf(Vector3.zero, new Vector3(-30f, 0f, 1f), true), "left is 270, never -90");
            Assert.AreEqual(180, AbilityAim.HeadingOf(Vector3.zero, new Vector3(0f, 0f, -20f), false));
        }

        [Test]
        public void AShortDragRunsTheAbilitysOwnLengthStraightUpTheField()
        {
            Assert.AreEqual(0, AbilityAim.HeadingOf(At, At + new Vector3(0.5f, 0f, 0.5f), false, 0), "player 0 looks up +Z");
            Assert.AreEqual(180, AbilityAim.HeadingOf(At, At + new Vector3(0.5f, 0f, 0.5f), false, 1), "player 1 looks down -Z");
            Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.SmokeScreen, out var smoke));
            Assert.AreEqual(0, AbilityAim.LengthOf(At, At + new Vector3(1f, 0f, 0f), smoke), "0: the ability's own length");
            Assert.AreEqual(Mathf.RoundToInt(OffMapAbilitySystem.MinLength), AbilityAim.LengthOf(At, At + new Vector3(4f, 0f, 0f), smoke), "never shorter than the least");
            Assert.AreEqual(Mathf.RoundToInt(smoke.Length), AbilityAim.LengthOf(At, At + new Vector3(300f, 0f, 0f), smoke), "never longer than the ability's");
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.Beam);
            aim.Press(At, 1, 1, out _);
            Assert.IsTrue(aim.Release(At + new Vector3(0.3f, 0f, 0f), 1, 1, out var cmd));
            AbilityArgs.Unpack(cmd.B, out int heading, out _, out int length);
            Assert.AreEqual(180, heading); Assert.AreEqual(0, length);
        }

        [Test]
        public void TabCyclesOnlyThePatternsTheAbilityOffers()
        {
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.HeBarrage);
            Assert.AreEqual(0, aim.Pattern); Assert.IsFalse(aim.IsLine, "the disc");
            Assert.IsTrue(aim.CyclePattern()); Assert.AreEqual(AbilityPattern.Line, aim.Pattern); Assert.IsTrue(aim.IsLine, "the line");
            Assert.IsTrue(aim.CyclePattern()); Assert.AreEqual(AbilityPattern.Box, aim.Pattern);
            Assert.IsTrue(aim.CyclePattern()); Assert.AreEqual(0, aim.Pattern, "and round");
            aim.Arm(OffMapAbilityId.SmokeScreen);
            Assert.IsFalse(aim.CyclePattern(), "the smoke screen has only its one form"); Assert.AreEqual(0, aim.Pattern);
            aim.Arm(OffMapAbilityId.ChlorineGas);
            Assert.IsFalse(aim.IsLine);
            Assert.IsTrue(aim.CyclePattern()); Assert.AreEqual(AbilityPattern.Creeping, aim.Pattern); Assert.IsTrue(aim.IsLine, "creeping gas is a line");
        }

        [Test]
        public void TheShapeIsADiscForAPointAndACorridorForALine()
        {
            var aim = new AbilityAim();
            aim.Arm(OffMapAbilityId.HeBarrage);
            Assert.IsTrue(aim.Shape(At, 0, out var disc));
            Assert.IsFalse(disc.Line); Assert.AreEqual(25f, disc.Radius); Assert.AreEqual(At, disc.Start);
            aim.CyclePattern();
            Assert.IsTrue(aim.Shape(At, 0, out var line));
            Assert.IsTrue(line.Line); Assert.AreEqual(60f, line.Length, "before the press: the full length"); Assert.AreEqual(4f, line.HalfWidth);
            Assert.AreEqual(1f, line.Dir.z, 1e-3f, "straight up the field");
            aim.Arm(OffMapAbilityId.StrafeRun);
            aim.Press(At, 1, 0, out _); aim.Drag(At + new Vector3(40f, 0f, 0f));
            Assert.IsTrue(aim.Shape(At + new Vector3(70f, 0f, 9f), 0, out var drag));
            Assert.IsTrue(drag.Line); Assert.AreEqual(At, drag.Start, "from the press, not the cursor"); Assert.AreEqual(1f, drag.Dir.x, 1e-3f); Assert.AreEqual(40f, drag.Length); Assert.AreEqual(3f, drag.HalfWidth);
            Assert.AreEqual(At.x + 40f, drag.End.x, 1e-2f);
            aim.Arm(OffMapAbilityId.CreepingBarrage);
            Assert.IsTrue(aim.Shape(At, 0, out var creeping));
            Assert.AreEqual(6f, creeping.StepMetres, "a tick every lift"); Assert.AreEqual(10f, creeping.HalfWidth);
            aim.Arm(OffMapAbilityId.Beam);
            Assert.IsTrue(aim.Shape(At, 0, out var beam)); Assert.AreEqual(2f, beam.HalfWidth); Assert.AreEqual(60f, beam.Length);
            aim.Cancel();
            Assert.IsFalse(aim.Shape(At, 0, out _));
        }

        static ScreenUnit Unit(float x, float z, bool ours) => new ScreenUnit { World = new Vector3(x, 0f, z), Ours = ours };

        [Test]
        public void TheReadoutCountsACorridorAsACapsule()
        {
            var units = new List<ScreenUnit>
            {
                Unit(20f, 2f, false), Unit(20f, 5f, false), Unit(50f, 0f, false), Unit(-5f, 0f, false), Unit(41f, 0f, false),
                Unit(20f, 7f, true), Unit(20f, 9f, true), Unit(30f, -6f, true),
            };
            var enemy = new List<ScreenUnit>(); var ours = new List<ScreenUnit>();
            AimReadout.TallyLine(units, new Vector2(0f, 0f), new Vector2(1f, 0f), 40f, 3f, 8f, enemy, ours);
            Assert.AreEqual(2, enemy.Count, "inside the corridor: 2 m off the line, and 1 m past its end (the burst reaches)");
            Assert.AreEqual(2, ours.Count, "ours within the reach either side, not beyond it");
            Assert.IsTrue(AimReadout.Radii(OffMapAbilityId.StrafeRun, out float hit, out float reach));
            Assert.AreEqual(3f, hit, "a line's reticle is its half width"); Assert.Greater(reach, hit);
            Assert.IsTrue(AimReadout.Radii(OffMapAbilityId.Beam, out hit, out reach)); Assert.AreEqual(2f, hit);
        }

        [Test]
        public void EverySupportAbilityHasACardAKeyAndStats()
        {
            Assert.AreEqual(6, HudView.SupportAbilities.Length);
            for (int i = 0; i < HudView.SupportAbilities.Length; i++)
            {
                var id = HudView.SupportAbilities[i];
                Assert.IsTrue(OffMapAbilitySystem.TryGetStats((int)id, out _), id + " has no stats");
                Assert.IsNotEmpty(HudText.SupportHotkey(i), id + " has no key on its card");
                var text = HudText.Support(id);
                Assert.IsNotEmpty(text.Name); Assert.IsNotEmpty(text.Card); Assert.IsNotEmpty(text.Portrait);
                Assert.LessOrEqual(text.Tip.Length, 100, id + "'s tooltip would be clipped: " + text.Tip);
                Assert.That(System.Array.IndexOf(TW.UI.SkinSpec.PortraitNames, text.Portrait), Is.GreaterThanOrEqualTo(0), id + "'s card names a portrait the skin does not bake");
            }
            var seen = new HashSet<string>();
            for (int i = 0; i < HudView.SupportAbilities.Length; i++) Assert.IsTrue(seen.Add(HudText.SupportHotkey(i)), "two cards share a key");
        }
    }
}
