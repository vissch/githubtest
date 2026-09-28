// Phase: A4 wrecks (2026-09-28, the seam) — a wreck breaks in stages and then is gone (owner, 2026-09-28): whole wreck
// (blocks, 50 % cover), broken wreck (blocks, 35 %), scrap (does not block, 15 %), cleared (nothing). This file holds
// the stages' rules; the steps that make wrecks take harm add their tests here.
using NUnit.Framework;
using TW.Sim;
using TW.Sim.Terrain;

namespace TW.Tests
{
    public class WreckDecayTests
    {
        [Test]
        public void TheNewStagesHaveTheRulesTheOwnerChose()
        {
            Assert.AreEqual(PropKind.BrokenWreck, PropRules.Next(PropKind.Wreck));
            Assert.AreEqual(PropKind.Scrap, PropRules.Next(PropKind.BrokenWreck));
            Assert.AreEqual(PropKind.Cleared, PropRules.Next(PropKind.Scrap));
            Assert.AreEqual(PropKind.Cleared, PropRules.Next(PropKind.Cleared), "cleared is the end");

            Assert.AreEqual(50, PropRules.CoverPercent(PropKind.Wreck));
            Assert.AreEqual(35, PropRules.CoverPercent(PropKind.BrokenWreck));
            Assert.AreEqual(15, PropRules.CoverPercent(PropKind.Scrap));
            Assert.AreEqual(0, PropRules.CoverPercent(PropKind.Cleared));

            Assert.IsTrue(PropRules.Blocks(PropKind.Wreck));
            Assert.IsTrue(PropRules.Blocks(PropKind.BrokenWreck), "a broken wreck still blocks");
            Assert.IsFalse(PropRules.Blocks(PropKind.Scrap), "men and machines go over a scrap pile");
            Assert.IsFalse(PropRules.Blocks(PropKind.Cleared));

            foreach (var k in new[] { PropKind.Wreck, PropKind.BrokenWreck, PropKind.Scrap }) Assert.IsTrue(PropRules.IsWreckage(k), k.ToString());
            foreach (var k in new[] { PropKind.Tree, PropKind.BrokenTree, PropKind.Stump, PropKind.Log, PropKind.Bridge, PropKind.Cleared }) Assert.IsFalse(PropRules.IsWreckage(k), k.ToString());

            // the trees are as they were
            Assert.AreEqual(PropKind.BrokenTree, PropRules.Next(PropKind.Tree));
            Assert.AreEqual(PropKind.Stump, PropRules.Next(PropKind.BrokenTree));
            Assert.AreEqual(PropKind.Stump, PropRules.Next(PropKind.Stump));
        }

        [Test]
        public void TheKindsAndTheEventAreAppendedNotInserted()
        {
            // hashed as numbers and read by the picture: an insert would renumber everything after it
            Assert.AreEqual(4, (int)PropKind.Wreck); Assert.AreEqual(5, (int)PropKind.Bridge);
            Assert.AreEqual(6, (int)PropKind.BrokenWreck); Assert.AreEqual(7, (int)PropKind.Scrap); Assert.AreEqual(8, (int)PropKind.Cleared);
            Assert.AreEqual((int)SimEventType.RocketFired + 1, (int)SimEventType.PropWorn);
        }

        [Test]
        public void AWrecksSizeFollowsItsMachine()
        {
            Assert.AreEqual(1.5f, PropRules.WreckSize(3600f), 1e-5f, "the Maw: as big as it gets");
            Assert.AreEqual(2000f / PropRules.WreckSizeHp, PropRules.WreckSize(2000f), 1e-5f, "the Tusk");
            Assert.AreEqual(PropRules.WreckSizeMin, PropRules.WreckSize(100f), "a test's 100 hp tank is not a wreck of nothing");
            Assert.AreEqual(1f, PropRules.WreckSize(PropRules.WreckSizeHp), 1e-5f);
        }
    }
}
