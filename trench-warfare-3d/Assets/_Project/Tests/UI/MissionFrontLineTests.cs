// Phase: bug-catcher sweep 20261108-2 — MissionCard.FrontLine is hand-typed text (e.g. "90 x 240 M"), never
// checked against the battlefield the card actually generates. This walks every shipped card and asserts the
// string matches the real NavWidth/NavLength of BattlefieldGenerator.Create(MatchLaunch.Field(card.Ground, seed)),
// so a future preset tweak cannot silently make the mission select panel lie to the player.
using System.Text.RegularExpressions;
using NUnit.Framework;
using Unity.Collections;
using UnityEditor;
using TW.Presentation;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class MissionFrontLineTests
    {
        static readonly Regex Shape = new Regex(@"^\d+ x \d+ M$");

        [Test]
        public void EveryCardsFrontLineMatchesTheMapItGenerates()
        {
            var assets = AssetDatabase.LoadAssetAtPath<ShellAssets>("Assets/_Project/UI/Resources/ShellAssets.asset");
            Assert.That(assets, Is.Not.Null, "ShellAssets.asset missing (TW/UI/Build Shell Assets)");
            Assert.That(assets.Catalog, Is.Not.Null);
            Assert.That(assets.Catalog.Cards, Is.Not.Null.And.Not.Empty);

            foreach (var c in assets.Catalog.Cards)
            {
                if (!c.GeneratedBattlefield)
                {
                    TestContext.WriteLine($"{c.Title}: GeneratedBattlefield is off (uses the playtest map), skipped.");
                    continue;
                }

                Assert.That(Shape.IsMatch(c.FrontLine), Is.True,
                    $"{c.Title}: FrontLine '{c.FrontLine}' is not of the form 'W x L M'");

                using var map = BattlefieldGenerator.Create(MatchLaunch.Field(c.Ground, c.BattlefieldSeed), Allocator.Persistent);
                string expected = $"{map.NavWidth * MapData.NavCellSize:0} x {map.NavLength * MapData.NavCellSize:0} M";
                Assert.That(c.FrontLine, Is.EqualTo(expected),
                    $"{c.Title}: the panel says '{c.FrontLine}' but the map it generates is {expected}");

                // the stat is stored once per card rather than computed live, which is only honest while the
                // generated size does not depend on the seed - pin that so a preset change that breaks it is caught here.
                using var otherSeed = BattlefieldGenerator.Create(MatchLaunch.Field(c.Ground, c.BattlefieldSeed + 1), Allocator.Persistent);
                Assert.That(otherSeed.NavWidth, Is.EqualTo(map.NavWidth), $"{c.Title}: map size must not depend on the seed for a static FrontLine string to stay true");
                Assert.That(otherSeed.NavLength, Is.EqualTo(map.NavLength), $"{c.Title}: map size must not depend on the seed for a static FrontLine string to stay true");
            }
        }
    }
}
