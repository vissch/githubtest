// Phase: B6 (implemented) — the key table is complete, unambiguous and survives a round trip through settings.json.
// A binding with no key is an action the player cannot reach; two actions on one primary key is a fight the player
// loses; a JSON that comes back different is a settings screen that lies. Each is one assertion here.
using NUnit.Framework;
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Presentation;

namespace TW.Tests
{
    public class KeyMapTests
    {
        [Test]
        public void EveryActionHasAPrimaryKey()
        {
            var d = KeyMap.Defaults();
            for (int i = 0; i < KeyMap.ActionCount; i++)
                Assert.That(d.Primary[i], Is.Not.EqualTo(Key.None), $"{(GameAction)i} has no default key");
        }

        [Test]
        public void NoTwoActionsShareAPrimaryKey()
        {
            var d = KeyMap.Defaults();
            for (int i = 0; i < KeyMap.ActionCount; i++)
                for (int j = i + 1; j < KeyMap.ActionCount; j++)
                    Assert.That(d.Primary[i], Is.Not.EqualTo(d.Primary[j]),
                        $"{(GameAction)i} and {(GameAction)j} both default to {d.Primary[i]}");
        }

        [Test]
        public void SecondaryKeysDoNotShadowAnotherPrimary()
        {
            var d = KeyMap.Defaults();
            for (int i = 0; i < KeyMap.ActionCount; i++)
            {
                if (d.Secondary[i] == Key.None) continue;
                for (int j = 0; j < KeyMap.ActionCount; j++)
                    if (j != i) Assert.That(d.Secondary[i], Is.Not.EqualTo(d.Primary[j]),
                        $"{(GameAction)i}'s secondary {d.Secondary[i]} is {(GameAction)j}'s primary");
            }
        }

        [Test]
        public void SpaceIsTheTacticalPauseAndEscapeIsTheMenu()
        {
            var d = KeyMap.Defaults();
            Assert.That(d.Primary[(int)GameAction.TacticalPause], Is.EqualTo(Key.Space), "Dust Front convention: Space pauses");
            Assert.That(d.Primary[(int)GameAction.Menu], Is.EqualTo(Key.Escape));
            Assert.That(KeyMap.Rebindable(GameAction.Menu), Is.False, "Esc must always open the menu");
        }

        [Test]
        public void DeploySlotsAreTheDigitsInOrder()
        {
            var d = KeyMap.Defaults();
            Key[] digits = { Key.Digit1, Key.Digit2, Key.Digit3, Key.Digit4, Key.Digit5, Key.Digit6, Key.Digit7, Key.Digit8 };
            for (int s = 0; s < 8; s++)
                Assert.That(d.Primary[(int)GameAction.Deploy1 + s], Is.EqualTo(digits[s]), $"deploy slot {s + 1}");
        }

        [Test]
        public void EveryKeyHasADisplayName()
        {
            var d = KeyMap.Defaults();
            for (int i = 0; i < KeyMap.ActionCount; i++)
            {
                Assert.That(KeyMap.Display(d.Primary[i]), Is.Not.Empty, $"{(GameAction)i}: {d.Primary[i]} has no display name");
                Assert.That(KeyMap.Label((GameAction)i), Is.Not.Empty, $"{(GameAction)i} has no label");
                Assert.That(KeyMap.Label((GameAction)i), Is.EqualTo(KeyMap.Label((GameAction)i).ToUpperInvariant()), "labels are uppercase: USS has no text-transform");
            }
            Assert.That(KeyMap.Display(Key.Space), Is.EqualTo("SPACE"));
            Assert.That(KeyMap.Display(Key.Digit1), Is.EqualTo("1"));
            Assert.That(KeyMap.Display(Key.F1), Is.EqualTo("F1"));
            Assert.That(KeyMap.Display(Key.None), Is.Empty);
        }

        [Test]
        public void BindingsSurviveJson()
        {
            var d = KeyMap.Defaults();
            var back = JsonUtility.FromJson<KeyMap.Bindings>(JsonUtility.ToJson(d));
            back.Normalise();
            for (int i = 0; i < KeyMap.ActionCount; i++)
            {
                Assert.That(back.Primary[i], Is.EqualTo(d.Primary[i]), $"{(GameAction)i} primary");
                Assert.That(back.Secondary[i], Is.EqualTo(d.Secondary[i]), $"{(GameAction)i} secondary");
            }
        }

        [Test]
        public void ShortArraysFromAnOlderBuildAreResized()
        {
            var b = new KeyMap.Bindings { Primary = new[] { Key.W, Key.S }, Secondary = null };
            b.Normalise();
            Assert.That(b.Primary.Length, Is.EqualTo(KeyMap.ActionCount));
            Assert.That(b.Secondary.Length, Is.EqualTo(KeyMap.ActionCount));
            Assert.That(b.Primary[0], Is.EqualTo(Key.W));
            Assert.That(b.Primary[2], Is.EqualTo(Key.None));
        }

        [Test]
        public void BoundToFindsConflictsAndSkipsTheActionBeingRebound()
        {
            var saved = KeyMap.Current;
            try
            {
                KeyMap.Current = KeyMap.Defaults();
                Assert.That(KeyMap.BoundTo(Key.Space), Is.EqualTo(GameAction.TacticalPause));
                Assert.That(KeyMap.BoundTo(Key.UpArrow), Is.EqualTo(GameAction.PanUp), "secondary keys count as bound");
                Assert.That(KeyMap.BoundTo(Key.Space, except: GameAction.TacticalPause), Is.Null);
                Assert.That(KeyMap.BoundTo(Key.Numpad5), Is.Null);
                KeyMap.Set(GameAction.Advance, Key.Numpad5);
                Assert.That(KeyMap.BoundTo(Key.Numpad5), Is.EqualTo(GameAction.Advance));
            }
            finally { KeyMap.Current = saved; }
        }
    }
}
