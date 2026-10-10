// Phase: B6 (implemented) — the deploy card's tiny film, held without a panel, a sim or a frame.
// What matters about it is a clock: nothing before the plate's delay, ten frames a second after it, and the same
// frame again three seconds later (the goal's three seconds). The rest is the rules about which card plays: a
// locked or a match-over card plays nothing, a poor one does, a card with no sheet stays as it is today, and only
// one card plays at a time. The sheets themselves are held by their shape: 960 x 800, as cardfilm.py bakes them.
using NUnit.Framework;
using Unity.Collections;
using UnityEditor;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim;
using TW.UI;

namespace TW.Tests
{
    public class CardFilmTests
    {
        const float Delay = HudLayout.TooltipDelaySeconds;   // 0.35, the tooltip plate's: film and plate open together

        [Test]
        public void NothingBeforeTheDelayThenFrameZero()
        {
            Assert.That(CardFilm.Frame(0f), Is.EqualTo(-1), "a card under the pointer for no time plays nothing");
            Assert.That(CardFilm.Frame(0.34f), Is.EqualTo(-1), "still nothing a hair before the plate's delay");
            Assert.That(CardFilm.Frame(0.36f), Is.EqualTo(0), "the first frame, a hair after the delay");
        }

        [Test]
        public void TenFramesASecond()
        {
            Assert.That(CardFilm.Fps, Is.EqualTo(10f));
            Assert.That(CardFilm.Frame(Delay + 1f / CardFilm.Fps), Is.EqualTo(1), "one tenth of a second in: frame 1");
            Assert.That(CardFilm.Frame(Delay + 1f), Is.EqualTo(10), "a second in: frame 10");
        }

        [Test]
        public void ThreeSecondsIsTheWholeLoop()
        {
            Assert.That(CardFilm.Seconds, Is.EqualTo(3f), "the goal's three seconds");
            Assert.That(CardFilm.Frames, Is.EqualTo(30));
            Assert.That(CardFilm.Frame(Delay + CardFilm.Seconds), Is.EqualTo(0), "three seconds in, the film is back at its first frame");
            Assert.That(CardFilm.Frame(Delay + CardFilm.Seconds - 0.05f), Is.EqualTo(CardFilm.Frames - 1), "the last frame, just before the loop");
        }

        [Test]
        public void ProgressCountsTheLoopAndNeverFills()
        {
            Assert.That(CardFilm.Progress(0f), Is.EqualTo(0f), "no line before the delay");
            Assert.That(CardFilm.Progress(Delay), Is.EqualTo(0f).Within(1e-5f), "the line starts empty");
            Assert.That(CardFilm.Progress(Delay + 2.99f), Is.LessThan(1f), "the line is never full: it restarts with the film");
            Assert.That(CardFilm.Progress(Delay + 2.99f), Is.GreaterThan(0.99f));
            Assert.That(CardFilm.Progress(Delay + CardFilm.Seconds), Is.EqualTo(0f).Within(1e-5f), "and starts again");
        }

        [Test]
        public void TheMawSheetIsThirtyFramesOfOneSixtyInASixByFiveGrid()
        {
            var sheet = Resources.Load<Texture2D>(CardFilm.FolderPath + "Maw");
            Assert.That(sheet, Is.Not.Null, "Tools/cardfilm.py bakes UI/Resources/CardFilms/Maw.png");
            Assert.That(sheet.width, Is.EqualTo(960), "6 x 160");
            Assert.That(sheet.height, Is.EqualTo(800), "5 x 160");
            Assert.That(CardFilm.Cols * CardFilm.Rows, Is.GreaterThanOrEqualTo(CardFilm.Frames));
            Assert.That(CardFilm.Cols * CardFilm.Side, Is.EqualTo(sheet.width));
            Assert.That(CardFilm.Rows * CardFilm.Side, Is.EqualTo(sheet.height));
        }

        [Test]
        public void NoSheetNoFilm()
        {
            Assert.That(CardFilm.Of("NoSuchUnitEver", 0), Is.Null, "a unit nobody baked a film for plays nothing");
            Assert.That(CardFilm.Of(null, 0), Is.Null);
            Assert.That(CardFilm.Of("Maw", -1), Is.Null);
            Assert.That(CardFilm.Of("Maw", CardFilm.Frames), Is.Null, "no frame past the loop");
        }

        // ---- the player ---------------------------------------------------------------------------------------------

        static CardRefs Card(string filmName)
        {
            var holder = new VisualElement();
            var b = new Button { name = "card" }; holder.Add(b);
            var film = new VisualElement { name = "film" }; var line = new VisualElement { name = "filmline" };
            film.style.display = DisplayStyle.None; line.style.display = DisplayStyle.None;
            b.Add(film); b.Add(line);
            return new CardRefs { Root = b, Film = film, FilmLine = line, FilmName = filmName };
        }

        // These elements never join a panel, so the inline style the player wrote is the whole answer.
        static bool Playing(CardRefs c) => c.Film.style.display.value == DisplayStyle.Flex;

        [Test]
        public void ACardWithNoSheetStaysHidden()
        {
            var c = Card("NoSuchUnitEver");
            var p = new CardFilmPlayer();
            p.Preview(c, 1.5f);
            Assert.That(Playing(c), Is.False, "no sheet keeps the film element display: none");
        }

        [Test]
        public void ALockedOrOverCardPlaysNothingAPoorOnePlays()
        {
            var p = new CardFilmPlayer();

            var locked = Card("Maw"); locked.LastLocked = true;
            p.Preview(locked, 1.5f);
            Assert.That(Playing(locked), Is.False, "a locked card shows its lock, not a film");

            var over = Card("Maw"); over.LastOver = true;
            p.Preview(over, 1.5f);
            Assert.That(Playing(over), Is.False, "the match is over: the bar is grey and still");

            var poor = Card("Maw"); poor.LastPoor = true;
            p.Preview(poor, 1.5f);
            Assert.That(Playing(poor), Is.True, "a card you cannot afford still shows what the unit is, under its shade");
        }

        [Test]
        public void TheFilmFollowsTheTooltipSetting()
        {
            var c = Card("Maw");
            new CardFilmPlayer(() => false).Preview(c, 1.5f);
            Assert.That(Playing(c), Is.False, "tooltips off: no plate and no film");
            new CardFilmPlayer(() => true).Preview(c, 1.5f);
            Assert.That(Playing(c), Is.True);
        }

        [Test]
        public void EnteringASecondCardStopsTheFirst()
        {
            var a = Card("Maw"); var b = Card("Rifleman");
            var p = new CardFilmPlayer();
            p.Preview(a, 1.5f);
            Assert.That(Playing(a), Is.True);
            p.Preview(b, 1.5f);
            Assert.That(Playing(a), Is.False, "one card at a time: entering a card replaces the hovered one");
            Assert.That(Playing(b), Is.True);
            Assert.That(p.Hovered, Is.SameAs(b));
        }

        [Test]
        public void LeavingTheCardStopsIt()
        {
            var c = Card("Maw");
            var p = new CardFilmPlayer();
            p.Preview(c, 1.5f);
            p.Leave(c);
            Assert.That(Playing(c), Is.False);
            Assert.That(p.Hovered, Is.Null);
        }

        [Test]
        public void APlayingCardSaysWhichFrameOfTheLoopItIsOn()
        {
            // what a capture writes into its sidecar: the frame, how far through the loop, and the length.
            var c = Card("Maw");
            var p = new CardFilmPlayer();
            p.Preview(c, 1.5f);
            Assert.That(p.LastFrame, Is.EqualTo(15), "a second and a half into a 10 fps loop is frame 15");
            Assert.That(p.LastProgress, Is.EqualTo(0.5f).Within(0.02f));
            Assert.That(p.Held, Is.EqualTo(HudLayout.TooltipDelaySeconds + 1.5f).Within(0.05f));
            p.Preview(c, 0.2f);
            Assert.That(p.LastFrame, Is.EqualTo(2));
            p.Preview(c, 2.9f);
            Assert.That(p.LastFrame, Is.EqualTo(29), "the last frame before the loop restarts");
        }

        [Test]
        public void APlayingCardWearsTheFilmingClassAndDropsItWhenThePointerLeaves()
        {
            // the skin thickens the cost and key plates over a moving picture (.tw-card.is-filming in
            // dustfront.components.uss): without the class they sit on fire-lit pixels.
            var c = Card("Maw");
            var p = new CardFilmPlayer();
            Assert.That(c.Root.ClassListContains(CardFilmPlayer.PlayingClass), Is.False);
            p.Preview(c, 1.5f);
            Assert.That(c.Root.ClassListContains(CardFilmPlayer.PlayingClass), Is.True);
            p.Leave(c);
            Assert.That(c.Root.ClassListContains(CardFilmPlayer.PlayingClass), Is.False, "the card goes back to its still");
            Assert.That(p.LastFrame, Is.EqualTo(-1));
        }

        [Test]
        public void ACardWithNoFilmNeverWearsTheFilmingClass()
        {
            var c = Card("NoSuchUnitEver");
            new CardFilmPlayer().Preview(c, 1.5f);
            Assert.That(c.Root.ClassListContains(CardFilmPlayer.PlayingClass), Is.False);
        }

        [Test]
        public void EveryBuiltCardKnowsWhichFilmToLookFor()
        {
            var tree = AssetDatabase.LoadAssetAtPath<VisualTreeAsset>("Assets/_Project/UI/Resources/Hud/BattleHud.uxml");
            Assert.That(tree, Is.Not.Null);
            var na = new NativeArray<RosterEntry>(RosterEntry.SlotCount, Allocator.Temp);
            RosterEntry.FillDefault(na, 0);
            var roster = na.ToArray(); na.Dispose();
            var refs = HudView.Build(tree.Instantiate(), null, roster, new[] { 150, 120 });
            foreach (var c in refs.Cards)
            {
                Assert.That(c.Film, Is.Not.Null, "every card the fallback builds carries a film element");
                Assert.That(c.FilmLine, Is.Not.Null);
                Assert.That(c.FilmName, Is.Not.Null.And.Not.Empty, "SetPortrait stores the name CardFilm looks the sheet up under");
            }
            Assert.That(CardFilm.Sheet(refs.Cards[0].FilmName), Is.Not.Null, "the first infantry card has a baked film");
        }
    }
}
