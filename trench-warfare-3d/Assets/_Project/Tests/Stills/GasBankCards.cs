// Phase: ability-fx-captures-r2-2 (2026-10-19, the owner's call (a): a capture-path fault) — the CHEAP ISOLATION
// STILL for "133 submitted GasBank cards leave no pixels in a batch-mode Camera.Render". GasBankStills drives the
// sim, the ability, the gas field, the bank cache and the book all at once, so a blank still accuses all of them.
// This test cuts every one of them out: its own FlipbookFx, three hand-packed cards at known places, no sim call and
// no gas field, submitted from an ORDINARY MonoBehaviour.Update (so before CaptureRig's own LateUpdate at 30000),
// and ONE STILL PER BOOK from the same pose at the same spot, plus a plate with nothing drawn. One card to a picture,
// so a missing card accuses that book alone and no card can hide behind another:
//   - all three blank      -> the capture path misses every flipbook, whatever the book;
//   - only the gas blank   -> the GasBank row's own numbers, or the gas card's size, squash and anchor;
//   - all three there      -> the fault is upstream of the book: the field, the cache or the recipe switch.
// docs/evidence/ability-fx-captures-r2/sheetink.py --blobs counts the changed pixels of a still against the plate.
using System.Collections;
using System.Collections.Generic;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using Unity.Mathematics;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using TW.UI;

namespace TW.Tests
{
    public class GasBankCards
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        /// <summary>Every card is this wide; the gas one is squashed by CombatFx.GasBankSquashFor, as the bank is.</summary>
        const float CardWidth = 12f;
        /// <summary>The frame of each book that is drawn: inside GasBankFirst 17 .. GasBankLast 28, where the sheet's
        /// ink is (sheetink.py: mean alpha 0.18 at frames 17-28 of GasBank.png).</summary>
        const float CardFrame = 22f;

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Submits one hand-packed card per book from Update, at the default execution order, so the cards
        /// are in the frame CaptureRig photographs from its own LateUpdate (order 30000).</summary>
        sealed class Drawer : MonoBehaviour
        {
            public FlipbookFx Books;
            public SimHost Source;
            public Vector3 At;
            /// <summary>Which of Which_Books is drawn this frame; -1 draws nothing (the plate).</summary>
            public int Which = -1;
            public int LastDraws;
            readonly List<Matrix4x4> one = new List<Matrix4x4>(1);

            /// <summary>The three books the still set walks through: the gas bank under suspicion, a second Deep bank
            /// book, and an ordinary card. The gas one is squashed as the real bank is.</summary>
            public static readonly FlipbookFx.Book[] Which_Books =
                { FlipbookFx.Book.GasBank, FlipbookFx.Book.SmokeBank, FlipbookFx.Book.Puff };
            public static readonly string[] Which_Names = { "gas", "smoke", "puff" };

            void Update()
            {
                if (Which < 0 || Books == null || !Books.Ready || Source == null || Source.Local == null) return;
                var map = Source.Local.Map;
                var size = map.SizeMeters;
                var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f),
                                        new Vector3(size.x + 20f, 60f, size.y + 20f));
                float height = Which == 0 ? CardWidth * CombatFx.GasBankSquashFor(CardWidth, 1) : CardWidth;
                one.Clear();
                float y = RenderGround.Sample(map, At.x, At.z) - 0.35f;   // the gas bank's own anchor
                one.Add(FlipbookFx.Pack(new Vector3(At.x, y, At.z), CardWidth, height, CardFrame, 1f, 1f, 0f,
                                        FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, 1f));
                Books.DrawPacked(Which_Books[Which], one, bounds);
                LastDraws = FrameBudget.DrawCalls;
            }
        }

        [UnityTest, Explicit("One still of three hand-packed flipbook cards and its plate; run by name, with a graphics device.")]
        public IEnumerator ThreeHandPackedCardsAndThePlate()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            yield return Shoot();     // a fresh enumerator after the reload (WalkerStills' lesson)
        }

        static IEnumerator Shoot()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");

            string dir = SessionState.GetString("tw.gasbank.dir", "");
            if (string.IsNullOrEmpty(dir)) dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-gasbank");
            Directory.CreateDirectory(dir);
            TestContext.Out.WriteLine("cards into " + dir);

            CombatFx.ShowOverlays = false;
            CameraShake.Strength = 0f; CameraShake.Reset();
            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 60; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;   // the owner's night look, as GasBankStills shoots
            Time.captureDeltaTime = 1f / 30f;

            var tc = Object.FindFirstObjectByType<TacticalCamera>();
            Assert.That(tc, Is.Not.Null, "no TacticalCamera in the scene");
            var map = Host.Local.Map;
            var size = map.SizeMeters;

            // the open middle of the field, the patch GasBankStills gasses, so the two sets are comparable
            float x = -1f, z = -1f, nearest = float.MaxValue;
            for (float zz = 30f; zz < size.y - 30f; zz += 4f)
                for (float xx = 12f; xx < size.x - 12f; xx += 4f)
                {
                    if (!Open(map, xx, zz, 10f)) continue;
                    float d = (xx - size.x * 0.5f) * (xx - size.x * 0.5f) + (zz - size.y * 0.5f) * (zz - size.y * 0.5f);
                    if (d < nearest) { nearest = d; x = xx; z = zz; }
                }
            Assert.Greater(x, 0f, "no open ground for the cards on this map");
            TestContext.Out.WriteLine("cards centre " + x + ", " + z);

            var go = new GameObject("TW GasBankCards drawer");
            var drawer = go.AddComponent<Drawer>();
            var books = new FlipbookFx();
            bool ready = books.Ready;
            if (ready)
            {
                drawer.Books = books;
                drawer.Source = Host;
                drawer.At = new Vector3(x, 0f, z);

                tc.BaseYaw = -90f; tc.Zoom = 30f; tc.Focus = new Vector2(x, z);
                for (int f = 0; f < 30; f++) yield return null;

                for (int b = 0; b < Drawer.Which_Books.Length; b++)
                {
                    drawer.Which = b;
                    for (int f = 0; f < 10; f++) yield return null;
                    yield return One(dir, "cards-" + Drawer.Which_Names[b], x, z, drawer);
                }
                drawer.Which = -1;
                for (int f = 0; f < 10; f++) yield return null;
                yield return One(dir, "cards-plate", x, z, drawer);
            }

            Object.Destroy(go);
            books.Dispose();            // or the material leak trips StaticLifecycleTests in the gate
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            CameraShake.Strength = 1f;  // a player setting: put it back
            CombatFx.ShowOverlays = true;
            yield return new ExitPlayMode();

            Assert.That(ready, Is.True, "the flipbook sheets did not load: no card of any book can be drawn");
        }

        static IEnumerator One(string dir, string stem, float x, float z, Drawer drawer)
        {
            string path = Path.Combine(dir, stem + ".png");
            CaptureRig.Shot(path, x, z, 30f, 40f, 25f, 1280, 720);
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            Assert.That(CaptureRig.Pending(), Is.EqualTo("0"), "the capture queue stalled");
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
            Assert.That(File.Exists(path), Is.True, stem + ".png was never written");
            TestContext.Out.WriteLine(stem + ": FrameBudget.DrawCalls " + FrameBudget.DrawCalls
                                      + ", the drawer's own last frame " + drawer.LastDraws
                                      + ", book drawn " + (drawer.Which < 0 ? "none" : Drawer.Which_Names[drawer.Which]));
        }

        /// <summary>Open ground for a card standing at (x, z): no trench, wire, bunker or blocked cell within r, and
        /// no prop (GasBankStills.Open, without the static-cover sweep a bank needs).</summary>
        static bool Open(MapData map, float x, float z, float r)
        {
            var size = map.SizeMeters;
            for (float zz = z - r; zz <= z + r; zz += 2f)
                for (float xx = x - r; xx <= x + r; xx += 2f)
                {
                    if (xx < 3f || zz < 3f || xx > size.x - 3f || zz > size.y - 3f) return false;
                    var layer = map.LayerAt(new float3(xx, 0f, zz));
                    if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire | NavLayer.Bunker)) != 0) return false;
                }
            for (int i = 0; i < map.Props.Length; i++)
            {
                var q = map.Props[i].Pos;
                if (math.abs(q.x - x) < r && math.abs(q.z - z) < r) return false;
            }
            return true;
        }
    }
}
