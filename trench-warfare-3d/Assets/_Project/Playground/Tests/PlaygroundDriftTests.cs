// Phase: Playground (2026-10-14, lane/show/review-fixes) — the playground shows what the game shows
// Code review 2026-10-04, unit 27 (Y1-Y5): the playground scored behaviour the game does not have. Each test here is
// tagged with the finding it guards and asserts the game's own number, taken from the review or from the game's table,
// never from what the playground happened to do.
using System.Linq;
using NUnit.Framework;
using TW.Playground;
using TW.Playground.Editor;
using UnityEditor;
using UnityEngine;

namespace TW.Tests.Playground
{
    public sealed class PlaygroundDriftTests
    {
        static PlaygroundLibrary lib;
        static PlaygroundLibrary Lib()
        {
            if (lib == null) { PlaygroundSetup.Build(); lib = AssetDatabase.LoadAssetAtPath<PlaygroundLibrary>(PlaygroundSetup.LibraryPath); }
            return lib;
        }

        [Test]
        public void TheSkimmerIsDrawnAtTheGamesScale()
        {
            // [Y1] the playground drew every machine 1.7 times its file, so the Skimmer measured about 12 m where the game
            // draws it 7 m across its pods: the mech- and hover-split models are written in metres and drawn at 1.
            Assert.That(PlaygroundHost.DrawScale("Skimmer", 0f), Is.EqualTo(1f).Within(1e-4f), "[Y1] the Skimmer is drawn at 1 in the game");
            Assert.That(PlaygroundHost.DrawScale("Croaker", 0f), Is.EqualTo(1f).Within(1e-4f), "[Y1] the Croaker is drawn at 1 in the game");
            Assert.That(PlaygroundHost.DrawScale("Skimmer", 1.4f), Is.EqualTo(1.4f).Within(1e-4f), "[Y1] an explicit size still overrides the table");

            var parent = new GameObject("test stage").transform;
            try
            {
                foreach (var e in Lib().Vehicles)
                {
                    var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                    r.CookDelay = -1f;
                    var hull = r.Find("Hull") ?? r.Find("Body");
                    if (hull == null) continue;
                    var d = hull.Box.size * r.Size;
                    float longest = Mathf.Max(d.x, Mathf.Max(d.y, d.z));
                    Assert.That(longest, Is.LessThan(8f), $"[Y1] {e.Name} is drawn {longest:0.0} m long, over the game's size");
                }
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        static PlaygroundLibrary.VehicleEntry Walker() => Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).walker);

        [Test]
        public void TheWalkerTakesTheGamesSwingAndArc()
        {
            // [Y2] the playground built a bare WalkerGait (swing and arc 1), where the game builds it with the machine's
            // drive style: the Croaker swings 0.9 and arcs 1.5 (TankRenderer.DriveStyle.cs:86).
            var e = Walker();
            if (e == null) Assert.Ignore("no walker in the library");
            if (e.Name != "Croaker") Assert.Ignore("the library's walker is not the Croaker: " + e.Name);
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                r.CookDelay = -1f;
                Assert.That(r.Walker.GaitSwing, Is.EqualTo(0.9f).Within(1e-4f), "[Y2] the Croaker's swing is the game's 0.9");
                Assert.That(r.Walker.GaitArc, Is.EqualTo(1.5f).Within(1e-4f), "[Y2] the Croaker's arc is the game's 1.5");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void ADeadWalkersFeetSlideOut()
        {
            // [Y2] a dead walker kept the feet it stood on at death; the game's gait skids a collapsing walker's feet
            // outward, and the playground now shows the same slide (the body is still held up by the fold clamp).
            var e = Walker();
            if (e == null) Assert.Ignore("no walker in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, new Vector3(200f, 0f, 0f), 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                r.CookDelay = -1f; r.Walker.Speed = 2.5f;
                for (int f = 0; f < 120; f++) r.Advance(1f / 60f);
                r.KnockOut();
                r.Advance(1f / 60f);
                float before = Vector3.Distance(r.Walker.FootPlace(0), r.Walker.FootPlace(1));
                for (int f = 0; f < 120; f++) r.Advance(1f / 60f);
                float after = Vector3.Distance(r.Walker.FootPlace(0), r.Walker.FootPlace(1));
                Assert.That(after - before, Is.GreaterThan(0.2f), $"[Y2] a dead walker's feet stayed put ({before:0.00} m to {after:0.00} m apart)");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
    }
}
