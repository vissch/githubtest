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

        /// <summary>The rad/s a part is turning at, read off the part over one frame (1/60 s).</summary>
        static float Rate(VehicleRig r, VehicleRig.Part p)
        {
            var was = p.T.localRotation;
            r.Advance(1f / 60f);
            return Quaternion.Angle(was, p.T.localRotation) * Mathf.Deg2Rad * 60f;
        }

        [Test]
        public void TheFanTurnsAtTheGamesSpeed()
        {
            // [Y3] the playground spun a hovercraft's fan at 900-1200 deg/s (15.7-22 rad/s); the game turns it at
            // FanIdle 9 rad/s and never shows it faster than FanMaxShown 14 (TankRenderer.cs:242,257).
            var e = Lib().Vehicles.FirstOrDefault(v => VehicleManifest.Parse(v.Manifest).hover);
            if (e == null) Assert.Ignore("no hovercraft in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                r.CookDelay = -1f;
                var fan = r.Find("Fan");
                if (fan == null) Assert.Ignore("the hovercraft has no fan part");
                for (int f = 0; f < 240; f++) r.Advance(1f / 60f);
                // measured off the fan itself, so the number is what is drawn, not what the drive says
                float idle = Rate(r, fan);
                Assert.That(idle, Is.EqualTo(9f).Within(0.2f), $"[Y3] at rest the fan turns at the game's 9 rad/s, not {idle:0.0}");
                r.Flyer.Speed = 40f;
                float fastest = 0f;
                for (int f = 0; f < 600; f++) { r.Advance(1f / 60f); fastest = Mathf.Max(fastest, Rate(r, fan)); }
                Assert.That(fastest, Is.LessThanOrEqualTo(14.001f), $"[Y3] the fan was shown at {fastest:0.0} rad/s, past the game's cap of 14");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void AMachineIsDrawnFarAtTheGamesDistance()
        {
            // [Y3] the playground picked one of three LODs by screen share; the game draws the near model and, past
            // TankRenderer.DefaultLodDistance (170 m), the far one — two levels by distance.
            var e = Lib().Vehicles.FirstOrDefault();
            if (e == null) Assert.Ignore("no vehicle in the library");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, Vector3.zero, 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                r.CookDelay = -1f;
                var mid = r.Centre;
                Assert.That(r.AutoLod(mid + new Vector3(0f, 0f, 169f)), Is.EqualTo(0), "[Y3] at 169 m the near model is drawn");
                Assert.That(r.AutoLod(mid + new Vector3(0f, 0f, 171f)), Is.EqualTo(1), "[Y3] at 171 m the far model is drawn");
                Assert.That(r.AutoLod(mid + new Vector3(0f, 0f, 600f)), Is.EqualTo(1), "[Y3] there is no third level by distance");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void AWheeledMachineCanBeDriven()
        {
            // [Y3] a machine that is neither walker nor flyer had no drive at all: it stood still whatever `walk` said.
            var e = Lib().Vehicles.FirstOrDefault(v => { var m = VehicleManifest.Parse(v.Manifest); return !m.walker && !m.flyer; });
            if (e == null) Assert.Ignore("every machine in the library walks or flies");
            var parent = new GameObject("test stage").transform;
            try
            {
                var r = VehicleRig.Build(e, null, parent, new Vector3(400f, 0f, 0f), 0f, PlaygroundHost.DrawScale(e.Name, 0f), 3);
                r.CookDelay = -1f;
                Assert.That(r.Ground, Is.Not.Null, $"[Y3] {e.Name} is tracked or wheeled and has no drive");
                r.Ground.Speed = 2.5f;
                for (int f = 0; f < 300; f++) r.Advance(1f / 60f);
                Assert.That(r.Ground.WalkPos.magnitude, Is.GreaterThan(8f), $"[Y3] {e.Name} drove {r.Ground.WalkPos.magnitude:0.0} m in 5 s at 2.5 m/s");
                Assert.That(r.Ground.WheelTurn, Is.Not.EqualTo(0f), "[Y3] its wheels never turned");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }
    }
}
