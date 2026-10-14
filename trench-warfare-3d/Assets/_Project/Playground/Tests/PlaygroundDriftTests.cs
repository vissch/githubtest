// Phase: Playground (2026-10-14, lane/show/review-fixes) — the playground shows what the game shows
// Code review 2026-10-04, unit 27 (Y1-Y5): the playground scored behaviour the game does not have. Each test here is
// tagged with the finding it guards and asserts the game's own number, taken from the review or from the game's table,
// never from what the playground happened to do.
using System.Linq;
using NUnit.Framework;
using TW.Playground;
using TW.Playground.Editor;
using UnityEditor;
using UnityEngine.TestTools;
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

        static int ClipOf(string file)
        {
            var l = Lib();
            for (int i = 0; i < l.Clips.Length; i++) if (l.Clips[i].Name == file) return i;
            return -1;
        }

        [Test]
        public void TheLoopFlagAndRootComeFromTheGamesTable()
        {
            // [Y4] the playground guessed loop/once and the root travel from the file's name: every "Firing*" and every
            // "*Idle*" looped, every one-shot kept its travel. The game reads its own table (Editor/InfantryClipTable.cs):
            // "Firing Rifle (1)" and "Rifle Idle (3)" are one-shots (O(Clip.FireStand, ...), O(Clip.FidgetLookAround, ...)),
            // and the root travel is stripped on every clip but the deaths marked keepRoot (D(..., keepRoot: true)).
            var l = Lib();
            int fire = ClipOf("Firing Rifle (1)"), fidget = ClipOf("Rifle Idle (3)");
            if (fire < 0 || fidget < 0) Assert.Ignore("the clip folder has no Firing Rifle (1) / Rifle Idle (3)");
            Assert.That(l.Clips[fire].Loop, Is.False, "[Y4] Firing Rifle (1) is a one-shot in the game's table");
            Assert.That(l.Clips[fidget].Loop, Is.False, "[Y4] Rifle Idle (3) is a one-shot in the game's table");

            int walking = ClipOf("Walking To Dying"), front = ClipOf("Death From The Front");
            if (walking < 0 || front < 0) Assert.Ignore("the clip folder has no Walking To Dying / Death From The Front");
            if (l.Units.Length == 0) Assert.Ignore("no unit in the library");
            var parent = new GameObject("test stage").transform;
            ClipDeck deck = null;
            try
            {
                deck = ClipDeck.Make(l, parent);
                if (deck == null) Assert.Ignore("the library has no ClipSource");
                var u = UnitRig.Build(l.Units[0], deck, null, parent, Vector3.zero, 0f, 1f);
                // the figure is a different size from the rig the clip was authored on: its travel comes over scaled by
                // the hip ratio Retarget uses (ClipDeck.Pose), so the clip's own travel is measured and scaled the same way
                float hip = u.Skel.HipsRest.y / deck.ClipHipHeight;
                float ownWalking = Own(l, parent, walking) * hip, ownFront = Own(l, parent, front) * hip;
                float drawnWalking = Carry(deck, u, walking), drawnFront = Carry(deck, u, front);
                Assert.That(drawnFront, Is.GreaterThan(0.8f * ownFront),
                    $"[Y4] Death From The Front keeps its root: it should fall {ownFront:0.00} m from where he stood, drawn {drawnFront:0.00} m");
                Assert.That(drawnWalking, Is.LessThan(0.5f * ownWalking),
                    $"[Y4] Walking To Dying has its travel stripped: of the clip's {ownWalking:0.00} m the playground still carried {drawnWalking:0.00} m");
            }
            finally { if (deck != null) deck.Destroy(); Object.DestroyImmediate(parent.gameObject); }
        }

        /// <summary>The clip file's own hip travel over the window the playground plays, on the rig it was authored on.</summary>
        static float Own(PlaygroundLibrary l, Transform parent, int clip)
        {
            var go = Object.Instantiate(l.ClipSource, parent);
            try
            {
                foreach (var a in go.GetComponentsInChildren<Animator>(true)) a.enabled = false;
                var skel = Retarget.Skeleton.Of(go.transform);
                var c = l.Clips[clip].Clip;
                c.SampleAnimation(go, 0f); var p0 = skel.HipsNow; p0.y = 0f;
                c.SampleAnimation(go, c.length); var p1 = skel.HipsNow; p1.y = 0f;
                return Vector3.Distance(p0, p1);
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>How far the hips move along the ground from a clip's start to its last frame, as drawn.</summary>
        static float Carry(ClipDeck deck, UnitRig u, int clip)
        {
            deck.Pose(u, clip, 0f, 1f, null);
            var a = u.Skel.Bones[0].position; a.y = 0f;
            deck.Pose(u, clip, 100f, 1f, null);   // past the end: a one-shot holds its last frame
            var b = u.Skel.Bones[0].position; b.y = 0f;
            return Vector3.Distance(a, b);
        }

        [Test]
        public void TheCutAndRateComeFromTheGamesTable()
        {
            // [Y4] clips played whole and at rate 1. The game cuts a window out of the file and plays it at its own rate:
            // "Rifle Idle (3)" is cut at 5.3 s and "Reloading" runs at 1.5x (Editor/InfantryClipTable.cs, the fidget and
            // ReloadStand rows), so a reload takes the file's length over 1.5.
            var l = Lib();
            int fidget = ClipOf("Rifle Idle (3)"), reload = ClipOf("Reloading");
            if (fidget < 0 || reload < 0) Assert.Ignore("the clip folder has no Rifle Idle (3) / Reloading");
            Assert.That(l.Clips[fidget].CutEnd, Is.EqualTo(5.3f).Within(1e-3f), "[Y4] Rifle Idle (3) is cut at the game's 5.3 s");
            Assert.That(l.Clips[reload].Rate, Is.EqualTo(1.5f).Within(1e-3f), "[Y4] a reload runs at the game's 1.5x");
            var parent = new GameObject("test stage").transform;
            ClipDeck deck = null;
            try
            {
                deck = ClipDeck.Make(l, parent);
                if (deck == null) Assert.Ignore("the library has no ClipSource");
                float want = l.Clips[reload].Clip.length / 1.5f;
                Assert.That(deck.ClipLength(reload), Is.EqualTo(want).Within(1e-3f), $"[Y4] the reload should play for {want:0.00} s");
            }
            finally { if (deck != null) deck.Destroy(); Object.DestroyImmediate(parent.gameObject); }
        }
        [Test]
        public void AShelledBuildingKeepsNoMorePiecesLooseThanTheGame()
        {
            // [Y5] the playground threw every broken chunk, however many; the game caps the chunks in the air or lying at
            // 48 (PropDestruction.MaxLoose) and past it a chunk goes to rubble on the spot. The Gasholder has 65 chunks in
            // Resources/Env/Ruins/houses.json, so a heavy shelling can pass the cap. The 48 is the game's number, written
            // out here so this test compiles on the old code.
            var parent = new GameObject("test stage").transform;
            try
            {
                var b = BuildingRig.Build("Ruins", "Gasholder", null, parent, Vector3.zero, 0f, 11);
                Assert.That(b, Is.Not.Null, "the Ruins set has the Gasholder");
                Assert.That(b.Pieces.Count, Is.GreaterThan(48), "the Gasholder has more chunks than the cap");
                for (int k = 0; k < 24; k++)
                {
                    float ang = k * 2.39996f, r = b.Radius * 0.55f;
                    b.ShellLocal(new Vector3(Mathf.Cos(ang) * r, 1f + (k % 5) * 1.2f, Mathf.Sin(ang) * r), 200f);
                    for (int f = 0; f < 45; f++) b.Advance(1f / 60f);
                    Assert.That(b.Pieces.Count(p => p.Loose), Is.LessThanOrEqualTo(48),
                                "[Y5] shell " + (k + 1) + ": more chunks loose at once than the game's 48");
                }
                for (int f = 0; f < 1200; f++) b.Advance(1f / 60f);
                Assert.That(b.Pieces.Count(p => p.Loose), Is.LessThanOrEqualTo(48),
                            "[Y5] settled: more chunks loose at once than the game's 48");
                Assert.That(b.Standing, Is.LessThan(b.Pieces.Count / 2), "[Y5] 24 heavy shells bring over half the house down");
            }
            finally { Object.DestroyImmediate(parent.gameObject); }
        }

        [Test]
        public void ATypoInACommandIsRefused()
        {
            // [Y6] an unknown name used to answer "ok", so a typo in a capture round scored a picture of something else.
            // The refused lines are the finding's own list of typos; the accepted ones are the spellings the rounds use.
            LogAssert.ignoreFailingMessages = true;   // "set Foo": HouseKit logs that no such set exists, which is the point
            foreach (var line in new[] { "biome Foo", "set Foo", "house Nope", "cam 0 2 0" })
                Assert.That(PlaygroundHost.Reject(line, "Ruins"), Is.Not.Empty, "[Y6] \"" + line + "\" must be refused, not answered ok");
            foreach (var line in new[] { "biome NightMud", "vehicle", "cam standard", "cam 0 2 0 0 20 24 35", "set Ruins" })
                Assert.That(PlaygroundHost.Reject(line, "Ruins"), Is.Empty, "[Y6] \"" + line + "\" is a good line and must run");
            LogAssert.ignoreFailingMessages = false;
        }

        [Test]
        public void TheResultOfAQueuedCommandComesBack()
        {
            // [Y6] q() always said "queued" and the real result went to a Status field nobody read.
            var two = new[] { "ok vehicle", "no biome Foo" };
            var both = PlaygroundHost.Join(two, 0, 2);
            Assert.That(both, Does.StartWith("ERR"), "[Y6] a bad line among the results must come back as an error");
            Assert.That(both, Does.Contain("ok vehicle").And.Contain("no biome Foo"), "[Y6] every result comes back");
            Assert.That(PlaygroundHost.Join(two, 0, 3), Is.EqualTo("PENDING"), "[Y6] a command that has not run yet is PENDING");
            Assert.That(PlaygroundHost.Join(new[] { "ok vehicle" }, 0, 1), Is.EqualTo("ok vehicle"), "[Y6] a good line comes back as it is");
        }
    }
}
