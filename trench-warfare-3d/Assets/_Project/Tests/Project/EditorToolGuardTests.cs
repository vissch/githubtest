// Phase: tooling (code review 2026-10-04, E3 and E4). Two faults of the editor labs that an agent could not see from
// what they answered: TankCapture.Get() looked its DontSave Shooter up with FindFirstObjectByType, which never returns
// one, so every Shot/Follow made another Shooter and a Follow could not be stopped; and out of Play every lab entry
// still answered "queued ..." / "filming ..." while nothing could ever happen.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;

namespace TW.Tests
{
    public class EditorToolGuardTests
    {
        const string OutOfPlay = "not in Play";

        [Test]
        public void TwoCallsShareOneShooter()   // [E3]
        {
            var get = typeof(TankCapture).GetMethod("Get",
                BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public);
            Assert.IsNotNull(get, "[E3] TankCapture.Get is gone");
            var a = get.Invoke(null, null);
            var b = get.Invoke(null, null);
            Assert.AreSame(a, b, "[E3] a second call made a second Shooter");
            // Resources.FindObjectsOfTypeAll does find DontSave objects; FindFirstObjectByType is the one that does not.
            Assert.AreEqual(1, Resources.FindObjectsOfTypeAll<TankCapture.Shooter>().Length,
                "[E3] more than one TankCapture Shooter is alive");
        }

        [Test]
        public void OutOfPlayEveryToolSaysSo()   // [E4]
        {
            var said = new (string what, string answer)[]
            {
                ("CaptureRig.Shot", CaptureRig.Shot("e4.png", 0f, 0f, 20f, 0f)),
                ("CaptureRig.ShotCrowd", CaptureRig.ShotCrowd("e4.png", 20f, 0f)),
                ("CaptureRig.Series", CaptureRig.Series("e4", "e4", 0f, 0f, 20f, 0f, 25f, 2)),
                ("CaptureRig.Profile", CaptureRig.Profile("e4.json", 2)),
                ("TankCapture.Shot", TankCapture.Shot("e4.png")),
                ("TankCapture.Follow", TankCapture.Follow(1)),
                ("RiderLab.Film", RiderLab.Film("e4-film")),
                ("RiderLab.VolleyShot", RiderLab.VolleyShot("e4.png")),
                ("WeightLab.Knob", WeightLab.Knob("tank.weight", "1")),
                ("WeightLab.Trace", WeightLab.Trace(1, "e4.csv", 1f)),
                ("LookLab.Knob", LookLab.Knob("look.lift", "1")),
                ("LookLab.Clear", LookLab.Clear()),
            };
            foreach (var s in said)
                Assert.That(s.answer, Does.Contain(OutOfPlay),
                    "[E4] " + s.what + " answered \"" + s.answer + "\" in edit mode, where nothing can happen");
            Assert.AreEqual("0", CaptureRig.Pending(), "[E4] an edit-mode call queued a shot nobody will ever take");
        }

        [TearDown]
        public void Clean()
        {
            Drop<CaptureRig.Rig>(typeof(CaptureRig), "rig", null);
            Drop<TankCapture.Shooter>(typeof(TankCapture), "shooter", null);
            Drop<RiderLab.Bench>(null, null, typeof(RiderLab.Bench));
            Drop<WeightLab.Bench>(null, null, typeof(WeightLab.Bench));
            Drop<LookLab.Lab>(null, null, typeof(LookLab.Lab));
            if (System.IO.Directory.Exists("e4-film")) System.IO.Directory.Delete("e4-film", true);
        }

        /// <summary>Destroys every object of this kind (DontSave ones too) and nulls the static that holds it.</summary>
        static void Drop<T>(System.Type owner, string field, System.Type instanceHolder) where T : Component
        {
            foreach (var c in Resources.FindObjectsOfTypeAll<T>())
                if (c != null) Object.DestroyImmediate(c.gameObject);
            if (owner != null)
                owner.GetField(field, BindingFlags.Static | BindingFlags.NonPublic | BindingFlags.Public)
                     ?.SetValue(null, null);
            if (instanceHolder != null)
                instanceHolder.GetField("Instance", BindingFlags.Static | BindingFlags.Public)
                              ?.SetValue(null, null);
        }
    }
}
