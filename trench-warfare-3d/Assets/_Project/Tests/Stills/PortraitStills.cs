// Phase: readability (2026-10-19, special-portraits-r3) - the two stills a card portrait is cut from.
// The Flamer's and the Sentry's portraits (2026-10-19, special-portraits-r2). Both files were byte copies of
// Shield.png while the commits called them "his own portrait". A card picture has to be made from something, so this
// test photographs each man alone in GreyboxCorridor at the game's own near zoom, where units.Sockets(slot, ...)
// exists and CombatFx.DrawKit / DrawPlate actually draw his tanks, his wand and his plate, and then photographs the
// same empty ground from the same pose. The pair is what docs/evidence/special-portraits-r2/portrait_cut.py turns
// into a cutout with an alpha: man minus ground.
//
// Run it by name WITH a graphics device (never -nographics), the way UnitCloseStills is run:
//   Unity.exe -batchmode -projectPath <project> -runTests -testPlatform EditMode -testFilter PortraitStills \
//             -testResults <out.xml> -logFile <out.log>
// Environment: TW_STILLS_DIR where the stills go (default %TEMP%/tw-portraits: never inside the checkout).
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Units;
using TW.Sim;
using TW.Sim.Match;
using TW.UI;

namespace TW.Tests
{
    public class PortraitStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";
        // Zoom 9 is clamped up to TacticalCamera.NearZoom (~15 m) by CaptureRig.Pose; that is still the near tier,
        // which is the point: without sockets the kit is not drawn and the portrait would be a bare soldier.
        const float Zoom = 9f, Pitch = 35f;
        const int Side = 1024;
        const int ScoutSide = 512;
        const float PoseBar = 0.5f;      // the owner's evidence rule, 2026-10-19
        // The harsh critic's r1 reading of r2: "men barely separate from the ground", contrast_median 0.064. The
        // capture itself has to prove the figure reads now, so the Flamer's man still fails the test below this.
        const float ContrastBar = 0.15f;
        static readonly float[] ScoutOffsets = { 0f, 90f, 180f, 270f };
        static readonly float[] ScoutPitches = { 35f, 55f };

        // Prop is the piece of kit the third still drops, so the cut can measure how many portrait pixels it owns.
        // The angle is swept per man from his own barrel facing (step 1-2), not a constant shared with another
        // man: at the shared yaw 21 the Flamer stood behind a pale lump of scenery that hid everything but his
        // helmet, one hand and a boot - 2 235 differing pixels, a cutout of his wand alone.
        static readonly (byte Archetype, string Name, string Prop, System.Action<bool> Hide)[] Men =
        {
            (InfantryArchetype.Flamethrower, "Flamer", "wand", v => CombatFx.HideWand = v),
            (InfantryArchetype.Sentry, "Sentry", "plate", v => CombatFx.HidePlate = v),
        };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        // both are static: a run that dies between the no-prop shot and the clear would hide the kit for everyone
        [TearDown]
        public void ShowTheKitAgain() { CombatFx.HideWand = false; CombatFx.HidePlate = false; }

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>Frames with the clock still pinned: the world does not move, the renderer catches up.
        /// The three stills of one man have to be the SAME world moment. Settle() lets time run, so a Settle
        /// between two of them put 20 frames of idle animation, grass and fog into the difference mask: the cut
        /// came out a pale blob, and the no-prop twin would have measured the animation instead of the prop.</summary>
        static IEnumerator Frames(int frames)
        {
            for (int f = 0; f < frames; f++) yield return null;
        }

        /// <summary>Lets the sim run for a few real frames and then pins the clock and the weather again.</summary>
        static IEnumerator Settle(int frames)
        {
            Time.timeScale = 1f;
            for (int f = 0; f < frames; f++) yield return null;
            CaptureRig.Hold(120f);
        }

        static float Num(string json, string key)
        {
            int i = json.IndexOf("\"" + key + "\"");
            if (i < 0) return float.NaN;
            i = json.IndexOf(':', i) + 1;
            int end = i;
            // no char literal for the closing brace here: the brace-balance check counts braces outside strings
            while (end < json.Length && json[end] != ',' && json[end] != '\n') end++;
            return float.TryParse(json.Substring(i, end - i).Trim(), NumberStyles.Float, CultureInfo.InvariantCulture, out float v) ? v : float.NaN;
        }

        [UnityTest, Explicit("Photographs the Flamer and the Sentry alone, and the ground without them, for the portrait cut; run by name, with a graphics device.")]
        public IEnumerator TheFlamerAndTheSentrySitForTheirPortrait()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            yield return new EnterPlayMode();
            // everything after the reload is a fresh enumerator: locals captured before the domain reload come back null
            yield return InPlay();
        }

        static void ClearField(SimHost host)
        {
            host.WriteWorlds(m => { var w = m.World; for (int s = 0; s < w.HighWater; s++) if (w.IsAlive(s)) w.Despawn(s); });
        }

        static IEnumerator InPlay()
        {
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-portraits");
            Directory.CreateDirectory(dir);

            var host = Host;
            host.ScriptedPeer = false; host.PeerAttacks = false;
            host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TestContext.Out.WriteLine(CaptureRig.Hold(120f));
            yield return Settle(120);

            var lines = new List<string>();
            var offenders = new List<string>();
            lines.Add("night " + (SceneMood.Night ? 1 : 0)
                      + "\tzoom " + Zoom.ToString("0.0", CultureInfo.InvariantCulture)
                      + "\tside " + Side.ToString(CultureInfo.InvariantCulture));

            var units = Object.FindFirstObjectByType<VATRenderer>();

            foreach (var man in Men)
            {
                ClearField(host);
                yield return Settle(20);
                int placed = 0;
                host.WriteWorlds(m => placed = ProvingGround.Place(m, 0, man.Archetype, 1));
                yield return Settle(60);

                var pos = Vector3.zero; int alive = 0; int slot = -1;
                { var w = host.Local.World; for (int s = 0; s < w.HighWater; s++) if (w.IsAlive(s)) { alive++; pos = (Vector3)w.Position[s]; slot = s; } }
                TestContext.Out.WriteLine(man.Name + ": placed " + placed + ", alive " + alive + ", at " + pos.ToString("0.00"));
                Assert.That(alive, Is.EqualTo(1), man.Name + ": wanted exactly one man on the field, got " + alive);
                lines.Add(man.Name + "\tpos " + pos.x.ToString("0.00", CultureInfo.InvariantCulture)
                          + " " + pos.y.ToString("0.00", CultureInfo.InvariantCulture)
                          + " " + pos.z.ToString("0.00", CultureInfo.InvariantCulture));

                // read his own barrel facing: a yaw shared with another man can orbit into scenery that hides his kit
                float faceYaw = 0f;
                if (units != null && units.Sockets(slot, out _, out var barrel, out _))
                    faceYaw = Mathf.Atan2(barrel.x, barrel.z) * Mathf.Rad2Deg;
                TestContext.Out.WriteLine(man.Name + ": faceYaw " + faceYaw.ToString("0.0"));

                // a scout sweep around his own facing picks the angle that reads him best, by contrast_median
                float bestYaw = faceYaw, bestPitch = ScoutPitches[0], bestContrast = float.NegativeInfinity;
                var scoutPaths = new List<string>();
                foreach (float off in ScoutOffsets)
                {
                    foreach (float sp in ScoutPitches)
                    {
                        string scoutPath = Path.Combine(dir, man.Name + "-scout-" + off.ToString("0") + "-" + sp.ToString("0") + ".png");
                        scoutPaths.Add(scoutPath);
                        TestContext.Out.WriteLine(CaptureRig.Shot(scoutPath, pos.x, pos.z, Zoom, faceYaw + off, sp, ScoutSide, ScoutSide, pos.y + 1.2f));
                        yield return Drain(scoutPath);
                        string scoutSide = Path.ChangeExtension(scoutPath, ".json");
                        float c = File.Exists(scoutSide) ? Num(File.ReadAllText(scoutSide), "contrast_median") : float.NegativeInfinity;
                        lines.Add(man.Name + "\tscout yaw " + (faceYaw + off).ToString("0.0", CultureInfo.InvariantCulture)
                                  + "\tpitch " + sp.ToString("0.0", CultureInfo.InvariantCulture)
                                  + "\tcontrast_median " + c.ToString("0.0000", CultureInfo.InvariantCulture));
                        if (c > bestContrast || (c == bestContrast && sp < bestPitch))
                        {
                            bestContrast = c; bestYaw = faceYaw + off; bestPitch = sp;
                        }
                    }
                }
                foreach (string sp in scoutPaths)
                {
                    if (File.Exists(sp)) File.Delete(sp);
                    string sj = Path.ChangeExtension(sp, ".json");
                    if (File.Exists(sj)) File.Delete(sj);
                }
                TestContext.Out.WriteLine(man.Name + ": chosen yaw " + bestYaw.ToString("0.0") + " pitch " + bestPitch.ToString("0.0") + " contrast " + bestContrast.ToString("0.0000"));
                lines.Add(man.Name + "\tchosen yaw " + bestYaw.ToString("0.0", CultureInfo.InvariantCulture)
                          + "\tpitch " + bestPitch.ToString("0.0", CultureInfo.InvariantCulture));

                // the man, then the same pose with the ground alone: the pair gives the cutout its alpha
                string manPath = Path.Combine(dir, man.Name + "-man.png");
                TestContext.Out.WriteLine(CaptureRig.Shot(manPath, pos.x, pos.z, Zoom, bestYaw, bestPitch, Side, Side, pos.y + 1.2f));
                yield return Drain(manPath);

                // the same man in the same pose with his prop dropped: the cut of this one, through the same crop
                // box, is what tells the probe which portrait pixels are the wand or the plate
                string nopropPath = Path.Combine(dir, man.Name + "-noprop.png");
                man.Hide(true);
                yield return Frames(4);
                TestContext.Out.WriteLine(CaptureRig.Shot(nopropPath, pos.x, pos.z, Zoom, bestYaw, bestPitch, Side, Side, pos.y + 1.2f));
                yield return Drain(nopropPath);
                man.Hide(false);

                ClearField(host);
                yield return Frames(4);
                string emptyPath = Path.Combine(dir, man.Name + "-empty.png");
                TestContext.Out.WriteLine(CaptureRig.Shot(emptyPath, pos.x, pos.z, Zoom, bestYaw, bestPitch, Side, Side, pos.y + 1.2f));
                yield return Drain(emptyPath);

                foreach (var pair in new[] { (Path: manPath, WantMan: true), (Path: nopropPath, WantMan: true), (Path: emptyPath, WantMan: false) })
                {
                    string stem = Path.GetFileNameWithoutExtension(pair.Path);
                    string side = Path.ChangeExtension(pair.Path, ".json");
                    if (!File.Exists(side)) { offenders.Add(stem + ": no sidecar written"); continue; }
                    string json = File.ReadAllText(side);
                    float err = Num(json, "pose_error_m"), men = Num(json, "men_in_frame");
                    float contrast = Num(json, "contrast_median");
                    lines.Add(stem + "\tpose_error_m " + err.ToString("0.0000", CultureInfo.InvariantCulture)
                              + "\tmen_in_frame " + men.ToString("0", CultureInfo.InvariantCulture)
                              + "\tcontrast_median " + contrast.ToString("0.0000", CultureInfo.InvariantCulture));
                    if (!(err < PoseBar)) offenders.Add(stem + ": pose_error_m " + err.ToString("0.0000", CultureInfo.InvariantCulture) + " (bar < 0.50)");
                    if (pair.WantMan && !(men >= 1f)) offenders.Add(stem + ": men_in_frame " + men.ToString("0", CultureInfo.InvariantCulture) + ", the man is not in his own portrait shot");
                    // the critic's finding, measured at the source: the Flamer has to separate from the mud
                    if (pair.Path == manPath && man.Name == "Flamer" && !(contrast >= ContrastBar))
                        offenders.Add(stem + ": contrast_median " + contrast.ToString("0.0000", CultureInfo.InvariantCulture)
                                      + " (bar >= " + ContrastBar.ToString("0.00", CultureInfo.InvariantCulture) + "), the man barely separates from the ground");
                }
            }

            File.WriteAllText(Path.Combine(dir, "info.txt"), string.Join("\n", lines) + "\n");
            foreach (var l in lines) TestContext.Out.WriteLine(l);
            CaptureRig.Release();
            TestContext.Out.WriteLine("stills in " + dir + ", " + offenders.Count + " bad");
            Assert.That(offenders, Is.Empty, "portrait stills that cannot be cut:\n" + string.Join("\n", offenders));
            yield return new ExitPlayMode();
        }
    }
}
