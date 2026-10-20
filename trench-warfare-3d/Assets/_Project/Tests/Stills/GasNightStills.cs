// Phase: gas-smoke-night (the owner, 2026-10-19: the ability clouds were tuned by day; at night black or grey smoke
// vanishes). The instrument for that one question: the gas bank and the smoke bank, each on its own open ground, each
// filmed once at the zoom the game is played at (30) after ten seconds of captured time, so the cloud is at full spread.
// Env: TW_STILLS_DIR the folder (default %TEMP%/tw-gasnight), TW_STILLS_FIELD the look (NightMud is the night, Winter
// the day field) — both VfxStills' own, so no new flag — and TW_STILLS_TAKE the take number (default 1), appended to
// every png as _t<take> so three runs of the same field do not overwrite each other. It asserts only that the stills
// were written: the numbers come from docs/evidence/gas-smoke-night-g2/probe.py, measured off the pixels.
//
// Phase: gas-smoke-night-g2 (the owner, 2026-10-20). Each still also gets a sidecar <png stem>.aim.json: where the
// cloud actually stands in the frame, in pixels, worked out from the sim field and the still's own camera pose BEFORE
// the shot is queued. A fixed centre box landed half on the wreck that stands in the gas frame; the probe reads these
// boxes instead. The pose is TacticalCamera's own arithmetic, the same CaptureRig.Rig.Pose does, because the rig
// computes its pose inside the frame and puts the camera back again, so there is no camera left to project with.
using System.Collections;
using System.IO;
using NUnit.Framework;
using UnityEngine;
using UnityEditor.SceneManagement;
using UnityEngine.TestTools;
using TW.Editor;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Terrain;
using Unity.Mathematics;
using TW.UI;

namespace TW.Tests
{
    public class GasNightStills
    {
        const string Scene = "Assets/_Project/Scenes/GreyboxCorridor.unity";

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static IEnumerator Drain(string path)
        {
            for (int f = 0; f < 900 && CaptureRig.Pending() != "0"; f++) yield return null;
            for (int f = 0; f < 300 && !File.Exists(path); f++) yield return null;
        }

        /// <summary>Open ground for a cloud centred on (x, z): VfxStills' own test, so the two instruments stage alike.</summary>
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
            if (map.StaticCover.IsCreated)
                for (int i = 0; i < map.StaticCover.Length; i++)
                {
                    var cv = map.StaticCover[i];
                    if (cv.OwnerSlot >= 0) continue;
                    if (math.abs(cv.Center.x - x) < r + 6f && math.abs(cv.Center.z - z) < r + 6f) return false;
                }
            return true;
        }

        const float Room = 12f;     // a bank spreads: VfxStills' room for "gas" and "smoke"

        // ---- aiming the box at the cloud (gas-smoke-night-g2) ------------------------------------------------

        /// <summary>Invariant number for the sidecar: a json a python probe reads may never carry a comma decimal.</summary>
        static string Num(float v) => v.ToString("0.####", System.Globalization.CultureInfo.InvariantCulture);

        const int ShotW = 1280, ShotH = 720;
        const float BoxFracW = 0.30f, BoxFracH = 0.22f;   // the aimed box, as a fraction of the frame
        const float GroundFracW = 0.10f;                  // the left and right tenth, over the box's rows
        const float CellsRadius = 30f;                    // this scene's cloud only: the other bank stands 34 m off
        // the floors CombatFx.DrawGasBank and DrawSmokeBank use for a cell to stand (CombatFx.Banks.cs)
        const float GasFloor = 0.8f, SmokeFloor = 1.5f;

        /// <summary>The gas and smoke field, reached without naming its type: TW.Sim.Combat is not on this test
        /// assembly's reference list, and an asmdef reference list is a seam (CLAUDE.md), not something an instrument
        /// may widen on its own. SimWorld.Systems and ISimSystem are both in TW.Sim, which is referenced.</summary>
        static object GasSystem()
        {
            foreach (var s in Host.Local.World.Systems)
                if (s.GetType().Name == "GasSmokeSystem") return s;
            return null;
        }

        static Unity.Collections.NativeArray<float> FieldOf(object sys, string name)
            => (Unity.Collections.NativeArray<float>)sys.GetType().GetField(name).GetValue(sys);

        static int IntOf(object sys, string name) => (int)sys.GetType().GetField(name).GetValue(sys);

        /// <summary>A throwaway camera posed exactly as CaptureRig.Rig.Pose poses the real one for this shot, at the
        /// still's own size, so WorldToScreenPoint lands on the png's pixels. Disabled, with a render texture only to
        /// give it 1280x720 pixels to count in; it never renders. Destroyed when the shot is written.</summary>
        static Camera PoseCam(TW.Presentation.Tactical.TacticalCamera tc, MapData map, float x, float z,
                              float zoom, float yaw, float pitch, out Vector3 eye)
        {
            var go = new GameObject("TW Aim Cam") { hideFlags = HideFlags.DontSave };
            var cam = go.AddComponent<Camera>();
            cam.enabled = false;
            cam.targetTexture = new RenderTexture(ShotW, ShotH, 0);
            float zc = Mathf.Clamp(zoom, tc.NearZoom, tc.ZoomMax);
            float close = 1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(tc.NearZoom, tc.CloseZoom, zc));
            float fov = Mathf.Lerp(tc.Fov, tc.CloseFov, close);
            cam.fieldOfView = fov; cam.nearClipPlane = 0.2f;
            var rot = Quaternion.Euler(Mathf.Clamp(pitch, tc.PitchMin, tc.PitchMax), tc.BaseYaw + yaw, 0f);
            float distance = zc * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(fov * 0.5f * Mathf.Deg2Rad);
            float ground = map.Height.Sample(x, z);
            var aim = new Vector3(x, ground * close + tc.CloseAimHeight * close, z);
            eye = aim - rot * Vector3.forward * distance;
            go.transform.SetPositionAndRotation(eye, rot);
            return cam;
        }

        /// <summary>A world point on the png's pixels: x from the left, y from the TOP, as Pillow counts rows.</summary>
        static Vector2 Px(Camera cam, Vector3 world)
        {
            var sp = cam.WorldToScreenPoint(world);
            return new Vector2(sp.x, ShotH - 1f - sp.y);
        }

        /// <summary>Every field cell of this scene's cloud that stands: above the bank's floor and inside CellsRadius
        /// of the staging, each at the ground height its card stands on. The same walk CombatFx.Refresh does.</summary>
        static System.Collections.Generic.List<Vector3> Standing(
            Unity.Collections.NativeArray<float> field, int width, int length, float floor, MapData map, float x, float z)
        {
            var cells = new System.Collections.Generic.List<Vector3>(512);
            if (!field.IsCreated) return cells;
            float cs = MapData.FieldCellSize;
            for (int cz = 0; cz < length; cz++)
                for (int cx = 0; cx < width; cx++)
                {
                    if (field[cz * width + cx] < floor) continue;
                    float wx = (cx + 0.5f) * cs, wz = (cz + 0.5f) * cs;
                    if (Mathf.Abs(wx - x) > CellsRadius || Mathf.Abs(wz - z) > CellsRadius) continue;
                    cells.Add(new Vector3(wx, RenderGround.Sample(map, wx, wz), wz));
                }
            return cells;
        }

        /// <summary>The sidecar the probe aims by: the box around the cloud's centroid, the ground strips over its own
        /// rows, the cloud's far ground edge, and how many pixels a metre is at the centroid. Boxes are [x0, y0, x1, y1]
        /// in pixels, x1 and y1 exclusive, y from the top.</summary>
        static void WriteAim(string pngPath, Camera cam, Vector3 eye,
                             System.Collections.Generic.List<Vector3> cells, string scene, float floor)
        {
            string aimPath = pngPath.Substring(0, pngPath.Length - 4) + ".aim.json";
            var sb = new System.Text.StringBuilder();
            sb.Append("{\n");
            sb.Append("  \"scene\": \"").Append(scene).Append("\",\n");
            sb.Append("  \"frame\": [").Append(ShotW).Append(", ").Append(ShotH).Append("],\n");
            sb.Append("  \"floor\": ").Append(Num(floor)).Append(",\n");
            sb.Append("  \"cells\": ").Append(cells.Count).Append(",\n");
            sb.Append("  \"box_frac\": [").Append(Num(BoxFracW)).Append(", ").Append(Num(BoxFracH)).Append("],\n");
            if (cells.Count == 0)
            {
                // No cloud standing at all is a fact the probe must see, not a crash: an empty box and no numbers.
                sb.Append("  \"empty\": true\n}\n");
                File.WriteAllText(aimPath, sb.ToString());
                TestContext.Out.WriteLine("aim " + Path.GetFileName(aimPath) + ": no cells above " + Num(floor));
                return;
            }
            var mid = Vector3.zero;
            foreach (var c in cells) mid += c;
            mid /= cells.Count;
            var c0 = Px(cam, mid);
            float pxPerM = Vector2.Distance(c0, Px(cam, mid + Vector3.up));
            var far = cells[0]; float best = -1f;
            foreach (var c in cells) { float d = (c - eye).sqrMagnitude; if (d > best) { best = d; far = c; } }
            var fe = Px(cam, far);

            float bw = ShotW * BoxFracW, bh = ShotH * BoxFracH;
            int bx0 = Mathf.Clamp(Mathf.RoundToInt(c0.x - bw * 0.5f), 0, ShotW - 1);
            int bx1 = Mathf.Clamp(Mathf.RoundToInt(c0.x + bw * 0.5f), bx0 + 1, ShotW);
            int by0 = Mathf.Clamp(Mathf.RoundToInt(c0.y - bh * 0.5f), 0, ShotH - 1);
            int by1 = Mathf.Clamp(Mathf.RoundToInt(c0.y + bh * 0.5f), by0 + 1, ShotH);
            int gw = Mathf.Max(1, Mathf.RoundToInt(ShotW * GroundFracW));
            sb.Append("  \"centroid\": [").Append(Num(c0.x)).Append(", ").Append(Num(c0.y)).Append("],\n");
            sb.Append("  \"centroid_world\": [").Append(Num(mid.x)).Append(", ").Append(Num(mid.y)).Append(", ").Append(Num(mid.z)).Append("],\n");
            sb.Append("  \"far_edge\": [").Append(Num(fe.x)).Append(", ").Append(Num(fe.y)).Append("],\n");
            sb.Append("  \"px_per_m\": ").Append(Num(pxPerM)).Append(",\n");
            sb.Append("  \"box\": [").Append(bx0).Append(", ").Append(by0).Append(", ").Append(bx1).Append(", ").Append(by1).Append("],\n");
            // The cloud's far ground edge sits well ABOVE the centroid in the frame (farther away is higher up the
            // picture): in the first film it landed 80 px above the box's top row. A mask cut to the box would have had
            // nothing above the far edge at all, so a height measured off it would read 0 for any cloud, rim or no rim.
            // So the mask gets the box's columns and rows stretched to take the far edge in; contrast keeps the plain
            // box. (This leg, 2026-10-20, measured off the first film's aim.json.)
            int mb0 = Mathf.Clamp(Mathf.Min(by0, Mathf.FloorToInt(fe.y) - 4), 0, ShotH - 1);
            int mb1 = Mathf.Clamp(Mathf.Max(by1, Mathf.CeilToInt(fe.y) + 4), mb0 + 1, ShotH);
            sb.Append("  \"mask_box\": [").Append(bx0).Append(", ").Append(mb0).Append(", ").Append(bx1).Append(", ").Append(mb1).Append("],\n");
            sb.Append("  \"ground_l\": [0, ").Append(by0).Append(", ").Append(gw).Append(", ").Append(by1).Append("],\n");
            sb.Append("  \"ground_r\": [").Append(ShotW - gw).Append(", ").Append(by0).Append(", ").Append(ShotW).Append(", ").Append(by1).Append("],\n");
            sb.Append("  \"empty\": false\n}\n");
            File.WriteAllText(aimPath, sb.ToString());
            TestContext.Out.WriteLine("aim " + Path.GetFileName(aimPath) + ": " + cells.Count + " cells, box "
                + bx0 + "," + by0 + " " + bx1 + "," + by1 + ", px/m " + Num(pxPerM));
        }

        [UnityTest, Explicit("The gas and smoke banks photographed at zoom 30; run by name, with a graphics device.")]
        public IEnumerator GasAndSmokeBanksArePhotographed()
        {
            HudBootstrap.Disabled = true;
            ShellBoot.Disabled = true;
            CaptureRig.Rig.Verbose = false;
            EditorSceneManager.OpenScene(Scene, OpenSceneMode.Single);
            string fieldName = System.Environment.GetEnvironmentVariable("TW_STILLS_FIELD");
            if (System.Enum.TryParse(fieldName, true, out TW.Presentation.Terrain.Biome field))
            {
                foreach (var g in Object.FindObjectsByType<TW.Presentation.Terrain.GreyboxTerrainView>(FindObjectsSortMode.None)) g.Field = field;
                foreach (var at in Object.FindObjectsByType<TW.Presentation.Terrain.Atmosphere>(FindObjectsSortMode.None)) at.Field = field;
            }
            yield return new EnterPlayMode();
            for (int f = 0; f < 900 && (Host == null || Host.Local == null); f++) yield return null;
            Assert.That(Host?.Local, Is.Not.Null, "no match");
            string dir = System.Environment.GetEnvironmentVariable("TW_STILLS_DIR");
            if (string.IsNullOrEmpty(dir)) dir = Path.Combine(Path.GetTempPath(), "tw-gasnight");
            Directory.CreateDirectory(dir);
            // the take, so three runs of the same field stand beside each other instead of overwriting (g2)
            string take = System.Environment.GetEnvironmentVariable("TW_STILLS_TAKE");
            if (string.IsNullOrEmpty(take)) take = "1";
            // named after the field actually filmed, read back off the sky, not off the variable asked for
            var skyNow = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            string stem = skyNow != null ? skyNow.Field.ToString().ToLowerInvariant() : "scene";
            TestContext.Out.WriteLine("field asked " + (fieldName ?? "(none)") + ", filmed " + stem + ", take " + take);

            Host.ScriptedPeer = false; Host.PeerAttacks = false;
            Host.WriteWorlds(m => { var b = m.World.GetSystem<AmbientBombardmentSystem>(); if (b != null) b.ShellsPerMinute = 0f; });
            for (int f = 0; f < 120; f++) yield return null;
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky != null) { sky.Rain = 0f; sky.Squalls = 0f; }
            TW.Presentation.Terrain.Atmosphere.PinnedClock = 30f;
            Time.captureDeltaTime = 1f / 30f;   // the game clock steps 1/30 s a frame while filming (a still stalls ~0.3 s)
            var tc = Object.FindFirstObjectByType<TW.Presentation.Tactical.TacticalCamera>();
            if (tc != null) tc.BaseYaw = -90f;  // pinned before the first pose is computed, so the aim and the shot agree

            var map = Host.Local.Map;
            var size = map.SizeMeters;
            int written = 0;
            var used = new System.Collections.Generic.List<Vector2>();
            foreach (string scene in new[] { "gas", "smoke" })
            {
                float x = -1f, z = -1f;
                for (float zz = 40f; zz < size.y - 40f && x < 0f; zz += 6f)
                    for (float xx = 20f; xx < size.x - 20f && x < 0f; xx += 6f)
                    {
                        bool clear = true;
                        foreach (var u in used) if (Mathf.Abs(u.x - xx) < 34f && Mathf.Abs(u.y - zz) < 34f) clear = false;
                        if (clear && Open(map, xx, zz, Room)) { x = xx; z = zz; }
                    }
                Assert.Greater(x, 0f, scene + ": no open ground left");
                used.Add(new Vector2(x, z));
                TestContext.Out.WriteLine(scene + " at " + x + ", " + z + ": " + VfxLab.Scene(scene, x, z));
                // ten seconds of captured time: the gas warms up over 60 ticks and the field decays with a ~9 s
                // half-life, so this is the bank at its fullest spread
                float due = Time.time + 10f;
                while (Time.time < due) yield return null;
                string path = Path.Combine(dir, $"{scene}_{stem}_t{take}.png");
                // where the cloud stands in THIS frame, projected with this shot's own pose, before the shot is queued
                var gasSys = GasSystem();
                if (tc != null && gasSys != null)
                {
                    var cam = PoseCam(tc, map, x, z, 30f, 40f, 25f, out var eye);
                    float floor = scene == "gas" ? GasFloor : SmokeFloor;
                    var cells = Standing(FieldOf(gasSys, scene == "gas" ? "Gas" : "Smoke"),
                                         IntOf(gasSys, "Width"), IntOf(gasSys, "Length"), floor, map, x, z);
                    WriteAim(path, cam, eye, cells, scene, floor);
                    var rt = cam.targetTexture; cam.targetTexture = null;
                    Object.DestroyImmediate(rt); Object.DestroyImmediate(cam.gameObject);
                }
                CaptureRig.Shot(path, x, z, 30f, 40f, 25f, ShotW, ShotH, 1f);
                yield return Drain(path);
                if (File.Exists(path)) written++;
            }
            Time.captureDeltaTime = 0f;
            TW.Presentation.Terrain.Atmosphere.PinnedClock = -1f;
            TestContext.Out.WriteLine("wrote " + written + " stills to " + dir);
            yield return new ExitPlayMode();
            Assert.AreEqual(2, written, "both stills must be written to " + dir);
        }
    }
}
