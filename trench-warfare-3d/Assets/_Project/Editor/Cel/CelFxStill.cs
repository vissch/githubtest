// Phase: CelFx kit (2026-11-09, tooling) - a contact sheet of a cel recipe, rendered in an empty scene in edit mode,
// to prove the kit draws (every primitive) without the game around it. The clock is pinned (CelFx.Clock), the recipe
// is played once, and the sheet is the same records rendered at several moments of their life.
//
//   Unity.exe -batchmode -projectPath <wt>/trench-warfare-3d -executeMethod TW.Editor.CelFxStill.Batch
//             -celOut <dir> [-celZoom 10] [-celRecipe cel_demo] -logFile <log>        (NOT -nographics)
//
// Writes <dir>/cel_sheet.png (4 x 2 moments), <dir>/cel_peak.png (full size) and <dir>/cel_still.json (per moment:
// the time, the indirect draws, the records held). Chunks (DebrisRenderer) and lights (NightLights) need Play and
// are not in the still. An effect agent's recipe: CelFxStill.Sheet(recipe, dir, zoom) from its own executeMethod.
using System;
using System.Globalization;
using System.IO;
using System.Text;
using UnityEditor;
using UnityEditor.SceneManagement;
using UnityEngine;
using UnityEngine.Rendering.Universal;
using TW.Presentation.VFX.Cel;

namespace TW.Editor
{
    public static class CelFxStill
    {
        public static readonly float[] Moments = { 0.12f, 0.26f, 0.4f, 0.6f, 0.9f, 1.4f, 2.1f, 3.0f };

        public static void Batch()
        {
            int code = 0;
            try
            {
                string dir = Arg("-celOut", Path.Combine(Path.GetTempPath(), "celfx_still"));
                float zoom = float.Parse(Arg("-celZoom", "10"), CultureInfo.InvariantCulture);
                Debug.Log("CelFxStill: " + Sheet(CelDemo.Recipe, dir, zoom));
            }
            catch (Exception e) { Debug.LogError("CelFxStill failed: " + e); code = 1; }
            EditorApplication.Exit(code);
        }

        static string Arg(string name, string fallback)
        {
            var a = Environment.GetCommandLineArgs();
            for (int i = 0; i < a.Length - 1; i++) if (a[i] == name) return a[i + 1];
            return fallback;
        }

        /// <summary>Render the recipe's sheet into dir; returns a one-line summary.</summary>
        public static string Sheet(CelFxRecipe recipe, string dir, float zoom, int w = 960, int h = 540)
        {
            Directory.CreateDirectory(dir);
            EditorSceneManager.NewScene(NewSceneSetup.EmptyScene, NewSceneMode.Single);
            var target = new Vector3(0f, 1f, 0f);
            var cam = Stage(target);
            CelFx.ZoomOverride = zoom;
            CelFx.Clock = 10f;
            var renderer = CelFx.Renderer;
            renderer.Clear();
            Vector3 from = target + new Vector3(-14f, 0.2f, 0f);
            CelFx.Play(recipe, from, target - from, target, 1u);

            var rt = new RenderTexture(w, h, 24, RenderTextureFormat.ARGB32, RenderTextureReadWrite.sRGB) { antiAliasing = 4 };
            var frame = new Texture2D(w, h, TextureFormat.RGB24, false);
            int cols = 4, rows = (Moments.Length + cols - 1) / cols;
            int tw = w / 2, th = h / 2;
            var sheet = new Texture2D(tw * cols, th * rows, TextureFormat.RGB24, false);
            // JSON by hand; the braces are written as chars so the source stays brace-balanced for validate.py
            const char Lb = (char)123, Rb = (char)125;
            var json = new StringBuilder().Append(Lb).Append("\"recipe\":\"" + recipe.Name + "\",\"zoom\":" + F(zoom) + ",\"moments\":[");
            int maxDraws = 0;
            for (int m = 0; m < Moments.Length; m++)
            {
                CelFx.Clock = 10f + Moments[m];
                renderer.Submit(CelFx.Clock);
                cam.targetTexture = rt;
                cam.Render();
                RenderTexture.active = rt;
                frame.ReadPixels(new Rect(0, 0, w, h), 0, 0);
                frame.Apply();
                RenderTexture.active = null;
                if (m == 2) File.WriteAllBytes(Path.Combine(dir, "cel_peak.png"), frame.EncodeToPNG());
                int cx = m % cols, cy = rows - 1 - m / cols;
                for (int y = 0; y < th; y++)
                for (int x = 0; x < tw; x++)
                    sheet.SetPixel(cx * tw + x, cy * th + y, frame.GetPixelBilinear((x + 0.5f) / tw, (y + 0.5f) / th));
                maxDraws = Mathf.Max(maxDraws, renderer.DrawCalls);
                json.Append(m > 0 ? "," : "").Append(Lb).Append("\"t\":").Append(F(Moments[m])).Append(",\"draws\":").Append(renderer.DrawCalls).Append(",\"held\":").Append(renderer.Held).Append(Rb);
            }
            sheet.Apply();
            File.WriteAllBytes(Path.Combine(dir, "cel_sheet.png"), sheet.EncodeToPNG());
            json.Append("],\"kinds\":[");
            for (int k = 0; k < renderer.KindCount; k++) json.Append(k > 0 ? "," : "").Append(Lb).Append("\"name\":\"").Append(renderer.KindName(k)).Append("\",\"spawned\":").Append(renderer.SpawnedOf(k)).Append(Rb);
            json.Append("],\"maxDraws\":").Append(maxDraws).Append(",\"written\":\"").Append(DateTime.Now.ToString("s")).Append('"').Append(Rb);
            File.WriteAllText(Path.Combine(dir, "cel_still.json"), json.ToString());
            cam.targetTexture = null;
            UnityEngine.Object.DestroyImmediate(rt);
            CelFx.Clock = float.NaN; CelFx.ZoomOverride = float.NaN;
            return "sheet in " + dir + ", max " + maxDraws + " draws";
        }

        static string F(float v) => v.ToString("0.###", CultureInfo.InvariantCulture);

        /// <summary>A night-ish stage: a dark ground, a post with a board (the dummy), a camera looking at the target
        /// from the front with the shot coming in from the left, HDR with bloom.</summary>
        static Camera Stage(Vector3 target)
        {
            var unlit = Shader.Find("Universal Render Pipeline/Unlit");
            Material Flat(Color c) { var m = new Material(unlit); m.SetColor("_BaseColor", c); return m; }
            var ground = GameObject.CreatePrimitive(PrimitiveType.Plane);
            ground.transform.localScale = new Vector3(8f, 1f, 8f);
            ground.GetComponent<Renderer>().sharedMaterial = Flat(new Color(0.055f, 0.06f, 0.075f));
            var post = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
            post.transform.position = new Vector3(target.x, 0.6f, target.z + 0.4f); post.transform.localScale = new Vector3(0.12f, 0.6f, 0.12f);
            post.GetComponent<Renderer>().sharedMaterial = Flat(new Color(0.30f, 0.22f, 0.14f));
            var board = GameObject.CreatePrimitive(PrimitiveType.Cylinder);
            board.transform.position = new Vector3(target.x, 1.25f, target.z + 0.35f); board.transform.rotation = Quaternion.Euler(90f, 0f, 0f); board.transform.localScale = new Vector3(0.7f, 0.03f, 0.7f);
            board.GetComponent<Renderer>().sharedMaterial = Flat(new Color(0.42f, 0.32f, 0.20f));

            var go = new GameObject("CelStillCamera");
            var cam = go.AddComponent<Camera>();
            cam.transform.position = target + new Vector3(-1.5f, 2.6f, -13f);
            cam.transform.LookAt(target + new Vector3(-2.2f, 0.6f, 0f));
            cam.fieldOfView = 42f; cam.nearClipPlane = 0.1f; cam.farClipPlane = 500f;
            cam.clearFlags = CameraClearFlags.SolidColor; cam.backgroundColor = new Color(0.07f, 0.08f, 0.11f);
            cam.allowHDR = true;
            var data = cam.GetUniversalAdditionalCameraData();
            data.renderPostProcessing = true;
            Bloom(go);
            return cam;
        }

        /// <summary>A global volume with bloom and a neutral tonemap, built by reflection (TW.Editor does not reference
        /// the Core runtime, where Volume lives).</summary>
        static void Bloom(GameObject go)
        {
            try
            {
                var volumeType = Type.GetType("UnityEngine.Rendering.Volume, Unity.RenderPipelines.Core.Runtime");
                var profileType = Type.GetType("UnityEngine.Rendering.VolumeProfile, Unity.RenderPipelines.Core.Runtime");
                var bloomType = Type.GetType("UnityEngine.Rendering.Universal.Bloom, Unity.RenderPipelines.Universal.Runtime");
                var toneType = Type.GetType("UnityEngine.Rendering.Universal.Tonemapping, Unity.RenderPipelines.Universal.Runtime");
                if (volumeType == null || profileType == null || bloomType == null) { Debug.LogWarning("CelFxStill: no Volume types; no bloom"); return; }
                var profile = ScriptableObject.CreateInstance(profileType);
                var add = profileType.GetMethod("Add", new[] { typeof(Type), typeof(bool) });
                var bloom = add.Invoke(profile, new object[] { bloomType, true });
                Param(bloom, "threshold", 0.95f); Param(bloom, "intensity", 0.9f); Param(bloom, "scatter", 0.6f);
                if (toneType != null)
                {
                    var tone = add.Invoke(profile, new object[] { toneType, true });
                    var modeParam = tone.GetType().GetField("mode").GetValue(tone);
                    var enumType = modeParam.GetType().GetProperty("value").PropertyType;
                    Param(tone, "mode", Enum.Parse(enumType, "Neutral"));
                }
                var volume = go.AddComponent(volumeType);
                Member(volume, "isGlobal", true);
                Member(volume, "sharedProfile", profile);
            }
            catch (Exception e) { Debug.LogWarning("CelFxStill: bloom not set up: " + e); }
        }

        static void Param(object component, string field, object value)
        {
            var p = component.GetType().GetField(field).GetValue(component);
            p.GetType().GetProperty("overrideState").SetValue(p, true);
            p.GetType().GetProperty("value").SetValue(p, value);
        }

        static void Member(object o, string name, object value)
        {
            var t = o.GetType();
            var prop = t.GetProperty(name);
            if (prop != null && prop.CanWrite) { prop.SetValue(o, value); return; }
            var field = t.GetField(name);
            if (field != null) field.SetValue(o, value);
            else Debug.LogWarning("CelFxStill: Volume has no " + name);
        }
    }
}
