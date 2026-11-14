// Phase: final CelFx delivery (2026-11-14) - optional Windows-player preview of the selected runtime effects.
// Start the player with -twvfx <effect>. Uses the same runtime callbacks as the captured reference sequences.
using System;
using System.Collections;
using UnityEngine;
using UnityEngine.SceneManagement;
using TW.Presentation;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX
{
    public sealed class FinalVfxPreview : MonoBehaviour
    {
        public static readonly string[] Effects = { "green_laser", "blue_laser", "bullet_impacts", "explosion",
            "fire_bolts", "flamethrower", "artillery", "cyan_beam", "orbital_laser", "neon_acid" };
        TargetDummy dummy;
        Camera eye;
        int selected;
        float nextPlay;
        bool ready;
        string requested;

        [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.AfterSceneLoad)]
        static void Boot()
        {
            var args = Environment.GetCommandLineArgs();
            int index = Array.IndexOf(args, "-twvfx");
            if (index < 0) return;
            var host = new GameObject("Final VFX preview").AddComponent<FinalVfxPreview>();
            host.requested = index + 1 < args.Length ? args[index + 1] : Effects[0];
            DontDestroyOnLoad(host.gameObject);
        }

        IEnumerator Start()
        {
            // An empty scene owns the preview; the live match never receives the reference volleys.
            var previous = SceneManager.GetActiveScene();
            var scene = SceneManager.CreateScene("Final VFX");
            SceneManager.SetActiveScene(scene);
            foreach (var root in previous.GetRootGameObjects()) root.SetActive(false);
            yield return SceneManager.UnloadSceneAsync(previous);
            SceneStatics.ResetSession();
            Time.timeScale = 1f;
            FxQuality.Set(FxTier.Epic);
            RenderSettings.fog = false;
            RenderSettings.ambientLight = new Color(.35f, .40f, .50f);
            eye = new GameObject("Preview camera").AddComponent<Camera>();
            eye.tag = "MainCamera";
            eye.transform.position = new Vector3(8f, 7f, -14f);
            eye.transform.LookAt(new Vector3(0f, 1f, 0f));
            eye.orthographic = true; eye.orthographicSize = 7.5f;
            eye.backgroundColor = new Color(.08f, .12f, .18f);
            eye.clearFlags = CameraClearFlags.SolidColor;
            eye.allowHDR = true;
            var sun = new GameObject("Preview light").AddComponent<Light>();
            sun.type = LightType.Directional; sun.intensity = 1f;
            sun.transform.rotation = Quaternion.Euler(40f, -30f, 0f);
            var floor = GameObject.CreatePrimitive(PrimitiveType.Plane);
            floor.name = "Preview ground";
            floor.transform.localScale = Vector3.one * 20f;
            var shader = Shader.Find("TW/Toon (URP)");
            var material = new Material(shader);
            material.SetColor("_BaseColor", new Color(.22f, .26f, .29f));
            floor.GetComponent<Renderer>().sharedMaterial = material;
            dummy = new GameObject("Preview target").AddComponent<TargetDummy>();
            dummy.Place(Vector3.zero, 180f);
            selected = Mathf.Max(0, Array.IndexOf(Effects, requested));
            ready = true;
            Play();
        }

        void Play()
        {
            CelFx.Renderer.Clear();
            CelFx.Clock = float.NaN;
            CelFx.ZoomOverride = 7.5f;
            Vector3 target = dummy.Chest;
            Vector3 side = eye.transform.right;
            Vector3 back = -eye.transform.forward; back.y = 0f; back.Normalize();
            CelRange.Begin(target - side * 15f + back * 5f - Vector3.up,
                target - side * 15f - back * 3f + Vector3.up * 2f, target, Vector3.zero);
            var shot = CelRange.Fire(Effects[selected], .8f);
            if (shot != null) { shot.Seed = 1917; shot.Lod = RangeLod.Close; shot.Zoom = 7.5f; shot.Eye = eye.transform.position; }
            Debug.Log("FINAL_VFX_PLAY " + Effects[selected]);
            nextPlay = Time.time + 9f;
        }

        void Update()
        {
            if (!ready) return;
            CelRange.Tick();
            if (Time.time >= nextPlay) Play();
        }

        void OnGUI()
        {
            if (!ready) return;
            GUILayout.BeginArea(new Rect(12, 12, 230, 500), GUI.skin.box);
            GUILayout.Label("Final VFX - " + Effects[selected]);
            for (int i = 0; i < Effects.Length; i++)
                if (GUILayout.Button(Effects[i])) { selected = i; Play(); }
            if (GUILayout.Button("Replay")) Play();
            if (GUILayout.Button("Quit")) Application.Quit();
            GUILayout.EndArea();
        }
    }
}
