// Phase: final CelFx delivery (2026-11-14) - optional Windows-player preview of the selected runtime effects.
// Start the player with -twvfx <effect>. Uses the same runtime callbacks as the captured reference sequences.
using System;
using System.Collections;
using UnityEngine;
using UnityEngine.SceneManagement;
using UnityEngine.UIElements;
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
            BuildMenu();
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

        void BuildMenu()
        {
            var document = gameObject.AddComponent<UIDocument>();
            var panel = ScriptableObject.CreateInstance<PanelSettings>();
            panel.sortingOrder = 1000;
            document.panelSettings = panel;
            var root = document.rootVisualElement;
            root.style.position = Position.Absolute;
            root.style.left = 12; root.style.top = 12; root.style.width = 230;
            root.style.paddingLeft = root.style.paddingRight = 10;
            root.style.paddingTop = root.style.paddingBottom = 10;
            root.style.backgroundColor = new Color(.07f, .10f, .15f, .94f);
            root.style.unityFont = Resources.GetBuiltinResource<Font>("LegacyRuntime.ttf");
            root.style.color = Color.white;
            root.Add(new Label("Final VFX"));
            for (int i = 0; i < Effects.Length; i++)
            {
                int choice = i;
                var button = new Button(() => { selected = choice; Play(); }) { text = Effects[i] };
                button.style.height = 27; button.style.marginTop = 3;
                button.style.backgroundColor = new Color(.16f, .22f, .30f);
                button.style.color = Color.white;
                root.Add(button);
            }
            var replay = new Button(Play) { text = "Replay" };
            replay.style.height = 27; root.Add(replay);
            var quit = new Button(() => Application.Quit()) { text = "Quit" };
            quit.style.height = 27; root.Add(quit);
        }
    }
}
