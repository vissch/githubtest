// Phase: B2 (implemented) — thunder and lightning for the night look. The camera looks down at the field and never sees
// the sky, so a strike has to come to the picture: each bolt is aimed at a point in the upper part of the frame and lands
// there, its trunk running up and out of the top of the view. A strike is:
//   - a forked bolt (a random walk down from the cloud with two or three dimmer branches), rebuilt as camera-facing
//     ribbons for each strike and drawn additive and over-bright (TW/Bolt);
//   - two to four flickers over a third of a second, which drive everything together: the bolt, a cold point light and a
//     glow where it lands, and Atmosphere's sheet flash (the moon light swings round to come from the bolt, so every
//     shadow on the field jumps toward you, the sky and every puddle go white);
//   - thunder, made here from noise (no audio assets exist yet): a crack and a long rolling rumble, three variants by
//     distance, played late by the speed of sound and quieter from further off.
// Strikes come more often when the rain is heavy (Atmosphere.RainNow). All of it is one small mesh, one light, one glow
// card and one AudioSource.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;

namespace TW.Presentation.Terrain
{
    public sealed class Storm : MonoBehaviour
    {
        [Tooltip("Seconds between strikes: in a downpour, and in light rain.")]
        public Vector2 Every = new Vector2(7f, 24f);
        [Range(0f, 1f)] public float ThunderVolume = 0.8f;
        public Color BoltLight = new Color(0.78f, 0.86f, 1f);

        struct Pulse { public float At, Length, Strength; }
        readonly List<Pulse> pulses = new List<Pulse>();
        float struckAt = -100f, nextStrike, strikeStrength;
        Vector3 struckWhere;
        Mesh bolt; Material boltMaterial, glowMaterial; Light flashLight; GameObject glowCard;
        AudioSource voice; AudioClip[] thunder;
        struct Pending { public float At, Volume; public int Clip; }
        readonly List<Pending> pending = new List<Pending>();
        readonly List<Vector3> verts = new List<Vector3>(); readonly List<Vector2> uvs = new List<Vector2>(); readonly List<Color> cols = new List<Color>(); readonly List<int> tris = new List<int>();

        void Start()
        {
            bolt = new Mesh { name = "Lightning", hideFlags = HideFlags.HideAndDontSave }; bolt.MarkDynamic();
            boltMaterial = new Material(Shader.Find("TW/Bolt (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            var go = new GameObject("Lightning") { hideFlags = HideFlags.DontSave };
            go.transform.SetParent(transform, false);
            go.AddComponent<MeshFilter>().sharedMesh = bolt;
            var r = go.AddComponent<MeshRenderer>(); r.sharedMaterial = boltMaterial; r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off; r.receiveShadows = false;

            var lightGo = new GameObject("Lightning light") { hideFlags = HideFlags.DontSave };
            lightGo.transform.SetParent(transform, false);
            flashLight = lightGo.AddComponent<Light>();
            flashLight.type = LightType.Point; flashLight.color = BoltLight; flashLight.range = 90f; flashLight.intensity = 0f; flashLight.shadows = LightShadows.None; flashLight.enabled = false;

            // where it lands: one glow card (TW/Glow layers it white, blue-white, deep blue)
            glowMaterial = new Material(Shader.Find("TW/Glow (URP)")) { hideFlags = HideFlags.HideAndDontSave };
            glowMaterial.SetColor("_Tint", Color.black);
            glowCard = new GameObject("Lightning glow") { hideFlags = HideFlags.DontSave };
            glowCard.transform.SetParent(lightGo.transform, false);
            var gm = new Mesh { name = "Lightning glow", hideFlags = HideFlags.HideAndDontSave };
            gm.SetVertices(new List<Vector3> { Vector3.zero, Vector3.zero, Vector3.zero, Vector3.zero });
            gm.SetUVs(0, new List<Vector2> { new Vector2(-1, -1), new Vector2(1, -1), new Vector2(1, 1), new Vector2(-1, 1) });
            gm.SetUVs(1, new List<Vector4> { new Vector4(22f, .2f, 0f, .1f), new Vector4(22f, .2f, 0f, .1f), new Vector4(22f, .2f, 0f, .1f), new Vector4(22f, .2f, 0f, .1f) });
            gm.SetColors(new List<Color> { new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f) });
            gm.SetTriangles(new[] { 0, 2, 1, 0, 3, 2 }, 0); gm.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
            glowCard.AddComponent<MeshFilter>().sharedMesh = gm;
            var gr = glowCard.AddComponent<MeshRenderer>(); gr.sharedMaterial = glowMaterial; gr.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;

            voice = gameObject.AddComponent<AudioSource>();
            voice.spatialBlend = 0f; voice.playOnAwake = false;
            if (FindFirstObjectByType<AudioListener>() == null && Camera.main != null) Camera.main.gameObject.AddComponent<AudioListener>();
            thunder = new[] { MakeThunder(0, 1f, 5.5f), MakeThunder(1, .45f, 6.5f), MakeThunder(2, .08f, 7.5f) };   // near: a crack; far: all rumble
            nextStrike = Time.time + 9f;
        }

        void OnDestroy()
        {
            Atmosphere.StormFlash = 0f;
            if (bolt != null) Destroy(bolt);
            if (boltMaterial != null) Destroy(boltMaterial);
            if (glowMaterial != null) Destroy(glowMaterial);
            if (thunder != null) foreach (var clip in thunder) if (clip != null) Destroy(clip);
        }

        /// <summary>Thunder from noise: a sharp crack (as much of it as `crack`), then rolling low rumble that swells a few times and dies away.</summary>
        static AudioClip MakeThunder(int variant, float crack, float seconds)
        {
            const int rate = 22050;
            int n = (int)(rate * seconds);
            var data = new float[n];
            uint seed = 0x9E3779B9u * (uint)(variant + 1);
            float low = 0f, lower = 0f;
            // three or four swells of different size rolling in after the crack
            var swellAt = new float[4]; var swellSize = new float[4];
            for (int k = 0; k < 4; k++) { seed = seed * 1664525u + 1013904223u; swellAt[k] = .15f + k * (.55f + (seed >> 8 & 0xFF) / 255f * .7f); seed = seed * 1664525u + 1013904223u; swellSize[k] = .45f + (seed >> 8 & 0xFF) / 255f * .55f; }
            for (int i = 0; i < n; i++)
            {
                float t = i / (float)rate;
                seed = seed * 1664525u + 1013904223u;
                float white = ((seed >> 9) & 0x7FFF) / 16383.5f - 1f;
                low += (white - low) * .045f;          // about 160 Hz
                lower += (low - lower) * .02f;         // and what is under it
                float roll = 0f;
                for (int k = 0; k < 4; k++) { float d = (t - swellAt[k]) / (.35f + k * .15f); roll += swellSize[k] * Mathf.Exp(-d * d); }
                float envelope = (roll + .25f) * Mathf.Exp(-t * .55f) * Mathf.Clamp01(t * 30f) * Mathf.Clamp01((seconds - t) * 2f);
                float snap = crack * white * Mathf.Exp(-t * 22f) * .9f + crack * low * Mathf.Exp(-t * 6f) * 2.2f;
                data[i] = Mathf.Clamp((low * 3.2f + lower * 9f) * envelope + snap, -1f, 1f);
            }
            var clip = AudioClip.Create("Thunder " + variant, n, 1, rate, false);
            clip.SetData(data, 0);
            return clip;
        }

        void Strike()
        {
            var cam = Camera.main; if (cam == null) return;
            var t = cam.transform;
            float toGround = t.position.y / Mathf.Max(.15f, -t.forward.y);
            Vector3 look = t.position + t.forward * toGround;
            // The camera never sees the sky, so the strike is aimed at the picture: a point in the upper part of the frame is
            // followed down to the ground, and the bolt lands there, its trunk running up and out of the top of the view.
            var ray = cam.ViewportPointToRay(new Vector3(Random.Range(.12f, .88f), Random.Range(.60f, .92f), 0f));
            float reach = ray.direction.y < -.02f ? Mathf.Min(260f, -ray.origin.y / ray.direction.y) : 200f;
            Vector3 foot = ray.origin + ray.direction * reach;
            float far = Vector3.Distance(look, foot);
            foot.y = RenderGround.Map != null ? RenderGround.Sample(RenderGround.Map, Mathf.Clamp(foot.x, 0f, RenderGround.Map.SizeMeters.x - 1f), Mathf.Clamp(foot.z, 0f, RenderGround.Map.SizeMeters.y - 1f)) : 0f;
            struckWhere = foot; struckAt = Time.time;
            strikeStrength = Mathf.Lerp(1f, .6f, Mathf.InverseLerp(15f, 110f, far));

            pulses.Clear();
            int count = Random.Range(2, 5); float at = 0f;
            for (int k = 0; k < count; k++)
            {
                pulses.Add(new Pulse { At = at, Length = Random.Range(.06f, .13f), Strength = k == 0 ? 1f : Random.Range(.35f, .85f) });
                at += Random.Range(.07f, .16f);
            }
            BuildBolt(foot, t.position);

            float distance = Vector3.Distance(t.position, foot) + 120f;   // the flash is ground to cloud; most of the sound comes from up there
            pending.Add(new Pending { At = Time.time + distance / 343f, Volume = ThunderVolume * Mathf.Lerp(1f, .5f, Mathf.InverseLerp(150f, 380f, distance)), Clip = distance < 210f ? 0 : distance < 300f ? 1 : 2 });
            Atmosphere.StormLightFrom = (look - (foot + Vector3.up * 70f)).normalized;
        }

        /// <summary>A random walk from the cloud base to the foot, with two or three branches that fork off downward and die out.</summary>
        void BuildBolt(Vector3 foot, Vector3 eye)
        {
            verts.Clear(); uvs.Clear(); cols.Clear(); tris.Clear();
            float height = Random.Range(150f, 210f);
            Vector3 top = foot + new Vector3(Random.Range(-35f, 35f), height, Random.Range(-35f, 35f));
            const int steps = 16;
            var path = new Vector3[steps + 1];
            for (int k = 0; k <= steps; k++)
            {
                float f = k / (float)steps, slack = Mathf.Sin(f * Mathf.PI);   // free in the middle, pinned at both ends
                path[k] = Vector3.Lerp(top, foot, f) + new Vector3(Random.Range(-1f, 1f), Random.Range(-.3f, .3f), Random.Range(-1f, 1f)) * (height * .055f * slack);
            }
            for (int k = 0; k < steps; k++) Ribbon(path[k], path[k + 1], eye, 1.5f, 1f);
            int branches = Random.Range(2, 4);
            for (int b = 0; b < branches; b++)
            {
                int from = Random.Range(3, steps - 3);
                Vector3 at = path[from], drift = new Vector3(Random.Range(-1f, 1f), -Random.Range(.5f, 1.1f), Random.Range(-1f, 1f)).normalized;
                int length = Random.Range(4, 8);
                for (int k = 0; k < length; k++)
                {
                    Vector3 next = at + (drift + new Vector3(Random.Range(-.5f, .5f), Random.Range(-.2f, .2f), Random.Range(-.5f, .5f))) * (height * .05f);
                    Ribbon(at, next, eye, .8f * (1f - k / (float)length) + .25f, .5f * (1f - k / (float)length));
                    at = next;
                }
            }
            bolt.Clear();
            bolt.SetVertices(verts); bolt.SetUVs(0, uvs); bolt.SetColors(cols); bolt.SetTriangles(tris, 0);
            bolt.bounds = new Bounds(foot + Vector3.up * 100f, new Vector3(400f, 400f, 400f));
        }

        void Ribbon(Vector3 a, Vector3 b, Vector3 eye, float width, float brightness)
        {
            Vector3 side = Vector3.Cross(b - a, eye - a).normalized * (width * .38f * (1f + Vector3.Distance(eye, a) / 160f));   // wider far off, so it never thins below a pixel
            int v0 = verts.Count;
            verts.Add(a - side); verts.Add(a + side); verts.Add(b + side); verts.Add(b - side);
            uvs.Add(new Vector2(-1f, 0f)); uvs.Add(new Vector2(1f, 0f)); uvs.Add(new Vector2(1f, 1f)); uvs.Add(new Vector2(-1f, 1f));
            var c = new Color(1f, 1f, 1f, brightness); cols.Add(c); cols.Add(c); cols.Add(c); cols.Add(c);
            tris.Add(v0); tris.Add(v0 + 1); tris.Add(v0 + 2); tris.Add(v0); tris.Add(v0 + 2); tris.Add(v0 + 3);
        }

        void Update()
        {
            float rain = Mathf.Clamp01(Atmosphere.RainNow);
            if (Time.time >= nextStrike)
            {
                nextStrike = Time.time + Mathf.Lerp(Every.y, Every.x, rain) * Random.Range(.6f, 1.4f);
                if (rain > .12f) Strike();
            }
            float since = Time.time - struckAt, flash = 0f;
            for (int k = 0; k < pulses.Count; k++)
            {
                float a = since - pulses[k].At;
                if (a >= 0f && a < pulses[k].Length) flash = Mathf.Max(flash, pulses[k].Strength * (1f - a / pulses[k].Length));
            }
            flash *= strikeStrength;
            Atmosphere.StormFlash = flash;
            boltMaterial.SetColor("_Tint", Color.white * flash * 1.6f);
            glowMaterial.SetColor("_Tint", Color.white * flash);
            flashLight.enabled = flash > .01f;
            if (flashLight.enabled) { flashLight.transform.position = struckWhere + Vector3.up * 9f; flashLight.intensity = 320f * flash; }

            for (int k = pending.Count - 1; k >= 0; k--)
            {
                if (Time.time < pending[k].At) continue;
                voice.PlayOneShot(thunder[pending[k].Clip], pending[k].Volume);
                pending.RemoveAt(k);
            }
        }
    }
}
