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
// The freeze frame (owner, 2026-09-22): the world stops for FreezeSeconds while a strike flickers, like a photograph
// taken by its light: Time.timeScale holds at 0 (the sim, the men, the rain and every effect stand still; the camera and
// the storm run on unscaled time), the flicker is spread across the hold so the frozen picture stays lit, and time
// comes back as the last flicker dies. 0 turns it off. In single player only: a lockstep peer cannot stop the sim.
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
        [Tooltip("The world holds still this long while a strike flickers (seconds; 0 = off).")]
        public float FreezeSeconds = 1f;
        float frozenUntil = -1f, scaleBefore = 1f;
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
            gm.SetUVs(1, new List<Vector4> { new Vector4(13f, .2f, 0f, .1f), new Vector4(13f, .2f, 0f, .1f), new Vector4(13f, .2f, 0f, .1f), new Vector4(13f, .2f, 0f, .1f) });
            gm.SetColors(new List<Color> { new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f), new Color(.7f, .82f, 1f, .7f) });
            gm.SetTriangles(new[] { 0, 2, 1, 0, 3, 2 }, 0); gm.bounds = new Bounds(Vector3.zero, Vector3.one * 4000f);
            glowCard.AddComponent<MeshFilter>().sharedMesh = gm;
            var gr = glowCard.AddComponent<MeshRenderer>(); gr.sharedMaterial = glowMaterial; gr.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off;

            voice = gameObject.AddComponent<AudioSource>();
            voice.spatialBlend = 0f; voice.playOnAwake = false;
            // deliberately NOT creating an AudioListener here. FindFirstObjectByType skips inactive objects, so a
            // frame where the camera is inactive used to add a SECOND listener, which Unity warns about and which
            // makes global volume non-deterministic. Listener ownership belongs to the camera, not to the weather.
            thunder = new[] { MakeThunder(0, 1f, 5.5f), MakeThunder(1, .45f, 6.5f), MakeThunder(2, .08f, 7.5f) };   // near: a crack; far: all rumble
            nextStrike = Time.unscaledTime + 9f;
        }

        void OnDisable() { Thaw(); }

        void OnDestroy()
        {
            Thaw();
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
                // a near strike starts at once; a far one creeps in over half a second, the crack long since lost on the way
                float attack = Mathf.Clamp01(t / Mathf.Lerp(.6f, .02f, crack));
                float envelope = (roll + .25f) * Mathf.Exp(-t * .55f) * attack * Mathf.Clamp01((seconds - t) * 2f);
                float snap = crack * white * Mathf.Exp(-t * 22f) * .9f + crack * low * Mathf.Exp(-t * 6f) * 2.2f;
                data[i] = (low * 3.2f + lower * 9f) * envelope + snap;
            }
            // no hard clipping: round the peaks off (tanh), then bring the loudest sample to 0.9
            float peak = 0f;
            for (int i = 0; i < n; i++) { data[i] = (float)System.Math.Tanh(data[i] * 1.2f); peak = Mathf.Max(peak, Mathf.Abs(data[i])); }
            float gain = peak > 0f ? .9f / peak : 1f;
            for (int i = 0; i < n; i++) data[i] *= gain;
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
            struckWhere = foot; struckAt = Time.unscaledTime;
            strikeStrength = Mathf.Lerp(1f, .6f, Mathf.InverseLerp(15f, 110f, far));

            pulses.Clear();
            int count = Random.Range(2, 5); float at = 0f;
            // frozen, the flickers come a little further apart and the last one fades slowly over the rest of the hold, so the
            // still picture stays lit and goes dark just as time comes back (a dark frozen frame reads as a stall)
            bool freeze = FreezeSeconds > 0f;
            for (int k = 0; k < count; k++)
            {
                bool last = k == count - 1;
                float length = Random.Range(.06f, .13f);
                if (freeze && last) length = Mathf.Max(.15f, FreezeSeconds - at);
                pulses.Add(new Pulse { At = at, Length = length, Strength = k == 0 ? 1f : Random.Range(.35f, .85f) * (freeze && last ? .8f : 1f) });
                at += Random.Range(.07f, .16f) * (freeze ? 1.35f : 1f);
            }
            if (freeze) Freeze(FreezeSeconds);
            BuildBolt(foot, t.position);

            float distance = Vector3.Distance(t.position, foot) + 120f;   // the flash is ground to cloud; most of the sound comes from up there
            pending.Add(new Pending { At = Time.unscaledTime + distance / 343f, Volume = ThunderVolume * Mathf.Lerp(1f, .5f, Mathf.InverseLerp(150f, 380f, distance)), Clip = distance < 210f ? 0 : distance < 300f ? 1 : 2 });
            Atmosphere.StormLightFrom = (look - (foot + Vector3.up * 70f)).normalized;
        }

        /// <summary>A random walk from the cloud base to the foot, with two or three branches that fork off downward and die out.</summary>
        void BuildBolt(Vector3 foot, Vector3 eye)
        {
            verts.Clear(); uvs.Clear(); cols.Clear(); tris.Clear();
            float height = Random.Range(150f, 210f);
            Vector3 top = foot + new Vector3(Random.Range(-35f, 35f), height, Random.Range(-35f, 35f));
            // The camera only ever sees the bottom fifth of the bolt, so that is where the detail goes: the points crowd
            // toward the ground, and the zigzag is a random walk (each kink starts from the last one) with its drift taken
            // back out, so it is as crooked at the foot as in the middle and still lands exactly where it was aimed.
            const int steps = 24;
            var path = new Vector3[steps + 1]; var wander = new Vector3[steps + 1];
            for (int k = 1; k <= steps; k++) wander[k] = wander[k - 1] + new Vector3(Random.Range(-1f, 1f), 0f, Random.Range(-1f, 1f)) * (height * .018f);
            for (int k = 0; k <= steps; k++)
            {
                float f = Mathf.Pow(k / (float)steps, .55f);
                path[k] = Vector3.Lerp(top, foot, f) + wander[k] - wander[steps] * (k / (float)steps);
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

        /// <summary>Stop the world for this many (unscaled) seconds; a second strike inside the hold extends it.</summary>
        void Freeze(float seconds)
        {
            if (frozenUntil < 0f) scaleBefore = Time.timeScale;
            frozenUntil = Mathf.Max(frozenUntil, Time.unscaledTime + seconds);
            Time.timeScale = 0f;
        }

        void Thaw()
        {
            if (frozenUntil < 0f) return;
            Time.timeScale = scaleBefore > 0f ? scaleBefore : 1f;
            frozenUntil = -1f;
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.StormUpdate.Auto();
            if (frozenUntil >= 0f && Time.unscaledTime >= frozenUntil) Thaw();
            float rain = Mathf.Clamp01(Atmosphere.RainNow);
            if (Time.unscaledTime >= nextStrike)
            {
                nextStrike = Time.unscaledTime + Mathf.Lerp(Every.y, Every.x, rain) * Random.Range(.6f, 1.4f);
                if (rain > .12f && Time.timeScale > 0f) Strike();   // never while something else has the game paused
            }
            float since = Time.unscaledTime - struckAt, flash = 0f;
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
            if (flashLight.enabled) { flashLight.transform.position = struckWhere + Vector3.up * 3f; flashLight.intensity = 320f * flash; }

            for (int k = pending.Count - 1; k >= 0; k--)
            {
                if (Time.unscaledTime < pending[k].At) continue;
                // the ambience bus only: Master is already on the listener, and applying it here too squared it.
                // Baked at fire time rather than live, which is right for a one-shot — a 6 s thunderclap keeps the
                // ambience level it was fired at, while Master stays live on the listener.
                float level = AudioLevels.Ambience;
                if (level > 0.001f) voice.PlayOneShot(thunder[pending[k].Clip], pending[k].Volume * level);
                pending.RemoveAt(k);
            }
        }
    }
}
