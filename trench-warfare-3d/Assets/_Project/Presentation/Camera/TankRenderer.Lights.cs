// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: MachineLamps, MachineSockets, SceneHooks
// (MachineGlows, MachineLight: NightLights.Machines.cs), Knobs.
// A machine's own lights, behind knobs that draw today's machines at 0 (the fx.deathAbsurd rule):
//  - tank.lamps: four running lamps on the corners of every live machine's hull (MachineLamps), as glow cards that
//    NightLights draws with its flash glows (no draw call of their own), at night, the nearest machines first; out
//    when the machine is dead, knocked out or stalled. The rear pair burns brighter than the front, as tail lamps do;
//  - tank.lampHue: 0 the side's colour, 1 the red Dust Front's hulls carry, for the owner to compare;
//  - tank.lampSize: a lamp's card across, in metres, never under 0.006 of its distance, so it holds on the screen;
//  - tank.exhaustGlow: the exhausts glow with the throttle, and the Maw's furnace mouth with its fire;
//  - tank.lightReach: how far from the camera a machine still shows them (110 m; the standard view stands 77.6 m off).
// With NightLights' machine pool on (lights.machinePool, 0 by default), real lights of their own: a burning machine's
// fire, held while it burns; its cook-off, 1.2 s; the Maw's furnace, held. A gun keeps the shared flash it always had.
using UnityEngine;
using TW.Sim;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        public const int MaxMachineGlows = 128, MachineLightsPerFrame = 8;
        const float LampRear = 1f, LampFront = 0.55f, LampFlicker = 0.03f, LampHaze = 0.3f, LampPerMetre = 0.006f;
        const float CookOffPeak = 30f, CookOffSeconds = 1.2f, MachineLightReach = 10f;
        /// <summary>The red of Dust Front's running lamps (tank.lampHue 1).</summary>
        public static readonly Color TrailerRed = new Color(1f, 0.16f, 0.07f);
        static readonly Color ExhaustHot = new Color(1f, 0.42f, 0.12f), FurnaceHot = new Color(1f, 0.48f, 0.16f), FireHot = new Color(1f, 0.52f, 0.2f);
        bool lampsOn; float lampHue, lampSize = 0.45f, exhaustGlow, lightReach = 110f;
        int lampKnobs = -1;
        readonly Vector3[] glowPos = new Vector3[MaxMachineGlows];
        readonly Color[] glowCol = new Color[MaxMachineGlows];
        readonly float[] glowSize = new float[MaxMachineGlows];
        float[] litD = new float[64]; View[] litV = new View[64];
        readonly System.Collections.Generic.Dictionary<TankModel, Vector3[]> lampsOf = new System.Collections.Generic.Dictionary<TankModel, Vector3[]>();
        readonly System.Collections.Generic.Dictionary<TankModel, Vector4> furnaceOf = new System.Collections.Generic.Dictionary<TankModel, Vector4>();   // w 1: it has one

        void LampKnobs()
        {
            if (lampKnobs == Knobs.Generation) return;
            lampKnobs = Knobs.Generation;
            lampsOn = Knobs.Get("tank.lamps", false);
            lampHue = Mathf.Clamp01(Knobs.Get("tank.lampHue", 0f));
            lampSize = Mathf.Max(0.05f, Knobs.Get("tank.lampSize", 0.45f));
            exhaustGlow = Mathf.Clamp01(Knobs.Get("tank.exhaustGlow", 0f));
            lightReach = Mathf.Max(0f, Knobs.Get("tank.lightReach", 110f));
        }

        /// <summary>The colour of a side's running lamps: its own colour, or toward Dust Front's red with tank.lampHue.</summary>
        public static Color LampColour(byte team, float hue) => Color.Lerp(team == 1 ? TeamB : TeamA, TrailerRed, Mathf.Clamp01(hue));

        /// <summary>How wide a lamp's card is drawn at this distance: its own size, or wider far off, so it stays a point
        /// of light and does not shrink to nothing under the bloom's threshold.</summary>
        public static float LampCard(float size, float distance) => Mathf.Max(size, LampPerMetre * distance);

        Vector3[] LampsFor(TankModel m)
        {
            if (!lampsOf.TryGetValue(m, out var at)) { at = MachineLamps.Place(m); lampsOf[m] = at; }
            return at;
        }

        bool FurnaceFor(TankModel m, out Vector3 at)
        {
            if (!furnaceOf.TryGetValue(m, out var f))
            {
                Vector3 c = default;
                bool has = m.Archetype == VehicleArchetype.Maw && MachineLamps.Furnace(m, out c);   // the one machine TW/Tank lights a furnace on
                f = has ? new Vector4(c.x, c.y, c.z, 1f) : Vector4.zero;
                furnaceOf[m] = f;
            }
            at = f;
            return f.w > 0f;
        }

        /// <summary>This frame's machine lights: the cards go to NightLights (drawn next frame, so each is put where its
        /// hull will be, one frame on), the nearest machines first; and, with its machine pool on, the lights asked for.</summary>
        void EmitMachineLights(float now)
        {
            LampKnobs();
            bool cards = SceneMood.Night && (lampsOn || exhaustGlow > 0f) && SceneHooks.MachineGlows != null;
            bool lights = SceneHooks.MachineLight != null;
            if (!cards && !lights) return;
            var cam = Camera.main;
            if (cam == null) return;
            Vector3 eye = cam.transform.position;
            // the machines in reach, nearest first (an insertion sort: a few dozen, and nothing allocated)
            int n = 0; float reach2 = lightReach * lightReach;
            foreach (var v in views.Values)
            {
                if (v.Dead || v.World == null) continue;
                float d2 = (v.Pos - eye).sqrMagnitude;
                if (d2 > reach2) continue;
                if (n == litD.Length) { System.Array.Resize(ref litD, n * 2); System.Array.Resize(ref litV, n * 2); }
                int j = n++;
                while (j > 0 && litD[j - 1] > d2) { litD[j] = litD[j - 1]; litV[j] = litV[j - 1]; j--; }
                litD[j] = d2; litV[j] = v;
            }
            int g = 0, asked = 0;
            for (int i = 0; i < n; i++)
            {
                var v = litV[i];
                if (cards && g < MaxMachineGlows) g = MachineCards(v, eye, g);
                if (lights && asked < MachineLightsPerFrame) asked += AskMachineLights(v, now);
            }
            for (int i = 0; i < n; i++) litV[i] = null;
            if (cards && g > 0) SceneHooks.MachineGlows(glowPos, glowCol, glowSize, g);
        }

        /// <summary>One machine's cards from index g on; returns the next free index.</summary>
        int MachineCards(View v, Vector3 eye, int g)
        {
            if (v.Stalled) return g;   // dead engine, knocked out: its lamps and its exhausts are out
            Vector3 ahead = v.Pos - v.LastPos; ahead.y = 0f;
            if (ahead.sqrMagnitude > 0.25f) ahead = ahead.normalized * 0.5f;   // a presenter's catch-up is not a speed
            var hull = v.World[0];
            if (lampsOn)
            {
                var lamps = LampsFor(v.Model);
                if (lamps != null)
                {
                    var c = LampColour(v.Team, lampHue);
                    for (int k = 0; k < 4 && g < MaxMachineGlows; k++)
                    {
                        float strength = k >= MachineLamps.RearLeft ? LampRear : LampFront;
                        g = Card(g, hull.MultiplyPoint3x4(lamps[k]) + ahead, eye, lampSize, new Color(c.r, c.g, c.b, strength));
                    }
                }
            }
            if (exhaustGlow > 0f)
            {
                float heat = exhaustGlow * (0.3f + 0.7f * v.Throttle);
                for (int k = 0; k < 2 && g < MaxMachineGlows; k++)
                {
                    var at = SocketWorld(v, "Socket_Exhaust" + k, out bool ok);
                    if (ok) g = Card(g, at + ahead, eye, 0.9f, new Color(ExhaustHot.r, ExhaustHot.g, ExhaustHot.b, heat));
                }
                if (g < MaxMachineGlows && FurnaceFor(v.Model, out Vector3 mouth))
                    g = Card(g, hull.MultiplyPoint3x4(mouth) + ahead, eye, 1.3f, new Color(FurnaceHot.r, FurnaceHot.g, FurnaceHot.b, exhaustGlow * 0.9f * v.Furnace));
            }
            return g;
        }

        /// <summary>A card: drawn a little toward the camera, so the plate it sits on does not cut it in half.</summary>
        int Card(int g, Vector3 at, Vector3 eye, float size, Color color)
        {
            Vector3 toEye = eye - at;
            float d = toEye.magnitude;
            float across = LampCard(size, d);
            if (d > 1e-3f) at += toEye * (Mathf.Min(0.35f, across * 0.5f) / d);
            glowPos[g] = at; glowCol[g] = color; glowSize[g] = across;
            return g + 1;
        }

        /// <summary>The lights one machine asks NightLights' machine pool for this frame; returns how many it asked.</summary>
        int AskMachineLights(View v, float now)
        {
            int asked = 0;
            if (v.Fire > 0.02f)
            {
                float flick = 0.82f + 0.18f * Mathf.Sin(now * 11f + v.Slot) * Mathf.Sin(now * 4.3f + v.Slot * 1.7f);
                var at = SocketWorld(v, "Socket_Fire0", out _) + Vector3.up * 0.8f;
                SceneHooks.MachineLight(v.Slot * 4 + 1, 2, at, FireHot, (3f + 6f * v.Fire) * flick, MachineLightReach, 0f);
                asked++;
            }
            if (!v.Stalled && FurnaceFor(v.Model, out Vector3 mouth))
            {
                var at = v.World[0].MultiplyPoint3x4(mouth);
                SceneHooks.MachineLight(v.Slot * 4 + 3, 1, at, FurnaceHot, 1.5f * v.Furnace, 6f, 0f);
                asked++;
            }
            return asked;
        }

        /// <summary>A cook-off's light, from the machine pool: the one moment a machine lights everything round it.</summary>
        void CookOffLight(View v, Vector3 at)
        {
            if (SceneHooks.MachineLight == null) return;
            var cam = Camera.main;
            if (cam != null && (cam.transform.position - at).sqrMagnitude > lightReach * lightReach) return;
            SceneHooks.MachineLight(v.Slot * 4 + 2, 3, at, FireHot, CookOffPeak, MachineLightReach, CookOffSeconds);
        }
    }
}
