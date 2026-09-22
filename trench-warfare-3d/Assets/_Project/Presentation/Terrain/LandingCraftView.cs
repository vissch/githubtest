// Phase: B2 (implemented) — the boats the reinforcements arrive in (SeaLandingSystem decides where each one is;
// nothing here moves anything). A beetle: the bluff steel lighter with a bow ramp that put men ashore at Gallipoli
// in 1915, which is the right boat for this war and reads at any distance as a box with a door in the front.
//
// Six hulls at most, so they are drawn one at a time with Graphics.RenderMesh — three meshes a craft (hull, ramp,
// the band in its side's colour) rather than an instanced batch, which would cost more to set up than it saves.
//
// The craft rides the same swell the sea shader draws, worked out from the same constants, so a boat sits in the
// water instead of on it: it heaves, pitches into the face of the wave and rolls a little, and all of that stops
// dead the moment it grounds. Its wake and the wash round its bow are rings on the water (WaterRings), which the
// sea and the river already draw, so the boat costs no effect of its own.
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim.Match;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed class LandingCraftView : MonoBehaviour
    {
        public SimHost Host;
        public const float Length = 11.4f, Beam = 3.7f, Freeboard = 1.25f, RampLength = 3.4f;

        Mesh hull, ramp, band, steamer;
        Material steel, rust, paint;
        /// <summary>The owner's gunboat (Resources/Vehicles/Cutter, Tools/crabsplit.py): what stands off the beach.
        /// Null until it loads, and the welded steamer below is what is drawn in its place if it never does.</summary>
        TW.Presentation.Tactical.TankModel cutter;
        Material cutterMat;
        MapData map;
        float nextWake;
        bool subscribed;
        /// <summary>When each gunboat last fired (LandingCraftView flashes its gun and lets the smoke drift off).</summary>
        readonly float[] shipFired = new float[TW.Sim.Match.SeaLandingSystem.Ships];
        /// <summary>The fleet that is always there: ships standing off in the haze and the hulks of earlier waves on
        /// the sand. Worked out once from the map, drawn like the craft, and part of no simulation.</summary>
        struct Hulk { public Vector3 At; public Quaternion Turn; public float Size; public bool Ship; public bool Broken; }
        Hulk[] fleet;

        void OnSimEvent(TW.Sim.SimEvent e)
        {
            // a gunboat lays a shell inland: the burst itself is BlastSystem's, this is the gun going off out at sea
            if (e.Type != TW.Sim.SimEventType.ShipFired) return;
            if (e.A >= 0 && e.A < shipFired.Length) shipFired[e.A] = Time.timeSinceLevelLoad;
        }

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            foreach (var o in new Object[] { hull, ramp, band, steamer, steel, rust, paint, cutterMat }) if (o != null) DestroyImmediate(o);
        }

        // ---- the shape ----------------------------------------------------------------------------------------
        /// <summary>Boxes welded into one mesh, each given in metres about the craft's own origin (midships, waterline).</summary>
        static Mesh Weld(string name, params (Vector3 centre, Vector3 size, Vector3 euler)[] parts)
        {
            var cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            var ci = new CombineInstance[parts.Length];
            for (int i = 0; i < parts.Length; i++)
                ci[i] = new CombineInstance { mesh = cube, transform = Matrix4x4.TRS(parts[i].centre, Quaternion.Euler(parts[i].euler), parts[i].size) };
            var mesh = new Mesh { name = name, hideFlags = HideFlags.DontSave };
            mesh.CombineMeshes(ci, true, true);
            mesh.RecalculateNormals(); mesh.RecalculateBounds();
            // the ink outline is pushed out along UV3, not the face normal: welded boxes would come apart at every
            // corner without it. Same rule as BattlefieldKit.Combine, so a boat is outlined like everything else.
            var verts = mesh.vertices; var normals = mesh.normals;
            var sum = new System.Collections.Generic.Dictionary<Vector3Int, Vector3>();
            var keys = new Vector3Int[verts.Length];
            for (int i = 0; i < verts.Length; i++)
            {
                keys[i] = new Vector3Int(Mathf.RoundToInt(verts[i].x * 500f), Mathf.RoundToInt(verts[i].y * 500f), Mathf.RoundToInt(verts[i].z * 500f));
                sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
            }
            var smooth = new System.Collections.Generic.List<Vector3>(verts.Length);
            for (int i = 0; i < verts.Length; i++) smooth.Add(sum[keys[i]].normalized);
            mesh.SetUVs(3, smooth);
            return mesh;
        }

        void BuildMeshes()
        {
            float half = Length * .5f, side = Beam * .5f;
            // hull: a flat-bottomed box with slabbed sides, a raised coaming aft, a wheelhouse and a stub mast. The bow
            // is cut back at the top so the ramp has something to hinge from.
            hull = Weld("Beetle",
                (new Vector3(0f, -.25f, 0f), new Vector3(Beam, .5f, Length), Vector3.zero),                       // bottom
                (new Vector3(-side + .12f, Freeboard * .5f - .2f, -.4f), new Vector3(.24f, Freeboard, Length - 1.6f), new Vector3(0f, 0f, -4f)),
                (new Vector3(side - .12f, Freeboard * .5f - .2f, -.4f), new Vector3(.24f, Freeboard, Length - 1.6f), new Vector3(0f, 0f, 4f)),
                (new Vector3(0f, .12f, -half + .3f), new Vector3(Beam - .1f, .9f, .6f), new Vector3(-12f, 0f, 0f)),  // transom
                (new Vector3(0f, .55f, -half + 1.5f), new Vector3(Beam - .5f, 1.1f, 1.9f), Vector3.zero),            // engine coaming
                (new Vector3(0f, 1.35f, -half + 1.7f), new Vector3(1.5f, .95f, 1.3f), Vector3.zero),                 // wheelhouse
                (new Vector3(0f, 2.3f, -half + 2.1f), new Vector3(.12f, 2.2f, .12f), new Vector3(6f, 0f, 0f)),       // mast
                (new Vector3(0f, .35f, half - .55f), new Vector3(Beam - .3f, .9f, .45f), new Vector3(24f, 0f, 0f)),  // the bow, raked
                (new Vector3(0f, -.05f, 1.2f), new Vector3(Beam - .7f, .35f, 4.2f), Vector3.zero));                  // the deck men stand on
            // the ramp hangs from the bow lip: its own origin is the hinge, so a rotation is all the animation it needs
            ramp = Weld("Ramp",
                (new Vector3(0f, 0f, RampLength * .5f), new Vector3(Beam - .55f, .16f, RampLength), Vector3.zero),
                (new Vector3(-(Beam - .7f) * .5f, .18f, RampLength * .5f), new Vector3(.14f, .5f, RampLength), Vector3.zero),
                (new Vector3((Beam - .7f) * .5f, .18f, RampLength * .5f), new Vector3(.14f, .5f, RampLength), Vector3.zero));
            band = Weld("Band",
                (new Vector3(-side + .02f, Freeboard * .62f, -.4f), new Vector3(.18f, .30f, Length - 3.4f), new Vector3(0f, 0f, -4f)),
                (new Vector3(side - .02f, Freeboard * .62f, -.4f), new Vector3(.18f, .30f, Length - 3.4f), new Vector3(0f, 0f, 4f)));

            // a steamer for the horizon: the same language, four times the size, with a funnel to read against the sky
            steamer = Weld("Steamer",
                (new Vector3(0f, 1.6f, 0f), new Vector3(9f, 4.4f, 46f), Vector3.zero),
                (new Vector3(0f, 3.4f, -6f), new Vector3(8.2f, 1.2f, 30f), Vector3.zero),
                (new Vector3(0f, 5.2f, -4f), new Vector3(6.4f, 3.2f, 9f), Vector3.zero),                  // bridge and house
                (new Vector3(0f, 8.0f, -3f), new Vector3(4.2f, 2.4f, 5f), Vector3.zero),
                (new Vector3(0f, 11.2f, -4.5f), new Vector3(2.6f, 5.2f, 2.6f), new Vector3(-4f, 0f, 0f)),  // funnel
                (new Vector3(0f, 10f, 9f), new Vector3(.5f, 13f, .5f), new Vector3(3f, 0f, 0f)),           // foremast
                (new Vector3(0f, 9f, -15f), new Vector3(.45f, 11f, .45f), new Vector3(-3f, 0f, 0f)),       // mainmast
                (new Vector3(0f, 3.2f, 21f), new Vector3(6.5f, 2.6f, 5f), new Vector3(9f, 0f, 0f)),        // the raked bow
                (new Vector3(0f, 4.6f, 13f), new Vector3(3.4f, 1.6f, 4f), Vector3.zero));                  // derrick house

            cutter = TW.Presentation.Tactical.TankModel.Load("Cutter", 0, "Body");
            var tank = Shader.Find("TW/Tank (URP)");
            if (cutter != null && tank != null)
            {
                cutterMat = new Material(tank) { hideFlags = HideFlags.DontSave, name = "Cutter" };
                var atlas = Resources.Load<Texture2D>("Vehicles/CutterAtlas");
                if (atlas != null) cutterMat.SetTexture("_BaseMap", atlas);
                cutterMat.SetFloat("_OutlineWidth", 2.2f);
            }

            steel = Toon(new Color(.44f, .46f, .47f));   // in the night look a hull this dark reads as a hole in the beach
            rust = Toon(new Color(.50f, .39f, .31f));
            // the two sides, as the tanks wear them: player 0 cold, player 1 warm
            paint = Toon(new Color(.88f, .25f, .16f));
        }

        static Material Toon(Color c)
        {
            var m = new Material(Shader.Find("TW/Toon (URP)")) { hideFlags = HideFlags.DontSave };
            m.SetColor("_BaseColor", c);
            m.SetFloat("_OutlineWidth", 2.2f);
            return m;
        }

        /// <summary>Where the ships stand and where the wrecks lie: hashed off the map, so a sector always has the
        /// same fleet off it. Ships keep well out; hulks lie off the ends of the beach, clear of the landing lanes.</summary>
        void BuildFleet()
        {
            float w = map.SizeMeters.x, away = map.SeaAway;
            var list = new System.Collections.Generic.List<Hulk>();
            float R(int i, int s) { uint h = (uint)(i * 0x9E3779B1u) ^ (uint)(s * 0x85EBCA77u) ^ (uint)map.MapId * 2654435761u; h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12; return (h & 0xFFFF) / 65535f; }
            // the gunboats lie where the SIMULATION says they lie: they shell the shore, so their positions are
            // part of the match rather than a pretty arrangement made here
            var landing = Host.Local.Landing;
            for (int i = 0; i < TW.Sim.Match.SeaLandingSystem.Ships; i++)
            {
                var sim = landing != null ? landing.ShipAt(i) : default;
                Vector3 at = landing != null ? new Vector3(sim.x, sim.y, sim.z)
                                             : new Vector3(w * (.1f + R(i, 2) * .9f), map.SeaLevel, map.ShoreZ + away * (360f + R(i, 1) * 420f));
                list.Add(new Hulk
                {
                    At = at,
                    Turn = Quaternion.Euler(0f, 74f + R(i, 4) * 32f + (R(i, 5) < .5f ? 180f : 0f), 0f),
                    Size = .9f + R(i, 6) * .5f, Ship = true,
                });
            }
            for (int i = 0; i < 5; i++)
            {
                // two ends of the beach, and one or two further out where an earlier wave was caught
                bool inshore = i < 3;
                float x = i % 2 == 0 ? -12f - R(i, 7) * 70f : w + 12f + R(i, 8) * 70f;
                float off = inshore ? -1.5f + R(i, 9) * 6f : 14f + R(i, 10) * 26f;
                float ground = map.SeaLevel - (inshore ? 0f : 1f);
                list.Add(new Hulk
                {
                    At = new Vector3(x, ground, map.ShoreZ + away * off),
                    Turn = Quaternion.Euler((R(i, 11) - .3f) * 26f, R(i, 12) * 360f, (R(i, 13) - .5f) * 44f),
                    Size = 1f + R(i, 14) * .35f, Broken = true,
                });
            }
            fleet = list.ToArray();
        }

        void DrawFleet()
        {
            if (fleet == null) BuildFleet();
            float now = Time.timeSinceLevelLoad;
            for (int i = 0; i < fleet.Length; i++)
            {
                var f = fleet[i];
                var at = f.At;
                var turn = f.Turn;
                if (f.Ship)
                {
                    // a ship at anchor works slowly against the swell; it is 400 m away, so this is all it needs
                    float heave = Swell(map, at.x, at.z, 12f, now * .35f) * 1.6f;
                    at.y = map.SeaLevel + heave - 1.2f;
                    turn = f.Turn * Quaternion.Euler(Mathf.Sin(now * .21f + i) * 1.4f, 0f, Mathf.Sin(now * .17f + i * 2f) * 2.6f);
                }
                var body = Matrix4x4.TRS(at, turn, Vector3.one * f.Size);
                if (f.Ship && cutter != null && cutterMat != null)
                {
                    // the gunboat, drawn part by part off its own model: hull, and the gun on it laid out to sea
                    var lod = cutter.Lods[0];
                    for (int k = 0; k < lod.Parts.Count; k++)
                    {
                        var part = lod.Parts[k];
                        var local = Matrix4x4.TRS(part.Local, part.LocalRot, Vector3.one);
                        if (part.Role == TW.Presentation.Tactical.TankPartRole.Gun)
                        {
                            // it lays the gun slowly along the shore, and kicks when it fires
                            float since = i < shipFired.Length ? now - shipFired[i] : 99f;
                            float kick = since < .45f ? Mathf.Sin((1f - since / .45f) * Mathf.PI) * 0.5f : 0f;
                            local *= Matrix4x4.TRS(new Vector3(0f, 0f, -kick), Quaternion.Euler(-4f - kick * 9f, Mathf.Sin(now * .13f + i) * 22f, 0f), Vector3.one);
                        }
                        Graphics.RenderMesh(new RenderParams(cutterMat) { shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true },
                            part.Mesh, 0, body * local);
                    }
                    float fired = i < shipFired.Length ? now - shipFired[i] : 99f;
                    if (fired < 1.2f && SceneHooks.AddRing != null && fired < Time.deltaTime * 3f)
                        SceneHooks.AddRing(at.x, at.z - map.SeaAway * -9f, 7f);   // the shock of it on the water off her bow
                    continue;
                }
                var rp = new RenderParams(f.Ship ? steel : rust) { shadowCastingMode = f.Ship ? ShadowCastingMode.Off : ShadowCastingMode.On, receiveShadows = true };
                Graphics.RenderMesh(rp, f.Ship ? steamer : hull, 0, body);
                if (!f.Broken) continue;
                // a wreck's ramp is down in the sand where the men went off it, or torn half off
                var hinge = body * Matrix4x4.TRS(new Vector3(0f, Freeboard * .55f, Length * .5f - .3f), Quaternion.Euler(24f, (i % 3 - 1) * 14f, (i % 2) * 18f), Vector3.one);
                Graphics.RenderMesh(rp, ramp, 0, hinge);
            }
        }

        // ---- the swell, as the sea shader draws it ------------------------------------------------------------
        /// <summary>The height of the water at a point, from the same two swells TW/Sea displaces its mesh with.</summary>
        public static float Swell(MapData map, float x, float z, float depth, float t)
        {
            float shoal = Mathf.Clamp01(depth / 3.5f);
            float amp = Ocean.SwellHeight * Mathf.Lerp(.15f, 1f, shoal) * Mathf.Clamp01(depth / .55f);
            Vector2 dir = new Vector2(0f, -map.SeaAway);
            Vector2 across = new Vector2(-dir.y, dir.x);
            Vector2 second = (dir * .92f + across * .38f).normalized;
            float k1 = 2f * Mathf.PI / Mathf.Max(2f, Ocean.SwellLength * Mathf.Lerp(.45f, 1f, shoal));
            float k2 = 2f * Mathf.PI / Mathf.Max(2f, Ocean.SwellLength * .61f * Mathf.Lerp(.45f, 1f, shoal));
            float p1 = (dir.x * x + dir.y * z) * k1 - t * Ocean.SwellSpeed * k1;
            float p2 = (second.x * x + second.y * z) * k2 - t * Ocean.SwellSpeed * .8f * k2;
            return Mathf.Sin(p1) * amp + Mathf.Sin(p2) * amp * .42f;
        }

        // ---- drawing ------------------------------------------------------------------------------------------
        void Update()
        {
            if (Host == null || Host.Local == null) return;
            var landing = Host.Local.Landing;
            if (landing == null) { enabled = false; return; }
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (hull == null) BuildMeshes();
            map = Host.Local.Map;
            var view = GetComponent<GreyboxTerrainView>();
            float now = Time.timeSinceLevelLoad;
            bool wake = Time.time >= nextWake;
            if (wake) nextWake = Time.time + .18f;

            DrawFleet();
            for (int i = 0; i < landing.Count; i++)
            {
                var state = landing.StateOf(i);
                if (state == LandingState.Idle) continue;
                var at = landing.PositionOf(i);
                float ground = view != null && view.Surface != null ? Ground(view, at.x, at.z) : map.SeaLevel - 3f;
                float depth = Mathf.Max(0f, map.SeaLevel - ground);
                bool afloat = state == LandingState.RunIn || state == LandingState.Retracting;

                // afloat it rides the swell; aground it sits on the sand, bow up, and stops moving
                float heave = afloat ? Swell(map, at.x, at.z, depth, now) : 0f;
                float ahead = Swell(map, at.x, at.z + map.SeaAway * -3f, depth, now) - Swell(map, at.x, at.z + map.SeaAway * 3f, depth, now);
                float roll = afloat ? (Swell(map, at.x + 2f, at.z, depth, now) - Swell(map, at.x - 2f, at.z, depth, now)) * 6f : 0f;
                float pitch = afloat ? ahead * 4.5f : -4.5f;
                // afloat it floats at its own draught; aground its flat bottom is IN the sand, not hovering over it
                float y = afloat ? map.SeaLevel + heave - .1f : ground + .45f;

                var rotation = Quaternion.Euler(pitch, landing.YawOf(i) * Mathf.Rad2Deg, roll);
                var body = Matrix4x4.TRS(new Vector3(at.x, y, at.z), rotation, Vector3.one);
                var rp = new RenderParams(steel) { shadowCastingMode = ShadowCastingMode.On, receiveShadows = true };
                Graphics.RenderMesh(rp, hull, 0, body);
                rp.material = landing.TeamOf(i) == 1 ? paint : rust;
                Graphics.RenderMesh(rp, band, 0, body);

                // the ramp: shipped upright at sea, down on the sand when the men go
                float down = landing.RampOf(i);
                var hinge = body * Matrix4x4.TRS(new Vector3(0f, Freeboard * .55f, Length * .5f - .3f), Quaternion.Euler(Mathf.Lerp(-78f, 21f, down), 0f, 0f), Vector3.one);
                rp.material = steel;
                Graphics.RenderMesh(rp, ramp, 0, hinge);

                if (!wake) continue;
                // the wake, and the wash that works round a grounded hull: rings the water already knows how to draw
                if (SceneHooks.AddRing == null) continue;
                if (afloat)
                {
                    float sternZ = at.z + map.SeaAway * Length * .5f;          // the stern is the seaward end
                    SceneHooks.AddRing(at.x, sternZ, 4.5f);
                    SceneHooks.AddRing(at.x + Beam * .5f, sternZ - map.SeaAway * 2f, 2.6f);
                    SceneHooks.AddRing(at.x - Beam * .5f, sternZ - map.SeaAway * 2f, 2.6f);
                }
                else if (Random.value < .5f) SceneHooks.AddRing(at.x + Random.Range(-Beam, Beam), at.z - map.SeaAway * Random.Range(1f, 5f), 3.2f);
            }
        }

        float Ground(GreyboxTerrainView view, float x, float z)
        {
            bool inside = x >= 0f && z >= 0f && x <= map.SizeMeters.x && z <= map.SizeMeters.y;
            return inside ? view.Surface.VisualHeight(x, z) : GreyboxTerrainView.SkirtHeight(map, x, z);
        }
    }
}
