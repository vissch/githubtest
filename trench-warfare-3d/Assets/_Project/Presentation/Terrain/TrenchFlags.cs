// Phase: B5 (the owner's idea of 2026-10-08, concept option A) — every trench gets a pole, and when it changes hands
// a frog at its base hauls up the taker's colour while the old flag flutters down. The front is told by flags
// instead of by the list top left.
//
// All presentation: the sim already says when a trench changes hands (SimEventType.TrenchCaptured, a = trench id,
// b = team) and where the trench is (MapData.Trenches). Nothing here writes to a world; the state and the poles'
// strength live in this component, the way TrenchSection's do. The sizes, the colours, the climb and the break
// are all TrenchFlagRules.
//
// What costs what: three instanced draws at the standard view (the poles, and one per side's cloth), the frog and
// its rope only with the camera among the men (SceneHooks.CloseUp), and past TrenchFlagRules.PoleFadeMeters the
// pole is dropped and the flag drawn Swell bigger so a far overview still reads as two colours. A swap also
// spends one CombatFx.DustDab (the Puff sheet) at the butt where the rope runs. Everything goes
// through FrameBudget.Draw (FrameBudgetCoverageTests reads the source).
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Terrain;
using TW.Presentation.Tactical;

namespace TW.Presentation.Terrain
{
    public sealed class TrenchFlags : MonoBehaviour
    {
        public SimHost Host;
        /// <summary>Who holds the flipbooks. Found on the camera rig the first frame; the flags draw fine without it.</summary>
        public CombatFx Fx;

        struct Pole
        {
            public Vector3 Anchor;
            public float Yaw;          // the parapet's facing: the flag flies across it
            public byte Owner;         // 255 neutral: a bare pole
            public byte Losing;        // the side whose flag is in the air (255 none)
            public float Since;        // seconds since the swap began
            public float Hp;
            public FlagState State;
            public bool Dust;          // a swap just began: owes one dab of dust at the butt
        }

        Pole[] poles = System.Array.Empty<Pole>();
        short[] byId = System.Array.Empty<short>();   // trench id -> index into poles, -1 unknown

        Mesh poleMesh, flagMesh, ropeMesh, frogMesh;
        Material poleMat, frogMat;
        readonly Material[] clothMat = new Material[2];
        Matrix4x4[] poleDraw = System.Array.Empty<Matrix4x4>(), frogDraw = System.Array.Empty<Matrix4x4>(), ropeDraw = System.Array.Empty<Matrix4x4>();
        readonly List<Matrix4x4>[] clothDraw = { new List<Matrix4x4>(), new List<Matrix4x4>() };
        bool built, subscribed;

        void OnDestroy()
        {
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
            if (poleMesh != null) Destroy(poleMesh);
            if (flagMesh != null) Destroy(flagMesh);
            if (poleMat != null) Destroy(poleMat);
            if (frogMat != null) Destroy(frogMat);
            for (int k = 0; k < 2; k++) if (clothMat[k] != null) Destroy(clothMat[k]);
        }

        void OnSimEvent(SimEvent e)
        {
            if (e.Type == SimEventType.TrenchCaptured) Swap(e.A, e.B);
        }

        /// <summary>A trench changes hands: the old flag is cut loose and the new one starts up the pole. A pole that
        /// is down flies nothing, but it still changes hands, so the colour is there again if it is ever rebuilt.</summary>
        void Swap(int trenchId, int team)
        {
            int i = Index(trenchId);
            if (i < 0) return;
            var p = poles[i];
            if (p.Owner == (byte)team) return;
            p.Losing = p.Owner;
            p.Owner = (byte)team;
            p.Since = 0f;
            p.Dust = true;
            poles[i] = p;
        }

        int Index(int trenchId)
            => trenchId >= 0 && trenchId < byId.Length ? byId[trenchId] : -1;

        /// <summary>Stage a capture from an eval or a test, exactly as the sim's event does.</summary>
        public void SwapForTests(int trench, int team) => Swap(trench, team);

        /// <summary>Shell a pole from an eval or a test (the shape of PropDestruction.StrikeForTests).</summary>
        public void StrikeForTests(int trench, float harm, bool heavy)
        {
            int i = Index(trench);
            if (i < 0) return;
            var p = poles[i];
            p.State = TrenchFlagRules.Apply(ref p.Hp, harm, heavy, p.State);
            poles[i] = p;
        }

        /// <summary>What a pole is doing, for an eval's report: its state, whose colour it flies, and how far into a
        /// swap it is.</summary>
        public (FlagState state, int owner, float since) ReportForTests(int trench)
        {
            int i = Index(trench);
            return i < 0 ? (FlagState.Gone, 255, 0f) : (poles[i].State, poles[i].Owner, poles[i].Since);
        }

        /// <summary>How many poles stand on this field (0 before the map is up).</summary>
        public int PoleCountForTests => poles.Length;

        void Build()
        {
            built = true;
            var map = Host.Local.Map;
            int count = map.Trenches.Length;
            poles = new Pole[count];
            int maxId = 0;
            for (int i = 0; i < count; i++) maxId = Mathf.Max(maxId, map.Trenches[i].Id);
            byId = new short[maxId + 1];
            for (int i = 0; i < byId.Length; i++) byId[i] = -1;
            for (int i = 0; i < count; i++)
            {
                var t = map.Trenches[i];
                poles[i] = new Pole
                {
                    Anchor = TrenchFlagRules.Anchor(t, map),
                    Yaw = t.FacingYaw,
                    Owner = t.OwnerTeam,
                    Losing = 255,
                    Since = TrenchFlagRules.FallSeconds,   // the flags a field starts with are already up
                    Hp = TrenchFlagRules.MaxHp,
                    State = FlagState.Intact,
                };
                if (t.Id >= 0) byId[t.Id] = (short)i;
            }
            poleDraw = new Matrix4x4[count];
            frogDraw = new Matrix4x4[count];
            ropeDraw = new Matrix4x4[count];

            poleMesh = Tapered();
            flagMesh = Pennant();
            ropeMesh = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            var frog = TankModel.Load("Croaker", VehicleArchetype.Croaker, "Hull");
            if (frog != null && frog.Lods[0] != null && frog.Lods[0].Parts.Count > 0) frogMesh = frog.Lods[0].Parts[0].Mesh;
            if (frogMesh == null) frogMesh = Resources.GetBuiltinResource<Mesh>("Capsule.fbx");

            var toon = Shader.Find("TW/Toon (URP)");
            if (toon == null) return;
            poleMat = Paint(toon, new Color(0.30f, 0.25f, 0.19f));            // a stripped sapling, dark against the mud
            frogMat = Paint(toon, new Color(0.26f, 0.33f, 0.21f));
            clothMat[0] = Paint(toon, TrenchFlagRules.Cloth(0));
            clothMat[1] = Paint(toon, TrenchFlagRules.Cloth(1));
        }

        static Material Paint(Shader shader, Color colour)
        {
            var m = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            m.SetColor("_BaseColor", colour);
            m.SetFloat("_OutlineWidth", 0.8f);
            return m;
        }

        /// <summary>A unit box tapered toward its top, pivot at the butt: the pole, scaled to its height.</summary>
        static Mesh Tapered()
        {
            const float butt = 0.5f, tip = 0.3f;   // of the scale's X/Z, so a 0.17 m scale is 17 cm across at the butt
            var v = new Vector3[8];
            for (int k = 0; k < 4; k++)
            {
                float sx = (k == 0 || k == 3) ? -1f : 1f, sz = k < 2 ? -1f : 1f;
                v[k] = new Vector3(sx * butt, 0f, sz * butt);
                v[k + 4] = new Vector3(sx * tip, 1f, sz * tip);
            }
            int[] tris =
            {
                0,1,5, 0,5,4,  1,2,6, 1,6,5,  2,3,7, 2,7,6,  3,0,4, 3,4,7,  4,5,6, 4,6,7,
            };
            var mesh = new Mesh { name = "FlagPole", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(v); mesh.SetTriangles(tris, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds();
            return mesh;
        }

        /// <summary>One slightly curved quad, a unit across and a unit tall, hanging from its hoist edge: a stiff
        /// pennant, no cloth simulation. The curve is a few columns bowed along the wind so the flag is not a
        /// cardboard plane edge-on. Both faces are wound, so a flag read from behind is not a hole.</summary>
        static Mesh Pennant()
        {
            const int cols = 4;
            var v = new List<Vector3>(); var tris = new List<int>();
            for (int c = 0; c <= cols; c++)
            {
                float u = c / (float)cols;
                float bow = Mathf.Sin(u * Mathf.PI) * 0.09f;
                v.Add(new Vector3(u, 0f, bow));
                v.Add(new Vector3(u, -1f, bow * 0.7f));
            }
            for (int c = 0; c < cols; c++)
            {
                int a = c * 2;
                tris.Add(a); tris.Add(a + 2); tris.Add(a + 3);
                tris.Add(a); tris.Add(a + 3); tris.Add(a + 1);
                tris.Add(a); tris.Add(a + 3); tris.Add(a + 2);
                tris.Add(a); tris.Add(a + 1); tris.Add(a + 3);
            }
            var mesh = new Mesh { name = "Pennant", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(v); mesh.SetTriangles(tris, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds();
            return mesh;
        }

        void Update()
        {
            if (Host == null || Host.Local == null) return;
            var map = Host.Local.Map;
            if (!built)
            {
                if (!map.Trenches.IsCreated) return;
                Build();
            }
            if (!subscribed)
            {
                Host.Events.OnEvent += OnSimEvent;
                if (Fx == null) Fx = Host.GetComponentInParent<CombatFx>();
                if (Fx == null) Fx = UnityEngine.Object.FindAnyObjectByType<CombatFx>();
                subscribed = true;
            }
            if (poleMat == null || poles.Length == 0) return;

            var cam = Camera.main;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            bool close = SceneHooks.CloseUp > 0f;
            float dt = Time.deltaTime, now = Time.time;
            clothDraw[0].Clear(); clothDraw[1].Clear();
            int polesDrawn = 0, frogsDrawn = 0;

            for (int i = 0; i < poles.Length; i++)
            {
                var p = poles[i];
                if (p.Since < TrenchFlagRules.FallSeconds)
                {
                    p.Since += dt;
                    if (p.Since >= TrenchFlagRules.FallSeconds) p.Losing = 255;
                    poles[i] = p;
                }
                if (p.State == FlagState.Gone) { poles[i].Dust = false; continue; }
                if (p.Dust)
                {
                    poles[i].Dust = false;
                    // the rope runs, the butt is scuffed: one dab off an existing sheet, no new book
                    if (Fx != null) Fx.DustDab(p.Anchor + Vector3.up * 0.18f, 1.1f);
                }

                float far = Vector3.Distance(eye, p.Anchor);
                bool tiny = far > TrenchFlagRules.PoleFadeMeters;
                float high = p.State == FlagState.Snapped ? TrenchFlagRules.StumpHeight : TrenchFlagRules.PoleHeight;
                var up = Quaternion.Euler(0f, p.Yaw * Mathf.Rad2Deg, 0f);
                if (!tiny) poleDraw[polesDrawn++] = Matrix4x4.TRS(p.Anchor, up, new Vector3(0.17f, high, 0.17f));
                if (p.State != FlagState.Intact) continue;   // a snapped pole is a stump: no cloth, no frog

                // the flag flies across the parapet, its hoist edge on the pole
                var across = up * Quaternion.Euler(0f, 90f, 0f);
                float swell = tiny ? TrenchFlagRules.Swell : 1f;
                var cloth = new Vector3(TrenchFlagRules.FlagWide * swell, TrenchFlagRules.FlagTall * swell, 1f);
                float top = high - 0.08f, bottom = TrenchFlagRules.FlagTall * 0.55f;

                if (TrenchFlagRules.Flies(p.Owner))
                {
                    float hoist = Mathf.Lerp(bottom, top, TrenchFlagRules.Rise(p.Since));
                    float flutter = Mathf.Sin(now * 1.7f + i) * 2.5f;
                    clothDraw[p.Owner & 1].Add(Matrix4x4.TRS(p.Anchor + Vector3.up * hoist, across * Quaternion.Euler(0f, flutter, 0f), cloth));
                }
                if (TrenchFlagRules.Flies(p.Losing) && p.Since < TrenchFlagRules.FallSeconds)
                {
                    // cut loose: it swings clear of the pole and tumbles down, still airborne over the parapet when it
                    // stops being drawn (TrenchFlagRules.FallClearance) — a flag flies off, it does not sink in
                    var pos = p.Anchor
                              + Vector3.up * TrenchFlagRules.FallHeight(p.Since, top)
                              + across * Vector3.right * (0.4f + TrenchFlagRules.FallOut(p.Since));
                    float spun = TrenchFlagRules.FallAngle(p.Since) * Mathf.Rad2Deg;
                    var spin = across * Quaternion.Euler(spun, 0f, spun * 0.4f);
                    clothDraw[p.Losing & 1].Add(Matrix4x4.TRS(pos, spin, cloth));
                }

                // the frog and its rope are a close-up's business only
                if (close && far < 60f && frogMesh != null)
                {
                    bool hauling = p.Since < TrenchFlagRules.RiseSeconds;
                    float haul = hauling ? Mathf.Sin(p.Since * 11f) * 0.09f : 0f;
                    var stand = p.Anchor + up * new Vector3(0.52f, 0f, -0.12f);
                    frogDraw[frogsDrawn] = Matrix4x4.TRS(stand, up * Quaternion.Euler(0f, 160f, 0f), Vector3.one * (TrenchFlagRules.FrogHeight / 3f));
                    float ropeHigh = Mathf.Lerp(bottom, top, hauling ? TrenchFlagRules.Rise(p.Since) : 1f);
                    ropeDraw[frogsDrawn] = Matrix4x4.TRS(p.Anchor + up * new Vector3(0.10f, 0f, 0f) + Vector3.up * ropeHigh * 0.5f,
                                                         up * Quaternion.Euler(0f, 0f, 3f + haul * 20f), new Vector3(0.025f, ropeHigh, 0.025f));
                    frogsDrawn++;
                }
            }

            var size = map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * .5f, 0f, size.y * .5f), new Vector3(size.x + 40f, 80f, size.y + 40f));
            if (polesDrawn > 0)
                FrameBudget.Draw(new RenderParams(poleMat) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true }, poleMesh, 0, poleDraw, polesDrawn);
            for (int k = 0; k < 2; k++)
                if (clothDraw[k].Count > 0 && clothMat[k] != null)
                    FrameBudget.Draw(new RenderParams(clothMat[k]) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true }, flagMesh, 0, clothDraw[k]);
            if (frogsDrawn > 0 && frogMat != null)
            {
                var rp = new RenderParams(frogMat) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true };
                FrameBudget.Draw(rp, frogMesh, 0, frogDraw, frogsDrawn);
                FrameBudget.Draw(rp, ropeMesh, 0, ropeDraw, frogsDrawn);
            }
        }
    }
}
