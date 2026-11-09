// Phase: CelFx job (2026-11-09, lane/show/cel-vfx) - the Range's wooden target dummy, the thing every reference clip
// shoots at: a post, a square board with three dark rings, a round head with a painted smile, straw at the arms and
// the waist, leaning a little. About 1.85 m tall, low-poly, built in code (one mesh, vertex colours) and drawn with
// TW/Toon like the battlefield kit, through FrameBudget. Lab only: nothing in the game places one.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.VFX
{
    [DefaultExecutionOrder(100)]
    public sealed class TargetDummy : MonoBehaviour
    {
        /// <summary>Height of the board's middle above the foot (the chest, where rounds go), metres.</summary>
        public const float ChestHeight = 0.95f;
        public const float Height = 1.85f;

        public bool Visible = true;
        public Vector3 Foot;
        public float Yaw;          // degrees: the board faces local +z turned by this
        public float Lean = 4f;    // degrees to its left
        public int Layer;          // the layer it is drawn on (the Range draws it on its own, to measure it)
        Mesh mesh; Material mat;
        RenderParams rp;

        public Matrix4x4 LocalToWorld => Matrix4x4.TRS(Foot, Quaternion.Euler(0f, Yaw, 0f) * Quaternion.Euler(0f, 0f, Lean), Vector3.one);

        /// <summary>The chest point in the world.</summary>
        public Vector3 Chest => LocalToWorld.MultiplyPoint3x4(new Vector3(0f, ChestHeight, 0.04f));

        public Mesh Mesh => mesh != null ? mesh : (mesh = Build());

        public void Place(Vector3 foot, float yaw) { Foot = foot; Yaw = yaw; }

        void OnEnable()
        {
            if (mesh == null) mesh = Build();
            if (mat == null)
            {
                mat = new Material(Shader.Find("TW/Toon (URP)")) { name = "RangeDummy", enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
                mat.SetColor("_BaseColor", Color.white);
                mat.SetFloat("_OutlineWidth", 1.6f);
                mat.SetColor("_Emission", new Color(0.13f, 0.09f, 0.05f));   // the plate's dummy reads warm against the blue night
            }
            rp = new RenderParams(mat) { shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true, layer = 0 };
        }

        void OnDestroy()
        {
            if (mesh != null) Destroy(mesh);
            if (mat != null) Destroy(mat);
        }

        void LateUpdate()
        {
            if (!Visible || mesh == null || mat == null) return;
            rp.layer = Layer;
            rp.worldBounds = new Bounds(Foot + Vector3.up * 1f, new Vector3(3f, 3f, 3f));
            FrameBudget.Draw(rp, mesh, 0, LocalToWorld);
        }

        /// <summary>The dummy's corners in the world (every vertex above its foot), for its screen box.</summary>
        public void WorldPoints(List<Vector3> into)
        {
            into.Clear();
            var m = LocalToWorld;
            foreach (var v in Mesh.vertices) if (v.y >= 0f) into.Add(m.MultiplyPoint3x4(v));   // the post's foot under the ground is not seen
        }

        // ------------------------------------------------------------------------------------------------ the mesh
        static readonly Color Wood = new Color(0.58f, 0.41f, 0.24f), Board = new Color(0.74f, 0.55f, 0.34f), Ink = new Color(0.13f, 0.09f, 0.06f),
            Straw = new Color(0.80f, 0.64f, 0.30f), Head = new Color(0.76f, 0.57f, 0.34f);

        sealed class Builder
        {
            public readonly List<Vector3> V = new List<Vector3>(); public readonly List<Vector3> N = new List<Vector3>();
            public readonly List<Color> C = new List<Color>(); public readonly List<int> T = new List<int>();

            void Quad(Vector3 a, Vector3 b, Vector3 c, Vector3 d, Vector3 n, Color col)
            {
                int i = V.Count;
                V.Add(a); V.Add(b); V.Add(c); V.Add(d);
                for (int k = 0; k < 4; k++) { N.Add(n); C.Add(col); }
                T.Add(i); T.Add(i + 1); T.Add(i + 2); T.Add(i); T.Add(i + 2); T.Add(i + 3);
            }

            /// <summary>A box at `c` with half sizes `h`, rotated by `r`.</summary>
            public void Box(Vector3 c, Vector3 h, Quaternion r, Color col)
            {
                Vector3 P(float x, float y, float z) => c + r * new Vector3(x * h.x, y * h.y, z * h.z);
                Quad(P(-1, -1, 1), P(1, -1, 1), P(1, 1, 1), P(-1, 1, 1), r * Vector3.forward, col);
                Quad(P(1, -1, -1), P(-1, -1, -1), P(-1, 1, -1), P(1, 1, -1), r * Vector3.back, col);
                Quad(P(1, -1, 1), P(1, -1, -1), P(1, 1, -1), P(1, 1, 1), r * Vector3.right, col);
                Quad(P(-1, -1, -1), P(-1, -1, 1), P(-1, 1, 1), P(-1, 1, -1), r * Vector3.left, col);
                Quad(P(-1, 1, 1), P(1, 1, 1), P(1, 1, -1), P(-1, 1, -1), r * Vector3.up, col);
                Quad(P(-1, -1, -1), P(1, -1, -1), P(1, -1, 1), P(-1, -1, 1), r * Vector3.down, col);
            }

            /// <summary>A flat ring (or disc when inner is 0) facing +z at depth z, centred on (cx, cy).</summary>
            public void Ring(float cx, float cy, float z, float inner, float outer, int seg, Color col)
            {
                for (int s = 0; s < seg; s++)
                {
                    float a0 = s * Mathf.PI * 2f / seg, a1 = (s + 1) * Mathf.PI * 2f / seg;
                    Vector3 o0 = new Vector3(cx + Mathf.Cos(a0) * outer, cy + Mathf.Sin(a0) * outer, z), o1 = new Vector3(cx + Mathf.Cos(a1) * outer, cy + Mathf.Sin(a1) * outer, z);
                    Vector3 i0 = new Vector3(cx + Mathf.Cos(a0) * inner, cy + Mathf.Sin(a0) * inner, z), i1 = new Vector3(cx + Mathf.Cos(a1) * inner, cy + Mathf.Sin(a1) * inner, z);
                    Quad(i0, i1, o1, o0, Vector3.forward, col);
                }
            }

            /// <summary>A low sphere (lat-long) of radius r at c.</summary>
            public void Ball(Vector3 c, float r, int lon, int lat, Color col)
            {
                int start = V.Count;
                for (int y = 0; y <= lat; y++)
                {
                    float v = Mathf.PI * y / lat;
                    for (int x = 0; x <= lon; x++)
                    {
                        float u = Mathf.PI * 2f * x / lon;
                        var n = new Vector3(Mathf.Sin(v) * Mathf.Cos(u), Mathf.Cos(v), Mathf.Sin(v) * Mathf.Sin(u));
                        V.Add(c + n * r); N.Add(n); C.Add(col);
                    }
                }
                for (int y = 0; y < lat; y++)
                    for (int x = 0; x < lon; x++)
                    {
                        int a = start + y * (lon + 1) + x, b = a + lon + 1;
                        T.Add(a); T.Add(a + 1); T.Add(b); T.Add(a + 1); T.Add(b + 1); T.Add(b);
                    }
            }

            /// <summary>A bundle of straw: thin sticks from `root` fanned along `dir`.</summary>
            public void Tuft(Vector3 root, Vector3 dir, float length, int sticks, int salt)
            {
                for (int k = 0; k < sticks; k++)
                {
                    float a = (k - (sticks - 1) * 0.5f) * 0.28f + 0.07f * Mathf.Sin(salt * 3.1f + k);
                    var d = Quaternion.AngleAxis(a * Mathf.Rad2Deg, Vector3.forward) * dir;
                    float len = length * (0.75f + 0.25f * Mathf.Abs(Mathf.Sin(salt + k * 1.7f)));
                    var r = Quaternion.FromToRotation(Vector3.right, d);
                    Box(root + d * (len * 0.5f), new Vector3(len * 0.5f, 0.018f, 0.018f), r, Straw);
                }
            }
        }

        /// <summary>The dummy's mesh: foot at the origin, the board facing +z.</summary>
        public static Mesh Build()
        {
            var b = new Builder();
            var id = Quaternion.identity;
            // the post, from just under the ground into the board
            b.Box(new Vector3(0f, 0.30f, -0.07f), new Vector3(0.055f, 0.40f, 0.055f), id, Wood);
            // the board: 0.62 x 1.0, 6 cm thick, from 0.45 to 1.45
            b.Box(new Vector3(0f, ChestHeight, 0f), new Vector3(0.31f, 0.50f, 0.03f), id, Board);
            // three dark rings and the bull, painted a hair in front of the board
            const float z = 0.032f;
            b.Ring(0f, ChestHeight, z, 0f, 0.055f, 12, Ink);
            b.Ring(0f, ChestHeight, z, 0.12f, 0.155f, 20, Ink);
            b.Ring(0f, ChestHeight, z, 0.21f, 0.245f, 24, Ink);
            // the neck and the head, a ball with two eyes and a smile on its front
            b.Box(new Vector3(0f, 1.48f, -0.01f), new Vector3(0.05f, 0.05f, 0.05f), id, Wood);
            const float hy = 1.65f, hr = 0.20f;
            b.Ball(new Vector3(0f, hy, 0f), hr, 10, 7, Head);
            float fz = hr + 0.005f;
            b.Box(new Vector3(-0.065f, hy + 0.05f, fz - 0.03f), new Vector3(0.022f, 0.03f, 0.03f), id, Ink);
            b.Box(new Vector3(0.065f, hy + 0.05f, fz - 0.03f), new Vector3(0.022f, 0.03f, 0.03f), id, Ink);
            for (int k = 0; k < 5; k++)
            {
                float a = Mathf.Lerp(-0.9f, 0.9f, k / 4f);
                b.Box(new Vector3(Mathf.Sin(a) * 0.09f, hy - 0.03f - Mathf.Cos(a) * 0.05f + 0.05f, fz - 0.04f - 0.02f * Mathf.Abs(a)), new Vector3(0.025f, 0.012f, 0.03f), Quaternion.Euler(0f, 0f, a * Mathf.Rad2Deg), Ink);
            }
            // straw at the arms (the board's top corners) and at the waist
            b.Tuft(new Vector3(-0.30f, 1.33f, 0.01f), new Vector3(-1f, 0.15f, 0f).normalized, 0.15f, 5, 1);
            b.Tuft(new Vector3(0.30f, 1.33f, 0.01f), new Vector3(1f, 0.10f, 0f).normalized, 0.15f, 5, 2);
            b.Tuft(new Vector3(0.30f, 1.05f, 0.01f), new Vector3(1f, -0.35f, 0f).normalized, 0.11f, 3, 3);
            b.Tuft(new Vector3(-0.12f, 0.46f, 0.02f), new Vector3(-0.3f, -1f, 0f).normalized, 0.13f, 4, 4);
            b.Tuft(new Vector3(0.10f, 0.46f, 0.02f), new Vector3(0.35f, -1f, 0f).normalized, 0.12f, 3, 5);

            var m = new Mesh { name = "RangeDummy", hideFlags = HideFlags.HideAndDontSave };
            m.SetVertices(b.V); m.SetNormals(b.N); m.SetColors(b.C); m.SetTriangles(b.T, 0);
            m.RecalculateBounds();
            return m;
        }
    }
}
