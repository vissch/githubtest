// Phase: deaths (2026-09-28, implemented) — part of DebrisRenderer: a man's parts cut at load from the soldier the VAT
// baker made (Resources/Units/FigureSoldier), so what flies off a man is his own head, helmet, arm, leg, boot, chest,
// hips or half of him. The baked mesh holds the idle pose in its vertices, the limb in UV1.x (VATBaker.LimbOf: 0 body,
// 1 head and helmet, 2/3 left/right arm, 4/5 left/right leg) and the cloth mask in vertex alpha (1 the uniform the
// side's tint dyes, 0 skin, boots and kit, which keep their colour). A part is the triangles of its limb (a boot: the
// leg's below the ankle; the chest and the hips: the body's above and below the waist; the helmet: the head's olive
// triangles, as the soldier's texture paints it), turned the way the debris shader rests a piece: a limb, a chest, hips
// or a half lying with its longest side along +Z and its thinnest up (the idle pose holds the rifle, so an arm points
// forward, not down), a head, a helmet and a boot upright. The rifle (limb 0, its baked colour) is left out: it flies
// as its own piece. Owner, 2026-09-28: break-apart parts cut from
// the models already in the repo, the base model untouched; the cut is made here, in memory, once (the plan said
// Blender; the figure already carries everything a cut needs). When the figure is missing or cannot be read, each part
// is its stand-in (DebrisRenderer.Parts).
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class DebrisRenderer
    {
        public const string FigurePath = "Units/FigureSoldier";
        /// <summary>The rifle's baked colour (VATBaker): how its vertices are told from the body's.</summary>
        public static readonly Color RifleColour = new Color(0.27f, 0.18f, 0.11f, 0f);

        /// <summary>The helmet's paint (the soldier's texture, kept in the vertex colour): olive, green at least as strong
        /// as red, blue well under both, and dark. The hood under it is pale and bluish, the chin strap brown.</summary>
        public static bool IsHelmet(Color c) => c.a < 0.5f && c.r < 0.5f && c.g >= c.r - 0.02f && c.b < c.r - 0.04f;
        /// <summary>Where the waist is between the bottom and the top of the body (limb 0), and the ankle up the leg.</summary>
        public const float WaistShare = 0.3f, AnkleShare = 0.14f;

        static FigureSource figure;
        static bool figureTried;

        /// <summary>The soldier's mesh read once: positions, colours, limbs and triangles, with the body's height span.</summary>
        sealed class FigureSource
        {
            public Vector3[] V; public Color[] C; public int[] Limb; public int[] T;
            public float BodyLow, BodyHigh, LegLow, LegHigh;
        }

        static FigureSource Figure()
        {
            if (figureTried) return figure;
            figureTried = true;
            var data = Resources.Load<VatAssetData>(FigurePath);
            var mesh = data != null ? data.Mesh : null;
            if (mesh == null || !mesh.isReadable) return null;
            var uv = new List<Vector2>(); mesh.GetUVs(1, uv);
            var f = new FigureSource { V = mesh.vertices, C = mesh.colors, T = mesh.triangles };
            if (uv.Count != f.V.Length || f.C.Length != f.V.Length || f.T.Length == 0) return null;
            f.Limb = new int[f.V.Length];
            f.BodyLow = f.LegLow = float.MaxValue; f.BodyHigh = f.LegHigh = float.MinValue;
            for (int i = 0; i < f.V.Length; i++)
            {
                f.Limb[i] = Mathf.RoundToInt(uv[i].x);
                float y = f.V[i].y;
                if (f.Limb[i] == 0 && !Near(f.C[i], RifleColour)) { f.BodyLow = Mathf.Min(f.BodyLow, y); f.BodyHigh = Mathf.Max(f.BodyHigh, y); }
                if (f.Limb[i] == 4) { f.LegLow = Mathf.Min(f.LegLow, y); f.LegHigh = Mathf.Max(f.LegHigh, y); }
            }
            if (f.BodyHigh <= f.BodyLow || f.LegHigh <= f.LegLow) return null;
            return figure = f;
        }

        static bool Near(Color a, Color b) => Mathf.Abs(a.r - b.r) < 0.02f && Mathf.Abs(a.g - b.g) < 0.02f && Mathf.Abs(a.b - b.b) < 0.02f && a.a < 0.5f;

        /// <summary>Is triangle k (its first index) part of `piece`? By its vertices' limb and colour and its middle's height.</summary>
        static bool Takes(FigureSource f, Piece piece, int k)
        {
            int a = f.T[k], b = f.T[k + 1], c = f.T[k + 2];
            int limb = f.Limb[a];
            if (f.Limb[b] != limb || f.Limb[c] != limb) return false;   // the seams between limbs stay on neither side
            if (Near(f.C[a], RifleColour)) return false;
            // two of its three corners painted olive: the helmet (one is the seam with the hood, which stays on the head)
            bool helmet = (IsHelmet(f.C[a]) ? 1 : 0) + (IsHelmet(f.C[b]) ? 1 : 0) + (IsHelmet(f.C[c]) ? 1 : 0) >= 2;
            float y = (f.V[a].y + f.V[b].y + f.V[c].y) / 3f;
            float waist = Mathf.Lerp(f.BodyLow, f.BodyHigh, WaistShare), ankle = Mathf.Lerp(f.LegLow, f.LegHigh, AnkleShare);
            switch (piece)
            {
                case Piece.Head: return limb == 1 && !helmet;
                case Piece.Helm: return limb == 1 && helmet;
                case Piece.Arm: return limb == 2;
                case Piece.Leg: return limb == 4;
                case Piece.Boot: return limb == 4 && y < ankle;
                case Piece.Torso: return limb == 0 && y >= waist;
                case Piece.Pelvis: return limb == 0 && y < waist;
                case Piece.UpperHalf: return (limb == 0 && y >= waist) || limb == 1 || limb == 2 || limb == 3;
                case Piece.LowerHalf: return (limb == 0 && y < waist) || limb == 4 || limb == 5;
                default: return false;
            }
        }

        /// <summary>A lying piece is turned so its longest side is along +Z and its thinnest up (Lay); a head, a helmet and a
        /// boot stand as they were.</summary>
        static bool Lies(Piece piece) => piece != Piece.Head && piece != Piece.Helm && piece != Piece.Boot;

        /// <summary>`piece` cut from the soldier into v/t/c. False (nothing added) when there is no figure to cut, or the
        /// piece is not one of his (the pack).</summary>
        public static bool FigurePart(Piece piece, List<Vector3> v, List<int> t, List<Color> c)
        {
            if (piece == Piece.Pack) return false;
            var f = Figure();
            if (f == null) return false;
            var map = new Dictionary<int, int>();
            int first = v.Count, firstTri = t.Count;
            bool lies = Lies(piece);
            for (int k = 0; k + 2 < f.T.Length; k += 3)
            {
                if (!Takes(f, piece, k)) continue;
                for (int j = 0; j < 3; j++)
                {
                    int src = f.T[k + j];
                    if (!map.TryGetValue(src, out int dst))
                    {
                        dst = v.Count; map[src] = dst;
                        v.Add(f.V[src]);
                        c.Add(f.C[src]);
                    }
                    t.Add(dst);
                }
            }
            if (t.Count - firstTri >= 12)
            {
                if (lies) Lay(v, first);
                Cap(piece, v, t, c, first, firstTri);
                return true;
            }
            v.RemoveRange(first, v.Count - first); c.RemoveRange(first, c.Count - first); t.RemoveRange(firstTri, t.Count - firstTri);   // too little to be a part: the stand-in
            return false;
        }

        /// <summary>The colours of a cut's cap: a wound, red with the bone white in its middle; a boot's charred; the
        /// underside of a helmet dark steel. Kit-coloured (vertex alpha 0): the side's tint never dyes them. The wound a
        /// full red (critic round 7: at 0.42 the heap's parts read as mud at the play zoom, no cut end to be seen).</summary>
        public static readonly Color Wound = new Color(0.66f, 0.04f, 0.03f, 0f), Bone = new Color(0.86f, 0.82f, 0.72f, 0f),
                                      Charred = new Color(0.12f, 0.10f, 0.09f, 0f), SteelInside = new Color(0.20f, 0.21f, 0.18f, 0f);

        /// <summary>A hole no wider than this across is capped; a wider one (a waist, a shoulder's ragged rim) is left open
        /// and its inside drawn as flesh (Debris_URP): fanned to one middle it made spikes (the first renders of the chest).</summary>
        public const float CapRadius = 0.2f;

        /// <summary>Close the small holes the cut leaves (a neck, a wrist, an ankle): the part's vertices are welded by
        /// position (a UV seam is not a hole), every loop of edges that only one of its triangles uses, and that is no
        /// wider than CapRadius about its middle, is fanned to that middle, wound the other way round the rim so it faces out.</summary>
        static void Cap(Piece piece, List<Vector3> v, List<int> t, List<Color> c, int first, int firstTri)
        {
            Color rim = piece == Piece.Boot ? Charred : piece == Piece.Helm ? SteelInside : Wound;
            Color middle = piece == Piece.Boot || piece == Piece.Helm ? rim : Bone;
            var weld = new int[v.Count - first];
            var byPos = new Dictionary<Vector3Int, int>();
            for (int i = first; i < v.Count; i++)
            {
                var key = new Vector3Int(Mathf.RoundToInt(v[i].x * 2000f), Mathf.RoundToInt(v[i].y * 2000f), Mathf.RoundToInt(v[i].z * 2000f));
                if (!byPos.TryGetValue(key, out int w)) { w = i; byPos[key] = i; }
                weld[i - first] = w;
            }
            // directed edges on the welded vertices; one whose reverse no triangle uses is on a rim
            var edges = new HashSet<long>();
            int end = t.Count;
            for (int k = firstTri; k < end; k += 3)
                for (int j = 0; j < 3; j++)
                {
                    int a = weld[t[k + j] - first], b = weld[t[k + (j + 1) % 3] - first];
                    if (a != b) edges.Add(EdgeKey(a, b));
                }
            var next = new Dictionary<int, int>();
            foreach (long e in edges)
            {
                int a = (int)(e >> 32), b = (int)(e & 0xffffffffL);
                if (!edges.Contains(EdgeKey(b, a))) next[a] = b;   // a vertex on two rims keeps one; the other stays open
            }
            var seen = new HashSet<int>();
            var starts = new List<int>(next.Keys);
            starts.Sort();   // the same caps every load
            foreach (int start in starts)
            {
                if (seen.Contains(start)) continue;
                var loop = new List<int>();
                int at = start;
                while (!seen.Contains(at) && next.TryGetValue(at, out int to) && loop.Count < 1024) { seen.Add(at); loop.Add(at); at = to; }
                if (loop.Count < 3 || at != start) continue;   // an open chain: left as it is
                Vector3 mid = Vector3.zero;
                foreach (int i in loop) mid += v[i];
                mid /= loop.Count;
                float radius = 0f;
                foreach (int i in loop) radius = Mathf.Max(radius, (v[i] - mid).magnitude);
                if (radius > CapRadius) continue;   // too wide or too ragged to fan: its inside shows as flesh
                int centre = v.Count; v.Add(mid); c.Add(middle);
                int ring = v.Count;
                foreach (int i in loop) { v.Add(v[i]); c.Add(rim); }
                for (int k = 0; k < loop.Count; k++) { t.Add(ring + (k + 1) % loop.Count); t.Add(ring + k); t.Add(centre); }
            }
        }

        static long EdgeKey(int a, int b) => ((long)a << 32) | (uint)b;

        /// <summary>Turn v[first..] (a proper turn: the winding holds) so the longest side of its box lies along +Z and the
        /// thinnest points up: how the debris shader rests a piece, yaw-only.</summary>
        static void Lay(List<Vector3> v, int first)
        {
            Vector3 lo = v[first], hi = v[first];
            for (int i = first; i < v.Count; i++) { lo = Vector3.Min(lo, v[i]); hi = Vector3.Max(hi, v[i]); }
            Vector3 e = hi - lo;
            int longest = 0, thinnest = 0;
            for (int k = 1; k < 3; k++) { if (e[k] > e[longest]) longest = k; if (e[k] < e[thinnest]) thinnest = k; }
            if (thinnest == longest) thinnest = (longest + 1) % 3;
            Vector3 z = Axis(longest), y = Axis(thinnest), x = Vector3.Cross(y, z);
            for (int i = first; i < v.Count; i++) { var p = v[i]; v[i] = new Vector3(Vector3.Dot(p, x), Vector3.Dot(p, y), Vector3.Dot(p, z)); }
        }

        static Vector3 Axis(int k) => k == 0 ? Vector3.right : k == 1 ? Vector3.up : Vector3.forward;
    }
}
