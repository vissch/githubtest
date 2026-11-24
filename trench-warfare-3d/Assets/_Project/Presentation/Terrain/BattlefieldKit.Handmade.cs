// Phase: B2 (implemented) — part of BattlefieldKit: the trench lining as men under fire built it (owner, 2026-11-24:
// "made by humans in a rush, used and sometimes unlogic crooked ... keep it optimized"). The same eighteen modules as the
// even panels of BuildTrenchVariants (TrenchSectionTests), each no heavier than the one it replaces (HandmadeTrenchTests),
// so the field draws no more. Sacks are laid one at a time and settle on whatever is under them: they sag into the joint
// of the course below, droop over an edge, bulge at the foot and lift their tied ears. Boards are cut to no common
// length, sawn off square or not, nailed up crooked, with gaps, a patch, a prop, a board slipped at one end. What still
// repeats from place to place the shader breaks up: every part carries its pivot and a seed in UV2, and _KITJITTER in
// TW/Toon (URP) turns, shifts, sinks and tones each part by a hash of the panel's own matrix. Knob props.handmade = 0
// builds the even panels again. The grime (props.grime) reads, beside the part's height and seed, how far each point lies
// above the nearest joint (Joints), so the rot starts where the parts touch and climbs toward what lies bare.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldKit
    {
        /// <summary>props.handmade: 1 (the default) the hand-built lining; 0 the even panels of 2026-09.</summary>
        public const string HandmadeKnob = "props.handmade";
        /// <summary>Read once, when the kit is built: the lining's meshes and materials depend on it.</summary>
        public readonly bool Handmade = Knobs.Get(HandmadeKnob, true);
        /// <summary>props.grime: the lining's grime in TW/Toon (URP) _KitGrime: silt in the joints (Joints), the wet round
        /// it, the rot and the wear. 1 (the default) on, 0 off. Read once, when the kit is built.</summary>
        public const string GrimeKnob = "props.grime";
        public readonly int Grime = Mathf.Clamp(Knobs.Get(GrimeKnob, 1), 0, 1);

        /// <summary>What the shader does to a lining part (TW/Toon _KitJitter): turn (degrees), shift (m), tone, sink (m),
        /// and how much of the turn goes about each axis (x pitch, y yaw, z roll) with, in w, the share of loose boards
        /// left out. Sacks hardly move (they rest on each other); boards tilt in their own plane; duckboards turn and sink.</summary>
        public static readonly Vector4 SackJitter = new Vector4(8f, .03f, .10f, 0f), SackAxes = new Vector4(.2f, 1f, .2f, .16f);
        public static readonly Vector4 WallJitter = new Vector4(2.6f, .022f, .13f, 0f), WallAxes = new Vector4(.35f, .2f, 1f, .06f);
        public static readonly Vector4 FloorJitter = new Vector4(3.5f, .02f, .12f, .025f), FloorAxes = new Vector4(.35f, 1f, .5f, .03f);

        /// <summary>A small seeded dice for building meshes (never the sim's: this is the look only).</summary>
        struct Dice
        {
            uint s;
            public Dice(int seed) { s = (uint)seed * 0x9E3779B1u + 0x7F4A7C15u; Next(); }
            public float Next() { s ^= s << 13; s ^= s >> 17; s ^= s << 5; return (s & 0xFFFFFF) / 16777216f; }
            public float Range(float a, float b) => a + (b - a) * Next();
            public float Sym(float r) => (Next() * 2f - 1f) * r;
            public bool Chance(float p) => Next() < p;
        }

        /// <summary>One panel's mesh in the making. Every vertex knows its part's pivot and seed (UV2: a negative seed is a
        /// part the shader may never leave out), its tone (colour) and its paint (UV0); Finish adds the outline's smoothed
        /// normals in UV3 the way Combine does. Triangles are wound by the outward direction they are given.</summary>
        sealed class Panel
        {
            readonly List<Vector3> v = new List<Vector3>(); readonly List<Vector2> uv = new List<Vector2>();
            readonly List<Color> c = new List<Color>(); readonly List<Vector4> part = new List<Vector4>(); readonly List<int> t = new List<int>();
            Vector4 current; Color tone = Color.white; int parts;
            public int Count => v.Count;
            public Vector3 this[int i] => v[i];
            public void Begin(Vector3 pivot, Color partTone, bool mayGo)
            {
                parts++;
                float seed = .05f + (parts * .618034f) % .9f;
                current = new Vector4(pivot.x, pivot.y, pivot.z, mayGo ? seed : -seed);
                tone = partTone;
            }
            public int Vert(Vector3 p, Vector2 paint, float shade)
            {
                v.Add(p); uv.Add(paint); var k = tone * shade; k.a = 1f; c.Add(k); part.Add(current); return v.Count - 1;
            }
            public void Tri(int a, int b, int d, Vector3 outward)
            {
                if (Vector3.Dot(Vector3.Cross(v[b] - v[a], v[d] - v[a]), outward) < 0f) { int s = b; b = d; d = s; }
                t.Add(a); t.Add(b); t.Add(d);
            }
            public void Quad(int a, int b, int d, int e, Vector3 outward) { Tri(a, b, d, outward); Tri(a, d, e, outward); }
            public Mesh Finish(string name, List<Mesh> owned)
            {
                var m = new Mesh { name = name, hideFlags = HideFlags.HideAndDontSave };
                m.SetVertices(v); m.SetUVs(0, uv); m.SetColors(c); m.SetUVs(2, part); m.SetTriangles(t, 0);
                m.RecalculateNormals(); m.RecalculateBounds();
                var normals = m.normals; var sum = new Dictionary<Vector3Int, Vector3>(); var keys = new Vector3Int[v.Count];
                for (int i = 0; i < v.Count; i++)
                {
                    keys[i] = new Vector3Int(Mathf.RoundToInt(v[i].x * 500f), Mathf.RoundToInt(v[i].y * 500f), Mathf.RoundToInt(v[i].z * 500f));
                    sum.TryGetValue(keys[i], out var n); sum[keys[i]] = n + normals[i];
                }
                var smooth = new List<Vector3>(v.Count);
                for (int i = 0; i < v.Count; i++) smooth.Add(sum[keys[i]].normalized);
                m.SetUVs(3, smooth);
                owned.Add(m); return m;
            }
        }

        // ---- sacks --------------------------------------------------------------------------------------------------

        /// <summary>What the sacks rest on, as heights on a 4 cm grid over the panel: the ground (0) at first, then every
        /// sack's top as it is laid.</summary>
        sealed class Bed
        {
            public const float Cell = .04f, X0 = -1.8f, Z0 = -1.2f;
            public const int NX = 92, NZ = 70;
            readonly float[] h = new float[NX * NZ];
            int I(float x) => Mathf.Clamp(Mathf.RoundToInt((x - X0) / Cell), 0, NX - 1);
            int J(float z) => Mathf.Clamp(Mathf.RoundToInt((z - Z0) / Cell), 0, NZ - 1);
            public float At(float x, float z) => h[J(z) * NX + I(x)];
            /// <summary>What a sack's cloth feels under a point: the mean and the highest of the 20 cm round it, between
            /// the two, so it bridges a narrow joint rather than falling into it and still drops into a wide one.</summary>
            public float Soft(float x, float z)
            {
                float sum = 0f, max = float.MinValue;
                for (int dj = -2; dj <= 2; dj++)
                for (int di = -2; di <= 2; di++) { float y = At(x + di * Cell, z + dj * Cell); sum += y; if (y > max) max = y; }
                return Mathf.Lerp(sum / 25f, max, .55f);
            }
            public void Raise(int i, int j, float y) { int k = j * NX + i; if (y > h[k]) h[k] = y; }
            public static float X(int i) => X0 + i * Cell;
            public static float Z(int j) => Z0 + j * Cell;
        }

        /// <summary>One sack to lay: where its middle goes (panel x, z), which way it points, its size, how far its ends may
        /// droop past what holds it up, how much its weight has squashed and spread it, how far it sinks into the mud, and
        /// its colour.</summary>
        struct SackSpec
        {
            public float X, Z, Yaw, Length, Width, Height, Droop, Slump, Sink, Bend, Ear; public Color Tone;
            /// <summary>Nothing rests on it, so the shader may leave it out at some places (a gap, a lower stack).</summary>
            public bool MayGo;
        }

        // 56 vertices and 108 triangles a sack (the even sack had 104 and 168): seven stations along it, crowded toward the
        // ends so the plan rounds off instead of ending in a corner (six read as a hexagonal flagstone from the standard
        // camera, 2026-11-24), and eight round it with one on the crown, so the top is domed and not a slab
        const int SackStations = 7, SackRing = 8;
        const int SackVerts = SackStations * SackRing, SackTris = (SackStations - 1) * SackRing * 2 + 2 * (SackRing - 2);
        static readonly float[] SackStation = { -1f, -.88f, -.62f, 0f, .62f, .88f, 1f };
        const float SackAcross = .72f, SackUp = .9f;   // the cross-section's squareness: below 1 squarer (a pillow)

        // plan and height along the sack: full most of its length, rounding off, then gathered into the tied neck
        static float SackEnvelope(float t) => Mathf.Max(.24f, Mathf.Pow(Mathf.Max(0f, 1f - Mathf.Pow(Mathf.Abs(t), 3.6f)), .4f));
        static float SackEnvelopeHeight(float t) => Mathf.Max(.30f, Mathf.Pow(Mathf.Max(0f, 1f - Mathf.Pow(Mathf.Abs(t), 3.0f)), .45f));
        /// <summary>Hessian is never smooth: a lump here, a fold there, the same on every face that meets at a point.</summary>
        static float Lumps(float t, float k, float seed) => Mathf.PerlinNoise(t * 1.9f + seed * 13.1f, k * .61f + seed * 7.3f) - .5f;

        /// <summary>Lays one sack on the bed, then raises the bed by its top. A vertex's height is what it rests on (the bed
        /// under it, never lower than the sack's own seat less its droop, less what it sinks) plus its height in the sack,
        /// so the sack's whole column follows what is under it: across a joint it sags, over an edge it droops.</summary>
        static float LaySack(Panel panel, Bed bed, SackSpec s)
        {
            float cy = Mathf.Cos(s.Yaw * Mathf.Deg2Rad), sy = Mathf.Sin(s.Yaw * Mathf.Deg2Rad);
            Vector2 ToPanel(float lx, float lz) => new Vector2(s.X + cy * lx - sy * lz, s.Z + sy * lx + cy * lz);
            float halfL = s.Length * .5f, height = s.Height * (1f - .22f * s.Slump);
            // the seat: what the sack's middle finds under it, nearer the mean than the highest point, so a sack with one
            // end on its neighbour lets the other end down to the ground instead of hanging in the air
            float seatMax = 0f, seatSum = 0f;
            for (int a = -1; a <= 1; a++) for (int b = -1; b <= 1; b++)
            { var p = ToPanel(a * halfL * .45f, b * s.Width * .18f); float y = bed.Soft(p.x, p.y); seatSum += y; seatMax = Mathf.Max(seatMax, y); }
            float seat = Mathf.Lerp(seatSum / 9f, seatMax, .35f);
            float Foot(Vector2 p) => Mathf.Max(bed.Soft(p.x, p.y), seat - s.Droop) - s.Sink;
            panel.Begin(new Vector3(s.X, seat + height * .5f, s.Z), s.Tone, s.MayGo);
            int first = panel.Count;
            var spine = new Vector3[SackStations];
            for (int st = 0; st < SackStations; st++)
            {
                float t = SackStation[st], e = SackEnvelope(t), eh = SackEnvelopeHeight(t);
                float bend = s.Bend * (1f - t * t);
                float ear = Mathf.Pow(Mathf.Abs(t), 6f) * s.Ear;   // the folded ends stand up a little
                var mid = ToPanel(t * halfL, bend);
                spine[st] = new Vector3(mid.x, Foot(mid) + height * .5f + ear, mid.y);
                for (int k = 0; k < SackRing; k++)
                {
                    // the ring starts and ends under the belly, so the paint's seam is where nobody looks
                    float phi = -Mathf.PI * .5f + 2f * Mathf.PI * k / SackRing;
                    float cphi = Mathf.Cos(phi), sphi = Mathf.Sin(phi);
                    float up = .5f + .5f * Mathf.Sign(sphi) * Mathf.Pow(Mathf.Abs(sphi), SackUp);   // 0 under, 1 on top
                    float across = Mathf.Sign(cphi) * Mathf.Pow(Mathf.Abs(cphi), SackAcross) * s.Width * .5f * e;
                    across *= 1f + .20f * (1f - up) * (.4f + s.Slump);   // the weight spreads the foot
                    float lump = Lumps(t, k, s.Ear * 10f + s.X);
                    across *= 1f + .10f * lump;
                    float lift = height * (.5f + (up - .5f) * eh) + ear + height * .10f * lump * up;
                    lift -= height * (.10f + .10f * s.Slump) * (1f - t * t) * up * up;   // the top sags under its own weight
                    var p = ToPanel(t * halfL * (1f - .05f * (1f - e)), bend + across);
                    // a pillow's light painted in: brightest on the crown, darker toward its edges and its tied ends and
                    // darkest where it presses on what is under it (the toon light's two steps alone read it as a slab)
                    float edge = Mathf.Abs(cphi);
                    float shade = Mathf.Lerp(.72f, 1.10f, Mathf.Pow(up, .7f)) * (1f - .13f * edge * edge * up) * (1f - .16f * Mathf.Pow(Mathf.Abs(t), 2.5f)) * (1f + .10f * lump);
                    panel.Vert(new Vector3(p.x, Foot(p) + lift, p.y), new Vector2((t + 1f) * .5f, k / (float)SackRing), shade);
                }
            }
            for (int st = 0; st < SackStations - 1; st++)
            for (int k = 0; k < SackRing; k++)
            {
                int a = first + st * SackRing + k, b = first + st * SackRing + (k + 1) % SackRing;
                int d = a + SackRing, e = b + SackRing;
                var centre = (panel[a] + panel[b] + panel[d] + panel[e]) * .25f;
                panel.Quad(a, b, e, d, centre - (spine[st] + spine[st + 1]) * .5f);
            }
            // the tied ends: a fan closes each, a flat folded end rather than a hole
            for (int end = 0; end < 2; end++)
            {
                int ring = first + (end == 0 ? 0 : (SackStations - 1) * SackRing);
                var tip = spine[end == 0 ? 0 : SackStations - 1];
                var outward = tip - spine[end == 0 ? 1 : SackStations - 2];
                for (int k = 1; k < SackRing - 1; k++) panel.Tri(ring, ring + k, ring + k + 1, outward);
            }
            // raise the bed by the sack's top: every top is found on the bed as it was, then written, or a cell raised
            // early would lift its neighbours' feet and the sack would climb on itself
            float reach = halfL + s.Width;
            var tops = new List<(int i, int j, float y)>();
            for (int i = 0; i < Bed.NX; i++)
            {
                float x = Bed.X(i); if (Mathf.Abs(x - s.X) > reach) continue;
                for (int j = 0; j < Bed.NZ; j++)
                {
                    float z = Bed.Z(j); if (Mathf.Abs(z - s.Z) > reach) continue;
                    float dx = x - s.X, dz = z - s.Z;
                    float lx = cy * dx + sy * dz, lz = -sy * dx + cy * dz;
                    float t = lx / halfL; if (Mathf.Abs(t) >= 1f) continue;
                    float half = s.Width * .5f * SackEnvelope(t) * (1f + .08f * s.Slump);
                    float u = (lz - s.Bend * (1f - t * t)) / half; if (Mathf.Abs(u) >= 1f) continue;
                    float cphi = Mathf.Pow(Mathf.Abs(u), 1f / SackAcross), top = .5f + .5f * Mathf.Pow(Mathf.Sqrt(Mathf.Max(0f, 1f - cphi * cphi)), SackUp);
                    tops.Add((i, j, Foot(new Vector2(x, z)) + height * (.5f + (top - .5f) * SackEnvelopeHeight(t))));
                }
            }
            foreach (var (i, j, y) in tops) bed.Raise(i, j, y);
            return seat;
        }

        static SackSpec FillSack(ref Dice d, float x, float z, float yaw, Color baseTone, float scale = 1f)
        {
            // a fat hessian sack, filled by a different man each time: about 1.06 x 0.63 x 0.35 m before its weight settles
            // it (the game's sacks are drawn a size up, as the even ones were: a real one is lost at the standard view)
            float value = d.Range(.98f, 1.18f);
            float warm = d.Sym(.05f);
            float kind = d.Next();
            if (kind < .10f) value *= .82f;                                   // soaked, or plastered with mud
            else if (kind < .28f) { warm = -.08f; value *= 1.06f; }           // a newer sack, paler and greyer
            return new SackSpec
            {
                X = x + d.Sym(.02f), Z = z + d.Sym(.04f), Yaw = yaw + d.Sym(5f),
                Length = d.Range(1.0f, 1.12f) * scale, Width = d.Range(.58f, .68f) * scale, Height = d.Range(.32f, .38f) * scale,
                Droop = d.Range(.08f, .15f), Slump = d.Range(.15f, .55f), Sink = d.Range(0f, .03f), Bend = d.Sym(.05f), Ear = d.Range(.02f, .06f),
                Tone = new Color(baseTone.r * value * (1f + warm), baseTone.g * value, baseTone.b * value * (1f - warm * 1.3f)),
            };
        }

        /// <summary>A sack burst by a shell or a burst of fire: half its earth gone, flat and wide.</summary>
        static SackSpec Burst(SackSpec s) { s.Height *= .5f; s.Width *= 1.18f; s.Slump = 1f; s.Droop = .2f; s.Tone *= .9f; return s; }

        /// <summary>Lays the sacks in order and returns the mesh. A dry run first finds what rests on what: a sack with
        /// nothing laid on it afterwards may be left out by the shader (SackAxes.w), so the same mesh is a different
        /// heap at every place and nothing is ever left hanging in the air.</summary>
        Mesh SackPanel(string name, List<SackSpec> sacks) => SackPanel(name, sacks, null);

        Mesh SackPanel(string name, List<SackSpec> sacks, List<float> seatsOut)
        {
            var seats = new List<float>();
            { var dry = new Panel(); var dryBed = new Bed(); foreach (var s in sacks) seats.Add(LaySack(dry, dryBed, s)); }
            for (int i = 0; i < sacks.Count; i++)
            {
                bool covered = false;
                for (int j = i + 1; j < sacks.Count && !covered; j++)
                {
                    float reach = .5f * (sacks[i].Length + sacks[j].Length);
                    covered = seats[j] > seats[i] + .06f && new Vector2(sacks[j].X - sacks[i].X, sacks[j].Z - sacks[i].Z).magnitude < reach;
                }
                var spec = sacks[i]; spec.MayGo = !covered; sacks[i] = spec;
            }
            var panel = new Panel(); var bed = new Bed();
            foreach (var s in sacks) LaySack(panel, bed, s);
            seatsOut?.AddRange(seats);
            return panel.Finish(name, ownedMeshes);
        }

        // ---- timber -------------------------------------------------------------------------------------------------

        /// <summary>One length of timber: its middle, its turn (degrees), its length along local x, its height (y) and
        /// thickness (z), how far off square each end was sawn, its colour, whether the shader may leave it out.</summary>
        struct BoardSpec
        {
            public Vector3 At, Euler; public float Length, Height, Thick, CutLeft, CutRight; public Color Tone; public bool MayGo;
        }

        /// <summary>A board of five faces (no underside: nothing looks up at one), 20 vertices against a cube's 24. Each end
        /// is sawn on a slope, so no two boards end alike; the paint starts at a different place on every board.</summary>
        static void LayBoard(Panel panel, BoardSpec b, ref Dice d, float grimeTop)
        {
            var m = Matrix4x4.TRS(b.At, Quaternion.Euler(b.Euler), Vector3.one);
            panel.Begin(b.At, b.Tone, b.MayGo);
            float hx = b.Length * .5f, hy = b.Height * .5f, hz = b.Thick * .5f;
            Vector3 Corner(int sx, int sy, int sz)
            {
                float cut = (sx < 0 ? b.CutLeft : b.CutRight) * sy * .5f;
                return m.MultiplyPoint3x4(new Vector3(sx * (hx + cut), sy * hy, sz * hz));
            }
            float u0 = d.Next(), v0 = d.Next(), grain = Mathf.Max(1f, b.Length / 1.4f);
            float Shade(Vector3 p) => Mathf.Lerp(.82f, 1.06f, Mathf.Clamp01(p.y / grimeTop));
            int V(Vector3 p, float u, float vv) => panel.Vert(p, new Vector2(u0 + u * grain, v0 + vv), Shade(p));
            Vector3 centre = b.At;
            void Face(Vector3 p0, Vector3 p1, Vector3 p2, Vector3 p3, float u0f, float u1f)
            {
                int a = V(p0, u0f, 0f), c1 = V(p1, u1f, 0f), c2 = V(p2, u1f, 1f), c3 = V(p3, u0f, 1f);
                panel.Quad(a, c1, c2, c3, (p0 + p1 + p2 + p3) * .25f - centre);
            }
            Face(Corner(-1, -1, -1), Corner(1, -1, -1), Corner(1, 1, -1), Corner(-1, 1, -1), 0f, 1f);    // front
            Face(Corner(-1, -1, 1), Corner(1, -1, 1), Corner(1, 1, 1), Corner(-1, 1, 1), 0f, 1f);        // back
            Face(Corner(-1, 1, -1), Corner(1, 1, -1), Corner(1, 1, 1), Corner(-1, 1, 1), 0f, 1f);        // top
            Face(Corner(-1, -1, -1), Corner(-1, -1, 1), Corner(-1, 1, 1), Corner(-1, 1, -1), 0f, .1f);   // left end
            Face(Corner(1, -1, -1), Corner(1, -1, 1), Corner(1, 1, 1), Corner(1, 1, -1), 0f, .1f);       // right end
        }

        const int BoardVerts = 20;

        static Color TimberTone(ref Dice d, Color timber)
        {
            float value = d.Range(.92f, 1.16f);
            float r = 1f, g = 1f, b = 1f;
            float kind = d.Next();
            if (kind < .28f) { r = .90f; g = .95f; b = 1.04f; value *= 1.04f; }    // grey, out in the rain since 1915
            else if (kind < .45f) { r = 1.08f; g = 1.0f; b = .86f; value *= 1.06f; } // freshly cut, still pale
            else if (kind < .53f) value *= .8f;                                       // tarred or soaked
            return new Color(timber.r * value * r, timber.g * value * g, timber.b * value * b);
        }

        Mesh BoardPanel(string name, List<BoardSpec> boards, int seed, float grimeTop)
        {
            var panel = new Panel(); var d = new Dice(seed);
            foreach (var b in boards) LayBoard(panel, b, ref d, grimeTop);
            return panel.Finish(name, ownedMeshes);
        }

        // ---- the walls ----------------------------------------------------------------------------------------------
        // Panel space as the composer places a wall: x along the trench (2 m, scaled by the edge's length), y up from the
        // floor (2 m, scaled by the bay's depth / 2), +z into the earth; the trench side is -z. Posts stand in front (-z).

        static BoardSpec Post(ref Dice d, float x, float height, Color timber, float lean)
        {
            float side = d.Range(.14f, .19f);
            return new BoardSpec
            {
                At = new Vector3(x, height * .5f - .06f, -.10f), Euler = new Vector3(d.Sym(2f), d.Sym(6f), 90f + lean),
                Length = height, Height = side, Thick = side, CutLeft = 0f, CutRight = d.Range(.05f, .16f) * (d.Chance(.5f) ? 1f : -1f),
                Tone = TimberTone(ref d, timber) * .92f, MayGo = false,
            };
        }

        static BoardSpec Plank(ref Dice d, float x, float y, float length, float height, float z, float roll, Color timber, bool mayGo = true)
        {
            return new BoardSpec
            {
                At = new Vector3(x, y, z), Euler = new Vector3(d.Sym(1.2f), d.Sym(1.5f), roll),
                Length = length, Height = height, Thick = d.Range(.05f, .08f),
                CutLeft = d.Sym(.07f), CutRight = d.Sym(.07f), Tone = TimberTone(ref d, timber), MayGo = mayGo,
            };
        }

        /// <summary>Horizontal boards up the wall, none the width of the next, held by two posts of no common height.</summary>
        static void WallOfBoards(ref Dice d, List<BoardSpec> into, Color timber, int cap, bool gap)
        {
            float y = 0f; int row = 0;
            while (y < 1.62f && (into.Count + (gap && row == 2 ? 2 : 1)) * BoardVerts <= cap)
            {
                float h = d.Range(.26f, .40f);
                float roll = d.Sym(3.2f);
                if (gap && row == 2)
                {
                    // a length that was too short: two boards that do not meet, the earth showing between
                    float split = d.Range(-.35f, .25f);
                    into.Add(Plank(ref d, (-1.04f + split - .06f) * .5f, y + h * .5f, split + 1.0f, h, .02f, roll, timber));
                    into.Add(Plank(ref d, (split + .12f + 1.06f) * .5f, y + h * .5f, 1.02f - split - .10f, h * d.Range(.85f, 1f), .025f, roll + d.Sym(3f), timber));
                }
                else into.Add(Plank(ref d, d.Sym(.12f), y + h * .5f, d.Range(1.92f, 2.26f), h, .015f + (row % 2) * .018f, roll, timber, row > 0));
                y += h + d.Range(-.03f, .025f);   // overlapping or gapping a little: no two rows meet the same way
                row++;
            }
        }

        Mesh[] BuildHandmadeWalls(Color timber, out Mesh[] damaged)
        {
            var whole = new Mesh[3]; damaged = new Mesh[3];
            int[] caps = { 168, 168, 192 };   // the even panels' vertex counts: never heavier (HandmadeTrenchTests)
            for (int variant = 0; variant < 3; variant++)
            {
                var d = new Dice(1101 + variant * 37);
                var boards = new List<BoardSpec>();
                int posts = 0;
                if (variant == 1)
                {
                    // planks stood on end, each sawn to its own height, a rail across them and one post
                    float x = -1.02f;
                    while (x < .96f && (boards.Count + 3) * BoardVerts <= caps[variant])
                    {
                        float w = d.Range(.25f, .36f), h = d.Range(1.35f, 1.92f);
                        var p = Plank(ref d, x + w * .5f, h * .5f, h, w, .02f, 90f + d.Sym(3.5f), timber, boards.Count > 0);
                        p.CutRight = d.Sym(.16f);   // the top: no two sawn alike
                        boards.Add(p);
                        x += w + d.Range(-.01f, .045f);
                    }
                    boards.Add(Plank(ref d, d.Sym(.1f), d.Range(1.05f, 1.3f), d.Range(1.9f, 2.2f), d.Range(.14f, .2f), -.05f, d.Sym(4f), timber, false));
                    boards.Add(Post(ref d, -.9f + d.Sym(.06f), d.Range(1.75f, 2.1f), timber, d.Sym(5f))); posts = 1;
                }
                else
                {
                    WallOfBoards(ref d, boards, timber, caps[variant] - 2 * BoardVerts - (variant == 2 ? BoardVerts : 0), variant == 2);
                    boards.Add(Post(ref d, -.92f + d.Sym(.05f), d.Range(1.7f, 2.15f), timber, d.Sym(4.5f)));
                    boards.Add(Post(ref d, d.Range(.25f, .7f), d.Range(1.55f, 2.05f), timber, d.Sym(6f))); posts = 2;
                    if (variant == 2)
                    {
                        // a prop wedged from the floor against a wall that was leaning in: its foot out in the trench,
                        // its top against the boards (turned about x after standing it up: the top goes toward +z)
                        boards.Add(new BoardSpec
                        {
                            At = new Vector3(d.Range(.0f, .5f), .78f, -.30f), Euler = new Vector3(d.Range(16f, 21f), d.Sym(8f), 90f + d.Sym(6f)),
                            Length = 1.6f, Height = .13f, Thick = .13f, CutLeft = .08f, CutRight = -.1f, Tone = TimberTone(ref d, timber) * .9f, MayGo = false,
                        });
                    }
                }
                while (boards.Count * BoardVerts > caps[variant]) boards.RemoveAt(boards.Count - 1 - posts);
                whole[variant] = BoardPanel("Hand-built revetment " + variant, boards, 2101 + variant, 1.4f);

                // broken: what was low stays, the top boards are gone or hang out into the trench, the posts snapped short
                var broken = new List<BoardSpec>();
                float lowTop = 0f;
                foreach (var b in boards)
                {
                    // the prop still props what is left (and keeps the panel's depth: TrenchSectionTests' footprint)
                    if (b.Euler.x > 12f) { broken.Add(b); continue; }
                    bool post = b.Euler.z > 60f && b.Thick > .12f;
                    if (post)
                    {
                        var stump = b; stump.Length = b.Length * d.Range(.5f, .62f); stump.At.y = stump.Length * .5f - .06f; stump.CutRight = d.Sym(.22f);
                        broken.Add(stump); continue;
                    }
                    float top = b.At.y + (variant == 1 && b.Euler.z > 60f ? b.Length : b.Height) * .5f;
                    if (top < .95f || (variant == 1 && b.Euler.z > 60f && broken.Count < 6))
                    {
                        var low = b;
                        if (variant == 1 && b.Euler.z > 60f) { low.Length = Mathf.Min(b.Length, d.Range(.55f, 1.0f)); low.At.y = low.Length * .5f; low.CutRight = d.Sym(.25f); }
                        broken.Add(low); lowTop = Mathf.Max(lowTop, low.At.y + low.Height * .5f);
                    }
                }
                // one board torn loose at one end, hanging out into the trench (toward -z only: it must not pass the wall's back)
                var hanging = Plank(ref d, .2f, Mathf.Max(lowTop, .5f) + .18f, 1.5f, .3f, -.02f, -14f + d.Sym(5f), timber, false);
                hanging.Euler.x = -10f;   // its top out toward the trench (-z), never into the earth behind the wall
                if ((broken.Count + 1) * BoardVerts <= 168) broken.Add(hanging);
                while (broken.Count * BoardVerts > 168) broken.RemoveAt(broken.Count - 1);
                damaged[variant] = BoardPanel("Broken hand-built revetment " + variant, broken, 2201 + variant, 1.4f);
            }
            return whole;
        }

        // ---- the floors ---------------------------------------------------------------------------------------------
        // Panel space as the composer lays duckboards: x along the trench (2 m), z across it, y up from the floor.

        static BoardSpec Slat(ref Dice d, float x, float width, Color timber, bool mayGo = true)
        {
            return new BoardSpec
            {
                At = new Vector3(x, .085f + d.Range(0f, .03f), d.Sym(.09f)), Euler = new Vector3(d.Sym(2f), 90f + d.Sym(3.5f), d.Sym(2.5f)),
                Length = d.Range(1.32f, 1.72f), Height = .09f, Thick = width,
                CutLeft = d.Sym(.05f), CutRight = d.Sym(.05f), Tone = TimberTone(ref d, timber), MayGo = mayGo,
            };
        }

        static BoardSpec Rail(ref Dice d, float z, Color timber)
        {
            return new BoardSpec
            {
                At = new Vector3(d.Sym(.03f), .02f, z + d.Sym(.05f)), Euler = new Vector3(0f, d.Sym(2.5f), d.Sym(1f)),
                Length = d.Range(1.94f, 2.04f), Height = .1f, Thick = .13f, CutLeft = d.Sym(.04f), CutRight = d.Sym(.04f),
                Tone = TimberTone(ref d, timber) * .82f, MayGo = false,
            };
        }

        Mesh[] BuildHandmadeFloors(Color timber, out Mesh[] damaged)
        {
            var whole = new Mesh[3]; damaged = new Mesh[3];
            for (int variant = 0; variant < 3; variant++)
            {
                var d = new Dice(1301 + variant * 41);
                var boards = new List<BoardSpec> { Rail(ref d, -.55f, timber), Rail(ref d, .55f, timber) };
                float x = -.98f; int slat = 0;   // the slats stay inside 2.2 m with their turn: AssetScaleTable kit/TrenchFloors
                while (x < .78f && boards.Count < (variant == 2 ? 7 : 8))
                {
                    float w = d.Range(.26f, .38f);
                    bool missing = variant == 1 && slat == 2;   // one never laid, or taken for the fire
                    if (!missing)
                    {
                        var s = Slat(ref d, x + w * .5f, w, timber, slat > 0);
                        if (variant == 1 && slat == 3) { s.Length = d.Range(.8f, 1.0f); s.At.z = d.Sym(.35f); s.Euler.z = -6f; s.At.y -= .02f; }   // snapped, one end in the mud
                        if (variant == 2 && slat == 1) { s.Euler.z = 5f; s.At.y += .03f; }                                                  // one end sprung up
                        boards.Add(s);
                    }
                    x += w + d.Range(.004f, .03f);
                    slat++;
                }
                if (variant == 2 && boards.Count < 8)
                {
                    // a plank laid along the slats where they had rotted: a mend, not a repair
                    boards.Add(new BoardSpec
                    {
                        At = new Vector3(d.Sym(.2f), .17f, d.Sym(.25f)), Euler = new Vector3(0f, d.Sym(9f), d.Sym(1.5f)),
                        Length = d.Range(1.3f, 1.7f), Height = .07f, Thick = d.Range(.26f, .34f), CutLeft = d.Sym(.08f), CutRight = d.Sym(.08f),
                        Tone = TimberTone(ref d, timber), MayGo = false,
                    });
                }
                while (boards.Count * BoardVerts > 168) boards.RemoveAt(boards.Count - 1);
                whole[variant] = BoardPanel("Hand-laid duckboards " + variant, boards, 2301 + variant, .3f);

                // broken: the rails and every other slat, and one snapped and kicked askew (the widest across survive, so
                // the boards still reach as far across the trench)
                var broken = new List<BoardSpec> { boards[0], boards[1] };
                int widest = 2; float reach = 0f;
                for (int i = 2; i < boards.Count; i++) if (boards[i].Length > reach) { reach = boards[i].Length; widest = i; }
                for (int i = 2; i < boards.Count && broken.Count < 5; i++) if (i == widest || i % 2 == 0) broken.Add(boards[i]);
                var snapped = Slat(ref d, d.Range(-.5f, .5f), .28f, timber, false);
                snapped.Length = .62f; snapped.Euler.y = 90f + 22f + d.Sym(8f); snapped.At.z = d.Sym(.3f);
                if ((broken.Count + 1) * BoardVerts <= 144) broken.Add(snapped);
                damaged[variant] = BoardPanel("Broken hand-laid duckboards " + variant, broken, 2401 + variant, .3f);
            }
            return whole;
        }

        // ---- the parapet --------------------------------------------------------------------------------------------
        // Panel space as the composer lays a parapet course: x along the trench, y up from the lip, +z away from the
        // trench (toward the enemy); the panel's origin is on the lip.

        /// <summary>How many sacks a course may have and stay no heavier than the even course (4, 3 and 2 sacks of 104
        /// vertices and 168 triangles; the burst course 3), and how tall it may stand (AssetScaleTable kit/TrenchBags at
        /// the composer's tallest scale).</summary>
        static readonly int[] MaxSacks = { 6, 4, 3 };
        const int MaxBurstSacks = 4;
        static readonly float[] MaxCourseHeight = { .74f, .66f, .56f };

        /// <summary>A course as men throw it up under fire: a foot of sacks along the whole length that overlap or leave
        /// a gap, now and then one crosswise; sacks thrown in behind; the rest thrown on top wherever they land, at any
        /// angle, to settle on what is there (LaySack). Rolled by the dice, kept only if it stands inside its rules.</summary>
        static List<SackSpec> Heap(ref Dice d, Color sack, int variant)
        {
            var list = new List<SackSpec>();
            int max = MaxSacks[variant];
            int foot = variant == 2 ? (d.Chance(.45f) ? 2 : 3) : (d.Chance(.3f) ? 3 : 2);
            float x = -1.08f;
            for (int i = 0; i < foot; i++)
            {
                bool across = d.Chance(variant == 2 ? .3f : .15f);
                float reach = across ? .62f : 1.04f;
                float centre = x + reach * .5f * d.Range(.85f, 1f);
                list.Add(FillSack(ref d, centre, d.Sym(.12f), across ? d.Range(64f, 116f) : d.Sym(17f), sack, across ? .92f : 1f));
                x = centre + reach * .5f * d.Range(.72f, 1.05f);   // the next overlaps it a little or leaves a gap
                if (foot == 2 && i == 0) x = Mathf.Max(x, -.05f);  // two must still reach both ends
            }
            if (variant < 2)
            {
                int behind = variant == 0 ? d.Range(1f, 2.99f) > 2f ? 2 : 1 : (d.Chance(.5f) ? 1 : 0);
                for (int i = 0; i < behind && list.Count < max; i++)
                    list.Add(FillSack(ref d, d.Range(-.6f, .6f), d.Range(.72f, .86f), d.Range(70f, 110f), sack, .9f));
            }
            while (list.Count < max)
            {
                // thrown on: anywhere along the length, a little forward or back, any angle; some across the rest
                bool across = d.Chance(.22f);
                var s = FillSack(ref d, d.Range(-.75f, .75f), d.Range(-.12f, .45f), across ? d.Range(60f, 120f) : d.Sym(40f), sack, d.Range(.88f, 1f));
                s.Droop = d.Range(.14f, .32f); s.Slump = d.Range(.3f, .8f);
                list.Add(s);
            }
            return list;
        }

        Mesh[] BuildHandmadeParapets(Color sack, out Mesh[] damaged)
        {
            var whole = new Mesh[3]; damaged = new Mesh[3];
            for (int variant = 0; variant < 3; variant++)
            {
                for (int attempt = 0; ; attempt++)
                {
                    var d = new Dice(1501 + variant * 43 + attempt * 977);
                    var sacks = Heap(ref d, sack, variant);
                    var seats = new List<float>();
                    var mesh = SackPanel("Hand-filled parapet " + variant, sacks, seats);
                    // broken: the sacks at the foot stay, some burst flat; the first thrown on has slipped; one more lies
                    // where it rolled, out in front of the lip
                    var burst = new List<SackSpec>(); bool slipped = false;
                    for (int i = 0; i < sacks.Count && burst.Count < MaxBurstSacks; i++)
                    {
                        if (seats[i] < .05f) burst.Add(d.Chance(.55f) ? Burst(sacks[i]) : sacks[i]);
                        else if (!slipped) { burst.Add(Slipped(ref d, sacks[i])); slipped = true; }
                    }
                    if (burst.Count < MaxBurstSacks) burst.Add(Rolled(ref d, sack, d.Range(-.5f, .5f), d.Range(.8f, 1.05f)));
                    var broken = SackPanel("Burst hand-filled parapet " + variant, burst);
                    Vector3 a = mesh.bounds.size, b = broken.bounds.size;
                    bool fits = a.x >= 1.9f && a.x <= 2.7f && a.y <= MaxCourseHeight[variant] && Mathf.Abs(mesh.bounds.center.x) < .25f
                        && b.x >= 1.8f && b.x <= a.x * 1.1f && b.z >= a.z * .9f && b.z <= a.z * 1.1f + .6f && b.y <= a.y + .02f && b.y >= a.y * .4f;
                    if (fits || attempt == 40)
                    {
                        if (!fits) Debug.LogWarning("BattlefieldKit: parapet " + variant + " kept a heap outside its rules after 40 rolls");
                        whole[variant] = mesh; damaged[variant] = broken; break;
                    }
                    ownedMeshes.Remove(mesh); ownedMeshes.Remove(broken); DiscardObject(mesh); DiscardObject(broken);
                }
            }
            return whole;
        }


        /// <summary>A sack knocked half off what held it: turned, and free to droop further.</summary>
        static SackSpec Slipped(ref Dice d, SackSpec s) { s.Yaw += d.Range(18f, 30f); s.X += d.Sym(.12f); s.Droop = .26f; s.Slump = .8f; return s; }

        /// <summary>A sack blown off the top and come to rest in front of the parapet, on the ground beyond the lip.</summary>
        static SackSpec Rolled(ref Dice d, Color sack, float x, float z)
        {
            var s = FillSack(ref d, x, z, 40f + d.Sym(12f), sack); s.Slump = .7f; s.Tone *= .88f; return s;
        }

        /// <summary>The hand-built lining: the same eighteen modules BuildTrenchVariants makes, on the same footprints, each
        /// with the shader's part jitter.</summary>
        void BuildHandmadeTrench(Color timber, Color sack)
        {
            var walls = BuildHandmadeWalls(timber, out var brokenWalls);
            var floors = BuildHandmadeFloors(timber, out var brokenFloors);
            var bags = BuildHandmadeParapets(sack, out var burstBags);
            for (int k = 0; k < 3; k++)
            {
                TrenchWalls[k] = Lining(walls[k], timber, false, 1.6f, BattlefieldPigment.Surface.Timber, WallJitter, WallAxes);
                TrenchWallsDamaged[k] = Lining(brokenWalls[k], timber * .9f, false, 1.6f, BattlefieldPigment.Surface.Timber, WallJitter, WallAxes);
                TrenchFloors[k] = Lining(floors[k], timber, false, .8f, BattlefieldPigment.Surface.Timber, FloorJitter, FloorAxes);
                TrenchFloorsDamaged[k] = Lining(brokenFloors[k], timber, false, .8f, BattlefieldPigment.Surface.Timber, FloorJitter, FloorAxes);
                TrenchBags[k] = Lining(bags[k], sack, true, 1.5f, BattlefieldPigment.Surface.Sacking, SackJitter, SackAxes);
                TrenchBagsDamaged[k] = Lining(burstBags[k], sack, true, 1.5f, BattlefieldPigment.Surface.Sacking, SackJitter, SackAxes);
            }
        }

        Module Lining(Mesh mesh, Color color, bool shadows, float outline, BattlefieldPigment.Surface surface, Vector4 jitter, Vector4 axes)
        {
            var module = Make(mesh, color, shadows, outline);
            Paint(module, surface);
            module.Material.EnableKeyword("_KITJITTER");
            module.Material.SetVector("_KitJitter", jitter);
            module.Material.SetVector("_KitJitterAxes", axes);
            if (surface == BattlefieldPigment.Surface.Sacking) module.Material.SetFloat("_Matte", .8f);   // soaked hessian, not glazed
            module.Material.SetFloat("_KitGrime", Grime);
            Joints(mesh);
            return module;
        }

        /// <summary>Where a part meets another part or the ground is a joint: rain runs into it and stays, and the silt,
        /// the damp and the rot start there and climb toward where the part lies bare. Writes into UV1.y, for the
        /// lining's grime (_KitGrime), how far each vertex lies from the nearest joint level with or below it, stretched sideways so
        /// the damp climbs more than it spreads: 0 at a joint, JointReach or more where nothing touches. UV1.x stays 0 (the
        /// house chunk channel). A part is told by its UV2 (pivot and seed). Built once with the kit.</summary>
        const float JointTouch = .045f, JointReach = .6f, JointSideways = 1.5f;

        static void Joints(Mesh mesh)
        {
            var v = mesh.vertices; var tris = mesh.triangles; var parts = new List<Vector4>(); mesh.GetUVs(2, parts);
            // a part is sampled at its vertices and at the middle of every triangle (a sack's stations are 30 cm apart)
            int n = v.Length, count = n + tris.Length / 3;
            var at = new Vector3[count]; var owner = new int[count]; var ids = new Dictionary<Vector4, int>();
            int Id(int i) { if (!ids.TryGetValue(parts[i], out int k)) ids[parts[i]] = k = ids.Count; return k; }
            for (int i = 0; i < n; i++) { at[i] = v[i]; owner[i] = Id(i); }
            for (int k = 0; k < tris.Length; k += 3) { at[n + k / 3] = (v[tris[k]] + v[tris[k + 1]] + v[tris[k + 2]]) / 3f; owner[n + k / 3] = owner[tris[k]]; }
            var grid = new Dictionary<Vector3Int, List<int>>();
            Vector3Int Cell(Vector3 p) => new Vector3Int(Mathf.FloorToInt(p.x / JointTouch), Mathf.FloorToInt(p.y / JointTouch), Mathf.FloorToInt(p.z / JointTouch));
            for (int i = 0; i < count; i++) { var c = Cell(at[i]); if (!grid.TryGetValue(c, out var l)) grid[c] = l = new List<int>(); l.Add(i); }
            var joints = new List<Vector3>();
            for (int i = 0; i < count; i++)
            {
                bool joint = at[i].y <= JointTouch * .6f;   // on (or sunk into) the ground
                var c = Cell(at[i]);
                for (int dx = -1; dx <= 1 && !joint; dx++)
                for (int dy = -1; dy <= 1 && !joint; dy++)
                for (int dz = -1; dz <= 1 && !joint; dz++)
                {
                    if (!grid.TryGetValue(c + new Vector3Int(dx, dy, dz), out var l)) continue;
                    foreach (int j in l)
                        if (owner[j] != owner[i] && (at[j] - at[i]).sqrMagnitude < JointTouch * JointTouch) { joint = true; break; }
                }
                if (joint) joints.Add(at[i]);
            }
            var wick = new List<Vector2>(n);
            for (int i = 0; i < n; i++)
            {
                float best = JointReach * JointReach; var p = v[i];
                foreach (var q in joints)
                {
                    float up = p.y - q.y;
                    if (up < -JointTouch) continue;   // the damp climbs: a joint above does not wet what hangs below it
                    float sx = (p.x - q.x) * JointSideways, sz = (p.z - q.z) * JointSideways;
                    float dd = sx * sx + sz * sz + up * up;
                    if (dd < best) best = dd;
                }
                wick.Add(new Vector2(0f, Mathf.Sqrt(best)));
            }
            mesh.SetUVs(1, wick);
        }
    }
}
