// Phase: riders prototype (presentation only) — depends on: TankModel (Resources/Vehicles/<Crab>, readable meshes)
// Where infantry can sit on a walker's back, and the way each man gets up there. Read off the machine's own mesh
// rather than authored, so a resized crab simply has more (or fewer) places:
//  - rays are dropped onto the body and what is bolted to it, in the body's own frame. A spot is a seat when it is
//    high on the shell, flat enough to kneel on, with footing a man's width round it, and not on (or under) a part a
//    man cannot sit on: a turret that traverses, the reactor, a hatch. A turret's gun sweeps its circle too, so a
//    seat under a barrel that would pass through a kneeling man's head is dropped as well;
//  - seats are a man's width apart, and are handed out spread over the deck (the middle first, then always the
//    free seat furthest from those taken), so four men sit round the shell rather than in a clump;
//  - each seat has a way up: the side of the deck (or its rear) whose foot is furthest from the legs' hips, so a
//    man climbs up the hull between the legs rather than through one. Edge is the last point of deck on the way
//    out, Foot the ground-level point just clear of the body below it (its y is meaningless: the ground decides).
// The model is already grown to VehicleSize (and any per-walker factor) when it gets here: every number is metres.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class RiderSeats
    {
        public struct Seat
        {
            public Vector3 Local;   // in the body part's frame: the point a kneeling man's knees rest on
            public float Yaw;       // radians in the body's frame, 0 = the way the machine faces
            public float Flat;      // the surface normal's y there (1 = level)
            public Vector3 Edge;    // the deck's edge on his way up/down, body frame
            public Vector3 Foot;    // where he stands on the ground to climb, body frame (xz only)
            public Vector3 Out;     // the way out from the seat to the edge, body frame, flat and unit
            public ulong Blind;     // bit k: the hull rises over his rifle within BlindReach on bearing k * 10 deg (body frame)
                                    // - he cannot shoot that way (tanks only; 0 on a walker)

            /// <summary>Is a bearing (radians in the body's frame, 0 = forward) behind the machine's own bulk for him?</summary>
            public bool BlindAt(float bearing)
            {
                int k = ((int)Mathf.Round(bearing * Mathf.Rad2Deg / 10f) % 36 + 36) % 36;
                return (Blind >> k & 1UL) != 0;
            }
        }
        /// <summary>A tank rider's rifle is this high over his seat, and the hull within BlindReach that rises over it hides
        /// that bearing from him: on the Maw's rear deck the head stands 2 m over the men, and shots fired forward crossed it
        /// (critic t4).</summary>
        public const float RifleHeight = 1.0f, BlindReach = 8f;

        public readonly List<Seat> Seats = new List<Seat>();
        /// <summary>What the last solve threw away, stage by stage (for the lab's readout and the tests).</summary>
        public int Deck, Sloped, Unfooted, UnderGun, Spaced, NoWay, OffTier, AtEdge;
        /// <summary>How far the machine reaches from its body's origin across the ground (xz), for throwing men clear of it.</summary>
        public float Span;
        public int BodyPart;
        /// <summary>The body's top in its own frame, and the lowest height still counted as deck.</summary>
        public float DeckTop, DeckFloor;

        /// <summary>How far apart two riders sit (a kneeling man is ~0.9 m across at UnitScale 1.125).</summary>
        public const float Spacing = 1.05f;
        /// <summary>A seat needs footing this far round it on every side, within Footing metres of height.</summary>
        public const float FootRadius = 0.32f, Footing = 0.30f;
        /// <summary>Only the upper share of the body counts as deck: below it are flanks and the skirt the legs hang from.</summary>
        public const float DeckShare = 0.55f;
        /// <summary>Surface normal y at or above which a man can kneel on it.</summary>
        public const float MinFlat = 0.85f;   // 0.74 let men kneel upright on the Pincer's 35-degree brow: they read as glued to its face (critic, 2026-09-25)
        /// <summary>A kneeling man's height at UnitScale 1.125, for the barrel check.</summary>
        public const float KneelHeight = 1.35f;
        public const int MaxSeats = 24;   // 16 capped a 1.4x Pincer and crammed its men into one corner of an empty deck
        /// <summary>A tank's men ride on ONE level of its hull, this far above or below the level with the most room: on the
        /// Maw the head's dome (8 m) and the rear deck (6 m) both counted, and men kneeling on two levels read as a heap
        /// (critic t2).</summary>
        public const float TankTier = 0.5f;
        /// <summary>A tank's seats stay this far inside the hull's outline: at 0.4 m men hung over the track guards (critic t2).</summary>
        public const float TankInset = 0.7f;
        /// <summary>A tank's men sit further apart than a walker's: its squad is capped, and at a man's width four of the
        /// Maw's eight stood shoulder to shoulder in a file down one edge of the deck (critic t5).</summary>
        public static readonly float[] TankSpacing = { 1.6f, 1.45f, 1.3f, 1.2f, Spacing };   // the widest that still seats the squad
        /// <summary>The most men a machine carries: a squad, not a crowd (19 on the Maw read as an unreadable heap, critic t2).</summary>
        public static int CapFor(byte archetype) => archetype == TW.Sim.VehicleArchetype.Maw ? 8 : MaxSeats;
        /// <summary>Keep riders out from under a traversing gun (a barrel at head height passes through a kneeling man).
        /// Off, they sit anywhere and the guns traverse through them: the Pincer's two turrets sweep most of its deck,
        /// so this decides between 3 seats and ~12 there. An owner's call; the lab flips it (RiderLab.ClearOfGuns).</summary>
        public static bool ClearOfGuns = true;

        /// <summary>A traversing gun in the body frame: the turret part that turns it, the turret's axis, how far the
        /// barrel reaches and how low it hangs, and where it points at rest (Bearing0, radians from the nose, positive to
        /// the right) with the turret at Yaw0. Kept whatever ClearOfGuns says: with the rule off, riders sit under these
        /// and duck as a barrel comes round (UnderBarrel).</summary>
        public struct GunSweep { public int Turret; public Vector2 Pivot; public float Reach, Low, Bearing0, Yaw0; }
        public readonly List<GunSweep> Guns = new List<GunSweep>();
        /// <summary>How far ahead of a turning barrel a man starts to duck, on top of his own width.</summary>
        public const float DuckAhead = 25f * Mathf.Deg2Rad;   // 22 left the kneel-to-prone under way as the barrel crossed his head (critic r1); 40 flattened most of the deck on every traverse and silenced the volleys (measured r19)

        /// <summary>Is the man kneeling at `local` (body frame) in the way of gun `gun`'s barrel, with its turret turned to
        /// `turretYaw` (radians, body frame, measured like Yaw0)?</summary>
        public bool UnderBarrel(Vector3 local, int gun, float turretYaw) => UnderBarrel(local, gun, turretYaw, DuckAhead);

        /// <summary>The same, with `ahead` radians of warning on top of the man's own width instead of DuckAhead.</summary>
        public bool UnderBarrel(Vector3 local, int gun, float turretYaw, float ahead)
        {
            var g = Guns[gun];
            float r = (new Vector2(local.x, local.z) - g.Pivot).magnitude;
            if (r > g.Reach + 0.25f || local.y + KneelHeight <= g.Low) return false;
            float a = Mathf.Atan2(local.x - g.Pivot.x, local.z - g.Pivot.y);
            float bearing = g.Bearing0 + (turretYaw - g.Yaw0);
            float margin = Mathf.Atan2(0.45f, Mathf.Max(0.3f, r)) + ahead;
            return Mathf.Abs(Mathf.DeltaAngle(a * Mathf.Rad2Deg, bearing * Mathf.Rad2Deg)) * Mathf.Deg2Rad <= margin;
        }

        /// <summary>A turret part's heading in the body frame (radians), from its matrix in that frame.</summary>
        public static float YawOf(Matrix4x4 inBody) { var f = inBody.MultiplyVector(Vector3.forward); return Mathf.Atan2(f.x, f.z); }

        struct Tri { public Vector3 A, B, C, N; public bool Seat; }
        struct Sweep { public Vector2 Pivot; public float Body, Gun, GunLow, Rest, Arc; }

        public static RiderSeats For(TankModel model, float step = 0.30f)
        {
            var seats = new RiderSeats();
            if (model == null || model.Lods[0] == null) return seats;
            var lod = model.Lods[0];
            var parts = lod.Parts;
            int body = -1;
            for (int i = 0; i < parts.Count; i++) if (parts[i].Parent < 0 && parts[i].Role == TankPartRole.Hull) { body = i; break; }
            if (body < 0) for (int i = 0; i < parts.Count; i++) if (parts[i].Parent < 0) { body = i; break; }
            if (body < 0) return seats;
            seats.BodyPart = body;

            // every part that rides ON the body, in the body's frame; the ones nobody sits on are kept as obstacles
            var tris = new List<Tri>(4096);
            var toBody = new Matrix4x4[parts.Count];
            var sweeps = new List<Sweep>();
            for (int i = 0; i < parts.Count; i++)
            {
                var p = parts[i];
                var local = Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
                toBody[i] = i == body ? Matrix4x4.identity : (p.Parent >= 0 && p.Parent != body ? toBody[p.Parent] * local : local);
                // a tank's tracks and wheels are nobody's seat, but they are in the way: kept as obstacles, a climber's foot
                // lands outside them and he goes up over them, not through them
                bool rolling = p.Role == TankPartRole.Track || p.Role == TankPartRole.Wheel;
                if (i != body && !Carried(lod, i, body) && !rolling) continue;
                AddTriangles(tris, p.Mesh, toBody[i], !rolling && (i == body || Seatable(p.Role)));
            }
            // what traverses: a turret's body, and its gun's circle at the barrel's height
            for (int i = 0; i < parts.Count; i++)
            {
                if (parts[i].Role != TankPartRole.Turret) continue;
                var pivot3 = toBody[i].GetPosition();
                var sw = new Sweep { Pivot = new Vector2(pivot3.x, pivot3.z), GunLow = float.MaxValue, Arc = Mathf.PI };
                sw.Body = Reach(parts[i].Mesh, toBody[i], sw.Pivot, out _);
                for (int c = 0; c < parts.Count; c++)
                {
                    if (parts[c].Role != TankPartRole.Gun || !Under(lod, c, i)) continue;
                    sw.Gun = Mathf.Max(sw.Gun, Reach(parts[c].Mesh, toBody[c], sw.Pivot, out float low));
                    sw.GunLow = Mathf.Min(sw.GunLow, low);
                    // the gun traverses only its arc (TankSpec): the wedge behind it is never swept
                    var spec = TW.Sim.Combat.TankSpec.For(model.Archetype);
                    int g = parts[c].Gun;
                    if (g >= 0 && g < spec.GunCount && !spec.Gun(g).FullCircle) { sw.Rest = spec.Gun(g).RestYaw; sw.Arc = spec.Gun(g).ArcHalf; }
                }
                sweeps.Add(sw);
                if (sw.Gun > 0f)
                {
                    // where the barrel points at rest: from the turret's axis to the middle of its guns
                    Vector2 aim = Vector2.zero; int nv = 0;
                    for (int c = 0; c < parts.Count; c++)
                    {
                        if (parts[c].Role != TankPartRole.Gun || !Under(lod, c, i) || parts[c].Mesh == null || !parts[c].Mesh.isReadable) continue;
                        foreach (var v in parts[c].Mesh.vertices) { var w = toBody[c].MultiplyPoint3x4(v); aim += new Vector2(w.x, w.z); nv++; }
                    }
                    if (nv > 0)
                    {
                        aim = aim / nv - sw.Pivot;
                        seats.Guns.Add(new GunSweep { Turret = i, Pivot = sw.Pivot, Reach = sw.Gun, Low = sw.GunLow, Bearing0 = Mathf.Atan2(aim.x, aim.y), Yaw0 = YawOf(toBody[i]) });
                    }
                }
            }
            if (tris.Count == 0) return seats;

            var b = parts[body].Mesh.bounds;
            seats.DeckTop = b.max.y;
            seats.DeckFloor = b.min.y + (b.max.y - b.min.y) * DeckShare;
            float top = b.max.y + 4f;
            var all = b;
            foreach (var t in tris) { all.Encapsulate(t.A); all.Encapsulate(t.B); all.Encapsulate(t.C); }
            // every drop asks only the triangles over its own half-metre cell: testing all of them for each of a tank's
            // ~9 drops per candidate took the Maw past the editor's 5 s call limit
            var grid = new TriGrid(tris, all, 0.5f);

            // candidates: a grid over the footprint, each dropped onto the deck
            var cands = new List<Seat>(256);
            for (float z = b.min.z + step * 0.5f; z < b.max.z; z += step)
                for (float x = b.min.x + step * 0.5f; x < b.max.x; x += step)
                {
                    if (!Drop(grid, x, z, top, out float y, out Vector3 n, out bool seatable) || !seatable || y < seats.DeckFloor) continue;
                    seats.Deck++;
                    if (n.y < MinFlat) { seats.Sloped++; continue; }
                    bool footed = true;
                    for (int k = 0; k < 8 && footed; k++)
                    {
                        float a = k * Mathf.PI * 0.25f;
                        footed = Drop(grid, x + Mathf.Cos(a) * FootRadius, z + Mathf.Sin(a) * FootRadius, top, out float fy, out _, out bool fs) && fs && Mathf.Abs(fy - y) <= Footing;
                    }
                    if (!footed) { seats.Unfooted++; continue; }
                    if (ClearOfGuns && Swept(sweeps, x, y, z)) { seats.UnderGun++; continue; }
                    cands.Add(new Seat { Local = new Vector3(x, y, z), Flat = n.y });
                }

            foreach (var t in tris) foreach (var p in new[] { t.A, t.B, t.C }) seats.Span = Mathf.Max(seats.Span, new Vector2(p.x, p.z).magnitude);

            bool tank = TW.Sim.VehicleArchetype.IsTank(model.Archetype);
            if (tank && cands.Count > 0)
            {
                // one level: the height band holding the most candidates
                float bestY = cands[0].Local.y; int bestN = -1;
                foreach (var c in cands)
                {
                    int n = 0;
                    foreach (var o in cands) if (Mathf.Abs(o.Local.y - c.Local.y) <= TankTier) n++;
                    if (n > bestN) { bestN = n; bestY = c.Local.y; }
                }
                for (int k = cands.Count - 1; k >= 0; k--)
                {
                    var p = cands[k].Local;
                    if (Mathf.Abs(p.y - bestY) > TankTier) { cands.RemoveAt(k); seats.OffTier++; continue; }
                    if (p.x < b.min.x + TankInset || p.x > b.max.x - TankInset || p.z < b.min.z + TankInset || p.z > b.max.z - TankInset) { cands.RemoveAt(k); seats.AtEdge++; }
                }
            }

            // pick: the middle of the deck first, then outward, never closer than Spacing to a seat already taken
            Vector2 mid = new Vector2(b.center.x, b.center.z);
            cands.Sort((p, q) => (new Vector2(p.Local.x, p.Local.z) - mid).sqrMagnitude.CompareTo((new Vector2(q.Local.x, q.Local.z) - mid).sqrMagnitude));
            var picked = new List<Seat>();
            int spaced = 0;
            foreach (float gap in tank ? TankSpacing : new[] { Spacing })
            {
                picked.Clear(); spaced = 0;
                foreach (var c in cands)
                {
                    if (picked.Count >= MaxSeats) break;
                    bool clear = true;
                    foreach (var s in picked)
                        if (new Vector2(s.Local.x - c.Local.x, s.Local.z - c.Local.z).sqrMagnitude < gap * gap) { clear = false; break; }
                    if (clear) picked.Add(c); else spaced++;
                }
                if (picked.Count >= CapFor(model.Archetype)) break;
            }
            seats.Spaced += spaced;

            // the way up for each: the side or the rear, whichever puts its foot furthest from the hips
            var hips = new List<Vector2>();
            if (lod.Legs != null) foreach (var leg in lod.Legs) if (leg != null) hips.Add(new Vector2(leg.Hip.x, leg.Hip.z));
            float halfW = Mathf.Max(0.5f, b.extents.x);
            for (int k = picked.Count - 1; k >= 0; k--)
            {
                var s = picked[k];
                // every way off the deck but forward (the claws and the jaw are there), rear quarters included
                var dirs = new[] { new Vector3(1f, 0f, 0f), new Vector3(-1f, 0f, 0f), new Vector3(0f, 0f, -1f), new Vector3(0.7071f, 0f, -0.7071f), new Vector3(-0.7071f, 0f, -0.7071f), new Vector3(0.9239f, 0f, 0.3827f), new Vector3(-0.9239f, 0f, 0.3827f) };
                float best = float.NegativeInfinity; bool found = false;
                foreach (var d in dirs)
                {
                    if (!WayOut(grid, s.Local, d, seats.DeckFloor, top, all, out var edge, out var foot)) continue;
                    // clear of the legs, but not by crossing the whole deck: every metre walked over the shell costs as
                    // much as a metre of room at the foot (a way up 1 m from a hip is still a way up; 4 m of deck is a stroll)
                    float clear = 99f;
                    foreach (var h in hips) clear = Mathf.Min(clear, (new Vector2(foot.x, foot.z) - h).magnitude);
                    float walk = new Vector2(edge.x - s.Local.x, edge.z - s.Local.z).magnitude;
                    float score = Mathf.Min(clear, 1.6f) - walk;
                    if (score > best) { best = score; s.Edge = edge; s.Foot = foot; s.Out = d; found = true; }
                }
                if (!found) { picked.RemoveAt(k); seats.NoWay++; continue; }
                if (tank)
                    for (int a = 0; a < 36; a++)
                    {
                        float ang = a * 10f * Mathf.Deg2Rad; var dir = new Vector2(Mathf.Sin(ang), Mathf.Cos(ang));
                        for (float r = 0.6f; r <= BlindReach; r += 0.3f)
                            if (Drop(grid, s.Local.x + dir.x * r, s.Local.z + dir.y * r, top, out float hy, out _, out _) && hy > s.Local.y + RifleHeight) { s.Blind |= 1UL << a; break; }
                    }
                // a man on the flank looks out over his side; a man on the spine looks the way the machine goes
                float side = (s.Local.x - b.center.x) / halfW;
                s.Yaw = Mathf.Abs(side) > 0.4f ? Mathf.Atan2(s.Local.x - b.center.x, Mathf.Max(0.35f, s.Local.z - b.center.z + halfW * 0.6f)) : 0f;
                picked[k] = s;
            }

            // hand them out spread: the most central first, then always the seat furthest from every seat taken
            var used = new bool[picked.Count];
            for (int n = 0; n < picked.Count; n++)
            {
                int pick = 0; float far = -1f;
                for (int i = 0; i < picked.Count; i++)
                {
                    if (used[i]) continue;
                    if (n == 0) { pick = i; break; }   // already sorted centre-out
                    float near = float.MaxValue;
                    foreach (var s in seats.Seats) near = Mathf.Min(near, new Vector2(s.Local.x - picked[i].Local.x, s.Local.z - picked[i].Local.z).sqrMagnitude);
                    if (near > far) { far = near; pick = i; }
                }
                used[pick] = true;
                seats.Seats.Add(picked[pick]);
                if (seats.Seats.Count >= CapFor(model.Archetype)) break;   // handed out spread, so the first few are the spread few
            }
            return seats;
        }

        /// <summary>From a seat outward along `d`: the last point of deck (Edge), then on until nothing of the machine is
        /// under him, plus a stride (Foot). False if the way crosses something nobody walks over, or never gets off.</summary>
        static bool WayOut(TriGrid tris, Vector3 seat, Vector3 d, float floor, float top, Bounds all, out Vector3 edge, out Vector3 foot)
        {
            edge = seat; foot = seat;
            const float s = 0.1f;
            bool onDeck = true;
            float limit = all.extents.magnitude * 2f + 2f;
            for (float t = s; t < limit; t += s)
            {
                Vector3 p = seat + d * t;
                bool hit = Drop(tris, p.x, p.z, top, out float y, out _, out bool seatable);
                if (onDeck)
                {
                    if (hit && y >= floor - 0.4f && Mathf.Abs(y - edge.y) < 0.9f)
                    {
                        if (!seatable) return false;   // the way out runs over the reactor, a turret...
                        edge = new Vector3(p.x, y, p.z);
                        continue;
                    }
                    onDeck = false;
                }
                if (!hit) { foot = p + d * 0.35f; foot.y = 0f; return true; }
            }
            return false;
        }

        static bool Swept(List<Sweep> sweeps, float x, float y, float z)
        {
            foreach (var sw in sweeps)
            {
                float r = (new Vector2(x, z) - sw.Pivot).magnitude;
                if (r < sw.Body + 0.25f) return true;
                if (r < sw.Gun + 0.25f && y + KneelHeight > sw.GunLow)
                {
                    // the angle of the seat from the turret, from the nose, positive to the right: inside the arc (plus
                    // the width of a man at that distance) the barrel passes through him
                    float a = Mathf.Atan2(x - sw.Pivot.x, z - sw.Pivot.y), margin = Mathf.Atan2(0.45f, Mathf.Max(0.3f, r));
                    if (Mathf.Abs(Mathf.DeltaAngle(a * Mathf.Rad2Deg, sw.Rest * Mathf.Rad2Deg)) * Mathf.Deg2Rad <= sw.Arc + margin) return true;
                }
            }
            return false;
        }

        /// <summary>How far the mesh reaches from a vertical axis at `pivot` (xz), and its lowest point.</summary>
        static float Reach(Mesh mesh, Matrix4x4 m, Vector2 pivot, out float low)
        {
            low = float.MaxValue; float r = 0f;
            if (mesh == null || !mesh.isReadable) return 0f;
            foreach (var v in mesh.vertices)
            {
                var w = m.MultiplyPoint3x4(v);
                r = Mathf.Max(r, (new Vector2(w.x, w.z) - pivot).magnitude);
                low = Mathf.Min(low, w.y);
            }
            return r;
        }

        // a Shield is posed apart from the body in play (the Pavise's swings), so men seated on its rim at rest hung in the
        // air beside it (critic r9): it is an obstacle, never a seat
        static bool Seatable(TankPartRole r) => r != TankPartRole.Turret && r != TankPartRole.Reactor && r != TankPartRole.Hatch && r != TankPartRole.Cupola && r != TankPartRole.Horn && r != TankPartRole.Exhaust && r != TankPartRole.Shield;

        static bool Under(TankModel.Lod lod, int i, int ancestor)
        {
            for (int k = lod.Parts[i].Parent; k >= 0; k = lod.Parts[k].Parent) if (k == ancestor) return true;
            return false;
        }

        /// <summary>A part carried on the body: reached from the body through no leg, claw, jaw or gun.</summary>
        static bool Carried(TankModel.Lod lod, int i, int body)
        {
            for (int k = i; k >= 0; k = lod.Parts[k].Parent)
            {
                if (k == body) return true;
                var r = lod.Parts[k].Role;
                if (r == TankPartRole.Leg || r == TankPartRole.Thigh || r == TankPartRole.Shin || r == TankPartRole.Foot ||
                    r == TankPartRole.Claw || r == TankPartRole.Jaw || r == TankPartRole.Gun || r == TankPartRole.Track || r == TankPartRole.Wheel) return false;
            }
            return false;
        }

        static void AddTriangles(List<Tri> tris, Mesh mesh, Matrix4x4 m, bool seat)
        {
            if (mesh == null || !mesh.isReadable) return;
            var v = mesh.vertices; var idx = mesh.triangles;
            for (int t = 0; t + 2 < idx.Length; t += 3)
            {
                var tri = new Tri { A = m.MultiplyPoint3x4(v[idx[t]]), B = m.MultiplyPoint3x4(v[idx[t + 1]]), C = m.MultiplyPoint3x4(v[idx[t + 2]]), Seat = seat };
                tri.N = Vector3.Cross(tri.B - tri.A, tri.C - tri.A);
                if (tri.N.sqrMagnitude < 1e-10f) continue;
                tri.N.Normalize();
                tris.Add(tri);
            }
        }

        /// <summary>The highest surface under (x, z), looking straight down from `top`, and whether a man may sit on it.</summary>
        /// <summary>The triangles bucketed by the xz cells their footprint covers.</summary>
        sealed class TriGrid
        {
            public readonly List<Tri> Tris;
            readonly List<int>[] cells; readonly float x0, z0, size; readonly int nx, nz;
            public TriGrid(List<Tri> tris, Bounds b, float size)
            {
                Tris = tris; this.size = size; x0 = b.min.x; z0 = b.min.z;
                nx = Mathf.Max(1, Mathf.CeilToInt(b.size.x / size) + 1); nz = Mathf.Max(1, Mathf.CeilToInt(b.size.z / size) + 1);
                cells = new List<int>[nx * nz];
                for (int i = 0; i < tris.Count; i++)
                {
                    var t = tris[i];
                    int ax = Cx(Mathf.Min(t.A.x, Mathf.Min(t.B.x, t.C.x))), bx = Cx(Mathf.Max(t.A.x, Mathf.Max(t.B.x, t.C.x)));
                    int az = Cz(Mathf.Min(t.A.z, Mathf.Min(t.B.z, t.C.z))), bz = Cz(Mathf.Max(t.A.z, Mathf.Max(t.B.z, t.C.z)));
                    for (int z = az; z <= bz; z++)
                        for (int x = ax; x <= bx; x++) { ref var c = ref cells[z * nx + x]; (c ??= new List<int>(8)).Add(i); }
                }
            }
            int Cx(float x) => Mathf.Clamp((int)((x - x0) / size), 0, nx - 1);
            int Cz(float z) => Mathf.Clamp((int)((z - z0) / size), 0, nz - 1);
            public List<int> At(float x, float z)
            {
                int cx = (int)Mathf.Floor((x - x0) / size), cz = (int)Mathf.Floor((z - z0) / size);
                return cx < 0 || cz < 0 || cx >= nx || cz >= nz ? null : cells[cz * nx + cx];
            }
        }

        static bool Drop(TriGrid grid, float x, float z, float top, out float y, out Vector3 normal, out bool seatable)
        {
            y = float.NegativeInfinity; normal = Vector3.up; seatable = false; bool hit = false;
            var cell = grid.At(x, z);
            if (cell == null) return false;
            var tris = grid.Tris;
            foreach (int i in cell)
            {
                var t = tris[i];
                // 2D point-in-triangle in the xz plane, then the plane's height there
                float d = (t.B.z - t.C.z) * (t.A.x - t.C.x) + (t.C.x - t.B.x) * (t.A.z - t.C.z);
                if (Mathf.Abs(d) < 1e-9f) continue;
                float l0 = ((t.B.z - t.C.z) * (x - t.C.x) + (t.C.x - t.B.x) * (z - t.C.z)) / d;
                float l1 = ((t.C.z - t.A.z) * (x - t.C.x) + (t.A.x - t.C.x) * (z - t.C.z)) / d;
                float l2 = 1f - l0 - l1;
                if (l0 < -1e-4f || l1 < -1e-4f || l2 < -1e-4f) continue;
                float hy = l0 * t.A.y + l1 * t.B.y + l2 * t.C.y;
                if (hy > top || hy <= y) continue;
                y = hy; normal = t.N.y < 0f ? -t.N : t.N; seatable = t.Seat; hit = true;
            }
            return hit;
        }
    }
}
