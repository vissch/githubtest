// Phase: A5b / C4 (implemented) — depends on: Resources/Vehicles/<Tank>/<Tank>_LOD0|1.fbx (Tools/tanksplit.py, TankImport)
// One of the owner's tanks as TankRenderer needs it: the parts of each LOD in parent-first order with their pivots
// (each part's origin; the FBX hierarchy is kept), what each part does (its role, its side, which gun it carries),
// the Socket_* empties (muzzles, exhaust outlets, fire on the engine deck, the crew hatch, dust behind the tracks),
// and what is measured off them: the direction each gun was modelled pointing (a sponson's rest), the track gauge
// and length. The far LOD has the same part names and pivots minus the tracks and wheels (fused into its hull).
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public enum TankPartRole : byte
    {
        Hull, Track, Wheel, Turret, Gun, Hatch, Sponson, Cupola, Horn, Exhaust, Other,
        // the walkers (Tools/crabsplit.py): a leg in one piece, or a leg in three, and what a crab carries
        Leg, Thigh, Shin, Foot, Claw, Jaw, Shield, Drum, Reactor,
    }

    public sealed class TankModel
    {
        public sealed class Part
        {
            public string Name;
            public Mesh Mesh;
            public int Parent = -1;
            public Vector3 Local;
            public Quaternion LocalRot = Quaternion.identity;
            public TankPartRole Role;
            public int Side;            // -1 left, +1 right, 0 on the centre line
            public int Gun = -1;        // the gun it is (or carries): 0 / 1
            public Vector3 Center;      // mesh bounds centre, in the part's own frame
            public float Radius;
            /// <summary>A walker's legs: which leg this part belongs to (-1 none; the left side's first, in name
            /// order, which is the order VehicleModulesSystem takes them off in), and the way that leg points out
            /// from the body in the body's own frame. A shin and a foot take both from the thigh above them.</summary>
            public int Leg = -1;
            public Vector3 Outward = Vector3.forward;
            /// <summary>A gun with no turret above it (a mortar, a pintle gun): it takes the traverse itself.</summary>
            public bool SelfAimed;
        }

        /// <summary>What WalkerGait needs to put one leg's foot on a chosen piece of ground: the parts from the hip
        /// outward, where that hip sits in the body's frame, where the toe sits in the last part's frame, and how far
        /// the leg reaches when it is standing as it was modelled. A leg may be one piece (Pincer), a thigh and a
        /// shin, or a thigh, a shin and a foot (Kettle) — the solver takes them all the same way.</summary>
        public sealed class LegRig
        {
            public int[] Chain;             // part indices, hip first
            public Vector3 Hip;             // the first part's pivot, in the body's frame
            public Vector3 Toe;             // the tip, in the LAST chain part's own frame
            public Vector3 Rest;            // hip -> toe in the body's frame, as modelled
            public Vector3 Outward;         // the way it stands out from the body, flattened and normalised
            public float Reach;             // |Rest|: how far the toe is from the hip when it stands as modelled
            public float Drop;              // how far the toe falls below the hip at rest (always positive)
            public float[] Bone;            // link lengths: hip->joint, ..., last joint->toe
            public Quaternion[] RestRot;    // each chain part's orientation in the body's frame, as modelled
            public Vector3[] RestDir;       // each bone's direction in the body's frame, as modelled
            public Quaternion ParentRot;    // the hip's parent's orientation in the body's frame (the body's own)
        }

        public sealed class Lod
        {
            public readonly List<Part> Parts = new List<Part>();
            /// <summary>By leg number, or null on a machine with no legs. An entry may be null if that leg number
            /// has no parts (a lopsided split).</summary>
            public LegRig[] Legs;
            public int Find(string name)
            {
                for (int i = 0; i < Parts.Count; i++) if (Parts[i].Name == name) return i;
                return -1;
            }
        }

        public string Name;
        public byte Archetype;
        public readonly Lod[] Lods = new Lod[2];
        /// <summary>LOD0 part index and offset in that part's frame, by socket name.</summary>
        public readonly Dictionary<string, (int part, Vector3 local)> Sockets = new Dictionary<string, (int, Vector3)>();
        public readonly float[] ArtRestYaw = new float[2];       // radians from the nose, positive to the right
        public readonly Vector3[] MuzzleLocal = new Vector3[2];  // in the gun's (or sponson's) own frame
        public readonly int[] GunPart = { -1, -1 };               // LOD0 part that fires gun k
        public float LinkLength = 0.34f;                          // tanksplit: a tread link every ~34 cm
        public float WheelRadius = 0.36f;
        public float HalfGauge = 1.65f, HalfLength = 2.5f, Height = 3f;   // Height: the hull's top above the ground
        public int LegCount;                                              // 0 on a tank
        /// <summary>How many leg NUMBERS each side has. Not LegCount/2: the numbering leaves a gap when a split
        /// comes out lopsided (Tripo builds these things with three legs one side and two the other), so the side a
        /// leg belongs to is leg/LegsPerSide and nothing else.</summary>
        public int LegsPerSide;

        /// <summary>The tanks' root part is the Hull; a walker's is its Body (Tools/crabsplit.py).</summary>
        /// <param name="scale">how much bigger than the sculpt to build it (VehicleSize). Baked into the
        /// pivots and the vertices here rather than carried in a matrix, so that every number taken off the
        /// model afterwards - a leg's reach, the hull's height, a part's radius - is already in real metres and
        /// nothing downstream has to remember to multiply.</param>
        public static TankModel Load(string name, byte archetype, string root = "Hull", float scale = 1f)
        {
            var model = new TankModel { Name = name, Archetype = archetype };
            for (int lod = 0; lod < 2; lod++)
            {
                var go = Resources.Load<GameObject>($"Vehicles/{name}/{name}_LOD{lod}");
                var hull = go != null ? Find(go.transform, root) : null;
                if (hull == null)
                {
                    if (lod == 0) { Debug.LogWarning($"TankModel: Resources/Vehicles/{name}/{name}_LOD0 is missing or has no {root}"); return null; }
                    model.Lods[1] = model.Lods[0];
                    break;
                }
                var l = new Lod();
                model.Lods[lod] = l;
                Walk(model, l, hull, -1, lod == 0, scale);
            }
            model.LinkLength *= scale; model.WheelRadius *= scale;
            var parts = model.Lods[0].Parts;
            for (int i = 0; i < parts.Count; i++) if (parts[i].Gun >= 0) model.GunPart[parts[i].Gun] = i;
            for (int k = 0; k < 2; k++)
            {
                string socket = k == 0 ? (model.Sockets.ContainsKey("Socket_Muzzle") ? "Socket_Muzzle" : "Socket_Muzzle_L") : "Socket_Muzzle_R";
                if (!model.Sockets.TryGetValue(socket, out var s)) continue;
                model.MuzzleLocal[k] = s.local;
                model.ArtRestYaw[k] = Mathf.Atan2(s.local.x, s.local.z);
            }
            int tl = model.Lods[0].Find("Track_L"), tr = model.Lods[0].Find("Track_R");
            if (tl >= 0 && tr >= 0)
            {
                model.HalfGauge = (Mathf.Abs(parts[tl].Local.x) + Mathf.Abs(parts[tr].Local.x)) * 0.5f;
                model.HalfLength = parts[tl].Mesh.bounds.extents.z;
            }
            int hullPart = model.Lods[0].Find(root);
            if (hullPart >= 0)
            {
                var hull = parts[hullPart].Mesh.bounds;
                model.Height = hull.max.y;
                // A machine with no treads to measure: take its body, rather than keep a default that was
                // written for a tank and is used to place fire and smoke on the hull.
                if (tl < 0 || tr < 0) { model.HalfGauge = hull.extents.x; model.HalfLength = hull.extents.z; }
            }
            for (int lod = 0; lod < 2; lod++) if (model.Lods[lod] != null && (lod == 0 || model.Lods[1] != model.Lods[0])) NumberLegs(model, model.Lods[lod]);
            for (int lod = 0; lod < 2; lod++) if (model.Lods[lod] != null && (lod == 0 || model.Lods[1] != model.Lods[0])) BuildRigs(model, model.Lods[lod], lod == 0);
            for (int lod = 0; lod < 2; lod++)
            {
                var l = model.Lods[lod];
                if (l == null || (lod == 1 && l == model.Lods[0])) continue;
                foreach (var p in l.Parts)
                    p.SelfAimed = p.Role == TankPartRole.Gun && (p.Parent < 0 || l.Parts[p.Parent].Role != TankPartRole.Turret);
            }
            return model;
        }

        /// <summary>Give every leg its number and the direction it stands out in. The numbering has to agree with the
        /// simulation's, which takes a walker's legs off left side first, in order, so the leg that flies off is the
        /// leg that stopped working.</summary>
        static void NumberLegs(TankModel model, Lod lod)
        {
            var parts = lod.Parts;
            var root = new Vector3[parts.Count];
            for (int i = 0; i < parts.Count; i++) root[i] = parts[i].Parent >= 0 ? root[parts[i].Parent] + parts[i].Local : parts[i].Local;
            var left = new List<int>(); var right = new List<int>();
            for (int i = 0; i < parts.Count; i++)
                if (parts[i].Role == TankPartRole.Leg || parts[i].Role == TankPartRole.Thigh)
                    (root[i].x < 0f ? left : right).Add(i);
            left.Sort((a, b) => string.CompareOrdinal(parts[a].Name, parts[b].Name));
            right.Sort((a, b) => string.CompareOrdinal(parts[a].Name, parts[b].Name));
            int perSide = Mathf.Max(left.Count, right.Count);
            for (int k = 0; k < left.Count; k++) Number(parts, left[k], k, root[left[k]]);
            for (int k = 0; k < right.Count; k++) Number(parts, right[k], perSide + k, root[right[k]]);
            model.LegCount = left.Count + right.Count;
            model.LegsPerSide = perSide;
            // a shin and a foot belong to the thigh above them
            for (int i = 0; i < parts.Count; i++)
            {
                if (parts[i].Leg >= 0) continue;
                for (int k = parts[i].Parent; k >= 0; k = parts[k].Parent)
                    if (parts[k].Leg >= 0) { parts[i].Leg = parts[k].Leg; parts[i].Outward = parts[k].Outward; break; }
            }
        }

        /// <summary>Work out each leg's chain, hip and toe. The toe is taken from a Socket_Toe_* if the splitter
        /// left one, and otherwise from the geometry: the lowest point of the last part in the chain, under its
        /// middle. Deriving it matters — crabsplit only writes that socket for a part called Leg_* or Foot_*, so the
        /// machines whose legs end in a shin or a bare thigh (Censer, Pavise, Banner) have never had one.</summary>
        static void BuildRigs(TankModel model, Lod lod, bool useSockets)
        {
            if (model.LegCount <= 0) return;
            var parts = lod.Parts;
            lod.Legs = new LegRig[Mathf.Max(model.LegCount, model.LegsPerSide * 2)];

            // each part's frame in the body's, the way Pose accumulates them
            var pos = new Vector3[parts.Count];
            var rot = new Quaternion[parts.Count];
            for (int i = 0; i < parts.Count; i++)
            {
                int up = parts[i].Parent;
                if (up >= 0) { rot[i] = rot[up] * parts[i].LocalRot; pos[i] = pos[up] + rot[up] * parts[i].Local; }
                else { rot[i] = parts[i].LocalRot; pos[i] = parts[i].Local; }
            }

            for (int leg = 0; leg < lod.Legs.Length; leg++)
            {
                // the parts of this leg, hip first: Parts is already parent-first, so index order is chain order
                var chain = new List<int>();
                for (int i = 0; i < parts.Count; i++) if (parts[i].Leg == leg && IsLimb(parts[i].Role)) chain.Add(i);
                if (chain.Count == 0) continue;

                int hip = chain[0], tip = chain[chain.Count - 1];
                Vector3 toe = ToeOf(model, lod, chain, tip, useSockets, out Vector3 authored);
                Vector3 toeInBody = pos[tip] + rot[tip] * toe;
                Vector3 rest = toeInBody - pos[hip];
                // Taking the toe's height from the mesh is right wherever the socket and the geometry disagree by
                // a plausible amount, and it is what stops a Pincer floating and a Kettle wading. Redoubt is the
                // case where it is not: its front sockets sit 2.15 m below the foot mesh, far enough that the
                // mesh's own floor is ABOVE the hip, and a leg whose toe is above its hip is not a leg. Where the
                // correction produces that, the authored socket was carrying information this does not have, so
                // it is left alone rather than forced.
                if (rest.y >= -0.05f)
                {
                    toe = authored;
                    toeInBody = pos[tip] + rot[tip] * toe;
                    rest = toeInBody - pos[hip];
                }

                var bone = new float[chain.Count];
                var restRot = new Quaternion[chain.Count];
                var restDir = new Vector3[chain.Count];
                for (int k = 0; k < chain.Count; k++)
                {
                    Vector3 from = pos[chain[k]];
                    Vector3 to = k + 1 < chain.Count ? pos[chain[k + 1]] : toeInBody;
                    Vector3 along = to - from;
                    bone[k] = along.magnitude;
                    restRot[k] = rot[chain[k]];
                    restDir[k] = bone[k] > 1e-5f ? along / bone[k] : Vector3.down;
                }

                Vector3 flat = new Vector3(rest.x, 0f, rest.z);
                if (flat.sqrMagnitude < 1e-6f) flat = parts[hip].Outward;
                lod.Legs[leg] = new LegRig
                {
                    Chain = chain.ToArray(), Hip = pos[hip], Toe = toe, Rest = rest,
                    Outward = flat.normalized, Reach = rest.magnitude, Drop = Mathf.Max(0.05f, -rest.y), Bone = bone,
                    RestRot = restRot, RestDir = restDir,
                    ParentRot = parts[hip].Parent >= 0 ? rot[parts[hip].Parent] : Quaternion.identity,
                };
            }
        }

        static bool IsLimb(TankPartRole r)
            => r == TankPartRole.Leg || r == TankPartRole.Thigh || r == TankPartRole.Shin || r == TankPartRole.Foot;

        /// <summary>The tip of a leg in the last chain part's own frame.</summary>
        static Vector3 ToeOf(TankModel model, Lod lod, List<int> chain, int tip, bool useSockets)
            => ToeOf(model, lod, chain, tip, useSockets, out _);

        /// <summary>`asAuthored` is the toe before the mesh has had its say, so a caller that finds the corrected
        /// one impossible can fall back to it rather than ship a leg with its toe above its hip.</summary>
        static Vector3 ToeOf(TankModel model, Lod lod, List<int> chain, int tip, bool useSockets, out Vector3 asAuthored)
        {
            asAuthored = Vector3.zero;
            var tipMesh = lod.Parts[tip].Mesh;
            float floor = 0f; bool haveFloor = false;
            if (tipMesh != null)
            {
                var vs = tipMesh.vertices;
                if (vs.Length > 0)
                {
                    floor = vs[0].y;
                    for (int i = 1; i < vs.Length; i++) if (vs[i].y < floor) floor = vs[i].y;
                    haveFloor = true;
                }
            }
            if (useSockets)
                foreach (var kv in model.Sockets)
                {
                    if (!kv.Key.StartsWith("Socket_Toe_")) continue;
                    if (kv.Value.part != tip) continue;
                    // The socket says WHERE on the foot the machine bears its weight, and that is the artist's to
                    // decide. It does not get to say how HIGH that point is, because the mesh already answers
                    // that and the two disagree badly in the shipped assets: every one of Pincer's six toe
                    // sockets hangs 0.59 m BELOW its own leg mesh, so planting it left the whole machine floating
                    // 0.64 m off the ground, and Kettle's front sockets sit 0.96 m ABOVE its lowest geometry, so
                    // planting those drove the legs a metre under. Keep the socket's x/z, take y from the mesh.
                    var at = kv.Value.local;
                    asAuthored = at;
                    return haveFloor ? new Vector3(at.x, floor, at.z) : at;
                }
            if (haveFloor)
            {
                // The LOWEST VERTEX, not the bottom-face centre of the bounding box. The box's bottom centre is a
                // synthetic point that need not lie on the mesh at all: on a limb modelled diagonally the lowest
                // vertices sit at the box's x/z extremes, so its bottom centre hangs in open air below the
                // geometry, and the gait plants THAT. A vertex the mesh actually has cannot be in the wrong place.
                var vs = tipMesh.vertices;
                var low = vs[0];
                for (int i = 1; i < vs.Length; i++) if (vs[i].y < low.y) low = vs[i];
                asAuthored = low;
                return low;
            }
            var bb = tipMesh.bounds;                   // no CPU copy to read: the old estimate, better than nothing
            asAuthored = new Vector3(bb.center.x, bb.min.y, bb.center.z);
            return asAuthored;
        }

        static void Number(List<Part> parts, int i, int leg, Vector3 at)
        {
            var p = parts[i];
            p.Leg = leg;
            var outward = new Vector3(at.x, 0f, at.z);
            p.Outward = outward.sqrMagnitude > 1e-4f ? outward.normalized : Vector3.right * Mathf.Sign(at.x == 0f ? 1f : at.x);
        }

        static Transform Find(Transform t, string name)
        {
            if (t.name == name) return t;
            for (int i = 0; i < t.childCount; i++) { var f = Find(t.GetChild(i), name); if (f != null) return f; }
            return null;
        }

        /// <summary>The same mesh built larger, made once and kept.
        ///
        /// The alternative was a scale in the root matrix, which costs nothing and would have been wrong: the
        /// gait works in world metres and reads the rig's hip, reach and bone lengths straight out of the model,
        /// so a scale living in a matrix would have had to be remembered at every one of those sites and at
        /// every effect that measures off a part. Fourteen thousand vertices across all seven machines is a
        /// cheaper thing to spend than that. The mesh loaded from Resources is shared and must never be
        /// written to, so this copies. Instancing still batches, because there is one copy per size.</summary>
        static readonly Dictionary<(Mesh, int), Mesh> grown = new Dictionary<(Mesh, int), Mesh>();

        static Mesh Grown(Mesh src, float scale)
        {
            if (src == null || Mathf.Abs(scale - 1f) < 1e-4f) return src;
            var key = (src, Mathf.RoundToInt(scale * 1000f));
            if (grown.TryGetValue(key, out var kept) && kept != null) return kept;
            var m = UnityEngine.Object.Instantiate(src);
            m.name = src.name + "_x" + scale.ToString("0.###");
            var v = m.vertices;
            for (int i = 0; i < v.Length; i++) v[i] *= scale;
            m.vertices = v;
            m.RecalculateBounds();         // a uniform enlargement leaves the normals alone

            // Did it take? On a mesh imported without Read/Write, `v` came back empty, the write above did
            // nothing and threw nothing, and what is returned is a full-sized mesh wearing an enlarged name.
            // The pivots scale regardless, so the machine is drawn with its legs at the new spacing and its
            // body at the old one — it comes apart on screen. That is worth an error rather than a shrug.
            float was = src.bounds.size.magnitude, now = m.bounds.size.magnitude;
            if (was > 1e-4f && Mathf.Abs(now - was * scale) > was * scale * 0.01f)
                Debug.LogError($"TankModel: '{src.name}' would not be enlarged ({now:F2} m across, not {was * scale:F2} m). " +
                    $"Its {v.Length} vertices could not be written, which means the model is imported without " +
                    "Read/Write enabled. The machine will be drawn with its legs at the new size and its body at " +
                    "the old one. Tick Read/Write on the model under Resources/Vehicles and reimport.");

            grown[key] = m;
            return m;
        }

        static void Walk(TankModel model, Lod lod, Transform t, int parent, bool sockets, float scale)
        {
            if (t.name.StartsWith("Socket_"))
            {
                if (sockets && parent >= 0) model.Sockets[t.name] = (parent, t.localPosition * scale);
                return;
            }
            int index = parent;
            var filter = t.GetComponent<MeshFilter>();
            if (filter != null && filter.sharedMesh != null)
            {
                var mesh = Grown(filter.sharedMesh, scale);
                var b = mesh.bounds;
                var p = new Part
                {
                    Name = t.name, Mesh = mesh, Parent = parent, Local = t.localPosition * scale, LocalRot = t.localRotation,
                    Role = RoleOf(t.name), Side = SideOf(t.name),
                    Center = b.center, Radius = b.extents.magnitude,
                };
                if (p.Role == TankPartRole.Gun) p.Gun = p.Side > 0 ? 1 : 0;            // Gun_L is gun 0, Gun_R gun 1
                else if (p.Role == TankPartRole.Sponson) p.Gun = p.Side < 0 ? 0 : 1;   // TankSpec: the Maw's gun 0 is the left sponson
                index = lod.Parts.Count;
                lod.Parts.Add(p);
            }
            for (int i = 0; i < t.childCount; i++) Walk(model, lod, t.GetChild(i), index, sockets, scale);
        }

        /// <summary>Track_L, Wheel_LF, Horn_R, Sponson_R: the letter after the first underscore (left is -X in Unity).</summary>
        static int SideOf(string name)
        {
            int u = name.IndexOf('_');
            if (u < 0 || u + 1 >= name.Length) return 0;
            return name[u + 1] == 'L' ? -1 : name[u + 1] == 'R' ? 1 : 0;
        }

        static TankPartRole RoleOf(string name)
        {
            if (name == "Hull" || name == "Body") return TankPartRole.Hull;
            if (name.StartsWith("Leg")) return TankPartRole.Leg;
            if (name.StartsWith("Thigh")) return TankPartRole.Thigh;
            if (name.StartsWith("Shin")) return TankPartRole.Shin;
            if (name.StartsWith("Foot")) return TankPartRole.Foot;
            if (name.StartsWith("Claw")) return TankPartRole.Claw;
            if (name.StartsWith("Jaw")) return TankPartRole.Jaw;
            if (name == "Shield") return TankPartRole.Shield;
            if (name == "Drum") return TankPartRole.Drum;
            if (name == "Reactor") return TankPartRole.Reactor;
            if (name == "Mortar") return TankPartRole.Gun;          // the piece that is aimed IS the weapon on a Kettle
            if (name.StartsWith("Turret")) return TankPartRole.Turret;
            if (name.StartsWith("Gun")) return TankPartRole.Gun;
            if (name.StartsWith("Track")) return TankPartRole.Track;
            if (name.StartsWith("Wheel")) return TankPartRole.Wheel;
            if (name == "Hatch") return TankPartRole.Hatch;
            if (name.StartsWith("Sponson")) return TankPartRole.Sponson;
            if (name == "Cupola") return TankPartRole.Cupola;
            if (name.StartsWith("Horn")) return TankPartRole.Horn;
            if (name == "Exhaust") return TankPartRole.Exhaust;
            return TankPartRole.Other;
        }
    }
}
