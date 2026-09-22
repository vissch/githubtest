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

        public sealed class Lod
        {
            public readonly List<Part> Parts = new List<Part>();
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

        /// <summary>The tanks' root part is the Hull; a walker's is its Body (Tools/crabsplit.py).</summary>
        public static TankModel Load(string name, byte archetype, string root = "Hull")
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
                Walk(model, l, hull, -1, lod == 0);
            }
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
            if (hullPart >= 0) model.Height = parts[hullPart].Mesh.bounds.max.y;
            for (int lod = 0; lod < 2; lod++) if (model.Lods[lod] != null && (lod == 0 || model.Lods[1] != model.Lods[0])) NumberLegs(model, model.Lods[lod]);
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
            // a shin and a foot belong to the thigh above them
            for (int i = 0; i < parts.Count; i++)
            {
                if (parts[i].Leg >= 0) continue;
                for (int k = parts[i].Parent; k >= 0; k = parts[k].Parent)
                    if (parts[k].Leg >= 0) { parts[i].Leg = parts[k].Leg; parts[i].Outward = parts[k].Outward; break; }
            }
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

        static void Walk(TankModel model, Lod lod, Transform t, int parent, bool sockets)
        {
            if (t.name.StartsWith("Socket_"))
            {
                if (sockets && parent >= 0) model.Sockets[t.name] = (parent, t.localPosition);
                return;
            }
            int index = parent;
            var filter = t.GetComponent<MeshFilter>();
            if (filter != null && filter.sharedMesh != null)
            {
                var b = filter.sharedMesh.bounds;
                var p = new Part
                {
                    Name = t.name, Mesh = filter.sharedMesh, Parent = parent, Local = t.localPosition, LocalRot = t.localRotation,
                    Role = RoleOf(t.name), Side = SideOf(t.name),
                    Center = b.center, Radius = b.extents.magnitude,
                };
                if (p.Role == TankPartRole.Gun) p.Gun = p.Side > 0 ? 1 : 0;            // Gun_L is gun 0, Gun_R gun 1
                else if (p.Role == TankPartRole.Sponson) p.Gun = p.Side < 0 ? 0 : 1;   // TankSpec: the Maw's gun 0 is the left sponson
                index = lod.Parts.Count;
                lod.Parts.Add(p);
            }
            for (int i = 0; i < t.childCount; i++) Walk(model, lod, t.GetChild(i), index, sockets);
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
