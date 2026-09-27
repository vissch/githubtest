// Phase: Playground (2026-09-26, lane/show/playground) — a destructible vehicle whose parts are the same at every LOD
// A destructible vehicle built from a Tools/tank3split.py manifest. The same named parts exist at every LOD, and each
// part is ONE transform whose mesh (and atlas: each LOD has its own UV layout) is swapped when the LOD changes. So a
// turret in the air keeps flying when the LOD switches, and three copies forced to three LODs, given the same seed and
// the same hits, fall apart identically: everything that moves a part (impulses, spin, ground contact) reads the part's
// LOD0 box and the shared pivots, never the mesh being drawn.
//
// Damage model (a playground stand-in for the sim's VehicleModulesSystem, which decides the real thing):
//   Intact -> Damaged (hull under half: smoke from the deck) -> Immobilised (a track is off) -> KnockedOut (hull at
//   zero: the gun droops, the deck catches fire, soot creeps over the paint) -> CookedOff (after CookDelay, or at once
//   on an overkill: the turret goes up, the casemate plates blow out, the fire takes the whole hull, then smoulders).
// Parts come off by tier: 1 accessories (antenna, lamps, stack), 2 running gear and rear gun, 3 turret and main gun
// (only at the cook-off), 4 casemate plates (only at the cook-off). The hull never leaves.
using System.Collections.Generic;
using TW.Presentation.Tactical;
using UnityEngine;

namespace TW.Playground
{
    public sealed class VehicleRig : MonoBehaviour
    {
        public enum Stage { Intact, Damaged, Immobilised, KnockedOut, CookedOff }
        public enum HitKind { AP, HE }

        public sealed class Part
        {
            public string Name; public int Index, Parent, Tier; public float Mass;
            public Transform T; public MeshFilter F; public MeshRenderer R;
            public Mesh[] Lods; public Bounds Box;          // LOD0 bounds in the part's frame: physics reads this at every LOD
            public Vector3 RestLocal; public Quaternion RestRot = Quaternion.identity;
            public float Hp, MaxHp;
            public bool Loose;
            public Tumble Fly;                              // a loose part's flight, in the vehicle's own frame
            public float Scorch, Ember, Flash, Recoil, Droop;
            public float BurnUntil, NextFlame;
        }

        public const float Step = 1f / 120f;
        public PlaygroundFx Fx;
        public VehicleManifest Manifest;
        public readonly List<Part> Parts = new List<Part>();
        public Stage State { get; private set; }
        public float Hp = 100f, MaxHp = 100f;
        public int ForcedLod = -1;
        public WalkerDrive Walker;                      // a machine on legs (tank3.json "walker"): walks it after the pose
        public FlyerDrive Flyer;                        // a flying machine (tank3.json "flyer"): flies its hull after the pose
        public int Lod { get; private set; } = -1;
        public int LodCount { get; private set; }
        // screen-height shares of the bounding sphere (8 m for this tank at 1.7x): LOD0 (7.8k tris, over the 3-5k vehicle
        // budget) for close-ups only (under ~45 m), LOD1 at the standard view's 78 m, LOD2 beyond ~145 m
        public LodPicker Picker = new LodPicker(0.70f, 0.22f);
        public float Size = 1f;
        public int Seed = 1;
        public float CookDelay = 7f;                    // < 0: never cooks off, burns out to a wreck
        public float GroundY;
        public bool Traverse;
        public float FireLevel { get; private set; }
        /// <summary>-1 none; 0 or 1: the battle's side colours (TankRenderer.TeamA/B). The game paints a tank's horns in it;
        /// this one has none, so its lamps and antenna wear it, and side 1 gets the field-grey tint over the olive.</summary>
        public int Team = -1;
        public Color[] LodTints { get; private set; }
        /// <summary>Switch the per-LOD colour match on or off (to measure what it does).</summary>
        /// <summary>Put these per-LOD tints on (a fit from the render).</summary>
        public void ApplyLodTints(Color[] t) { LodTints = (Color[])t.Clone(); UseLodTints(true); }
        public void UseLodTints(bool on) { if (LodTints != null) for (int k = 0; k < mats.Length && k < LodTints.Length; k++) mats[k].SetColor("_BaseColor", on ? LodTints[k] : Color.white); }
        public string LastEvent = "";
        public int TrisDrawn { get; private set; }
        public float Radius { get; private set; } = 4f;
        Bounds footprint = new Bounds(Vector3.zero, Vector3.one);   // every part at build, in the vehicle's frame: where its ring goes

        readonly Dictionary<string, (int part, Vector3 local)> sockets = new Dictionary<string, (int, Vector3)>();
        Material[] mats;
        MaterialPropertyBlock mpb;
        System.Random rng;
        Transform loose;
        float acc, cookAt = -1f, knockedAt, cookedAt, nextSmoke, nextFlame, turretYaw, turretTarget;
        Light fireLight;

        // ------------------------------------------------------------------------------------------------ building
        public static VehicleRig Build(PlaygroundLibrary.VehicleEntry e, PlaygroundFx fx, Transform parent, Vector3 at, float yaw, float size, int seed)
        {
            var root = new GameObject(e.Name + "#" + seed);
            root.transform.SetParent(parent, false);
            root.transform.SetPositionAndRotation(at, Quaternion.Euler(0f, yaw, 0f));
            root.transform.localScale = Vector3.one * size;
            var rig = root.AddComponent<VehicleRig>();
            rig.Fx = fx; rig.Size = size; rig.Seed = seed; rig.GroundY = at.y;
            rig.Manifest = VehicleManifest.Parse(e.Manifest);
            rig.rng = new System.Random(seed);
            // parts that come off hang here, in the vehicle's own frame: they fly in local units, so where the vehicle
            // stands in the world never enters their arithmetic (Tumble)
            var lz = new GameObject("loose");
            lz.transform.SetParent(root.transform, false);
            rig.loose = lz.transform;
            rig.LodCount = e.Lods.Length;
            var shader = Shader.Find("TW/Tank (URP)");
            rig.mats = new Material[e.Lods.Length];
            for (int k = 0; k < e.Lods.Length; k++)
            {
                var m = new Material(shader) { name = e.Name + "_LOD" + k, enableInstancing = true };
                var tex = e.Atlas != null && e.Atlas.Length > 0 ? e.Atlas[Mathf.Min(k, e.Atlas.Length - 1)] : null;
                if (tex != null) m.SetTexture("_BaseMap", tex);
                rig.mats[k] = m;
            }
            rig.mpb = new MaterialPropertyBlock();
            var defs = rig.Manifest.partList;
            var byName = new Dictionary<string, Part>();
            for (int i = 0; i < defs.Length; i++)
            {
                var d = defs[i];
                var p = new Part { Name = d.name, Index = i, Tier = d.tier, Mass = d.mass, Lods = new Mesh[e.Lods.Length] };
                for (int k = 0; k < e.Lods.Length; k++)
                {
                    var t = FindDeep(e.Lods[k].transform, d.name);
                    var mf = t != null ? t.GetComponent<MeshFilter>() : null;
                    p.Lods[k] = mf != null ? mf.sharedMesh : null;
                    if (p.Lods[k] == null) Debug.LogError($"VehicleRig {e.Name}: part {d.name} has no mesh at LOD{k}");
                }
                p.Box = p.Lods[0] != null ? p.Lods[0].bounds : new Bounds(Vector3.zero, Vector3.one);
                p.MaxHp = p.Hp = d.tier switch { 1 => 10f, 2 => 40f, 3 => 90f, 4 => 120f, _ => 9999f };
                var go = new GameObject(d.name);
                p.T = go.transform; p.F = go.AddComponent<MeshFilter>(); p.R = go.AddComponent<MeshRenderer>();
                p.R.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On;
                rig.Parts.Add(p); byName[d.name] = p;
            }
            // hierarchy: a part hangs from its parent's transform at its pivot, relative to the parent's pivot
            for (int i = 0; i < defs.Length; i++)
            {
                var p = rig.Parts[i]; var d = defs[i];
                Part up = !string.IsNullOrEmpty(d.parent) && byName.TryGetValue(d.parent, out var q) ? q : null;
                p.Parent = up != null ? up.Index : -1;
                Vector3 pivot = VehicleManifest.V(d.pivot), upPivot = up != null ? VehicleManifest.V(defs[up.Index].pivot) : Vector3.zero;
                p.T.SetParent(up != null ? up.T : root.transform, false);
                p.T.localPosition = pivot - upPivot; p.T.localRotation = Quaternion.identity;
                p.RestLocal = p.T.localPosition; p.RestRot = p.T.localRotation;
            }
            foreach (var s in rig.Manifest.socketList)
                if (byName.TryGetValue(s.part, out var sp)) rig.sockets[s.name] = (sp.Index, VehicleManifest.V(s.pos));
            // the extent of the thing, for the LOD pick
            var hull = rig.Find("Hull");
            if (hull != null)
            {
                var b = new Bounds(Vector3.zero, Vector3.zero);
                foreach (var p in rig.Parts) { var c = p.T.localPosition + p.Box.center; b.Encapsulate(new Bounds(c, p.Box.size)); }
                rig.Radius = b.extents.magnitude * size;
                rig.footprint = b;
            }
            // facing check: a gun's barrel runs forward (+Z) of its trunnion
            var gun = rig.Find("Gun");
            if (gun != null && gun.Box.center.z < 0f) Debug.LogWarning($"VehicleRig {e.Name}: the Gun mesh lies BEHIND its pivot - the parts are turned 180 degrees");
            // each LOD's colour matched to LOD0's (LodTint) - measured, then left OFF for vehicles: on the Brute it moved
            // the rendered mean colour at 1->2 from 0.7 to 3.8 (/255; the mesh mean counts the undersides and track caps
            // the camera never sees). `lodtint 1` puts it on. A fit on the render (`lodfit`, LodTint.Fitted) goes on instead.
            var means = new Color[e.Lods.Length];
            for (int k = 0; k < e.Lods.Length; k++)
            {
                var ms = new List<Mesh>(); foreach (var p in rig.Parts) ms.Add(p.Lods[Mathf.Min(k, p.Lods.Length - 1)]);
                means[k] = LodTint.MeanColour(ms, e.Atlas != null && e.Atlas.Length > 0 ? e.Atlas[Mathf.Min(k, e.Atlas.Length - 1)] : null);
            }
            rig.LodTints = new Color[e.Lods.Length];
            for (int k = 0; k < e.Lods.Length; k++) rig.LodTints[k] = LodTint.Match(means[0], means[k]);
            if (LodTint.Fitted.TryGetValue(e.Name, out var fit) && fit.Length == e.Lods.Length) rig.ApplyLodTints(fit);
            rig.SetLod(e.Lods.Length - 1);
            rig.SetLod(0);
            if (rig.Manifest.walker) rig.Walker = root.AddComponent<WalkerDrive>().Init(rig);
            if (rig.Manifest.flyer) rig.Flyer = root.AddComponent<FlyerDrive>().Init(rig);
            return rig;
        }

        static Transform FindDeep(Transform t, string name)
        {
            if (t.name == name) return t;
            for (int i = 0; i < t.childCount; i++) { var r = FindDeep(t.GetChild(i), name); if (r != null) return r; }
            return null;
        }

        public Part Find(string name) { foreach (var p in Parts) if (p.Name == name) return p; return null; }

        /// <summary>The first part of a destruction tier, in the manifest's order (null if none): what a script aimed at a
        /// tank's plate or track hits on a machine that has neither.</summary>
        public Part FirstOfTier(int tier) { foreach (var p in Parts) if (p.Tier == tier) return p; return null; }

        public Vector3 Socket(string name)
        {
            if (sockets.TryGetValue(name, out var s)) return Parts[s.part].T.TransformPoint(s.local);
            return transform.position + Vector3.up * 2f * Size;
        }

        /// <summary>The middle of the machine in the world. A walker's or a flyer's is its hull's, wherever that has walked or
        /// flown to (the rig's root stays where it was built: a report put the circling gunship in the frame's corner while
        /// it was in the middle, loop 2 r32; and its LOD was picked by the ground under it).</summary>
        public Vector3 Centre
        {
            get
            {
                if (Walker != null || Flyer != null)
                {
                    var h = Find("Hull");
                    if (h != null && !h.Loose) return h.T.TransformPoint(h.Box.center);
                }
                return transform.TransformPoint(new Vector3(0f, 0.35f * Radius / Size, 0f));
            }
        }

        /// <summary>A part's pose in the vehicle's frame, from local values only (pivots, the part's own local turn, a loose
        /// part's flight): never from a world matrix, so copies anywhere in the world agree to the bit.</summary>
        public void LocalPose(Part p, out Vector3 pos, out Quaternion rot)
        {
            if (p.Loose) { pos = p.Fly.Pos; rot = p.Fly.Rot; return; }
            if (p.Parent < 0) { pos = p.T.localPosition; rot = p.T.localRotation; return; }
            LocalPose(Parts[p.Parent], out var pp, out var pr);
            pos = pp + pr * p.T.localPosition; rot = pr * p.T.localRotation;
        }

        public Vector3 LocalCentre(Part p) { LocalPose(p, out var pos, out var rot); return pos + rot * p.Box.center; }

        public Vector3 SocketLocal(string name)
        {
            if (!sockets.TryGetValue(name, out var s)) return new Vector3(0f, 2f, 0f);
            LocalPose(Parts[s.part], out var pos, out var rot);
            return pos + rot * s.local;
        }

        void OnDestroy()
        {
            if (loose != null) Kill(loose.gameObject);
            if (mats != null) foreach (var m in mats) if (m != null) Kill(m);
            if (fireLight != null) Kill(fireLight.gameObject);
        }

        public static void Kill(Object o) { if (Application.isPlaying) Destroy(o); else DestroyImmediate(o); }

        // ------------------------------------------------------------------------------------------------ LOD
        public void SetLod(int lod)
        {
            lod = Mathf.Clamp(lod, 0, LodCount - 1);
            if (lod == Lod) return;
            Lod = lod; TrisDrawn = 0;
            foreach (var p in Parts)
            {
                var m = p.Lods[Mathf.Min(lod, p.Lods.Length - 1)];
                p.F.sharedMesh = m; p.R.sharedMaterial = mats[lod];
                if (m != null) TrisDrawn += (int)(m.GetIndexCount(0) / 3);
            }
        }

        void PickLod()
        {
            if (ForcedLod >= 0) { SetLod(ForcedLod); return; }
            SetLod(Picker.Pick(LodPicker.ScreenShare(Camera.main, Centre, Radius)));
        }

        // ------------------------------------------------------------------------------------------------ damage
        float R01() => (float)rng.NextDouble();
        float R(float a, float b) => a + (b - a) * R01();
        static float V(float a, float b) => Random.Range(a, b);   // looks only: never the physics stream, or copies at different distances drift apart
        Vector3 RSphere() { float z = R(-1f, 1f), a = R(0f, 6.2832f), r = Mathf.Sqrt(1f - z * z); return new Vector3(r * Mathf.Cos(a), z, r * Mathf.Sin(a)); }

        /// <summary>Distance in metres from a point in the vehicle's frame to a part's box.</summary>
        float DistanceTo(Part p, Vector3 local)
        {
            LocalPose(p, out var pos, out var rot);
            var l = Quaternion.Inverse(rot) * (local - pos);
            var c = p.Box.ClosestPoint(l);
            return Vector3.Distance(pos + rot * c, local) * Size;
        }

        /// <summary>The part nearest a point in the vehicle's frame. Where the point is inside several boxes (the hull's box
        /// holds the tracks), the smallest of them is the one hit: the most particular thing there.</summary>
        public Part Nearest(Vector3 local, bool attachedOnly = true)
        {
            Part best = null; float bd = float.MaxValue, bv = float.MaxValue;
            foreach (var p in Parts)
            {
                if (attachedOnly && p.Loose) continue;
                float d = DistanceTo(p, local), v = p.Box.size.x * p.Box.size.y * p.Box.size.z;
                if (d < bd - 0.05f || (d < bd + 0.05f && v < bv)) { bd = Mathf.Min(bd, d); bv = v; best = p; }
            }
            return best;
        }

        /// <summary>An AP round on a named part (scripted runs, the panel), arriving from the vehicle's front left.</summary>
        public void HitPart(Part p, float damage)
        {
            if (p == null || p.Loose) return;
            var at = LocalCentre(p);
            Fx?.Spark(transform.TransformPoint(at), transform.TransformDirection(new Vector3(1f, -0.2f, -1f).normalized), 0.9f);
            Hurt(p, damage, at - new Vector3(1f, -0.2f, -1f).normalized, 5f);
            LastEvent = $"AP {damage:0} on {p.Name}";
        }

        /// <summary>A round or a shell arriving at a point in the world (converted once to the vehicle's frame).</summary>
        public void Hit(Vector3 point, Vector3 dir, float damage, HitKind kind)
            => HitLocal(transform.InverseTransformPoint(point), transform.InverseTransformDirection(dir).normalized, damage, kind);

        /// <summary>A round or a shell arriving at a point in the vehicle's own frame: AP strikes the part nearest the
        /// point, HE hurts everything in reach. Scripted runs give their hits this way, so copies agree to the bit.</summary>
        public void HitLocal(Vector3 at, Vector3 dir, float damage, HitKind kind)
        {
            var world = transform.TransformPoint(at);
            if (kind == HitKind.AP)
            {
                var p = Nearest(at);
                if (p == null) return;
                Fx?.Spark(world, transform.TransformDirection(dir), 0.9f);
                Hurt(p, damage, at, 5f);
                LastEvent = $"AP {damage:0} on {p.Name}";
            }
            else
            {
                float reach = 3.5f * Size;
                var ground = new Vector3(world.x, GroundY, world.z);
                Fx?.Burst(ground, 1.4f * Mathf.Sqrt(Size));
                LastEvent = $"HE {damage:0} at {new Vector2(at.x, at.z).magnitude * Size:0.0} m";
                float nearest = float.MaxValue;
                foreach (var p in Parts)
                {
                    if (p.Loose) continue;
                    float d = DistanceTo(p, at);
                    if (d > reach) continue;
                    nearest = Mathf.Min(nearest, d);
                    Hurt(p, damage * (1f - d / reach), at, 7f, false);
                }
                if (nearest < reach) HurtHull(damage * (1f - nearest / reach));
                Fx?.Debris?.Burst(DebrisRenderer.Piece.Clod, ground, 10, 7f, 0.35f, new Color(0.35f, 0.29f, 0.22f), 12f, 0f, 1.6f, default, (uint)Seed);
            }
        }

        /// <summary>from: where the harm came from, in the vehicle's frame. throwSpeed: m/s.</summary>
        void Hurt(Part p, float damage, Vector3 from, float throwSpeed, bool countsOnHull = true)
        {
            p.Flash = 1f;
            p.Scorch = Mathf.Min(1f, p.Scorch + 0.3f * Mathf.Clamp01(damage / 30f));
            p.Hp -= damage;
            if (countsOnHull) Hp -= damage * (p.Tier >= 3 || p.Name == "Hull" ? 1f : 0.5f);
            if (p.Hp <= 0f && !p.Loose && p.Tier >= 1 && p.Tier <= 2)
            {
                var c = LocalCentre(p);
                var away = c - from; away.y = 0f;
                if (away.sqrMagnitude < 1e-4f) away = c; away.y = 0f;
                away = away.sqrMagnitude > 1e-4f ? away.normalized : Vector3.right;
                bool track = p.Name.StartsWith("Track");
                // a track does not fly: it is knocked off its wheels and falls over OUTWARD, away from the hull whatever
                // side the round came from (it used to be pushed along the round and ended inside the hull)
                float side = Mathf.Sign(c.x == 0f ? 1f : c.x);
                // anything knocked off flies out from the hull's middle, never through it (the stack, on the right, was
                // thrown by a burst on the right through the hull to land 10 m out on the left)
                var outward = new Vector3(c.x, 0f, c.z); outward = outward.sqrMagnitude > 1e-4f ? outward.normalized : away;
                var v = track ? new Vector3(side * R(2f, 2.8f), 2f, 0f)
                              : outward * throwSpeed * R(0.6f, 1.1f) + Vector3.up * throwSpeed * R(0.5f, 0.9f);
                var spin = track ? Vector3.forward * (-side * 4f) : RSphere() * R(3f, 9f);
                Detach(p, v, spin);
                // a track leaves already leaning out 25 degrees: it topples off its wheels rather than sliding
                if (track) { p.Fly.Rot = Quaternion.AngleAxis(-side * 25f, Vector3.forward) * p.Fly.Rot; p.T.localRotation = p.Fly.Rot; }
                if (track && State < Stage.Immobilised) { State = Stage.Immobilised; LastEvent += " - immobilised"; }
            }
            Stages();
        }

        void HurtHull(float damage) { Hp -= damage; Stages(); }

        void Stages()
        {
            if (Hp <= 0.5f * MaxHp && State == Stage.Intact) State = Stage.Damaged;
            if (Hp <= 0f && State < Stage.KnockedOut) KnockOut();
            // overkill cooks it off, but not in the same breath as the knock-out: a machine killed by one big hit went
            // straight to the cook-off and never showed itself burning (critic loop 2 r30, the Croaker)
            if (Hp <= -MaxHp * 0.6f && State < Stage.CookedOff && State == Stage.KnockedOut && Time.time - knockedAt > 1.5f) CookOff();
        }

        /// <summary>Throw a part off. velocity in m/s and spin in rad/s, both in the vehicle's frame.</summary>
        public void Detach(Part p, Vector3 velocity, Vector3 spin)
        {
            if (p.Loose || p.Name == "Hull") return;
            LocalPose(p, out var pos, out var rot);
            p.Loose = true;
            p.Fly = new Tumble { Pos = pos, Rot = rot, Vel = velocity / Size, Spin = spin, Nudge = new Vector3(R(-1f, 1f), 0f, R(-1f, 1f)) };
            p.T.SetParent(loose, false);
            p.T.localPosition = pos; p.T.localRotation = rot;
            if (FireLevel > 0.2f) p.BurnUntil = Time.time + R(6f, 14f);
            // what a hit tore off carries the hit's scorch (clean parts in a burnt-out wreck drew the eye first, loop 2 r32)
            p.Scorch = Mathf.Max(p.Scorch, 0.35f);
            Fx?.Spark(p.T.TransformPoint(p.Box.center), transform.TransformDirection(velocity.normalized), 0.8f);
        }

        public void KnockOut()
        {
            if (State >= Stage.KnockedOut) return;
            State = Stage.KnockedOut; knockedAt = Time.time; Hp = Mathf.Min(Hp, 0f);
            if (CookDelay >= 0f) cookAt = Time.time + CookDelay;
            var gun = Find("Gun"); if (gun != null) gun.Droop = 9f;
            LastEvent = "knocked out" + (CookDelay >= 0f ? $" - cooks off in {CookDelay:0} s" : " - burns out");
        }

        public void CookOff()
        {
            if (State >= Stage.CookedOff) return;
            if (State < Stage.KnockedOut) KnockOut();
            State = Stage.CookedOff; cookedAt = Time.time; cookAt = -1f; FireLevel = 1f;
            var deckWorld = Socket("Socket_Deck");
            var deck = SocketLocal("Socket_Deck");
            Fx?.CookOff(deckWorld, Size);
            // the turret goes up the ammunition's own column, the plates blow out from the fighting compartment
            foreach (var p in Parts)
            {
                if (p.Loose || p.Name == "Hull") continue;
                var c = LocalCentre(p);
                var out_ = c - deck; out_.y = 0f; out_ = out_.sqrMagnitude > 1e-4f ? out_.normalized : RSphere();
                float s = Mathf.Sqrt(Size);
                Vector3 v; Vector3 spin;
                if (p.Name == "Turret") { v = Vector3.up * R(11f, 14f) * s + out_ * R(0.5f, 1.5f); spin = RSphere() * R(2f, 5f); }
                else if (p.Tier == 4) { v = out_ * R(2.5f, 4f) * s + Vector3.up * R(4f, 7f) * s; spin = Vector3.Cross(Vector3.up, out_) * R(4f, 8f); }
                else if (p.Name == "Gun") { v = Vector3.forward * R(3f, 5f) * s + Vector3.up * R(5f, 7f) * s; spin = Vector3.right * R(3f, 6f); }
                else if (p.Name.StartsWith("Track")) continue;   // the running gear stays on the ground it was on
                else { v = out_ * R(2f, 4.5f) * s + Vector3.up * R(5f, 9f) * s; spin = RSphere() * R(4f, 10f); }
                Detach(p, v, spin);
                p.BurnUntil = Time.time + R(8f, 20f); p.Scorch = Mathf.Max(p.Scorch, 0.5f);
            }
            // what was already lying about goes up with it: near the hull it catches, and everything already off is blackened
            // by the blast (the thrown track stayed clean and blue; loop 2 r32: claws, pods and shrouds shed earlier and
            // lying 10-15 units off stayed clean in every wreck)
            foreach (var p in Parts)
            {
                if (!p.Loose) continue;
                p.Scorch = Mathf.Max(p.Scorch, 0.7f);
                if (Vector3.Distance(p.Fly.Pos, deck) < 12f) p.BurnUntil = Mathf.Max(p.BurnUntil, Time.time + R(8f, 16f));
            }
            Fx?.Debris?.Burst(DebrisRenderer.Piece.Plate, deckWorld, 14, 13f, 0.35f * Size, new Color(0.30f, 0.30f, 0.26f), 60f, 1f, 1.6f, default, (uint)(Seed * 7919));
            LastEvent = "COOKED OFF";
        }

        public void FireGun()
        {
            if (State >= Stage.KnockedOut) return;
            var gun = Find("Gun"); if (gun == null || gun.Loose) return;
            Fx?.Muzzle(Socket("Socket_Muzzle"), gun.T.forward, Size);
            gun.Recoil = 1f;
            LastEvent = "fired";
        }

        public void Repair()
        {
            // put every part back on its pivot, clean
            foreach (var p in Parts)
            {
                if (p.Loose) { p.T.SetParent(p.Parent >= 0 ? Parts[p.Parent].T : transform, false); p.Loose = false; }
                p.T.localPosition = p.RestLocal; p.T.localRotation = p.RestRot; p.T.localScale = Vector3.one;
                p.Hp = p.MaxHp; p.Fly = default; p.Scorch = p.Ember = p.Flash = p.Recoil = p.Droop = 0f; p.BurnUntil = 0f;
            }
            Hp = MaxHp; State = Stage.Intact; FireLevel = 0f; cookAt = -1f; rng = new System.Random(Seed); LastEvent = "repaired";
            Fx?.ClearCards();
            if (fireLight != null) { Destroy(fireLight.gameObject); fireLight = null; }
        }

        // ------------------------------------------------------------------------------------------------ frame
        void Update()
        {
            Advance(Time.deltaTime);
            PickLod();
            Push();
            if (Fx != null && Team >= 0)
            {
                // round the whole intact vehicle (TankRenderer measures the track gauge and pads 0.9 m; the full width
                // and a 0.6 m pad come out the same size), staying where the hull stood when the parts fly
                var mid = transform.TransformPoint(footprint.center); var e = Vector3.Scale(footprint.extents, transform.lossyScale);
                float yaw = transform.eulerAngles.y;
                // a flyer's ring is on the ground under the aircraft, wherever it is flying, not where the rig stands
                var hull = Flyer != null ? Find("Hull") : null;
                if (hull != null && !hull.Loose)
                {
                    mid = hull.T.TransformPoint(transform.InverseTransformPoint(mid) - hull.RestLocal);
                    yaw = hull.T.eulerAngles.y;
                }
                Fx.Ring(new Vector3(mid.x, GroundY, mid.z), yaw, e.x, e.z, 0.6f, 0.6f, Team, State >= Stage.KnockedOut);
            }
        }

        /// <summary>One frame of everything that moves: the fixed-step flight of loose parts, the stage timers, fire, the
        /// pose of the turret and gun. Public so a test can run a vehicle without the player loop.</summary>
        public void Advance(float dt)
        {
            if (dt > 0f)
            {
                acc += Mathf.Min(dt, 0.1f);
                while (acc >= Step) { Integrate(Step); acc -= Step; }
                Timers(dt);
                Burn(dt);
            }
            Pose(dt);
            if (Walker != null) Walker.Drive(dt);
            if (Flyer != null) Flyer.Drive(dt);
        }

        void Timers(float dt)
        {
            if (cookAt > 0f && Time.time >= cookAt) CookOff();
            if (State == Stage.KnockedOut) FireLevel = Mathf.Min(0.75f, FireLevel + dt / 3f);
            else if (State == Stage.CookedOff) FireLevel = 0.3f + 0.7f * Mathf.Exp(-(Time.time - cookedAt) / 8f);   // flares, then burns down to a smoulder
            if (Traverse && State < Stage.KnockedOut)
            {
                if (Mathf.Abs(Mathf.DeltaAngle(turretYaw, turretTarget)) < 1f) turretTarget = R(-70f, 70f);
                turretYaw = Mathf.MoveTowardsAngle(turretYaw, turretTarget, 25f * dt);
            }
        }

        void Pose(float dt)
        {
            foreach (var p in Parts)
            {
                p.Flash = Mathf.Max(0f, p.Flash - dt * 6f);
                if (p.Loose) continue;
                if (p.Name == "Turret") p.T.localRotation = p.RestRot * Quaternion.Euler(0f, turretYaw, 0f);
                if (p.Name == "Gun")
                {
                    p.Recoil = Mathf.Max(0f, p.Recoil - dt * 2.5f);
                    float kick = p.Recoil * p.Recoil * 0.35f;
                    p.T.localPosition = p.RestLocal - Vector3.forward * kick;
                    float droop = State >= Stage.KnockedOut ? Mathf.SmoothStep(0f, p.Droop, (Time.time - knockedAt) / 1.2f) : 0f;
                    p.T.localRotation = p.RestRot * Quaternion.Euler(droop, 0f, 0f);
                }
            }
        }

        /// <summary>Loose parts: Tumble in the vehicle's frame, reading only the LOD0 box, so the flight is the same at every
        /// LOD and wherever the vehicle stands.</summary>
        void Integrate(float h)
        {
            foreach (var p in Parts)
            {
                if (!p.Loose || p.Fly.Resting) continue;
                float impact = p.Fly.Step(p.Box, Size, h);
                p.T.localPosition = p.Fly.Pos; p.T.localRotation = p.Fly.Rot;
                if (impact > 3f && p.Mass > 0.5f && Fx != null && Fx.Debris != null)
                {
                    var w = p.T.TransformPoint(p.Box.center); w.y = GroundY;
                    Fx.Debris.Burst(DebrisRenderer.Piece.Clod, w, 3, 2.5f, 0.25f, new Color(0.35f, 0.29f, 0.22f), 8f, 0f, 1.2f, default, (uint)(p.Index * 31 + Seed));
                }
            }
        }

        /// <summary>Fire and smoke: from the deck sockets while the hull burns, from each loose part that left burning,
        /// smoke from the engine deck once the hull is hurt. Soot creeps over whatever burns.</summary>
        void Burn(float dt)
        {
            float now = Time.time;
            if (Fx == null) return;
            bool near = Camera.main == null || Vector3.Distance(Camera.main.transform.position, Centre) < 400f;
            if (FireLevel > 0.01f)
            {
                if (now >= nextFlame && near)
                {
                    nextFlame = now + Mathf.Lerp(0.34f, 0.12f, FireLevel);
                    string[] feet = State == Stage.CookedOff ? new[] { "Socket_Fire0", "Socket_Fire1", "Socket_Fire2", "Socket_Deck" } : new[] { "Socket_Fire0", "Socket_Fire1" };
                    var foot = Socket(feet[Random.Range(0, feet.Length)]);
                    // after the cook-off the plates the fire sockets were cast onto are gone: the flames stand on what is
                    // left of the hull, not in the air where the plates were (loop 2 r33, v5)
                    var hullPart = Find("Hull");
                    if (State == Stage.CookedOff && hullPart != null) foot.y = Mathf.Min(foot.y, hullPart.T.TransformPoint(new Vector3(0f, hullPart.Box.max.y, 0f)).y);
                    float big = State == Stage.CookedOff ? 1.5f : 1f;
                    Fx.Flame(foot, V(1.2f, 1.9f) * Size * big * FireLevel, V(2.2f, 3.4f) * Size * big * Mathf.Sqrt(FireLevel), V(0.6f, 0.9f));
                }
                if (fireLight == null) fireLight = Fx.Lamp(Socket("Socket_Deck") + Vector3.up * 1.5f * Size, new Color(1f, 0.55f, 0.25f), 8f * Fx.Glow, 14f * Size, 99999f, true, transform);
                else fireLight.intensity = 8f * Fx.Glow * FireLevel * (0.75f + 0.25f * Mathf.PerlinNoise(Seed, now * 7f));
            }
            if ((State >= Stage.Damaged || FireLevel > 0f) && now >= nextSmoke && near)
            {
                bool fire = FireLevel > 0.05f;
                nextSmoke = now + (fire ? Mathf.Lerp(0.5f, 0.2f, FireLevel) : 0.55f);
                var at = Socket(fire ? "Socket_Fire0" : "Socket_Exhaust0") + Vector3.up * (0.6f + 2f * FireLevel) * Size;
                Fx.Smoke(at, (1.4f + 2.6f * FireLevel) * Size, 1.4f + 1.5f * FireLevel, fire ? Mathf.Clamp01(0.45f + FireLevel * 0.45f) : 0.35f, fire ? 7f : 4f);
            }
            foreach (var p in Parts)
            {
                bool burning = p.Loose ? now < p.BurnUntil : FireLevel > 0.05f;
                if (burning) p.Scorch = Mathf.Min(1f, p.Scorch + dt * (State == Stage.CookedOff ? 0.6f : p.Loose ? 0.3f : 0.18f));
                // until the cook-off the fire is on the deck: a part glows by how near it is to it (the whole knocked-out hull
                // glowed lava orange end to end); after it everything that stayed on is in the fire
                float byFire = 1f;
                if (!p.Loose && State < Stage.CookedOff)
                {
                    var mid = LocalCentre(p); float dmin = float.MaxValue;
                    for (int k = 0; k < 3; k++) dmin = Mathf.Min(dmin, Vector3.Distance(mid, SocketLocal("Socket_Fire" + k)));
                    byFire = Mathf.Clamp01(1f - (dmin * Size - 1f) / 2f);
                }
                p.Ember = p.Loose ? (now < p.BurnUntil ? 1f : Mathf.Max(0f, p.Ember - dt * 0.05f)) : FireLevel * byFire;
                if (p.Loose && now < p.BurnUntil && now >= p.NextFlame && near && p.Mass >= 0.5f)
                {
                    p.NextFlame = now + V(0.25f, 0.45f);
                    var c = p.T.TransformPoint(p.Box.center);
                    float s = Mathf.Clamp(p.Box.extents.magnitude * Size, 0.4f, 2.2f);
                    Fx.Flame(new Vector3(c.x, Mathf.Max(GroundY, c.y - p.Box.extents.y * Size * 0.5f), c.z), s * 0.9f, s * 1.3f, V(0.5f, 0.8f), 0.9f);
                }
            }
        }

        void Push()
        {
            foreach (var p in Parts)
            {
                mpb.SetVector("_Damage", new Vector4(p.Scorch, p.Ember * 0.8f, p.Flash, 0f));
                mpb.SetVector("_Tint", Team == 1 ? new Vector4(0.62f, 0.64f, 0.62f, 0.55f) : new Vector4(1f, 1f, 1f, 0f));
                bool wears = Team >= 0 && (p.Name.StartsWith("Lamp") || p.Name == "Antenna");
                var c = Team == 1 ? TankRenderer.TeamB : TankRenderer.TeamA;
                mpb.SetVector("_Team", wears ? new Vector4(c.r, c.g, c.b, State >= Stage.KnockedOut ? 0.5f : 1f) : Vector4.zero);
                p.R.SetPropertyBlock(mpb);
            }
        }

        /// <summary>For the side-by-side LOD proof: where every part is, as one string (positions to the centimetre).</summary>
        public string PoseSignature()
        {
            var sb = new System.Text.StringBuilder();
            foreach (var p in Parts)
            {
                LocalPose(p, out var c, out _); c *= Size;
                sb.Append(p.Name).Append(p.Loose ? "*" : "").Append(':').Append(c.x.ToString("0.00")).Append(',').Append(c.y.ToString("0.00")).Append(',').Append(c.z.ToString("0.00")).Append(' ');
            }
            return sb.ToString();
        }
    }
}
