// Phase: Playground (2026-09-26, lane/show/playground) — a multi-LOD skinned figure driven by retargeted clips
// A figure from Tools/frogrig.py: one armature driving every LOD mesh (the LODs are skinned to the same bones, the
// simpler ones to fewer of them), so the LOD can switch at any frame of any clip without a pop in the pose. One
// SkinnedMeshRenderer is enabled at a time. Clips are the game's own, carried over by Retarget from ClipDeck's source rig.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Playground
{
    /// <summary>The game's clips and the rig they are sampled on (shared by every unit in the playground).</summary>
    public sealed class ClipDeck
    {
        public PlaygroundLibrary Library;
        public Retarget.Skeleton Source;
        public float ClipHipHeight = 1f;
        readonly Dictionary<int, (Vector3 start, Vector3 end)> travel = new Dictionary<int, (Vector3, Vector3)>();
        GameObject sourceGo;

        public static ClipDeck Make(PlaygroundLibrary lib, Transform parent)
        {
            if (lib.ClipSource == null) { Debug.LogError("ClipDeck: the library has no ClipSource"); return null; }
            var d = new ClipDeck { Library = lib };
            d.sourceGo = Object.Instantiate(lib.ClipSource, parent);
            d.sourceGo.name = "ClipSource (hidden)";
            d.sourceGo.transform.position = new Vector3(0f, -500f, 0f);
            foreach (var r in d.sourceGo.GetComponentsInChildren<Renderer>(true)) r.enabled = false;
            foreach (var a in d.sourceGo.GetComponentsInChildren<Animator>(true)) a.enabled = false;
            d.Source = Retarget.Skeleton.Of(d.sourceGo.transform);
            int refClip = lib.ClipIndex(lib.ReferenceClip);
            if (refClip >= 0)
            {
                lib.Clips[refClip].Clip.SampleAnimation(d.sourceGo, 0f);
                d.ClipHipHeight = Mathf.Max(0.05f, d.Source.HipsNow.y);
            }
            else d.ClipHipHeight = Mathf.Max(0.05f, d.Source.HipsRest.y);
            return d;
        }

        public void Destroy() { if (sourceGo != null) VehicleRig.Kill(sourceGo); }

        (Vector3 start, Vector3 end) Travel(int clip)
        {
            if (travel.TryGetValue(clip, out var t)) return t;
            var c = Library.Clips[clip].Clip;
            c.SampleAnimation(sourceGo, 0f); var a = Source.HipsNow;
            c.SampleAnimation(sourceGo, c.length); var b = Source.HipsNow;
            t = (new Vector3(a.x, 0f, a.z), new Vector3(b.x, 0f, b.z));
            travel[clip] = t;
            return t;
        }

        /// <summary>Pose the source at a clip's time and put that pose on a unit. Loops wrap, one-shots hold their last frame.</summary>
        public void Pose(UnitRig u, int clip, float time, float fadeWeight, Quaternion[] from)
        {
            var e = Library.Clips[clip]; var c = e.Clip;
            float len = Mathf.Max(0.01f, c.length);
            float t = e.Loop ? Mathf.Repeat(time, len) : Mathf.Min(time, len);
            c.SampleAnimation(sourceGo, t);
            var (a, b) = Travel(clip);
            // a loop walks on the spot: its travel comes off as a straight line, the sway stays. A one-shot keeps its travel
            // from where it started (a death falls where it falls).
            var off = e.Loop ? Vector3.Lerp(a, b, t / len) : a;
            float hip = u.Skel.HipsRest.y / ClipHipHeight;
            Retarget.Apply(Source, u.Skel, hip, off, fadeWeight, from);
        }
    }

    public sealed class UnitRig : MonoBehaviour
    {
        public SkinnedMeshRenderer[] Lods;
        public Retarget.Skeleton Skel;
        public int Clip = -1;
        public float Time01, ClipTime, Speed = 1f;
        public int ForcedLod = -1;
        public int Lod { get; private set; } = -1;
        // screen-height shares for a 2 m figure at the battle's 25 degree lens: LOD0 only up close (under ~22 m), LOD1 to
        // ~63 m, LOD2 (892 vertices, 16 bones) at the standard view's 78 m and out to ~167 m, LOD3 beyond (critic r1:
        // LOD1's 1,924 imported vertices are over the 1,200-1,500 budget for a crowd at the standard view)
        public LodPicker Picker = new LodPicker(0.20f, 0.08f, 0.03f);
        public float Height = 1.78f;
        public float Scorch, Ember, BurnUntil;
        public bool Dead;
        /// <summary>-1 none; 0 or 1: a light wash of the side's colour over the whole figure (the playground has no uniform
        /// mask; the game's VAT figures recolour only the cloth).</summary>
        public int Team = -1;
        public PlaygroundFx Fx;
        public ClipDeck Deck;
        public int VertsDrawn { get; private set; }
        public int[] BonesPerLod { get; private set; }

        Quaternion[] fadeFrom; float fadeStart = -1f, fadeLen = 0.2f;
        // the rifle, exactly as VATBaker gives the baked men one: a 0.06 x 0.10 x 1.15 m box, 0.30 m of it ahead of the
        // grip; from the right hand through the left while both hold it, else in whichever hand still does, by a grip
        // taken in the bind pose (forward, a little across toward the left hand, a little up)
        Transform rifle; Matrix4x4 gripR, gripL; float gunScale = 1f;
        static Material rifleMat; static Mesh cube;
        MaterialPropertyBlock mpb;
        Material[] mats;
        float nextFlame, nextSmoke, nextShot;
        int afterBurn = -1;

        public static UnitRig Build(PlaygroundLibrary.UnitEntry e, ClipDeck deck, PlaygroundFx fx, Transform parent, Vector3 at, float yaw, float scale)
        {
            var root = new GameObject(e.Name);
            root.transform.SetParent(parent, false);
            root.transform.SetPositionAndRotation(at, Quaternion.Euler(0f, yaw, 0f));
            root.transform.localScale = Vector3.one * scale;
            var model = Instantiate(e.Model, root.transform, false);
            model.name = e.Name + " model";
            foreach (var a in model.GetComponentsInChildren<Animator>(true)) a.enabled = false;
            // Unity's FBX importer builds an LODGroup by itself for meshes named *_LOD0..n, and that group, not the
            // renderers' enabled flags, then decides what draws: it hid LOD1-3 whatever we asked. Our picker decides.
            foreach (var g in model.GetComponentsInChildren<LODGroup>(true)) VehicleRig.Kill(g);
            var u = root.AddComponent<UnitRig>();
            u.Deck = deck; u.Fx = fx;
            // face the rig's +Z whatever way the FBX came in: measure the skeleton's own forward and turn the model to it
            var probe = Retarget.Skeleton.Of(model.transform);
            var f = probe.Forward; f.y = 0f;
            if (f.sqrMagnitude > 1e-4f) model.transform.rotation = Quaternion.FromToRotation(f.normalized, root.transform.forward) * model.transform.rotation;
            u.Skel = Retarget.Skeleton.Of(root.transform);
            var smrs = new List<SkinnedMeshRenderer>(model.GetComponentsInChildren<SkinnedMeshRenderer>(true));
            smrs.Sort((x, y) => string.CompareOrdinal(x.name, y.name));
            u.Lods = smrs.ToArray();
            // height as drawn, in the root's frame (the meshes are stored Z-up under a turned node)
            u.Height = 0f;
            if (u.Lods.Length > 0 && u.Lods[0].sharedMesh != null)
            {
                var m = root.transform.worldToLocalMatrix * u.Lods[0].transform.localToWorldMatrix;
                float lo = float.MaxValue, hi = float.MinValue;
                foreach (var v in u.Lods[0].sharedMesh.vertices) { float y = m.MultiplyPoint3x4(v).y; lo = Mathf.Min(lo, y); hi = Mathf.Max(hi, y); }
                u.Height = hi - lo;
            }
            var shader = Shader.Find("TW/Tank (URP)");
            u.mats = new Material[u.Lods.Length];
            u.BonesPerLod = new int[u.Lods.Length];
            for (int k = 0; k < u.Lods.Length; k++)
            {
                var m = new Material(shader) { name = e.Name + "_LOD" + k, enableInstancing = true };
                var tex = e.Atlas != null && e.Atlas.Length > 0 ? e.Atlas[Mathf.Min(k, e.Atlas.Length - 1)] : null;
                bool painted = u.Lods[k].sharedMesh != null && u.Lods[k].sharedMesh.uv.Length == 0;   // colour in its vertices
                m.SetTexture("_BaseMap", painted ? Texture2D.whiteTexture : tex);
                m.SetFloat("_OutlineWidth", 1.6f);
                u.mats[k] = m;
                var r = u.Lods[k];
                r.sharedMaterial = m;
                r.quality = k <= 1 ? SkinQuality.Bone4 : SkinQuality.Bone2;
                r.updateWhenOffscreen = true;
                r.shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On;
                u.BonesPerLod[k] = BonesUsed(r);
            }
            u.mpb = new MaterialPropertyBlock();
            u.MakeRifle();
            u.SetLod(0);
            return u;
        }

        void OnDestroy() { if (mats != null) foreach (var m in mats) if (m != null) VehicleRig.Kill(m); }

        /// <summary>How many bones actually move this LOD (weights above zero), not how many the FBX lists: every LOD of
        /// one armature lists all of them.</summary>
        public static int BonesUsed(SkinnedMeshRenderer r)
        {
            if (r == null || r.sharedMesh == null) return 0;
            var used = new HashSet<int>();
            foreach (var w in r.sharedMesh.GetAllBoneWeights()) if (w.weight > 0.001f) used.Add(w.boneIndex);
            return used.Count;
        }

        void MakeRifle()
        {
            var hr = Skel.Bones[13]; var hl = Skel.Bones[9];
            if (hr == null || hl == null) return;
            gunScale = Height / 1.78f;   // the baker's u: the figure's height over the 1.78 m man the numbers are for
            var fwd = Skel.Forward; var across = (hl.position - hr.position).normalized;
            var dir = (fwd * 0.62f + across * 0.38f + Vector3.up * 0.08f).normalized;
            var look = Quaternion.LookRotation(dir, Vector3.up);
            gripR = hr.worldToLocalMatrix * Matrix4x4.TRS(hr.position, look, Vector3.one);
            gripL = hl.worldToLocalMatrix * Matrix4x4.TRS(hl.position, look, Vector3.one) * Matrix4x4.Translate(new Vector3(0f, 0f, -0.35f * gunScale * transform.lossyScale.y));
            if (cube == null) { var tmp = GameObject.CreatePrimitive(PrimitiveType.Cube); cube = tmp.GetComponent<MeshFilter>().sharedMesh; VehicleRig.Kill(tmp); }
            if (rifleMat == null)
            {
                rifleMat = new Material(Shader.Find("TW/Toon (URP)")) { name = "PG_Rifle", enableInstancing = true };
                rifleMat.SetColor("_BaseColor", new Color(0.30f, 0.23f, 0.16f)); rifleMat.SetFloat("_OutlineWidth", 1.2f);
            }
            rifle = new GameObject("Rifle").transform; rifle.SetParent(transform, false);
            var box = new GameObject("Box"); box.transform.SetParent(rifle, false);
            box.transform.localPosition = new Vector3(0f, 0f, 0.30f * gunScale);
            box.transform.localScale = new Vector3(0.06f, 0.10f, 1.15f) * gunScale;
            box.AddComponent<MeshFilter>().sharedMesh = cube;
            var r = box.AddComponent<MeshRenderer>(); r.sharedMaterial = rifleMat;
        }

        void HoldRifle()
        {
            if (rifle == null) return;
            var hr = Skel.Bones[13]; var hl = Skel.Bones[9];
            float s = transform.lossyScale.y * gunScale;
            var span = hl.position - hr.position;
            Matrix4x4 grip;
            // the right hand is off it (a bolt, a reload, a throw) - but never while aiming: VATBaker's rule is
            // "span > 0.48 && !aiming", and a firing frog's hands are 0.70 apart (the rifle hung from the left hand, 18 deg down)
            bool aimingClip = Clip >= 0 && Deck != null && (Deck.Library.Clips[Clip].Name.StartsWith("Fir") || Deck.Library.Clips[Clip].Name.Contains("Aim"));
            if (span.magnitude > 0.48f * s && !aimingClip) grip = hl.localToWorldMatrix * gripL;
            else if (span.magnitude > 0.08f * s) grip = Matrix4x4.TRS(hr.position, Quaternion.LookRotation(AimFromSource(span), Vector3.up), Vector3.one);
            else grip = hr.localToWorldMatrix * gripR;
            rifle.SetPositionAndRotation(grip.GetColumn(3), grip.rotation);
            // held at hand height, the 1.3 m rifle's butt or muzzle went through the floor: pitch it up about the grip
            // until its lowest corner clears the ground
            float ground = transform.position.y + 0.05f * transform.lossyScale.y;
            for (int k = 0; k < 14 && LowestRifleCorner() < ground; k++)
                rifle.rotation = Quaternion.AngleAxis(-6f * Mathf.Sign(Vector3.Dot(rifle.forward, Vector3.down) + 1e-4f), rifle.right) * rifle.rotation;
            // and out of the body: if much of it is inside the torso, point it forward and down from the grip; failing that
            // carry it across the chest, in front
            if (KeepRifleClear && InsideBody() > 0.25f)
            {
                var f = Skel.Forward; f.y = 0f; f.Normalize();
                // aiming, straight ahead (the barrel through the belly was pushed forward-DOWN, 18 degrees into the ground,
                // for every firing pose); otherwise forward and down, carried
                bool aiming = Clip >= 0 && Deck != null && (Deck.Library.Clips[Clip].Name.StartsWith("Fir") || Deck.Library.Clips[Clip].Name.Contains("Aim"));
                rifle.rotation = Quaternion.LookRotation((f - Vector3.up * (aiming ? 0.03f : 0.35f)).normalized, Vector3.up);
                if (InsideBody() > 0.25f || LowestRifleCorner() < ground)
                {
                    var chest = Vector3.Lerp(Skel.Bones[0].position, Skel.Bones[4].position, 0.6f) + f * 0.28f * transform.lossyScale.y * gunScale;
                    var across = Vector3.Cross(Vector3.up, f);
                    rifle.SetPositionAndRotation(chest - across * 0.25f * transform.lossyScale.y * gunScale, Quaternion.LookRotation((across + Vector3.up * 0.35f).normalized, Vector3.up));
                }
            }
            RifleInside = InsideBody();
        }

        /// <summary>The rifle's pointing direction in the figure's own frame (the same at every LOD by construction: it hangs
        /// on the bones, not on a mesh).</summary>
        public Vector3 RifleDir => rifle != null ? transform.InverseTransformDirection(rifle.forward) : Vector3.zero;

        /// <summary>Share of the rifle's barrel side inside the torso (a capsule from the hips to the neck).</summary>
        public float RifleInside { get; private set; }
        float InsideBody()
        {
            if (rifle == null) return 0f;
            var a = Skel.Bones[0].position; var b = Skel.Bones[4].position;
            float r = 0.30f * transform.lossyScale.y * gunScale, inside = 0f; int counted = 0;
            for (int i = 0; i < 24; i++)
            {
                // the grip and the stock behind it are in the hand and under the arm: only the barrel side counts
                float along = 0.30f - 0.575f + 1.15f * i / 23f;
                if (along < 0.15f) continue;
                counted++;
                var p = rifle.TransformPoint(new Vector3(0f, 0f, along * gunScale));
                var ab = b - a; float t = Mathf.Clamp01(Vector3.Dot(p - a, ab) / ab.sqrMagnitude);
                if ((a + ab * t - p).magnitude < r) inside += 1f;
            }
            return counted > 0 ? inside / counted : 0f;
        }

        /// <summary>The clip's own right-to-left-hand direction, read off the rig it was authored on and turned into this
        /// figure's frame (the source is posed at this figure's clip time: Deck.Pose ran just before). Falls back to the
        /// figure's own hands.</summary>
        Vector3 AimFromSource(Vector3 ownSpan)
        {
            var src = Deck != null ? Deck.Source : null;
            if (src == null || src.Bones[9] == null || src.Bones[13] == null) return ownSpan.normalized;
            var span = src.Bones[9].position - src.Bones[13].position;
            if (span.sqrMagnitude < 1e-8f) return ownSpan.normalized;
            var local = Quaternion.Inverse(src.Root.rotation * src.Frame) * span.normalized;
            // aiming, the barrel is level: the hands are not (the forestock hand is under the barrel line, so right hand to
            // left hand points ~18 degrees down, and every shot's puff sat at knee height, critic r5). Keep a quarter of it.
            if (Clip >= 0 && Deck != null)
            {
                string n = Deck.Library.Clips[Clip].Name;
                if (n.StartsWith("Fir") || n.Contains("Aim")) { local.y *= 0.25f; local.Normalize(); }
            }
            return (transform.rotation * Skel.Frame) * local;
        }

        /// <summary>Keep the rifle out of the body (the tests switch it off to prove RifleInside can read above zero).</summary>
        public bool KeepRifleClear = true;

        /// <summary>Pose this figure at a clip's time and seat its rifle, now (tests; the frame does it in LateUpdate).</summary>
        public void PoseAt(int clip, float time)
        {
            Clip = clip; ClipTime = time;
            Deck.Pose(this, clip, time, 1f, null);
            HoldRifle();
        }

        float LowestRifleCorner()
        {
            float lo = float.MaxValue;
            for (int k = 0; k < 8; k++)
            {
                var c = new Vector3((k & 1) == 0 ? -0.03f : 0.03f, (k & 2) == 0 ? -0.05f : 0.05f, 0.30f + ((k & 4) == 0 ? -0.575f : 0.575f)) * gunScale;
                lo = Mathf.Min(lo, rifle.TransformPoint(c).y);
            }
            return lo;
        }

        // a dead man lets go of it: it falls on the Tumble a vehicle's parts use, in the figure's frame
        Tumble dropped; bool dropping;
        void DropRifle()
        {
            if (rifle == null || dropping) return;
            dropping = true;
            var local = transform.InverseTransformPoint(rifle.position);
            var rot = Quaternion.Inverse(transform.rotation) * rifle.rotation;
            dropped = new Tumble { Pos = local, Rot = rot, Vel = new Vector3(Random.Range(-0.6f, 0.6f), 0.5f, Random.Range(-0.6f, 0.6f)), Spin = Random.onUnitSphere * 3f, Nudge = new Vector3(1f, 0f, 0.3f) };
        }

        void FallRifle(float dt)
        {
            var box = new Bounds(new Vector3(0f, 0f, 0.30f * gunScale), new Vector3(0.06f, 0.10f, 1.15f) * gunScale);
            dropped.Step(box, transform.lossyScale.y, Mathf.Min(dt, 0.05f));
            rifle.position = transform.TransformPoint(dropped.Pos); rifle.rotation = transform.rotation * dropped.Rot;
        }

        static readonly Dictionary<Mesh, (int[] hand, int[] foot)> dominant = new Dictionary<Mesh, (int[], int[])>();
        Mesh baked;
        public bool Drift(out Vector3 hand, out Vector3 foot)
        {
            hand = foot = Vector3.zero;
            var r = Lods[Lod]; var m = r.sharedMesh; if (m == null) return false;
            if (!dominant.TryGetValue(m, out var d))
            {
                var bpv = m.GetBonesPerVertex(); var w = m.GetAllBoneWeights();
                var hs = new List<int>(); var fs = new List<int>(); int at = 0;
                for (int i = 0; i < bpv.Length; i++)
                {
                    int best = -1; float bw = 0f;
                    for (int j = 0; j < bpv[i]; j++) if (w[at + j].weight > bw) { bw = w[at + j].weight; best = w[at + j].boneIndex; }
                    at += bpv[i];
                    if (best < 0) continue;
                    string n = r.bones[best].name;
                    if (n.EndsWith("RightHand") || n.EndsWith("RightForeArm")) hs.Add(i);
                    if (n.EndsWith("LeftFoot") || n.EndsWith("LeftLeg")) fs.Add(i);
                }
                d = (hs.ToArray(), fs.ToArray()); dominant[m] = d;
            }
            if (baked == null) baked = new Mesh();
            r.BakeMesh(baked, true);
            var v = baked.vertices; var rest = m.vertices;
            var toRoot = transform.worldToLocalMatrix * r.transform.localToWorldMatrix;
            // how far the skin has carried them from where they stand in the bind pose: a DISPLACEMENT, so a coarse LOD
            // whose hand vertices simply sit elsewhere on the hand (fewer of them, spread differently) does not read as
            // drift; only bending differently does
            Vector3 Moved(int[] idx) { var s = Vector3.zero; foreach (int i in idx) s += toRoot.MultiplyPoint3x4(v[i]) - toRoot.MultiplyPoint3x4(rest[i]); return idx.Length > 0 ? s / idx.Length : Vector3.zero; }
            hand = Moved(d.hand) * transform.lossyScale.y; foot = Moved(d.foot) * transform.lossyScale.y;
            return true;
        }

        /// <summary>Draw this LOD for one render (LOD-pop measurement): the renderers only, not the picker's state.</summary>
        public void SetLodSilently(int lod) { for (int k = 0; k < Lods.Length; k++) Lods[k].enabled = k == lod; Lods[lod].SetPropertyBlock(mpb); }

        public void SetLod(int lod)
        {
            lod = Mathf.Clamp(lod, 0, Lods.Length - 1);
            if (lod == Lod) return;
            Lod = lod;
            for (int k = 0; k < Lods.Length; k++) Lods[k].enabled = k == lod;
            VertsDrawn = Lods[lod].sharedMesh != null ? Lods[lod].sharedMesh.vertexCount : 0;
        }

        public Vector3 Centre => transform.position + Vector3.up * (0.5f * Height * transform.lossyScale.y);

        public void Play(int clip, float fade = 0.2f, float startTime = 0f)
        {
            if (clip < 0 || Deck == null || clip >= Deck.Library.Clips.Length) return;
            if (Clip >= 0 && fade > 0f)
            {
                fadeFrom ??= new Quaternion[Retarget.Names.Length];
                for (int i = 0; i < Retarget.Names.Length; i++) if (Skel.Bones[i] != null) fadeFrom[i] = Skel.Bones[i].localRotation;
                fadeStart = Time.time; fadeLen = fade;
            }
            Clip = clip; ClipTime = startTime;
        }

        public void Kill(int deathClip)
        {
            if (Dead) return;
            Dead = true; Play(deathClip, 0.12f);
            DropRifle();
        }

        /// <summary>Set alight: runs burning, then drops, charred.</summary>
        public void Ignite(int burningClip, int deathClip, float seconds = 5f)
        {
            if (Dead) return;
            BurnUntil = Time.time + seconds; afterBurn = deathClip;
            if (burningClip >= 0) Play(burningClip, 0.15f);
        }

        public void Revive(int idle)
        {
            Dead = false; Scorch = Ember = 0f; BurnUntil = 0f; afterBurn = -1; dropping = false; Play(idle, 0.2f, Phase);
        }

        /// <summary>Where in its clips this figure is, against the others: kept through a change of clip so a squad does not
        /// march in lockstep.</summary>
        public float Phase;

        void LateUpdate()
        {
            float dt = Time.deltaTime;
            if (Deck != null && Clip >= 0)
            {
                ClipTime += dt * Speed;
                float w = fadeStart >= 0f ? Mathf.Clamp01((Time.time - fadeStart) / fadeLen) : 1f;
                if (w >= 1f) fadeStart = -1f;
                Deck.Pose(this, Clip, ClipTime, w, fadeFrom);
            }
            if (dropping) FallRifle(dt); else HoldRifle();
            if (!Dead && rifle != null && Fx != null && Clip >= 0 && Deck.Library.Clips[Clip].Name.StartsWith("Fir") && Time.time >= nextShot)
            {
                nextShot = Time.time + Random.Range(0.7f, 1.1f);
                Fx.RifleShot(rifle.TransformPoint(new Vector3(0f, 0f, 0.875f * gunScale)), rifle.forward);
            }
            // fire on a man: the standing flame wraps him, soot takes him, and when it is over he drops
            if (BurnUntil > Time.time)
            {
                Scorch = Mathf.Min(1f, Scorch + dt * 0.25f); Ember = 1f;
                if (Fx != null && Time.time >= nextFlame)
                {
                    nextFlame = Time.time + Random.Range(0.14f, 0.22f);
                    float s = transform.lossyScale.y;
                    var body = Skel.Bones[4] != null ? Skel.Bones[4].position : transform.position;   // under the NECK: a lean carries the chest ahead of the spine
                    var toCam = Camera.main != null ? Camera.main.transform.position - body : Vector3.back; toCam.y = 0f;
                    var foot = new Vector3(body.x, transform.position.y, body.z) + toCam.normalized * 0.35f * s;
                    Fx.Flame(foot, 1.1f * s, Height * 1.3f * s, Random.Range(0.5f, 0.7f), 0.95f);
                    if (Skel.Bones[5] != null)
                        Fx.Flame(Skel.Bones[5].position - Vector3.up * 0.35f * s + toCam.normalized * 0.25f * s, 0.6f * s, 0.9f * s, Random.Range(0.35f, 0.5f), 0.95f);
                }
            }
            else if (afterBurn >= 0)
            {
                Scorch = 1f; Kill(afterBurn); afterBurn = -1;
            }
            else Ember = Mathf.Max(0f, Ember - dt * (Dead ? 0.35f : 0.08f));
            if (Dead && Ember > 0.05f && Fx != null && Time.time >= nextSmoke)
            {
                nextSmoke = Time.time + 0.6f;
                Fx.Smoke(transform.position + Vector3.up * 0.4f, 0.9f, 0.9f, 0.3f * Ember, 4f);
            }
            if (ForcedLod >= 0) SetLod(ForcedLod);
            else SetLod(Picker.Pick(LodPicker.ScreenShare(Camera.main, Centre, 0.5f * Height * transform.lossyScale.y)));
            mpb.SetVector("_Damage", new Vector4(Scorch, Ember * 0.7f, 0f, 0f));
            mpb.SetVector("_Tint", new Vector4(1f, 1f, 1f, 0f));
            var tc = Team == 1 ? TW.Presentation.Tactical.TankRenderer.TeamB : TW.Presentation.Tactical.TankRenderer.TeamA;
            mpb.SetVector("_Team", Team >= 0 ? new Vector4(tc.r, tc.g, tc.b, Dead ? 0.05f : 0.16f) : Vector4.zero);
            Lods[Lod].SetPropertyBlock(mpb);
        }
    }
}
