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
        // screen-height shares for a 2 m figure at the battle's 25 degree lens: LOD0 only up close (under ~22 m), LOD1 the
        // standard view's figure (to ~100 m, the full 1,200-1,500 vertex budget), LOD2 the far figure (to ~225 m),
        // LOD3 beyond
        public LodPicker Picker = new LodPicker(0.20f, 0.045f, 0.02f);
        public float Height = 1.78f;
        public float Scorch, Ember, BurnUntil;
        public bool Dead;
        public PlaygroundFx Fx;
        public ClipDeck Deck;
        public int VertsDrawn { get; private set; }
        public int[] BonesPerLod { get; private set; }

        Quaternion[] fadeFrom; float fadeStart = -1f, fadeLen = 0.2f;
        MaterialPropertyBlock mpb;
        Material[] mats;
        float nextFlame, nextSmoke;
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
                if (tex != null) m.SetTexture("_BaseMap", tex);
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
            Dead = false; Scorch = Ember = 0f; BurnUntil = 0f; afterBurn = -1; Play(idle, 0.2f);
        }

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
            // fire on a man: the standing flame wraps him, soot takes him, and when it is over he drops
            if (BurnUntil > Time.time)
            {
                Scorch = Mathf.Min(1f, Scorch + dt * 0.25f); Ember = 1f;
                if (Fx != null && Time.time >= nextFlame)
                {
                    nextFlame = Time.time + Random.Range(0.14f, 0.22f);
                    float s = transform.lossyScale.y;
                    Fx.Flame(transform.position, 1.1f * s, Height * 1.3f * s, Random.Range(0.5f, 0.7f), 0.95f);
                }
            }
            else if (afterBurn >= 0)
            {
                Scorch = 1f; Kill(afterBurn); afterBurn = -1;
            }
            else Ember = Mathf.Max(0f, Ember - dt * 0.08f);
            if (Dead && Ember > 0.05f && Fx != null && Time.time >= nextSmoke)
            {
                nextSmoke = Time.time + 0.6f;
                Fx.Smoke(transform.position + Vector3.up * 0.4f, 0.9f, 0.9f, 0.3f * Ember, 4f);
            }
            if (ForcedLod >= 0) SetLod(ForcedLod);
            else SetLod(Picker.Pick(LodPicker.ScreenShare(Camera.main, Centre, 0.5f * Height * transform.lossyScale.y)));
            mpb.SetVector("_Damage", new Vector4(Scorch, Ember * 0.7f, 0f, 0f));
            mpb.SetVector("_Tint", new Vector4(1f, 1f, 1f, 0f));
            mpb.SetVector("_Team", Vector4.zero);
            Lods[Lod].SetPropertyBlock(mpb);
        }
    }
}
