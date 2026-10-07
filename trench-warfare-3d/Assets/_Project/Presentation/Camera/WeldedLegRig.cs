// Phase: Playground (2026-09-28, lane/show/playground) — skinned legs on a hull whose legs are welded to it
// Here, not in the Playground, since 2026-10-07: the battle draws the same legs (TankRenderer.HopLegs.cs), and the
// Playground's assembly reads this one, not the other way round. It was Playground/Runtime/LegRig.cs.
// Tripo welded the Bullfrog's four legs into its hull, so no part can move them. Tools/legrig.py gives each hull LOD's
// vertices bone weights (Blender's bone heat on an armature laid into the legs: thigh, shin, foot; arm, forearm, hand;
// the body's bones hold the torso) and writes <Name>_legs.json; this class skins the rig's own copy of each hull mesh on
// the CPU from a pose (a rotation about each bone's head, in the hull's frame, children after their parents), positions
// and normals, only the LOD on show. HopDrive poses it through a hop and stands the body on the skinned feet.
// Each Unity vertex takes the weights of the welded vertex at its position: Unity split the vertices at UV seams, and
// the weights were made on the welded mesh. If any vertex has no welded vertex within a millimetre the mesh has changed
// since the weights were made (re-split): the legs stay still and a warning says to run legrig.py again.
using System;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed class WeldedLegRig
    {
        [Serializable] sealed class BoneData { public string name, parent; public float[] head, tail; }
        [Serializable] sealed class LodData { public float[] verts; public int[] wi; public float[] ww; }
        [Serializable] sealed class FileData { public float yaw; public float[] cen; public BoneData[] bones; public LodData[] lods; }

        public readonly string[] Names;
        /// <summary>Each bone's rotation about its head, in its parent's posed frame (identity: the sculpt's pose).</summary>
        public readonly Quaternion[] Pose;
        /// <summary>The body's own frame in the hull's: to its right, forward (the sculpt stands turned on its base).</summary>
        public readonly Vector3 Right, Forward;
        readonly Vector3[] head; readonly int[] parent; readonly Matrix4x4[] m; readonly Quaternion[] q;
        readonly Mesh[] mesh; readonly Vector3[][] rest, restN; readonly int[][] wi; readonly float[][] ww;
        Vector3[] buf, bufN; int shown = -1; bool posed, shownPosed;
        readonly float[][] throatW;
        readonly Vector3[][] throatDir;   // the way each vertex of the sac goes out: from the sac's centre, so split vertices go together
        /// <summary>The vocal sac under the chin, 0..1: its vertices pushed out along their normals by ThroatDepth.</summary>
        public float Throat;
        public const float ThroatDepth = 0.2f;    // hull units at a full swell (0.11 hardly showed at the default camera, g15)
        /// <summary>The belly pressed flat on the ground, hull units: its underside (from Floor up BellyBand) pushed up by
        /// this much at the bottom, less higher up, and bulging out. The toad's belly rests 1 cm above its feet, so without
        /// it the body could not sink over its folding legs at all (0.017 m, not the 0.15 asked for, critic g13).</summary>
        public float Squash;
        public float Floor;
        public const float BellyBand = 0.5f;
        readonly float[][] bodyW; readonly float cx, cz;
        /// <summary>The ground, in the hull's frame (HopDrive sets it knocked out): a vertex that follows a leg and would
        /// be below it (dot(GroundN, p) under GroundC) is pressed onto it along GroundDir, so a sprawled or kicking leg lies
        /// on the ground instead of going into it (the body is stood on its belly alone).</summary>
        public bool HasGround;
        public Vector3 GroundN, GroundDir;
        public float GroundC;

        public int Bone(string name) => Array.IndexOf(Names, name);

        WeldedLegRig(FileData f, Mesh[] copies, int[] fileLod, float scale, float[] within)
        {
            int n = f.bones.Length;
            Names = new string[n]; head = new Vector3[n]; parent = new int[n]; Pose = new Quaternion[n]; m = new Matrix4x4[n]; q = new Quaternion[n];
            for (int b = 0; b < n; b++) { Names[b] = f.bones[b].name; head[b] = V(f.bones[b].head, 0) * scale; Pose[b] = Quaternion.identity; }
            for (int b = 0; b < n; b++) parent[b] = string.IsNullOrEmpty(f.bones[b].parent) ? -1 : Bone(f.bones[b].parent);
            float yaw = f.yaw * Mathf.Deg2Rad;
            Right = new Vector3(Mathf.Cos(yaw), 0f, -Mathf.Sin(yaw)); Forward = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
            mesh = copies; int lods = copies.Length;
            rest = new Vector3[lods][]; restN = new Vector3[lods][]; wi = new int[lods][]; ww = new float[lods][]; throatW = new float[lods][]; throatDir = new Vector3[lods][]; bodyW = new float[lods][];
            cx = f.cen != null && f.cen.Length > 1 ? f.cen[0] : 0f; cz = f.cen != null && f.cen.Length > 1 ? f.cen[1] : 0f;
            for (int k = 0; k < lods; k++)
            {
                rest[k] = copies[k].vertices; restN[k] = copies[k].normals;
                var d = f.lods[Mathf.Min(fileLod != null && k < fileLod.Length ? fileLod[k] : k, f.lods.Length - 1)];
                int nv = rest[k].Length, nw = d.verts.Length / 3;
                wi[k] = new int[nv * 4]; ww[k] = new float[nv * 4];
                for (int i = 0; i < nv; i++)
                {
                    // the welded vertex at this position (brute force: a few thousand each, once per rig)
                    int best = -1; float bd = float.MaxValue; var p = rest[k][i];
                    for (int j = 0; j < nw; j++)
                    {
                        float dx = d.verts[j * 3] * scale - p.x, dy = d.verts[j * 3 + 1] * scale - p.y, dz = d.verts[j * 3 + 2] * scale - p.z, dd = dx * dx + dy * dy + dz * dz;
                        if (dd < bd) { bd = dd; best = j; }
                    }
                    float near = within != null && k < within.Length ? within[k] : 1e-3f;
                    if (bd > near * near * scale * scale) throw new InvalidOperationException($"LOD{k} vertex {i} is {Mathf.Sqrt(bd):0.000} from any weighted vertex");
                    Array.Copy(d.wi, best * 4, wi[k], i * 4, 4); Array.Copy(d.ww, best * 4, ww[k], i * 4, 4);
                }
                bodyW[k] = new float[nv];
                for (int i = 0; i < nv; i++) bodyW[k][i] = 1f - LegShare(k, i);
                // the throat: the white under the chin (body frame a within 0.75, y 1.0-1.6, f 0.9-1.6), its front half.
                // Weight and direction from the position alone: a normal-based weight and push tore the sac from the lip at
                // the mesh's split seam (a dark slit at a full swell, g15)
                throatW[k] = new float[nv]; throatDir[k] = new Vector3[nv];
                var sac = new Vector3(cx + Forward.x * 1.0f, 1.25f, cz + Forward.z * 1.0f);
                for (int i = 0; i < nv; i++)
                {
                    var p = rest[k][i]; float x = p.x - cx, z = p.z - cz;
                    float a = x * Right.x + z * Right.z, fw = x * Forward.x + z * Forward.z;
                    float e = (a / 0.75f) * (a / 0.75f) + ((p.y - 1.3f) / 0.32f) * ((p.y - 1.3f) / 0.32f) + ((fw - 1.25f) / 0.38f) * ((fw - 1.25f) / 0.38f);
                    throatW[k][i] = e < 1f && fw > 1.0f ? Mathf.SmoothStep(0f, 1f, 1f - e) * Mathf.Clamp01((fw - 1.0f) / 0.15f) : 0f;
                    var o = p - sac; throatDir[k][i] = o.sqrMagnitude > 1e-6f ? o.normalized : Forward;
                }
            }
        }

        static Vector3 V(float[] a, int i) => new Vector3(a[i], a[i + 1], a[i + 2]);

        /// <summary>The legs for this hull, on the given copies of its LOD meshes (readable), or null (no file, or the
        /// meshes no longer match it: a warning).</summary>
        /// <param name="fileLod">Which of the file's LODs each copy is (null: in order). The battle's far model is the Playground's LOD2.</param>
        /// <param name="scale">How much larger than the file's hull the copies are (the battle draws the Bullfrog 1.6 times its sculpt).
        /// The throat and the belly squash are in the file's units and are the Playground's alone.</param>
        /// <param name="within">How near (file units) a copy's vertex must be to a weighted one, per copy (null: a millimetre,
        /// the same mesh). A copy that is another cut of the same sculpt (the battle's far model) takes the weights of the
        /// nearest point of the densest LOD instead: give it the gap its cut leaves.</param>
        public static WeldedLegRig Parse(string json, Mesh[] copies, string who, int[] fileLod = null, float scale = 1f, float[] within = null)
        {
            if (string.IsNullOrEmpty(json) || copies == null) return null;
            try
            {
                var f = JsonUtility.FromJson<FileData>(json);
                if (f?.bones == null || f.lods == null || f.lods.Length == 0) return null;
                return new WeldedLegRig(f, copies, fileLod, scale, within);
            }
            catch (Exception ex)
            {
                Debug.LogWarning($"WeldedLegRig {who}: legs left still ({ex.Message}); run Tools/legrig.py on the current hull (docs/22)");
                return null;
            }
        }

        /// <summary>Each bone's matrix from Pose: the parent's, then a turn about the bone's rest head.</summary>
        public void Solve()
        {
            posed = false;
            for (int b = 0; b < m.Length; b++)
            {
                var local = Matrix4x4.Translate(head[b]) * Matrix4x4.Rotate(Pose[b]) * Matrix4x4.Translate(-head[b]);
                m[b] = parent[b] >= 0 ? m[parent[b]] * local : local;
                q[b] = parent[b] >= 0 ? q[parent[b]] * Pose[b] : Pose[b];
                if (Quaternion.Angle(Pose[b], Quaternion.identity) > 0.01f) posed = true;
            }
        }

        /// <summary>Bone b's head where the pose puts it (after Solve), in the hull's frame: the joint it turns about.</summary>
        public Vector3 Joint(int b) => parent[b] >= 0 ? m[parent[b]].MultiplyPoint3x4(head[b]) : head[b];

        /// <summary>LOD k's vertex i where the pose puts it (after Solve), the throat swelled.</summary>
        public Vector3 Skin(int k, int i)
        {
            var p = rest[k][i];
            if (Throat > 0f && throatW[k][i] > 0f) p += throatDir[k][i] * (Throat * ThroatDepth * throatW[k][i]);
            if (Squash > 0f)
            {
                float h = p.y - Floor;
                if (h < BellyBand)
                {
                    float up = Squash * (1f - Mathf.Max(0f, h) / BellyBand) * bodyW[k][i];
                    float out_ = 1f + 0.3f * up / BellyBand;
                    p = new Vector3(cx + (p.x - cx) * out_, p.y + up, cz + (p.z - cz) * out_);
                }
            }
            if (!posed) return p;
            Vector3 s = Vector3.zero; float body = 0f;
            for (int j = 0; j < 4; j++)
            {
                int b = wi[k][i * 4 + j]; float w = ww[k][i * 4 + j];
                if (w <= 0f) continue;
                if (b < 0) body += w; else s += w * m[b].MultiplyPoint3x4(p);
            }
            var q = s + body * p;
            if (HasGround && body < 0.98f)
            {
                float under = GroundC - Vector3.Dot(GroundN, q);
                if (under > 0f) q += GroundDir * (under * Mathf.Clamp01((1f - body) / 0.1f));
            }
            return q;
        }

        /// <summary>How much of LOD k's vertex i follows the legs (0: the body).</summary>
        public float LegShare(int k, int i)
        {
            float s = 0f;
            for (int j = 0; j < 4; j++) if (wi[k][i * 4 + j] >= 0) s += ww[k][i * 4 + j];
            return s;
        }

        public int VertexCount(int k) => rest[k].Length;

        /// <summary>Write LOD k's skinned positions and normals into its mesh; the LOD shown before goes back to rest.</summary>
        public void Apply(int k)
        {
            k = Mathf.Clamp(k, 0, mesh.Length - 1);
            if (shown >= 0 && shown != k) { mesh[shown].vertices = rest[shown]; mesh[shown].normals = restN[shown]; }
            bool moved = posed || Throat > 0f || Squash > 0f;
            if (!moved && !shownPosed && shown == k) return;   // standing still: already at rest
            shown = k; shownPosed = moved;
            int n = rest[k].Length;
            if (buf == null || buf.Length < n) { buf = new Vector3[n]; bufN = new Vector3[n]; }
            for (int i = 0; i < n; i++)
            {
                buf[i] = Skin(k, i);
                if (!posed) { bufN[i] = restN[k][i]; continue; }   // (the throat's swell and the belly's squash keep their normals)
                Vector3 nn = Vector3.zero;
                for (int j = 0; j < 4; j++)
                {
                    int b = wi[k][i * 4 + j]; float w = ww[k][i * 4 + j];
                    if (w > 0f) nn += w * (b < 0 ? restN[k][i] : q[b] * restN[k][i]);
                }
                bufN[i] = nn.normalized;
            }
            mesh[k].SetVertices(buf, 0, n); mesh[k].SetNormals(bufN, 0, n);
        }
    }
}
