// Phase: atrifle-look-r3 — part of CombatFx (see CombatFx.cs): the AT rifleman's gun, drawn.
// Only two figures are baked (Soldier and Sniper), so the AT rifleman is drawn as a plain Soldier and read as one. The
// harsh critique of 2026-10-19 measured it: his silhouette was all but the Sniper's. What tells him apart is the thing
// he carries - a 1.65 m bipod-laid anti-tank rifle (a real Boys or PTRD is 1.6 to 2.0 m) - so it is drawn as a prop of
// its own, laid along his facing at the height his drawn stance holds a rifle: on the ground by his hands when he lies
// behind it (AnimationController lays him down at his post), at his shoulder when he is up and moving.
// One mesh built in code, one instanced draw a frame for every living AT rifleman.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>The gun's length life-size, metres (a Boys / PTRD anti-tank rifle is 1.6 to 2.0 m long).</summary>
        public const float AtRifleLength = 1.65f;

        /// <summary>Does this archetype carry a drawn gun of its own? Only the AT rifleman: every other man's rifle is
        /// part of his baked figure.</summary>
        public static bool Carries(byte archetype) => archetype == InfantryArchetype.AtRifle;

        /// <summary>Where the gun is drawn: the centre of its length, the rotation that lays it along his facing, and
        /// how long it is at his drawn scale.</summary>
        public struct GunLay { public Vector3 Centre; public Quaternion Rotation; public float Length; }

        /// <summary>
        /// The gun of a man drawn at `foot` (his feet on the render ground, `groundY`) facing `shownYaw` in `stance`,
        /// at his drawn figure scale: laid along his facing at the chest height of that stance (CombatFx.ChestHeight),
        /// so it rides his hands prone and his shoulder standing, and reaches a little ahead of him.
        /// </summary>
        public static GunLay Lay(Vector3 foot, float groundY, float shownYaw, Stance stance, float scale)
        {
            float chest = ChestHeight(stance, false);
            var facing = new Vector3(Mathf.Sin(shownYaw), 0f, Mathf.Cos(shownYaw));
            var centre = new Vector3(foot.x, groundY + chest * scale, foot.z) + facing * (0.3f * scale);
            return new GunLay { Centre = centre, Rotation = Quaternion.LookRotation(facing, Vector3.up), Length = AtRifleLength * scale };
        }

        /// <summary>Is a gun drawn for this man? The three reasons not to, in one place so a still can say "no gun was
        /// drawn" as a number (CaptureRig writes `at_rifles`): he must be alive, carry one of his own, and not be a
        /// vehicle wearing an infantry archetype.</summary>
        public static bool Draws(bool alive, byte archetype, uint flags)
            => alive && Carries(archetype) && (flags & (uint)UnitFlags.Vehicle) == 0;

        /// <summary>How many guns the last frame drew.</summary>
        public int AtRiflesDrawn => atRifles.Count;

        Mesh atRifleMesh; Material atRifleMat; bool atRifleTried;
        readonly List<Matrix4x4> atRifles = new List<Matrix4x4>(64);
        Matrix4x4[] atRifleBatch = new Matrix4x4[128];

        /// <summary>Once a frame: one gun for every living AT rifleman, where his drawn pose puts it.</summary>
        void DrawAtRifles(Bounds bounds)
        {
            var w = Host.Local.World;
            atRifles.Clear();
            float scale = FigureScale();
            var anim = Host.Animation;
            for (int i = 0; i < w.HighWater; i++)
            {
                if (!Draws(w.IsAlive(i), w.Archetype[i], w.Flags[i])) continue;
                Vector3 foot = drawnAt != null ? drawnAt(i) : (Vector3)w.Position[i];
                float yaw = anim != null && anim.Yaw.IsCreated && i < anim.Yaw.Length ? anim.Yaw[i] : w.Yaw[i];
                var stance = (Stance)(anim != null && Host.UseAnimationController ? anim.State[i].Stance : w.StanceOf[i]);
                var lay = Lay(foot, foot.y, yaw, stance, scale);
                atRifles.Add(Matrix4x4.TRS(lay.Centre, lay.Rotation, new Vector3(scale, scale, lay.Length / AtRifleLength)));
                if (atRifles.Count == 1023) break;   // the instancing limit; a line this long is nobody's picture
            }
            if (atRifles.Count == 0) return;
            if (!AtRifleReady()) return;
            if (atRifleBatch.Length < atRifles.Count) atRifleBatch = new Matrix4x4[Mathf.NextPowerOfTwo(atRifles.Count)];
            atRifles.CopyTo(atRifleBatch, 0);
            var rp = new RenderParams(atRifleMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On };
            FrameBudget.Draw(rp, atRifleMesh, 0, atRifleBatch, atRifles.Count);   // every draw goes through the budget (FrameBudgetCoverageTests)
        }

        /// <summary>The mesh and the material, built once. False when the toon shader is not in the build's shader list
        /// (then the gun is simply not drawn, said once).</summary>
        bool AtRifleReady()
        {
            if (atRifleMesh != null && atRifleMat != null) return true;
            if (atRifleTried) return false;
            atRifleTried = true;
            var shader = Shader.Find("TW/Toon (URP)");
            if (shader == null) { Debug.LogWarning("CombatFx: no TW/Toon (URP) shader; the AT rifle is not drawn"); return false; }
            atRifleMat = new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            atRifleMat.SetColor("_BaseColor", new Color(0.17f, 0.16f, 0.15f));   // gunmetal, darker than the men's khaki
            atRifleMat.SetFloat("_OutlineWidth", 1.5f);
            atRifleMesh = BuildAtRifle();
            return true;
        }

        /// <summary>
        /// The gun life-size, lying along +Z with its centre at the origin (the draw scales it by his figure scale), the barrel on the axis,
        /// the receiver and box magazine behind it and two bipod legs spread under the barrel - the card picture
        /// (UiSkinGenerator.WeaponGlyph) as a solid. About 150 vertices.
        /// </summary>
        static Mesh BuildAtRifle()
        {
            var v = new List<Vector3>(160); var t = new List<int>(300);
            const float L = AtRifleLength;
            // half-extents, so the whole gun spans -0.5 L to +0.5 L: muzzle at the front, butt at the back
            Box(v, t, new Vector3(0f, 0f, 0.22f * L), new Vector3(0.035f, 0.035f, 0.28f * L));        // the barrel, out front
            Box(v, t, new Vector3(0f, 0.005f, -0.22f * L), new Vector3(0.055f, 0.06f, 0.14f * L));    // the receiver
            Box(v, t, new Vector3(0f, -0.08f, -0.17f * L), new Vector3(0.03f, 0.08f, 0.05f * L));     // the box magazine, under it
            Box(v, t, new Vector3(0f, -0.01f, -0.44f * L), new Vector3(0.04f, 0.045f, 0.06f * L));    // the butt
            for (int side = -1; side <= 1; side += 2)                                                  // the bipod legs
                Leg(v, t, new Vector3(0f, -0.03f, 0.30f * L), new Vector3(side * 0.16f, -0.26f, 0.22f * L), 0.016f);
            var mesh = new Mesh { name = "TW/AtRifle" };
            mesh.SetVertices(v); mesh.SetTriangles(t, 0);
            mesh.RecalculateNormals(); mesh.RecalculateBounds();
            return mesh;
        }

        /// <summary>A box of half-extents `half` about `at`, flat-shaded (its own four corners a face).</summary>
        static void Box(List<Vector3> v, List<int> t, Vector3 at, Vector3 half)
        {
            Vector3[] c =
            {
                at + new Vector3(-half.x, -half.y, -half.z), at + new Vector3(half.x, -half.y, -half.z),
                at + new Vector3(half.x, -half.y, half.z),   at + new Vector3(-half.x, -half.y, half.z),
                at + new Vector3(-half.x, half.y, -half.z),  at + new Vector3(half.x, half.y, -half.z),
                at + new Vector3(half.x, half.y, half.z),    at + new Vector3(-half.x, half.y, half.z),
            };
            int[] faces = { 4, 5, 6, 7, 1, 0, 3, 2, 0, 4, 7, 3, 5, 1, 2, 6, 3, 7, 6, 2, 0, 1, 5, 4 };
            for (int f = 0; f < faces.Length; f += 4)
            {
                int b = v.Count;
                for (int k = 0; k < 4; k++) v.Add(c[faces[f + k]]);
                t.Add(b); t.Add(b + 1); t.Add(b + 2); t.Add(b); t.Add(b + 2); t.Add(b + 3);
            }
        }

        /// <summary>A bipod leg: a square bar from `from` to `to`, `r` thick.</summary>
        static void Leg(List<Vector3> v, List<int> t, Vector3 from, Vector3 to, float r)
        {
            Vector3 along = to - from;
            Vector3 side = Vector3.Cross(along, Vector3.forward);
            if (side.sqrMagnitude < 1e-6f) side = Vector3.Cross(along, Vector3.right);
            side = side.normalized * r;
            Vector3 up = Vector3.Cross(along, side).normalized * r;
            Vector3[] a = { from - side - up, from + side - up, from + side + up, from - side + up };
            int b = v.Count;
            for (int k = 0; k < 4; k++) v.Add(a[k]);
            for (int k = 0; k < 4; k++) v.Add(a[k] + along);
            int[] quads = { 0, 1, 5, 4, 1, 2, 6, 5, 2, 3, 7, 6, 3, 0, 4, 7, 4, 5, 6, 7, 3, 2, 1, 0 };
            for (int f = 0; f < quads.Length; f += 4)
            {
                t.Add(b + quads[f]); t.Add(b + quads[f + 1]); t.Add(b + quads[f + 2]);
                t.Add(b + quads[f]); t.Add(b + quads[f + 2]); t.Add(b + quads[f + 3]);
            }
        }
    }
}
