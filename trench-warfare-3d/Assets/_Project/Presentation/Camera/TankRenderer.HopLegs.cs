// Phase: the battle (2026-10-07) — the Bullfrog's legs move as it hops
// Owner, 2026-10-07, on the film of the Bullfrog in the battle: "we need to animate his legs as well as if he jumps,
// then we land it. i think we already did this at some point that needs to carry through". It was done in the
// Playground (HopDrive: the legs welded into the hull are skinned by WeldedLegRig and posed through a hop); the battle
// drew the hull as one stiff piece that rose and fell. This carries the legs through: each living Bullfrog has its own
// copy of the hull mesh at both distances, posed by HopLegs from the battle's own hop (TankRenderer.HopPose) and drawn
// in place of the shared hull. The battle model's hull IS the Playground's (measured 2026-10-07: the same 2022 points
// in the same frame), so the weights of Tools/legrig.py fit it as they are, scaled by the size it is drawn at. The far
// model is another cut of the sculpt and borrows the near one's weights by nearest point.
//
// The battle's hop is an arc over its whole phase (it is in the air from 0 to 1), so the phase is mapped onto the
// flight share of the Playground's hop: the hind legs unfold and trail, the forelegs tuck and then reach for the
// ground. A landing folds the hind knees for a moment (Absorb). Sitting, the legs are as sculpted. Killed, they sprawl.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        /// <summary>A hopper's own legs: its copies of the hull at both distances, the rig that skins them, and the
        /// drives it is posed by now (eased, so a hop that starts or is cut short does not snap).</summary>
        public sealed class HopRig
        {
            public WeldedLegRig Rig; public HopLegs.Bones Bones; public Mesh[] Mesh;
            public float Extend, Trail, Open, Tuck, Reach, Absorb, Splay;
        }

        const string HopLegsResource = "Vehicles/Bullfrog/Bullfrog_legs";
        /// <summary>Which LOD of the legs' file each of the battle's two levels takes its weights from, and how near a
        /// vertex must be to a weighted one (file units). The near model is the file's LOD0, point for point. The far
        /// model is another cut of the same sculpt (860 points, neither of the Playground's lower LODs: measured
        /// 2026-10-07), so each of its points takes the weights of the nearest point of LOD0.</summary>
        public static readonly int[] HopLegsLod = { 0, 0 };
        public static readonly float[] HopLegsWithin = { 1e-3f, 0.25f };
        const float HopLegEase = 16f;        // how fast the legs follow their drives, 1/s
        const float HopAbsorbFade = 3.5f;    // a landing's fold of the knees dies away, 1/s
        string hopLegsJson; bool hopLegsMissing;

        /// <summary>The legs of this view, made the first time it is posed; null when the model has no legs file or its
        /// hull no longer matches it (a warning from WeldedLegRig, once: the hull is then drawn stiff, as before).</summary>
        HopRig HopRigOf(View v)
        {
            if (v.Hop != null || hopLegsMissing) return v.Hop;
            if (hopLegsJson == null)
            {
                var text = Resources.Load<TextAsset>(HopLegsResource);
                hopLegsJson = text != null ? text.text : "";
            }
            var copies = new Mesh[2];
            for (int lod = 0; lod < 2; lod++)
            {
                var l = v.Model.Lods[lod];
                int hull = l != null ? RootPart(l) : -1;
                var src = hull >= 0 ? l.Parts[hull].Mesh : null;
                if (src == null || !src.isReadable) { hopLegsMissing = true; break; }
                var copy = Instantiate(src); copy.name = src.name + " (legs)";
                var bb = src.bounds; bb.Expand(0.6f * bb.size.magnitude); copy.bounds = bb;   // a leg reaching out is never culled
                copies[lod] = copy;
            }
            var rig = hopLegsMissing ? null : WeldedLegRig.Parse(hopLegsJson, copies, "Bullfrog (battle)", HopLegsLod, BullfrogScale, HopLegsWithin);
            if (rig == null)
            {
                hopLegsMissing = true;
                foreach (var m in copies) if (m != null) Destroy(m);
                return null;
            }
            v.Hop = new HopRig { Rig = rig, Bones = new HopLegs.Bones(rig), Mesh = copies };
            return v.Hop;
        }

        static int RootPart(TankModel.Lod l)
        {
            for (int i = 0; i < l.Parts.Count; i++) if (l.Parts[i].Parent < 0) return i;
            return -1;
        }

        /// <summary>Pose the legs for this frame: phase 0 is sitting, anything above is that far through the hop's arc.</summary>
        void PoseHopLegs(View v, float phase, bool landed, bool dead, float dt)
        {
            var h = HopRigOf(v);
            if (h == null) return;
            float extend = 0f, trail = 0f, open = 0f, tuck = 0f, reach = 0f, splay = dead ? 1f : 0f;
            if (!dead && phase > 0f)
                HopLegs.Drives(HopLegs.Crouch + HopLegs.Flight * Mathf.Clamp01(phase), out extend, out trail, out open, out tuck, out reach, out _);
            if (landed) h.Absorb = 1f;
            float k = 1f - Mathf.Exp(-dt * HopLegEase);
            h.Extend = Mathf.Lerp(h.Extend, extend, k); h.Trail = Mathf.Lerp(h.Trail, trail, k); h.Open = Mathf.Lerp(h.Open, open, k);
            h.Tuck = Mathf.Lerp(h.Tuck, tuck, k); h.Reach = Mathf.Lerp(h.Reach, reach, k);
            h.Splay = Mathf.Lerp(h.Splay, splay, 1f - Mathf.Exp(-dt * 4f));
            h.Absorb = dead ? 0f : Mathf.Max(0f, h.Absorb - dt * HopAbsorbFade);
            HopLegs.Pose(h.Rig, h.Bones, h.Extend, h.Extend, h.Trail, h.Open, h.Tuck, h.Reach, Mathf.Sin(h.Absorb * Mathf.PI) * 0.9f, h.Splay, 0f);
            h.Rig.Solve();
        }

        /// <summary>The mesh a part is drawn with: a hopper's own posed hull for its root part, the model's for every other.</summary>
        Mesh HopMesh(View v, TankModel.Part p, int lod)
        {
            if (v.Hop == null || p.Parent >= 0) return p.Mesh;
            v.Hop.Rig.Apply(lod);
            return v.Hop.Mesh[lod];
        }

        /// <summary>A view that is gone gives its hull copies back (and the batches they were drawn through).</summary>
        void FreeHopRig(View v)
        {
            if (v.Hop == null) return;
            foreach (var m in v.Hop.Mesh) if (m != null) { batches.Remove(m); Destroy(m); }
            v.Hop = null;
        }
    }
}
