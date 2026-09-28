// Phase: deaths (2026-09-28, implemented) — part of DebrisRenderer: a man's parts as pieces (Head, Helm, Torso, Pelvis,
// Arm, Leg, Boot, Pack and the two halves), for the absurd deaths. Life-size, thrown at the figure's drawn scale
// (CombatFx.FigureScale), and laid out the way the debris shader rests a piece: it settles yaw-only, so a limb or a
// torso lies along +Z with its thin side up, a boot stands on its sole. Until the parts cut from the figures are in
// (Tools/gibsplit.py) each kind is a stand-in built here; the throws never need to know which.
// Vertex alpha is the cloth mask: 1 where the side's tint dyes it (the uniform), 0 where it keeps its own colour.
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class DebrisRenderer
    {
        /// <summary>A stand-in for a man's part, built in code.</summary>
        static void Part(Piece piece, List<Vector3> v, List<int> t, List<Color> c)
        {
            var rng = new DebrisRng(new Vector3((int)piece * 5.1f, 2f, 1.3f), 77u);
            Color cloth = new Color(1f, 1f, 1f, 1f), clothLow = new Color(0.86f, 0.86f, 0.86f, 1f);
            Color bare = new Color(1f, 1f, 1f, 0f), bareLow = new Color(0.88f, 0.88f, 0.88f, 0f);
            switch (piece)
            {
                case Piece.Head: Lump(v, t, c, 6, 4, 0.12f, 0.06f, ref rng, bareLow, bare, new Vector3(0.9f, 1f, 1.05f)); break;
                case Piece.Helm:
                {
                    int first = v.Count;
                    HelmetMesh(v, t, c);
                    for (int i = first; i < v.Count; i++) { v[i] *= 0.32f; var k = c[i]; k.a = 0f; c[i] = k; }
                    break;
                }
                case Piece.Torso: Box(v, t, c, new Vector3(0.40f, 0.24f, 0.52f), 0.9f, ref rng, clothLow, cloth, 0.03f); break;
                case Piece.Pelvis: Box(v, t, c, new Vector3(0.36f, 0.22f, 0.28f), 1f, ref rng, clothLow, cloth, 0.03f); break;
                case Piece.Arm: Along(v, t, c, 0.05f, 0.04f, 0.62f, Vector3.zero, clothLow, cloth); break;
                case Piece.Leg: Along(v, t, c, 0.08f, 0.055f, 0.86f, Vector3.zero, clothLow, cloth); break;
                case Piece.Boot:
                {
                    // the shaft over the heel and the foot ahead of it, standing on the sole
                    var low = new Color(0.8f, 0.8f, 0.8f, 0f); var high = new Color(1f, 1f, 1f, 0f);
                    int first = v.Count;
                    Box(v, t, c, new Vector3(0.10f, 0.22f, 0.12f), 1f, ref rng, low, high, 0.02f);
                    for (int i = first; i < v.Count; i++) v[i] += new Vector3(0f, 0.15f, -0.05f);
                    first = v.Count;
                    Box(v, t, c, new Vector3(0.10f, 0.08f, 0.27f), 0.85f, ref rng, low, high, 0.02f);
                    for (int i = first; i < v.Count; i++) v[i] += new Vector3(0f, 0.04f, 0.03f);
                    break;
                }
                case Piece.Pack: Box(v, t, c, new Vector3(0.30f, 0.14f, 0.34f), 0.95f, ref rng, clothLow, cloth, 0.04f); break;
                case Piece.UpperHalf:
                {
                    Box(v, t, c, new Vector3(0.40f, 0.24f, 0.52f), 0.9f, ref rng, clothLow, cloth, 0.03f);
                    int first = v.Count;
                    Lump(v, t, c, 6, 4, 0.12f, 0.06f, ref rng, bareLow, bare);
                    for (int i = first; i < v.Count; i++) v[i] += new Vector3(0f, 0.02f, 0.38f);
                    Along(v, t, c, 0.05f, 0.04f, 0.55f, new Vector3(-0.26f, 0f, -0.18f), clothLow, cloth);
                    Along(v, t, c, 0.05f, 0.04f, 0.55f, new Vector3(0.26f, 0f, -0.18f), clothLow, cloth);
                    break;
                }
                case Piece.LowerHalf:
                {
                    Box(v, t, c, new Vector3(0.36f, 0.22f, 0.28f), 1f, ref rng, clothLow, cloth, 0.03f);
                    Along(v, t, c, 0.08f, 0.055f, 0.86f, new Vector3(-0.1f, 0f, 0.12f), clothLow, cloth);
                    Along(v, t, c, 0.08f, 0.055f, 0.86f, new Vector3(0.1f, 0f, 0.12f), clothLow, cloth);
                    break;
                }
            }
        }

        /// <summary>A tapered tube lying along +Z from start (Taper's +Y turned a quarter about X, a proper turn so the
        /// faces keep their winding).</summary>
        static void Along(List<Vector3> v, List<int> t, List<Color> c, float r0, float r1, float length, Vector3 start, Color low, Color high)
        {
            int first = v.Count;
            Taper(v, t, c, r0, r1, length, 6, low, high, true);
            for (int i = first; i < v.Count; i++) { var p = v[i]; v[i] = start + new Vector3(p.x, -p.z, p.y); }
        }
    }
}
