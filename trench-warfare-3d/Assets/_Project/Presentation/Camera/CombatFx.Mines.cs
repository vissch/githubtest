// Phase: A5 / docs/21 SIM-D (implemented 2026-09-26: the picture of the mines; the sapper that lays them waits for
// units-meta) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools). Our own mines and
// tripwires are marked on the ground from MinePlaced until MineTriggered or MineCleared takes them away (the enemy's
// are never drawn: a mine is found by stepping on it); a trigger throws a flash of dust and clods at once, and the
// burst itself is the next tick's Explosion (Source 70 + kind), drawn as any shell's. The marks are kept by the sim's
// mine index, which a spent mine keeps and the next one reuses, so a placed mark replaces an old one.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        public struct MineMark { public int Index; public Vector3 Pos, Dir; public float Length; public byte Kind; }

        /// <summary>The marks of our own mines by the sim's index: placed, replaced, taken away. Plain, so a test can drive it.</summary>
        public sealed class MineMarks
        {
            public readonly List<MineMark> Marks = new List<MineMark>(64);
            /// <summary>The seat the picture is drawn for (as the incoming markers: b == 0 is ours).</summary>
            public const int LocalPlayer = 0;

            /// <summary>MinePlaced: a = index, b = player, pos, dir = heading x length (zero for a mine), scalar = kind.</summary>
            public void Placed(int index, int player, Vector3 pos, Vector3 dirLength, byte kind)
            {
                Gone(index);
                if (player != LocalPlayer) return;   // the enemy's are found by stepping on them
                var flat = new Vector3(dirLength.x, 0f, dirLength.z);
                float length = flat.magnitude;
                Marks.Add(new MineMark { Index = index, Pos = pos, Dir = length > 1e-3f ? flat / length : Vector3.forward, Length = length, Kind = kind });
            }

            /// <summary>MineTriggered or MineCleared: the mark goes, whoever's it was.</summary>
            public void Gone(int index)
            {
                for (int i = Marks.Count - 1; i >= 0; i--) if (Marks[i].Index == index) Marks.RemoveAt(i);
            }
        }

        readonly MineMarks mineMarks = new MineMarks();
        public const float MineMarkRadius = 0.45f, TripwireMarkWidth = 0.08f;

        void OnMinePlaced(SimEvent e) => mineMarks.Placed(e.A, e.B, (Vector3)e.Pos, (Vector3)e.Dir, (byte)Mathf.RoundToInt(e.Scalar));

        /// <summary>MineTriggered (a flash now; the burst is the next tick's Explosion) or MineCleared (the mark goes).</summary>
        void OnMineGone(SimEvent e, bool triggered)
        {
            mineMarks.Gone(e.A);
            if (!triggered || Host == null || Host.Local == null) return;
            Vector3 p = (Vector3)e.Pos;
            p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z);
            if (!Near(p, 160f)) return;
            if (books != null && books.Ready)
                for (int k = 0; k < 3; k++)
                    books.Add(FlipbookFx.Book.Spurt, p + new Vector3(Mathf.Lerp(-0.6f, 0.6f, Hash01(p.x, p.z, 30 + k)), 0.1f, Mathf.Lerp(-0.6f, 0.6f, Hash01(p.x, p.z, 40 + k))), 1.1f, 0.4f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 3f, grow: 0.5f);
            Throw(p + Vector3.up * 0.3f, 6, 2, 3f, 1.6f);
            CameraShake.Add(p, 0.5f);
        }

        /// <summary>Our mines: a small disc on the ground; our tripwires: a thin line along the wire. The marker material.</summary>
        void DrawMineMarks(Bounds bounds)
        {
            var marks = mineMarks.Marks;
            if (marks.Count == 0 || markMine == null || Host == null || Host.Local == null) return;
            var rp = new RenderParams(markMine) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off };
            var map = Host.Local.Map;
            batch.Clear();
            for (int i = 0; i < marks.Count; i++)
            {
                var m = marks[i];
                if (m.Length > 0f) continue;
                var at = m.Pos; at.y = RenderGround.Sample(map, at.x, at.z) + 0.12f;
                batch.Add(Matrix4x4.TRS(at, Quaternion.identity, new Vector3(MineMarkRadius * 2f, 0.04f, MineMarkRadius * 2f)));
            }
            if (batch.Count > 0) Flush(sphere, rp);
            batch.Clear();
            for (int i = 0; i < marks.Count; i++)
            {
                var m = marks[i];
                if (m.Length <= 0f) continue;
                var mid = m.Pos + m.Dir * (m.Length * 0.5f); mid.y = RenderGround.Sample(map, mid.x, mid.z) + 0.15f;
                batch.Add(Matrix4x4.TRS(mid, Quaternion.LookRotation(m.Dir), new Vector3(TripwireMarkWidth, 0.04f, m.Length)));
            }
            if (batch.Count > 0) Flush(cube, rp);
        }
    }
}
