// Phase: B6 (implemented) — while a barrage or gas is being aimed: who is under it. Beside the reticle, the enemy the
// strike covers and, in red, how many of OUR men are within its reach; on the field, rust brackets under the enemy in
// the circle and red ones under ours in reach, so shelling your own line is a choice you see, not a surprise. The reach
// for ours is the circle plus one shell's blast (HE shells land anywhere in the circle and burst ShellRadius wide);
// gas has no radius in the sim (it drifts), so its reticle is used for the enemy and twice that for ours.
// Counts come from the drawn positions (UnitPicker's frame), the same place the player sees the men. A line ability
// (docs/21 phase 5) is counted as a capsule: the enemy within the corridor's half width of the line, ours within the
// half width plus a burst's reach of it (ShowLine, from AbilityAim's Shape).
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;
using TW.Sim.Match;
using TW.Presentation.Tactical;

namespace TW.UI
{
    public sealed class AimReadout
    {
        public const float GasReticleM = 8f, GasReachFactor = 2f, OffsetX = 26f, OffsetY = -10f;

        readonly VisualElement box;
        readonly Label enemyLine, oursLine;
        public readonly List<ScreenUnit> EnemyIn = new List<ScreenUnit>(64), OursIn = new List<ScreenUnit>(64);
        int shownEnemy = -1, shownOurs = -1;
        static readonly string[] Cache = new string[1024];

        public AimReadout(VisualElement root)
        {
            box = root?.Q("aim-readout");
            if (box == null) return;
            box.pickingMode = PickingMode.Ignore; box.Clear();
            enemyLine = new Label { pickingMode = PickingMode.Ignore }; enemyLine.AddToClassList("hud-aim__enemy"); box.Add(enemyLine);
            oursLine = new Label { pickingMode = PickingMode.Ignore }; oursLine.AddToClassList("hud-aim__ours"); box.Add(oursLine);
            box.style.display = DisplayStyle.None;
        }

        /// <summary>The circle the enemy is counted in and the reach our men are counted in, metres.</summary>
        public static bool Radii(OffMapAbilityId id, out float hit, out float reach)
        {
            hit = reach = 0f;
            if (!OffMapAbilitySystem.TryGetStats((int)id, out var s)) return false;
            if (s.Target == AbilityTargetMode.Line || s.Target == AbilityTargetMode.Heading) { hit = s.HalfWidth; reach = s.HalfWidth + Mathf.Max(s.ShellRadius, GasReticleM); }
            else if (s.Radius > 0f) { hit = s.Radius; reach = s.Radius + s.ShellRadius; }
            else { hit = GasReticleM; reach = GasReticleM * GasReachFactor; }
            return true;
        }

        /// <summary>Sort the units into the enemy within the corridor (halfWidth of the segment) and ours within reach of it.</summary>
        public static void TallyLine(List<ScreenUnit> units, Vector2 startXZ, Vector2 dirXZ, float length, float halfWidth, float reach, List<ScreenUnit> enemyIn, List<ScreenUnit> oursIn)
        {
            enemyIn.Clear(); oursIn.Clear();
            float len2 = length * length;
            foreach (var u in units)
            {
                float px = u.World.x - startXZ.x, pz = u.World.z - startXZ.y;
                float t = len2 > 1e-4f ? Mathf.Clamp01((px * dirXZ.x + pz * dirXZ.y) / length) : 0f;
                float ax = px - dirXZ.x * length * t, az = pz - dirXZ.y * length * t;
                float d2 = ax * ax + az * az;
                if (u.Ours) { if (d2 <= reach * reach) oursIn.Add(u); }
                else if (d2 <= halfWidth * halfWidth) enemyIn.Add(u);
            }
        }

        /// <summary>Count and show for a corridor (a line ability being aimed); cursor in HUD px.</summary>
        public void ShowLine(List<ScreenUnit> units, OffMapAbilityId id, in AimShape shape, Vector2 cursor)
        {
            if (box == null || !Radii(id, out _, out float reach)) { Hide(); return; }
            TallyLine(units, new Vector2(shape.Start.x, shape.Start.z), new Vector2(shape.Dir.x, shape.Dir.z), shape.Length, shape.HalfWidth, reach, EnemyIn, OursIn);
            Present(cursor);
        }

        /// <summary>Sort the units into the enemy inside hit and ours inside reach of the aim point (x, z).</summary>
        public static void Tally(List<ScreenUnit> units, Vector2 aimXZ, float hit, float reach, List<ScreenUnit> enemyIn, List<ScreenUnit> oursIn)
        {
            enemyIn.Clear(); oursIn.Clear();
            float h2 = hit * hit, r2 = reach * reach;
            foreach (var u in units)
            {
                float dx = u.World.x - aimXZ.x, dz = u.World.z - aimXZ.y, d2 = dx * dx + dz * dz;
                if (u.Ours) { if (d2 <= r2) oursIn.Add(u); }
                else if (d2 <= h2) enemyIn.Add(u);
            }
        }

        /// <summary>Count and show for this aim point; cursor in HUD px.</summary>
        public void Show(List<ScreenUnit> units, OffMapAbilityId id, Vector3 aim, Vector2 cursor)
        {
            if (box == null || !Radii(id, out float hit, out float reach)) { Hide(); return; }
            Tally(units, new Vector2(aim.x, aim.z), hit, reach, EnemyIn, OursIn);
            Present(cursor);
        }

        void Present(Vector2 cursor)
        {
            if (EnemyIn.Count != shownEnemy)
            {
                shownEnemy = EnemyIn.Count;
                enemyLine.text = Text(0, shownEnemy);
                enemyLine.EnableInClassList("hud-aim__enemy--none", shownEnemy == 0);
            }
            if (OursIn.Count != shownOurs)
            {
                shownOurs = OursIn.Count;
                oursLine.text = Text(1, shownOurs);
                oursLine.EnableInClassList("hud-aim__ours--danger", shownOurs > 0);
            }
            box.style.left = cursor.x + OffsetX; box.style.top = cursor.y + OffsetY;
            if (box.style.display != DisplayStyle.Flex) box.style.display = DisplayStyle.Flex;
        }

        public void Hide()
        {
            EnemyIn.Clear(); OursIn.Clear();
            if (box != null && box.style.display != DisplayStyle.None) box.style.display = DisplayStyle.None;
        }

        /// <summary>"UNDER IT: 14 ENEMY" / "3 OF OURS IN REACH", cached per count (no string per frame).</summary>
        static string Text(int line, int n)
        {
            int k = line * 512 + Mathf.Min(n, 511);
            return Cache[k] ??= line == 0
                ? (n == 0 ? "NO ENEMY UNDER IT" : "UNDER IT: " + n + " ENEMY")
                : (n == 0 ? "NONE OF OURS IN REACH" : n + " OF OURS IN REACH");
        }
    }
}
