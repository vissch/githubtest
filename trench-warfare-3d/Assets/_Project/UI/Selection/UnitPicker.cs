// Phase: B6 (implemented) — where every living unit is on the screen this frame, for the click, the drag box, the
// double-click and the hover label. Each unit is projected from where it is DRAWN (the presenter's interpolated
// position, laid on the drawn ground, at the height of its body), with a pick radius from its drawn size: men grow
// with the zoom exactly as VATRenderer draws them, vehicles by their footprint. The hit tests are static functions
// over plain structs, so the rules are tested without a camera.
// AOSA C67 (2026-09-26): with knob hud.selectionFast on (the default), Build reads the near plane once and projects with
// the camera's world-to-clip matrix taken once per frame (Project, the engine's own CameraProject arithmetic) instead
// of one Camera.WorldToScreenPoint call per man, which rebuilt that matrix and the viewport rect 3,000 times a frame.
// Screen positions only feed the hit tests (nothing is drawn from them); they agree with the engine's to float
// rounding (SelectionTests.ProjectMatchesTheEngine). hud.selectionFast=0 is the exact old loop.
using System.Collections.Generic;
using UnityEngine;
using TW.Presentation;
using TW.Sim;
using TW.Sim.Nav;
using TW.Sim.Terrain;

namespace TW.UI
{
    /// <summary>A living unit as the screen sees it this frame (screen px, bottom-left origin, as Mouse.position).</summary>
    public struct ScreenUnit
    {
        public int Slot; public ushort Gen;
        public Vector2 Screen; public float RadiusPx, Depth;
        public Vector3 World;       // the drawn body centre
        public float Size;          // drawn footprint diameter, metres (the selection marker's size)
        public byte Archetype; public bool Ours, Vehicle;
        public UnitHandle Handle => new UnitHandle(Slot, Gen);
    }

    public sealed class UnitPicker
    {
        // as VATRenderer draws a man: UnitScale 1.5, growing with the zoom from 24 up to x4
        public const float FigureScale = 1.5f, GrowFromZoom = 24f, MaxGrow = 4f;
        public const float ManRadiusM = 0.45f, ManBodyM = 0.9f, VehicleBodyM = 1.3f, MinPickPx = 10f, ClumpPx = 18f;

        public readonly List<ScreenUnit> Units = new List<ScreenUnit>(1024);
        public int BuiltFrame { get; private set; } = -1;
        /// <summary>C67: knob hud.selectionFast (read once by SelectionController): project with one matrix per frame.
        /// False is the old loop, one WorldToScreenPoint and one nearClipPlane read per unit.</summary>
        public bool FastProject;

        public static float Grow(float zoom) => Mathf.Clamp(zoom / GrowFromZoom, 1f, MaxGrow);

        /// <summary>Project every living unit once per frame (a second call in the same frame is free).</summary>
        public void Build(SimWorld w, SimPresenter p, MapData map, Camera cam, float zoom)
        {
            if (BuiltFrame == Time.frameCount) return;
            BuiltFrame = Time.frameCount;
            Units.Clear();
            if (w == null || p == null || map == null || cam == null) return;
            float grow = Grow(zoom), man = FigureScale * grow;
            float pxPerRad = Screen.height / (2f * Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad));
            Vector3 camPos = cam.transform.position, fwd = cam.transform.forward;
            bool fast = FastProject;
            float near = fast ? cam.nearClipPlane : 0f;
            Matrix4x4 toClip = fast ? cam.projectionMatrix * cam.worldToCameraMatrix : default;   // the engine's GetWorldToClipMatrix
            Rect view = fast ? cam.pixelRect : default;
            for (int i = 0; i < w.HighWater; i++)
            {
                uint f = w.Flags[i];
                if ((f & (uint)UnitFlags.Alive) == 0) continue;
                bool vehicle = (f & (uint)UnitFlags.Vehicle) != 0;
                var d = p.Drawn(i);
                float ground = RenderGround.Sample(map, d.x, d.z);
                float radiusM, size;
                Vector3 body;
                if (vehicle)
                {
                    var prof = VehicleProfile.ForArchetype(w.Archetype[i]);
                    radiusM = Mathf.Max(prof.HalfLength, prof.HalfWidth) * 0.8f;
                    size = Mathf.Max(prof.HalfLength, prof.HalfWidth) * 2.3f;
                    body = new Vector3(d.x, ground + VehicleBodyM, d.z);
                }
                else
                {
                    radiusM = ManRadiusM * man;
                    size = 1.1f * man;
                    body = new Vector3(d.x, ground + ManBodyM * man, d.z);
                }
                float depth = Vector3.Dot(body - camPos, fwd);
                if (depth <= (fast ? near : cam.nearClipPlane)) continue;
                Vector2 screen;
                if (!fast || !Project(toClip, view, body, out screen))
                {
                    Vector3 s = cam.WorldToScreenPoint(body);
                    screen = new Vector2(s.x, s.y);
                }
                Units.Add(new ScreenUnit
                {
                    Slot = i, Gen = w.Generation[i], Screen = screen, Depth = depth,
                    RadiusPx = Mathf.Max(MinPickPx, radiusM * pxPerRad / depth),
                    World = body, Size = size, Archetype = w.Archetype[i], Ours = (w.Team[i] & 1) == 0, Vehicle = vehicle,
                });
            }
        }

        /// <summary>
        /// C67: a world point to screen px (bottom-left origin) as Camera.WorldToScreenPoint does it for a mono camera:
        /// clip = worldToClip x (p, 1), divided by w, then mapped into the camera's pixel rect. False where w is not
        /// clearly positive (on or behind the eye plane), where the caller asks the engine instead.
        /// </summary>
        public static bool Project(in Matrix4x4 worldToClip, Rect pixelRect, Vector3 p, out Vector2 screen)
        {
            ref readonly Matrix4x4 m = ref worldToClip;
            float cw = m.m30 * p.x + m.m31 * p.y + m.m32 * p.z + m.m33;
            if (!(cw > 1e-7f)) { screen = default; return false; }
            float inv = 1f / cw;
            float cx = (m.m00 * p.x + m.m01 * p.y + m.m02 * p.z + m.m03) * inv;
            float cy = (m.m10 * p.x + m.m11 * p.y + m.m12 * p.z + m.m13) * inv;
            screen = new Vector2(pixelRect.x + (1f + cx) * pixelRect.width * 0.5f, pixelRect.y + (1f + cy) * pixelRect.height * 0.5f);
            return true;
        }

        /// <summary>
        /// The unit under the cursor, or -1: of those whose pick circle holds the point, the one the cursor is most
        /// centrally on (distance over radius), the nearer to the camera on a tie, so a man standing in front wins.
        /// </summary>
        public static int Nearest(List<ScreenUnit> units, Vector2 at)
        {
            int best = -1; float bestK = float.MaxValue, bestDepth = float.MaxValue;
            for (int i = 0; i < units.Count; i++)
            {
                var u = units[i];
                float k = (u.Screen - at).magnitude / u.RadiusPx;
                if (k > 1f) continue;
                if (k < bestK - 0.05f || (Mathf.Abs(k - bestK) <= 0.05f && u.Depth < bestDepth)) { best = i; bestK = k; bestDepth = u.Depth; }
            }
            return best;
        }

        /// <summary>
        /// The knot of units under the cursor: the nearest (as <see cref="Nearest"/>) and every unit of its side whose
        /// pick circle, widened to at least ClumpPx, holds the cursor too. Zoomed in, only men who really overlap on the
        /// screen join; zoomed out, where a squad is a smudge of dots, the smudge is one clump. The nearest comes first.
        /// Returns the nearest's index, or -1 (and an empty list) over empty ground.
        /// </summary>
        public static int Clump(List<ScreenUnit> units, Vector2 at, List<int> into)
        {
            into.Clear();
            int best = Nearest(units, at);
            if (best < 0) return -1;
            into.Add(best);
            bool ours = units[best].Ours;
            for (int i = 0; i < units.Count; i++)
            {
                if (i == best) continue;
                var u = units[i];
                if (u.Ours != ours) continue;
                float r = Mathf.Max(u.RadiusPx, ClumpPx);
                if ((u.Screen - at).sqrMagnitude <= r * r) into.Add(i);
            }
            return best;
        }

        /// <summary>Indices of the units whose screen centre is inside the rectangle (any two corners, screen px).</summary>
        public static void InRect(List<ScreenUnit> units, Vector2 a, Vector2 b, bool oursOnly, List<int> into)
        {
            into.Clear();
            float x0 = Mathf.Min(a.x, b.x), x1 = Mathf.Max(a.x, b.x), y0 = Mathf.Min(a.y, b.y), y1 = Mathf.Max(a.y, b.y);
            for (int i = 0; i < units.Count; i++)
            {
                var u = units[i];
                if (oursOnly && !u.Ours) continue;
                if (u.Screen.x >= x0 && u.Screen.x <= x1 && u.Screen.y >= y0 && u.Screen.y <= y1) into.Add(i);
            }
        }

        /// <summary>Double-click: every unit of that archetype and side whose centre is on the screen.</summary>
        public static void SameTypeOnScreen(List<ScreenUnit> units, byte archetype, bool ours, float screenW, float screenH, List<int> into)
        {
            into.Clear();
            for (int i = 0; i < units.Count; i++)
            {
                var u = units[i];
                if (u.Archetype != archetype || u.Ours != ours) continue;
                if (u.Screen.x >= 0f && u.Screen.x <= screenW && u.Screen.y >= 0f && u.Screen.y <= screenH) into.Add(i);
            }
        }
    }
}
