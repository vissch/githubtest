// Phase: B6 (implemented) - where every living unit is on the screen this frame, for the click, the drag box, the
// double-click and the hover label. Each unit is projected from where it is DRAWN (the presenter's interpolated
// position, laid on the drawn ground, at the height of its body), with a pick radius from its drawn size: men grow
// with the zoom exactly as VATRenderer draws them, vehicles by their footprint. The hit tests are static functions
// over plain structs, so the rules are tested without a camera.
// AOSA C67 (2026-09-26): Build reads the near plane once and projects with the camera's world-to-clip matrix taken
// once per frame (Project, the engine's own CameraProject arithmetic) instead of one Camera.WorldToScreenPoint call
// per man, which rebuilt that matrix and the viewport rect 3,000 times a frame. Screen positions only feed the hit
// tests (nothing is drawn from them); they agree with the engine's to float rounding
// (SelectionTests.ProjectMatchesTheEngine).
// Z016.1 (2026-10-12): Build is a Burst job over SimPresenter.Poses - already alive-only and interpolated this frame,
// so the managed loop over every slot, its second interpolation and its second height sample are gone - writing a
// NativeList<ScreenUnit> every reader takes. The drawn height is RenderGround.DrawnY, the renderer's own formula, so
// a climbing or lifted man is picked where his body is drawn, not on the trench floor. Knob hud.selectionFast no
// longer touches the projection (there is one path now); it still skips filling the handle index while nothing is
// selected (SelectionController).
using System.Collections.Generic;
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
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

    public sealed class UnitPicker : System.IDisposable
    {
        // as VATRenderer draws a man (FigureMetrics is the one definition both read)
        public const float FigureScale = FigureMetrics.UnitScale, GrowFromZoom = FigureMetrics.GrowFromZoom, MaxGrow = FigureMetrics.MaxGrow;
        public const float ManRadiusM = 0.45f, ManBodyM = 0.9f, VehicleBodyM = 1.3f, MinPickPx = 10f, ClumpPx = 18f;

        /// <summary>Every living unit on the screen this frame, written by BuildJob (Persistent: Dispose with the owner).</summary>
        public NativeList<ScreenUnit> Units = new NativeList<ScreenUnit>(1024, Allocator.Persistent);
        public int BuiltFrame { get; private set; } = -1;
        /// <summary>Vehicle footprints where the sim has no kinematics system (a test world): VehicleProfile.ForArchetype
        /// is a switch Burst cannot call, so it is baked once into a native table.</summary>
        NativeArray<VehicleProfile> fallbackProfiles;
        /// <summary>Stand-in for Lift and Hop when the controller is off: a job cannot be scheduled with an
        /// unassigned container (VATRenderer.FillJob keeps the same kind of spare).</summary>
        NativeArray<float> spare;

        public static float Grow(float zoom) => Mathf.Clamp(zoom / GrowFromZoom, 1f, MaxGrow);

        /// <summary>Project every living unit once per frame (a second call in the same frame is free).</summary>
        /// <param name="lift">AnimationController.Lift and Hop, per slot, with controlled saying the controller drives
        /// the figures: without them a climbing or lifted man would be picked off his drawn body.</param>
        public void Build(SimWorld w, SimPresenter p, MapData map, Camera cam, float zoom,
                          NativeArray<float> lift = default, NativeArray<float> hop = default, bool controlled = false)
        {
            if (BuiltFrame == Time.frameCount) return;
            BuiltFrame = Time.frameCount;
            Units.Clear();
            if (w == null || p == null || map == null || cam == null || p.PoseCount == 0) return;
            if (Units.Capacity < p.PoseCount) Units.Capacity = p.PoseCount;
            float grow = Grow(zoom), man = FigureScale * grow;
            var kin = w.GetSystem<TW.Sim.Nav.VehicleKinematicsSystem>();
            var profiles = kin != null && kin.Profiles.IsCreated ? kin.Profiles : FallbackProfiles();
            float pxPerRad = Screen.height / (2f * Mathf.Tan(cam.fieldOfView * 0.5f * Mathf.Deg2Rad));
            bool useLift = controlled && lift.IsCreated && hop.IsCreated;
            new BuildJob
            {
                Poses = p.Poses, PoseSlot = p.PoseSlot, PoseCount = p.PoseCount, Generation = w.Generation,
                Height = map.Height, Ground = ReferenceEquals(RenderGround.Map, map) && RenderGround.Grid.Heights.IsCreated
                                              ? RenderGround.Grid
                                              : new RenderGroundGrid { Heights = Spare(), Width = 0, Length = 0, Step = 1f },
                Lift = useLift ? lift : Spare(), Hop = useLift ? hop : Spare(), Controlled = useLift,
                Profiles = profiles, Man = man, PxPerRad = pxPerRad,
                CamPos = cam.transform.position, Fwd = cam.transform.forward, Near = cam.nearClipPlane,
                ToClip = cam.projectionMatrix * cam.worldToCameraMatrix,   // the engine's GetWorldToClipMatrix
                View = cam.pixelRect, Out = Units,
            }.Run();
        }

        NativeArray<float> Spare()
        {
            if (!spare.IsCreated) spare = new NativeArray<float>(1, Allocator.Persistent);
            return spare;
        }

        NativeArray<VehicleProfile> FallbackProfiles()
        {
            if (!fallbackProfiles.IsCreated)
            {
                fallbackProfiles = new NativeArray<VehicleProfile>(256, Allocator.Persistent);
                for (int a = 0; a < 256; a++) fallbackProfiles[a] = VehicleProfile.ForArchetype((byte)a);
            }
            return fallbackProfiles;
        }

        public void Dispose()
        {
            if (Units.IsCreated) Units.Dispose();
            if (fallbackProfiles.IsCreated) fallbackProfiles.Dispose();
            if (spare.IsCreated) spare.Dispose();
        }

        /// <summary>Z016.1: one man a pose, over the poses the renderer draws from.</summary>
        [BurstCompile]
        struct BuildJob : IJob
        {
            [ReadOnly] public NativeArray<UnitPose> Poses;
            [ReadOnly] public NativeArray<int> PoseSlot;
            public int PoseCount;
            [ReadOnly] public NativeArray<ushort> Generation;
            [ReadOnly] public NativeArray<float> Lift, Hop;
            [ReadOnly] public NativeArray<VehicleProfile> Profiles;
            public Heightfield Height;
            public RenderGroundGrid Ground;
            public bool Controlled;
            public float Man, PxPerRad, Near;
            public float3 CamPos, Fwd;
            public Matrix4x4 ToClip;
            public Rect View;
            [WriteOnly] public NativeList<ScreenUnit> Out;

            public void Execute()
            {
                for (int i = 0; i < PoseCount; i++)
                {
                    var pose = Poses[i];
                    int slot = PoseSlot[i];
                    float ground = RenderGround.DrawnY(in Height, in Ground, pose.Pos.x, pose.Pos.z,
                                                       Controlled ? Lift[slot] : 0f, Controlled ? Hop[slot] : 0f, Man);
                    bool vehicle = (pose.Flags & (byte)UnitFlags.Vehicle) != 0;
                    float radiusM, size, bodyM;
                    if (vehicle)
                    {
                        var prof = Profiles[pose.Archetype];
                        float half = math.max(prof.HalfLength, prof.HalfWidth);
                        radiusM = half * 0.8f; size = half * 2.3f; bodyM = VehicleBodyM;
                    }
                    else
                    {
                        radiusM = ManRadiusM * Man; size = 1.1f * Man; bodyM = ManBodyM * Man;
                    }
                    var body = new Vector3(pose.Pos.x, ground + bodyM, pose.Pos.z);
                    float depth = math.dot((float3)body - CamPos, Fwd);
                    if (depth <= Near) continue;
                    if (!Project(ToClip, View, body, out Vector2 screen)) continue;
                    Out.AddNoResize(new ScreenUnit
                    {
                        Slot = slot, Gen = Generation[slot], Screen = screen, Depth = depth,
                        RadiusPx = math.max(MinPickPx, radiusM * PxPerRad / depth),
                        World = body, Size = size, Archetype = pose.Archetype, Ours = (pose.Team & 1) == 0, Vehicle = vehicle,
                    });
                }
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
        public static int Nearest(NativeArray<ScreenUnit> units, Vector2 at)
        {
            int best = -1; float bestK = float.MaxValue, bestDepth = float.MaxValue;
            for (int i = 0; i < units.Length; i++)
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
        public static int Clump(NativeArray<ScreenUnit> units, Vector2 at, List<int> into)
        {
            into.Clear();
            int best = Nearest(units, at);
            if (best < 0) return -1;
            into.Add(best);
            bool ours = units[best].Ours;
            for (int i = 0; i < units.Length; i++)
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
        public static void InRect(NativeArray<ScreenUnit> units, Vector2 a, Vector2 b, bool oursOnly, List<int> into)
        {
            into.Clear();
            float x0 = Mathf.Min(a.x, b.x), x1 = Mathf.Max(a.x, b.x), y0 = Mathf.Min(a.y, b.y), y1 = Mathf.Max(a.y, b.y);
            for (int i = 0; i < units.Length; i++)
            {
                var u = units[i];
                if (oursOnly && !u.Ours) continue;
                if (u.Screen.x >= x0 && u.Screen.x <= x1 && u.Screen.y >= y0 && u.Screen.y <= y1) into.Add(i);
            }
        }

        /// <summary>Double-click: every unit of that archetype and side whose centre is on the screen.</summary>
        public static void SameTypeOnScreen(NativeArray<ScreenUnit> units, byte archetype, bool ours, float screenW, float screenH, List<int> into)
        {
            into.Clear();
            for (int i = 0; i < units.Length; i++)
            {
                var u = units[i];
                if (u.Archetype != archetype || u.Ours != ours) continue;
                if (u.Screen.x >= 0f && u.Screen.x <= screenW && u.Screen.y >= 0f && u.Screen.y <= screenH) into.Add(i);
            }
        }
    }
}
