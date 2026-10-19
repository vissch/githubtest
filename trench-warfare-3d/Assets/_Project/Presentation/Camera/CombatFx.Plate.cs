// Phase: character sim (2026-10-19) — part of CombatFx (see CombatFx.cs for the pools and the shared batch): the Sentry's
// own look. Only Soldier and Sniper are baked (VATRenderer.FigureOfArchetype), so he is the baked soldier like the MG
// man he borrows his gun from. What tells the two apart on a T2 sheet is drawn over him: the 8 mm plate he fires over,
// on its post.
//
// r2 (the harsh critic, 12/100): r1 hung the plate off the chest socket every frame, so its post ended 0.455 m above the
// ground and the whole thing swung round with the gun as he traversed, then vanished the frame he died. A plate is
// emplaced: the barrel is read once, when he sets it down, and from then on the plate stands on the ground where he put
// it, whatever the gun does and whether he is alive.
using UnityEngine;
using Unity.Mathematics;
using TW.Sim;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>At most this many men are given their plate in a frame, so the stress preset cannot pay for a crowd.</summary>
        public const int PlateMax = 64;
        /// <summary>The most pieces one man's plate has (the plate and its post), so a caller can size its array.</summary>
        public const int PlatePieceMax = 2;
        readonly Matrix4x4[] platePieces = new Matrix4x4[PlatePieceMax];

        /// <summary>Where one Sentry set his plate down: the point on the terrain its post stands on, and the way it faces.</summary>
        public struct PlateSpot { public Vector3 Foot, Fwd; public bool Set; }

        PlateSpot[] spots = new PlateSpot[0];

        /// <summary>Where one man's plate stands, for tooling to read (docs/evidence/sentry-look-r2/shoot.py checks a
        /// spot was emplaced before it shoots). An unset spot for a slot that carries no plate.</summary>
        public PlateSpot SpotOf(int slot) => slot >= 0 && slot < spots.Length ? spots[slot] : default;

        /// <summary>
        /// When this is on, the plate, its post and a calibration bar are drawn unlit and flat-coloured, so a still of
        /// them can be measured in pixels (docs/evidence/sentry-look-r2). Off in play.
        /// </summary>
        public bool PlateMask;
        Material maskPlateMat, maskBarMat;

        /// <summary>
        /// Where the plate goes, decided once. <paramref name="had"/> is returned unchanged the moment it is set: the
        /// barrel is read at emplacement and never again, so the plate does not traverse with the gun. The caller puts the
        /// terrain height into <c>Foot.y</c>.
        /// </summary>
        public static PlateSpot Emplace(PlateSpot had, Vector3 feet, Vector3 barrel, float scale)
        {
            if (had.Set) return had;
            Vector3 fwd = new Vector3(barrel.x, 0f, barrel.z);
            fwd = fwd.sqrMagnitude > 0.0001f ? fwd.normalized : Vector3.forward;
            return new PlateSpot { Foot = feet + fwd * (0.32f * scale), Fwd = fwd, Set = true };
        }

        /// <summary>
        /// The pieces of one Sentry's plate standing on <paramref name="foot"/>: 0 the plate, 1 the post under it, whose
        /// bottom is exactly <paramref name="foot"/>. Pure - no Unity state, no camera - so an EditMode test can measure
        /// it. Returns how many of <paramref name="into"/> were filled: 0 for every archetype that carries no plate.
        /// </summary>
        public static int PlatePieces(byte archetype, Vector3 foot, Vector3 fwd, float scale, Matrix4x4[] into)
        {
            if (archetype != InfantryArchetype.Sentry || into == null || into.Length < PlatePieceMax) return 0;
            Frame(fwd, out var turn, out Vector3 up);
            // the plate: 0.90 m across (two shoulders, so the 60 degree arc reads at T2) by 0.62 m tall (chest to thigh
            // on a man 25 % shorter, UnitScale) by 0.03 m - the 8 mm plate drawn thick enough to see. Centred 1.04 m up,
            // so its top is 1.35 m: still just over the muzzle of a man whose chest socket is about 1.30 m.
            var plate = new Vector3(0.90f, 0.62f, 0.03f) * scale;
            into[0] = Matrix4x4.TRS(foot + up * (1.04f * scale), turn, plate);
            // the post: its height is what is left between the plate's bottom (1.04 - 0.31 = 0.73) and the ground, so its
            // own bottom is the foot point exactly. r1 fixed it at 0.55 m and left the thing floating.
            into[1] = Matrix4x4.TRS(foot + up * (0.365f * scale), turn, new Vector3(0.10f, 0.73f, 0.10f) * scale);
            return PlatePieceMax;
        }

        /// <summary>
        /// The calibration bar of the mask mode: 0.50 m tall standing on the same ground as the post, and wider than the
        /// 0.90 m plate at both ends, so a still turns pixels into metres.
        /// </summary>
        public static Matrix4x4 PlateBar(Vector3 foot, Vector3 fwd, float scale)
        {
            Frame(fwd, out var turn, out Vector3 up);
            return Matrix4x4.TRS(foot + up * (0.25f * scale), turn, new Vector3(1.40f, 0.50f, 0.02f) * scale);
        }

        /// <summary>The plate's frame from the way it faces: upright, facing <paramref name="fwd"/> flattened.</summary>
        static void Frame(Vector3 fwd, out Quaternion turn, out Vector3 up)
        {
            Vector3 f = fwd.sqrMagnitude > 0.0001f ? fwd.normalized : Vector3.forward;
            Vector3 right = Vector3.Cross(Vector3.up, f);
            right = right.sqrMagnitude > 0.0001f ? right.normalized : Vector3.right;
            up = Vector3.Cross(f, right).normalized;
            turn = Quaternion.LookRotation(f, up);
        }

        static Material Unlit(Color color)
        {
            var shader = Shader.Find("Universal Render Pipeline/Unlit");
            var mat = shader != null ? new Material(shader) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave } : Painted(color, 0f);
            mat.SetColor("_BaseColor", color);
            if (mat.HasProperty("_Emission")) mat.SetColor("_Emission", color);
            return mat;
        }

        /// <summary>Draw the plate of every Sentry on the field, from where he emplaced it: near tier only, where the
        /// sockets exist, but once set it is drawn after he dies too - a plate does not fall over with its man.</summary>
        void DrawPlate(Bounds bounds)
        {
            if (units == null) return;
            var w = Host.Local.World;
            if (spots.Length < w.HighWater) System.Array.Resize(ref spots, Mathf.Max(w.HighWater, 64));
            batch.Clear();
            int men = 0;
            for (int slot = 0; slot < w.HighWater && men < PlateMax; slot++)
            {
                uint flags = w.Flags[slot];
                if ((flags & (uint)UnitFlags.Vehicle) != 0) continue;
                if (w.Archetype[slot] != InfantryArchetype.Sentry) { spots[slot] = default; continue; }   // a reused slot keeps no dead man's plate
                ViewNow(out _, out _, out float scale);
                if (!spots[slot].Set)
                {
                    if ((flags & (uint)UnitFlags.Alive) == 0) continue;                                   // he died before he could set it down
                    if (!units.Sockets(slot, out _, out var barrel, out _)) continue;                    // far tier, or the controller is off
                    float3 p = Host.Presenter.Drawn(slot);
                    var spot = Emplace(spots[slot], new Vector3(p.x, 0f, p.z), barrel, scale);
                    spot.Foot.y = RenderGround.Sample(Host.Local.Map, spot.Foot.x, spot.Foot.z);
                    spots[slot] = spot;
                }
                int n = PlatePieces(InfantryArchetype.Sentry, spots[slot].Foot, spots[slot].Fwd, scale, platePieces);
                for (int k = 0; k < n; k++) batch.Add(platePieces[k]);
                men++;
            }
            if (batch.Count == 0) return;
            if (!PlateMask)
            {
                Flush(cube, new RenderParams(dirtMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On });
                return;
            }
            if (maskPlateMat == null) { maskPlateMat = Unlit(new Color(1f, 0f, 1f)); maskBarMat = Unlit(new Color(0f, 1f, 1f)); }
            Flush(cube, new RenderParams(maskPlateMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
            batch.Clear();
            for (int slot = 0; slot < spots.Length; slot++)
            {
                if (!spots[slot].Set) continue;
                ViewNow(out _, out _, out float scale);
                batch.Add(PlateBar(spots[slot].Foot, spots[slot].Fwd, scale));
            }
            if (batch.Count > 0) Flush(cube, new RenderParams(maskBarMat) { worldBounds = bounds, shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.Off });
        }
    }
}
