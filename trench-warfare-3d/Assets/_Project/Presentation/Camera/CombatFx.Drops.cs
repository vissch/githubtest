// Phase: SHOW (2026-10-01) — the paratroop drop's picture. The sim says a drop is coming (DropInbound: a = player,
// b = men, pos = the point, scalar = seconds to landing) and then that each man is down (DropLanded); nothing drew
// either, so eight men simply appeared in the mud. Now the aircraft crosses the point as they jump, a canopy comes
// down for each man onto the very spot the sim will land him (DropSpot repeats OffMapAbilitySystem.Land's die, pinned by
// ParaDropPictureTests), swinging on its cords, and folds onto the ground beside him as he lands.
// Timed by the sim's clock (SimNow), so a paused or slowed match holds the canopies in the air.
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>Seconds a canopy takes to come down, the height it opens at, the seconds it takes to fold on the
        /// ground, its radius in metres, and the seconds between one man leaving the aircraft and the next.</summary>
        public const float DropFall = 3.5f, DropHeight = 24f, DropFold = 1.6f, CanopyRadius = 1.9f, DropStagger = 0.06f;
        /// <summary>How far under the silk's top the man hangs, in canopy radii: his lines are as long as the canopy is wide.</summary>
        public const float CanopyDrop = 2.9f;

        struct Canopy { public Vector3 Land, Drift; public float JumpAt, Fall, Phase; public int Team; }
        readonly List<Canopy> canopies = new List<Canopy>(16);
        Mesh canopyMesh, stripeMesh, cordMesh, hangMesh;
        Material silkMat, cordMat, manMat;
        readonly Material[] stripeMat = new Material[2];   // every other gore in the side's colour: whose drop it is, and never a puddle

        /// <summary>Where the sim will land man <paramref name="k"/> of a drop called at <paramref name="point"/> that
        /// lands on <paramref name="landTick"/>: OffMapAbilitySystem.Land's own die and its own open-ground search.</summary>
        public static float3 DropSpot(SimWorld w, TW.Sim.Terrain.MapData map, int player, int k, float3 point, uint landTick, float radius)
        {
            var rng = SimRandom.For(w.Config.Seed, landTick, SimRandom.SystemId.AirDrop, (uint)(player * 64 + k));
            float angle = rng.NextFloat(0f, 2f * math.PI);
            float r = radius * SimMath.Sqrt(rng.NextFloat());
            float3 at = w.ClampToMap(point + new float3(SimMath.Cos(angle) * r, 0f, SimMath.Sin(angle) * r));
            return map != null ? VehicleModulesSystem.OpenGround(map, at) : at;
        }

        /// <summary>A canopy's height in metres at <paramref name="t"/> of its fall (0 as it opens .. 1 on the ground).
        /// It drops fast before the silk fills and slower under it.</summary>
        public static float CanopyHeight(float t)
        {
            t = Mathf.Clamp01(t);
            return DropHeight * (1f - t) * (1f - 0.35f * t);
        }

        void DropInbound(in SimEvent e)
        {
            if (Host == null || Host.Local == null || !OffMapAbilitySystem.TryGetStats((int)OffMapAbilityId.ParaDrop, out var stats)) return;
            var w = Host.Local.World;
            float tick = w.Config.TickSeconds;
            float fall = Mathf.Min(DropFall, e.Scalar);
            Vector3 dir = e.A == 0 ? Vector3.forward : Vector3.back;   // the aircraft comes from its own side of the field
            Vector3 point = (Vector3)e.Pos;
            float lands = e.Tick * tick + e.Scalar;
            // over the point as the first man jumps: a flyover is over Start at Fired + Warm
            flyovers.Add(new Flyover { Start = point, Dir = dir, Length = 0f, Fired = e.Tick * tick, Warm = Mathf.Max(0f, e.Scalar - fall) });
            uint landTick = e.Tick + (uint)stats.WarmupTicks;
            int men = Mathf.Clamp(e.B, 0, 32);
            for (int k = 0; k < men; k++)
            {
                Vector3 land = (Vector3)DropSpot(w, Host.Local.Map, e.A, k, e.Pos, landTick, stats.Radius);
                land.y = RenderGround.Sample(Host.Local.Map, land.x, land.z);
                // they leave the aircraft one after another, each later man with less air under him, and the wind
                // carries each along its heading in to his spot
                float late = k * DropStagger;
                canopies.Add(new Canopy { Land = land, Drift = -dir * (10f + k * 1.5f), JumpAt = lands - fall + late, Fall = Mathf.Max(0.5f, fall - late), Phase = k * 2.399963f, Team = e.A == 1 ? 1 : 0 });
            }
        }

        void DropLanded(in SimEvent e)
        {
            if (books == null || !books.Ready || Host == null || Host.Local == null) return;
            Vector3 at = (Vector3)e.Pos; at.y = RenderGround.Sample(Host.Local.Map, at.x, at.z) + 0.3f;
            books.Add(FlipbookFx.Book.Puff, at, 1.6f, 0.8f, velocity: Vector3.up * 0.6f, grow: 1.5f, alpha: 0.45f);
        }

        /// <summary>The canopies in the air and the ones folding on the ground.</summary>
        void DrawCanopies(float simNow)
        {
            if (canopies.Count == 0) return;
            if (canopyMesh == null)
            {
                canopyMesh = BuildCanopy(0); stripeMesh = BuildCanopy(1); cordMesh = BuildCords(); hangMesh = BuildHanging();
                // the silk is seen from above and from under: one skin drawn on both faces (the toon outline pass
                // would paint a two-skinned dome black), pale enough to find against a night sky. Every other gore is
                // in the side's colour (critic, 2026-10-01: all pale, a canopy read as a mushroom in the air and as one
                // more puddle once it was down)
                silkMat = Silk(new Color(0.84f, 0.82f, 0.70f));
                stripeMat[0] = Silk(Color.Lerp(TankRenderer.TeamA, Color.black, 0.25f));
                stripeMat[1] = Silk(Color.Lerp(TankRenderer.TeamB, Color.black, 0.15f));
                cordMat = Painted(new Color(0.55f, 0.53f, 0.47f), 0f);
                manMat = Painted(new Color(0.42f, 0.38f, 0.26f), 1.2f);
            }
            float size = units != null ? units.UnitScale : 1f;
            for (int k = canopies.Count - 1; k >= 0; k--)
            {
                var c = canopies[k];
                float t = (simNow - c.JumpAt) / c.Fall;
                if (t < 0f) continue;                      // still in the aircraft
                float down = (simNow - c.JumpAt - c.Fall) / DropFold;
                if (down >= 1f) { canopies.RemoveAt(k); continue; }
                float open = Mathf.Lerp(0.15f, 1f, Mathf.Clamp01(t * 5f));   // the silk fills in the first fifth
                float swing = Mathf.Sin(simNow * 1.9f + c.Phase) * 9f;
                float left = 1f - Mathf.Clamp01(t);
                float r = CanopyRadius * size;
                Vector3 wind = c.Drift.normalized;
                if (down <= 0f)
                {
                    // the man hangs a canopy's width below the silk, and both swing about the silk
                    Vector3 man = c.Land + c.Drift * (left * left) + Vector3.up * CanopyHeight(t);
                    var lean = Quaternion.AngleAxis(swing, Vector3.forward) * Quaternion.AngleAxis(Mathf.Cos(simNow * 1.3f + c.Phase) * 6f, Vector3.right);
                    Vector3 top = man + lean * (Vector3.up * (r * CanopyDrop));
                    var silk = Matrix4x4.TRS(top, lean, new Vector3(r * open, r * (0.6f + 0.4f * open), r * open));
                    Graphics.DrawMesh(canopyMesh, silk, silkMat, 0);
                    Graphics.DrawMesh(stripeMesh, silk, stripeMat[c.Team], 0);
                    Graphics.DrawMesh(cordMesh, Matrix4x4.TRS(top, lean, new Vector3(r * open, r * CanopyDrop, r * open)), cordMat, 0);
                    Graphics.DrawMesh(hangMesh, Matrix4x4.TRS(man, lean, Vector3.one * size), manMat, 0);
                }
                else
                {
                    // down: the silk spills downwind of him, settles flat, and is gathered in
                    float d = Mathf.Clamp01(down * 2.2f);
                    Vector3 spill = c.Land - wind * (r * 1.3f * d) + Vector3.up * (r * CanopyDrop * (1f - d) * (1f - d) + 0.62f * r * Mathf.Lerp(1f, 0.22f, d) + 0.05f);
                    var lay = Quaternion.AngleAxis(Mathf.Lerp(swing, 20f, d), Vector3.Cross(Vector3.up, -wind));
                    float gather = down > 0.7f ? (1f - down) / 0.3f : 1f;
                    // it does not settle round: it spills long downwind and narrow across, a heap and not a disc
                    var heap = Matrix4x4.TRS(spill, lay * Quaternion.AngleAxis(c.Phase * Mathf.Rad2Deg, Vector3.up), new Vector3(r * (1f + 0.45f * d), r * Mathf.Lerp(1f, 0.3f, d), r * (1f - 0.35f * d)) * gather);
                    Graphics.DrawMesh(canopyMesh, heap, silkMat, 0);
                    Graphics.DrawMesh(stripeMesh, heap, stripeMat[c.Team], 0);
                }
            }
        }

        /// <summary>A parachute's silk (every other gore, by parity: the pale ones or the side's): eight gores of a shallow dome of radius 1, its top at y 0 and its hem at
        /// y -0.62, the hem riding up between the cords. One skin: its material draws both faces.</summary>
        static Mesh BuildCanopy(int parity)
        {
            const int gores = 8, rings = 3, cuts = 2;   // two segments a gore, so the hem can rise between the cords
            int sides = gores * cuts;
            var vertices = new List<Vector3>(); var triangles = new List<int>();
            for (int ring = 0; ring <= rings; ring++)
            for (int side = 0; side <= sides; side++)
            {
                float a = side * Mathf.PI * 2f / sides, b = ring * (Mathf.PI * 0.5f) / rings;
                float scallop = ring == rings && side % cuts != 0 ? 0.12f : 0f;
                vertices.Add(new Vector3(Mathf.Cos(a) * Mathf.Sin(b), Mathf.Cos(b) * 0.62f - 0.62f + scallop, Mathf.Sin(a) * Mathf.Sin(b)));
                if (ring == rings || side == sides || ((side / cuts) & 1) != parity) continue;   // every other gore
                int i = ring * (sides + 1) + side;
                triangles.Add(i); triangles.Add(i + 1); triangles.Add(i + sides + 1);
                triangles.Add(i + 1); triangles.Add(i + sides + 2); triangles.Add(i + sides + 1);
            }
            var mesh = new Mesh { name = parity == 0 ? "Parachute silk" : "Parachute silk, the side's gores", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetTriangles(triangles, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds(); return mesh;
        }

        static Material Silk(Color colour)
        {
            var lit = Shader.Find("Universal Render Pipeline/Lit"); if (lit == null) lit = Shader.Find("Standard");
            var m = new Material(lit) { enableInstancing = true, hideFlags = HideFlags.HideAndDontSave };
            m.SetColor("_BaseColor", colour); m.SetFloat("_Cull", 0f); m.SetFloat("_Smoothness", 0.1f);
            m.EnableKeyword("_EMISSION"); m.SetColor("_EmissionColor", colour * 0.3f);
            return m;
        }

        /// <summary>The cords: eight thin blades from the hem to the man. Scaled apart from the silk (x and z by its
        /// radius, y by the drop from its top to the man), so y runs from the hem down to his shoulders, a man's height short of -1.</summary>
        static Mesh BuildCords()
        {
            var vertices = new List<Vector3>(); var triangles = new List<int>();
            for (int g = 0; g < 8; g++)
            {
                float a = g * Mathf.PI * 0.25f;
                var hem = new Vector3(Mathf.Cos(a), -0.62f / CanopyDrop, Mathf.Sin(a));
                var side = new Vector3(-Mathf.Sin(a), 0f, Mathf.Cos(a)) * 0.01f;
                int i = vertices.Count;
                vertices.Add(hem - side); vertices.Add(hem + side); vertices.Add(new Vector3(0f, -1f + 1.5f / (CanopyDrop * CanopyRadius), 0f));
                triangles.Add(i); triangles.Add(i + 1); triangles.Add(i + 2);
                triangles.Add(i); triangles.Add(i + 2); triangles.Add(i + 1);
            }
            var mesh = new Mesh { name = "Parachute cords", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetTriangles(triangles, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds(); return mesh;
        }

        /// <summary>The man under the silk, as far as the tactical camera reads him: a dark body, head and legs, his
        /// feet at y 0.</summary>
        static Mesh BuildHanging()
        {
            var vertices = new List<Vector3>(); var triangles = new List<int>();
            void Box(Vector3 lo, Vector3 hi)
            {
                int i = vertices.Count;
                for (int c = 0; c < 8; c++) vertices.Add(new Vector3((c & 1) == 0 ? lo.x : hi.x, (c & 2) == 0 ? lo.y : hi.y, (c & 4) == 0 ? lo.z : hi.z));
                int[] q = { 0, 2, 3, 1, 4, 5, 7, 6, 0, 1, 5, 4, 2, 6, 7, 3, 0, 4, 6, 2, 1, 3, 7, 5 };
                for (int f = 0; f < 24; f += 4)
                {
                    triangles.Add(i + q[f]); triangles.Add(i + q[f + 1]); triangles.Add(i + q[f + 2]);
                    triangles.Add(i + q[f]); triangles.Add(i + q[f + 2]); triangles.Add(i + q[f + 3]);
                }
            }
            Box(new Vector3(-0.22f, 0.75f, -0.14f), new Vector3(0.22f, 1.45f, 0.14f));    // body and pack
            Box(new Vector3(-0.12f, 1.45f, -0.12f), new Vector3(0.12f, 1.72f, 0.12f));    // head
            Box(new Vector3(-0.2f, 0f, -0.1f), new Vector3(-0.04f, 0.75f, 0.1f));         // legs
            Box(new Vector3(0.04f, 0f, -0.1f), new Vector3(0.2f, 0.75f, 0.1f));
            var mesh = new Mesh { name = "Hanging paratrooper", hideFlags = HideFlags.HideAndDontSave };
            mesh.SetVertices(vertices); mesh.SetTriangles(triangles, 0); mesh.RecalculateNormals(); mesh.RecalculateBounds(); return mesh;
        }
    }
}
