// Phase: B1 (implemented for P0 needs; nav/LoS/cover layers grow with A1/A2)
// Renders every alive unit as an instanced capsule coloured by team, draws flow-field arrows for the cells around
// the mouse (editor gizmo lines), shows tick/hash/silver/stall stats, and maps number keys 1-5 to DeployUnit and
// F1-F3 to overlay layers. This is Track A's main debugging tool.
using System.Collections.Generic;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.InputSystem;
using TW.Sim;
using TW.Sim.Nav;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed class DebugOverlay : MonoBehaviour
    {
        public SimHost Host;
        public bool ShowFlowField = true;
        public bool ShowStats = true;
        public int FlowArrowRadiusCells = 12;

        Mesh capsule;
        Material matA, matB;
        readonly List<Matrix4x4> batch = new List<Matrix4x4>(1023);
        Matrix4x4[] batchArray = new Matrix4x4[1023];

        void Start()
        {
            capsule = Resources.GetBuiltinResource<Mesh>("Capsule.fbx");
            var shader = Shader.Find("Universal Render Pipeline/Lit");
            if (shader == null) shader = Shader.Find("Standard");
            matA = new Material(shader) { enableInstancing = true, color = new Color(0.55f, 0.45f, 0.25f) };
            matB = new Material(shader) { enableInstancing = true, color = new Color(0.35f, 0.4f, 0.5f) };
        }

        void Update()
        {
            if (Host == null || Host.Presenter == null) return;
            HandleInput();
            DrawUnits();
            if (ShowFlowField) DrawFlowField();
        }

        void HandleInput()
        {
            var kb = Keyboard.current;
            if (kb == null) return;
            uint t = Host.Local.World.Tick;
            if (kb.digit1Key.wasPressedThisFrame) Host.Issue(SimCommand.Deploy(t, 0, 0));
            if (kb.digit2Key.wasPressedThisFrame) Host.Issue(SimCommand.Deploy(t, 0, 1));
            if (kb.digit3Key.wasPressedThisFrame) Host.Issue(SimCommand.Deploy(t, 0, 2));
            if (kb.digit4Key.wasPressedThisFrame) Host.Issue(SimCommand.Deploy(t, 0, 3));
            if (kb.digit5Key.wasPressedThisFrame) Host.Issue(SimCommand.Deploy(t, 0, 4));
            if (kb.f1Key.wasPressedThisFrame) ShowFlowField = !ShowFlowField;
            if (kb.f2Key.wasPressedThisFrame) ShowStats = !ShowStats;
        }

        void DrawUnits()
        {
            var p = Host.Presenter;
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));
            var rpA = new RenderParams(matA) { shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On, worldBounds = bounds };
            var rpB = new RenderParams(matB) { shadowCastingMode = UnityEngine.Rendering.ShadowCastingMode.On, worldBounds = bounds };
            var hf = Host.Local.Map.Height;
            for (int team = 0; team < 2; team++)
            {
                batch.Clear();
                for (int i = 0; i < p.PoseCount; i++)
                {
                    var pose = p.Poses[i];
                    if (pose.Team != team) continue;
                    float y = hf.Sample(pose.Pos.x, pose.Pos.z);
                    bool low = pose.AnimRow == (ushort)AnimRow.ProneCrawl || pose.AnimRow == (ushort)AnimRow.PinnedLoop;
                    float scaleY = low ? 0.35f : pose.AnimRow == (ushort)AnimRow.CrouchWalk ? 0.6f : 0.9f;
                    batch.Add(Matrix4x4.TRS(new Vector3(pose.Pos.x, y + scaleY, pose.Pos.z), Quaternion.Euler(0f, (float)pose.Yaw * Mathf.Rad2Deg, 0f), new Vector3(0.6f, scaleY, 0.6f)));
                    if (batch.Count == 1023) Flush(team == 0 ? rpA : rpB);
                }
                if (batch.Count > 0) Flush(team == 0 ? rpA : rpB);
            }
        }

        void Flush(RenderParams rp)
        {
            batch.CopyTo(batchArray, 0);
            Graphics.RenderMeshInstanced(rp, capsule, 0, batchArray, batch.Count);
            batch.Clear();
        }

        void DrawFlowField()
        {
            var cam = UnityEngine.Camera.main;
            var mouse = Mouse.current;
            if (cam == null || mouse == null) return;
            var ray = cam.ScreenPointToRay(mouse.position.ReadValue());
            if (Mathf.Abs(ray.direction.y) < 1e-4f) return;
            float t = -ray.origin.y / ray.direction.y;
            if (t < 0f) return;
            Vector3 hit = ray.origin + ray.direction * t;
            var map = Host.Local.Map;
            var field = Host.Local.Movement.FieldTeam0;
            var hf = map.Height;
            var c = map.NavCellOf(new float3(hit.x, 0f, hit.z));
            for (int dz = -FlowArrowRadiusCells; dz <= FlowArrowRadiusCells; dz++)
            for (int dx = -FlowArrowRadiusCells; dx <= FlowArrowRadiusCells; dx++)
            {
                int x = c.x + dx, z = c.y + dz;
                if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                int idx = map.NavIndex(x, z);
                float2 d = field.DirectionAt(idx);
                float3 center = map.NavCellCenter(idx);
                float y = hf.Sample(center.x, center.z) + 0.2f;
                var layer = (TW.Sim.Terrain.NavLayer)map.NavLayers[idx];
                Color col = (layer & TW.Sim.Terrain.NavLayer.Link) != 0 ? Color.yellow : (layer & TW.Sim.Terrain.NavLayer.Trench) != 0 ? Color.cyan : Color.green;
                if (math.lengthsq(d) == 0f) { Debug.DrawLine(new Vector3(center.x - 0.3f, y, center.z), new Vector3(center.x + 0.3f, y, center.z), Color.red); continue; }
                Debug.DrawRay(new Vector3(center.x, y, center.z), new Vector3(d.x, 0f, d.y) * 0.8f, col);
            }
        }

        void OnGUI()
        {
            if (!ShowStats || Host == null || Host.Local == null) return;
            var w = Host.Local.World;
            GUI.Label(new Rect(10, 10, 700, 160),
                $"tick {w.Tick}  hash {w.LastHash:X16}  alive {w.AliveCount}  silver P0 {w.Silver[0]} P1 {w.Silver[1]}\n" +
                $"stall {Host.LocalDriver.StallTicks}  desync {(Host.Desync ? "YES" : "no")}  events/frame {Host.Events.Frame.Count}  overrun {Host.Events.OverrunTotal}\n" +
                "1-5 deploy slot   F1 flow field   F2 stats   WASD pan   wheel zoom   Q/E rotate");
        }
    }
}
