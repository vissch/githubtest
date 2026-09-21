// Phase: B3 (implemented with a placeholder soldier; C2 replaces the asset, not the renderer)
// Every infantryman on the map in one draw: a Burst job turns SimPresenter.Poses into 32-byte instance records
// (position on the terrain, yaw, anim row and phase, team), one GraphicsBuffer upload, one
// Graphics.RenderMeshIndirect. Sized for the 3,000-unit ceiling. Two tiers, chosen by camera zoom because the
// tactical camera is close to orthographic and every unit has the same size on screen: frame-blended below
// LodTiers.BlendZoom, nearest-frame above it. The soldier is ~260 vertices, so 3,000 of them are under a million
// vertices a frame and impostors are not needed (docs/11 B3). Vehicles are instanced boxes until C2.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Sim.Terrain;
using TW.Presentation;

namespace TW.Presentation.Units
{
    public struct VatInstance { public float3 Pos; public float Yaw, AnimRow, AnimT, Tint, Scale; }

    public static class LodTiers
    {
        /// <summary>TacticalCamera zoom (metres of ground across the view) above which frames are no longer blended.</summary>
        public const float BlendZoom = 120f;
    }

    public sealed class VATRenderer : MonoBehaviour
    {
        public SimHost Host;
        public float UnitScale = 1.5f;   // a little over life size so a man reads at the default zoom
        public bool CastShadows = true;

        /// <summary>Infantry drawn last frame (for the stats overlay and tests).</summary>
        public int DrawnInfantry { get; private set; }
        public int DrawnVehicles { get; private set; }
        public bool Ready => material != null;

        VatAsset asset;
        Material material, tankMatA, tankMatB;
        Mesh tankMesh;
        GraphicsBuffer instanceBuffer, rowBuffer, argsBuffer;
        NativeArray<VatInstance> instances;
        NativeArray<float4> vehicles;   // xyz + yaw; w sign carries the team
        NativeArray<int> counts;
        readonly GraphicsBuffer.IndirectDrawIndexedArgs[] args = new GraphicsBuffer.IndirectDrawIndexedArgs[1];
        readonly Matrix4x4[] tankBatchA = new Matrix4x4[256], tankBatchB = new Matrix4x4[256];
        static readonly int LerpId = Shader.PropertyToID("_Lerp");

        void Start()
        {
            var shader = Shader.Find("TW/VAT Infantry (URP)");
            if (shader == null || !SystemInfo.supportsComputeShaders) { Debug.LogWarning("VATRenderer: VAT shader unavailable, units fall back to capsules"); return; }
            asset = ProceduralSoldier.Build();
            material = new Material(shader) { hideFlags = HideFlags.HideAndDontSave };
            material.SetTexture("_PosTex", asset.Positions);
            material.SetTexture("_NrmTex", asset.Normals);
            material.SetFloat("_VertexCount", asset.Mesh.vertexCount);
            material.SetFloat("_TotalFrames", asset.TotalFrames);

            rowBuffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, asset.RowTable.Length, 8);
            rowBuffer.SetData(asset.RowTable);
            argsBuffer = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, 1, GraphicsBuffer.IndirectDrawIndexedArgs.size);

            var lit = Shader.Find("Universal Render Pipeline/Lit");
            tankMatA = new Material(lit) { enableInstancing = true, color = new Color(0.36f, 0.33f, 0.22f) };
            tankMatB = new Material(lit) { enableInstancing = true, color = new Color(0.30f, 0.33f, 0.36f) };
            tankMesh = BuildTank();
        }

        void EnsureCapacity(int maxSlots)
        {
            if (instances.IsCreated && instances.Length >= maxSlots) return;
            Release();
            instances = new NativeArray<VatInstance>(maxSlots, Allocator.Persistent);
            vehicles = new NativeArray<float4>(maxSlots, Allocator.Persistent);
            counts = new NativeArray<int>(2, Allocator.Persistent);
            instanceBuffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, maxSlots, 32);
        }

        void LateUpdate()   // after SimHost.Update has interpolated this frame's poses
        {
            if (material == null || Host == null || Host.Presenter == null || Host.Local == null) return;
            var presenter = Host.Presenter;
            EnsureCapacity(presenter.Poses.Length);
            new FillJob
            {
                Poses = presenter.Poses, PoseCount = presenter.PoseCount, Height = Host.Local.Map.Height, Scale = UnitScale,
                Instances = instances, Vehicles = vehicles, Counts = counts,
            }.Run();
            DrawnInfantry = counts[0];
            DrawnVehicles = counts[1];

            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));
            if (DrawnInfantry > 0)
            {
                instanceBuffer.SetData(instances, 0, 0, DrawnInfantry);
                args[0] = new GraphicsBuffer.IndirectDrawIndexedArgs
                {
                    indexCountPerInstance = asset.Mesh.GetIndexCount(0), instanceCount = (uint)DrawnInfantry,
                    startIndex = asset.Mesh.GetIndexStart(0), baseVertexIndex = asset.Mesh.GetBaseVertex(0), startInstance = 0,
                };
                argsBuffer.SetData(args);
                var cam = Camera.main;
                float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var z) ? z.CurrentZoom : 0f;
                material.SetFloat(LerpId, zoom > LodTiers.BlendZoom ? 0f : 1f);
                var rp = new RenderParams(material)
                {
                    worldBounds = bounds, shadowCastingMode = CastShadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true,
                    matProps = Props(),
                };
                Graphics.RenderMeshIndirect(rp, asset.Mesh, argsBuffer, 1);
            }
            if (DrawnVehicles > 0) DrawVehicles(bounds);
        }

        MaterialPropertyBlock props;
        MaterialPropertyBlock Props()
        {
            if (props == null) props = new MaterialPropertyBlock();
            props.SetBuffer("_Instances", instanceBuffer);
            props.SetBuffer("_RowTable", rowBuffer);
            return props;
        }

        void DrawVehicles(Bounds bounds)
        {
            int a = 0, b = 0;
            for (int i = 0; i < DrawnVehicles; i++)
            {
                float4 v = vehicles[i];
                bool teamB = v.w < 0f;
                float yaw = math.abs(v.w) - 10f;
                var m = Matrix4x4.TRS(new Vector3(v.x, v.y, v.z), Quaternion.Euler(0f, yaw * Mathf.Rad2Deg, 0f), Vector3.one);
                if (teamB) { if (b < tankBatchB.Length) tankBatchB[b++] = m; }
                else if (a < tankBatchA.Length) tankBatchA[a++] = m;
            }
            if (a > 0) Graphics.RenderMeshInstanced(new RenderParams(tankMatA) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On }, tankMesh, 0, tankBatchA, a);
            if (b > 0) Graphics.RenderMeshInstanced(new RenderParams(tankMatB) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On }, tankMesh, 0, tankBatchB, b);
        }

        [BurstCompile]
        struct FillJob : IJob
        {
            [ReadOnly] public NativeArray<UnitPose> Poses;
            [ReadOnly] public Heightfield Height;
            public int PoseCount;
            public float Scale;
            public NativeArray<VatInstance> Instances;
            public NativeArray<float4> Vehicles;
            public NativeArray<int> Counts;

            public void Execute()
            {
                int n = 0, v = 0;
                for (int i = 0; i < PoseCount; i++)
                {
                    var p = Poses[i];
                    float y = Height.Sample(p.Pos.x, p.Pos.z);
                    if ((p.Flags & (byte)UnitFlags.Vehicle) != 0)
                    {
                        // yaw is within (-2pi, 2pi); +10 keeps it positive so the sign is free for the team
                        Vehicles[v++] = new float4(p.Pos.x, y, p.Pos.z, (p.Yaw + 10f) * (p.Team == 0 ? 1f : -1f));
                        continue;
                    }
                    Instances[n++] = new VatInstance
                    {
                        Pos = new float3(p.Pos.x, y, p.Pos.z), Yaw = p.Yaw, AnimRow = p.AnimRow, AnimT = p.AnimT, Tint = p.Team, Scale = Scale,
                    };
                }
                Counts[0] = n; Counts[1] = v;
            }
        }

        /// <summary>A rhomboid hull, two sponsons: enough to read as a Mark IV from above.</summary>
        static Mesh BuildTank()
        {
            var parts = new[]
            {
                new Bounds(new Vector3(0f, 1.25f, 0f), new Vector3(2.0f, 1.5f, 7.6f)),      // hull
                new Bounds(new Vector3(-1.45f, 1.1f, 0f), new Vector3(0.9f, 2.2f, 8.0f)),   // tracks
                new Bounds(new Vector3(1.45f, 1.1f, 0f), new Vector3(0.9f, 2.2f, 8.0f)),
                new Bounds(new Vector3(-2.2f, 1.3f, 0.4f), new Vector3(0.7f, 1.0f, 2.2f)),  // sponsons
                new Bounds(new Vector3(2.2f, 1.3f, 0.4f), new Vector3(0.7f, 1.0f, 2.2f)),
                new Bounds(new Vector3(0f, 2.25f, 1.2f), new Vector3(1.4f, 0.5f, 1.6f)),    // cab
            };
            var cube = Resources.GetBuiltinResource<Mesh>("Cube.fbx");
            var combine = new CombineInstance[parts.Length];
            for (int i = 0; i < parts.Length; i++)
                combine[i] = new CombineInstance { mesh = cube, transform = Matrix4x4.TRS(parts[i].center, Quaternion.identity, parts[i].size) };
            var mesh = new Mesh { name = "PlaceholderTank", hideFlags = HideFlags.HideAndDontSave };
            mesh.CombineMeshes(combine, true, true);
            return mesh;
        }

        void Release()
        {
            if (instances.IsCreated) instances.Dispose();
            if (vehicles.IsCreated) vehicles.Dispose();
            if (counts.IsCreated) counts.Dispose();
            instanceBuffer?.Dispose(); instanceBuffer = null;
        }

        void OnDestroy()
        {
            Release();
            rowBuffer?.Dispose(); argsBuffer?.Dispose();
        }
    }
}
