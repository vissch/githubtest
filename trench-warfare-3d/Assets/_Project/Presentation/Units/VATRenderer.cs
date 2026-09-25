// Phase: B3 (implemented), C1 (clip atlas), C2 (figures)
// Every infantryman on the map in a handful of draws: a Burst job turns SimPresenter.Poses into 48-byte instance
// records (position on the terrain, yaw, the clip and phase, the clip fading out, team), sorted by figure, one
// GraphicsBuffer upload, one Graphics.RenderMeshIndirect per figure. Sized for the 3,000-unit ceiling. Two tiers,
// chosen by camera zoom because the tactical camera is close to orthographic and every unit has the same size on
// screen: frame-blended below LodTiers.BlendZoom, nearest-frame above it.
// Men outside the camera frustum are dropped in the fill job, and the draw is held to LodTiers.VertexBudget: when
// the men on screen times the mesh's vertices (twice with shadows) exceed it, shadows go first.
// The standard view is flat (25 degrees), so one frame holds men 40 m and 400 m away: beyond LodDistance a man is
// drawn with the far model in its own indirect draw from the same instance buffer (near records from the front,
// far records from the back). The far tier is the 264-vertex box soldier.
// Figures: the baked atlases in Resources/Units (Figure<Name>, VATBaker.Figures order): the Soldier for the
// rifleman, assault and machine-gunner, the hooded Sniper for the sniper. Without any bake the box soldier is the
// only tier. Rows: the controller writes a Clip per man; a clip atlas plays it as is, the box soldier maps it to one
// of its 18 procedural rows (Clips.Table[].Fallback).
using System.Collections.Generic;
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
    /// <summary>Pad: the limbs a dead man has lost and the grime on him, packed by VatPad.</summary>
    public struct VatInstance { public float3 Pos; public float Yaw, AnimRow, AnimT, Tint, Scale, PrevRow, PrevT, Blend, Pad; }

    /// <summary>
    /// What a record's Pad carries: bits 0-5 the limbs a dead man has lost (a bit per limb id, AddFallen's gib; none on a
    /// living man), bits 6-13 his grime, 0-255 (the mud and soot of the bursts near him, AnimationController.Grime), bits
    /// 14-21 a seed of his own, so the blotches of it land differently on each man. A float holds every integer to 2^24
    /// exactly, so VAT_URP reads them back as they went in.
    /// </summary>
    public static class VatPad
    {
        public const int LimbBits = 63;
        public static float Pack(int lost, float grime, int seed = 0) => (lost & LimbBits) | (math.clamp((int)math.round(grime * 255f), 0, 255) << 6) | ((seed & 255) << 14);
        public static int Lost(float pad) => (int)(pad + 0.5f) & LimbBits;
        public static float Grime(float pad) => (((int)(pad + 0.5f)) >> 6 & 255) / 255f;
        public static int Seed(float pad) => ((int)(pad + 0.5f)) >> 14 & 255;
    }

    public static class LodTiers
    {
        /// <summary>TacticalCamera zoom (metres of ground across the view) above which frames are no longer blended.</summary>
        public const float BlendZoom = 120f;
        /// <summary>Unit vertices per frame, shadow pass included. Sized for a GTX 1050 next to a heavy environment (estimate, unmeasured).</summary>
        public const int VertexBudget = 1500000;
        /// <summary>Metres around a man that still count as on screen (his body, his shadow).</summary>
        public const float CullRadius = 3f;
    }

    public sealed class VATRenderer : MonoBehaviour
    {
        public SimHost Host;
        /// <summary>A man's drawn size, as a multiple of his 1.78 m.
        ///
        /// 1.5 drew him 2.67 m tall, which was taller than the 2 m SeparationJob keeps between two men in a
        /// trench - a full garrison stood inside itself. A quarter off brings him to 2.0 m, so the spacing the
        /// simulation keeps is now the spacing you see, and the machines read against him as machines.</summary>
        public float UnitScale = FigureMetrics.UnitScale;   // the picker reads FigureMetrics too: change it there
        [Tooltip("Men grow with the zoom so they stay readable from far out: x1 up to this zoom, then in proportion, capped at MaxGrow.")]
        public float GrowFromZoom = FigureMetrics.GrowFromZoom;
        public float MaxGrow = FigureMetrics.MaxGrow;
        public bool CastShadows = true;
        [Tooltip("Metres from the camera beyond which the far model is used (a man is under about 40 pixels tall there).")]
        public float LodDistance = 170f;

        /// <summary>Infantry drawn last frame (for the stats overlay and tests).</summary>
        public int DrawnInfantry { get; private set; }
        public int DrawnVehicles { get; private set; }
        public int DrawnNear { get; private set; }
        public int DrawnFar { get; private set; }
        /// <summary>Unit vertices submitted last frame, shadow pass included.</summary>
        public long VerticesThisFrame { get; private set; }
        public bool ShadowsThisFrame { get; private set; }
        public bool Ready => figures != null && figures.Length > 0;

        /// <summary>The figure names, in archetype-map order; the assets are Resources/Units/Figure&lt;Name&gt;.</summary>
        public static readonly string[] FigureNames = { "Soldier", "Sniper" };
        /// <summary>Which figure each archetype is drawn with (rifleman, assault, machine-gunner: the soldier; sniper: the hooded man).</summary>
        public static int FigureOfArchetype(int archetype) => archetype == 3 ? 1 : 0;

        sealed class Figure
        {
            public VatAsset Asset; public Material Material, Fallen; public GraphicsBuffer Rows; public int Near, Start;
            public MaterialPropertyBlock Props, FallenProps;
        }
        Figure[] figures;            // the near tier(s); one box-soldier figure when nothing is baked
        Figure far;                  // the box soldier beyond LodDistance (null when nothing is baked: the box is then the near tier)
        bool clipAtlas;
        Material tankMatA, tankMatB;
        Mesh tankMesh;
        GraphicsBuffer instanceBuffer, argsBuffer;
        NativeArray<VatInstance> instances, sorted;
        NativeArray<byte> figureOf;
        NativeArray<float4> vehicles;   // xyz + yaw; w sign carries the team
        NativeArray<int> counts;
        NativeArray<float4> planes;
        NativeArray<ushort> nearRowOf, farRowOf;   // Clip -> row in the near / far atlas
        NativeArray<float> spare;                   // stands in for the controller's arrays when there is no controller
        readonly Plane[] frustum = new Plane[6];
        GraphicsBuffer.IndirectDrawIndexedArgs[] args;
        readonly Matrix4x4[] tankBatchA = new Matrix4x4[256], tankBatchB = new Matrix4x4[256];
        static readonly int LerpId = Shader.PropertyToID("_Lerp"), PosMinId = Shader.PropertyToID("_PosMin"), PosSizeId = Shader.PropertyToID("_PosSize"), CullId = Shader.PropertyToID("_Cull");
        /// <summary>The atlas the near tier plays for the first figure: the baked clips when TW/VAT/Bake Infantry has run, else the box soldier.</summary>
        public VatAsset NearAsset => figures != null && figures.Length > 0 ? figures[0].Asset : null;
        public VatAsset FigureAsset(int figure) => figures != null && figures.Length > 0 ? figures[math.clamp(figure, 0, figures.Length - 1)].Asset : null;
        public bool ClipAtlas => clipAtlas;

        static Figure Make(Shader shader, VatAsset a)
        {
            var m = new Material(shader) { hideFlags = HideFlags.HideAndDontSave };
            m.SetTexture("_PosTex", a.Positions); m.SetTexture("_NrmTex", a.Normals);
            m.SetFloat("_VertexCount", a.Mesh.vertexCount); m.SetFloat("_TotalFrames", a.TotalFrames);
            m.SetVector(PosMinId, a.PosMin); m.SetVector(PosSizeId, a.PosSize);
            var rows = new GraphicsBuffer(GraphicsBuffer.Target.Structured, a.RowTable.Length, 8);
            rows.SetData(a.RowTable);
            // the fallen are drawn from the same atlas with both sides: where a shell took a limb off the inside shows (dark),
            // not the field through a hollow shell. Cull is render state, not a property block value, hence a second material.
            var fallen = new Material(m) { hideFlags = HideFlags.HideAndDontSave };
            fallen.SetFloat(CullId, (float)CullMode.Off);
            fallen.EnableKeyword("_TW_LIMBCUT");   // and only they can have lost a limb: the living draw with no clip() (VAT_URP)
            // new Material(m) copies only what the shader declares: _PosMin/_PosSize are not in its Properties block, and
            // without them every vertex decodes to the origin (the fallen drew as a dot). Set them again.
            fallen.SetVector(PosMinId, a.PosMin); fallen.SetVector(PosSizeId, a.PosSize);
            // the range goes on the property blocks too: a shader reload during Play resets a material's values to the
            // shader's defaults (every man then decodes to a 1 m box or a point), and a block is never touched by that
            var props = new MaterialPropertyBlock(); var fallenProps = new MaterialPropertyBlock();
            props.SetVector(PosMinId, a.PosMin); props.SetVector(PosSizeId, a.PosSize);
            fallenProps.SetVector(PosMinId, a.PosMin); fallenProps.SetVector(PosSizeId, a.PosSize);
            return new Figure { Asset = a, Material = m, Fallen = fallen, Rows = rows, Props = props, FallenProps = fallenProps };
        }

        void Start()
        {
            var shader = Shader.Find("TW/VAT Infantry (URP)");
            if (shader == null || !SystemInfo.supportsComputeShaders) { Debug.LogWarning("VATRenderer: VAT shader unavailable, units fall back to capsules"); return; }
            var baked = new List<VatAsset>();
            foreach (var name in FigureNames)
            {
                var data = Resources.Load<VatAssetData>("Units/Figure" + name);   // written by TW/VAT/Bake Infantry
                if (data != null && data.Valid) baked.Add(data.ToAsset());
                else if (baked.Count > 0) baked.Add(baked[baked.Count - 1]);   // a missing figure borrows the one before it
                else Debug.LogWarning("VATRenderer: no bake for figure " + name);
            }
            if (baked.Count > 0 && baked.Count < FigureNames.Length) for (int k = baked.Count; k < FigureNames.Length; k++) baked.Add(baked[0]);
            clipAtlas = baked.Count > 0 && baked[0].ClipAtlas;
            if (baked.Count > 0)
            {
                figures = new Figure[baked.Count];
                for (int k = 0; k < baked.Count; k++) figures[k] = Make(shader, baked[k]);
                far = Make(shader, ProceduralSoldier.Build());
                far.Material.SetFloat(LerpId, 0f); far.Fallen.SetFloat(LerpId, 0f);
                if (clipAtlas) Clips.Apply(baked[0].RowSeconds);   // the controller times its one-shots by the bake
            }
            else figures = new[] { Make(shader, ProceduralSoldier.Build()) };
            argsBuffer = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, figures.Length + 1, GraphicsBuffer.IndirectDrawIndexedArgs.size);
            args = new GraphicsBuffer.IndirectDrawIndexedArgs[figures.Length + 1];
            // the controller hands every man a Clip; each tier turns it into a row of its own atlas
            nearRowOf = new NativeArray<ushort>((int)Clip.Count, Allocator.Persistent);
            farRowOf = new NativeArray<ushort>((int)Clip.Count, Allocator.Persistent);
            for (int c = 0; c < (int)Clip.Count; c++)
            {
                farRowOf[c] = (ushort)Clips.Table[c].Fallback;
                nearRowOf[c] = clipAtlas ? (ushort)c : farRowOf[c];
            }

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
            sorted = new NativeArray<VatInstance>(maxSlots, Allocator.Persistent);
            figureOf = new NativeArray<byte>(maxSlots, Allocator.Persistent);
            vehicles = new NativeArray<float4>(maxSlots, Allocator.Persistent);
            counts = new NativeArray<int>(3, Allocator.Persistent);
            planes = new NativeArray<float4>(6, Allocator.Persistent);
            spare = new NativeArray<float>(1, Allocator.Persistent);
            instanceBuffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, maxSlots, 48);
        }

        static GraphicsBuffer.IndirectDrawIndexedArgs Args(Mesh mesh, int count, int start) => new GraphicsBuffer.IndirectDrawIndexedArgs
        {
            indexCountPerInstance = mesh.GetIndexCount(0), instanceCount = (uint)count, startIndex = mesh.GetIndexStart(0), baseVertexIndex = mesh.GetBaseVertex(0), startInstance = (uint)start,
        };

        void LateUpdate()   // after SimHost.Update has interpolated this frame's poses
        {
            using var perf = TW.Sim.PerfMarkers.VatLate.Auto();
            if (figures == null || Host == null || Host.Presenter == null || Host.Local == null) return;
            var presenter = Host.Presenter;
            EnsureCapacity(presenter.Poses.Length);
            var cam = Camera.main;
            if (cam != null)
            {
                GeometryUtility.CalculateFrustumPlanes(cam, frustum);
                for (int i = 0; i < 6; i++) planes[i] = new float4(frustum[i].normal, frustum[i].distance);
            }
            float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var z) ? z.CurrentZoom : 0f;
            float grow = Mathf.Clamp(zoom / Mathf.Max(1f, GrowFromZoom), 1f, MaxGrow);
            var anim = Host.Animation;
            bool controlled = Host.UseAnimationController && anim != null;
            new FillJob
            {
                Poses = presenter.Poses, PoseCount = presenter.PoseCount, PoseSlot = presenter.PoseSlot, Height = Host.Local.Map.Height, Scale = UnitScale * grow,
                Ground = ReferenceEquals(RenderGround.Map, Host.Local.Map) ? RenderGround.Grid : default,
                Instances = instances, FigureOf = figureOf, Figures = figures.Length, Vehicles = vehicles, Counts = counts, Planes = planes, Cull = cam != null, Radius = LodTiers.CullRadius * UnitScale,
                CamPos = cam != null ? (float3)cam.transform.position : default, FarSq = far != null && cam != null ? LodDistance * LodDistance : float.MaxValue,
                Controlled = controlled, NearRowOf = nearRowOf, FarRowOf = farRowOf,
                PrevRow = anim != null ? anim.PrevRow : nearRowOf, PrevPhase = anim != null ? anim.PrevPhase : spare, Blend = anim != null ? anim.Blend : spare, Lift = anim != null ? anim.Lift : spare,
                Hop = anim != null ? anim.Hop : spare, Grime = anim != null ? anim.Grime : spare,
            }.Run();
            DrawnNear = counts[0]; DrawnFar = counts[2];
            DrawnInfantry = DrawnNear + DrawnFar;
            DrawnVehicles = counts[1];

            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));
            if (DrawnInfantry > 0)
            {
                // the near records grouped by figure, so each figure is one contiguous indirect draw
                for (int k = 0; k < figures.Length; k++) figures[k].Near = 0;
                for (int i = 0; i < DrawnNear; i++) figures[figureOf[i]].Near++;
                for (int k = 0, at = 0; k < figures.Length; k++) { figures[k].Start = at; at += figures[k].Near; }
                for (int i = 0; i < DrawnNear; i++) { var f = figures[figureOf[i]]; sorted[f.Start++] = instances[i]; }
                for (int k = 0; k < figures.Length; k++) figures[k].Start -= figures[k].Near;
                int farStart = instances.Length - DrawnFar;
                if (DrawnNear > 0) instanceBuffer.SetData(sorted, 0, 0, DrawnNear);
                if (DrawnFar > 0) instanceBuffer.SetData(instances, farStart, farStart, DrawnFar);
                long nearVerts = 0;
                for (int k = 0; k < figures.Length; k++) { args[k] = Args(figures[k].Asset.Mesh, figures[k].Near, figures[k].Start); nearVerts += (long)figures[k].Near * figures[k].Asset.Mesh.vertexCount; }
                long farVerts = far != null ? (long)DrawnFar * far.Asset.Mesh.vertexCount : 0;
                if (far != null) args[figures.Length] = Args(far.Asset.Mesh, DrawnFar, farStart);
                argsBuffer.SetData(args);
                ShadowsThisFrame = CastShadows && nearVerts * 2 + farVerts <= LodTiers.VertexBudget;   // only the near tier casts
                VerticesThisFrame = nearVerts * (ShadowsThisFrame ? 2 : 1) + farVerts;
                for (int k = 0; k < figures.Length; k++)
                {
                    var f = figures[k];
                    if (f.Near == 0) continue;
                    f.Material.SetFloat(LerpId, zoom > LodTiers.BlendZoom ? 0f : 1f);
                    f.Fallen.SetFloat(LerpId, zoom > LodTiers.BlendZoom ? 0f : 1f);
                    f.Props.SetBuffer("_Instances", instanceBuffer); f.Props.SetBuffer("_RowTable", f.Rows);
                    var rp = new RenderParams(f.Material) { worldBounds = bounds, shadowCastingMode = ShadowsThisFrame ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true, matProps = f.Props };
                    FrameBudget.DrawIndirect(rp, f.Asset.Mesh, argsBuffer, 1, k);
                }
                if (DrawnFar > 0 && far != null)
                {
                    far.Props.SetBuffer("_Instances", instanceBuffer); far.Props.SetBuffer("_RowTable", far.Rows);
                    var rp = new RenderParams(far.Material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true, matProps = far.Props };
                    FrameBudget.DrawIndirect(rp, far.Asset.Mesh, argsBuffer, 1, figures.Length);
                }
            }
            if (DrawnVehicles > 0) DrawVehicles(bounds);
            DrawFallen(cam, bounds, UnitScale * grow);
        }

        /// <summary>
        /// Where the drawn man's rifle muzzle is this frame (world), the way its barrel points, and his chest: sampled from
        /// his figure's baked sockets for the clip, phase, cross-fade and yaw the controller drew him with, at the drawn
        /// position and scale. False for vehicles, beyond the near tier (the box soldier carries no sockets), or without
        /// the controller; the caller then estimates.
        /// </summary>
        public bool Sockets(int slot, out Vector3 muzzle, out Vector3 barrel, out Vector3 chest)
        {
            muzzle = barrel = chest = default;
            var anim = Host != null ? Host.Animation : null;
            if (!clipAtlas || figures == null || anim == null || !Host.UseAnimationController || Host.Presenter == null || Host.Local == null) return false;
            var w = Host.Local.World;
            if (slot < 0 || slot >= w.HighWater || slot >= anim.Row.Length || (w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0) return false;
            var asset = figures[math.clamp(FigureOfArchetype(w.Archetype[slot]), 0, figures.Length - 1)].Asset;
            if (asset.Sockets == null) return false;
            float3 p = Host.Presenter.Drawn(slot);
            var map = Host.Local.Map;
            float ground = RenderGround.Sample(map, p.x, p.z), lift = anim.Lift[slot];
            if (lift > 0f) ground = Mathf.Lerp(ground, map.Height.Sample(p.x, p.z), lift);
            var at = new Vector3(p.x, ground, p.z);
            var cam = Camera.main;
            if (cam != null && far != null && (at - cam.transform.position).sqrMagnitude > LodDistance * LodDistance) return false;
            float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var z) ? z.CurrentZoom : 0f;
            float scale = UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, GrowFromZoom), 1f, MaxGrow);
            at.y += anim.Hop[slot] * scale;   // a shell has him in the air (life-size metres, drawn at his scale)
            int row = nearRowOf[math.min((int)anim.Row[slot], nearRowOf.Length - 1)], prev = nearRowOf[math.min((int)anim.PrevRow[slot], nearRowOf.Length - 1)];
            float t = anim.Phase[slot], pt = anim.PrevPhase[slot], blend = anim.Blend[slot];
            Vector3 Sample(int k)
            {
                asset.Socket(row, t, k, out var v);
                if (blend > 0.001f && asset.Socket(prev, pt, k, out var pv)) v = Vector3.Lerp(v, pv, blend);   // the cross-fade, as the shader blends it
                return v;
            }
            var turn = Quaternion.Euler(0f, anim.Yaw[slot] * Mathf.Rad2Deg, 0f);
            muzzle = at + turn * (Sample(VatAsset.Muzzle) * scale);
            barrel = (turn * Sample(VatAsset.Barrel)).normalized;
            chest = at + turn * (Sample(VatAsset.Chest) * scale);
            return true;
        }

        // ---- the fallen: the same figure as the living, played once through his death and held on the last frame
        // where he fell. Their own small buffer (near model up close, box model beyond), no shadows.
        struct FallenMan { public Vector3 Pos, From; public float Yaw, Born, Seconds, FromT, Fade, Flight, Up, Top, Rate, Spin, Lies, Grime; public byte Team, Figure, Gib; public ushort Row, FarRow, FromRow; }
        static float Hash01(Vector3 v) { float h = Mathf.Sin(v.x * 12.9898f + v.z * 78.233f) * 43758.5453f; return h - Mathf.Floor(h); }
        /// <summary>Gravity for a thrown corpse (m/s2): a little over the real thing, so the arc reads as a blow, not a float.</summary>
        public float ThrowGravity = 14f;
        readonly List<FallenMan> fallenMen = new List<FallenMan>(128);
        public int MaxFallen = 600;
        public float FallenSeconds = 30f;      // a body lies this long, the last SinkSeconds of it sinking into the mud
        public const float SinkSeconds = 2.5f, SinkDepth = 1.1f;
        public float FallSeconds = 0.9f, FallenNearDistance = 70f;
        public int FallenCount => fallenMen.Count;
        GraphicsBuffer fallenBuffer, fallenArgs;
        NativeArray<VatInstance> fallenInstances;
        GraphicsBuffer.IndirectDrawIndexedArgs[] fallenArgsData;
        readonly List<int>[] fallenByFigure = new List<int>[8];

        /// <summary>
        /// A man died here: he goes down facing yaw (radians) and stays. The oldest is taken away past MaxFallen. With the clip
        /// atlas the near tier plays the death the controller chose (clip) on his archetype's figure; the far tier and the box
        /// soldier use the procedural death the variant picks. gib: the limbs a shell took off him, a bit each (1 head,
        /// 2 left arm, 3 right arm, 4 left leg, 5 right leg), carried to the shader in the record's spare float; the mesh
        /// must carry a limb id per vertex (VATBaker writes it to UV1.x) for the cut to show. grime: the mud and soot he wore
        /// (AnimationController.Grime), so a man does not come clean as he dies.
        /// </summary>
        public void AddFallen(Vector3 pos, float yaw, int team, int variant, Clip clip = Clip.None, int archetype = 0, Clip fromClip = Clip.None, float fromPhase = 0f, float fade = 0f, Vector3 fly = default, int gib = 0, float grime = 0f)
        {
            if (fallenMen.Count >= MaxFallen) fallenMen.RemoveAt(0);
            ushort farRow = (ushort)((int)AnimRow.Death0 + (variant & 3));
            int figure = figures != null ? math.clamp(FigureOfArchetype(archetype), 0, figures.Length - 1) : 0;
            bool near = clipAtlas && clip != Clip.None;
            float seconds = near ? figures[figure].Asset.RowSeconds[(int)clip] : FallSeconds;
            // the clip he was in as he was hit fades out over the death's first moments (the living instance stops drawing him)
            bool blend = near && fromClip != Clip.None && fade > 0.01f;
            var man = new FallenMan { Pos = pos, From = pos, Yaw = yaw, Born = Time.time, Seconds = Mathf.Max(0.1f, seconds), Lies = FallenSeconds * (0.7f + 0.6f * Hash01(pos)), Team = (byte)team, Figure = (byte)figure, Gib = (byte)(gib & 0xFF), Grime = grime, Row = near ? (ushort)clip : farRow, FarRow = farRow,
                FromRow = blend ? (ushort)fromClip : (ushort)0, FromT = fromPhase, Fade = blend ? fade : 0f, Rate = 1f };
            // thrown: fly.xz is how far, fly.y how high above the higher end the arc goes. He lands on the drawn ground there,
            // and his death clip is played so his back meets it as he lands.
            if (fly.y > 0.05f && Host != null && Host.Local != null)
            {
                var size = Host.Local.Map.SizeMeters;
                Vector3 land = pos + new Vector3(fly.x, 0f, fly.z);
                land.x = Mathf.Clamp(land.x, 0.5f, size.x - 0.5f); land.z = Mathf.Clamp(land.z, 0.5f, size.y - 0.5f);
                land.y = RenderGround.Sample(Host.Local.Map, land.x, land.z) - 0.02f;
                float g = Mathf.Max(1f, ThrowGravity), top = Mathf.Max(pos.y, land.y) + fly.y;
                float up = Mathf.Sqrt(2f * (top - pos.y) / g), down = Mathf.Sqrt(2f * (top - land.y) / g);
                man.Pos = land; man.Up = up; man.Top = top; man.Flight = up + down;
                // a high arc lasts longer, but under about 0.8x he tumbles in slow motion over a battlefield at full speed:
                // below that he lands a moment early and holds the pose
                if (near && clip == Clip.DeathThrown) man.Rate = Mathf.Clamp(AnimationController.ThrownLands / man.Flight, 0.8f, 1.8f);
                man.Spin = (Hash01(pos) < 0.5f ? -1f : 1f) * Mathf.Lerp(1.1f, 2.6f, Hash01(pos + new Vector3(7.3f, 0f, 3.1f)));   // he goes over as he flies
            }
            fallenMen.Add(man);
        }

        /// <summary>Where a fallen man is drawn now: on his arc while a shell's throw lasts, then where he landed, and at
        /// the end of his time sinking into the mud (the drawn ground hides him: no fading, nothing for the shader to do).</summary>
        Vector3 FallenAt(in FallenMan f, float now)
        {
            float age = now - f.Born;
            float lies = f.Lies > 0.01f ? f.Lies : FallenSeconds;
            float sunk = FallenSeconds > 0f ? Mathf.Clamp01((age - (lies - SinkSeconds)) / SinkSeconds) * SinkDepth : 0f;
            if (f.Flight <= 0f || age >= f.Flight) return sunk > 0f ? f.Pos - new Vector3(0f, sunk, 0f) : f.Pos;
            Vector3 at = Vector3.Lerp(f.From, f.Pos, age / f.Flight);
            float g = Mathf.Max(1f, ThrowGravity), fromTop = age - f.Up;
            at.y = f.Top - 0.5f * g * fromTop * fromTop;
            return at;
        }

        void DrawFallen(Camera cam, Bounds bounds, float scale)
        {
            if (fallenMen.Count == 0 || figures == null) return;
            if (!fallenInstances.IsCreated)
            {
                fallenInstances = new NativeArray<VatInstance>(MaxFallen, Allocator.Persistent);
                fallenBuffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, MaxFallen, 48);
                fallenArgs = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, figures.Length + 1, GraphicsBuffer.IndirectDrawIndexedArgs.size);
                fallenArgsData = new GraphicsBuffer.IndirectDrawIndexedArgs[figures.Length + 1];
                for (int k = 0; k < fallenByFigure.Length; k++) fallenByFigure[k] = new List<int>(64);
            }
            // the mud takes them: a body lies FallenSeconds, then it is gone (they are in the order they fell, so the front of the list goes first)
            if (FallenSeconds > 0f)
            {
                // each man lies his own 0.7-1.3 of the time, so a barrage's dead do not all go under together
                float now0 = Time.time;
                for (int i = fallenMen.Count - 1; i >= 0; i--)
                {
                    var f = fallenMen[i];
                    if (now0 - f.Born > (f.Lies > 0.01f ? f.Lies : FallenSeconds)) fallenMen.RemoveAt(i);
                }
                if (fallenMen.Count == 0) return;
            }
            for (int k = 0; k < figures.Length; k++) fallenByFigure[k].Clear();
            int farCount = 0, last = fallenInstances.Length - 1;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            float now = Time.time, nearSq = far != null && cam != null ? FallenNearDistance * FallenNearDistance : float.MaxValue;
            for (int i = 0; i < fallenMen.Count && i < fallenInstances.Length; i++)
            {
                var f = fallenMen[i];
                Vector3 at = FallenAt(f, now);
                if (cam != null)
                {
                    bool seen = true;
                    for (int k = 0; k < 6 && seen; k++) seen = frustum[k].GetDistanceToPoint(at) > -2.5f * scale;
                    if (!seen) continue;
                }
                bool distant = (at - eye).sqrMagnitude > nearSq;
                if (distant) { if (farCount < fallenInstances.Length) fallenInstances[last - farCount++] = Fallen(f, far.Asset, f.FarRow, now, scale, at); }
                else fallenByFigure[math.min(f.Figure, figures.Length - 1)].Add(i);
            }
            int near = 0;
            for (int k = 0; k < figures.Length; k++)
            {
                int start = near;
                foreach (int i in fallenByFigure[k]) { if (near + farCount >= fallenInstances.Length) break; fallenInstances[near++] = Fallen(fallenMen[i], figures[k].Asset, fallenMen[i].Row, now, scale, FallenAt(fallenMen[i], now)); }
                fallenArgsData[k] = Args(figures[k].Asset.Mesh, near - start, start);
            }
            if (near + farCount == 0) return;
            int farStart = fallenInstances.Length - farCount;
            if (near > 0) fallenBuffer.SetData(fallenInstances, 0, 0, near);
            if (farCount > 0) fallenBuffer.SetData(fallenInstances, farStart, farStart, farCount);
            if (far != null) fallenArgsData[figures.Length] = Args(far.Asset.Mesh, farCount, farStart);
            fallenArgs.SetData(fallenArgsData);
            for (int k = 0; k < figures.Length; k++)
            {
                if (fallenArgsData[k].instanceCount == 0) continue;
                var fig = figures[k];
                fig.FallenProps.SetBuffer("_Instances", fallenBuffer); fig.FallenProps.SetBuffer("_RowTable", fig.Rows);
                FrameBudget.DrawIndirect(new RenderParams(fig.Fallen) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true, matProps = fig.FallenProps }, fig.Asset.Mesh, fallenArgs, 1, k);
            }
            if (farCount > 0 && far != null)
            {
                far.FallenProps.SetBuffer("_Instances", fallenBuffer); far.FallenProps.SetBuffer("_RowTable", far.Rows);
                FrameBudget.DrawIndirect(new RenderParams(far.Material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true, matProps = far.FallenProps }, far.Asset.Mesh, fallenArgs, 1, figures.Length);
            }
        }

        static VatInstance Fallen(in FallenMan f, VatAsset tier, ushort row, float now, float scale, Vector3 at)
        {
            float t = Mathf.Clamp01((now - f.Born) * f.Rate / f.Seconds);
            if (tier.Loops(row)) { float frames = Mathf.Max(2f, tier.Frames(row)); t *= (frames - 0.99f) / frames; }   // a looping row: stop on its last frame
            float blend = f.Fade > 0f && row == f.Row ? Mathf.Clamp01(1f - (now - f.Born) / f.Fade) : 0f;   // the near tier only: the far rows are another atlas
            // thrown: he turns as he goes through the air and comes to rest the way he landed
            float yaw = f.Yaw;
            if (f.Flight > 0f && f.Spin != 0f) yaw += f.Spin * Mathf.Min(now - f.Born, f.Flight);
            return new VatInstance { Pos = at, Yaw = yaw, AnimRow = row, AnimT = t, Tint = f.Team, Scale = scale, PrevRow = f.FromRow, PrevT = f.FromT, Blend = blend, Pad = VatPad.Pack(f.Gib, f.Grime, (int)(Hash01(f.From) * 255f)) };
        }

        void DrawVehicles(Bounds bounds)
        {
            if (SceneHooks.TanksDrawn) return;   // TankRenderer draws them from their parts
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
            if (a > 0) FrameBudget.Draw(new RenderParams(tankMatA) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On }, tankMesh, 0, tankBatchA, a);
            if (b > 0) FrameBudget.Draw(new RenderParams(tankMatB) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On }, tankMesh, 0, tankBatchB, b);
        }

        [BurstCompile]
        struct FillJob : IJob
        {
            [ReadOnly] public NativeArray<UnitPose> Poses;
            [ReadOnly] public NativeArray<int> PoseSlot;
            [ReadOnly] public Heightfield Height;
            public RenderGroundGrid Ground;
            public int PoseCount, Figures;
            public float Scale;
            public bool Controlled;
            [ReadOnly] public NativeArray<ushort> NearRowOf, FarRowOf, PrevRow;
            [ReadOnly] public NativeArray<float> PrevPhase, Blend, Lift, Hop, Grime;
            public NativeArray<VatInstance> Instances;
            public NativeArray<byte> FigureOf;
            public NativeArray<float4> Vehicles;
            public NativeArray<int> Counts;
            [ReadOnly] public NativeArray<float4> Planes;
            public bool Cull;
            public float Radius, FarSq;
            public float3 CamPos;

            bool Visible(float3 p)
            {
                if (!Cull) return true;
                for (int k = 0; k < 6; k++)
                    if (math.dot(Planes[k].xyz, p) + Planes[k].w < -Radius) return false;
                return true;
            }

            public void Execute()
            {
                int n = 0, v = 0, far = 0, last = Instances.Length - 1;
                for (int i = 0; i < PoseCount; i++)
                {
                    var p = Poses[i];
                    float original = Height.Sample(p.Pos.x, p.Pos.z);
                    float y = Ground.Sample(p.Pos.x, p.Pos.z, original);
                    if (Controlled) { float lift = Lift[PoseSlot[i]]; if (lift > 0f) y = math.lerp(y, original, lift); y += Hop[PoseSlot[i]] * Scale; }   // climbing: drawn up the trench wall; blown off his feet: in the air
                    if (!Visible(new float3(p.Pos.x, y + 1f, p.Pos.z))) continue;
                    if ((p.Flags & (byte)UnitFlags.Vehicle) != 0)
                    {
                        // yaw is within (-2pi, 2pi); +10 keeps it positive so the sign is free for the team
                        Vehicles[v++] = new float4(p.Pos.x, y, p.Pos.z, (p.Yaw + 10f) * (p.Team == 0 ? 1f : -1f));
                        continue;
                    }
                    bool distant = math.distancesq(CamPos, new float3(p.Pos.x, y, p.Pos.z)) > FarSq;
                    var map = distant ? FarRowOf : NearRowOf;
                    int row = p.AnimRow, prevRow = row, seed = 0; float prevT = 0f, blend = 0f, grime = 0f;
                    if (Controlled)
                    {
                        int slot = PoseSlot[i];
                        grime = Grime[slot]; seed = slot * 37;
                        row = map[math.min(row, map.Length - 1)];
                        prevRow = map[math.min(PrevRow[slot], map.Length - 1)]; prevT = PrevPhase[slot]; blend = Blend[slot];
                    }
                    var inst = new VatInstance
                    {
                        Pos = new float3(p.Pos.x, y, p.Pos.z), Yaw = p.Yaw, AnimRow = row, AnimT = p.AnimT, Tint = p.Team, Scale = Scale,
                        PrevRow = prevRow, PrevT = prevT, Blend = blend, Pad = VatPad.Pack(0, grime, seed),
                    };
                    if (distant) Instances[last - far++] = inst;
                    else { FigureOf[n] = (byte)math.min(Figures - 1, FigureOfArchetype(p.Archetype)); Instances[n++] = inst; }
                }
                Counts[0] = n; Counts[1] = v; Counts[2] = far;
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
            if (sorted.IsCreated) sorted.Dispose();
            if (figureOf.IsCreated) figureOf.Dispose();
            if (vehicles.IsCreated) vehicles.Dispose();
            if (counts.IsCreated) counts.Dispose();
            if (planes.IsCreated) planes.Dispose();
            if (spare.IsCreated) spare.Dispose();
            instanceBuffer?.Dispose(); instanceBuffer = null;
        }

        /// <summary>
        /// Everything Start built, given back. The GraphicsBuffers and NativeArrays were always freed here; the
        /// textures, meshes and materials were not, and they are the large ones. Every atlas texture is created with
        /// HideFlags.HideAndDontSave, which exempts it from leaving Play mode, from scene unload and from
        /// Resources.UnloadUnusedAssets alike — so before this, a second Start simply decoded a second 70 MB set on
        /// top of the first, and the editor held both until it was quit. That doubling is the whole of the ~144 MB
        /// the VAT atlases were measured at.
        ///
        /// Assets are de-duplicated by reference because a figure whose bake is missing borrows the one before it
        /// (see Start), so the same VatAsset appears in several slots and must be released once.
        /// </summary>
        void OnDestroy()
        {
            Release();
            var freed = new HashSet<VatAsset>();
            if (figures != null) foreach (var f in figures) Give(f, freed);
            Give(far, freed);
            argsBuffer?.Dispose();
            fallenBuffer?.Dispose(); fallenArgs?.Dispose(); if (fallenInstances.IsCreated) fallenInstances.Dispose();
            if (nearRowOf.IsCreated) nearRowOf.Dispose(); if (farRowOf.IsCreated) farRowOf.Dispose();
            VatAsset.Kill(tankMatA); tankMatA = null;
            VatAsset.Kill(tankMatB); tankMatB = null;
            VatAsset.Kill(tankMesh); tankMesh = null;
            figures = null; far = null;
        }

        /// <summary>One figure's buffer, material and (once only) its atlas.</summary>
        static void Give(Figure f, HashSet<VatAsset> freed)
        {
            if (f == null) return;
            f.Rows?.Dispose();
            VatAsset.Kill(f.Material); VatAsset.Kill(f.Fallen);
            if (f.Asset != null && freed.Add(f.Asset)) f.Asset.Release();
        }
    }
}
