// Phase: CelFx orbital shadow lab prototype (2026-11-14). Opt-in, one-strike radial depth.
using System;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.VFX.Cel
{
    public static class OrbitalShadowDepth
    {
        public const int Resolution = 256, BatchCapacity = 128;
        public const float Range = 14f, Near = 0.05f, SourceHeight = 5f, BiasMetres = 0.07f;
        static readonly int ValidId = Shader.PropertyToID("_OrbitalShadowValid");
        static readonly int TextureId = Shader.PropertyToID("_OrbitalShadowDepthCube");
        static readonly int SourceId = Shader.PropertyToID("_OrbitalShadowSourceRange");
        static readonly int StrikeId = Shader.PropertyToID("_OrbitalShadowStrike");
        static readonly int CentreId = Shader.PropertyToID("_OrbitalShadowCentre");
        static readonly int VpId = Shader.PropertyToID("_OrbitalShadowVP");
        static readonly int MaskId = Shader.PropertyToID("_ChunkMask");
        static readonly int TimeId = Shader.PropertyToID("_Time");
        // Matches installed CoreUtils.lookAtList / upVectorList. Orientation still needs six-face GPU validation.
        static readonly Vector3[] directions = { Vector3.right, Vector3.left, Vector3.up, Vector3.down, Vector3.forward, Vector3.back };
        static readonly Vector3[] ups = { Vector3.up, Vector3.up, Vector3.back, Vector3.forward, Vector3.up, Vector3.up };
        static readonly Matrix4x4[] matrices = new Matrix4x4[BatchCapacity];
        static readonly Vector4[] masks = new Vector4[BatchCapacity];
        static readonly Plane[] facePlanes = new Plane[6];
        static Camera camera;
        static RenderTexture cube;
        static CommandBuffer commands;
        static MaterialPropertyBlock properties;
        static OrbitalShadowCasterBuffer casters;
        static Vector3 centre, right;
        static float born, life;
        static bool hasStrike, hooked, requireDummy;
        public static string Failure { get; private set; }
        public static int RenderedCameras { get; private set; }
        public static int PeakCasters { get; private set; }
        public static int PeakProps { get; private set; }
        public static int PeakDraws { get; private set; }
        public static int DisplacedStrikes { get; private set; }
        public static bool Configured => camera != null && cube != null && string.IsNullOrEmpty(Failure);
        static OrbitalShadowDepth() => SceneStatics.Register(nameof(OrbitalShadowDepth), ResetSession);

        public static void Configure(Camera target, bool fixtureRequiresDummy)
        {
            if (target == null) throw new ArgumentNullException(nameof(target));
            if (camera != null && camera != target) throw new InvalidOperationException("Orbital shadow owner already has a camera.");
            Release();
            Failure = null; RenderedCameras = PeakCasters = PeakProps = PeakDraws = DisplacedStrikes = 0;
            camera = target; requireDummy = fixtureRequiresDummy;
            try
            {
                if (!SystemInfo.SupportsRenderTextureFormat(RenderTextureFormat.RFloat))
                    throw new NotSupportedException("RFloat render target unavailable.");
                cube = new RenderTexture(Resolution, Resolution, 24, RenderTextureFormat.RFloat, RenderTextureReadWrite.Linear)
                {
                    name = "Orbital owned radial depth", hideFlags = HideFlags.HideAndDontSave,
                    dimension = TextureDimension.Cube, useMipMap = false, autoGenerateMips = false,
                    antiAliasing = 1, filterMode = FilterMode.Point, wrapMode = TextureWrapMode.Clamp
                };
                if (!cube.Create()) throw new InvalidOperationException("Orbital radial-depth cube Create failed.");
                commands = new CommandBuffer { name = "Orbital owned depth cube" };
                properties = new MaterialPropertyBlock();
                // Warm fixed vector-array length once. Native command payloads copy these values when queued.
                properties.SetVectorArray(MaskId, masks);
                casters = new OrbitalShadowCasterBuffer();
                RenderPipelineManager.beginCameraRendering += BeginCamera;
                RenderPipelineManager.endCameraRendering += EndCamera;
                hooked = true;
            }
            catch (Exception ex) { Fail(ex); throw; }
        }

        public static void TrackStrike(Vector3 groundCentre, float start, float duration, Vector3 axis)
        {
            if (!Configured) return;
            if (hasStrike && start < born + life && start >= born) DisplacedStrikes++;
            centre = groundCentre; right = axis; right.y = 0f;
            right = right.sqrMagnitude > 0.001f ? right.normalized : Vector3.right;
            born = start; life = duration; hasStrike = duration > 0f;
        }

        // Clear a record without tearing down the explicitly configured capture owner or erasing diagnostics.
        public static void ClearStrike()
        { hasStrike = false; Shader.SetGlobalFloat(ValidId, 0f); }
        public static void Prune(float now)
        { if (hasStrike && now >= born + life) ClearStrike(); }

        static void BeginCamera(ScriptableRenderContext context, Camera renderingCamera)
        {
            Shader.SetGlobalFloat(ValidId, 0f);
            if (!Configured || renderingCamera != camera || !hasStrike) return;
            float now = CelFx.Now, age = now - born;
            if (age >= life) { ClearStrike(); return; } // permanent: a later pinned-clock rewind cannot revive it.
            if (age <= 0f) return;
            try
            {
                // Render every requested camera time: pinned captures can change CelFx.Now within one frame.
                Vector3 source = centre + OrbitalLaserRecipe.SweepOffset(age, right);
                source.y = (RenderGround.Map != null ? RenderGround.Sample(RenderGround.Map, source.x, source.z) : centre.y) + SourceHeight;
                OrbitalShadowCasters.Collect(casters, source, Range);
                if (OrbitalShadowCasters.RegistrationOverflow || casters.Overflow)
                    throw new InvalidOperationException("Orbital caster/provider capacity exceeded; no truncated shadow accepted.");
                if (casters.Unsupported != 0)
                    throw new InvalidOperationException("Orbital radius contains material without OrbitalRadialDepth pass.");
                if (casters.Props == 0 || (requireDummy && casters.Dummies == 0))
                    throw new InvalidOperationException("Orbital cube requires nearby BattlefieldProps and the requested dummy; fixture incomplete.");
                commands.Clear();
                commands.SetGlobalVector(SourceId, new Vector4(source.x, source.y, source.z, Range));
                float clock = Application.isPlaying ? Time.time : Time.realtimeSinceStartup;
                commands.SetGlobalVector(TimeId, new Vector4(clock / 20f, clock, clock * 2f, clock * 3f));
                Matrix4x4 projection = Matrix4x4.Perspective(90f, 1f, Near, Range);
                int draws = 0;
                for (int face = 0; face < 6; face++)
                {
                    Matrix4x4 view = Matrix4x4.Scale(new Vector3(1, 1, -1))
                        * Matrix4x4.TRS(source, Quaternion.LookRotation(directions[face], ups[face]), Vector3.one).inverse;
                    GeometryUtility.CalculateFrustumPlanes(projection * view, facePlanes);
                    commands.SetRenderTarget(cube, 0, (CubemapFace)face);
                    commands.SetViewport(new Rect(0, 0, Resolution, Resolution));
                    commands.ClearRenderTarget(true, true, Color.white, SystemInfo.usesReversedZBuffer ? 0f : 1f);
                    commands.SetGlobalMatrix(VpId, GL.GetGPUProjectionMatrix(projection, true) * view);
                    int offset = 0;
                    while (offset < casters.Count)
                    {
                        var first = casters.Items[offset];
                        int held = 0;
                        do
                        {
                            var item = casters.Items[offset++];
                            if (!GeometryUtility.TestPlanesAABB(facePlanes, item.Bounds)) continue;
                            matrices[held] = item.Matrix; masks[held++] = item.ChunkMask;
                        }
                        while (held < BatchCapacity && offset < casters.Count && casters.Items[offset].Mesh == first.Mesh
                            && casters.Items[offset].Material == first.Material && casters.Items[offset].Pass == first.Pass);
                        if (held == 0) continue;
                        properties.Clear();
                        if (SystemInfo.supportsInstancing && first.Material.enableInstancing && held > 1)
                        {
                            properties.SetVectorArray(MaskId, masks);
                            FrameBudget.DrawShadowInstanced(commands, first.Mesh, 0, first.Material, first.Pass, matrices, held, properties);
                            draws++;
                        }
                        else for (int i = 0; i < held; i++)
                        {
                            properties.SetVector(MaskId, masks[i]);
                            FrameBudget.DrawShadow(commands, first.Mesh, 0, first.Material, first.Pass, matrices[i], properties);
                            draws++;
                        }
                    }
                }
                commands.SetGlobalTexture(TextureId, cube);
                commands.SetGlobalVector(StrikeId, new Vector4(born, life, BiasMetres, 1f));
                commands.SetGlobalVector(CentreId, new Vector4(centre.x, centre.y + 1.1f, centre.z, 0));
                commands.SetGlobalFloat(ValidId, 1f);
                context.ExecuteCommandBuffer(commands);
                commands.Clear();
                RenderedCameras++; PeakCasters = Mathf.Max(PeakCasters, casters.Count);
                PeakProps = Mathf.Max(PeakProps, casters.Props); PeakDraws = Mathf.Max(PeakDraws, draws);
            }
            catch (Exception ex) { Fail(ex); }
        }

        static void EndCamera(ScriptableRenderContext context, Camera renderingCamera)
        {
            if (commands == null || renderingCamera != camera) return;
            commands.Clear(); commands.SetGlobalFloat(ValidId, 0f);
            context.ExecuteCommandBuffer(commands); commands.Clear();
        }

        static void Fail(Exception ex)
        {
            Failure = ex.GetType().Name + ": " + ex.Message;
            Debug.LogError("Orbital shadow prototype failed: " + Failure);
            Release();
        }
        public static void Disable(Camera owner) { if (camera == owner) Release(); }
        static void Release()
        {
            Shader.SetGlobalFloat(ValidId, 0f);
            Shader.SetGlobalTexture(TextureId, (Texture)null);
            if (hooked)
            {
                RenderPipelineManager.beginCameraRendering -= BeginCamera;
                RenderPipelineManager.endCameraRendering -= EndCamera;
                hooked = false;
            }
            commands?.Release(); commands = null; properties = null; casters = null;
            if (cube != null)
            {
                cube.Release();
                if (Application.isPlaying) UnityEngine.Object.Destroy(cube); else UnityEngine.Object.DestroyImmediate(cube);
                cube = null;
            }
            camera = null; hasStrike = false;
        }
        public static void ResetSession()
        {
            Release(); Failure = null;
            RenderedCameras = PeakCasters = PeakProps = PeakDraws = DisplacedStrikes = 0;
            Array.Clear(matrices, 0, matrices.Length); Array.Clear(masks, 0, masks.Length);
        }
    }
}
