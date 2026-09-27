// Phase: riders prototype (presentation only) — depends on: VATRenderer (figures, row tables, frustum), VAT_URP
// Men drawn where the presentation says, not where the sim says: the riders on a walker's back (TankRenderer
// hands them over each frame at their seats), and anything else that wants a living figure at a pose of its own.
// Mirrors the fallen path (own buffer, own args), but with the living material, shadows when the frame affords
// them, and a Clip in each record's AnimRow that this maps to the near or far atlas row as the fallen do.
using System.Collections.Generic;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;

namespace TW.Presentation.Units
{
    public sealed partial class VATRenderer
    {
        /// <summary>The zoom growth living men were drawn with this frame (UnitScale * this is their Scale).</summary>
        public float CurrentGrow { get; private set; } = 1f;
        public const int MaxExtras = 512;

        GraphicsBuffer extraBuffer, extraArgs;
        NativeArray<VatInstance> extraInstances;
        GraphicsBuffer.IndirectDrawIndexedArgs[] extraArgsData;
        MaterialPropertyBlock[] extraProps; MaterialPropertyBlock farExtraProps;
        readonly List<int>[] extraByFigure = new List<int>[8];
        /// <summary>Extras drawn last frame (near + far), for the lab's readout.</summary>
        public int DrawnExtras { get; private set; }
        /// <summary>How far the extras (the men riding machines) are lifted off the dark hull they kneel on: brighter, with
        /// their side's colour on their edge (VAT_URP _Lift).</summary>
        [Range(0f, 1.5f)] public float ExtrasLift = 1f;
        static readonly int LiftId = Shader.PropertyToID("_Lift"), LiftAId = Shader.PropertyToID("_LiftA"), LiftBId = Shader.PropertyToID("_LiftB");
        /// <summary>The rim each side's riders carry: the colour of that side's rings and seat pips (TankRenderer.TeamA/B).</summary>
        public Color ExtrasRimA = new Color(0.35f, 0.85f, 1.0f), ExtrasRimB = new Color(0.88f, 0.25f, 0.16f);

        // ---- slots the presentation has taken over: a man riding a walker is still a unit in the sim, but he is drawn on
        // the machine's back by DrawExtras, so the ordinary pass must not draw him where the sim has him as well
        NativeArray<byte> hidden;

        NativeArray<byte> HiddenMask(int slots)
        {
            if (!hidden.IsCreated || hidden.Length < slots)
            {
                var was = hidden;
                hidden = new NativeArray<byte>(math.max(1, slots), Allocator.Persistent);
                if (was.IsCreated) { NativeArray<byte>.Copy(was, hidden, math.min(was.Length, hidden.Length)); was.Dispose(); }
            }
            return hidden;
        }

        /// <summary>Stop (or resume) drawing the living man in `slot` where the sim has him.</summary>
        public void Hide(int slot, bool on)
        {
            if (Host == null || Host.Local == null || slot < 0) return;
            var mask = HiddenMask(Host.Local.World.Config.MaxSlots);
            if (slot < mask.Length) mask[slot] = on ? (byte)1 : (byte)0;
        }

        public bool IsHidden(int slot) => hidden.IsCreated && slot >= 0 && slot < hidden.Length && hidden[slot] != 0;

        /// <summary>
        /// Where the rifle's muzzle is for a figure drawn with this record (near tier only; the box soldier carries no
        /// sockets), and the way the barrel points. AnimRow/PrevRow carry Clips, as for DrawExtras.
        /// </summary>
        public bool MuzzleOf(in VatInstance inst, int figure, out Vector3 muzzle, out Vector3 dir)
        {
            muzzle = inst.Pos; dir = Vector3.forward;
            if (!clipAtlas || figures == null || figures.Length == 0) return false;
            var asset = figures[math.clamp(figure, 0, figures.Length - 1)].Asset;
            if (asset.Sockets == null || !nearRowOf.IsCreated) return false;
            int row = nearRowOf[math.clamp((int)inst.AnimRow, 0, nearRowOf.Length - 1)];
            if (!asset.Socket(row, inst.AnimT, VatAsset.Muzzle, out var m) || !asset.Socket(row, inst.AnimT, VatAsset.Barrel, out var b)) return false;
            var turn = Quaternion.Euler(0f, inst.Yaw * Mathf.Rad2Deg, 0f);
            muzzle = (Vector3)inst.Pos + turn * (m * inst.Scale);
            dir = (turn * b).normalized;
            return true;
        }

        /// <summary>How long a clip plays on the near figure (the bake's own timing when there is one).</summary>
        public float SecondsFor(Clip clip)
        {
            int c = (int)clip;
            if (clipAtlas && figures != null && figures.Length > 0 && figures[0].Asset.RowSeconds != null && c < figures[0].Asset.RowSeconds.Length && figures[0].Asset.RowSeconds[c] > 0.01f)
                return figures[0].Asset.RowSeconds[c];
            return c < Clips.Table.Length ? Mathf.Max(0.1f, Clips.Table[c].Seconds) : 1f;
        }

        public bool Loops(Clip clip) => (int)clip < Clips.Table.Length && Clips.Table[(int)clip].Loop;

        /// <summary>
        /// Draw `count` living figures at the poses given. Each record's AnimRow and PrevRow carry a Clip, mapped here to the
        /// tier's atlas; Pos is the drawn position (his feet); Scale the caller's (UnitScale * CurrentGrow to match the men
        /// around him). Called from another renderer's LateUpdate, after this one's, so the frustum is this frame's.
        /// </summary>
        public void DrawExtras(VatInstance[] src, byte[] figureOfExtra, int count)
        {
            DrawnExtras = 0;
            if (count <= 0 || src == null || figures == null || Host == null || Host.Local == null) return;
            if (!extraInstances.IsCreated)
            {
                extraInstances = new NativeArray<VatInstance>(MaxExtras, Allocator.Persistent);
                extraBuffer = new GraphicsBuffer(GraphicsBuffer.Target.Structured, MaxExtras, 48);
                extraArgs = new GraphicsBuffer(GraphicsBuffer.Target.IndirectArguments, figures.Length + 1, GraphicsBuffer.IndirectDrawIndexedArgs.size);
                extraArgsData = new GraphicsBuffer.IndirectDrawIndexedArgs[figures.Length + 1];
                extraProps = new MaterialPropertyBlock[figures.Length];
                for (int k = 0; k < figures.Length; k++)
                {
                    extraProps[k] = new MaterialPropertyBlock();
                    extraProps[k].SetVector(PosMinId, figures[k].Asset.PosMin); extraProps[k].SetVector(PosSizeId, figures[k].Asset.PosSize);
                }
                if (far != null) { farExtraProps = new MaterialPropertyBlock(); farExtraProps.SetVector(PosMinId, far.Asset.PosMin); farExtraProps.SetVector(PosSizeId, far.Asset.PosSize); }
                for (int k = 0; k < extraByFigure.Length; k++) extraByFigure[k] = new List<int>(64);
            }
            for (int k = 0; k < figures.Length; k++) extraByFigure[k].Clear();
            var cam = Camera.main;
            Vector3 eye = cam != null ? cam.transform.position : Vector3.zero;
            float farSq = far != null && cam != null ? LodDistance * LodDistance : float.MaxValue;
            int farCount = 0, last = extraInstances.Length - 1;
            count = math.min(count, math.min(src.Length, MaxExtras));
            for (int i = 0; i < count; i++)
            {
                var inst = src[i];
                if (cam != null)
                {
                    bool seen = true;
                    for (int k = 0; k < 6 && seen; k++) seen = frustum[k].GetDistanceToPoint(inst.Pos) > -2.5f * inst.Scale;
                    if (!seen) continue;
                }
                bool distant = ((Vector3)inst.Pos - eye).sqrMagnitude > farSq;
                if (distant)
                {
                    if (farCount >= extraInstances.Length) break;
                    extraInstances[last - farCount++] = Mapped(inst, farRowOf);
                }
                else extraByFigure[math.clamp(figureOfExtra != null && i < figureOfExtra.Length ? figureOfExtra[i] : 0, 0, figures.Length - 1)].Add(i);
            }
            int near = 0;
            for (int k = 0; k < figures.Length; k++)
            {
                int start = near;
                foreach (int i in extraByFigure[k]) { if (near + farCount >= extraInstances.Length) break; extraInstances[near++] = Mapped(src[i], nearRowOf); }
                extraArgsData[k] = Args(figures[k].Asset.Mesh, near - start, start);
            }
            if (near + farCount == 0) return;
            DrawnExtras = near + farCount;
            int farStart = extraInstances.Length - farCount;
            if (near > 0) extraBuffer.SetData(extraInstances, 0, 0, near);
            if (farCount > 0) extraBuffer.SetData(extraInstances, farStart, farStart, farCount);
            if (far != null) extraArgsData[figures.Length] = Args(far.Asset.Mesh, farCount, farStart);
            extraArgs.SetData(extraArgsData);
            var size = Host.Local.Map.SizeMeters;
            var bounds = new Bounds(new Vector3(size.x * 0.5f, 0f, size.y * 0.5f), new Vector3(size.x + 20f, 60f, size.y + 20f));
            for (int k = 0; k < figures.Length; k++)
            {
                if (extraArgsData[k].instanceCount == 0) continue;
                var fig = figures[k];
                extraProps[k].SetBuffer("_Instances", extraBuffer); extraProps[k].SetBuffer("_RowTable", fig.Rows); extraProps[k].SetFloat(LiftId, ExtrasLift); extraProps[k].SetColor(LiftAId, ExtrasRimA); extraProps[k].SetColor(LiftBId, ExtrasRimB);
                var rp = new RenderParams(fig.Material) { worldBounds = bounds, shadowCastingMode = ShadowsThisFrame ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true, matProps = extraProps[k] };
                FrameBudget.DrawIndirect(rp, fig.Asset.Mesh, extraArgs, 1, k);
            }
            if (farCount > 0 && far != null)
            {
                farExtraProps.SetBuffer("_Instances", extraBuffer); farExtraProps.SetBuffer("_RowTable", far.Rows); farExtraProps.SetFloat(LiftId, ExtrasLift); farExtraProps.SetColor(LiftAId, ExtrasRimA); farExtraProps.SetColor(LiftBId, ExtrasRimB);
                FrameBudget.DrawIndirect(new RenderParams(far.Material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.Off, receiveShadows = true, matProps = farExtraProps }, far.Asset.Mesh, extraArgs, 1, figures.Length);
            }
        }

        static VatInstance Mapped(VatInstance inst, NativeArray<ushort> rowOf)
        {
            if (rowOf.IsCreated && rowOf.Length > 0)
            {
                inst.AnimRow = rowOf[math.clamp((int)inst.AnimRow, 0, rowOf.Length - 1)];
                inst.PrevRow = rowOf[math.clamp((int)inst.PrevRow, 0, rowOf.Length - 1)];
            }
            return inst;
        }

        void ReleaseExtras()
        {
            extraBuffer?.Dispose(); extraBuffer = null;
            extraArgs?.Dispose(); extraArgs = null;
            if (extraInstances.IsCreated) extraInstances.Dispose();
        }
    }
}
