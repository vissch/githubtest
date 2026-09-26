// Phase: B3 / docs/21 phase 4 (implemented) — part of VATRenderer: the fallen. The same figure as the living, played
// once through his death and held on the last frame where he fell, in their own small buffer (the near model up
// close, the box model beyond), no shadows. A man a shell threw flies an arc and tumbles as he goes (whole turns, so
// he lands as his clip left him: the pitch rides in the record's Tint above the team bit, VAT_URP); men who die on
// one 2 m cell pile up, each drawn a little higher, nudged aside and tilted; a man who died alight lies charred
// (VatPad's char bits), smoulders, and is gone sooner, shrinking as the mud takes him.
using System.Collections.Generic;
using Unity.Collections;
using Unity.Mathematics;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;

namespace TW.Presentation.Units
{
    public sealed partial class VATRenderer
    {
        struct FallenMan
        {
            public Vector3 Pos, From; public float Yaw, Born, Seconds, FromT, Fade, Flight, Up, Top, Rate, Spin, Lies, Grime;
            public byte Team, Figure, Gib, Flips, Char; public sbyte Pitch; public ushort Row, FarRow, FromRow; public int Cell;
        }
        static float Hash01(Vector3 v) { float h = Mathf.Sin(v.x * 12.9898f + v.z * 78.233f) * 43758.5453f; return h - Mathf.Floor(h); }
        /// <summary>Gravity for a thrown corpse (m/s2): a little over the real thing, so the arc reads as a blow, not a float.</summary>
        public float ThrowGravity = 14f;
        readonly List<FallenMan> fallenMen = new List<FallenMan>(128);
        public int MaxFallen = 600;
        public float FallenSeconds = 30f;      // a body lies this long, the last SinkSeconds of it sinking into the mud
        public const float SinkSeconds = 2.5f, SinkDepth = 1.1f;
        public float FallSeconds = 0.9f, FallenNearDistance = 70f;
        /// <summary>A charred body lies this long (it is not much to look at), and shrinks as it sinks.</summary>
        public const float CharredLies = 14f, CharredShrink = 0.85f;
        /// <summary>Seconds a man who died alight glows in the cracks before the embers go out (char 3 -> 2).</summary>
        public const float EmberSeconds = 4f;
        /// <summary>The heap: each man already dead on a 2 m cell lifts the next this much (life-size, at UnitScale), up to three.</summary>
        public const float PileStep = 0.28f, PileNudge = 0.4f; public const int PileMax = 3;
        /// <summary>The tumble's pitch steps a turn (VAT_URP decodes the same), and how long a flight is before he turns once and twice.</summary>
        public const int PitchSteps = 32; public const float OneFlipFlight = 0.9f, TwoFlipsFlight = 1.4f;
        public int FallenCount => fallenMen.Count;
        GraphicsBuffer fallenBuffer, fallenArgs;
        NativeArray<VatInstance> fallenInstances;
        GraphicsBuffer.IndirectDrawIndexedArgs[] fallenArgsData;
        readonly List<int>[] fallenByFigure = new List<int>[8];
        NativeArray<byte> pile;   // bodies per 2 m cell of the map
        int pileW, pileL;

        int PileCell(Vector3 at)
        {
            if (!pile.IsCreated)
            {
                var size = Host != null && Host.Local != null ? Host.Local.Map.SizeMeters : new float2(600f, 1200f);
                pileW = Mathf.Max(1, Mathf.CeilToInt(size.x / 2f)); pileL = Mathf.Max(1, Mathf.CeilToInt(size.y / 2f));
                pile = new NativeArray<byte>(pileW * pileL, Allocator.Persistent);
            }
            int cx = Mathf.Clamp((int)(at.x / 2f), 0, pileW - 1), cz = Mathf.Clamp((int)(at.z / 2f), 0, pileL - 1);
            return cz * pileW + cx;
        }

        void Remove(int index)
        {
            var f = fallenMen[index];
            if (pile.IsCreated && f.Cell >= 0 && f.Cell < pile.Length && pile[f.Cell] > 0) pile[f.Cell] = (byte)(pile[f.Cell] - 1);
            fallenMen.RemoveAt(index);
        }

        /// <summary>
        /// A man died here: he goes down facing yaw (radians) and stays. The oldest is taken away past MaxFallen. With the clip
        /// atlas the near tier plays the death the controller chose (clip) on his archetype's figure; the far tier and the box
        /// soldier use the procedural death the variant picks. gib: the limbs a shell took off him, a bit each (1 head,
        /// 2 left arm, 3 right arm, 4 left leg, 5 right leg), carried to the shader in the record's spare float; the mesh
        /// must carry a limb id per vertex (VATBaker writes it to UV1.x) for the cut to show. grime: the mud and soot he wore
        /// (AnimationController.Grime), so a man does not come clean as he dies. density: how many died beside him this
        /// moment (a long flight in a heap turns him over twice). chr: how burned he is (0..3, VatPad).
        /// </summary>
        public void AddFallen(Vector3 pos, float yaw, int team, int variant, Clip clip = Clip.None, int archetype = 0, Clip fromClip = Clip.None, float fromPhase = 0f, float fade = 0f, Vector3 fly = default, int gib = 0, float grime = 0f, int density = 0, int chr = 0)
        {
            if (fallenMen.Count >= MaxFallen) Remove(0);
            ushort farRow = (ushort)((int)AnimRow.Death0 + (variant & 3));
            int figure = figures != null ? math.clamp(FigureOfArchetype(archetype), 0, figures.Length - 1) : 0;
            bool near = clipAtlas && clip != Clip.None;
            float seconds = near ? figures[figure].Asset.RowSeconds[(int)clip] : FallSeconds;
            // the clip he was in as he was hit fades out over the death's first moments (the living instance stops drawing him)
            bool blend = near && fromClip != Clip.None && fade > 0.01f;
            float lies = chr >= 2 ? CharredLies * (0.85f + 0.3f * Hash01(pos)) : FallenSeconds * (0.7f + 0.6f * Hash01(pos));
            var man = new FallenMan { Pos = pos, From = pos, Yaw = yaw, Born = Time.time, Seconds = Mathf.Max(0.1f, seconds), Lies = lies, Team = (byte)team, Figure = (byte)figure, Gib = (byte)(gib & 0xFF), Grime = grime, Char = (byte)Mathf.Clamp(chr, 0, 3), Row = near ? (ushort)clip : farRow, FarRow = farRow,
                FromRow = blend ? (ushort)fromClip : (ushort)0, FromT = fromPhase, Fade = blend ? fade : 0f, Rate = 1f, Cell = -1 };
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
                // and end over end: once on a long flight, twice when the heap he came out of threw him high
                man.Flips = (byte)(man.Flight >= TwoFlipsFlight && density >= 2 ? 2 : man.Flight >= OneFlipFlight ? 1 : 0);
            }
            // the heap: whoever is already down on his cell lifts him, shoves him aside and tilts him
            int cell = PileCell(man.Pos);
            int stacked = pile[cell];
            if (stacked > 0)
            {
                float h = Hash01(man.Pos + new Vector3(1.7f, 0f, 9.2f));
                float a = h * 6.2831853f;
                man.Pos += new Vector3(Mathf.Sin(a), 0f, Mathf.Cos(a)) * PileNudge;
                man.Pos.y += PileStep * UnitScale * Mathf.Min(stacked, PileMax);
                man.Pitch = (sbyte)(h < 0.5f ? -1 : 1);
            }
            pile[cell] = (byte)Mathf.Min(255, stacked + 1);
            man.Cell = cell;
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
            // the mud takes them: a body lies its time, then it is gone (they are in the order they fell, so the front of the list goes first)
            if (FallenSeconds > 0f)
            {
                // each man lies his own 0.7-1.3 of the time, so a barrage's dead do not all go under together
                float now0 = Time.time;
                for (int i = fallenMen.Count - 1; i >= 0; i--)
                {
                    var f = fallenMen[i];
                    if (now0 - f.Born > (f.Lies > 0.01f ? f.Lies : FallenSeconds)) Remove(i);
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

        /// <summary>The pitch step (0..PitchSteps-1) a fallen man is drawn at now: turning end over end while he flies,
        /// his resting tilt on a heap once he is down. Whole turns, so the flight ends at 0 and he lies as his clip left him.</summary>
        public static int PitchStepOf(float age, float flight, int flips, int restPitch)
        {
            if (flight > 0f && flips > 0 && age < flight) return Mathf.RoundToInt(flips * PitchSteps * (age / flight)) & (PitchSteps - 1);
            return restPitch & (PitchSteps - 1);
        }

        static VatInstance Fallen(in FallenMan f, VatAsset tier, ushort row, float now, float scale, Vector3 at)
        {
            float age = now - f.Born;
            float t = Mathf.Clamp01(age * f.Rate / f.Seconds);
            if (tier.Loops(row)) { float frames = Mathf.Max(2f, tier.Frames(row)); t *= (frames - 0.99f) / frames; }   // a looping row: stop on its last frame
            float blend = f.Fade > 0f && row == f.Row ? Mathf.Clamp01(1f - age / f.Fade) : 0f;   // the near tier only: the far rows are another atlas
            // thrown: he turns as he goes through the air and comes to rest the way he landed
            float yaw = f.Yaw;
            if (f.Flight > 0f && f.Spin != 0f) yaw += f.Spin * Mathf.Min(age, f.Flight);
            int pitch = PitchStepOf(age, f.Flight, f.Flips, f.Pitch);
            // charred: the embers go out after EmberSeconds, and the mud takes what is left smaller than it was
            int chr = f.Char == 3 && age > EmberSeconds ? 2 : f.Char;
            float lies = f.Lies > 0.01f ? f.Lies : 30f;
            if (f.Char >= 2) scale *= Mathf.Lerp(1f, CharredShrink, Mathf.Clamp01((age - (lies - SinkSeconds)) / SinkSeconds));
            return new VatInstance { Pos = at, Yaw = yaw, AnimRow = row, AnimT = t, Tint = f.Team + 2f * pitch, Scale = scale, PrevRow = f.FromRow, PrevT = f.FromT, Blend = blend, Pad = VatPad.Pack(f.Gib, f.Grime, (int)(Hash01(f.From) * 255f), chr) };
        }

        void ReleaseFallen()
        {
            fallenBuffer?.Dispose(); fallenArgs?.Dispose();
            if (fallenInstances.IsCreated) fallenInstances.Dispose();
            if (pile.IsCreated) pile.Dispose();
        }
    }
}
