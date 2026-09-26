// Phase: A5 (implemented core) — depends on: MapData (trench / crater cells, heightfield), Suppression.
// Anything that explodes queues an Impact; this system resolves the tick's impacts in one Burst job.
//
// Damage falls from 100 % at the centre to 25 % at the edge of the radius, and is then cut by four things that are
// all about WHERE the man is standing relative to the burst rather than how far away he is:
//
//   THE TRENCH (owner, 2026-09-24: "the trench allways need to stand and allways give limited protection").
//   A shell in a man's own bay leaves him TrenchBayFactor of it -- not all of it, which is what it used to be, and
//   the one number in this file the owner asked for by name. Further along the same trench a traverse takes it down
//   to TrenchTraverseFactor, and a shell out in the field only reaches him at TrenchOutsideFactor. The reverse holds
//   too: a shell that goes off down in a trench is under the parapet, so it barely reaches the men in the open
//   (FieldShadow). A shell hole is CraterFactor, lying down or pinned ProneFactor, as before.
//
//   WHAT STANDS BETWEEN (terrain). A crater lip or a parapet between the burst and the man takes TerrainShadow off
//   it, asked of HeightfieldRaycast. A ray per man in the radius would be the most expensive thing in the tick, so
//   it is asked only past ShadowFrom of the radius, only when there is broken ground at one end or the other to cast
//   a shadow at all, and at most MaxRaycasts times per impact -- a cap taken in slot order, so it is the same cap on
//   every machine. (The same chain is where standing buildings will multiply in, once they are in the sim.)
//
//   WHICH WAY IT WAS GOING (owner: directional explosions). An Impact now carries the shell's flight direction, and
//   the far side of the burst takes 1 + DirBias of the damage while the side it came from takes 1 - DirBias: the
//   fragments carry on. The man is thrown along the same lean. A mortar coming almost straight down, a cook-off and
//   falling masonry have no direction and are unchanged.
//
//   WHOSE MEN ARE BEHIND IT (docs/21 phase 5). A creeping barrage walks ahead of the men who follow it, and the
//   battery knows where they are: an Impact with SafeBehind > 0 leaves the firing player's own men within that
//   many metres behind it (against Dir) untouched. A friend beside or ahead of the burst is as dead as anyone.
//
// Nothing may cut a burst to nothing: MinThrough is the floor, so a shell on top of a dugout still hurts.
// SUPPRESSION IS DELIBERATELY NOT SHADED, only leaned. Keeping your head down is what a man does whether or not the
// parapet saved him, and the M1.5 fun gate tuned a barrage's suppressive weight against the old numbers; shading it
// here would quietly make garrisons much harder to pin, which is not what was asked for.
//
// Vehicles are not touched here: VehicleModulesSystem reads Resolved and puts each burst against the armour.
// Craters are handed to DeformationSystem through Craters.
using Unity.Burst;
using Unity.Collections;
using Unity.Jobs;
using Unity.Mathematics;
using TW.Sim.Terrain;

namespace TW.Sim.Combat
{
    /// <summary>What kind of thing went off. A shell is directional and can hole a tank's top plate; masonry is a
    /// building coming down on the men underneath it, which no roof protects against and which cannot penetrate
    /// armour; a cook-off is the rounds in a hull going up. An incendiary bursts like a shell and then BurningSystem,
    /// which reads Resolved right after this, sets the men and the ground inside its radius alight. A beam's scorch
    /// (BeamSystem) is for the trees, the wire and the picture: the men in a beam are the beam's own, so BlastJob
    /// leaves them alone and VehicleModules leaves the hulls alone.</summary>
    public enum BlastShape : int { Shell = 0, Masonry = 1, CookOff = 2, Incendiary = 3, Beam = 4, Mine = 5 }

    public struct Impact
    {
        public float3 Pos;
        /// <summary>Unit flight direction at the burst, flattened to XZ. Zero means no direction: a cook-off, falling
        /// masonry, or a round coming almost straight down.</summary>
        public float3 Dir;
        public float Damage, Radius, Suppression, CraterRadius, CraterDepth;
        /// <summary>Metres of rubble this burst heaps up where it lands (falling masonry). Zero for a shell, which
        /// digs a hole instead.</summary>
        public float Rubble;
        /// <summary>The firing player's own men within this many metres behind the burst (against Dir) take nothing
        /// from it: a creeping barrage's lift walks ahead of them. Zero (every other burst) spares nobody.</summary>
        public float SafeBehind;
        public int Source;   // ability or weapon id, for the Explosion event
        public int Player;   // who fired it (-1 none); friendly fire is on
        public int Shape;    // BlastShape; 0 = Shell, so every existing call site is unchanged
    }

    /// <summary>The numbers a burst is cut down by. Balance, gathered in one place so a tuning pass is one file.</summary>
    public static class BlastRules
    {
        /// <summary>A shell in a man's own bay. The owner's rule: a trench ALWAYS gives limited protection, so this
        /// is below 1 even for a direct hit in the bay.</summary>
        public const float TrenchBayFactor = 0.7f;
        /// <summary>Same trench, but far enough along it that a traverse is in the way.</summary>
        public const float TrenchTraverseFactor = 0.5f;
        /// <summary>His trench, the shell out in the field.</summary>
        public const float TrenchOutsideFactor = 0.35f;
        /// <summary>How far along a trench counts as the same bay.</summary>
        public const float BayMetres = 12f;
        /// <summary>A shell that went off down inside a trench, reaching a man out in the open.</summary>
        public const float FieldShadow = 0.45f;
        public const float CraterFactor = 0.6f, ProneFactor = 0.7f;
        /// <summary>Falling masonry does not care which trench you are in; a bay is not a roof.</summary>
        public const float MasonryTrenchFactor = 0.5f, MasonryKnock = 0.5f;
        /// <summary>Broken ground between the burst and the man.</summary>
        public const float TerrainShadow = 0.55f;
        /// <summary>Terrain shadowing is only looked for past this share of the radius: close in there is nothing
        /// between them worth the ray.</summary>
        public const float ShadowFrom = 0.4f;
        /// <summary>The deterministic cost bound: at most this many terrain rays per impact, taken in slot order.</summary>
        public const int MaxRaycasts = 48;
        /// <summary>How much more the far side of a directional burst takes, and how much less the near side.</summary>
        public const float DirBias = 0.3f;
        /// <summary>Nothing ever cuts a burst below this: a shell on top of a dugout still hurts.</summary>
        public const float MinThrough = 0.08f;
    }

    public sealed class BlastSystem : ISimSystem
    {
        public int Order => SimSystemOrder.Blast;
        public const float KnockNear = 9f, KnockFar = 3f, KnockReach = 0.85f, KnockMax = 12f;

        readonly MapData map;
        public NativeList<Impact> Pending;         // filled by abilities / indirect fire earlier in the same tick
        public NativeList<CraterStamp> Craters;    // drained by DeformationSystem later in the same tick
        public NativeList<Impact> Resolved;        // this tick's impacts, for every system after this one in the tick (Burning, VehicleModules, Deformation, Mines); cleared here at the top of the next Step, nowhere else
        NativeList<int> killed;
        NativeList<float4> killedKnock;            // xz = the way each dead man was thrown, w = how hard (m/s); parallel to killed
        NativeArray<int> counters;                 // [0] terrain rays asked this tick, [1] of which came back blocked

        public BlastSystem(MapData map) { this.map = map; }

        /// <summary>How many terrain shadow rays the last tick asked for. The cost bound, for the tests.</summary>
        public int LastRaycasts => counters.IsCreated ? counters[0] : 0;
        public int LastShadowed => counters.IsCreated ? counters[1] : 0;

        public void Initialize(SimWorld world)
        {
            Pending = new NativeList<Impact>(32, Allocator.Persistent);
            Craters = new NativeList<CraterStamp>(32, Allocator.Persistent);
            Resolved = new NativeList<Impact>(32, Allocator.Persistent);
            killed = new NativeList<int>(64, Allocator.Persistent);
            killedKnock = new NativeList<float4>(64, Allocator.Persistent);
            counters = new NativeArray<int>(2, Allocator.Persistent);
        }

        public void Queue(Impact impact) => Pending.Add(impact);

        public void Step(SimWorld w)
        {
            Resolved.Clear();   // last tick's are gone; this tick's stay readable until the next Step (docs/21 SIM-D: Mines at 1130 reads them after Deformation at 1000)
            if (Pending.Length == 0) return;
            killed.Clear(); killedKnock.Clear();
            counters[0] = 0; counters[1] = 0;
            new BlastJob
            {
                Count = w.HighWater, Impacts = Pending.AsArray(), Killed = killed, KilledKnock = killedKnock, Counters = counters,
                Position = w.Position, Flags = w.Flags, StanceOf = w.StanceOf, Hp = w.Hp, Suppression = w.Suppression, Knock = w.Knock,
                Team = w.Team, Layers = map.NavLayers, CellTrenchId = map.CellTrenchId, NavWidth = map.NavWidth, NavLength = map.NavLength,
                Height = map.Height,
            }.Run();
            for (int k = 0; k < Pending.Length; k++)
            {
                var im = Pending[k];
                // the event carries the flight direction in Dir.xz and the shape in Dir.y, so the picture can lean
                // the way the shell was going without the presentation having to guess from the weapon id
                w.Events.Add(w.Tick, SimEventType.Explosion, im.Source, im.Player, im.Pos,
                    new float3(im.Dir.x, im.Shape, im.Dir.z), im.Radius);
                Resolved.Add(im);
                if (im.CraterRadius > 0f)
                    Craters.Add(new CraterStamp { Center = im.Pos, Radius = im.CraterRadius, Depth = im.CraterDepth, Kind = (int)CraterKind.Bowl });
                if (im.Rubble > 0f)
                    Craters.Add(new CraterStamp { Center = im.Pos, Radius = im.Radius, Depth = im.Rubble, Kind = (int)CraterKind.Mound });
            }
            // a dead man is thrown the way a live one would have been: the Death event carries it (dir.y = 1 says a
            // blast did it, xz the way, scalar how hard) so the picture can launch him without guessing
            for (int k = 0; k < killed.Length; k++)
            {
                float4 kn = killedKnock[k];
                w.Despawn(killed[k], (int)DeathCause.Blast, new float3(kn.x, 1f, kn.z), kn.w);
            }
            Pending.Clear();
        }

        [BurstCompile(CompileSynchronously = true, FloatMode = FloatMode.Strict, FloatPrecision = FloatPrecision.Standard)]
        struct BlastJob : IJob
        {
            public int Count, NavWidth, NavLength;
            [ReadOnly] public NativeArray<Impact> Impacts;
            [ReadOnly] public NativeArray<float3> Position;
            [ReadOnly] public NativeArray<uint> Flags;
            [ReadOnly] public NativeArray<byte> StanceOf, Layers, Team;
            [ReadOnly] public NativeArray<short> CellTrenchId;
            [ReadOnly] public Heightfield Height;
            public NativeArray<float> Hp, Suppression;
            public NativeArray<float3> Knock;
            public NativeList<int> Killed;
            public NativeList<float4> KilledKnock;
            public NativeArray<int> Counters;

            int CellOf(float3 p)
            {
                int cx = math.clamp((int)(p.x / MapData.NavCellSize), 0, NavWidth - 1);
                int cz = math.clamp((int)(p.z / MapData.NavCellSize), 0, NavLength - 1);
                return cz * NavWidth + cx;
            }

            public void Execute()
            {
                for (int k = 0; k < Impacts.Length; k++)
                {
                    var im = Impacts[k];
                    int burstCell = CellOf(im.Pos);
                    short hitTrench = CellTrenchId[burstCell];
                    bool masonry = im.Shape == (int)BlastShape.Masonry;
                    float3 lean = new float3(im.Dir.x, 0f, im.Dir.z);
                    float leanLen = SimMath.Length(lean);
                    bool directional = !masonry && leanLen > 1e-3f;
                    if (directional) lean /= leanLen;
                    bool burstBroke = (Layers[burstCell] & (byte)(NavLayer.Crater | NavLayer.Trench)) != 0;
                    int rays = 0;
                    if (im.Shape == (int)BlastShape.Beam) continue;   // the scorch under a beam: not for the men (BeamSystem has them)

                    for (int i = 0; i < Count; i++)
                    {
                        uint f = Flags[i];
                        if ((f & (uint)UnitFlags.Alive) == 0 || Hp[i] <= 0f) continue;
                        float3 d = Position[i] - im.Pos; d.y = 0f;
                        float dist = SimMath.Length(d);
                        if (dist >= im.Radius) continue;
                        float falloff = 1f - 0.75f * (dist / im.Radius);
                        int cell = CellOf(Position[i]);
                        if ((f & (uint)UnitFlags.Vehicle) != 0) continue;   // armour: VehicleModulesSystem
                        // ---- whose men are behind it: the lift's own, following it, are spared ------------
                        if (im.SafeBehind > 0f && directional && im.Player >= 0 && Team[i] == (byte)im.Player)
                        {
                            float behind = -math.dot(lean, d);
                            if (behind > 0f && behind <= im.SafeBehind) continue;
                        }

                        // ---- where he is standing ------------------------------------------------------------
                        float protection = 1f;
                        bool inTrench = (f & (uint)UnitFlags.InTrench) != 0;
                        bool prone = StanceOf[i] == (byte)Stance.Prone || StanceOf[i] == (byte)Stance.Pinned;
                        if (masonry)
                        {
                            // a building coming down: the trench under it is a hole to be filled, not a shelter
                            if (inTrench) protection = BlastRules.MasonryTrenchFactor;
                            else if ((Layers[cell] & (byte)NavLayer.Crater) != 0) protection = BlastRules.CraterFactor;
                            else if (prone) protection = BlastRules.ProneFactor;
                        }
                        else if (inTrench)
                        {
                            protection = hitTrench >= 0 && CellTrenchId[cell] == hitTrench
                                ? (dist < BlastRules.BayMetres ? BlastRules.TrenchBayFactor : BlastRules.TrenchTraverseFactor)
                                : BlastRules.TrenchOutsideFactor;
                        }
                        else if ((Layers[cell] & (byte)NavLayer.Crater) != 0) protection = BlastRules.CraterFactor;
                        else if (prone) protection = BlastRules.ProneFactor;

                        // ---- what stands between ------------------------------------------------------------
                        float shade = 1f;
                        // a shell that went off inside a trench is under the parapet: the walls shield the field
                        if (!masonry && !inTrench && hitTrench >= 0 && dist > 1.5f) shade *= BlastRules.FieldShadow;
                        // a crater lip or a parapet in the way, asked of the ground itself and strictly bounded
                        if (dist > BlastRules.ShadowFrom * im.Radius
                            && (burstBroke || (Layers[cell] & (byte)(NavLayer.Crater | NavLayer.Trench)) != 0)
                            && rays < BlastRules.MaxRaycasts)
                        {
                            rays++;
                            float3 from = new float3(im.Pos.x, Height.Sample(im.Pos.x, im.Pos.z) + 1f, im.Pos.z);
                            float3 to = new float3(Position[i].x, Height.Sample(Position[i].x, Position[i].z) + (prone ? 0.3f : 0.5f), Position[i].z);
                            if (!HeightfieldRaycast.HasLineOfSight(Height, from, to)) { shade *= BlastRules.TerrainShadow; Counters[1] = Counters[1] + 1; }
                        }

                        // ---- which way it was going ---------------------------------------------------------
                        float bias = 1f;
                        if (directional && dist > 0.05f) bias = 1f + BlastRules.DirBias * math.dot(lean, d / dist);

                        float through = math.max(BlastRules.MinThrough, protection * shade);
                        Hp[i] = Hp[i] - im.Damage * falloff * through * bias;
                        // suppression is leaned but NOT shaded: see the header
                        Suppression[i] = math.min(100f, Suppression[i] + im.Suppression * falloff * bias);
                        if (Hp[i] <= 0f)
                        {
                            // the dead are thrown from wherever they stood (a bay, a hole, lying down): the sim no
                            // longer moves them, so this is only a record of how hard the burst hit, for the picture
                            float dead = math.lerp(KnockNear * 1.5f, KnockFar, math.min(1f, dist / math.max(0.05f, im.Radius))) * bias;
                            if (masonry) dead *= BlastRules.MasonryKnock;
                            float3 deadAway = dist > 0.05f ? d / dist : new float3(1f, 0f, 0f);
                            if (directional) { deadAway = deadAway + lean * 0.6f; deadAway /= math.max(1e-3f, SimMath.Length(deadAway)); }
                            Killed.Add(i);
                            KilledKnock.Add(new float4(deadAway.x, 0f, deadAway.z, math.min(KnockMax * 1.5f, dead)));
                            continue;
                        }

                        // a man in the open who lives is thrown clear (not in a trench, a shell hole or a vehicle)
                        bool open = (f & ((uint)UnitFlags.Vehicle | (uint)UnitFlags.Emplacement | (uint)UnitFlags.InTrench)) == 0 && (Layers[cell] & (byte)NavLayer.Crater) == 0;
                        float reach = im.Radius * KnockReach;
                        if (open && dist < reach)
                        {
                            float speed = math.lerp(KnockNear, KnockFar, dist / reach) * bias;
                            if (prone) speed *= 0.5f;
                            if (masonry) speed *= BlastRules.MasonryKnock;
                            float3 away = dist > 0.05f ? d / dist : new float3(1f, 0f, 0f);
                            if (directional) { away = away + lean * 0.6f; away /= math.max(1e-3f, SimMath.Length(away)); }
                            float3 thrown = Knock[i] + away * speed;
                            float tl = SimMath.Length(thrown);
                            Knock[i] = tl > KnockMax ? thrown * (KnockMax / tl) : thrown;
                        }
                    }
                    Counters[0] = Counters[0] + rays;
                }
            }
        }

        public ulong Hash(ulong h) => h;   // Pending and Craters are empty between ticks; Hp and Suppression live in SimWorld

        public void Dispose()
        {
            if (Pending.IsCreated) Pending.Dispose();
            if (Craters.IsCreated) Craters.Dispose();
            if (Resolved.IsCreated) Resolved.Dispose();
            if (killed.IsCreated) killed.Dispose();
            if (killedKnock.IsCreated) killedKnock.Dispose();
            if (counters.IsCreated) counters.Dispose();
        }
    }
}
