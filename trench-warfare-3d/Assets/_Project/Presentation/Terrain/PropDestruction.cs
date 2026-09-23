// Phase: B5 (implemented) — the environment breaks. Every drawn prop that is not the sim's (the sim owns the trees, the
// wrecks and the bridge and breaks them itself) has a material class and a strength. Four things wear it down:
//   - a burst (Explosion): BlastSystem's falloff, from a grenade up to the HE barrage;
//   - a tank's AP round where it comes down (VehicleFired, scalar 0): a small hard strike;
//   - a tank driving over it (every sim tick, from the tick state): light things are flattened;
//   - and nothing else yet (small arms carry no impact point in the event stream).
// A hit that does not finish a prop still shows: it throws chips and splinters in proportion to the harm. At nothing the
// prop collapses: the instance is taken out of the draw (BattlefieldProps.Hide) and its volume is handed to
// DebrisRenderer as rubble, planks, sacks or plates, with a cloud of dust.
//
// Shelters are different (owner, 2026-09-23: "the shelter should have the sandbags blown off, but otherwise stay fine,
// shelters are reducing artillery damage"): a dugout, a concrete or sod shelter, a pillbox, an MG nest and the earth
// roof over them never collapse. Each hit blows sandbags off the top, until the shelter's stock of bags is spent; the
// loose bags, gabions and crates stood round it are ordinary props and do go.
//
// This is presentation only. The sim's cover comes from its own layers and is untouched. What has collapsed is
// remembered by module and quantised position, never by index: the field is recomposed after every crater and
// BattlefieldProps asks Suppress for each instance it places, so a destroyed prop stays down and a prop nowhere near a
// strike can never be hidden by mistake. Seeded from the event, and the tanks are read from the tick state, never the
// drawn one, so a replay flattens the same fences.
//
// Two props do something other than break. A dud shell cooks off a moment after it is hit or run over: a small burst
// of its own (SceneHooks.CookOff, drawn only) that strikes what is round it in turn, so a pile of duds goes one after
// another. And what a man drops (helmets, tins, spades, boots, a rifle) is not broken but thrown: it flies whole, lands
// and lies, as a house chunk does; seen from close only, as it is drawn only from close.
// The tree-top nicety: when a tree breaks, the sim swaps in the snag and the fallen top the same tick; CombatFx
// throws a toppling crown, so the static fallen top is suppressed until the thrown one has lain and starts to sink.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.Rendering;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Presentation.Terrain
{
    [RequireComponent(typeof(BattlefieldProps))]
    [DefaultExecutionOrder(600)]
    public sealed class PropDestruction : MonoBehaviour
    {
        /// <summary>What a kind of prop is made of: the pieces it breaks into, how many at unit volume, how big, how strong;
        /// whether it is a shelter (it sheds bags and stands) and whether a tank flattens it.</summary>
        struct Rule { public DebrisRenderer.Piece Piece; public float Hp, Size, Dust; public int Pieces; public Color Tint; public bool Shelter, Crush, Tinted, Cook, Kick; }
        /// <summary>A dud that has been set off, going up when it is due.</summary>
        struct Cooking { public Vector3 At; public float Due; public uint Salt; }
        /// <summary>A house chunk whose support has gone, to be looked at when it is due.</summary>
        struct Falling { public HouseKit.Chunk Chunk; public Matrix4x4 House; public float Due; public uint Salt; public int Depth; }

        /// <summary>Positions are keyed at this grain (m). Finer than the tightest spacing on the field (wire pickets about 2 m).</summary>
        public const float Quantum = 0.25f;
        /// <summary>Collapsed props remembered; past it nothing more is recorded (a later burst still hides the instance until the next composition).</summary>
        public const int MaxRemembered = 2000;
        public const float BlastReach = 1.15f;   // a prop is hit a little beyond the blast radius the men feel
        /// <summary>How long a tree's static fallen top is kept out after the break: the thrown crown falls 1.3 s and lies
        /// until 4.8 s (CombatFx.TreeBreaks), then sinks, and the static top comes in as it goes.</summary>
        public const float CrownFallSeconds = 4.8f;
        /// <summary>An AP round coming down: the reach and the harm of the strike where it lands (a field gun's burst is 6 m, 1.0).</summary>
        public const float RoundReach = 1.3f, RoundPower = 1.1f;
        /// <summary>How far round a moving tank's centre light props go under its tracks, and the speed below which it is parked.</summary>
        public const float CrushReach = 2.4f, CrushMinSpeed = 0.25f;
        /// <summary>Sandbags a shelter has to lose, and at most how many one hit blows off.</summary>
        public const int ShelterBags = 16, BagsPerHit = 5;
        /// <summary>How long after a house chunk goes the chunks it carried are looked at, a storey at a time: a house comes
        /// down from the bottom up, not all in the tick of the burst.</summary>
        public const float FallDelay = 0.45f;
        /// <summary>A house chunk that breaks comes away whole and flies as itself (its own mesh and material, one draw
        /// each), lands, bounces once, lies, then sinks; it throws its rubble where it lands. At most this many at once:
        /// past it a chunk goes to rubble on the spot, as other props do.</summary>
        public const int MaxLoose = 48;
        /// <summary>How long a landed chunk lies before it sinks, and how long the sinking takes (s).</summary>
        public const float LooseLie = 18f, LooseSink = 2.5f;
        /// <summary>At most this many house chunks waiting to fall.</summary>
        public const int MaxFalling = 256;
        /// <summary>A dud goes up this long after it is set off (s, and up to as much again), with this reach and harm to
        /// what is round it: a field gun's shell is 6 m and 1.0, a dud on the surface much less.</summary>
        public const float CookDelay = 0.35f, CookReach = 2.6f, CookPower = 0.8f;
        /// <summary>Loose places kept back from what a man drops, for house chunks.</summary>
        public const int KeptForChunks = 16;

        BattlefieldProps props;
        SimHost Host => props != null ? props.Host : null;
        Dictionary<BattlefieldKit.Module, Rule> rules;
        readonly List<BattlefieldKit.Module> crushable = new List<BattlefieldKit.Module>(48);
        readonly Dictionary<long, float> damage = new Dictionary<long, float>(256);
        readonly Dictionary<long, int> bagsLeft = new Dictionary<long, int>(32);
        readonly HashSet<long> destroyed = new HashSet<long>();
        readonly List<(int page, int slot, Matrix4x4 m)> found = new List<(int, int, Matrix4x4)>(64);
        readonly List<(int page, int slot, Matrix4x4 m)> near = new List<(int, int, Matrix4x4)>(4);
        readonly List<Falling> falling = new List<Falling>(32);
        readonly List<Cooking> cooking = new List<Cooking>(8);
        /// <summary>A house chunk in the air or lying where it came down. Moved about the middle of its bounds.</summary>
        struct Loose
        {
            public BattlefieldKit.Module Module; public Rule Rule; public HouseKit.Chunk Chunk;
            public Vector3 Centre, Velocity, Spin, Scale, Pivot;   // Spin: axis times radians a second; Pivot: bounds centre in the mesh
            public Quaternion Rotation;
            public float RestAt; public int Contacts; public uint Salt;
            public Matrix4x4 Matrix => Matrix4x4.TRS(Centre, Rotation, Scale) * Matrix4x4.Translate(-Pivot);
        }
        readonly List<Loose> loose = new List<Loose>(MaxLoose);
        /// <summary>House chunks flying or lying whole right now.</summary>
        public int LooseCount => loose.Count;
        readonly List<Vector4> freshBreaks = new List<Vector4>(8);   // xyz where a tree broke, w = when its static top may be drawn
        readonly Dictionary<BattlefieldKit.Module, int> indexOf = new Dictionary<BattlefieldKit.Module, int>();
        bool subscribed;
        uint lastCrushTick = uint.MaxValue;
        public int Collapsed => destroyed.Count;
        /// <summary>Props flattened by tanks and chips thrown by hits that did not finish a prop, for the capture tools.</summary>
        public int Crushed { get; private set; }
        public int Chipped { get; private set; }
        public int BagsBlown { get; private set; }
        /// <summary>House chunks that came down because what they rested on had gone.</summary>
        public int Fell { get; private set; }
        /// <summary>Duds that have gone up, and dropped things thrown whole, for the capture tools.</summary>
        public int CookedOff { get; private set; }
        public int Kicked { get; private set; }

        static readonly Color Stone = new Color(0.50f, 0.48f, 0.44f), Timber = new Color(0.44f, 0.36f, 0.26f), Sack = new Color(0.56f, 0.51f, 0.39f), Metal = new Color(0.36f, 0.37f, 0.34f), Scrub = new Color(0.42f, 0.38f, 0.26f), Earth = new Color(0.38f, 0.33f, 0.27f);

        void Start()
        {
            props = GetComponent<BattlefieldProps>();
            if (props == null) { enabled = false; return; }
            props.Suppress = Suppress;
            props.MaskOf = MaskOf;
        }

        void OnDestroy()
        {
            if (props != null && props.Suppress == (System.Func<BattlefieldKit.Module, Matrix4x4, bool>)Suppress) props.Suppress = null;
            if (props != null && props.MaskOf == (System.Func<BattlefieldKit.Module, Matrix4x4, int>)MaskOf) props.MaskOf = null;
            if (subscribed && Host != null) Host.Events.OnEvent -= OnSimEvent;
        }

        void Update()
        {
            using var perf = TW.Sim.PerfMarkers.DestructionUpdate.Auto();
            if (Host == null || Host.Local == null || props.Kit == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (rules == null) BuildRules(props.Kit);
            // a crown has landed: the static fallen top may be composed in now
            float now = Time.time; bool expired = false;
            for (int i = freshBreaks.Count - 1; i >= 0; i--) if (now >= freshBreaks[i].w) { freshBreaks.RemoveAt(i); expired = true; }
            if (expired) props.Recompose();
            if (falling.Count > 0) Settle(now);
            if (cooking.Count > 0) Cook(now);
            if (loose.Count > 0) Fly(Mathf.Min(Time.deltaTime, 0.05f), now);
            // the tracks: once per sim tick, from the tick state
            var w = Host.Local.World;
            if (w.Tick != lastCrushTick) { lastCrushTick = w.Tick; Crush(w); }
        }

        void BuildRules(BattlefieldKit kit)
        {
            rules = new Dictionary<BattlefieldKit.Module, Rule>();
            for (int i = 0; i < kit.Modules.Count; i++) indexOf[kit.Modules[i]] = i;
            // a sliced prop is never hit as itself: its chunks are (below)
            void Add(Rule rule, params BattlefieldKit.Module[] modules) { foreach (var m in modules) if (m != null && m.Sliced == null && !rules.ContainsKey(m)) rules[m] = rule; }
            // shelters: they stand, and shed their bags (Hp is how much a hit must do to blow a full load off)
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 1.0f, Size = 0.55f, Pieces = BagsPerHit, Dust = 4f, Tint = Sack, Shelter = true },
                kit.dugout, kit.roof, kit.bunker, kit.sodShelter, kit.pillbox, kit.mgNest, kit.armouredStand);
            // concrete and stone that is not a shelter: two close field-gun bursts
            Add(new Rule { Piece = DebrisRenderer.Piece.Rubble, Hp = 2.0f, Size = 0.45f, Pieces = 14, Dust = 5f, Tint = Stone },
                kit.ruin, kit.well, kit.wallStub, kit.rebarSlab, kit.barricade);
            // light timber: one burst near by, and a tank goes over it
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 0.8f, Size = 0.9f, Pieces = 9, Dust = 3f, Tint = Timber, Crush = true },
                kit.planks, kit.duckboards, kit.ladder, kit.supplies, kit.looseBoards, kit.bracedPlank, kit.crossedBoards, kit.hatchLid, kit.plankDoor,
                kit.signBoard, kit.graveMarker, kit.stakes, kit.hedgehog, kit.wirePost, kit.knifeRest);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.2f, Size = 0.9f, Pieces = 8, Dust = 3f, Tint = Timber }, kit.TrenchWalls);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.5f, Size = 0.9f, Pieces = 6, Dust = 2.5f, Tint = Timber }, kit.TrenchFloors);
            // bags: burst by a near miss; a lone sack goes under a track, a parapet or a gabion does not
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 0.9f, Size = 0.55f, Pieces = 8, Dust = 3.5f, Tint = Sack, Crush = true }, kit.sandbag);
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 0.9f, Size = 0.55f, Pieces = 8, Dust = 3.5f, Tint = Sack }, kit.sandbags, kit.gabion);
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 1.0f, Size = 0.55f, Pieces = 10, Dust = 3.5f, Tint = Sack }, kit.TrenchBags);
            // iron: sheets and fencing flatten; guns, limbers, the aeroplane are shot to pieces
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 1.0f, Size = 0.5f, Pieces = 6, Dust = 2.5f, Tint = Metal, Crush = true }, kit.corrugated, kit.wireFence);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 1.3f, Size = 0.5f, Pieces = 8, Dust = 3f, Tint = Metal },
                kit.fieldGun, kit.limber, kit.shellStack, kit.tankTurret, kit.biplane);
            // stumps, logs and the shell-torn trunk: split timber, stronger than a board; a tank rolls a log, not a stump
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.3f, Size = 0.8f, Pieces = 7, Dust = 2.5f, Tint = Timber }, kit.stumpTall, kit.stumpSplit, kit.stumpMoss, kit.fork);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.3f, Size = 0.8f, Pieces = 7, Dust = 2.5f, Tint = Timber, Crush = true }, kit.fallenLog);
            // stone lying about: a pile of stones scatters, a boulder takes a heavy shell close by
            Add(new Rule { Piece = DebrisRenderer.Piece.Rubble, Hp = 1.2f, Size = 0.3f, Pieces = 8, Dust = 2.5f, Tint = Stone, Crush = true }, kit.stones);
            Add(new Rule { Piece = DebrisRenderer.Piece.Rubble, Hp = 3.2f, Size = 0.5f, Pieces = 12, Dust = 4.5f, Tint = Stone }, kit.boulder);
            // brass: flattened and scattered
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 0.4f, Size = 0.18f, Pieces = 5, Dust = 1f, Tint = Metal, Crush = true }, kit.shellCases);
            // a dud: a little harm sets it off (a tank too), and it goes up (Cook)
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 0.5f, Size = 0.25f, Pieces = 5, Dust = 2f, Tint = Metal, Crush = true, Cook = true }, kit.dudShell);
            // what men drop and hang up: thrown whole by a burst near by, pressed into the mud by a track
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 0.2f, Size = 0.18f, Pieces = 3, Dust = 0.8f, Tint = Metal, Crush = true, Kick = true },
                kit.helmet, kit.messKit, kit.spade, kit.ammoTin, kit.boots, kit.leanRifle, kit.bucket, kit.hangingTins, kit.wireTins, kit.rag);
            // scrub: gone at a touch, a few twigs
            Add(new Rule { Piece = DebrisRenderer.Piece.Shard, Hp = 0.3f, Size = 0.25f, Pieces = 4, Dust = 1.2f, Tint = Scrub, Crush = true },
                kit.bush, kit.tuft, kit.reeds, kit.grass, kit.poppies, kit.cattails, kit.branches);
            // the village houses, chunk by chunk: stone and plaster to rubble, beams, boards and tiles to planks; a little
            // weaker than a lone wall stub (a chunk is a piece of a wall), never flattened by a tank
            var houseStone = new Rule { Piece = DebrisRenderer.Piece.Rubble, Hp = 1.4f, Size = 0.42f, Pieces = 12, Dust = 5f, Tint = Stone, Tinted = true };
            var houseTimber = new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.0f, Size = 0.85f, Pieces = 8, Dust = 3.5f, Tint = Timber, Tinted = true };
            // the rear's military buildings are built to take it: concrete and heavy timber, half as strong again
            var rearStone = houseStone; rearStone.Hp = 2.2f; rearStone.Pieces = 14;
            var rearTimber = houseTimber; rearTimber.Hp = 1.4f;
            // the kit's sliced props, a chunk at a time: the well is stone and timber as painted, the wall stub all stone
            // (its brick reads warm), the biplane wood and canvas, the field gun steel
            var propStone = houseStone; propStone.Hp = 1.2f; propStone.Pieces = 10;
            var propTimber = houseTimber; propTimber.Hp = 0.8f;
            var wing = houseTimber; wing.Hp = 0.7f; wing.Pieces = 6;
            var gunMetal = new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 1.1f, Size = 0.4f, Pieces = 8, Dust = 2.5f, Tint = Metal, Tinted = true };
            foreach (var house in kit.Houses)
                foreach (var chunk in house.Chunks)
                {
                    Rule rule;
                    switch (house.Set)
                    {
                        case "Houses": rule = chunk.Timber ? houseTimber : houseStone; break;
                        case "Military": rule = chunk.Timber ? rearTimber : rearStone; break;
                        default:
                            rule = house.Name == "FieldGun" ? gunMetal : house.Name == "Biplane" ? wing : house.Name == "WallStub" ? propStone
                                : chunk.Timber ? propTimber : propStone;
                            break;
                    }
                    Add(rule, chunk.Module);
                }
            crushable.Clear();
            foreach (var kv in rules) if (kv.Value.Crush) crushable.Add(kv.Key);
        }

        long Key(BattlefieldKit.Module module, in Matrix4x4 m)
        {
            indexOf.TryGetValue(module, out int index);
            long qx = (long)Mathf.RoundToInt(m.m03 / Quantum) & 0xFFFFF, qz = (long)Mathf.RoundToInt(m.m23 / Quantum) & 0xFFFFF;
            return ((long)index << 40) | (qx << 20) | qz;
        }

        /// <summary>BattlefieldProps asks this for every instance it composes: hidden if it has collapsed, or if it is a
        /// tree's fallen top whose crown is still in the air. A pure function of module, position and what has happened.</summary>
        bool Suppress(BattlefieldKit.Module module, Matrix4x4 m)
        {
            if (destroyed.Count > 0 && rules != null && rules.ContainsKey(module) && destroyed.Contains(Key(module, m))) return true;
            if (module == props.Kit?.fallen)
                for (int i = 0; i < freshBreaks.Count; i++)
                {
                    float dx = freshBreaks[i].x - m.m03, dz = freshBreaks[i].z - m.m23;
                    if (dx * dx + dz * dz < 1.5f * 1.5f) return true;
                }
            return false;
        }

        void OnSimEvent(SimEvent e)
        {
            switch (e.Type)
            {
                case SimEventType.PropChanged:
                    if (e.B == (int)TW.Sim.Terrain.PropKind.BrokenTree && freshBreaks.Count < 32) freshBreaks.Add(new Vector4(e.Pos.x, e.Pos.y, e.Pos.z, Time.time + CrownFallSeconds));
                    return;
                case SimEventType.Explosion:
                    // from a grenade (about 1 m of harm, 0.15 of a field gun) to the barrage's 8 m
                    if (rules == null || e.Scalar < 0.3f) return;
                    Strike(new Vector3(e.Pos.x, e.Pos.y, e.Pos.z), e.Scalar * BlastReach, Mathf.Clamp(e.Scalar / 6f, 0.15f, 1.6f), e.Tick * 31u, e.Scalar >= 3f);
                    return;
                case SimEventType.VehicleFired:
                    // an AP round (scalar 0) comes down where it went; HE arrives as its own Explosion
                    if (rules == null || e.Scalar > 0.5f) return;
                    Strike(new Vector3(e.Pos.x, e.Pos.y, e.Pos.z), RoundReach, RoundPower, e.Tick * 37u + (uint)e.A, false);
                    return;
            }
        }

        /// <summary>Harm to every prop within reach of a point, falling off with distance as BlastSystem's does.</summary>
        void Strike(Vector3 at, float reach, float power, uint salt, bool shake)
        {
            var debris = DebrisRenderer.Instance;
            var centre = new Vector2(at.x, at.z);
            Vector3 origin = new Vector3(at.x, GroundAt(at.x, at.z), at.z);
            int n = 0;
            foreach (var kv in rules)
            {
                var module = kv.Key; var rule = kv.Value;
                found.Clear();
                props.Within(module, centre, reach + 1f, found);
                for (int i = 0; i < found.Count; i++)
                {
                    var (page, slot, m) = found[i];
                    float d = Vector2.Distance(centre, new Vector2(m.m03, m.m23));
                    if (d > reach) continue;
                    long key = Key(module, m);
                    if (destroyed.Contains(key)) continue;
                    float harm = power * (1f - 0.75f * d / reach);
                    uint s = salt + (uint)(n++ * 7);
                    if (rule.Shelter) { ShedBags(module, rule, m, key, origin, harm, debris, s); continue; }
                    float hp = damage.TryGetValue(key, out float left) ? left : rule.Hp;
                    hp -= harm;
                    if (hp > 0f) { damage[key] = hp; Chip(module, rule, m, origin, harm, debris, s); continue; }
                    damage.Remove(key);
                    Down(module, page, slot, key);
                    if (rule.Kick) { Toss(module, rule, m, origin, power, debris, s); continue; }
                    if (!props.Kit.HouseChunkOf.ContainsKey(module) || !Throw(module, rule, m, origin, power, harm, s))
                        Collapse(module, rule, m, origin, power, debris, s, shake);
                    if (rule.Cook) SetOff(m, s);
                    Shaken(module, m, s, 0);
                }
            }
        }

        /// <summary>The tracks: every moving vehicle flattens the light props under it. Read from the tick state.</summary>
        void Crush(SimWorld w)
        {
            if (rules == null || crushable.Count == 0) return;
            var debris = DebrisRenderer.Instance;
            uint vehicle = (uint)UnitFlags.Vehicle | (uint)UnitFlags.Alive;
            for (int v = 0; v < w.HighWater; v++)
            {
                if ((w.Flags[v] & vehicle) != vehicle) continue;
                var vel = w.Velocity[v];
                if (vel.x * vel.x + vel.z * vel.z < CrushMinSpeed * CrushMinSpeed) continue;
                var p = w.Position[v];
                var centre = new Vector2(p.x, p.z);
                Vector3 heading = new Vector3(vel.x, 0f, vel.z).normalized;
                for (int k = 0; k < crushable.Count; k++)
                {
                    var module = crushable[k]; var rule = rules[module];
                    found.Clear();
                    props.Within(module, centre, CrushReach, found);
                    for (int i = 0; i < found.Count; i++)
                    {
                        var (page, slot, m) = found[i];
                        long key = Key(module, m);
                        if (destroyed.Contains(key)) continue;
                        damage.Remove(key);
                        Down(module, page, slot, key);
                        Flatten(module, rule, m, heading, debris, w.Tick * 41u + (uint)(v * 13 + i));
                        if (rule.Cook) SetOff(m, w.Tick * 43u + (uint)i);
                        Crushed++;
                    }
                }
            }
        }

        /// <summary>The chunks of the house drawn at this matrix that have come down: exactly the ones remembered as
        /// destroyed (a house chunk is always remembered, past MaxRemembered too, or its house would draw it again).</summary>
        int MaskOf(BattlefieldKit.Module whole, Matrix4x4 m)
        {
            if (destroyed.Count == 0 || props.Kit == null || !props.Kit.HouseOfWhole.TryGetValue(whole, out var house)) return 0;
            int mask = 0;
            var chunks = house.Chunks;
            for (int i = 0; i < chunks.Length; i++)
                if (destroyed.Contains(Key(chunks[i].Module, HouseKit.Place(m, chunks[i])))) mask |= 1 << i;
            return mask;
        }

        void Down(BattlefieldKit.Module module, int page, int slot, long key)
        {
            if (destroyed.Count < MaxRemembered || (props.Kit != null && props.Kit.HouseChunkOf.ContainsKey(module))) destroyed.Add(key);
            props.Hide(module, page, slot);
        }

        float GroundAt(float x, float z) => Host != null && Host.Local != null ? TW.Presentation.RenderGround.Sample(Host.Local.Map, x, z) : 0f;

        static void Measure(BattlefieldKit.Module module, in Matrix4x4 m, out Vector3 centre, out Vector3 size, out float scale, out float volume)
        {
            var b = module.Mesh.bounds;
            centre = m.MultiplyPoint3x4(b.center);
            size = Vector3.Scale(b.size, m.lossyScale);
            volume = Mathf.Max(0.05f, size.x * size.y * size.z);
            scale = Mathf.Clamp(Mathf.Pow(volume, 1f / 3f), 0.3f, 3f);   // a piece is sized to the prop
        }

        static Vector3 Away(Vector3 from, Vector3 to, float lean)
        {
            Vector3 away = to - from; away.y = 0f;
            return away.sqrMagnitude > 1e-3f ? away.normalized * lean : Vector3.zero;
        }

        /// <summary>The prop's volume becomes pieces, thrown out from the strike and up, with the dust of a wall coming down.</summary>
        void Collapse(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, Vector3 origin, float power, DebrisRenderer debris, uint salt, bool shake)
        {
            Measure(module, m, out var centre, out _, out float scale, out float volume);
            int count = Mathf.Clamp(Mathf.RoundToInt(rule.Pieces * Mathf.Sqrt(volume)), 3, 40);
            Vector3 away = Away(origin, centre, 0.6f);
            if (debris != null && debris.Ready)
            {
                debris.Burst(rule.Piece, centre, count, 4.5f + power * 3f, rule.Size * Mathf.Clamp(scale, 0.6f, 1.6f), Tint(module, rule), 45f, 0f, 1.2f, away, salt);
                if (rule.Piece == DebrisRenderer.Piece.Rubble || rule.Piece == DebrisRenderer.Piece.Plank)
                    debris.Burst(DebrisRenderer.Piece.Clod, centre, count / 2, 4f, 0.18f, Earth, 20f, 0f, 1.6f, away, salt + 101u);
                float dust = rule.Dust * Mathf.Clamp(scale, 0.5f, 2f);
                debris.Dust?.Invoke(new Vector3(centre.x, origin.y + 0.3f, centre.z), dust);
                debris.Dust?.Invoke(new Vector3(centre.x, origin.y + 0.3f, centre.z) + away * 1.5f, dust * 0.7f);
            }
            if (shake && rule.Piece == DebrisRenderer.Piece.Rubble) CameraShake.Add(centre, 4f * Mathf.Clamp(scale, 0.5f, 2f));
        }

        /// <summary>A hit that did not finish it: splinters, chips or twigs off the side facing the strike, as many as the harm.</summary>
        void Chip(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, Vector3 origin, float harm, DebrisRenderer debris, uint salt)
        {
            if (debris == null || !debris.Ready) return;
            Measure(module, m, out var centre, out var size, out float scale, out float volume);
            int count = Mathf.Clamp(Mathf.CeilToInt(rule.Pieces * Mathf.Sqrt(volume) * 0.4f * harm / rule.Hp), 1, 12);
            Vector3 away = Away(origin, centre, 0.7f);
            Vector3 face = centre - away * (0.5f * Mathf.Max(size.x, size.z)) + Vector3.up * (size.y * 0.2f);   // the side the strike came from
            var piece = rule.Piece == DebrisRenderer.Piece.Plank ? DebrisRenderer.Piece.Shard : rule.Piece == DebrisRenderer.Piece.Plate ? DebrisRenderer.Piece.Shard : rule.Piece;
            float chip = rule.Piece == DebrisRenderer.Piece.Plank ? 0.45f : rule.Size * 0.5f;
            debris.Burst(piece, face, count, 3.5f + harm * 3f, chip * Mathf.Clamp(scale, 0.6f, 1.3f), Tint(module, rule), 30f, 0f, 1.4f, away, salt);
            debris.Dust?.Invoke(face, rule.Dust * 0.35f * Mathf.Clamp(scale, 0.5f, 1.5f));
            Chipped++;
        }

        /// <summary>A shelter hit: it stands, and the blast blows sandbags off its top until its stock is spent; after that a
        /// hit only throws earth and dust. A concrete shell also loses a few chips of itself.</summary>
        void ShedBags(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, long key, Vector3 origin, float harm, DebrisRenderer debris, uint salt)
        {
            if (debris == null || !debris.Ready) return;
            Measure(module, m, out var centre, out var size, out float scale, out _);
            Vector3 top = centre + Vector3.up * (size.y * 0.42f);
            Vector3 away = Away(origin, centre, 0.8f);
            int left = bagsLeft.TryGetValue(key, out int l) ? l : ShelterBags;
            int bags = Mathf.Min(left, Mathf.Clamp(Mathf.RoundToInt(BagsPerHit * harm / rule.Hp), harm > 0.2f ? 1 : 0, BagsPerHit));
            if (bags > 0)
            {
                debris.Burst(DebrisRenderer.Piece.Sandbag, top, bags, 5f + harm * 3f, rule.Size, Sack, 45f, 0f, 1.5f, away, salt);
                bagsLeft[key] = left - bags;
                BagsBlown += bags;
            }
            debris.Burst(DebrisRenderer.Piece.Clod, top, Mathf.RoundToInt(3f + 6f * harm), 5f, 0.16f, Earth, 20f, 0f, 1.8f, away, salt + 53u);
            if (module == props.Kit.bunker || module == props.Kit.pillbox)
                debris.Burst(DebrisRenderer.Piece.Rubble, top, Mathf.RoundToInt(2f + 3f * harm), 4f, 0.22f, Stone, 30f, 0f, 1.4f, away, salt + 97u);
            debris.Dust?.Invoke(top, rule.Dust * Mathf.Clamp(scale, 0.6f, 1.6f));
        }

        /// <summary>A light prop under a track: pressed flat, its pieces pushed out low to the sides, a little dust.</summary>
        void Flatten(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, Vector3 heading, DebrisRenderer debris, uint salt)
        {
            if (debris == null || !debris.Ready) return;
            Measure(module, m, out var centre, out _, out float scale, out float volume);
            int count = Mathf.Clamp(Mathf.RoundToInt(rule.Pieces * Mathf.Sqrt(volume) * 0.7f), 2, 16);
            Vector3 low = new Vector3(centre.x, GroundAt(centre.x, centre.z) + 0.15f, centre.z);
            debris.Burst(rule.Piece, low, count, 2.2f, rule.Size * Mathf.Clamp(scale, 0.6f, 1.3f), Tint(module, rule), 60f, 0f, 0.35f, heading * 0.5f, salt);
            if (rule.Piece != DebrisRenderer.Piece.Shard) debris.Dust?.Invoke(low, rule.Dust * 0.5f);
        }

        /// <summary>A house chunk has gone: the chunks it carried are looked at a moment later (Settle).</summary>
        void Shaken(BattlefieldKit.Module module, in Matrix4x4 m, uint salt, int depth)
        {
            var kit = props.Kit;
            if (kit == null || !kit.HouseChunkOf.TryGetValue(module, out var chunk) || chunk.Carries.Length == 0) return;
            var house = HouseKit.HouseOf(m, chunk);
            var chunks = kit.Houses[chunk.House].Chunks;
            for (int i = 0; i < chunk.Carries.Length && falling.Count < MaxFalling; i++)
            {
                int j = chunk.Carries[i];
                // a storey at a time, and not all of a storey in the same instant: a beam lets go, then the next
                float jitter = (Hash01(salt + (uint)j * 977u) - 0.5f) * 0.5f * FallDelay;
                falling.Add(new Falling { Chunk = chunks[j], House = house, Due = Time.time + FallDelay * (1f + 0.25f * depth) + jitter + 0.06f * i, Salt = salt * 7u + (uint)j * 131u, Depth = depth + 1 });
            }
        }

        /// <summary>Every house chunk that is due: if it still stands and nothing it rests on does, it comes down, and what
        /// it carried is looked at in turn.</summary>
        void Settle(float now)
        {
            var debris = DebrisRenderer.Instance;
            for (int i = falling.Count - 1; i >= 0; i--)
            {
                var f = falling[i];
                if (now < f.Due) continue;
                falling.RemoveAt(i);
                if (f.Chunk.Grounded || !Standing(f.Chunk, f.House, out int page, out int slot, out var m)) continue;
                var chunks = props.Kit.Houses[f.Chunk.House].Chunks;
                bool held = false;
                foreach (int s in f.Chunk.RestsOn) if (Standing(chunks[s], f.House, out _, out _, out _)) { held = true; break; }
                if (held || !rules.TryGetValue(f.Chunk.Module, out var rule)) continue;
                long key = Key(f.Chunk.Module, m);
                damage.Remove(key);
                Down(f.Chunk.Module, page, slot, key);
                if (!Drop(f.Chunk.Module, rule, m, HouseKit.Place(f.House, f.Chunk).GetPosition() - f.House.GetPosition(), f.Salt))
                    Fall(f.Chunk.Module, rule, m, debris, f.Salt);
                Fell++;
                Shaken(f.Chunk.Module, m, f.Salt, f.Depth);
            }
        }

        /// <summary>Whether a house chunk is still drawn where its house puts it.</summary>
        bool Standing(HouseKit.Chunk chunk, in Matrix4x4 house, out int page, out int slot, out Matrix4x4 m)
        {
            var at = HouseKit.Place(house, chunk);
            near.Clear();
            props.Within(chunk.Module, new Vector2(at.m03, at.m23), 0.3f, near);
            if (near.Count == 0) { page = slot = -1; m = default; return false; }
            (page, slot, m) = near[0];
            return true;
        }

        /// <summary>A dud has been hit or run over: it goes up a moment later.</summary>
        void SetOff(in Matrix4x4 m, uint salt)
        {
            if (cooking.Count >= 32) return;
            cooking.Add(new Cooking { At = m.GetPosition(), Due = Time.time + CookDelay * (1f + Hash01(salt + 17u)), Salt = salt });
        }

        /// <summary>Every dud that is due goes up: its burst is drawn, and it strikes what is round it, the next dud with it.</summary>
        void Cook(float now)
        {
            for (int i = cooking.Count - 1; i >= 0; i--)
            {
                var c = cooking[i];
                if (now < c.Due) continue;
                cooking.RemoveAt(i);
                SceneHooks.CookOff?.Invoke(c.At, CookReach * 0.6f);
                Strike(c.At, CookReach, CookPower, c.Salt * 13u + 5u, false);
                CookedOff++;
            }
        }

        /// <summary>A dropped thing a burst has reached: thrown whole, away from the burst and up, tumbling; it lands, lies
        /// and sinks as a house chunk does. Only while the camera is close enough to draw such things; else it is simply gone.</summary>
        void Toss(BattlefieldKit.Module module, Rule rule, in Matrix4x4 m, Vector3 origin, float power, DebrisRenderer debris, uint salt)
        {
            bool seen = module.MaxDistance == float.PositiveInfinity || SceneHooks.CloseUp > 0f;
            if (!seen) return;
            if (loose.Count >= MaxLoose - KeptForChunks) { Collapse(module, rule, m, origin, power, debris, salt, false); return; }
            var piece = Begin(module, rule, m, salt);
            Vector3 away = piece.Centre - origin; away.y = 0f;
            away = away.sqrMagnitude > 1e-3f ? away.normalized : new Vector3(Hash01(salt) - 0.5f, 0f, Hash01(salt + 1u) - 0.5f).normalized;
            float push = Mathf.Clamp(power, 0.3f, 1.6f);
            piece.Velocity = away * (3f + 4f * push) * (0.6f + 0.6f * Hash01(salt + 3u)) + Vector3.up * (3f + 4f * push) * (0.6f + 0.6f * Hash01(salt + 4u));
            piece.Spin = Random3(salt + 6u) * (6f + 8f * push);
            loose.Add(piece);
            Kicked++;
        }

        static float Hash01(uint x) { x ^= x >> 16; x *= 0x7FEB352Du; x ^= x >> 15; x *= 0x846CA68Bu; x ^= x >> 16; return (x & 0xFFFFFF) / (float)0x1000000; }

        /// <summary>A house chunk a hit has broken off: it is thrown whole, away from the burst and up, tumbling over the
        /// axis across the throw, harder the more the hit had left over. A few chips come off it as it goes.</summary>
        bool Throw(BattlefieldKit.Module module, Rule rule, in Matrix4x4 m, Vector3 origin, float power, float harm, uint salt)
        {
            if (loose.Count >= MaxLoose) return false;
            var piece = Begin(module, rule, m, salt);
            Vector3 away = piece.Centre - origin; away.y = 0f;
            away = away.sqrMagnitude > 1e-3f ? away.normalized : new Vector3(Hash01(salt) - 0.5f, 0f, Hash01(salt + 1u) - 0.5f).normalized;
            float push = Mathf.Clamp(power, 0.3f, 1.6f);
            float side = (Hash01(salt + 2u) - 0.5f) * 0.6f;
            var sideways = Vector3.Cross(Vector3.up, away);
            piece.Velocity = (away + sideways * side) * (2.2f + 3.2f * push) * (0.7f + 0.5f * Hash01(salt + 3u)) + Vector3.up * (1.5f + 3.5f * push) * (0.6f + 0.6f * Hash01(salt + 4u));
            piece.Spin = Vector3.Cross(Vector3.up, away) * (2f + 4f * push * Hash01(salt + 5u)) + Random3(salt + 6u) * 1.2f;
            loose.Add(piece);
            var debris = DebrisRenderer.Instance;
            if (debris != null && debris.Ready)
            {
                Measure(module, m, out var centre, out _, out float scale, out _);
                var chip = rule.Piece == DebrisRenderer.Piece.Plank ? DebrisRenderer.Piece.Shard : rule.Piece;
                debris.Burst(chip, centre, Mathf.Clamp(rule.Pieces / 2, 3, 8), 4f + push * 3f, rule.Size * 0.5f, Tint(module, rule), 25f, 0f, 1.3f, away * 0.6f, salt + 11u);
                debris.Dust?.Invoke(centre, rule.Dust * 0.6f * Mathf.Clamp(scale, 0.5f, 1.5f));
            }
            return true;
        }

        /// <summary>A house chunk with nothing left under it: it tips outward from the middle of its house and drops.</summary>
        bool Drop(BattlefieldKit.Module module, Rule rule, in Matrix4x4 m, Vector3 outward, uint salt)
        {
            if (loose.Count >= MaxLoose) return false;
            var piece = Begin(module, rule, m, salt);
            outward.y = 0f;
            outward = outward.sqrMagnitude > 1e-3f ? outward.normalized : new Vector3(Hash01(salt) - 0.5f, 0f, Hash01(salt + 1u) - 0.5f).normalized;
            piece.Velocity = outward * (0.3f + 0.9f * Hash01(salt + 3u));
            piece.Spin = Vector3.Cross(Vector3.up, outward) * (0.6f + 1.6f * Hash01(salt + 5u)) + Random3(salt + 6u) * 0.3f;
            loose.Add(piece);
            var debris = DebrisRenderer.Instance;
            if (debris != null && debris.Ready)
            {
                Measure(module, m, out var centre, out _, out float scale, out _);
                debris.Dust?.Invoke(centre, rule.Dust * 0.35f * Mathf.Clamp(scale, 0.5f, 1.5f));   // the mortar letting go
            }
            return true;
        }

        static Vector3 Random3(uint salt) => new Vector3(Hash01(salt) - 0.5f, Hash01(salt + 7u) - 0.5f, Hash01(salt + 13u) - 0.5f) * 2f;

        Loose Begin(BattlefieldKit.Module module, Rule rule, in Matrix4x4 m, uint salt)
        {
            var pivot = module.Mesh.bounds.center;
            props.Kit.HouseChunkOf.TryGetValue(module, out var chunk);
            return new Loose { Module = module, Rule = rule, Chunk = chunk, Pivot = pivot, Centre = m.MultiplyPoint3x4(pivot), Rotation = m.rotation, Scale = m.lossyScale, RestAt = -1f, Salt = salt };
        }

        /// <summary>Moves every loose chunk: gravity, a spin that the ground takes out of it, one bounce, a slide to rest; at
        /// the first touch it throws its rubble and dust where it hit. It lies, sinks into the mud and is gone. Drawn here.</summary>
        void Fly(float dt, float now)
        {
            var debris = DebrisRenderer.Instance;
            for (int i = loose.Count - 1; i >= 0; i--)
            {
                var p = loose[i];
                var ext = p.Module.Mesh.bounds.extents;
                if (p.RestAt < 0f)
                {
                    p.Velocity.y -= 9.81f * dt;
                    p.Centre += p.Velocity * dt;
                    float w = p.Spin.magnitude;
                    if (w > 1e-4f) p.Rotation = Quaternion.AngleAxis(w * dt * Mathf.Rad2Deg, p.Spin / w) * p.Rotation;
                    // the lowest corner of its box against the drawn ground under its middle
                    float low = float.MaxValue;
                    for (int c = 0; c < 8; c++)
                    {
                        var corner = new Vector3((c & 1) == 0 ? -ext.x : ext.x, (c & 2) == 0 ? -ext.y : ext.y, (c & 4) == 0 ? -ext.z : ext.z);
                        low = Mathf.Min(low, (p.Rotation * Vector3.Scale(corner, p.Scale)).y);
                    }
                    float ground = GroundAt(p.Centre.x, p.Centre.z);
                    float under = ground - (p.Centre.y + low);
                    if (under > 0f)
                    {
                        p.Centre.y += under;
                        if (p.Contacts == 0 && p.Rule.Kick) { if (debris != null && debris.Ready) debris.Dust?.Invoke(new Vector3(p.Centre.x, ground + 0.1f, p.Centre.z), 0.4f); }   // a thing dropped: it thuds and lies
                        else if (p.Contacts == 0 && debris != null && debris.Ready)
                        {
                            // it breaks up where it hits: most of its rubble, and the dust of it coming down
                            Measure(p.Module, p.Matrix, out var centre, out _, out float scale, out float volume);
                            int count = Mathf.Clamp(Mathf.RoundToInt(p.Rule.Pieces * Mathf.Sqrt(volume) * 0.7f), 3, 24);
                            var at = new Vector3(centre.x, ground + 0.2f, centre.z);
                            var drift = new Vector3(p.Velocity.x, 0f, p.Velocity.z) * 0.15f;
                            debris.Burst(p.Rule.Piece, at, count, 2.5f + Mathf.Min(4f, -p.Velocity.y * 0.35f), p.Rule.Size * Mathf.Clamp(scale, 0.6f, 1.4f), Tint(p.Module, p.Rule), 40f, 0f, 0.9f, drift, p.Salt + 31u);
                            debris.Dust?.Invoke(at, p.Rule.Dust * Mathf.Clamp(scale, 0.5f, 2f));
                            if (p.Rule.Piece == DebrisRenderer.Piece.Rubble && -p.Velocity.y > 5f) CameraShake.Add(at, 1.5f * Mathf.Clamp(scale, 0.5f, 2f));
                        }
                        if (p.Contacts == 0 && p.Velocity.y < -1.5f)
                        {
                            // one bounce: a little of the fall comes back, most of the throw and spin are lost
                            p.Velocity.y = -p.Velocity.y * 0.22f;
                            p.Velocity.x *= 0.45f; p.Velocity.z *= 0.45f;
                            p.Spin *= 0.35f;
                        }
                        else
                        {
                            p.Velocity.y = Mathf.Max(0f, p.Velocity.y);
                            float grip = Mathf.Clamp01(1f - 6f * dt);
                            p.Velocity.x *= grip; p.Velocity.z *= grip; p.Spin *= Mathf.Clamp01(1f - 8f * dt);
                            if (new Vector2(p.Velocity.x, p.Velocity.z).sqrMagnitude < 0.04f && p.Spin.sqrMagnitude < 0.04f) p.RestAt = now;
                        }
                        p.Contacts++;
                    }
                    if (p.Centre.y < ground - 20f) p.RestAt = now - LooseLie;   // fell through the world somehow: sink it now
                }
                else
                {
                    float sinking = now - p.RestAt - LooseLie;
                    if (sinking > LooseSink) { loose.RemoveAt(i); continue; }
                    if (sinking > 0f) p.Centre.y -= dt * (ext.y * 2f * Mathf.Max(p.Scale.y, 0.1f) + 0.3f) / LooseSink;
                }
                loose[i] = p;
            }
            DrawLoose();
        }

        readonly Matrix4x4[] looseMatrices = new Matrix4x4[MaxLoose];
        readonly float[] looseMasks = new float[MaxLoose];
        MaterialPropertyBlock looseBlock;

        /// <summary>Every loose chunk of one house type in one instanced draw: its house's whole mesh, moved so the chunk
        /// sits where it flies, with every other chunk masked off.</summary>
        void DrawLoose()
        {
            var houses = props.Kit.Houses;
            looseBlock ??= new MaterialPropertyBlock();
            for (int h = 0; h < houses.Length; h++)
            {
                var whole = houses[h].Whole;
                if (whole == null) continue;
                int n = 0; Bounds bounds = default;
                for (int i = 0; i < loose.Count; i++)
                {
                    var p = loose[i];
                    if (p.Chunk == null || p.Chunk.House != h) continue;
                    looseMatrices[n] = p.Matrix * Matrix4x4.Translate(-p.Chunk.Offset);
                    looseMasks[n] = houses[h].AllBits & ~(1 << p.Chunk.Index);
                    var b = new Bounds(p.Centre, Vector3.one * 8f);
                    if (n == 0) bounds = b; else bounds.Encapsulate(b);
                    n++;
                }
                if (n == 0) continue;
                for (int i = n; i < MaxLoose; i++) looseMasks[i] = 0f;
                looseBlock.SetFloatArray("_ChunkMask", looseMasks);
                var rp = new RenderParams(whole.Material) { worldBounds = bounds, shadowCastingMode = ShadowCastingMode.On, receiveShadows = true, matProps = looseBlock };
                Graphics.RenderMeshInstanced(rp, whole.Mesh, 0, looseMatrices, n);
            }
            // what men dropped, thrown whole: each kind in one draw of its own mesh
            looseKinds.Clear();
            for (int i = 0; i < loose.Count; i++) if (loose[i].Chunk == null && !looseKinds.Contains(loose[i].Module)) looseKinds.Add(loose[i].Module);
            foreach (var module in looseKinds)
            {
                int n = 0; Bounds bounds = default;
                for (int i = 0; i < loose.Count; i++)
                {
                    if (loose[i].Module != module) continue;
                    looseMatrices[n] = loose[i].Matrix;
                    var b = new Bounds(loose[i].Centre, Vector3.one * 2f);
                    if (n == 0) bounds = b; else bounds.Encapsulate(b);
                    n++;
                }
                var rp = new RenderParams(module.Material) { worldBounds = bounds, shadowCastingMode = module.Shadows ? ShadowCastingMode.On : ShadowCastingMode.Off, receiveShadows = true };
                Graphics.RenderMeshInstanced(rp, module.Mesh, 0, looseMatrices, n);
            }
        }
        readonly List<BattlefieldKit.Module> looseKinds = new List<BattlefieldKit.Module>(8);

        /// <summary>A chunk with nothing under it: its pieces drop out of where it was, slow and spread, with the dust of it landing.</summary>
        void Fall(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, DebrisRenderer debris, uint salt)
        {
            if (debris == null || !debris.Ready) return;
            Measure(module, m, out var centre, out _, out float scale, out float volume);
            int count = Mathf.Clamp(Mathf.RoundToInt(rule.Pieces * Mathf.Sqrt(volume)), 3, 30);
            debris.Burst(rule.Piece, centre, count, 2.2f, rule.Size * Mathf.Clamp(scale, 0.6f, 1.6f), Tint(module, rule), 45f, 0f, 0.5f, default, salt);
            var ground = new Vector3(centre.x, GroundAt(centre.x, centre.z) + 0.3f, centre.z);
            debris.Dust?.Invoke(ground, rule.Dust * Mathf.Clamp(scale, 0.5f, 2f));
        }

        /// <summary>A procedural module is drawn in its material's colour; an imported one in its atlas, so the class's colour
        /// stands in (and always for a rule that says so: the house chunks are unnamed, but drawn from the atlas).</summary>
        static Color Tint(BattlefieldKit.Module module, Rule rule)
        {
            if (rule.Tinted) return rule.Tint;
            if (module.Name != null || module.Material == null || !module.Material.HasProperty("_BaseColor")) return rule.Tint;
            var c = module.Material.GetColor("_BaseColor");
            return module.Material.HasProperty("_Pigment") && module.Material.GetFloat("_Pigment") >= 0f ? c * 0.72f : c * 0.9f;   // a painted surface reads darker than its tint
        }
    }
}
