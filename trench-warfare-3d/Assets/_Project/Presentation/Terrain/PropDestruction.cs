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
// The tree-top nicety: when a tree breaks, the sim swaps in the snag and the fallen top the same tick; CombatFx
// throws a toppling crown, so the static fallen top is suppressed until the thrown one has lain and starts to sink.
using System.Collections.Generic;
using UnityEngine;
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
        struct Rule { public DebrisRenderer.Piece Piece; public float Hp, Size, Dust; public int Pieces; public Color Tint; public bool Shelter, Crush; }

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

        BattlefieldProps props;
        SimHost Host => props != null ? props.Host : null;
        Dictionary<BattlefieldKit.Module, Rule> rules;
        readonly List<BattlefieldKit.Module> crushable = new List<BattlefieldKit.Module>(48);
        readonly Dictionary<long, float> damage = new Dictionary<long, float>(256);
        readonly Dictionary<long, int> bagsLeft = new Dictionary<long, int>(32);
        readonly HashSet<long> destroyed = new HashSet<long>();
        readonly List<(int page, int slot, Matrix4x4 m)> found = new List<(int, int, Matrix4x4)>(64);
        readonly List<Vector4> freshBreaks = new List<Vector4>(8);   // xyz where a tree broke, w = when its static top may be drawn
        readonly Dictionary<BattlefieldKit.Module, int> indexOf = new Dictionary<BattlefieldKit.Module, int>();
        bool subscribed;
        uint lastCrushTick = uint.MaxValue;
        public int Collapsed => destroyed.Count;
        /// <summary>Props flattened by tanks and chips thrown by hits that did not finish a prop, for the capture tools.</summary>
        public int Crushed { get; private set; }
        public int Chipped { get; private set; }
        public int BagsBlown { get; private set; }

        static readonly Color Stone = new Color(0.50f, 0.48f, 0.44f), Timber = new Color(0.44f, 0.36f, 0.26f), Sack = new Color(0.56f, 0.51f, 0.39f), Metal = new Color(0.36f, 0.37f, 0.34f), Scrub = new Color(0.42f, 0.38f, 0.26f), Earth = new Color(0.38f, 0.33f, 0.27f);

        void Start()
        {
            props = GetComponent<BattlefieldProps>();
            if (props == null) { enabled = false; return; }
            props.Suppress = Suppress;
        }

        void OnDestroy()
        {
            if (props != null && props.Suppress == (System.Func<BattlefieldKit.Module, Matrix4x4, bool>)Suppress) props.Suppress = null;
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
            // the tracks: once per sim tick, from the tick state
            var w = Host.Local.World;
            if (w.Tick != lastCrushTick) { lastCrushTick = w.Tick; Crush(w); }
        }

        void BuildRules(BattlefieldKit kit)
        {
            rules = new Dictionary<BattlefieldKit.Module, Rule>();
            for (int i = 0; i < kit.Modules.Count; i++) indexOf[kit.Modules[i]] = i;
            void Add(Rule rule, params BattlefieldKit.Module[] modules) { foreach (var m in modules) if (m != null && !rules.ContainsKey(m)) rules[m] = rule; }
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
            // scrub: gone at a touch, a few twigs
            Add(new Rule { Piece = DebrisRenderer.Piece.Shard, Hp = 0.3f, Size = 0.25f, Pieces = 4, Dust = 1.2f, Tint = Scrub, Crush = true },
                kit.bush, kit.tuft, kit.reeds, kit.grass, kit.poppies, kit.cattails, kit.branches);
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
                    Collapse(module, rule, m, origin, power, debris, s, shake);
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
                        Crushed++;
                    }
                }
            }
        }

        void Down(BattlefieldKit.Module module, int page, int slot, long key)
        {
            if (destroyed.Count < MaxRemembered) destroyed.Add(key);
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

        /// <summary>A procedural module is drawn in its material's colour; an imported one in its atlas, so the class's colour stands in.</summary>
        static Color Tint(BattlefieldKit.Module module, Rule rule)
        {
            if (module.Name != null || module.Material == null || !module.Material.HasProperty("_BaseColor")) return rule.Tint;
            var c = module.Material.GetColor("_BaseColor");
            return module.Material.HasProperty("_Pigment") && module.Material.GetFloat("_Pigment") >= 0f ? c * 0.72f : c * 0.9f;   // a painted surface reads darker than its tint
        }
    }
}
