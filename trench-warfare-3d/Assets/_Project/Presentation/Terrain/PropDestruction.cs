// Phase: B5 (implemented) — the environment breaks. Every drawn prop that is not the sim's (the sim owns the trees, the
// wrecks and the bridge and breaks them itself) has a material class and a strength: a shell burst inside its radius
// wears it down by the same falloff BlastSystem uses on men, and at nothing it collapses: the instance is taken out
// of the draw (BattlefieldProps.Hide) and its volume is handed to DebrisRenderer as rubble, planks, sacks or plates
// that fly out from the burst, with a cloud of dust. Concrete shelters take two field-gun hits; a parapet of bags or a
// length of revetment goes at one close one.
//
// This is presentation only. The sim's cover and blocking come from its own props and nav layers and are untouched:
// a bunker cell is still a bunker after its roof has gone (a gameplay follow-up needs an owner decision). What has
// collapsed is remembered by module and quantised position, never by index: the field is recomposed after every
// crater and BattlefieldProps asks Suppress for each instance it places, so a destroyed prop stays down and a prop
// nowhere near a burst can never be hidden by mistake. Seeded from the event, so a replay collapses the same walls.
//
// The tree-top nicety: when a tree breaks, the sim swaps in the snag and the fallen top the same tick; CombatFx
// throws a toppling crown, so the static fallen top is suppressed for the fall and composed in when the crown lands.
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
        /// <summary>What a kind of prop is made of: the pieces it breaks into, how many at unit volume, how big, how strong.</summary>
        struct Rule { public DebrisRenderer.Piece Piece; public float Hp, Size, Dust; public int Pieces; public Color Tint; }

        /// <summary>Positions are keyed at this grain (m). Finer than the tightest spacing on the field (wire pickets about 2 m).</summary>
        public const float Quantum = 0.25f;
        /// <summary>Collapsed props remembered; past it nothing more is recorded (a later burst still hides the instance until the next composition).</summary>
        public const int MaxRemembered = 2000;
        public const float BlastReach = 1.15f;   // a prop is hit a little beyond the blast radius the men feel
        /// <summary>How long a tree's static fallen top is kept out after the break: the thrown crown falls 1.3 s and lies
        /// until 4.8 s (CombatFx.TreeBreaks), then sinks, and the static top comes in as it goes.</summary>
        public const float CrownFallSeconds = 4.8f;

        BattlefieldProps props;
        SimHost Host => props != null ? props.Host : null;
        Dictionary<BattlefieldKit.Module, Rule> rules;
        readonly Dictionary<long, float> damage = new Dictionary<long, float>(256);
        readonly HashSet<long> destroyed = new HashSet<long>();
        readonly List<(int page, int slot, Matrix4x4 m)> found = new List<(int, int, Matrix4x4)>(64);
        readonly List<Vector4> freshBreaks = new List<Vector4>(8);   // xyz where a tree broke, w = when its static top may be drawn
        readonly Dictionary<BattlefieldKit.Module, int> indexOf = new Dictionary<BattlefieldKit.Module, int>();
        bool subscribed;
        public int Collapsed => destroyed.Count;

        static readonly Color Stone = new Color(0.50f, 0.48f, 0.44f), Timber = new Color(0.44f, 0.36f, 0.26f), Sack = new Color(0.56f, 0.51f, 0.39f), Metal = new Color(0.36f, 0.37f, 0.34f), Scrub = new Color(0.42f, 0.38f, 0.26f);

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
            if (Host == null || Host.Local == null || props.Kit == null) return;
            if (!subscribed) { Host.Events.OnEvent += OnSimEvent; subscribed = true; }
            if (rules == null) BuildRules(props.Kit);
            // a crown has landed: the static fallen top may be composed in now
            float now = Time.time; bool expired = false;
            for (int i = freshBreaks.Count - 1; i >= 0; i--) if (now >= freshBreaks[i].w) { freshBreaks.RemoveAt(i); expired = true; }
            if (expired) props.Recompose();
        }

        void BuildRules(BattlefieldKit kit)
        {
            rules = new Dictionary<BattlefieldKit.Module, Rule>();
            for (int i = 0; i < kit.Modules.Count; i++) indexOf[kit.Modules[i]] = i;
            void Add(Rule rule, params BattlefieldKit.Module[] modules) { foreach (var m in modules) if (m != null && !rules.ContainsKey(m)) rules[m] = rule; }
            // concrete and stone: two close field-gun bursts
            Add(new Rule { Piece = DebrisRenderer.Piece.Rubble, Hp = 2.0f, Size = 0.45f, Pieces = 14, Dust = 5f, Tint = Stone },
                kit.bunker, kit.ruin, kit.pillbox, kit.sodShelter, kit.mgNest, kit.armouredStand, kit.well, kit.wallStub, kit.rebarSlab, kit.barricade);
            // timber: one burst near by
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 0.8f, Size = 0.9f, Pieces = 9, Dust = 3f, Tint = Timber },
                kit.dugout, kit.roof, kit.planks, kit.duckboards, kit.ladder, kit.supplies, kit.looseBoards, kit.bracedPlank, kit.crossedBoards, kit.hatchLid, kit.plankDoor,
                kit.signBoard, kit.graveMarker, kit.stakes, kit.hedgehog, kit.wirePost, kit.knifeRest);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.2f, Size = 0.9f, Pieces = 8, Dust = 3f, Tint = Timber }, kit.TrenchWalls);
            Add(new Rule { Piece = DebrisRenderer.Piece.Plank, Hp = 1.5f, Size = 0.9f, Pieces = 6, Dust = 2.5f, Tint = Timber }, kit.TrenchFloors);
            // bags: burst by a near miss
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 0.9f, Size = 0.55f, Pieces = 8, Dust = 3.5f, Tint = Sack }, kit.sandbags, kit.sandbag, kit.gabion);
            Add(new Rule { Piece = DebrisRenderer.Piece.Sandbag, Hp = 1.0f, Size = 0.55f, Pieces = 10, Dust = 3.5f, Tint = Sack }, kit.TrenchBags);
            // iron: guns, limbers, sheets, the aeroplane
            Add(new Rule { Piece = DebrisRenderer.Piece.Plate, Hp = 1.3f, Size = 0.5f, Pieces = 8, Dust = 3f, Tint = Metal },
                kit.fieldGun, kit.limber, kit.shellStack, kit.corrugated, kit.tankTurret, kit.biplane, kit.wireFence);
            // scrub: gone at a touch, a few twigs
            Add(new Rule { Piece = DebrisRenderer.Piece.Shard, Hp = 0.3f, Size = 0.25f, Pieces = 4, Dust = 1.2f, Tint = Scrub },
                kit.bush, kit.tuft, kit.reeds, kit.grass, kit.poppies, kit.cattails, kit.branches);
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
            if (e.Type == SimEventType.PropChanged)
            {
                if (e.B == (int)TW.Sim.Terrain.PropKind.BrokenTree && freshBreaks.Count < 32) freshBreaks.Add(new Vector4(e.Pos.x, e.Pos.y, e.Pos.z, Time.time + CrownFallSeconds));
                return;
            }
            if (e.Type != SimEventType.Explosion || rules == null || e.Scalar < 1.5f) return;
            var debris = DebrisRenderer.Instance;
            float r = e.Scalar * BlastReach;
            float power = Mathf.Clamp(e.Scalar / 6f, 0.3f, 1.6f);   // a field gun's 6 m is one unit of harm; the HE barrage's 8 m a third more
            var centre = new Vector2(e.Pos.x, e.Pos.z);
            Vector3 blast = new Vector3(e.Pos.x, RenderGround(e.Pos.x, e.Pos.z), e.Pos.z);
            uint salt = e.Tick * 31u;
            foreach (var kv in rules)
            {
                var module = kv.Key; var rule = kv.Value;
                found.Clear();
                props.Within(module, centre, r + 1f, found);
                for (int i = 0; i < found.Count; i++)
                {
                    var (page, slot, m) = found[i];
                    float d = Vector2.Distance(centre, new Vector2(m.m03, m.m23));
                    if (d > r) continue;
                    long key = Key(module, m);
                    if (destroyed.Contains(key)) continue;
                    float hp = damage.TryGetValue(key, out float left) ? left : rule.Hp;
                    hp -= power * (1f - 0.75f * d / r);
                    if (hp > 0f) { damage[key] = hp; continue; }
                    damage.Remove(key);
                    if (destroyed.Count < MaxRemembered) destroyed.Add(key);
                    props.Hide(module, page, slot);
                    Collapse(module, rule, m, blast, power, debris, salt + (uint)i);
                }
            }
        }

        float RenderGround(float x, float z) => Host != null && Host.Local != null ? TW.Presentation.RenderGround.Sample(Host.Local.Map, x, z) : 0f;

        /// <summary>The prop's volume becomes pieces, thrown out from the burst and up, with the dust of a wall coming down.</summary>
        void Collapse(BattlefieldKit.Module module, Rule rule, Matrix4x4 m, Vector3 blast, float power, DebrisRenderer debris, uint salt)
        {
            var b = module.Mesh.bounds;
            Vector3 centre = m.MultiplyPoint3x4(b.center);
            Vector3 size = Vector3.Scale(b.size, m.lossyScale);
            float volume = Mathf.Max(0.05f, size.x * size.y * size.z);
            float scale = Mathf.Clamp(Mathf.Pow(volume, 1f / 3f), 0.3f, 3f);                  // a piece is sized to the prop
            int count = Mathf.Clamp(Mathf.RoundToInt(rule.Pieces * Mathf.Sqrt(volume)), 3, 40);
            Vector3 away = centre - blast; away.y = 0f;
            if (away.sqrMagnitude > 1e-3f) away = away.normalized * 0.6f;
            if (debris != null && debris.Ready)
            {
                debris.Burst(rule.Piece, centre, count, 4.5f + power * 3f, rule.Size * Mathf.Clamp(scale, 0.6f, 1.6f), Tint(module, rule), 45f, 0f, 1.2f, away, salt);
                if (rule.Piece == DebrisRenderer.Piece.Rubble || rule.Piece == DebrisRenderer.Piece.Plank)
                    debris.Burst(DebrisRenderer.Piece.Clod, centre, count / 2, 4f, 0.18f, new Color(0.30f, 0.26f, 0.21f), 20f, 0f, 1.6f, away, salt + 101u);
                float dust = rule.Dust * Mathf.Clamp(scale, 0.5f, 2f);
                debris.Dust?.Invoke(new Vector3(centre.x, blast.y + 0.3f, centre.z), dust);
                debris.Dust?.Invoke(new Vector3(centre.x, blast.y + 0.3f, centre.z) + away * 1.5f, dust * 0.7f);
            }
            if (rule.Piece == DebrisRenderer.Piece.Rubble) CameraShake.Add(centre, 4f * Mathf.Clamp(scale, 0.5f, 2f));
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
