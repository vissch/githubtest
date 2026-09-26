// Phase: B5 (implemented) — what a stream of bullets does to the field. The other half of PropDestruction: a shell
// arrives once and takes a thing out, but a machine gun holds a line on a parapet for half a minute and has to wear it
// away sack by sack. Everything wears, at its material's rate: sacking, timber and scrub quickly, sheet iron and brass
// slowly, brick and stone slower, poured concrete barely at all. Stop firing and the wearing stops with it; what has
// already gone stays gone.
//
// The sim has no idea where rounds are landing — a miss is one probability roll, and there is no fire map anywhere in
// it. So this builds one, in presentation, from the event stream: every Shot says who was shot at (DirectFire raises
// exactly one per round), and where that man stands is where the rounds are going. Each one drops a round and a bearing
// into a 4 m grid; a pass every WearTicks of the sim's own ticks spends what has gathered. Cost is tied to the grid and
// a budget, never to the rate of fire — there are a few thousand rounds a second in a real battle and a full Strike
// costs a spatial query per rule, so a bullet can never be allowed to cost one.
//
// The one expensive thing, finding what stands in a cell, is done once per cell and kept: fire is sustained on the same
// ground by definition, so a machine gun holding a corner pays for that sweep once and then wears what it found.
//
// Feedback, because being shot at has to read on screen: a prop under fire spits a chip on every pass (one piece, dust
// only as it visibly loses material, the budget shared round-robin so nothing is starved), and light things — a helmet,
// a mess tin, brass, a loose board — are knocked about where they lie. Knocked, not thrown: a bullet must not launch a
// helmet out of the battle, so the prop keeps its place in the field and stays destructible. Its displacement is held
// under half of Quantum, because a prop IS its position rounded to 0.25 m (Key) and a prop that moved further would
// become a different prop and lose everything done to it.
//
// Presentation only, like the rest of the file: the sim's cover and line of sight never hear about any of it.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Presentation.Tactical;

namespace TW.Presentation.Terrain
{
    public sealed partial class PropDestruction
    {
        /// <summary>The fire grid's square (m). Coarse: it stands for "the ground being shot at", not a bullet hole.</summary>
        public const float CellSize = 4f;
        /// <summary>Sim ticks between wear passes (20 Hz / 4 = five a second). Off the tick, not the frame, so wear is the
        /// same on any machine and a replay wears the same walls.</summary>
        public const int WearTicks = 4;
        /// <summary>What one round takes off a thing made of sacking or timber (Erode 1). A machine gun fires 7 a second,
        /// so a sandbag stack (Hp 0.9) goes in about four and a half seconds of it, and a revetment (Hp 1.2) in under six.</summary>
        public const float WearPerRound = 0.03f;
        /// <summary>Rounds a square must take before it is worth finding what stands in it. Stray fire never sweeps: this
        /// is what "under constant fire" means in practice.</summary>
        public const int MinRoundsForSweep = 8;
        /// <summary>Squares worn in one pass, and squares whose contents are remembered at once.</summary>
        public const int MaxWearCells = 3, MaxCachedCells = 64;
        /// <summary>Pieces one pass may throw. DebrisRenderer's pools are ring buffers (Capacity, recycled at the head), so
        /// going over this would not overflow anything — it would quietly push a shell's own debris out of the pool and
        /// leave bursts looking thin. Eight a pass is forty a second, about a fifth of a pool.</summary>
        public const int SpallBudget = 8;
        /// <summary>A prop coughs dust once it has lost this much of itself since the last time, so dust marks losing
        /// material rather than being touched. Chips come every pass regardless: that is the feedback.</summary>
        public const float DustEveryWear = 0.15f;
        /// <summary>Things being knocked about at once, how long one takes to settle (s), and the furthest a round moves
        /// anything. Held under Quantum/2 = 0.125 m: past that a prop's Key changes and it forgets what it has taken.</summary>
        public const int MaxJolts = 48;
        public const float JoltSeconds = 0.25f, JoltReach = 0.08f;
        /// <summary>Wear a shelter must take to lose one more sandbag. ShedBags blows a whole sack off for any hit above
        /// 0.2 and nothing at all below it, and a pass of fire is about 0.04 — so a shelter gathers wear here instead and
        /// sheds on the same terms a shell does, one sack at a time. A machine gun strips a parapet in about a minute.</summary>
        public const float WearPerBag = 0.7f;
        /// <summary>Once the bags are gone the shelter itself starts to go, at concrete's rate: minutes of unbroken fire.
        /// A shell still never collapses a shelter (ShedBags); only a stream of bullets, and only after the sacks.</summary>
        public const float ShelterShellHp = 3f, ShelterShellErode = 0.08f;

        /// <summary>One square of the fire grid: rounds gathered since the last pass, and the bearing they came on.</summary>
        struct Cell { public int Rounds; public float DirX, DirZ; }
        /// <summary>One prop found standing in a square.</summary>
        struct Standing_ { public BattlefieldKit.Module Module; public int Page, Slot; public Matrix4x4 M; }
        /// <summary>What stands in a square, found once and kept while the fire lasts.</summary>
        sealed class Swept { public List<Standing_> Props; public int Cursor; public float Built; }
        /// <summary>A prop a round has knocked, on its way back to where it lies.</summary>
        struct Jolted { public BattlefieldKit.Module Module; public int Page, Slot; public Matrix4x4 Home; public Vector3 Push; public float Born; }

        Cell[] cells; int cellsX, cellsZ;
        readonly Dictionary<int, Swept> swept = new Dictionary<int, Swept>(MaxCachedCells);
        readonly List<int> hot = new List<int>(32);
        readonly List<Jolted> jolts = new List<Jolted>(MaxJolts);
        readonly Dictionary<long, float> shelterWear = new Dictionary<long, float>(16);
        readonly Dictionary<long, float> sinceDust = new Dictionary<long, float>(256);
        System.Comparison<int> byRounds;
        uint lastWearTick = uint.MaxValue;
        int generation = -1;

        /// <summary>Props worn away by small-arms fire, chips thrown by it, and sacks it has taken off shelters.</summary>
        public int Worn { get; private set; }
        public int Spalled { get; private set; }
        public int WornBags { get; private set; }
        /// <summary>Things being knocked about by fire right now.</summary>
        public int Knocked => jolts.Count;
        /// <summary>Squares whose contents are remembered, for the capture tools.</summary>
        public int FireCells => swept.Count;

        /// <summary>Rounds of one weapon needed to wear a thing away, at this strength and this material. The calibration:
        /// a machine gun fires 7 a second, so a sandbag stack (Hp 0.9, Erode 1) is gone in about five seconds of it.</summary>
        public static int RoundsToWear(float hp, float erode) => erode <= 0f ? int.MaxValue : Mathf.CeilToInt(hp / (WearPerRound * erode));
        /// <summary>Rounds needed to take every sandbag off a shelter, and then to open the shelter itself.</summary>
        public static int RoundsToStrip() => Mathf.CeilToInt(ShelterBags * WearPerBag / WearPerRound);
        public static int RoundsToOpenShelter() => RoundsToStrip() + RoundsToWear(ShelterShellHp, ShelterShellErode);

        /// <summary>Where a prop really lies. What is drawn may be a knock in progress (Twitch moves the instance in its
        /// page), and a prop IS its position rounded to Quantum — so anything that turns a found instance into a Key has
        /// to ask this first, or a prop knocked at the moment it is looked at becomes a different prop and forgets
        /// everything done to it. Costs nothing while nothing is being knocked about.</summary>
        Matrix4x4 Home(BattlefieldKit.Module module, int page, int slot, in Matrix4x4 drawn)
        {
            for (int i = 0; i < jolts.Count; i++)
                if (jolts[i].Slot == slot && jolts[i].Page == page && jolts[i].Module == module) return jolts[i].Home;
            return drawn;
        }

        /// <summary>A round is on its way: it lands where the man being shot at stands. One Shot an actual round
        /// (DirectFire), so this runs a few thousand times a second — it stays two array reads and an add.</summary>
        void FireAt(in SimEvent e)
        {
            if (props == null || Host == null || Host.Local == null) return;
            var w = Host.Local.World;
            if (e.B < 0 || e.B >= w.Position.Length) return;
            if (cells == null && !MakeGrid()) return;
            var at = w.Position[e.B];
            int i = CellAt(at.x, at.z);
            if (i < 0) return;
            cells[i].Rounds++;
            cells[i].DirX += e.Dir.x;
            cells[i].DirZ += e.Dir.z;
        }

        bool MakeGrid()
        {
            var map = Host.Local.Map;
            if (map == null) return false;
            var size = map.SizeMeters;
            cellsX = Mathf.Max(1, Mathf.CeilToInt(size.x / CellSize));
            cellsZ = Mathf.Max(1, Mathf.CeilToInt(size.y / CellSize));
            cells = new Cell[cellsX * cellsZ];
            return true;
        }

        int CellAt(float x, float z)
        {
            int cx = Mathf.FloorToInt(x / CellSize), cz = Mathf.FloorToInt(z / CellSize);
            if (cx < 0 || cz < 0 || cx >= cellsX || cz >= cellsZ) return -1;
            return cz * cellsX + cx;
        }

        Vector3 CellCentre(int index)
        {
            int cx = index % cellsX, cz = index / cellsX;
            float x = (cx + 0.5f) * CellSize, z = (cz + 0.5f) * CellSize;
            return new Vector3(x, GroundAt(x, z), z);
        }

        /// <summary>The pass: the squares taking the most fire give up what has gathered in them. Squares over the budget
        /// keep their rounds and are worn next time, so nothing fired is ever lost, only put off.</summary>
        void Wear(SimWorld w)
        {
            if (cells == null || rules == null || props == null) return;
            if (lastWearTick != uint.MaxValue && w.Tick - lastWearTick < WearTicks) return;
            lastWearTick = w.Tick;
            // a new composition renumbers every page and slot: what was found before means nothing now
            if (props.Generation != generation) { swept.Clear(); jolts.Clear(); generation = props.Generation; }

            hot.Clear();
            for (int i = 0; i < cells.Length; i++)
                if (cells[i].Rounds > 0 && (cells[i].Rounds >= MinRoundsForSweep || swept.ContainsKey(i))) hot.Add(i);
            if (hot.Count == 0) return;
            byRounds ??= (a, b) => cells[b].Rounds - cells[a].Rounds;
            hot.Sort(byRounds);

            var debris = DebrisRenderer.Instance;
            for (int n = 0, done = 0; n < hot.Count && done < MaxWearCells; n++)
            {
                int index = hot[n];
                var here = SweepCell(index);
                int rounds = cells[index].Rounds;
                float dx = cells[index].DirX, dz = cells[index].DirZ;
                cells[index].Rounds = 0; cells[index].DirX = 0f; cells[index].DirZ = 0f;
                if (here.Props.Count == 0) continue;   // nothing standing here: costs no budget
                done++;
                var dir = new Vector3(dx, 0f, dz);
                dir = dir.sqrMagnitude > 1e-4f ? dir.normalized : Vector3.forward;
                WearCell(here, rounds, dir, CellCentre(index), debris, w.Tick * 17u + (uint)index * 101u);
            }
        }

        /// <summary>What stands in a square, found once and kept. The expensive call in the file: a spatial query a rule,
        /// as a blast pays. Fire stays on the same ground, so it is paid once and then reused for as long as it lasts.</summary>
        Swept SweepCell(int index)
        {
            if (swept.TryGetValue(index, out var found_)) return found_;
            if (swept.Count >= MaxCachedCells)
            {
                int oldest = -1; float when = float.MaxValue;
                foreach (var kv in swept) if (kv.Value.Built < when) { when = kv.Value.Built; oldest = kv.Key; }
                if (oldest >= 0) swept.Remove(oldest);
            }
            var centre = CellCentre(index);
            var list = new List<Standing_>(16);
            float reach = CellSize * 0.5f + 1.5f;   // a little past the square: a prop leans over its edge
            foreach (var kv in rules)
            {
                found.Clear();
                props.Within(kv.Key, new Vector2(centre.x, centre.z), reach, found);
                for (int i = 0; i < found.Count; i++)
                {
                    var (page, slot, m) = found[i];
                    list.Add(new Standing_ { Module = kv.Key, Page = page, Slot = slot, M = Home(kv.Key, page, slot, m) });
                }
            }
            var made = new Swept { Props = list, Cursor = 0, Built = Time.time };
            swept[index] = made;
            return made;
        }

        /// <summary>Every prop in a square takes its share of the rounds that went into it. The chips are rationed and
        /// handed out round-robin, so a dozen things being shot at all spit, rather than three spitting and nine sitting
        /// there looking bulletproof.</summary>
        void WearCell(Swept here, int rounds, Vector3 dir, Vector3 centre, DebrisRenderer debris, uint salt)
        {
            var list = here.Props;
            int budget = SpallBudget, given = 0;
            for (int n = 0; n < list.Count; n++)
            {
                var it = list[(here.Cursor + n) % list.Count];
                if (!rules.TryGetValue(it.Module, out var rule) || rule.Erode <= 0f) continue;
                long key = KeyOf(it.Module, rule, it.M);
                if (destroyed.Contains(key)) continue;
                uint s = salt + (uint)(n * 13);
                // where the fire is coming from, so chips come off the right face and a thing is knocked the right way
                Vector3 origin = it.M.GetPosition() - dir * 3f;
                origin.y = GroundAt(origin.x, origin.z);
                bool piece = given < budget;

                if (rule.Shelter) { WearShelter(it, rule, key, rounds, origin, debris, s); continue; }

                float harm = rounds * WearPerRound * rule.Erode;
                float hp = damage.TryGetValue(key, out float rest) ? rest : rule.Hp;
                if (rule.Damaged != null || rule.Intact != null)
                {
                    // a section of the lining under fire: the same two steps a shell takes it through, never at once
                    var was = damaged.Contains(key) ? SectionState.Damaged : SectionState.Intact;
                    var state = TrenchSectionRules.Apply(ref hp, rule.Hp, harm, false, was);
                    if (state == SectionState.Gone)
                    {
                        Unjolt(it);
                        Finish(it.Module, rule, it.Page, it.Slot, key, it.M, origin, 0.5f, harm, debris, s, false);
                        Worn++;
                        continue;
                    }
                    if (state == SectionState.Damaged && was == SectionState.Intact)
                    {
                        sectionPieces = 0;
                        Section(it.Module, rule, it.Page, it.Slot, key, it.M, origin, 0.5f, harm, hp, was, state, debris, s, false);
                        continue;
                    }
                    damage[key] = hp;
                }
                else
                {
                    hp -= harm;
                    if (hp <= 0f)
                    {
                        Unjolt(it);
                        Finish(it.Module, rule, it.Page, it.Slot, key, it.M, origin, 0.5f, harm, debris, s, false);
                        Worn++;
                        continue;
                    }
                    damage[key] = hp;
                }
                if (!piece) continue;
                given++;
                sinceDust.TryGetValue(key, out float since); since += harm;
                bool dust = since >= rule.Hp * DustEveryWear;
                sinceDust[key] = dust ? 0f : since;
                Spall(it.Module, rule, it.M, dir, debris, dust, s);
                if (rule.Jolt > 0f) Knock(it, rule, dir, s);
            }
            here.Cursor = list.Count == 0 ? 0 : (here.Cursor + Mathf.Max(1, given)) % list.Count;
        }

        /// <summary>A shelter under fire: it stands, and loses its sandbags one at a time, the same sight a shell makes of
        /// it. Only when they are gone does the shell itself begin to go, and only to bullets, never to a shell.</summary>
        void WearShelter(in Standing_ it, Rule rule, long key, int rounds, Vector3 origin, DebrisRenderer debris, uint salt)
        {
            int left = bagsLeft.TryGetValue(key, out int l) ? l : ShelterBags;
            if (left > 0)
            {
                shelterWear.TryGetValue(key, out float pool);
                pool += rounds * WearPerRound * rule.Erode;
                if (pool >= WearPerBag)
                {
                    pool -= WearPerBag;
                    ShedBags(it.Module, rule, it.M, key, origin, 0.25f, debris, salt);   // 0.25 is exactly one sack's worth
                    WornBags++;
                }
                shelterWear[key] = pool;
                return;
            }
            float harm = rounds * WearPerRound * ShelterShellErode;
            float hp = damage.TryGetValue(key, out float rest) ? rest : ShelterShellHp;
            hp -= harm;
            if (hp > 0f) { damage[key] = hp; return; }
            // the shelter finally opens: it goes as the concrete it is, not as the sacks that were on it
            var shell = rule;
            shell.Shelter = false; shell.Piece = DebrisRenderer.Piece.Rubble; shell.Pieces = 14; shell.Size = 0.45f; shell.Dust = 5f; shell.Tint = Stone;
            Unjolt(it);
            Finish(it.Module, shell, it.Page, it.Slot, key, it.M, origin, 1f, harm, debris, salt, true);
            Worn++;
        }

        /// <summary>A round off the face of something: one chip, and dust only when it has lost enough to show. Deliberately
        /// not Chip, which throws a burst and a puff for every hit — at five passes a second that would push everything
        /// else out of the debris pools.</summary>
        void Spall(BattlefieldKit.Module module, Rule rule, in Matrix4x4 m, Vector3 dir, DebrisRenderer debris, bool dust, uint salt)
        {
            if (debris == null || !debris.Ready) return;
            Measure(module, m, out var centre, out var size, out float scale, out _);
            Vector3 face = centre - dir * (0.5f * Mathf.Max(size.x, size.z)) + Vector3.up * (size.y * 0.15f);
            var piece = rule.Piece == DebrisRenderer.Piece.Plank || rule.Piece == DebrisRenderer.Piece.Plate ? DebrisRenderer.Piece.Shard : rule.Piece;
            float chip = (rule.Piece == DebrisRenderer.Piece.Plank ? 0.3f : rule.Size * 0.4f) * Mathf.Clamp(scale, 0.6f, 1.3f);
            debris.Burst(piece, face, 1, 3.5f, chip, Tint(module, rule), 35f, 0f, 1.2f, dir * 0.6f, salt);
            if (dust) debris.Dust?.Invoke(face, rule.Dust * 0.25f * Mathf.Clamp(scale, 0.5f, 1.5f));
            Spalled++;
        }

        /// <summary>A round knocks a light thing about where it lies: it jumps the way the fire is going and settles back.
        /// It keeps its place in the field and can still be broken; only what is drawn moves.</summary>
        void Knock(in Standing_ it, Rule rule, Vector3 dir, uint salt)
        {
            if (jolts.Count >= MaxJolts) return;
            for (int i = 0; i < jolts.Count; i++)
                if (jolts[i].Page == it.Page && jolts[i].Slot == it.Slot && jolts[i].Module == it.Module) return;   // already jumping
            float amp = JoltReach * Mathf.Clamp01(rule.Jolt) * (0.55f + 0.9f * Hash01(salt + (uint)it.Slot * 31u));
            jolts.Add(new Jolted { Module = it.Module, Page = it.Page, Slot = it.Slot, Home = it.M, Push = dir * amp, Born = Time.time });
        }

        void Unjolt(in Standing_ it)
        {
            for (int i = jolts.Count - 1; i >= 0; i--)
                if (jolts[i].Page == it.Page && jolts[i].Slot == it.Slot && jolts[i].Module == it.Module) jolts.RemoveAt(i);
        }

        /// <summary>Every knocked thing on its way back: out fast, then settling. A prop taken out while it was jumping is
        /// dropped rather than put back, or its slot would come up out of the ground again.</summary>
        void Twitch(float now)
        {
            if (jolts.Count == 0) return;
            for (int i = jolts.Count - 1; i >= 0; i--)
            {
                var j = jolts[i];
                float k = (now - j.Born) / JoltSeconds;
                bool gone = destroyed.Contains(Key(j.Module, j.Home));
                if (gone) { jolts.RemoveAt(i); continue; }
                if (k >= 1f) { props.Move(j.Module, j.Page, j.Slot, j.Home); jolts.RemoveAt(i); continue; }
                float fade = (1f - k) * (1f - k);
                float reach = j.Push.magnitude;
                var pos = j.Home.GetPosition() + j.Push * fade + Vector3.up * (reach * 0.35f * fade);
                var rot = j.Home.rotation;
                if (reach > 1e-4f) rot = Quaternion.AngleAxis(fade * 14f, Vector3.Cross(Vector3.up, j.Push / reach)) * rot;
                props.Move(j.Module, j.Page, j.Slot, Matrix4x4.TRS(pos, rot, j.Home.lossyScale));
            }
        }
    }
}
