// Phase: A4 / C1 (implemented) — depends on: MapData, CraterStamp, MudField, WireBelt, PropDef, GreyboxMapGenerator helpers
// A whole battlefield from a seed and a handful of dials: rolling ground, a river across no man's land with fords
// and a plank bridge, low ground under the water table, shell holes, mud, wire with gaps, a shelled wood (some
// trees standing, most broken or stumps, fallen logs) and wrecks. Everything here is deterministic across machines:
// integer-hash value noise, Unity.Mathematics.Random and SimMath only (validate.py checks the rest). The same params give
// the same MapData.Hash, so a mission is a BattlefieldParams value, not a saved map.
// Trench layout and objectives are the playtest map's (reserve and front line per side).
using System.IO;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    public struct BattlefieldParams
    {
        public uint Seed;
        public float Width, Length;
        public float Forest;      // 0..1 how much of the ground is wooded
        public float Shelling;    // 0..1 crater count, and how much of the wood is already broken
        public float Mud;         // 0..1 share of open ground that is churned to half speed
        public float WaterLevel;  // metres; MapData.NoWater for a dry field
        public bool River;
        public int Wrecks;
        public float Bombardment; // ambient shells per minute on no man's land during the match; 0 = a quiet sector
        public bool Sea;          // the far side of the field is a coast: the map runs on into sand and water and
                                  // team 1's reinforcements land there (SeaLandingSystem)
        /// <summary>
        /// Metres of coast past the end of the layout. ZERO MEANS THE DEFAULT (BattlefieldGenerator.SeaMargin),
        /// so every map that existed before this field keeps the geometry - and the MapData hash - it had.
        ///
        /// It has to be a dial rather than a constant because the landing system reaches further out than the
        /// default coast is wide. SeaLandingSystem puts a craft at StandOff (96 m) seaward of the waterline and
        /// a gunboat at ShipStandOff +- its spread (90 m to 270 m), while 36 m of margin leaves 17 m of water
        /// in Z past ShoreZ. On ShelledForest every run-in and every gunboat is therefore off the map, over
        /// clamped height and outside the nav grid. Nothing breaks, because CellOf and Height.Sample both
        /// clamp - which is why it went unnoticed - but the approach cannot be watched, so the coast cannot
        /// carry a level while this is fixed at 36.
        /// </summary>
        public float SeaMargin;

        public static BattlefieldParams ShelledForest(uint seed) => new BattlefieldParams
        { Seed = seed, Width = 90f, Length = 240f, Forest = 0.55f, Shelling = 0.7f, Mud = 0.5f, WaterLevel = 0.15f, River = true, Wrecks = 3, Bombardment = 8f, Sea = true };

        /// <summary>
        /// THE COAST AS A BATTLEFIELD, rather than as the strip of scenery behind ShelledForest's far trench.
        ///
        /// 300 m of water: the first round number clearing the 270 m a gunboat can lie at, so the craft's run-in
        /// and the ships shelling over it are on the map instead of past its edge. That is the whole reason this
        /// preset exists - the geometry the landing already assumed but never had.
        ///
        /// No river: the water that matters here is the sea, and a second body of it across no man's land reads
        /// as a second sea. Thinner wood than ShelledForest (0.2) because a shore is scrub and wind-bent stumps,
        /// not a forest, and it must not hide the approach. Wider (120 m) so the beach has a frontage to hold.
        ///
        /// Who lands is not a new decision: the owner settled it when the sea was built - it lies beyond the
        /// ENEMY line and their reinforcements ride the boats in. So this is a coastal defence seen from the
        /// attacker's side, with the defender resupplied in plain sight under his own guns.
        /// </summary>
        public static BattlefieldParams Landing(uint seed) => new BattlefieldParams
        { Seed = seed, Width = 120f, Length = 170f, Forest = 0.2f, Shelling = 0.55f, Mud = 0.35f, WaterLevel = 0.15f, River = false, Wrecks = 4, Bombardment = 6f, Sea = true, SeaMargin = 300f };

        /// <summary>
        /// THE WINTER LINE: ground that has frozen, rather than ShelledForest wearing a blizzard.
        ///
        /// Until this existed the snow level was the shelled wood repainted - which meant a river running across
        /// no man's land and a water table under it, on a field whose own profile says `Flooding = 0.25` because
        /// "what water there is has frozen". A flowing river on a frozen battlefield is the single loudest thing
        /// wrong with the snow look, and no shader could fix it because the water was in the MAP.
        ///
        /// WaterLevel = NoWater does both jobs at once: it dries the water table AND suppresses the river, which
        /// the generator gates on `p.River && p.WaterLevel > NoWater`. Mud is 0.15 rather than ShelledForest's
        /// 0.5 because frozen ground does not churn - the half-speed mire is a thaw, not a winter. The wood is
        /// thinner (0.25) so the snowfield reads as open ground with broken timber standing in it.
        ///
        /// The LOOK is not carried here. Biome lives in Presentation (GreyboxTerrainView.Field / Atmosphere), and
        /// TW.Sim cannot see it without closing a reference cycle, so pairing this ground with Biome.Winter is a
        /// job for the mission card rather than for the params.
        /// </summary>
        public static BattlefieldParams WinterLine(uint seed) => new BattlefieldParams
        { Seed = seed, Width = 110f, Length = 240f, Forest = 0.25f, Shelling = 0.5f, Mud = 0.15f, WaterLevel = MapData.NoWater, River = false, Wrecks = 2, Bombardment = 7f, Sea = false };

        public byte[] Serialize()
        {
            using var ms = new MemoryStream();
            using var w = new BinaryWriter(ms);
            w.Write(Seed); w.Write(Width); w.Write(Length); w.Write(Forest); w.Write(Shelling); w.Write(Mud); w.Write(WaterLevel); w.Write(River); w.Write(Wrecks); w.Write(Bombardment); w.Write(Sea);
            w.Write(SeaMargin);   // appended, like Sea before it: an older replay simply has no such field
            return ms.ToArray();
        }

        public static BattlefieldParams Deserialize(byte[] bytes)
        {
            using var r = new BinaryReader(new MemoryStream(bytes));
            return new BattlefieldParams
            {
                Seed = r.ReadUInt32(), Width = r.ReadSingle(), Length = r.ReadSingle(), Forest = r.ReadSingle(), Shelling = r.ReadSingle(),
                Mud = r.ReadSingle(), WaterLevel = r.ReadSingle(), River = r.ReadBoolean(), Wrecks = r.ReadInt32(), Bombardment = r.ReadSingle(),
                Sea = r.BaseStream.Position < r.BaseStream.Length && r.ReadBoolean(),
                SeaMargin = r.BaseStream.Position < r.BaseStream.Length ? r.ReadSingle() : 0f,   // 0 = the default
            };
        }
    }

    public static class BattlefieldGenerator
    {
        public const int MapId = 3;
        // The layout was drawn for a 180 x 480 m field and scales with the params: along the front axis by Length / 480
        // (trench lines, HQ depth, the river's wander), across it by Width / 180 (crossings, wire runs).
        const float ReserveAt = 60f, FrontAt = 140f, HqDepthAt = 34f, BaseHeight = 1.2f;
        const float DugInHeight = 2.3f;   // trench lines and HQs sit on the higher ground, so a 1.8 m trench floor stays above the water table
        const float RiverHalfWidthAt = 10f, RiverBedDepth = 1.7f;
        // The coast, beyond the far end of the layout: dry sand and dunes, then the waterline, then shallows out to
        // the map edge. A craft grounds at the waterline and puts men down on the sand (SeaLandingSystem), so the dry
        // part has to be wide enough to land a company on and to hold the beach obstacles.
        public const float SeaMargin = 36f;     // metres of coast added to the map beyond the layout, when the params do not say
        /// <summary>How much coast this battlefield actually gets. Params win; zero means the default above.</summary>
        public static float SeaMarginOf(in BattlefieldParams p) => p.SeaMargin > 0f ? p.SeaMargin : SeaMargin;
        public const float ShoreAt = 19f;       // where the water meets the sand, measured from the top of the beach
        const float ShallowSlope = 0.085f;      // how fast the bed falls away under the water
        const float SeaFallback = 0.15f;        // the water table a dry field is given when it is made a coast

        public static MapData Create(BattlefieldParams p, Allocator allocator)
        {
            var map = new MapData(MapId, new float2(p.Width, p.Length + (p.Sea ? SeaMarginOf(p) : 0f)), allocator);
            map.WaterLevel = p.WaterLevel;
            if (p.Sea)
            {
                if (map.WaterLevel <= MapData.NoWater) map.WaterLevel = SeaFallback;   // the sea IS the water table: without one nothing would be wet
                map.SeaSide = 1; map.SeaStartZ = p.Length; map.ShoreZ = p.Length + ShoreAt; map.SeaLevel = map.WaterLevel;
            }
            var rng = new Random(math.max(1u, p.Seed * 747796405u + 2891336453u));
            float L = p.Length, W = p.Width, sz = L / 480f, sx = W / 180f;
            float ReserveZ = ReserveAt * sz, FrontZ = FrontAt * sz, HqDepth = HqDepthAt * sz, RiverHalfWidth = RiverHalf(p);
            float[] trenchZ = { ReserveZ, FrontZ, L - FrontZ, L - ReserveZ };
            // Nothing on a battlefield is ruled (owner, 2026-09-21). A trench follows the ground: its line wanders on two
            // octaves of noise (about 36 m and 13 m) and jogs a cell forward or back at traverses spaced 12 to 24 m
            // apart. Ladders stand at uneven intervals, each in a short straight piece so it opens onto clear ground.
            var bends = new int[4][]; var ladders = new bool[4][]; var line = new float[4][];
            for (int t = 0; t < 4; t++)
            {
                var lineRng = new Random(math.max(1u, p.Seed * 2246822519u + (uint)t * 3266489917u));
                int n = map.NavWidth;
                bends[t] = new int[n]; ladders[t] = new bool[n]; line[t] = new float[n];
                uint salt = p.Seed + 40u + (uint)t * 7u;
                int jog = 0, nextJog = lineRng.NextInt(5, 10);
                for (int x = 0; x < n; x++)
                {
                    if (x == nextJog) { jog = math.clamp(jog + (lineRng.NextBool() ? 1 : -1), -1, 1); nextJog = x + lineRng.NextInt(6, 13); }
                    float wx = (x + 0.5f) * MapData.NavCellSize;
                    float wander = 2.3f * (Noise(salt, wx, 0f, 36f) - 0.5f) * 2f + 0.9f * (Noise(salt + 1u, wx, 0f, 13f) - 0.5f) * 2f;
                    bends[t][x] = math.clamp((int)math.round(wander + jog), -3, 3);
                }
                for (int x = lineRng.NextInt(2, 6); x < n - 1; x += lineRng.NextInt(7, 13))
                {
                    ladders[t][x] = true;
                    for (int dx = -2; dx <= 2; dx++) if (x + dx >= 0 && x + dx < n) bends[t][x + dx] = bends[t][x];
                }
                for (int x = 0; x < n; x++) line[t][x] = trenchZ[t] + bends[t][x] * MapData.NavCellSize + 2f;   // the trench's own middle, in metres
            }
            // ---- 1. rolling ground, levelled where men dig in ---------------------------------------------------
            var hf = map.Height;
            for (int z = 0; z < hf.Length; z++)
            for (int x = 0; x < hf.Width; x++)
            {
                float wx = x + 0.5f, wz = z + 0.5f;
                if (p.Sea && wz > map.SeaStartZ) { hf.Set(x, z, BeachHeight(p, map.SeaLevel, map.SeaStartZ, wx, wz)); continue; }
                float h = BaseHeight + 2.6f * (Noise(p.Seed, wx, wz, 70f) - 0.5f) + 0.7f * (Noise(p.Seed + 1u, wx, wz, 18f) - 0.5f);
                float keep = math.min(Ramp(wz - HqDepth, 0f, 16f * sz), Ramp(L - HqDepth - wz, 0f, 16f * sz));   // HQ areas are level
                int nx = math.min(map.NavWidth - 1, x / 2);
                for (int t = 0; t < trenchZ.Length; t++) keep = math.min(keep, Ramp(math.abs(wz - line[t][nx]) - 5f, 0f, 14f * sz));   // dug-in ground follows the trench
                float dugIn = DugInHeight + 0.4f * Noise(p.Seed + 2u, wx, wz, 9f);   // never lower: the trench floor must clear the water table
                hf.Set(x, z, math.lerp(dugIn, h, keep));
            }

            // ---- 2. the river: a channel across the width, two fords and a plank bridge -------------------------
            bool river = p.River && p.WaterLevel > MapData.NoWater;
            float[] crossingX = { W * 0.2f + rng.NextFloat(-15f, 15f) * sx, W * 0.5f + rng.NextFloat(-20f, 20f) * sx, W * 0.8f + rng.NextFloat(-15f, 15f) * sx };
            int bridge = 1;
            float riverShift = rng.NextFloat(-10f, 10f) * sz;   // the meander takes the rest of the room
            if (river)
            {
                float bed = p.WaterLevel - RiverBedDepth;
                for (int z = 0; z < hf.Length; z++)
                for (int x = 0; x < hf.Width; x++)
                {
                    float wx = x + 0.5f, wz = z + 0.5f;
                    float half = RiverHalfAt(p, wx);
                    float ragged = 2.4f * (Noise(p.Seed + 6u, wx, wz, 5f) - 0.5f);   // banks cave in and silt up: never a clean edge
                    float d = math.abs(wz - RiverZ(p, riverShift, wx)) + ragged;
                    if (d >= half) continue;
                    float h = math.lerp(hf.HeightAtCell(x, z), bed, Ramp(half - d, 0f, 5f));
                    for (int c = 0; c < crossingX.Length; c++)
                    {
                        float reach = c == bridge ? 3f : 7f;
                        float top = c == bridge ? p.WaterLevel + 0.3f : p.WaterLevel - 0.4f;   // planks stay dry, a ford is knee deep
                        float on = 1f - Ramp(math.abs(wx - crossingX[c]) - reach, 0f, 3f);
                        h = math.max(h, math.lerp(h, top, on));
                    }
                    hf.Set(x, z, math.min(hf.HeightAtCell(x, z), h));
                }
            }

            // ---- 3. trenches, objectives, spawns (the playtest layout) ------------------------------------------
            GreyboxMapGenerator.AddFireTrench(map, 0, 0, ReserveZ, 0f, next0: 1, next1: -1, offsets: bends[0], ladders: ladders[0]);
            GreyboxMapGenerator.AddFireTrench(map, 0, 1, FrontZ, 0f, next0: 2, next1: 0, offsets: bends[1], ladders: ladders[1]);
            GreyboxMapGenerator.AddFireTrench(map, 1, 2, L - FrontZ, SimMath.Pi, next0: 3, next1: 1, offsets: bends[2], ladders: ladders[2]);
            GreyboxMapGenerator.AddFireTrench(map, 1, 3, L - ReserveZ, SimMath.Pi, next0: -1, next1: 2, offsets: bends[3], ladders: ladders[3]);
            GreyboxMapGenerator.AddLineObjective(map, 0, ObjectiveKind.MainLine, 0, 0, FrontZ, 1, bends[1]);
            GreyboxMapGenerator.AddLineObjective(map, 1, ObjectiveKind.ReserveLine, 0, 0, ReserveZ, 2, bends[0]);
            GreyboxMapGenerator.AddLineObjective(map, 2, ObjectiveKind.HQ, 0, 0, 20f * sz, 3);
            GreyboxMapGenerator.AddLineObjective(map, 3, ObjectiveKind.MainLine, 1, 1, L - FrontZ, 1, bends[2]);
            GreyboxMapGenerator.AddLineObjective(map, 4, ObjectiveKind.ReserveLine, 1, 1, L - ReserveZ, 2, bends[3]);
            GreyboxMapGenerator.AddLineObjective(map, 5, ObjectiveKind.HQ, 1, 1, L - 20f * sz, 3);
            map.Spawns.Add(new SpawnPoint { Team = 0, Pos = new float3(W * 0.5f, 0f, 6f), Kind = 0 });
            map.Spawns.Add(new SpawnPoint { Team = 1, Pos = new float3(W * 0.5f, 0f, L - 6f), Kind = 0 });
            map.SupplyRoad.Add(new float3(W * 0.5f, 0f, 0f)); map.SupplyRoad.Add(new float3(W * 0.5f, 0f, 40f * sz));
            map.SupplyRoad.Add(new float3(W * 0.5f, 0f, L)); map.SupplyRoad.Add(new float3(W * 0.5f, 0f, L - 40f * sz));
            map.Wind = new float2(0f, -1f);

            // ---- 4. standing water: the river and the lowest hollows. Only here may water close a cell. ---------
            map.ApplyWater(0, 0, map.NavWidth - 1, map.NavLength - 1, allowBlock: true);

            // ---- 5. years of shelling (craters fill where they go under the water table, never block) ----------
            // Shells come in salvos, not as even rain: a battery fires on one spot, so holes crowd together, overlap and
            // chain, and the ground between salvos is left alone. Each salvo is one big hole, a few medium, more small.
            int salvos = math.max(3, (int)(p.Shelling * 0.00075f * W * L));
            for (int k = 0; k < salvos; k++)
            {
                float cz = L * 0.5f + (rng.NextFloat() + rng.NextFloat() - 1f) * L * 0.42f;   // thickest in no man's land
                float cx = rng.NextFloat(4f, W - 4f);
                float spread = rng.NextFloat(4f, 9f);
                int count = rng.NextInt(3, 8);
                for (int i = 0; i < count; i++)
                {
                    float angle = rng.NextFloat(0f, 6.2831853f), dist = spread * math.sqrt(rng.NextFloat());
                    float x = cx + SimMath.Cos(angle) * dist, z = cz + SimMath.Sin(angle) * dist * 0.75f;
                    float radius = i == 0 ? rng.NextFloat(3.8f, 5.2f) : i < 3 ? rng.NextFloat(2.4f, 3.4f) : rng.NextFloat(1.3f, 2.1f);
                    if (x < 3f || x > W - 3f || z < HqDepth || z > L - HqDepth) continue;
                    bool onTrench = false;
                    int nx = math.clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1);
                    for (int t = 0; t < 4; t++) if (math.abs(z - line[t][nx]) < radius + 4f) onTrench = true;   // the generator leaves the garrison's trench whole
                    if (onTrench) continue;
                    new CraterStamp { Center = new float3(x, 0f, z), Radius = radius, Depth = radius * 0.38f }.Apply(map);
                }
            }

            // ---- 6. mud, by patches ------------------------------------------------------------------------------
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                float wx = (x + 0.5f) * MapData.NavCellSize, wz = (z + 0.5f) * MapData.NavCellSize;
                if (wz < HqDepth || wz > L - HqDepth) continue;
                if (Noise(p.Seed + 7u, wx, wz, 34f) >= p.Mud * 0.42f) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Bunker | NavLayer.Blocked)) != 0) continue;
                map.SetLayer(x, z, layer | NavLayer.Mud);
            }

            // ---- 7. wire in front of both front lines: it follows the trench at a wandering distance, thickens and
            // thins, and is broken by gaps of uneven width -------------------------------------------------------------
            for (int side = 0; side < 2; side++)
            {
                int t = side == 0 ? 1 : 2; float outward = side == 0 ? 1f : -1f;
                uint salt = p.Seed + 21u + (uint)side * 5u;
                int col = 0;
                while (col < map.NavWidth)
                {
                    int run = (int)(rng.NextFloat(14f, 30f) * sx) + 3;
                    for (int x = col; x < math.min(map.NavWidth, col + run); x++)
                    {
                        float wx = (x + 0.5f) * MapData.NavCellSize;
                        float away = 11f + 3.2f * (Noise(salt, wx, 0f, 17f) - 0.5f) * 2f;
                        bool thick = Noise(salt + 1u, wx, 0f, 9f) > 0.55f;
                        float z0 = line[t][x] + outward * away, z1 = z0 + outward * (thick ? 3.9f : 1.9f);
                        new WireBelt { Min = new float3(x * MapData.NavCellSize, 0f, math.min(z0, z1)), Max = new float3(x * MapData.NavCellSize + 1.9f, 0f, math.max(z0, z1)) }.Place(map);
                    }
                    col += run + rng.NextInt(3, 7);   // the gap
                }
            }

            // ---- 8. what is left of the wood, in clumps ----------------------------------------------------------
            // Trees do not stand on a grid. Clump centres are Poisson-disc spaced (no two within 11 m) inside the noise
            // patches that were wood; each clump has one big tree, a ring of medium ones and a scatter of small stumps
            // and logs, thinning outward. Shelling decides how much of each clump is still standing.
            var centres = new System.Collections.Generic.List<float2>();
            int tries = (int)(W * (L - 2f * HqDepth) / 12f);
            for (int k = 0; k < tries; k++)
            {
                var c = new float2(rng.NextFloat(3f, W - 3f), rng.NextFloat(HqDepth, L - HqDepth));
                float wood = Noise(p.Seed + 11u, c.x, c.y, 46f) * 0.7f + Noise(p.Seed + 12u, c.x, c.y, 15f) * 0.3f;
                if (wood < 1f - p.Forest * 0.78f) continue;
                bool crowded = false;
                for (int j = 0; j < centres.Count; j++) if (math.lengthsq(centres[j] - c) < 11f * 11f) { crowded = true; break; }
                if (!crowded) centres.Add(c);
            }
            for (int k = 0; k < centres.Count; k++)
            {
                int members = rng.NextInt(4, 10);
                for (int i = 0; i < members; i++)
                {
                    // 0 = the big one at the heart, 1-2 medium close by, the rest small and further out
                    float reach = i == 0 ? 0f : i < 3 ? rng.NextFloat(2.2f, 4.5f) : rng.NextFloat(3.5f, 8.5f);
                    float angle = rng.NextFloat(0f, 6.2831853f), roll = rng.NextFloat(), size = rng.NextFloat();
                    float x = centres[k].x + SimMath.Cos(angle) * reach, z = centres[k].y + SimMath.Sin(angle) * reach;
                    if (x < 3f || x > W - 3f || !ClearForProp(map, p, riverShift, river, line, x, z)) continue;
                    PropKind kind; float scale;
                    if (i == 0) { kind = roll < p.Shelling * 0.55f ? PropKind.BrokenTree : PropKind.Tree; scale = 1.25f + 0.35f * size; }
                    else if (i < 3) { kind = roll < p.Shelling * 0.75f ? PropKind.BrokenTree : PropKind.Tree; scale = 0.8f + 0.25f * size; }
                    else { kind = roll < 0.55f ? PropKind.Stump : roll < 0.85f ? PropKind.Log : PropKind.BrokenTree; scale = 0.5f + 0.3f * size; }
                    map.AddProp(new PropDef { Pos = new float3(x, 0f, z), Yaw = angle, Kind = kind, Scale = scale });
                }
            }
            for (int k = 0; k < p.Wrecks; k++)
            {
                float x = rng.NextFloat(20f, W - 20f), z = rng.NextFloat(FrontZ + 30f * sz, L - FrontZ - 30f * sz), yaw = rng.NextFloat(0f, 6.2831853f);
                if (ClearForProp(map, p, riverShift, river, line, x, z)) map.AddProp(new PropDef { Pos = new float3(x, 0f, z), Yaw = yaw, Kind = PropKind.Wreck });
            }
            if (river) map.AddProp(new PropDef { Pos = new float3(crossingX[bridge], 0f, RiverZ(p, riverShift, crossingX[bridge])), Yaw = 0f, Kind = PropKind.Bridge });

            map.RebuildCost();
            // last, after the trenches are cut and after the authored shelling above: from here on a shell may not
            // dig the ground a trench stands in, and none may dig through the bedrock
            map.BuildTrenchDistance();
            map.ComputeBedrock();
            map.RebuildCover();
            return map;
        }

        /// <summary>The coast beyond the layout: the rear's own height at the top, falling to the waterline at
        /// ShoreAt and on down under the water. Runnels and low banks of sand cross it, strongest where it is dry.
        /// It starts at exactly the height the rear ground has, so the beach joins the field without a step.</summary>
        public static float BeachHeight(BattlefieldParams p, float seaLevel, float seaStart, float x, float z)
        {
            float t = z - seaStart;
            float top = DugInHeight + 0.4f * Noise(p.Seed + 2u, x, z, 9f);            // the same expression the rear ground uses
            float y = t <= ShoreAt ? math.lerp(top, seaLevel, Ramp(t, 0f, ShoreAt)) : seaLevel - (t - ShoreAt) * ShallowSlope;
            float dry = math.saturate(1.15f - t / ShoreAt);
            return y + (Noise(p.Seed + 31u, x, z, 7.5f) - 0.5f) * 0.34f * dry
                     + (Noise(p.Seed + 32u, x * 0.4f, z, 2.8f) - 0.5f) * 0.11f;       // ribs of sand, running with the shore
        }

        /// <summary>Solid props stay off trench approaches (ladders must not be sealed), the river and its crossings,
        /// wire, water and the supply road.</summary>
        static bool ClearForProp(MapData map, BattlefieldParams p, float riverShift, bool river, float[][] line, float x, float z)
        {
            int nx = math.clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1);
            for (int t = 0; t < line.Length; t++) if (math.abs(z - line[t][nx]) < 8f) return false;
            if (river && math.abs(z - RiverZ(p, riverShift, x)) < RiverHalfAt(p, x) + 5f) return false;
            if (math.abs(x - p.Width * 0.5f) < 5f && (z < ReserveAt * p.Length / 480f || z > p.Length - ReserveAt * p.Length / 480f)) return false;
            var layer = map.LayerAt(new float3(x, 0f, z));
            return (layer & (NavLayer.Wire | NavLayer.Blocked | NavLayer.Trench | NavLayer.Link | NavLayer.Crater)) == 0 &&   // nothing is left standing in a shell hole
                   map.WaterDepthAtCell(map.NavCellOf(new float3(x, 0f, z)).x, map.NavCellOf(new float3(x, 0f, z)).y) < MapData.WetDepth;
        }

        static float RiverHalf(BattlefieldParams p) => RiverHalfWidthAt * math.max(0.6f, p.Length / 480f);

        /// <summary>The river's half width where it crosses x: it pools wide and pinches narrow.</summary>
        static float RiverHalfAt(BattlefieldParams p, float x) => RiverHalf(p) * (0.7f + 0.75f * Noise(p.Seed + 5u, x, 0f, 22f));

        /// <summary>The river meanders: a slow swing (about 40 m) with a quicker one (about 15 m) on top.</summary>
        static float RiverZ(BattlefieldParams p, float shift, float x)
            => p.Length * 0.5f + shift + (p.Length / 480f) * (44f * (Noise(p.Seed + 3u, x, 0f, 40f) - 0.5f) + 17f * (Noise(p.Seed + 4u, x, 0f, 15f) - 0.5f));

        /// <summary>True when infantry can walk from one spawn to the other (trenches through their ladders).</summary>
        public static bool Connected(MapData map)
        {
            int n = map.NavWidth * map.NavLength;
            var seen = new NativeArray<byte>(n, Allocator.Temp);
            var open = new NativeList<int>(1024, Allocator.Temp);
            int start = -1, goal = -1;
            for (int i = 0; i < map.Spawns.Length; i++)
            {
                var c = map.NavCellOf(map.Spawns[i].Pos);
                if (map.Spawns[i].Team == 0) start = map.NavIndex(c.x, c.y); else goal = map.NavIndex(c.x, c.y);
            }
            bool found = false;
            if (start >= 0 && goal >= 0)
            {
                open.Add(start); seen[start] = 1;
                while (open.Length > 0 && !found)
                {
                    int cell = open[open.Length - 1]; open.Length = open.Length - 1;
                    if (cell == goal) { found = true; break; }
                    int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
                    for (int d = 0; d < 4; d++)
                    {
                        int nx = cx + (d == 0 ? 1 : d == 1 ? -1 : 0), nz = cz + (d == 2 ? 1 : d == 3 ? -1 : 0);
                        if (nx < 0 || nz < 0 || nx >= map.NavWidth || nz >= map.NavLength) continue;
                        int ni = map.NavIndex(nx, nz);
                        if (seen[ni] != 0) continue;
                        byte from = map.NavLayers[cell], to = map.NavLayers[ni];
                        if ((to & (byte)NavLayer.Blocked) != 0) continue;
                        const byte traversal = (byte)(NavLayer.Surface | NavLayer.Trench);
                        bool link = ((from | to) & (byte)NavLayer.Link) != 0;
                        if (!link && (from & to & traversal) == 0) continue;   // FlowField.CanStepInfantry, which lives above this assembly
                        seen[ni] = 1; open.Add(ni);
                    }
                }
            }
            seen.Dispose(); open.Dispose();
            return found;
        }

        // ---- deterministic value noise ---------------------------------------------------------------------------
        static float Lattice(uint seed, int x, int z)
        {
            uint h = seed * 0x9E3779B1u ^ (uint)x * 0x85EBCA77u ^ (uint)z * 0xC2B2AE3Du;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12; h *= 0x297A2D39u; h ^= h >> 15;
            return (h & 0xFFFFFFu) * (1f / 16777216f);
        }

        /// <summary>Smooth value noise in 0..1 with features about <paramref name="cell"/> metres across.</summary>
        public static float Noise(uint seed, float x, float z, float cell)
        {
            float fx = x / cell, fz = z / cell;
            int x0 = (int)SimMath.Floor(fx), z0 = (int)SimMath.Floor(fz);
            float tx = fx - x0, tz = fz - z0;
            tx = tx * tx * (3f - 2f * tx); tz = tz * tz * (3f - 2f * tz);
            float a = math.lerp(Lattice(seed, x0, z0), Lattice(seed, x0 + 1, z0), tx);
            float b = math.lerp(Lattice(seed, x0, z0 + 1), Lattice(seed, x0 + 1, z0 + 1), tx);
            return math.lerp(a, b, tz);
        }

        /// <summary>0 at or below <paramref name="from"/>, 1 at or above from + width, smooth in between.</summary>
        static float Ramp(float v, float from, float width)
        {
            float t = math.saturate((v - from) / width);
            return t * t * (3f - 2f * t);
        }
    }
}
