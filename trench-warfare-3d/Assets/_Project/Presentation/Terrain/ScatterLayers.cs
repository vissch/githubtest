// Phase: B7 (docs/21 phase 2) — the layers the scatter rules lay, from the fields, as positions in metres.
//   Grass    density = Patch (1 + 1.5 Vertical) (1 - Traffic) Open (1 - 0.6 Wet), none where Traffic is past
//            TrafficBare: organic patches, thicker against anything standing up, nothing on a trench floor, a ladder,
//            a beaten corridor or the road. floor(density * GrassPerCell + hash) tufts a cell, hashed inside it.
//            Frozen: frost tufts instead, half as many, no flowers. Coast: thinner on the sand, no flowers.
//   Accent   one imported grass clump for about every third cell with grass.
//   Flower   only in a cell that has grass and is dense enough, FlowerShare of the tufts there.
//   Interior a trench cell (not a ladder): kit at the wall foot (tins, mess kit, a spade, a helmet, boots, a hatch),
//            a crate now and then, a lantern one cell in 25, wall debris. A dugout gets a crate and a few tins inside.
//   Rear     crates one in 40 m2 and shell stacks one in 200 m2 behind each side's rear trench; a lantern by each
//            rear building. Nothing of the camp goes outside these.
// Caps hold the budget (docs/21 phase 2); cells are walked in order, so what is dropped past a cap is the same every
// time. Every roll is a hash of the cell, a salt and the seed: the same seed lays the same field.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public enum ScatterKind : byte { Grass, GrassAccent, Flower, FrostTuft, CampKit, Crate, Lantern, WallDebris, ShellStack }

    public struct ScatterInstance
    {
        public ScatterKind Kind; public byte Variant;
        public float X, Z, Yaw, Scale;   // metres, degrees, a multiplier of the module's own size
    }

    public static class ScatterLayers
    {
        public const int MaxGrass = 12000, MaxAccent = 1500, MaxFlowers = 700, MaxInterior = 900, MaxLanterns = 60, MaxRear = 360;
        public const float TrafficBare = 0.6f, GrassPerCell = 5f, FlowerMinDensity = 0.45f, FlowerShare = 0.06f;
        public const float AccentEvery = 3f, SandGrass = 0.4f, FrostGrass = 0.5f;
        public const float KitPerCell = 0.35f, CratePerCell = 0.04f, LanternPerCell = 0.04f, DebrisPerCell = 0.08f;
        public const float RearCratePerCell = 0.1f, RearStackPerCell = 0.02f;
        public const int CampKitVariants = 6;   // ammo tin, mess kit, spade, helmet, boots, hatch lid
        /// <summary>The camp's size jitter: a tin is a tin, so within the Strict rows of AssetScaleTable (a test holds
        /// both ends inside them; the clamp then touches nothing the scatter lays).</summary>
        public const float CampKitScaleMin = 0.92f, CampKitScaleRange = 0.2f;
        /// <summary>Wall debris: loose boards may be small, spent cases lie near their built size (their row).</summary>
        public const float BoardsScaleMin = 0.7f, CasesScaleMin = 0.9f, DebrisScaleRange = 0.3f;
        /// <summary>A crate's size jitter: inside its row (a test holds both ends).</summary>
        public const float CrateScaleMin = 0.9f, CrateScaleRange = 0.1f;

        /// <summary>0..1 from a cell, a salt and the seed (the generator's own mixing).</summary>
        public static float Hash(uint seed, int cell, int salt)
        {
            uint h = seed * 0x9E3779B1u ^ (uint)cell * 0x85EBCA77u ^ (uint)salt * 0xC2B2AE3Du;
            h ^= h >> 15; h *= 0x2C1B3C6Du; h ^= h >> 12; h *= 0x297A2D39u; h ^= h >> 15;
            return (h & 0xFFFFFFu) * (1f / 16777216f);
        }

        /// <summary>Tufts a cell wants, before the caps: the rule itself.</summary>
        public static float GrassDensity(ScatterField f, int cell)
        {
            float traffic = f.Traffic[cell];
            if (traffic > TrafficBare) return 0f;
            // Vertical capped at 1: every wire cell counts as a post, so beside a belt it summed past 10 and a cell wanted 20+
            // tufts (whose seed offsets then ran into the flowers'); capped, a cell wants at most ~13 (critic r3, 2026-09-27)
            return f.Patch[cell] * (1f + 1.5f * Mathf.Min(1f, f.Vertical[cell])) * (1f - traffic) * f.Open[cell] * (1f - 0.6f * f.Wet[cell]);
        }

        /// <summary>What each layer lays: grass, accents, flowers, the trench interior, lanterns, the rear band.</summary>
        public struct Tally { public int Grass, Accents, Flowers, Interior, Lanterns, Rear; }

        /// <summary>The share of a layer's candidates kept so it lands under its cap: 1 when it fits.</summary>
        static float Keep(int demand, int cap) => demand <= cap ? 1f : cap / (float)demand;

        public static void Place(ScatterInput input, ScatterField field, List<ScatterInstance> into)
        {
            // Two passes: the first counts what each layer wants, the second keeps each candidate when its own hash is under
            // cap / demand, so a cap thins the whole field evenly. A running count in row order cut the far rows first when a
            // cap bound: team B's trenches, then the rear's lanterns (critic r3 #10). The hard caps stay as a backstop for the
            // rounding. The rear band has a cap of its own now (it had none).
            var want = Walk(input, field, null, default);
            Walk(input, field, into, want);
        }

        /// <summary>One pass over the field. With <paramref name="into"/> null it only counts what the rules want; with a
        /// list it lays, keeping each candidate at Keep(want, cap) by its own hash and never passing a cap.</summary>
        public static Tally Walk(ScatterInput input, ScatterField field, List<ScatterInstance> into, Tally want)
        {
            var got = new Tally();
            bool lay = into != null;
            float kg = lay ? Keep(want.Grass, MaxGrass) : 1f, ka = lay ? Keep(want.Accents, MaxAccent) : 1f, kf = lay ? Keep(want.Flowers, MaxFlowers) : 1f;
            // the trench lanterns leave room for one by each rear building (laid after the field, they got whatever was left)
            int rearLamps = Mathf.Min(input.RearLanterns.Count, MaxLanterns / 2), fieldLamps = MaxLanterns - rearLamps;
            float ki = lay ? Keep(want.Interior, MaxInterior) : 1f, kl = lay ? Keep(want.Lanterns, fieldLamps) : 1f, kr = lay ? Keep(want.Rear, MaxRear) : 1f;
            int capG = lay ? MaxGrass : int.MaxValue, capA = lay ? MaxAccent : int.MaxValue, capF = lay ? MaxFlowers : int.MaxValue;
            int capI = lay ? MaxInterior : int.MaxValue, capL = lay ? fieldLamps : int.MaxValue, capR = lay ? MaxRear : int.MaxValue;
            uint seed = input.Seed;
            for (int z = 0; z < input.L; z++)
            for (int x = 0; x < input.W; x++)
            {
                int cell = input.Index(x, z);
                float cx = ScatterInput.CentreX(x), cz = ScatterInput.CentreZ(z);
                // ---- what grows ------------------------------------------------------------------------------
                float density = GrassDensity(field, cell);
                bool sand = input.InSand(cz);
                if (sand) density *= SandGrass;
                if (input.Frozen) density *= FrostGrass;
                int n = density > 0f ? (int)(density * GrassPerCell + Hash(seed, cell, 1)) : 0;
                for (int k = 0; k < n && got.Grass < capG; k++)
                {
                    if (lay && Hash(seed, cell, 300 + k) >= kg) continue;
                    got.Grass++;
                    if (!lay) continue;
                    float ox = Hash(seed, cell, 100 + k * 3) * ScatterInput.Cell, oz = Hash(seed, cell, 101 + k * 3) * ScatterInput.Cell;
                    float scale = input.Frozen ? 0.7f + 0.4f * Hash(seed, cell, 102 + k * 3) : 0.6f + 0.7f * Hash(seed, cell, 102 + k * 3);
                    into.Add(new ScatterInstance { Kind = input.Frozen ? ScatterKind.FrostTuft : ScatterKind.Grass, X = x * ScatterInput.Cell + ox, Z = z * ScatterInput.Cell + oz, Yaw = Hash(seed, cell, 103 + k * 3) * 360f, Scale = scale });
                }
                if (n > 0 && !input.Frozen)
                {
                    if (got.Accents < capA && Hash(seed, cell, 7) < 1f / AccentEvery && (!lay || Hash(seed, cell, 13) < ka))
                    {
                        got.Accents++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.GrassAccent, X = x * ScatterInput.Cell + Hash(seed, cell, 8) * ScatterInput.Cell, Z = z * ScatterInput.Cell + Hash(seed, cell, 9) * ScatterInput.Cell, Yaw = Hash(seed, cell, 10) * 360f, Scale = 0.8f + 0.4f * Hash(seed, cell, 11) });
                    }
                    if (!sand && density > FlowerMinDensity)
                    {
                        int m = (int)(n * FlowerShare + Hash(seed, cell, 12));
                        for (int j = 0; j < m && got.Flowers < capF; j++)
                        {
                            if (lay && Hash(seed, cell, 400 + j) >= kf) continue;
                            got.Flowers++;
                            if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.Flower, X = x * ScatterInput.Cell + Hash(seed, cell, 200 + j * 3) * ScatterInput.Cell, Z = z * ScatterInput.Cell + Hash(seed, cell, 201 + j * 3) * ScatterInput.Cell, Yaw = Hash(seed, cell, 202 + j * 3) * 360f, Scale = 0.8f + 0.4f * Hash(seed, cell, 203 + j * 3) });
                        }
                    }
                }
                // ---- the camp: a trench's inside ----------------------------------------------------------------
                if (input.Is(cell, NavLayer.Trench) && !input.Is(cell, NavLayer.Link))
                {
                    if (got.Interior < capI && Hash(seed, cell, 21) < KitPerCell && (!lay || Hash(seed, cell, 43) < ki))
                    {
                        // at the wall foot: pushed out from the middle of the cell to one side or the other
                        float side = Hash(seed, cell, 25) < 0.5f ? -0.7f : 0.7f;
                        bool alongX = Hash(seed, cell, 26) < 0.5f;
                        got.Interior++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.CampKit, Variant = (byte)(Hash(seed, cell, 27) * (CampKitVariants - 0.001f)), X = cx + (alongX ? side : (Hash(seed, cell, 28) - 0.5f) * 0.8f), Z = cz + (alongX ? (Hash(seed, cell, 28) - 0.5f) * 0.8f : side), Yaw = Hash(seed, cell, 29) * 360f, Scale = CampKitScaleMin + CampKitScaleRange * Hash(seed, cell, 30) });
                    }
                    if (got.Interior < capI && Hash(seed, cell, 22) < CratePerCell && (!lay || Hash(seed, cell, 44) < ki))
                    {
                        got.Interior++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.Crate, X = cx + (Hash(seed, cell, 31) - 0.5f) * 0.6f, Z = cz + (Hash(seed, cell, 32) - 0.5f) * 0.6f, Yaw = Hash(seed, cell, 33) * 360f, Scale = CrateScaleMin + CrateScaleRange * Hash(seed, cell, 34) });
                    }
                    if (got.Lanterns < capL && Hash(seed, cell, 23) < LanternPerCell && (!lay || Hash(seed, cell, 45) < kl))
                    {
                        got.Lanterns++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.Lantern, X = cx + (Hash(seed, cell, 35) - 0.5f) * 0.4f, Z = cz + (Hash(seed, cell, 36) - 0.5f) * 0.4f, Yaw = Hash(seed, cell, 37) * 360f, Scale = 1f });
                    }
                    if (got.Interior < capI && Hash(seed, cell, 24) < DebrisPerCell && (!lay || Hash(seed, cell, 46) < ki))
                    {
                        got.Interior++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.WallDebris, Variant = (byte)(Hash(seed, cell, 38) < 0.6f ? 0 : 1), X = cx + (Hash(seed, cell, 39) - 0.5f) * 0.8f, Z = cz + (Hash(seed, cell, 40) - 0.5f) * 0.8f, Yaw = Hash(seed, cell, 41) * 360f, Scale = (Hash(seed, cell, 38) < 0.6f ? BoardsScaleMin : CasesScaleMin) + DebrisScaleRange * Hash(seed, cell, 42) });
                    }
                }
                // ---- the rear band --------------------------------------------------------------------------
                else if (input.InRear(cz) && field.Open[cell] > 0f)
                {
                    if (got.Rear < capR && Hash(seed, cell, 51) < RearCratePerCell && (!lay || Hash(seed, cell, 61) < kr))
                    {
                        got.Rear++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.Crate, X = cx + (Hash(seed, cell, 52) - 0.5f) * 1.2f, Z = cz + (Hash(seed, cell, 53) - 0.5f) * 1.2f, Yaw = Hash(seed, cell, 54) * 360f, Scale = CrateScaleMin + CrateScaleRange * Hash(seed, cell, 55) });
                    }
                    if (got.Rear < capR && Hash(seed, cell, 56) < RearStackPerCell && (!lay || Hash(seed, cell, 62) < kr))
                    {
                        got.Rear++;
                        if (lay) into.Add(new ScatterInstance { Kind = ScatterKind.ShellStack, X = cx + (Hash(seed, cell, 57) - 0.5f) * 0.8f, Z = cz + (Hash(seed, cell, 58) - 0.5f) * 0.8f, Yaw = Hash(seed, cell, 59) * 360f, Scale = 0.9f + 0.2f * Hash(seed, cell, 60) });
                    }
                }
            }
            if (lay) Tail(input, into, got.Lanterns);
            return got;
        }

        /// <summary>The dugouts' stock and the rear buildings' lanterns, after the field.</summary>
        static void Tail(ScatterInput input, List<ScatterInstance> into, int lanterns)
        {
            uint seed = input.Seed;
            // ---- the dugouts: a crate and a few tins inside ------------------------------------------------------
            for (int i = 0; i < input.Occupied.Count; i++)
            {
                var f = input.Occupied[i];
                if (!f.Dugout) continue;
                int items = 2 + (int)(Hash(seed, -1 - i, 61) * 2.999f);
                float a = f.Yaw * Mathf.Deg2Rad, s = Mathf.Sin(a), c = Mathf.Cos(a);
                for (int k = 0; k < items; k++)
                {
                    float lx = (Hash(seed, -1 - i, 62 + k * 3) - 0.5f) * 1.2f * f.HalfX, lz = (Hash(seed, -1 - i, 63 + k * 3) - 0.5f) * 1.2f * f.HalfZ;
                    float wx = f.X + lx * c + lz * s, wz = f.Z - lx * s + lz * c;   // Unity's yaw, as Footprint.Contains inverts it
                    bool crate = k == 0 || Hash(seed, -1 - i, 64 + k * 3) < 0.3f;
                    into.Add(new ScatterInstance { Kind = crate ? ScatterKind.Crate : ScatterKind.CampKit, Variant = (byte)(Hash(seed, -1 - i, 65 + k * 3) * (CampKitVariants - 0.001f)), X = wx, Z = wz, Yaw = f.Yaw + (Hash(seed, -1 - i, 66 + k * 3) - 0.5f) * 40f, Scale = crate ? 0.95f : 1f });
                }
            }
            // ---- a lantern by each rear building ---------------------------------------------------------------
            for (int i = 0; i < input.RearLanterns.Count && lanterns < MaxLanterns; i++, lanterns++)
                into.Add(new ScatterInstance { Kind = ScatterKind.Lantern, X = input.RearLanterns[i].x, Z = input.RearLanterns[i].y, Yaw = 180f, Scale = 1f });
        }
    }
}
