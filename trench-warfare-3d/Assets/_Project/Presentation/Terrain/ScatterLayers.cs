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
        public const int MaxGrass = 12000, MaxAccent = 1500, MaxFlowers = 700, MaxInterior = 900, MaxLanterns = 60;
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
            return f.Patch[cell] * (1f + 1.5f * f.Vertical[cell]) * (1f - traffic) * f.Open[cell] * (1f - 0.6f * f.Wet[cell]);
        }

        public static void Place(ScatterInput input, ScatterField field, List<ScatterInstance> into)
        {
            int grass = 0, accents = 0, flowers = 0, interior = 0, lanterns = 0;
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
                for (int k = 0; k < n && grass < MaxGrass; k++, grass++)
                {
                    float ox = Hash(seed, cell, 100 + k * 3) * ScatterInput.Cell, oz = Hash(seed, cell, 101 + k * 3) * ScatterInput.Cell;
                    float scale = input.Frozen ? 0.7f + 0.4f * Hash(seed, cell, 102 + k * 3) : 0.6f + 0.7f * Hash(seed, cell, 102 + k * 3);
                    into.Add(new ScatterInstance { Kind = input.Frozen ? ScatterKind.FrostTuft : ScatterKind.Grass, X = x * ScatterInput.Cell + ox, Z = z * ScatterInput.Cell + oz, Yaw = Hash(seed, cell, 103 + k * 3) * 360f, Scale = scale });
                }
                if (n > 0 && !input.Frozen)
                {
                    if (accents < MaxAccent && Hash(seed, cell, 7) < 1f / AccentEvery)
                    {
                        accents++;
                        into.Add(new ScatterInstance { Kind = ScatterKind.GrassAccent, X = x * ScatterInput.Cell + Hash(seed, cell, 8) * ScatterInput.Cell, Z = z * ScatterInput.Cell + Hash(seed, cell, 9) * ScatterInput.Cell, Yaw = Hash(seed, cell, 10) * 360f, Scale = 0.8f + 0.4f * Hash(seed, cell, 11) });
                    }
                    if (!sand && density > FlowerMinDensity)
                    {
                        int m = (int)(n * FlowerShare + Hash(seed, cell, 12));
                        for (int j = 0; j < m && flowers < MaxFlowers; j++, flowers++)
                            into.Add(new ScatterInstance { Kind = ScatterKind.Flower, X = x * ScatterInput.Cell + Hash(seed, cell, 200 + j * 3) * ScatterInput.Cell, Z = z * ScatterInput.Cell + Hash(seed, cell, 201 + j * 3) * ScatterInput.Cell, Yaw = Hash(seed, cell, 202 + j * 3) * 360f, Scale = 0.8f + 0.4f * Hash(seed, cell, 203 + j * 3) });
                    }
                }
                // ---- the camp: a trench's inside ----------------------------------------------------------------
                if (input.Is(cell, NavLayer.Trench) && !input.Is(cell, NavLayer.Link))
                {
                    if (interior < MaxInterior && Hash(seed, cell, 21) < KitPerCell)
                    {
                        // at the wall foot: pushed out from the middle of the cell to one side or the other
                        float side = Hash(seed, cell, 25) < 0.5f ? -0.7f : 0.7f;
                        bool alongX = Hash(seed, cell, 26) < 0.5f;
                        interior++;
                        into.Add(new ScatterInstance { Kind = ScatterKind.CampKit, Variant = (byte)(Hash(seed, cell, 27) * (CampKitVariants - 0.001f)), X = cx + (alongX ? side : (Hash(seed, cell, 28) - 0.5f) * 0.8f), Z = cz + (alongX ? (Hash(seed, cell, 28) - 0.5f) * 0.8f : side), Yaw = Hash(seed, cell, 29) * 360f, Scale = CampKitScaleMin + CampKitScaleRange * Hash(seed, cell, 30) });
                    }
                    if (interior < MaxInterior && Hash(seed, cell, 22) < CratePerCell)
                    {
                        interior++;
                        into.Add(new ScatterInstance { Kind = ScatterKind.Crate, X = cx + (Hash(seed, cell, 31) - 0.5f) * 0.6f, Z = cz + (Hash(seed, cell, 32) - 0.5f) * 0.6f, Yaw = Hash(seed, cell, 33) * 360f, Scale = CrateScaleMin + CrateScaleRange * Hash(seed, cell, 34) });
                    }
                    if (lanterns < MaxLanterns && Hash(seed, cell, 23) < LanternPerCell)
                    {
                        lanterns++;
                        into.Add(new ScatterInstance { Kind = ScatterKind.Lantern, X = cx + (Hash(seed, cell, 35) - 0.5f) * 0.4f, Z = cz + (Hash(seed, cell, 36) - 0.5f) * 0.4f, Yaw = Hash(seed, cell, 37) * 360f, Scale = 1f });
                    }
                    if (interior < MaxInterior && Hash(seed, cell, 24) < DebrisPerCell)
                    {
                        interior++;
                        into.Add(new ScatterInstance { Kind = ScatterKind.WallDebris, Variant = (byte)(Hash(seed, cell, 38) < 0.6f ? 0 : 1), X = cx + (Hash(seed, cell, 39) - 0.5f) * 0.8f, Z = cz + (Hash(seed, cell, 40) - 0.5f) * 0.8f, Yaw = Hash(seed, cell, 41) * 360f, Scale = (Hash(seed, cell, 38) < 0.6f ? BoardsScaleMin : CasesScaleMin) + DebrisScaleRange * Hash(seed, cell, 42) });
                    }
                }
                // ---- the rear band --------------------------------------------------------------------------
                else if (input.InRear(cz) && field.Open[cell] > 0f)
                {
                    if (Hash(seed, cell, 51) < RearCratePerCell)
                        into.Add(new ScatterInstance { Kind = ScatterKind.Crate, X = cx + (Hash(seed, cell, 52) - 0.5f) * 1.2f, Z = cz + (Hash(seed, cell, 53) - 0.5f) * 1.2f, Yaw = Hash(seed, cell, 54) * 360f, Scale = CrateScaleMin + CrateScaleRange * Hash(seed, cell, 55) });
                    if (Hash(seed, cell, 56) < RearStackPerCell)
                        into.Add(new ScatterInstance { Kind = ScatterKind.ShellStack, X = cx + (Hash(seed, cell, 57) - 0.5f) * 0.8f, Z = cz + (Hash(seed, cell, 58) - 0.5f) * 0.8f, Yaw = Hash(seed, cell, 59) * 360f, Scale = 0.9f + 0.2f * Hash(seed, cell, 60) });
                }
            }
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
                    float wx = f.X + lx * c - lz * s, wz = f.Z + lx * s + lz * c;
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
