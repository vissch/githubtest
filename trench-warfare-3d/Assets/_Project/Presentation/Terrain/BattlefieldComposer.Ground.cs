// Phase: B2 (implemented) — part of BattlefieldComposer: the ground. Debris and litter (grave markers now: the camp's
// kit is the scatter's, BattlefieldComposer.Scatter.cs), the gatherings of scrub and stones round the big shapes, the
// reeds on the margins, and the winter's drifts, crust and icicles.
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
    {
        /// <summary>Loose litter on open ground: branches, boards and braced planks everywhere, now and then a door or a
        /// crossed pair of boards laid flat; spent cases, fallen sacks and sheets of corrugated iron near the trenches. One
        /// candidate per 4.5 m square, placed by hash so it never moves between rebuilds.</summary>
        void Debris(MapData map, BattlefieldSurface surface)
        {
            const float grid = 4.5f;
            int k = 0;
            for (float gz = 4f; gz < map.SizeMeters.y - 4f; gz += grid)
            for (float gx = 3f; gx < map.SizeMeters.x - 3f; gx += grid, k++)
            {
                float x = gx + Rand(k, 62) * (grid - 1f), z = gz + Rand(k, 63) * (grid - 1f);
                // Litter gathers: thick in some stretches and absent in others (a slow noise), thicker again round shell
                // holes and along the trench, where things get thrown, dropped and blown.
                float gather = Mathf.PerlinNoise(x * .045f + 17f, z * .045f + 3f);
                var near = surface.At(x, z);
                float chance = gather * gather * 1.15f + (near.Hollow >= 0 ? .45f : 0f) + (near.BankDistance < 7f ? .30f : 0f);
                if (Rand(k, 61) > chance) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked | NavLayer.Wire)) != 0) continue;
                var at = surface.At(x, z);
                if (at.Wetness > .35f || at.BankDistance < 1.2f) continue;
                float pick = Rand(k, 64), ground = surface.VisualHeight(x, z);
                var yaw = Quaternion.Euler(0f, Rand(k, 65) * 360f, 0f);
                var tilt = Quaternion.Euler((Rand(k, 67) - .5f) * 8f, 0f, (Rand(k, 68) - .5f) * 8f);   // bedded unevenly in the mud
                float size = .85f + Rand(k, 66) * .5f;
                if (at.BankDistance < 7f)
                {
                    // by the trench: spent cases, sacks fallen off the parapet, sheets of iron, boards
                    if (pick < .30f) emit(kit.shellCases, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .44f) emit(kit.sandbag, Matrix4x4.TRS(new Vector3(x, ground - .04f, z), yaw * tilt, Vector3.one * (.9f + Rand(k, 66) * .25f)));
                    else if (pick < .53f) Sheet(x, z, ground, yaw, k);
                    else if (pick < .70f) emit(kit.branches, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .85f) emit(kit.looseBoards, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else emit(kit.bracedPlank, Matrix4x4.TRS(new Vector3(x, ground - .10f, z), yaw * tilt, Vector3.one * (.8f + Rand(k, 66) * .3f)));
                }
                else
                {
                    // planks everywhere: broken branches and boards, braced planks, a door off some farm, a crossed pair of boards
                    if (pick < .40f) emit(kit.branches, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .62f) emit(kit.looseBoards, Matrix4x4.TRS(new Vector3(x, ground + .01f, z), yaw, Vector3.one * size));
                    else if (pick < .84f) emit(kit.bracedPlank, Matrix4x4.TRS(new Vector3(x, ground - .10f, z), yaw * tilt, Vector3.one * (.8f + Rand(k, 66) * .3f)));
                    else if (pick < .93f) Flat(kit.plankDoor, x, z, ground, yaw * tilt, .78f + Rand(k, 66) * .14f, .25f);
                    else Flat(kit.crossedBoards, x, z, ground, yaw * tilt, .75f + Rand(k, 66) * .2f, .4f);
                }
            }
        }

        /// <summary>An upright imported piece laid on its back (its front face up), thinned to thickness of its depth.</summary>
        void Flat(BattlefieldKit.Module module, float x, float z, float ground, Quaternion rotation, float size, float thickness)
            => emit(module, Matrix4x4.TRS(new Vector3(x, ground + .05f, z), rotation * Quaternion.Euler(-90f, 0f, 0f), new Vector3(size, size, size * thickness)));

        /// <summary>A curved sheet of corrugated iron thrown down arch-up, half sunk in the mud.</summary>
        void Sheet(float x, float z, float ground, Quaternion yaw, int k)
        {
            float size = .6f + Rand(k, 69) * .18f;
            emit(kit.corrugated, Matrix4x4.TRS(new Vector3(x, ground + 1.35f * size * .45f, z), yaw * Quaternion.Euler((Rand(k, 70) - .5f) * 12f, 0f, 180f), Vector3.one * size));
        }

        /// <summary>
        /// The graves: now and then a rifle stood in the ground with a helmet on it, out in the open. The rest of what men
        /// drop and leave (helmets, boots, mess tins, tools, ammunition tins) is the camp's, and the camp keeps to the
        /// trenches, the dugouts and the rear (docs/21 phase 2: BattlefieldComposer.Scatter.cs). One candidate per 3.4 m
        /// square, placed by hash so it never moves between rebuilds.
        /// </summary>
        void Litter(MapData map, BattlefieldSurface surface)
        {
            const float grid = 3.4f;
            int k = 0;
            for (float gz = 4f; gz < map.SizeMeters.y - 4f; gz += grid)
            for (float gx = 3f; gx < map.SizeMeters.x - 3f; gx += grid, k++)
            {
                float x = gx + Rand(k, 132) * (grid - .6f), z = gz + Rand(k, 133) * (grid - .6f);
                var at = surface.At(x, z);
                bool byTrench = at.BankDistance < 6f;
                float gather = Mathf.PerlinNoise(x * .06f + 41f, z * .06f + 9f);
                float chance = gather * gather * .34f + (at.Hollow >= 0 ? .22f : 0f) + (byTrench ? .34f : 0f);
                if (Rand(k, 131) > chance) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) continue;
                if (at.Wetness > .35f || at.BankDistance < 1.1f) continue;
                float pick = Rand(k, 134);
                if (byTrench || pick >= .07f || at.Hollow >= 0) continue;   // only the graves are left to this step
                var module = kit.graveMarker;
                const float sink = .06f;
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - sink, z), Quaternion.Euler(0f, Rand(k, 135) * 360f, 0f), Vector3.one * (.92f + Rand(k, 136) * .2f)));
            }
        }

        bool Open(MapData map, BattlefieldSurface surface, float x, float z)
        {
            if (x < 1f || z < 1f || x > map.SizeMeters.x - 1f || z > map.SizeMeters.y - 1f) return false;
            var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
            if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) return false;
            var at = surface.At(x, z);
            return at.Wetness < .3f && at.BankDistance > 1.3f && at.Hollow < 0;
        }

        /// <summary>One gathering of small and medium shapes round a big one: scrub close in, grass and stones thinning
        /// outward (distance grows with the square root of a uniform roll, so the middle is densest).</summary>
        void Gather(MapData map, BattlefieldSurface surface, Vector3 heart, int key, float reach, int scrub, int small)
        {
            for (int i = 0; i < scrub + small; i++)
            {
                int k = key * 32 + i;
                bool medium = i < scrub;
                float far = medium ? .9f + Rand(k, 91) * reach * .55f : (.4f + Mathf.Sqrt(Rand(k, 91)) * reach), angle = Rand(k, 92) * Mathf.PI * 2f;
                float x = heart.x + Mathf.Cos(angle) * far, z = heart.z + Mathf.Sin(angle) * far;
                if (!Open(map, surface, x, z)) continue;
                // medium: scrub, a rock, a mossy stump; small: stones (the grass and the poppies are the scatter's now,
                // laid by its rules over the whole field: BattlefieldComposer.Scatter.cs)
                BattlefieldKit.Module module; float size;
                float pick = Rand(k, 93);
                if (medium)
                {
                    module = pick < .62f ? kit.bush : pick < .86f ? kit.boulder : kit.stumpMoss;
                    size = module == kit.bush ? .8f + Rand(k, 94) * .7f : module == kit.boulder ? .38f + Rand(k, 94) * .3f : .55f + Rand(k, 94) * .3f;
                }
                else
                {
                    module = kit.stones;
                    size = .7f + Rand(k, 94) * .9f;
                }
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - (module == kit.boulder ? .10f : .02f), z), Quaternion.Euler(0f, Rand(k, 95) * 360f, 0f), Vector3.one * size));
            }
        }

        /// <summary>Big, medium, small (owner, 2026-09-21): every big shape on the field gathers smaller ones round it,
        /// and the dry rises grow their own patches. Nothing here is on a grid: patch centres are dart-thrown with a
        /// minimum spacing, members fall off from the middle.</summary>
        void Clumps(MapData map, BattlefieldSurface surface)
        {
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                if (p.Kind == PropKind.Bridge) continue;
                bool big = p.Kind == PropKind.Wreck || p.Scale >= 1.1f;
                if (big) Gather(map, surface, p.Pos, i + 1, p.Kind == PropKind.Wreck ? 5.5f : 4.2f, 1 + (int)(Rand(i, 96) * 3f), 6 + (int)(Rand(i, 97) * 7f));
                else if (p.Scale >= .75f && Rand(i, 98) < .5f) Gather(map, surface, p.Pos, i + 1, 2.2f, 0, 2 + (int)(Rand(i, 97) * 4f));
            }
            for (int i = 0; i < surface.Hollows.Count; i++)
            {
                if (Rand(i, 99) > .55f) continue;   // fresh holes are bare; old ones have grown a fringe
                var h = surface.Hollows[i];
                float angle = Rand(i, 100) * Mathf.PI * 2f;   // on one side of the rim, not a wreath
                var heart = new Vector3(h.Center.x + Mathf.Cos(angle) * (h.Radius + 1.2f), 0f, h.Center.y + Mathf.Sin(angle) * (h.Radius + 1.2f));
                Gather(map, surface, heart, 5000 + i, 2.6f, Rand(i, 101) < .4f ? 1 : 0, 3 + (int)(Rand(i, 102) * 5f));
            }
            // free patches on the dry rises: dart-throwing with a 9 m minimum spacing
            var patches = new List<Vector2>();
            int darts = (int)(map.SizeMeters.x * map.SizeMeters.y / 40f);
            for (int d = 0; d < darts; d++)
            {
                var c = new Vector2(2f + Rand(d, 103) * (map.SizeMeters.x - 4f), 2f + Rand(d, 104) * (map.SizeMeters.y - 4f));
                if (BattlefieldSurface.Mound(c.x, c.y) < .08f || !Open(map, surface, c.x, c.y)) continue;
                bool crowded = false;
                foreach (var other in patches) if ((other - c).sqrMagnitude < 81f) { crowded = true; break; }
                if (crowded) continue;
                patches.Add(c);
                Gather(map, surface, new Vector3(c.x, 0f, c.y), 9000 + d, 3.4f, Rand(d, 105) < .6f ? 1 : 2, 5 + (int)(Rand(d, 106) * 8f));
            }
        }

        /// <summary>
        /// A spawn rule, the way a world generator scatters a biome: reeds grow where the ground is only just above
        /// standing water (the river's margin, the rim of a flooded shell hole, the wet end of a drainage line) and
        /// nowhere else. Stands are dart-thrown 3.5 m apart and each is a big / medium / small family of stems.
        /// </summary>
        void Margins(MapData map, BattlefieldSurface surface)
        {
            var stands = new List<Vector2>();
            int darts = (int)(map.SizeMeters.x * map.SizeMeters.y / 9f), planted = 0;
            bool river = map.WaterLevel > MapData.NoWater;
            for (int d = 0; d < darts && planted < 320; d++)
            {
                var c = new Vector2(2f + Rand(d, 121) * (map.SizeMeters.x - 4f), 2f + Rand(d, 122) * (map.SizeMeters.y - 4f));
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(c.x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(c.y / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link)) != 0) continue;
                var at = surface.At(c.x, c.y);
                if (at.BankDistance < 2.5f) continue;
                float bed = surface.Bed(c.x, c.y);
                bool shore = river && bed > map.WaterLevel + .03f && bed < map.WaterLevel + .40f;
                bool rim = false;
                if (!shore && at.Hollow >= 0)
                {
                    var h = surface.Hollows[at.Hollow];
                    float r = (c - h.Center).magnitude / h.Radius;
                    rim = h.Level > -100f && r > .80f && r < 1.12f && bed > h.Level - .05f;
                }
                bool seep = !shore && !rim && at.Hollow < 0 && surface.Rill(c.x, c.y) > .7f && Rand(d, 123) < .35f;
                if (!shore && !rim && !seep) continue;
                bool crowded = false;
                foreach (var other in stands) if ((other - c).sqrMagnitude < 12.25f) { crowded = true; break; }
                if (crowded) continue;
                stands.Add(c);
                int stems = 3 + (int)(Rand(d, 124) * 6f);
                for (int k = 0; k < stems; k++)
                {
                    float far = Mathf.Sqrt(Rand(d * 16 + k, 125)) * 1.3f, angle = Rand(d * 16 + k, 126) * Mathf.PI * 2f;
                    float x = c.x + Mathf.Cos(angle) * far, z = c.y + Mathf.Sin(angle) * far;
                    if (x < 1f || z < 1f || x > map.SizeMeters.x - 1f || z > map.SizeMeters.y - 1f) continue;
                    float size = k == 0 ? 1.25f + Rand(d, 127) * .35f : k < 3 ? .85f + Rand(d * 16 + k, 128) * .3f : .5f + Rand(d * 16 + k, 128) * .3f;
                    // most stands grow round a clump of cattails; the old reeds make up the rest of the family
                    if (k == 0 && Rand(d, 130) < .65f) emit(kit.cattails, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .04f, z), Quaternion.Euler(0f, Rand(d * 16 + k, 129) * 360f, 0f), Vector3.one * (.75f + Rand(d, 127) * .35f)));
                    else emit(kit.reeds, Matrix4x4.TRS(new Vector3(x, surface.Bed(x, z) - .04f, z), Quaternion.Euler(0f, Rand(d * 16 + k, 129) * 360f, 0f), Vector3.one * size));
                    planted++;
                }
            }
        }

        /// <summary>
        /// The ground micro-kit, winter only: drifts, broken crust, frozen tufts and clods, on a 1.7 m grid so
        /// the camera among the men always has something within arm's reach. None of it is submitted at the
        /// standard view - BattlefieldProps drops a finite MaxDistance while SceneHooks.CloseUp is 0 - so the
        /// density here is paid for only by the close lens.
        ///
        /// Drifts gather where wind-blown snow gathers: in the lee of a bank and in the hollows. The rest is
        /// scattered by the same density noise the litter uses, so the two agree about where the field is busy.
        /// </summary>
        void SnowGround(MapData map, BattlefieldSurface surface)
        {
            const float grid = 1.7f;
            int k = 0;
            for (float gz = 3f; gz < map.SizeMeters.y - 3f; gz += grid)
            for (float gx = 2f; gx < map.SizeMeters.x - 2f; gx += grid, k++)
            {
                float x = gx + Rand(k, 181) * (grid - .35f), z = gz + Rand(k, 182) * (grid - .35f);
                var at = surface.At(x, z);
                if (at.Wetness > .5f) continue;
                var layer = (NavLayer)map.NavLayers[map.NavIndex(Mathf.Clamp((int)(x / MapData.NavCellSize), 0, map.NavWidth - 1), Mathf.Clamp((int)(z / MapData.NavCellSize), 0, map.NavLength - 1))];
                if ((layer & (NavLayer.Trench | NavLayer.Link | NavLayer.Blocked)) != 0) continue;
                float gather = Mathf.PerlinNoise(x * .085f + 17f, z * .085f + 61f);
                float pick = Rand(k, 183);
                // Drifts gather HARDEST in the lee of a bank and in hollows, but they do not only form there:
                // sastrugi are cut on open exposed ground, which on this map is the whole of no man's land,
                // and that is precisely where the field had nothing standing on it. So open ground gets them
                // too, at about a third the rate and gathered by the same density noise the litter uses, so
                // the drifts run in belts rather than peppering the map evenly.
                bool lee = at.BankDistance < 4.5f || at.Hollow >= 0;
                float driftChance = lee ? .44f : .10f + gather * gather * .26f;
                BattlefieldKit.Module module;
                if (pick < driftChance) module = kit.drift;
                else if (pick < .34f + gather * .18f) module = kit.snowClod;
                else if (pick < .47f) module = kit.iceShard;   // fewer sites, but each is a run of plates
                else if (pick < .60f + gather * .12f) module = kit.frostTuft;
                else continue;
                if (module == null) continue;
                float sink = module == kit.frostTuft ? .02f : .05f;
                float s = .78f + Rand(k, 184) * .55f;
                // Drifts are cut by ONE wind, so they must agree with each other about which way it blew. A smooth
                // noise field turned into an angle gives neighbours nearly the same heading and lets it wander over
                // the map; random yaw, which is right for litter, reads as rubble for a drift.
                float yaw = module == kit.drift
                    ? 26f + (Mathf.PerlinNoise(x * .022f + 5f, z * .022f + 71f) - .5f) * 54f
                    : Rand(k, 185) * 360f;
                if (module == kit.iceShard)
                {
                    // a run of broken plates along one line: crust gives way where something crossed it
                    int pieces = 2 + (int)(Rand(k, 187) * 3f);
                    float heading = (26f + (Mathf.PerlinNoise(x * .022f + 5f, z * .022f + 71f) - .5f) * 54f + 90f) * Mathf.Deg2Rad;
                    float ax = Mathf.Sin(heading), az = Mathf.Cos(heading);
                    for (int q = 0; q < pieces; q++)
                    {
                        float step = (q - (pieces - 1) * .5f) * (.34f + Rand(k * 8 + q, 188) * .22f);
                        float px = x + ax * step + (Rand(k * 8 + q, 189) - .5f) * .16f;
                        float pz = z + az * step + (Rand(k * 8 + q, 190) - .5f) * .16f;
                        float ps = s * (.7f + Rand(k * 8 + q, 191) * .6f);
                        emit(module, Matrix4x4.TRS(new Vector3(px, surface.VisualHeight(px, pz) - sink, pz),
                                                   Quaternion.Euler(0f, Rand(k * 8 + q, 192) * 360f, 0f), Vector3.one * ps));
                    }
                    continue;
                }
                emit(module, Matrix4x4.TRS(new Vector3(x, surface.VisualHeight(x, z) - sink, z),
                                           Quaternion.Euler(0f, yaw, 0f),
                                           new Vector3(s, module == kit.drift ? s * (.7f + Rand(k, 186) * .6f) : s, s)));
            }
        }

        /// <summary>
        /// docs/18 W7, winter only: ice along a house's eaves. Hung on the four sides of the building's own
        /// footprint rather than scattered round it, because an icicle is made by a roof and reads wrong
        /// anywhere else. Hashed off the hamlet, so the same village freezes the same way every run, and
        /// gapped - a run of ice does not go the whole way round a building it has dripped off.
        /// </summary>
        void Icicles(Hamlet h)
        {
            float eave = h.Radius * 0.72f;             // in from the footprint's corner radius, at the wall
            for (int side = 0; side < 4; side++)
            for (int run = 0; run < 3; run++)
            {
                int key = h.House * 64 + side * 8 + run;
                if (Rand(key, 71) < 0.42f) continue;   // most eaves carry nothing; a few carry a long run
                float along = (run - 1) * (eave * 0.62f) + (Rand(key, 72) - 0.5f) * 0.5f;
                float turn = side * 90f;
                var outward = Quaternion.Euler(0f, turn, 0f);
                var at = h.Centre + outward * new Vector3(along, 0f, eave);
                float high = 2.35f + Rand(key, 73) * 0.9f;   // the eave line; the houses are one and two storeys
                emit(kit.icicles, Matrix4x4.TRS(new Vector3(at.x, h.Centre.y + high, at.z),
                                                outward * Quaternion.Euler(0f, 0f, (Rand(key, 74) - 0.5f) * 6f),
                                                Vector3.one * (0.75f + Rand(key, 75) * 0.5f)));
            }
        }
    }
}
