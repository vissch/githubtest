// Phase: B2 (implemented) — part of BattlefieldComposer: the village by the bridge and the rear's military buildings,
// placed once a map (the sites and the landmarks keep clear of them).
using System;
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
    {
        /// <summary>
        /// The village at the crossing (owner, 2026-09-23: "implement in the center near the water of the map on each side"):
        /// the kit's houses shared out between the two banks, half each, beside the road over the bridge in the middle of
        /// the map on its far side from the camera, set back a few metres from the water, fronts to the camera. Placed once a map,
        /// like the sites. Decoration, as the rest of the kit is: the sim knows nothing of the houses (no cover, no
        /// blocking), so they keep off the trenches, the wire, the road and the sim's own props.
        /// </summary>
        void PlaceHamlets(MapData map, BattlefieldSurface surface)
        {
            hamlets.Clear();
            var houses = kit.Houses == null ? new HouseKit.House[0] : System.Array.FindAll(kit.Houses, h => h.Set == "Houses");
            if (houses.Length == 0 || map.WaterLevel <= MapData.NoWater) return;
            bool bridged = false; Vector3 bridge = default;
            foreach (var prop in map.Props) if (prop.Kind == PropKind.Bridge) { bridge = new Vector3(prop.Pos.x, 0f, prop.Pos.z); bridged = true; break; }
            if (!bridged) return;
            // the same share for a seed: houses in a hashed order, the first half to the near bank
            var order = new int[houses.Length];
            for (int i = 0; i < order.Length; i++) order[i] = i;
            System.Array.Sort(order, (a, b) => Rand(seed * 31 + a, 900).CompareTo(Rand(seed * 31 + b, 900)));
            for (int side = 0; side < 2; side++)
            {
                float away = side == 0 ? -1f : 1f;   // from the water toward the side's own lines: team 0 holds the low z
                for (int n = side * houses.Length / 2, end = (side + 1) * houses.Length / 2; n < end; n++)
                {
                    var house = houses[order[n]];
                    var ext = house.Bounds.extents; var mid = house.Bounds.center;
                    float reach = Mathf.Max(ext.x, ext.z), radius = new Vector2(ext.x, ext.z).magnitude;
                    for (int attempt = 0; attempt < 48; attempt++)
                    {
                        int key = side * 997 + n * 61 + attempt;
                        // on the far side of the road only: the camera always looks from +X, so a house on the near side
                        // stands between it and the crossing and hides the fight for the bridge
                        float x = bridge.x - (RoadClear + reach + Rand(key, 901) * (8f + attempt * .5f));
                        float bank = Bank(map, surface, x, bridge.z, away);
                        if (float.IsNaN(bank)) continue;
                        float z = bank + away * (1.5f + reach + Rand(key, 902) * (5f + attempt * .3f));
                        // front (the sheet's front, local +Z) to the camera: every view of the field looks from +X back
                        // along -X (TacticalCamera.BaseYaw -90, following the battle by +-21), so a house turned from it
                        // shows only its blank back or its side. Turned up to 30 degrees either way, so no two read alike.
                        float yaw = 90f + (Rand(key, 904) - .5f) * 60f;
                        var rotation = Quaternion.Euler(0f, yaw, 0f);
                        var at = new Vector3(x, 0f, z);
                        var centre = at + rotation * new Vector3(mid.x, 0f, mid.z);
                        if (!Room(map, surface, centre, rotation, ext.x + .4f, ext.z + .4f)) continue;
                        bool free = true;
                        foreach (var other in hamlets)
                            if (new Vector2(other.Centre.x - centre.x, other.Centre.z - centre.z).magnitude < other.Radius + radius + 1.5f) { free = false; break; }
                        foreach (var prop in map.Props)
                            if (new Vector2(prop.Pos.x - centre.x, prop.Pos.z - centre.z).magnitude < radius + 1f) { free = false; break; }
                        if (!free) continue;
                        // on the lowest ground under it, sunk a little, so no corner hangs in the air
                        float floor = float.MaxValue;
                        for (int iz = -1; iz <= 1; iz++) for (int ix = -1; ix <= 1; ix++)
                        {
                            var p = centre + rotation * new Vector3(ix * ext.x, 0f, iz * ext.z);
                            floor = Mathf.Min(floor, surface.VisualHeight(p.x, p.z));
                        }
                        at.y = floor - .12f; centre.y = at.y;
                        hamlets.Add(new Hamlet(house.Index, Matrix4x4.TRS(at, rotation, Vector3.one), centre, radius));
                        break;
                    }
                }
            }
        }

        /// <summary>
        /// The rear of the allied lines (owner, 2026-09-23: "military buildings for in the back of the allied troops";
        /// 2026-10-06, on the ruins, "lets put them also in the rear with the other buildings"): the Ruins set (the four
        /// big ruins) and the Military set (watchtower, guard post, command post, blockhouse) behind team 0's rear fire
        /// trench, between the map's back edge and four metres short of the trench, on the far side of the supply road
        /// from the camera like the village, fronts to the camera. Ruins go first (they are the widest, and a seeded
        /// stream of their own keeps the military draws exactly where they stood before this). Placed once a map; the
        /// rear landmarks (field guns, stands, the well) keep clear of them, and a trench site is not put within reach
        /// of one.
        /// </summary>
        void PlaceRear(MapData map, BattlefieldSurface surface)
        {
            var ruins = kit.Houses == null ? new HouseKit.House[0] : System.Array.FindAll(kit.Houses, h => h.Set == "Ruins");
            var military = kit.Houses == null ? new HouseKit.House[0] : System.Array.FindAll(kit.Houses, h => h.Set == "Military");
            if (ruins.Length == 0 && military.Length == 0) return;
            float W = map.SizeMeters.x;
            // the front of the rear: the nearest point of team 0's trenches to the back edge, less ten metres
            float front = float.MaxValue;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var trench = map.Trenches[t];
                if (trench.OwnerTeam != 0) continue;
                for (int i = trench.CellStart; i < trench.CellStart + trench.CellCount; i++)
                    front = Mathf.Min(front, (map.TrenchCells[i] / map.NavWidth) * MapData.NavCellSize);
            }
            if (front == float.MaxValue) front = map.SizeMeters.y * .12f;
            front -= 4f;   // up to the trench's back: the rear is only some sixteen metres deep, and a watchtower wants to see over it
            // the road up from the back edge: where team 0's supply road leaves it, or the middle
            float road = W * .5f;
            if (map.SupplyRoad.IsCreated && map.SupplyRoad.Length > 0) road = map.SupplyRoad[0].x;
            PlaceRearSet(map, surface, ruins, road, front, orderSalt: 920, xSalt: 921, zSalt: 922, rotSalt: 923, keyBase: 9201, reasonTag: "ruin");
            PlaceRearSet(map, surface, military, road, front, orderSalt: 910, xSalt: 911, zSalt: 912, rotSalt: 913, keyBase: 5003, reasonTag: "military");
        }

        /// <summary>One rear set (ruins or military), placed with its own hashed stream so the two never shuffle each
        /// other's draw. A building whose reach is over 3 m (a ruin: 8.9 x 6.8 m against a watchtower's 2-4 m) gets the
        /// widened band the plan calls for, and a lane of its own across that band's width, so four such buildings do
        /// not cluster and reject each other; a lane grows to the whole band after 24 attempts.</summary>
        void PlaceRearSet(MapData map, BattlefieldSurface surface, HouseKit.House[] buildings, float road, float front,
            int orderSalt, int xSalt, int zSalt, int rotSalt, int keyBase, string reasonTag)
        {
            if (buildings.Length == 0) return;
            var order = new int[buildings.Length];
            for (int i = 0; i < order.Length; i++) order[i] = i;
            System.Array.Sort(order, (a, b) => Rand(seed * 37 + a, orderSalt).CompareTo(Rand(seed * 37 + b, orderSalt)));
            int lanes = buildings.Length;
            int laneIndex = 0;
            foreach (int n in order)
            {
                var building = buildings[n];
                var ext = building.Bounds.extents; var mid = building.Bounds.center;
                float reach = Mathf.Max(ext.x, ext.z), radius = new Vector2(ext.x, ext.z).magnitude;
                bool wide = reach > 3f;
                float zLo = wide ? 2f + reach : 3f + reach;
                float zHi = wide ? Mathf.Max(zLo, front) : zLo + Mathf.Max(1f, front - 3f - 2f * reach);
                float xLo = wide ? 2f + reach : road - RoadClear - reach - Mathf.Max(4f, road - RoadClear - 2f * reach - 4f);
                float xHi = wide ? Mathf.Max(xLo, road - RoadClear - reach) : road - RoadClear - reach;
                float laneWidth = (xHi - xLo) / lanes;
                float laneLo = xLo + laneIndex * laneWidth, laneHi = laneLo + laneWidth;
                bool found = false; string reason = reasonTag + ": room";
                for (int attempt = 0; attempt < 200; attempt++)
                {
                    int key = keyBase + n * 131 + attempt;
                    bool inLane = wide && attempt < 24;
                    float x = Mathf.Lerp(inLane ? laneLo : xLo, inLane ? laneHi : xHi, Rand(key, xSalt));
                    float z = Mathf.Lerp(zLo, zHi, Rand(key, zSalt));
                    var rotation = Quaternion.Euler(0f, 90f + (Rand(key, rotSalt) - .5f) * 60f, 0f);
                    var at = new Vector3(x, 0f, z);
                    var centre = at + rotation * new Vector3(mid.x, 0f, mid.z);
                    if (centre.x - reach < 2f || centre.z - reach < 2f) continue;
                    if (!Room(map, surface, centre, rotation, ext.x + .4f, ext.z + .4f, rise: wide ? 2.2f : 0f)) { reason = reasonTag + ": room"; continue; }
                    bool free = true;
                    foreach (var other in hamlets)
                    {
                        var otherSet = kit.Houses[other.House].Set;
                        float clear = (wide && otherSet == "Ruins") ? radius + other.Radius + 1f : radius + other.Radius + 3f;
                        if (new Vector2(other.Centre.x - centre.x, other.Centre.z - centre.z).magnitude < clear) { free = false; break; }
                    }
                    if (!free) { reason = reasonTag + ": spacing"; continue; }
                    foreach (var prop in map.Props)
                        if (new Vector2(prop.Pos.x - centre.x, prop.Pos.z - centre.z).magnitude < radius + 1f) { free = false; break; }
                    if (!free) { reason = reasonTag + ": prop"; continue; }
                    for (int i = 0; i < map.Spawns.Length; i++)
                        if (new Vector2(map.Spawns[i].Pos.x - centre.x, map.Spawns[i].Pos.z - centre.z).magnitude < radius + 3f) { free = false; break; }
                    if (!free) { reason = reasonTag + ": spawn"; continue; }
                    if (map.SupplyRoad.IsCreated)
                        for (int i = 0; i < map.SupplyRoad.Length; i++)
                            if (new Vector2(map.SupplyRoad[i].x - centre.x, map.SupplyRoad[i].z - centre.z).magnitude < radius + 2f) { free = false; break; }
                    if (!free) { reason = reasonTag + ": road"; continue; }
                    // on the lowest ground under it, sunk a little, so no corner hangs in the air (a 5x5 sample for a
                    // wide footprint, a ruin's corner being nine metres from its opposite one)
                    float floor = float.MaxValue;
                    for (int iz = -2; iz <= 2; iz++) for (int ix = -2; ix <= 2; ix++)
                    {
                        var p = centre + rotation * new Vector3(ix * ext.x * .5f, 0f, iz * ext.z * .5f);
                        floor = Mathf.Min(floor, surface.VisualHeight(p.x, p.z));
                    }
                    at.y = floor - (wide ? .20f : .12f); centre.y = at.y;
                    hamlets.Add(new Hamlet(building.Index, Matrix4x4.TRS(at, rotation, Vector3.one), centre, radius));
                    found = true;
                    break;
                }
                if (!found) { rejections.TryGetValue(reason, out int rejected); rejections[reason] = rejected + 1; }
                laneIndex++;
            }
        }

        /// <summary>The edge of the dry ground on one bank at column x: the river's lowest point near the bridge's line,
        /// then the first dry metre going <paramref name="away"/> from it. NaN if the column has no water near.</summary>
        static float Bank(MapData map, BattlefieldSurface surface, float x, float riverZ, float away)
        {
            if (x < 1f || x > map.SizeMeters.x - 1f) return float.NaN;
            float lowest = float.MaxValue, water = float.NaN;
            for (float z = riverZ - 16f; z <= riverZ + 16f; z += .5f)
            {
                if (z < 0f || z >= map.SizeMeters.y) continue;
                float h = surface.VisualHeight(x, z);
                if (h < lowest) { lowest = h; water = z; }
            }
            if (float.IsNaN(water) || lowest > map.WaterLevel) return float.NaN;
            for (float d = 0f; d < 30f; d += .5f)
            {
                float z = water + away * d;
                if (z < 0f || z >= map.SizeMeters.y) return float.NaN;
                bool dry = true;
                for (float e = 0f; e <= 1f && dry; e += .5f)
                {
                    float ze = z + away * e;
                    dry = surface.VisualHeight(x, ze) > map.WaterLevel + .3f && surface.At(x, ze).Wetness <= .45f;
                }
                if (dry) return z;
            }
            return float.NaN;
        }
    }
}
