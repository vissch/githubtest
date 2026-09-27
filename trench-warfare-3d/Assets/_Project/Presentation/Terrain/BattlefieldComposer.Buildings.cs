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
        /// The rear of the allied lines (owner, 2026-09-23: "military buildings for in the back of the allied troops"): the
        /// Military set (watchtower, guard post, command post, blockhouse) behind team 0's rear fire trench, between the
        /// map's back edge and four metres short of the trench, on the far side of the supply road from the camera like the
        /// village, fronts to the camera. Placed once a map; the rear landmarks (field guns, stands, the well) keep clear
        /// of them, and a trench site is not put within reach of one.
        /// </summary>
        void PlaceRear(MapData map, BattlefieldSurface surface)
        {
            var buildings = kit.Houses == null ? new HouseKit.House[0] : System.Array.FindAll(kit.Houses, h => h.Set == "Military");
            if (buildings.Length == 0) return;
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
            var order = new int[buildings.Length];
            for (int i = 0; i < order.Length; i++) order[i] = i;
            System.Array.Sort(order, (a, b) => Rand(seed * 37 + a, 910).CompareTo(Rand(seed * 37 + b, 910)));
            foreach (int n in order)
            {
                var building = buildings[n];
                var ext = building.Bounds.extents; var mid = building.Bounds.center;
                float reach = Mathf.Max(ext.x, ext.z), radius = new Vector2(ext.x, ext.z).magnitude;
                for (int attempt = 0; attempt < 64; attempt++)
                {
                    int key = 5003 + n * 131 + attempt;
                    float x = road - (RoadClear + reach + Rand(key, 911) * Mathf.Max(4f, road - RoadClear - 2f * reach - 4f));
                    float z = 3f + reach + Rand(key, 912) * Mathf.Max(1f, front - 3f - 2f * reach);   // the whole depth up to the front's margin
                    var rotation = Quaternion.Euler(0f, 90f + (Rand(key, 913) - .5f) * 60f, 0f);
                    var at = new Vector3(x, 0f, z);
                    var centre = at + rotation * new Vector3(mid.x, 0f, mid.z);
                    if (centre.x - reach < 2f || centre.z - reach < 2f) continue;
                    if (!Room(map, surface, centre, rotation, ext.x + .4f, ext.z + .4f)) continue;
                    bool free = true;
                    foreach (var other in hamlets)
                        if (new Vector2(other.Centre.x - centre.x, other.Centre.z - centre.z).magnitude < other.Radius + radius + 3f) { free = false; break; }
                    foreach (var prop in map.Props)
                        if (new Vector2(prop.Pos.x - centre.x, prop.Pos.z - centre.z).magnitude < radius + 1f) { free = false; break; }
                    if (!free) continue;
                    float floor = float.MaxValue;
                    for (int iz = -1; iz <= 1; iz++) for (int ix = -1; ix <= 1; ix++)
                    {
                        var p = centre + rotation * new Vector3(ix * ext.x, 0f, iz * ext.z);
                        floor = Mathf.Min(floor, surface.VisualHeight(p.x, p.z));
                    }
                    at.y = floor - .12f; centre.y = at.y;
                    hamlets.Add(new Hamlet(building.Index, Matrix4x4.TRS(at, rotation, Vector3.one), centre, radius));
                    break;
                }
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
