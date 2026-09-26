// Phase: B7 (docs/21 phase 2) — part of BattlefieldComposer: the scatter step. Builds the rules' input from the map,
// the surface and the layout the earlier steps placed (houses, shelters), runs the fields and the layers once per
// map, and emits the placements on every pass; a placement a shell hole has since opened under is left out, so a
// fresh crater strips its grass and its kit. Runs after the structural steps (their footprints mask it) and before
// the winter ground and the backdrop.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim.Terrain;

namespace TW.Presentation.Terrain
{
    public sealed partial class BattlefieldComposer
    {
        MapData scatterMap;
        readonly List<ScatterInstance> scatter = new List<ScatterInstance>(16384);
        /// <summary>What the rules laid for the current map (positions in metres), for the audit and the tests.</summary>
        public IReadOnlyList<ScatterInstance> ScatterPlacements => scatter;
        public ScatterInput LastScatterInput { get; private set; }

        /// <summary>The rules' input for a map: its cells and banks, what stands up on it, the footprints the layout took,
        /// the ladders of each side's front, the roads, the season, the coast and the rear bands.</summary>
        public ScatterInput BuildScatterInput(MapData map, BattlefieldSurface surface)
        {
            var input = ScatterInput.Blank(map.NavWidth, map.NavLength, (uint)seed);
            bool dist = map.CellTrenchDist.IsCreated && map.CellTrenchDist.Length > 0;
            for (int z = 0; z < map.NavLength; z++)
            for (int x = 0; x < map.NavWidth; x++)
            {
                int i = input.Index(x, z);
                float cx = ScatterInput.CentreX(x), cz = ScatterInput.CentreZ(z);
                input.Nav[i] = map.NavLayers[map.NavIndex(x, z)];
                var at = surface.At(cx, cz);
                input.BankDistance[i] = at.BankDistance; input.Wetness[i] = at.Wetness; input.Hollow[i] = at.Hollow >= 0;
                input.Mound[i] = BattlefieldSurface.Mound(cx, cz);
                if (dist)
                {
                    int hx = Mathf.Clamp((int)(cx / MapData.HeightCellSize), 0, map.Height.Width - 1), hz = Mathf.Clamp((int)(cz / MapData.HeightCellSize), 0, map.Height.Length - 1);
                    byte d = map.CellTrenchDist[hz * map.Height.Width + hx];
                    input.TrenchDist[i] = d == 255 ? 255f : d / 10f;
                }
                if (input.Is(i, NavLayer.Wire)) input.Obstructions.Add(new ScatterInput.Obstruction { X = cx, Z = cz, Radius = 0.6f, Height = 1.2f });
            }
            // what stands up on the field: the map's trees, stumps, logs and wrecks
            for (int i = 0; i < map.Props.Length; i++)
            {
                var p = map.Props[i];
                float radius, height;
                switch (p.Kind)
                {
                    case PropKind.Tree: radius = 0.5f; height = 6f; break;
                    case PropKind.BrokenTree: radius = 0.5f; height = 3f; break;
                    case PropKind.Stump: radius = 0.4f; height = 1f; break;
                    case PropKind.Log: radius = 0.3f; height = 0.6f; break;
                    case PropKind.Wreck: radius = 2.5f; height = 2.5f; break;
                    case PropKind.Bridge: continue;
                    default: radius = 0.4f; height = 1f; break;
                }
                input.Obstructions.Add(new ScatterInput.Obstruction { X = p.Pos.x, Z = p.Pos.z, Radius = radius, Height = height });
            }
            // the houses and the rear buildings: taken inside, their walls standing round; a lantern by each rear one
            foreach (var h in hamlets)
            {
                input.Occupied.Add(new ScatterInput.Footprint { X = h.Centre.x, Z = h.Centre.z, HalfX = h.Radius * 0.75f, HalfZ = h.Radius * 0.75f, Yaw = 0f, Dugout = false });
                for (int k = 0; k < 12; k++)
                {
                    float a = k * Mathf.PI / 6f;
                    input.Obstructions.Add(new ScatterInput.Obstruction { X = h.Centre.x + Mathf.Cos(a) * h.Radius * 0.8f, Z = h.Centre.z + Mathf.Sin(a) * h.Radius * 0.8f, Radius = 0.5f, Height = 5f });
                }
                if (kit.Houses != null && h.House >= 0 && h.House < kit.Houses.Length && kit.Houses[h.House].Set == "Military")
                    input.RearLanterns.Add(new Vector2(h.Centre.x + h.Radius + 0.7f, h.Centre.z));
            }
            // the shelters and dugouts the trench sites hold: taken inside (and stocked), their shells standing
            foreach (var site in sites)
            {
                var fp = site.Blueprint.Footprint;
                var c = site.Position + site.Rotation * fp.center;
                input.Occupied.Add(new ScatterInput.Footprint { X = c.x, Z = c.z, HalfX = fp.extents.x, HalfZ = fp.extents.z, Yaw = site.Rotation.eulerAngles.y, Dugout = true });
                input.Obstructions.Add(new ScatterInput.Obstruction { X = c.x, Z = c.z, Radius = Mathf.Max(fp.extents.x, fp.extents.z), Height = 2f });
            }
            // the supply roads: a polyline a side, in pairs of points
            if (map.SupplyRoad.IsCreated)
                for (int k = 0; k + 1 < map.SupplyRoad.Length; k += 2)
                    input.Roads.Add(new ScatterInput.Segment { X0 = map.SupplyRoad[k].x, Z0 = map.SupplyRoad[k].z, X1 = map.SupplyRoad[k + 1].x, Z1 = map.SupplyRoad[k + 1].z });
            // each side's front (its trench nearest the middle) and rear (its trench nearest its own edge)
            int frontA = -1, frontB = -1; int rowA = -1, rowB = int.MaxValue, rearA = int.MaxValue, rearB = -1;
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var trench = map.Trenches[t];
                int minRow = int.MaxValue, maxRow = -1;
                for (int i = trench.CellStart; i < trench.CellStart + trench.CellCount; i++)
                {
                    int row = map.TrenchCells[i] / map.NavWidth;
                    minRow = Mathf.Min(minRow, row); maxRow = Mathf.Max(maxRow, row);
                }
                if (maxRow < 0) continue;
                if (trench.OwnerTeam == 0) { rearA = Mathf.Min(rearA, minRow); if (maxRow > rowA) { rowA = maxRow; frontA = t; } }
                else { rearB = Mathf.Max(rearB, maxRow); if (minRow < rowB) { rowB = minRow; frontB = t; } }
            }
            if (rearA != int.MaxValue) input.RearA = rearA * ScatterInput.Cell - 4f;
            if (rearB >= 0) input.RearB = (rearB + 1) * ScatterInput.Cell + 4f;
            if (map.LinkCells.IsCreated)
                for (int k = 0; k < map.LinkCells.Length; k++)
                {
                    int cell = map.LinkCells[k], row = cell / map.NavWidth, col = cell % map.NavWidth;
                    if (frontA >= 0 && Mathf.Abs(row - rowA) <= 2) input.LinksA.Add(new Vector2Int(col, row));
                    else if (frontB >= 0 && Mathf.Abs(row - rowB) <= 2) input.LinksB.Add(new Vector2Int(col, row));
                }
            // the season and the coast
            input.Frozen = SceneTints.Now.Frozen;
            input.Coast = map.HasSea;
            if (map.HasSea)
            {
                if (map.SeaSide == 1) { input.SandFromZ = map.SeaStartZ; input.SandToZ = map.ShoreZ; input.WaterFromZ = map.ShoreZ; input.WaterToZ = float.PositiveInfinity; }
                else { input.SandFromZ = map.ShoreZ; input.SandToZ = map.SeaStartZ; input.WaterFromZ = float.NegativeInfinity; input.WaterToZ = map.ShoreZ; }
            }
            return input;
        }

        void Scatter(MapData map, BattlefieldSurface surface)
        {
            if (!ReferenceEquals(scatterMap, map))
            {
                var input = BuildScatterInput(map, surface);
                var field = ScatterField.Build(input);
                scatter.Clear();
                ScatterLayers.Place(input, field, scatter);
                LastScatterInput = input; scatterMap = map;
            }
            float w = map.SizeMeters.x, l = map.SizeMeters.y;
            for (int i = 0; i < scatter.Count; i++)
            {
                var s = scatter[i];
                if (s.X < 0.5f || s.Z < 0.5f || s.X > w - 0.5f || s.Z > l - 0.5f) continue;
                if (surface.At(s.X, s.Z).Hollow >= 0) continue;   // a shell hole since: the grass and the kit went with the earth
                var module = ModuleOf(s, out float sink, out float size);
                if (module == null) continue;
                emit(module, Matrix4x4.TRS(new Vector3(s.X, surface.VisualHeight(s.X, s.Z) - sink, s.Z), Quaternion.Euler(0f, s.Yaw, 0f), Vector3.one * (s.Scale * size)));
            }
        }

        /// <summary>The kit piece a placement is drawn with, how far it sinks into the ground, and the kind's own size.</summary>
        BattlefieldKit.Module ModuleOf(in ScatterInstance s, out float sink, out float size)
        {
            sink = .02f; size = 1f;
            switch (s.Kind)
            {
                case ScatterKind.Grass: return kit.grassMicro;
                case ScatterKind.GrassAccent: size = .7f; return kit.grass;
                case ScatterKind.Flower: return kit.poppiesMicro;
                case ScatterKind.FrostTuft: return kit.frostTuft;
                case ScatterKind.CampKit:
                    sink = s.Variant == 2 ? .06f : .015f;
                    switch (s.Variant)
                    {
                        case 0: return kit.ammoTin;
                        case 1: return kit.messKit;
                        case 2: return kit.spade;
                        case 3: return kit.helmet;
                        case 4: return kit.boots;
                        default: return kit.hatchLid;
                    }
                case ScatterKind.Crate: return kit.supplies;
                case ScatterKind.Lantern: sink = .05f; return kit.lantern;
                case ScatterKind.WallDebris: sink = .01f; return s.Variant == 0 ? kit.looseBoards : kit.shellCases;
                case ScatterKind.ShellStack: sink = .05f; return kit.shellStack;
            }
            return null;
        }
    }
}
