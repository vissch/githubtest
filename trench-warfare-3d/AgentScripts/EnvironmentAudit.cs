using System;
using System.Text;
using Unity.Collections;
using UnityEngine;
using TW.Sim.Terrain;
using TW.Presentation.Terrain;

public static class EnvironmentAudit
{
    public static async System.Threading.Tasks.Task<string> RepaintBudget()
    {
        var view = UnityEngine.Object.FindFirstObjectByType<GreyboxTerrainView>();
        var props = UnityEngine.Object.FindFirstObjectByType<BattlefieldProps>();
        var host = UnityEngine.Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        ulong hash = host.Local.Map.Hash(14695981039346656037UL);
        var terrainEvent = typeof(GreyboxTerrainView).GetMethod("OnSimEvent", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        var propEvent = typeof(BattlefieldProps).GetMethod("OnSimEvent", System.Reflection.BindingFlags.NonPublic | System.Reflection.BindingFlags.Instance);
        var e = new TW.Sim.SimEvent { Type = TW.Sim.SimEventType.CraterStamp, Pos = new Unity.Mathematics.float3(38f, 0f, 43f), Scalar = 5f };
        terrainEvent.Invoke(view, new object[] { e }); int one = view.PendingPaintTiles;
        for (int i = 0; i < 11; i++) terrainEvent.Invoke(view, new object[] { e });
        if (view.PendingPaintTiles != one) throw new Exception("Overlapping paint tiles were not coalesced");
        propEvent.Invoke(props, new object[] { e });
        var watch = System.Diagnostics.Stopwatch.StartNew(); float peak = 0f;
        while (view.PendingPaintTiles > 0 && watch.Elapsed.TotalSeconds < 15)
        {
            await System.Threading.Tasks.Task.Delay(16);
            peak = Mathf.Max(peak, view.LastPaintMilliseconds);
        }
        if (view.PendingPaintTiles != 0) throw new Exception("Repaint queue did not drain");
        if (hash != host.Local.Map.Hash(14695981039346656037UL)) throw new Exception("Repainting mutated the map");
        return $"12 overlapping presentation-only stamps coalesced to {one} tiles; drained in {watch.Elapsed.TotalMilliseconds:F0}ms, peak observed tile work {peak:F2}ms/frame; map hash unchanged. Excludes texture upload/mesh rebuild costs; timings apply to this machine.";
    }

    public static string Run()
    {
        if (!Application.isPlaying) throw new InvalidOperationException("Run EnvironmentAudit in Play mode so its temporary kit is released through Unity's runtime lifecycle.");
        var report = new StringBuilder();
        using var kit = new BattlefieldKit();
        for (int sample = 0; sample < 3; sample++)
        {
            var p = BattlefieldParams.ShelledForest((uint)(1917 + sample * 137));
            p.Width = 90f + sample * 30f; p.Length = 240f + sample * 60f;
            using var map = BattlefieldGenerator.Create(p, Allocator.TempJob);
            ulong before = map.Hash(14695981039346656037UL);
            var surface = new BattlefieldSurface(map);
            var joins = new System.Collections.Generic.Dictionary<Vector3, Vector3>();
            float maxSway = 0f;
            foreach (var edge in surface.Edges)
            {
                if (edge.DressLength < .5f || Vector3.Dot(edge.DressOutward, edge.Outward) < .5f) throw new Exception($"Collapsed or reversed trench segment: center={edge.Center}, length={edge.DressLength}, dot={Vector3.Dot(edge.DressOutward, edge.Outward)}");
                if (edge.Link && Vector3.Distance(edge.Center, edge.DressCenter) > .001f) throw new Exception("Trench contour moved a ladder opening");
                maxSway = Mathf.Max(maxSway, Vector3.Distance(edge.Center, edge.DressCenter));
                var tangent = Vector3.Cross(Vector3.up, edge.Outward);
                for (int end = 0; end < 2; end++)
                {
                    var original = edge.Center + tangent * (end == 0 ? -1f : 1f);
                    var shaped = end == 0 ? edge.DressStart : edge.DressEnd;
                    if (joins.TryGetValue(original, out var other) && Vector3.Distance(other, shaped) > .001f) throw new Exception("Open join in the trench contour");
                    joins[original] = shaped;
                }
            }
            if (maxSway < .25f || maxSway > 1.6f) throw new Exception("Trench sway outside intended bounds");
            var composer = new BattlefieldComposer(kit, 1917 + sample);
            ulong first = 0; int instances = 0;
            for (int repeat = 0; repeat < 3; repeat++)
            {
                if (repeat == 1) composer = new BattlefieldComposer(kit, 1917 + sample); // Independent regeneration, then cached update.
                ulong hash = 14695981039346656037UL; int count = 0;
                var watch = System.Diagnostics.Stopwatch.StartNew();
                composer.Build(map, surface, (module, matrix) =>
                {
                    count++;
                    for (int i = 0; i < 16; i++)
                    {
                        float value = matrix[i];
                        if (float.IsNaN(value) || float.IsInfinity(value)) throw new Exception("Non-finite placement transform");
                        hash = (hash ^ (uint)BitConverter.SingleToInt32Bits(value)) * 1099511628211UL;
                    }
                    hash = (hash ^ (uint)module.Mesh.vertexCount) * 1099511628211UL;
                });
                watch.Stop();
                if (repeat == 2) report.Append($"cached update={watch.Elapsed.TotalMilliseconds:F2}ms; ");
                if (repeat == 0) { first = hash; instances = count; }
                else if (hash != first || count != instances) throw new Exception("Composition is not repeatable");
            }
            foreach (var site in composer.Sites)
                if (!BattlefieldComposer.Fits(map, surface, site.Blueprint, site.Position, site.Rotation)) throw new Exception("Placed composition violates its footprint");
            for (int z = 0; z < map.NavLength; z++) for (int x = 0; x < map.NavWidth; x++)
            {
                var layer = (NavLayer)map.NavLayers[map.NavIndex(x, z)];
                if ((layer & (NavLayer.Trench | NavLayer.Link)) == 0) continue;
                float wx = (x + .5f) * MapData.NavCellSize, wz = (z + .5f) * MapData.NavCellSize;
                if (Mathf.Abs(surface.VisualHeight(wx, wz) - map.Height.Sample(wx, wz)) > .001f) throw new Exception("Visual profile displaced a trench/link floor");
            }
            if (before != map.Hash(14695981039346656037UL)) throw new Exception("Presentation mutated simulation map");
            report.AppendLine($"seed={p.Seed}, {p.Width}x{p.Length}: {composer.Sites.Count} valid sites, {surface.Edges.Count} boundary edges, {instances} instances; repeatability/map hash/floor checks PASS");
        }
        return report.ToString();
    }
}
