// Phase: prop index (2026-10-19, implemented) — depends on: PropDef/PropRules, MapData (builds and feeds it)
// Which props stand near a point, without walking all of them. DeformationSystem.Shake measured every prop in the map
// against every blast impact, and a barrage lands a dozen to forty impacts in one tick on a map carrying hundreds of
// props, so almost all of that work was on props nowhere near the burst.
// Props are append-only: MapData.AddProp is the only writer, Pos and Cell never change after it, and a prop that is
// worn down or cleared keeps its index. So this index is built once, grows by one entry per add, and never rebuilds.
// It is derived from MapData.Props, which is already hashed, so it is NOT hashed itself.
using System;
using Unity.Collections;
using Unity.Mathematics;

namespace TW.Sim.Terrain
{
    /// <summary>A uniform grid of prop indices over the map: one singly linked list per grid cell, newest first.</summary>
    public struct PropIndex : IDisposable
    {
        /// <summary>Four nav cells wide, so a 10 m burst touches nine buckets and a bucket holds a handful of props.</summary>
        public const float CellSize = 8f;

        public int Width, Length;
        /// <summary>First prop in each grid cell, -1 empty.</summary>
        public NativeList<int> Heads;
        /// <summary>Next prop in the same grid cell, -1 end; one entry per prop, indexed by prop index.</summary>
        public NativeList<int> Next;
        /// <summary>The furthest any wreck on this map reaches past its own middle (PropRules.WreckBlastReach times its
        /// size). Shake takes this off the distance to a wreck, so a query must pad its radius by it or it would miss
        /// hits the full scan found.</summary>
        public float MaxWreckReach;

        public bool IsCreated => Heads.IsCreated;

        public PropIndex(float2 sizeMeters, Allocator allocator)
        {
            Width = math.max(1, (int)math.ceil(sizeMeters.x / CellSize));
            Length = math.max(1, (int)math.ceil(sizeMeters.y / CellSize));
            Heads = new NativeList<int>(Width * Length, allocator);
            Heads.Length = Width * Length;
            for (int i = 0; i < Heads.Length; i++) Heads[i] = -1;
            Next = new NativeList<int>(256, allocator);
            MaxWreckReach = 0f;
        }

        /// <summary>The grid cell a world position falls in, clamped like MapData.NavCellOf: a prop outside the map's
        /// stated size keys to the nearest edge cell rather than out of range.</summary>
        public int KeyOf(float3 pos)
        {
            int x = math.clamp((int)(pos.x / CellSize), 0, Width - 1);
            int z = math.clamp((int)(pos.z / CellSize), 0, Length - 1);
            return z * Width + x;
        }

        /// <summary>Put prop <paramref name="propIndex"/> (just appended to MapData.Props) in its cell. O(1).</summary>
        public void Add(int propIndex, PropDef prop)
        {
            if (!Heads.IsCreated) return;
            while (Next.Length <= propIndex) Next.Add(-1);
            int key = KeyOf(prop.Pos);
            Next[propIndex] = Heads[key];
            Heads[key] = propIndex;
            if (PropRules.IsWreckage(prop.Kind))
                MaxWreckReach = math.max(MaxWreckReach, PropRules.WreckBlastReach * (prop.Scale > 0f ? prop.Scale : 1f));
        }

        /// <summary>Every prop whose cell overlaps the circle of <paramref name="radius"/> (padded by MaxWreckReach) around
        /// <paramref name="centre"/>, in ASCENDING prop index. The order matters: DeformationSystem.Shake feeds
        /// PropHarm, which folds (prop, stage) into the sim's hash chain and raises its events in visit order, so the
        /// candidates must come out in the same order the full scan visited them.</summary>
        public void Query(float3 centre, float radius, NativeList<int> into)
        {
            into.Clear();
            if (!Heads.IsCreated) return;
            float reach = radius + MaxWreckReach;
            int x0 = math.clamp((int)math.floor((centre.x - reach) / CellSize), 0, Width - 1);
            int x1 = math.clamp((int)math.floor((centre.x + reach) / CellSize), 0, Width - 1);
            int z0 = math.clamp((int)math.floor((centre.z - reach) / CellSize), 0, Length - 1);
            int z1 = math.clamp((int)math.floor((centre.z + reach) / CellSize), 0, Length - 1);
            for (int z = z0; z <= z1; z++)
                for (int x = x0; x <= x1; x++)
                    for (int p = Heads[z * Width + x]; p >= 0; p = Next[p])
                        into.Add(p);
            // insertion sort: a few buckets of a handful of props each, so tens of entries at most
            for (int i = 1; i < into.Length; i++)
            {
                int v = into[i], j = i - 1;
                while (j >= 0 && into[j] > v) { into[j + 1] = into[j]; j--; }
                into[j + 1] = v;
            }
        }

        public void Dispose()
        {
            if (Heads.IsCreated) Heads.Dispose();
            if (Next.IsCreated) Next.Dispose();
        }
    }
}
