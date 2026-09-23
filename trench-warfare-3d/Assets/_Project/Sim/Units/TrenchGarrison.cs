// Phase: A3 (implemented) — depends on: MapData trench cells (P0), SimWorld.PostCell/PostKind (A3, claude-10),
// MovementSystem to walk a man to his post, StanceSystem to decide what he does when he gets there.
// (This file kept the stub's name. Entering and leaving a trench is done by MovementSystem; what is added here is
// where a man stands once he is in one.)
// Gives every man garrisoned in a trench a POST of his own: a firing post hard against the parapet, or a reserve post
// back from it. Without this a garrison has nothing placing it but the soft repulsion in SeparationJob, which relaxes
// a crowd into an evenly spaced single file down the middle of the duckboards — which is exactly what the owner did
// not want (2026-09-22: "i do not want them to all line up on similair spots, there should be more of them in
// different places ready to shoot and defend ... they should spread around the trench").
// Firing posts come from MapData.FireStepCells (one per column, at the parapet), reserve posts from the trench's own
// floor cells. About three men in five take a firing post. Posts are thinned by a hash so the ones that exist are
// irregularly spaced: a trench reads as men holding a line, not as a queue.
// Everything is decided from (seed, trench, slot) and never from the tick, so a man keeps the same post for as long
// as he holds the trench, every machine agrees, and a replay reproduces it exactly.
using Unity.Collections;
using TW.Sim.Terrain;

namespace TW.Sim.Units
{
    public sealed class TrenchGarrisonSystem : ISimSystem
    {
        /// <summary>Of ten men in a trench, about this many take a post at the parapet; the rest stand back.</summary>
        public const int FiringInTen = 6;
        /// <summary>
        /// A candidate cell is left out this often (in sixteen), so the posts are not evenly spaced. Low, because
        /// the irregularity now comes from TrenchPost.Offset (TW.Sim.Core) - a man stands somewhere in his cell rather than at
        /// its centre - and deleting posts to get the same effect cost capacity the garrison could not spare:
        /// at 5/16 a 92-man garrison found only 61 posts and a third of it had nowhere to be (measured).
        /// </summary>
        public const int ThinnedInSixteen = 2;
        /// <summary>Its own stream, so it never correlates with another system drawing in the same tick.</summary>
        const uint StreamId = 11;
        /// <summary>The post layout belongs to the map, not to the match, so it is drawn from a fixed stream.</summary>
        const uint LayoutSeed = 0x5EA71Fu;

        public int Order => SimSystemOrder.TrenchGarrison;

        readonly MapData map;
        NativeArray<int> cellPost;    // nav cell -> post index, -1 if the cell is not a post
        NativeArray<int> postCell;    // post index -> its nav cell
        NativeArray<byte> postKind;   // post index -> 1 firing (at the parapet), 2 reserve
        NativeArray<int> holder;      // post index -> the slot holding it, -1 free. Rebuilt every step from the world.

        public TrenchGarrisonSystem(MapData map) { this.map = map; }

        /// <summary>
        /// Lays out every post on the map once. The fire step is offered first, so a cell that is both fire step and
        /// trench floor becomes a firing post; the floor cells that are left become reserve posts.
        /// </summary>
        public void Initialize(SimWorld world)
        {
            cellPost = new NativeArray<int>(map.NavWidth * map.NavLength, Allocator.Persistent);
            for (int i = 0; i < cellPost.Length; i++) cellPost[i] = -1;
            var cells = new NativeList<int>(Allocator.Temp);
            var kinds = new NativeList<byte>(Allocator.Temp);
            for (int t = 0; t < map.Trenches.Length; t++)
            {
                var def = map.Trenches[t];
                for (int k = 0; k < def.FireStepCount; k++) Offer(map.FireStepCells[def.FireStepStart + k], 1, cells, kinds);
                for (int k = 0; k < def.CellCount; k++) Offer(map.TrenchCells[def.CellStart + k], 2, cells, kinds);
            }
            postCell = new NativeArray<int>(cells.Length, Allocator.Persistent);
            postKind = new NativeArray<byte>(cells.Length, Allocator.Persistent);
            holder = new NativeArray<int>(cells.Length, Allocator.Persistent);
            for (int i = 0; i < cells.Length; i++) { postCell[i] = cells[i]; postKind[i] = kinds[i]; holder[i] = -1; }
            cells.Dispose(); kinds.Dispose();
        }

        /// <summary>Takes a cell as a post unless it is one already, or the thinning hash says leave this stretch empty.</summary>
        void Offer(int cell, byte kind, NativeList<int> cells, NativeList<byte> kinds)
        {
            if ((uint)cell >= (uint)cellPost.Length || cellPost[cell] >= 0) return;
            // a ladder is a trench cell too (GreyboxMapGenerator puts link cells in both lists), and it has to stay
            // clear for the men coming over the top: never post anybody on one
            if ((map.NavLayers[cell] & (byte)NavLayer.Link) != 0) return;
            if (SimRandom.Mix(LayoutSeed, 0u, StreamId, (uint)cell) % 16u < ThinnedInSixteen) return;   // a gap in the line
            cellPost[cell] = cells.Length;
            cells.Add(cell); kinds.Add(kind);
        }

        public void Step(SimWorld w)
        {
            if (!holder.IsCreated || holder.Length == 0) return;
            int n = w.HighWater;
            for (int i = 0; i < holder.Length; i++) holder[i] = -1;

            // Who still holds what. A man loses his post when he dies, leaves the trench, or finds a lower slot on it.
            // Ascending slot order throughout, so a contested post is always settled the same way on every machine.
            for (int i = 0; i < n; i++)
            {
                // only clear a man who actually holds something: at 3,000 units this loop was making two writes
                // per non-garrisoned slot per tick, to hashed arrays, for men who had no post to release
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || w.TrenchId[i] < 0) { if (w.PostCell[i] >= 0) Release(w, i); continue; }
                int cell = w.PostCell[i];
                if (cell < 0) continue;
                if ((uint)cell >= (uint)cellPost.Length || cellPost[cell] < 0 || map.CellTrenchId[cell] != w.TrenchId[i]) { Release(w, i); continue; }
                int post = cellPost[cell];
                if (holder[post] >= 0) { Release(w, i); continue; }
                holder[post] = i;
            }

            // Everyone left takes the nearest free post of the kind his own hash gives him, falling back to the other
            // kind rather than standing about with nowhere to be.
            for (int i = 0; i < n; i++)
            {
                if ((w.Flags[i] & (uint)UnitFlags.Alive) == 0 || w.TrenchId[i] < 0 || w.PostCell[i] >= 0) continue;
                short t = w.TrenchId[i];
                if ((uint)t >= (uint)map.Trenches.Length) continue;
                // The trench gets its OWN word rather than being packed into the slot. `t * 4096 + i` left
                // 512 slots of head room: SimConfig.MaxSlots is already 3584, and its own comment says the
                // ceiling is 3,000 units "and the rest is head room for vehicles and emplacements", so the
                // number has been raised once and sits at 87.5% of the limit. At 4096 trench t slot 4096
                // would collide with trench t+1 slot 0 and adjacent trenches would share this decision,
                // silently: the 6-in-10 split would stop being independent per trench and no test would say so.
                byte wanted = SimRandom.Mix(w.Config.Seed, (uint)t, StreamId, (uint)i) % 10u < FiringInTen ? (byte)1 : (byte)2;
                int post = Nearest(w, i, map.Trenches[t], wanted);
                if (post < 0) post = Nearest(w, i, map.Trenches[t], wanted == 1 ? (byte)2 : (byte)1);
                if (post < 0) continue;                                   // the trench is full: he holds where he stands
                holder[post] = i;
                w.PostCell[i] = postCell[post];
                w.PostKind[i] = postKind[post];
            }
        }

        /// <summary>
        /// How far apart men stand when the trench has the room, in nav cells. 2 cells = 6 m between neighbours.
        /// A hashed stretch of the line was tried here first and thrown away: minimising the walk against the
        /// stretch walks a man two thirds of the way to it, which on a 300 m trench is 200 m of marching, and the
        /// garrison was still crossing the map when the shooting started. Spacing is local, so the cure is local.
        /// </summary>
        public const int Roomiest = 2;

        /// <summary>
        /// The free post of this kind nearest the man, with as much elbow room as the trench can still afford: he is
        /// offered the widely spaced posts first, then the closer ones, then any free post at all. An empty trench
        /// therefore spreads a garrison out, a filling one packs it down by degrees, and a full one puts men shoulder
        /// to shoulder rather than leaving them with nowhere to be. Insisting on the room instead of falling back was
        /// what left a third of a 92-man garrison postless, collapsed onto the centreline under separation alone.
        /// </summary>
        int Nearest(SimWorld w, int slot, in TrenchDef def, byte kind)
        {
            for (int room = Roomiest; room > 0; room--)
            {
                int post = Search(w, slot, def, kind, room);
                if (post >= 0) return post;
            }
            return Search(w, slot, def, kind, 0);
        }

        int Search(SimWorld w, int slot, in TrenchDef def, byte kind, int room)
        {
            int start = kind == 1 ? def.FireStepStart : def.CellStart, count = kind == 1 ? def.FireStepCount : def.CellCount;
            int best = -1, bestDistance = int.MaxValue;
            int sx = (int)(w.Position[slot].x / MapData.NavCellSize), sz = (int)(w.Position[slot].z / MapData.NavCellSize);
            for (int k = 0; k < count; k++)
            {
                int cell = kind == 1 ? map.FireStepCells[start + k] : map.TrenchCells[start + k];
                if ((uint)cell >= (uint)cellPost.Length) continue;
                int post = cellPost[cell];
                if (post < 0 || postKind[post] != kind || holder[post] >= 0) continue;
                int dx = cell % map.NavWidth - sx, dz = cell / map.NavWidth - sz;
                int d = dx * dx + dz * dz;
                // A tie goes to whichever candidate comes FIRST in FireStepCells/TrenchCells - generator
                // order, not cell order. Both machines walk the same array, so it is stable either way; it
                // is simply not the ordering the old comment here claimed.
                if (d >= bestDistance) continue;
                if (room > 0 && Crowded(cell, room)) continue;
                bestDistance = d; best = post;
            }
            return best;
        }

        /// <summary>Is any cell within <paramref name="room"/> of this one already manned? Nav cells are 2 m, so a
        /// room of 1 keeps men 4 m apart and a room of 2 keeps them 6 m apart.</summary>
        bool Crowded(int cell, int room)
        {
            int cx = cell % map.NavWidth, cz = cell / map.NavWidth;
            for (int dz = -room; dz <= room; dz++)
                for (int dx = -room; dx <= room; dx++)
                {
                    if (dx == 0 && dz == 0) continue;
                    int x = cx + dx, z = cz + dz;
                    if (x < 0 || z < 0 || x >= map.NavWidth || z >= map.NavLength) continue;
                    int post = cellPost[z * map.NavWidth + x];
                    if (post >= 0 && holder[post] >= 0) return true;
                }
            return false;
        }

        static void Release(SimWorld w, int slot) { w.PostCell[slot] = -1; w.PostKind[slot] = 0; }

        /// <summary>Who stands where is authoritative: it decides cover, line of sight and what a man is drawn doing.</summary>
        public ulong Hash(ulong h) => holder.IsCreated ? SimHash.Array(holder, h) : h;

        public void Dispose()
        {
            if (cellPost.IsCreated) cellPost.Dispose();
            if (postCell.IsCreated) postCell.Dispose();
            if (postKind.IsCreated) postKind.Dispose();
            if (holder.IsCreated) holder.Dispose();
        }
    }
}
