// Phase: A3 (implemented) — depends on: MapData.SeaSide/ShoreZ/SeaLevel, SimWorld.SeaLift, RosterEntry
// The sea half of the supply line (owner, 2026-09-22: "we're also adding an ocean with boats arriving. bringing
// units."). A map with a sea beyond one rear edge lands that team's reinforcements instead of standing them up at
// a spawn point: a deploy is paid for at once and goes aboard a craft standing off the beach, which runs in, grounds
// itself, drops its ramp and puts the men ashore on the sand. They walk inland from the water like anybody else.
//
// Everything here is authoritative state and hashed: a craft's position decides where men appear. The whole system
// is a few dozen floats and runs on the main thread — there is no job here, and there does not need to be.
//
// A craft refuses cargo once it is close in, so nobody materialises on a boat that has already beached, and the lift
// as a whole refuses when every berth is full. A refused deploy falls back to the old instant spawn, so the boats
// can never block a player who is buying faster than the sea can carry.
using Unity.Collections;
using Unity.Mathematics;
using TW.Sim.Combat;
using TW.Sim.Terrain;

namespace TW.Sim.Match
{
    /// <summary>What a craft is doing. Drawn by Presentation.Terrain.LandingCraftView.</summary>
    public enum LandingState : byte { Idle = 0, RunIn = 1, Grounding = 2, Unloading = 3, Retracting = 4 }

    public sealed class SeaLandingSystem : ISimSystem, ISeaLift
    {
        public const int MaxCraft = 6;          // in the water at once, both sides together
        public const int Berths = 8;            // men one craft carries; a vehicle takes the whole hold
        public const float StandOff = 96f;      // metres seaward of the waterline a craft appears
        public const float RunSpeed = 7.0f;     // m/s in
        public const float BackSpeed = 4.5f;    // m/s out again
        public const float Draught = 2.2f;      // metres of bow that ground short of the waterline
        public const float RampReach = 4.6f;    // how far up the sand the ramp puts a man down
        public const int GroundTicks = 16;      // ramp down (0.8 s)
        public const int OutTicks = 3;          // between two men leaving the ramp
        public const int CloseToCargo = 26;     // metres out: past this a craft takes nobody else aboard
        public const float Beam = 3.4f;         // half the width men spread over as they come off
        public const int StoresFirst = 800;     // ticks before the first stores run (40 s)
        public const int StoresEvery = 1100;    // and between them after that (55 s): the beach is never dead
        public const int StoresLinger = 130;    // how long one lies with its ramp down before it pulls off
        // The gunboats standing off the beach (the owner's Cutter). They are part of the simulation, not scenery:
        // where they lie is worked out from the map so both machines agree, and every so often one of them lays a
        // shell on the line inland. The view draws them where the sim says they are.
        public const int Ships = 3;
        public const float ShipStandOff = 150f, ShipSpread = 110f;   // close enough that the fog leaves them a silhouette
        /// <summary>
        /// How far in and out of ShipStandOff a gunboat may be staggered. Named because it decides how much
        /// WATER a map needs: the furthest a ship can lie is ShipStandOff + ShipOutMax seaward of the waterline,
        /// and a map with less coast than that puts its fleet past its own edge. These were literals inside the
        /// placement, so nothing could compare them against a map - and nothing did. ShipSpread is the spread
        /// ALONG the coast and is a different quantity; reaching for it here is the obvious wrong guess.
        /// Renaming only: the numbers, every ship position and every hash are exactly as they were.
        /// </summary>
        public const float ShipOutMin = -60f, ShipOutMax = 120f;
        public const int ShipEvery = 460;       // ticks between two salvos from the fleet (23 s)
        public const float ShipDamage = 300f, ShipRadius = 7.5f, ShipSuppression = 80f, ShipCrater = 2.4f;
        public const int ShipSource = 60;       // Explosion.a for a naval shell

        readonly MapData map;
        NativeArray<float3> ships;              // where each gunboat lies: fixed for the match, hashed with the rest
        BlastSystem blast;
        NativeArray<byte> team, state, aboard;
        NativeArray<int> timer, nextOut;
        NativeArray<float3> pos;
        NativeArray<float> yaw, lane;
        NativeArray<byte> cargoArch, cargoVehicle;
        NativeArray<float> cargoHp, cargoSpeed;
        NativeArray<byte> cargoSlot, cargoRank;   // which roster slot paid for him and the veteran rank he carries (UnitDeployed)
        SimWorld world;

        public SeaLandingSystem(MapData map)
        {
            this.map = map;
            team = new NativeArray<byte>(MaxCraft, Allocator.Persistent);
            state = new NativeArray<byte>(MaxCraft, Allocator.Persistent);
            aboard = new NativeArray<byte>(MaxCraft, Allocator.Persistent);
            timer = new NativeArray<int>(MaxCraft, Allocator.Persistent);
            nextOut = new NativeArray<int>(MaxCraft, Allocator.Persistent);
            pos = new NativeArray<float3>(MaxCraft, Allocator.Persistent);
            yaw = new NativeArray<float>(MaxCraft, Allocator.Persistent);
            lane = new NativeArray<float>(MaxCraft, Allocator.Persistent);
            cargoArch = new NativeArray<byte>(MaxCraft * Berths, Allocator.Persistent);
            cargoVehicle = new NativeArray<byte>(MaxCraft * Berths, Allocator.Persistent);
            cargoHp = new NativeArray<float>(MaxCraft * Berths, Allocator.Persistent);
            cargoSpeed = new NativeArray<float>(MaxCraft * Berths, Allocator.Persistent);
            cargoSlot = new NativeArray<byte>(MaxCraft * Berths, Allocator.Persistent);
            cargoRank = new NativeArray<byte>(MaxCraft * Berths, Allocator.Persistent);
        }

        public int Order => SimSystemOrder.Economy + 20;   // after the commands that filled the holds, before anything moves

        public void Initialize(SimWorld world)
        {
            this.world = world;
            if (map.HasSea) world.SeaLift = this;
            blast = world.GetSystem<BlastSystem>();          // null without combat: then the fleet only sits there
            ships = new NativeArray<float3>(Ships, Allocator.Persistent);
            for (int k = 0; k < Ships; k++)
            {
                // spread along the coast and staggered out to sea, off the map's own seed so both machines agree
                var rng = SimRandom.For(world.Config.Seed, 0u, SimRandom.SystemId.Deployment, 0x5EC0u + (uint)k);
                float w = map.SizeMeters.x;
                ships[k] = new float3(
                    w * 0.5f + (k - (Ships - 1) * 0.5f) * ShipSpread + rng.NextFloat(-30f, 30f),
                    map.SeaLevel,
                    map.ShoreZ + map.SeaAway * (ShipStandOff + rng.NextFloat(ShipOutMin, ShipOutMax)));
            }
        }

        /// <summary>Where a gunboat of the fleet lies (LandingCraftView draws them here).</summary>
        public float3 ShipAt(int k) => ships.IsCreated && k >= 0 && k < Ships ? ships[k] : float3.zero;

        // ---- what the view needs ------------------------------------------------------------------------------
        public int Count => MaxCraft;
        public LandingState StateOf(int craft) => (LandingState)state[craft];
        public float3 PositionOf(int craft) => pos[craft];
        public float YawOf(int craft) => yaw[craft];
        public byte TeamOf(int craft) => team[craft];
        public int AboardOf(int craft) => aboard[craft];
        /// <summary>0 shipped, 1 fully down: the ramp falls as the craft grounds and is up again as it pulls off.</summary>
        public float RampOf(int craft)
        {
            var s = (LandingState)state[craft];
            if (s == LandingState.Grounding) return math.saturate(timer[craft] / (float)GroundTicks);
            if (s == LandingState.Unloading) return 1f;
            if (s == LandingState.Retracting) return math.saturate(1f - timer[craft] / (float)GroundTicks);
            return 0f;
        }
        /// <summary>Metres the craft still has to run; 0 once it is ashore. Drives the bow wave.</summary>
        public float WayToGo(int craft) => (LandingState)state[craft] == LandingState.RunIn ? math.abs(GroundZ() - pos[craft].z) : 0f;

        // ---- the beach ----------------------------------------------------------------------------------------
        /// <summary>Where a craft grounds: its bow just short of the waterline.</summary>
        public float GroundZ() => map.ShoreZ + map.SeaAway * Draught;
        /// <summary>Where the foot of a grounded craft's ramp lands: up the sand, clear of the water.</summary>
        public float RampZ() => map.ShoreZ - map.SeaAway * RampReach;

        // ---- taking men aboard --------------------------------------------------------------------------------
        public bool Embark(SimWorld w, byte player, int rosterSlot, int veteranRank, RosterEntry entry)
        {
            if (!map.HasSea || player != map.SeaTeam) return false;
            int craft = -1;
            // an inbound craft still far enough out takes him; otherwise the next empty hull is launched
            for (int i = 0; i < MaxCraft; i++)
                if (state[i] == (byte)LandingState.RunIn && team[i] == player && aboard[i] < Berths
                    && !entry.IsVehicle && !CarriesVehicle(i) && math.abs(pos[i].z - GroundZ()) > CloseToCargo)
                { craft = i; break; }
            if (craft < 0)
            {
                for (int i = 0; i < MaxCraft; i++) if (state[i] == (byte)LandingState.Idle) { craft = i; break; }
                if (craft < 0) return false;                                  // every hull is busy: he walks up from the rear
                Launch(craft, player);
            }
            int berth = craft * Berths + aboard[craft];
            cargoArch[berth] = entry.Archetype; cargoHp[berth] = entry.Hp; cargoSpeed[berth] = entry.Speed;
            cargoVehicle[berth] = (byte)(entry.IsVehicle ? 1 : 0);
            cargoSlot[berth] = (byte)rosterSlot; cargoRank[berth] = (byte)veteranRank;
            aboard[craft] = (byte)(aboard[craft] + 1);
            return true;
        }

        /// <summary>A tank has the hold to itself: nobody rides ashore sitting on it.</summary>
        bool CarriesVehicle(int craft) => aboard[craft] > 0 && cargoVehicle[craft * Berths] != 0;

        void Launch(int craft, byte player)
        {
            var rng = SimRandom.For(world.Config.Seed, world.Tick, SimRandom.SystemId.Deployment, 0x5EA0u + (uint)craft);
            // lanes are spread along the beach and never on top of a craft already running in
            float w = map.SizeMeters.x, want = rng.NextFloat(w * .12f, w * .88f);
            for (int guard = 0; guard < 8; guard++)
            {
                bool clash = false;
                for (int i = 0; i < MaxCraft; i++) if (i != craft && state[i] != (byte)LandingState.Idle && math.abs(lane[i] - want) < 13f) clash = true;
                if (!clash) break;
                want = rng.NextFloat(w * .12f, w * .88f);
            }
            lane[craft] = want;
            team[craft] = player;
            state[craft] = (byte)LandingState.RunIn;
            timer[craft] = 0; nextOut[craft] = 0; aboard[craft] = 0;
            pos[craft] = new float3(want + rng.NextFloat(-6f, 6f), map.SeaLevel, map.ShoreZ + map.SeaAway * StandOff);
            yaw[craft] = map.SeaAway > 0f ? SimMath.Pi : 0f;                   // bows on, pointing up the beach
            world.Events.Add(world.Tick, SimEventType.CraftInbound, craft, player, pos[craft]);
        }

        // ---- the run in ---------------------------------------------------------------------------------------
        public void Step(SimWorld w)
        {
            if (!map.HasSea) return;
            // a boat comes in with stores whether or not anybody is reinforcing: the shore of a held beach is working
            // day and night, and a sea with nothing on it reads as a painted backdrop
            if (w.Tick >= StoresFirst && w.Tick % StoresEvery == 0)
            {
                bool busy = false;
                for (int i = 0; i < MaxCraft; i++) if (state[i] != (byte)LandingState.Idle) busy = true;
                if (!busy)
                    for (int i = 0; i < MaxCraft; i++)
                        if (state[i] == (byte)LandingState.Idle) { Launch(i, map.SeaTeam); break; }
            }
            // the fleet's guns: one ship at a time lays a shell on the ground inland of the beach, which is what a
            // gunboat is for and what makes the sea worth looking at while nothing is landing
            if (blast != null && w.Tick > 200 && w.Tick % ShipEvery == 0)
            {
                int k = (int)((w.Tick / ShipEvery) % Ships);
                var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.IndirectFire, 0x5ED0u + (uint)k);
                // onto the enemy of the side that holds the beach, between the lines rather than on its own men
                float inland = map.SeaStartZ - rng.NextFloat(30f, 120f);
                float3 at = new float3(rng.NextFloat(6f, map.SizeMeters.x - 6f), 0f, map.SeaAway > 0f ? inland : map.SizeMeters.y - inland);
                float3 from = ships[k];
                float3 d = at - from; d.y = 0f;
                float len = SimMath.Length(d);
                float3 flight = len > 1e-3f ? d / len : new float3(0f, 0f, 1f);
                blast.Queue(new Impact
                {
                    Pos = at, Damage = ShipDamage, Radius = ShipRadius, Suppression = ShipSuppression,
                    CraterRadius = ShipCrater, CraterDepth = ShipCrater * 0.3f, Source = ShipSource, Player = map.SeaTeam,
                    Dir = flight,   // it came in off the sea: the inland side of the burst takes the fragments
                });
                w.Events.Add(w.Tick, SimEventType.ShipFired, k, map.SeaTeam, at, flight);
            }
            float dt = w.Config.TickSeconds, ground = GroundZ(), away = map.SeaAway;
            for (int i = 0; i < MaxCraft; i++)
            {
                var p = pos[i];
                switch ((LandingState)state[i])
                {
                    case LandingState.Idle: continue;

                    case LandingState.RunIn:
                    {
                        // it comes in bows first and straightens as it closes, the last few metres slowing onto the sand
                        float left = (p.z - ground) * away;
                        float speed = RunSpeed * math.lerp(.35f, 1f, math.saturate(left / 14f));
                        p.z -= away * speed * dt;
                        p.x = math.lerp(p.x, lane[i], math.saturate(dt * 1.2f));
                        p.y = map.SeaLevel;
                        if ((p.z - ground) * away <= 0f)
                        {
                            p.z = ground;
                            state[i] = (byte)LandingState.Grounding; timer[i] = 0;
                            w.Events.Add(w.Tick, SimEventType.CraftBeached, i, team[i], p);
                        }
                        break;
                    }

                    case LandingState.Grounding:
                        timer[i]++;
                        // an empty boat lies there a while with its ramp down, as one unloading stores would
                        if (timer[i] >= GroundTicks) { state[i] = (byte)LandingState.Unloading; timer[i] = 0; nextOut[i] = aboard[i] > 0 ? 0 : StoresLinger; }
                        break;

                    case LandingState.Unloading:
                    {
                        timer[i]++;
                        if (--nextOut[i] > 0) break;
                        nextOut[i] = OutTicks;
                        if (!PutAshore(w, i)) { state[i] = (byte)LandingState.Retracting; timer[i] = 0; }
                        break;
                    }

                    case LandingState.Retracting:
                    {
                        timer[i]++;
                        p.z += away * BackSpeed * dt;
                        p.y = map.SeaLevel;
                        if ((p.z - map.ShoreZ) * away > StandOff) { state[i] = (byte)LandingState.Idle; aboard[i] = 0; timer[i] = 0; }
                        break;
                    }
                }
                pos[i] = p;
            }
        }

        /// <summary>One man down the ramp. False when the hold is empty.</summary>
        bool PutAshore(SimWorld w, int craft)
        {
            int left = aboard[craft];
            if (left <= 0) return false;
            int berth = craft * Berths + left - 1;                             // the hold empties from the back of the boat forward
            if (cargoHp[berth] <= 0f) { aboard[craft] = (byte)(left - 1); return left > 1; }
            var rng = SimRandom.For(w.Config.Seed, w.Tick, SimRandom.SystemId.Deployment, 0x5EB0u + (uint)craft * 16u + (uint)left);
            bool vehicle = cargoVehicle[berth] != 0;
            float3 at = new float3(
                math.clamp(pos[craft].x + rng.NextFloat(-Beam, Beam), 1f, map.SizeMeters.x - 1f),
                0f,
                RampZ() - map.SeaAway * (vehicle ? 2.5f : rng.NextFloat(0f, 2.2f)));
            int slot = w.Spawn(team[craft], cargoArch[berth], w.ClampToMap(at), cargoHp[berth], cargoSpeed[berth], vehicle);
            if (slot < 0) return false;                                        // the field is full: the rest stay aboard and go back out
            w.Events.Add(w.Tick, SimEventType.UnitDeployed, slot, cargoSlot[berth], w.Position[slot], new float3(cargoRank[berth], team[craft], 0f));
            cargoHp[berth] = 0f;
            aboard[craft] = (byte)(left - 1);
            return left > 1;
        }

        public ulong Hash(ulong h)
        {
            h = SimHash.Array(team, h); h = SimHash.Array(state, h); h = SimHash.Array(aboard, h);
            h = SimHash.Array(timer, h); h = SimHash.Array(nextOut, h);
            h = SimHash.Array(pos, h); h = SimHash.Array(yaw, h); h = SimHash.Array(lane, h);
            h = SimHash.Array(cargoArch, h); h = SimHash.Array(cargoVehicle, h);
            h = SimHash.Array(cargoHp, h); h = SimHash.Array(cargoSpeed, h);
            if (ships.IsCreated) h = SimHash.Array(ships, h);
            h = SimHash.Array(cargoSlot, h); h = SimHash.Array(cargoRank, h);
            return h;
        }

        public void Dispose()
        {
            if (team.IsCreated) team.Dispose();
            if (state.IsCreated) state.Dispose();
            if (aboard.IsCreated) aboard.Dispose();
            if (timer.IsCreated) timer.Dispose();
            if (nextOut.IsCreated) nextOut.Dispose();
            if (pos.IsCreated) pos.Dispose();
            if (yaw.IsCreated) yaw.Dispose();
            if (lane.IsCreated) lane.Dispose();
            if (cargoArch.IsCreated) cargoArch.Dispose();
            if (cargoVehicle.IsCreated) cargoVehicle.Dispose();
            if (cargoHp.IsCreated) cargoHp.Dispose();
            if (cargoSpeed.IsCreated) cargoSpeed.Dispose();
            if (cargoSlot.IsCreated) cargoSlot.Dispose();
            if (cargoRank.IsCreated) cargoRank.Dispose();
            if (ships.IsCreated) ships.Dispose();
        }
    }
}
