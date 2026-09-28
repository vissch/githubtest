// Phase: deaths (2026-09-28, tooling) — depends on: SimHost, TankCapture, DeathGags, BlastSystem
// Editor-only helpers to stage and film the absurd deaths (DeathGags) in Play, driven from the command line (tw eval).
// Every death here is made by the sim's own systems inside a tick (a queued shell or incendiary burst, a support call,
// an enemy's gun, a machine driven over a row), never by a Despawn inside WriteWorlds: the next Step clears the events
// raised there, so the picture would never see the Death and draw no body.
//  - Absurd(x): pin how absurd (DeathGags.Pin; a negative value hands it back to the fx.deathAbsurd knob);
//  - Row(n, x, z, team, spacing, hp): n men in a line along x, facing +z, with hp each (1 = the first hit kills);
//  - Shell(x, z, radius, damage) / Fire(x, z, radius): one burst into every world's BlastSystem, bursting next tick;
//  - Call(ability, x, z): a support call for player 0 (silver topped up first): 1 HE, 3 chlorine, 11 the beam;
//  - Scene(name, x, z): a whole staging by name (shot, mg, shell, heap, gas, fire, beam, crush), then film it with
//    TankCapture.Follow/Shot or CaptureRig.
// None of this is part of the game; it exists for the capture-and-critique loop.
using System.Text;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Editor
{
    public static class DeathLab
    {
        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        /// <summary>Pins how absurd the deaths are (0 today's, 1 the new look, 2 ludicrous; negative: the knob decides).</summary>
        public static string Absurd(float intensity)
        {
            DeathGags.Pin(intensity);
            return "fx.deathAbsurd " + (intensity < 0f ? "from the knob: " + DeathGags.Intensity : DeathGags.Intensity.ToString("0.00"));
        }

        /// <summary>n men in a line along x from (x, z), facing +z, each with hp hit points; returns their slots.</summary>
        public static string Row(int n = 6, float x = 100f, float z = 120f, int team = 0, float spacing = 1.6f, float hp = 1f)
        {
            var h = Host; if (h == null) return "no SimHost";
            var sb = new StringBuilder("slots");
            for (int k = 0; k < n; k++)
            {
                string s = TankCapture.Spawn(team, 0, x + k * spacing, z, 0f);
                if (!s.StartsWith("slot ")) return s;
                int slot = int.Parse(s.Substring(5));
                h.WriteWorlds(m => { m.World.Hp[slot] = hp; m.World.Speed[slot] = 0f; });
                sb.Append(' ').Append(slot);
            }
            return sb.ToString();
        }

        static string Burst(float x, float z, float radius, float damage, BlastShape shape)
        {
            var h = Host; if (h == null) return "no SimHost";
            var impact = new Impact { Pos = new float3(x, 0f, z), Damage = damage, Radius = radius, Suppression = 60f, CraterRadius = shape == BlastShape.Shell ? radius * 0.4f : 0f, CraterDepth = 0.4f, Source = (int)OffMapAbilityId.HeBarrage, Player = 1, Shape = (int)shape };
            bool ok = h.WriteWorlds(m => m.World.GetSystem<BlastSystem>()?.Queue(impact));
            return ok ? shape + " at " + x + ", " + z + " next tick" : "worlds a tick apart: try again";
        }

        /// <summary>One shell, bursting next tick in every world (the men it kills die of it as they would in battle).</summary>
        public static string Shell(float x, float z, float radius = 6f, float damage = 400f) => Burst(x, z, radius, damage, BlastShape.Shell);

        /// <summary>An incendiary burst: the men inside it catch fire and burn to death over the next seconds.</summary>
        public static string Fire(float x, float z, float radius = 5f) => Burst(x, z, radius, 5f, BlastShape.Incendiary);

        /// <summary>A support call for player 0 at a point (silver topped up first).</summary>
        public static string Call(int ability, float x, float z)
        {
            var h = Host; if (h == null) return "no SimHost";
            TankCapture.Silver();
            h.Issue(new SimCommand { Tick = h.Local.World.Tick, Type = CommandType.SupportFire, A = ability, Pos = new float3(x, 0f, z) });
            return "called " + (OffMapAbilityId)ability + " at " + x + ", " + z;
        }

        /// <summary>A whole staging by name at (x, z): a row of men and what kills them.</summary>
        public static string Scene(string name, float x = 100f, float z = 120f)
        {
            switch (name)
            {
                case "shot": return Row(6, x, z, 0) + "; " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 4f, z + 30f, 180f) + " " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 6f, z + 30f, 180f);
                case "mg": return Row(6, x, z, 0) + "; " + TankCapture.Spawn(1, InfantryArchetype.Machinegunner, x + 4f, z + 30f, 180f);
                case "shell": return Row(6, x, z, 0, 3f, 100f) + "; " + Shell(x + 7.5f, z - 2f, 8f, 600f);
                case "heap": return Row(8, x, z, 0, 0.7f, 100f) + "; " + Shell(x + 2.5f, z, 6f, 800f);
                case "gas": return Row(6, x, z, 1, 1.6f, 30f) + "; " + Call((int)OffMapAbilityId.ChlorineGas, x + 4f, z);   // the enemy's men: player 0 calls it
                case "fire": return Row(6, x, z, 0, 1.6f, 40f) + "; " + Fire(x + 4f, z, 6f);
                case "beam": return Row(6, x, z, 1, 1.6f, 100f) + "; " + Call((int)OffMapAbilityId.Beam, x - 10f, z);
                case "crush":
                {
                    string row = Row(6, x, z, 1, 1.2f, 100f);
                    string tank = TankCapture.Spawn(0, VehicleArchetype.Maw, x + 3f, z - 25f, 0f);
                    if (!tank.StartsWith("slot ")) return row + "; " + tank;
                    return row + "; " + tank + "; " + RiderLab.Drive(int.Parse(tank.Substring(5)), 50f);
                }
                default: return "scenes: shot, mg, shell, heap, gas, fire, beam, crush";
            }
        }
    }
}
