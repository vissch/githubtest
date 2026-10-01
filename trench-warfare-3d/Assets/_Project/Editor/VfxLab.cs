// Phase: VFX pass (2026-10-01, tooling) — depends on: SimHost, TankCapture, RiderLab, BlastSystem, NightLights, Atmosphere
// Editor-only staging of one visual effect at a time, for the capture-and-critique loop (VfxStills films it; the owner:
// "go through all the vfx see how they can be better"). Every effect is made by the game's own systems: a burst queued
// into the sim's BlastSystem, a support call, a machine shelled to death, a star shell asked of NightLights, rain set
// on the Atmosphere. Nothing here is part of the game.
//  - Shell(x, z, radius, damage) / Fire(x, z, radius): one burst into every world's BlastSystem, bursting next tick;
//  - Call(ability, x, z, args): a support call for player 0 (silver topped up first): 1 HE, 3 chlorine, 6 smoke,
//    10 strafe, 11 the beam (args: AbilityArgs.Pack(heading, pattern, length) for a line ability, 0 its plain form);
//  - Men(n, x, z, team, spacing): a row of riflemen standing still along x, facing +z (something for a burst to throw);
//  - Wreck(archetype, x, z): a machine held where it stands and shelled to death 1.5 s later: its cook-off, then its
//    wreck burning and smoking for minutes;
//  - Burning(archetype, x, z): a machine alight and still alive (TankCapture.Ignite): its deck fire and smoke column;
//  - Flare(): a star shell now (NightLights.FireStarShell); Rain(amount): rain on the field (0 off);
//  - Later(seconds, act): something done a moment from now;
//  - Scene(name, x, z): a whole staging by name (Scenes lists them).
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Editor
{
    public static class VfxLab
    {
        /// <summary>Every staging Scene knows, in the order VfxStills films them by default.</summary>
        public static readonly string[] Scenes = { "shell", "barrage", "cookoff", "burning", "gas", "smoke", "beam", "strafe", "fire", "flare", "rain" };

        static SimHost Host => Object.FindFirstObjectByType<SimHost>();

        static string Burst(float x, float z, float radius, float damage, BlastShape shape)
        {
            var h = Host; if (h == null) return "no SimHost";
            var impact = new Impact { Pos = new float3(x, 0f, z), Damage = damage, Radius = radius, Suppression = 60f, CraterRadius = shape == BlastShape.Shell ? radius * 0.4f : 0f, CraterDepth = 0.4f, Source = (int)OffMapAbilityId.HeBarrage, Player = 1, Shape = (int)shape };
            bool ok = h.WriteWorlds(m => m.World.GetSystem<BlastSystem>()?.Queue(impact));
            return ok ? shape + " at " + x + ", " + z + " next tick" : "worlds a tick apart: try again";
        }

        /// <summary>One shell, bursting next tick in every world.</summary>
        public static string Shell(float x, float z, float radius = 6f, float damage = 400f) => Burst(x, z, radius, damage, BlastShape.Shell);

        /// <summary>An incendiary burst: the men inside it catch fire.</summary>
        public static string Fire(float x, float z, float radius = 5f) => Burst(x, z, radius, 5f, BlastShape.Incendiary);

        /// <summary>A support call for player 0 at a point (silver topped up first).</summary>
        public static string Call(int ability, float x, float z, int args = 0)
        {
            var h = Host; if (h == null) return "no SimHost";
            TankCapture.Silver();
            h.Issue(new SimCommand { Tick = h.Local.World.Tick, Type = CommandType.SupportFire, A = ability, B = args, Pos = new float3(x, 0f, z) });
            return "called " + (OffMapAbilityId)ability + " at " + x + ", " + z;
        }

        /// <summary>n riflemen of a side standing still in a line along x from (x, z), facing +z; returns their slots.</summary>
        public static string Men(int n, float x, float z, int team = 0, float spacing = 1.6f)
        {
            var h = Host; if (h == null) return "no SimHost";
            var sb = new System.Text.StringBuilder("slots");
            for (int k = 0; k < n; k++)
            {
                string s = TankCapture.Spawn(team, InfantryArchetype.Rifle, x + k * spacing, z, 0f);
                if (!s.StartsWith("slot ")) return s;
                int slot = int.Parse(s.Substring(5));
                h.WriteWorlds(m => m.World.Speed[slot] = 0f);
                sb.Append(' ').Append(slot);
            }
            return sb.ToString();
        }

        /// <summary>A machine held where it stands and shelled to death 1.5 s later.</summary>
        public static string Wreck(int archetype, float x, float z)
        {
            string tank = TankCapture.Spawn(1, archetype, x, z, 90f);
            if (!tank.StartsWith("slot ")) return tank;
            RiderLab.Stop(int.Parse(tank.Substring(5)));
            return tank + "; " + Later(1.5f, () => Shell(x, z, 4f, 50000f));
        }

        /// <summary>A machine alight and still alive, held where it stands.</summary>
        public static string Burning(int archetype, float x, float z)
        {
            string tank = TankCapture.Spawn(1, archetype, x, z, 90f);
            if (!tank.StartsWith("slot ")) return tank;
            int slot = int.Parse(tank.Substring(5));
            RiderLab.Stop(slot);
            return tank + "; " + Later(0.5f, () => TankCapture.Ignite(slot, 0.9f));
        }

        /// <summary>A star shell now (night: it lights the field for about 16 s).</summary>
        public static string Flare()
        {
            var lights = Object.FindFirstObjectByType<TW.Presentation.Terrain.NightLights>();
            if (lights == null) return "no NightLights";
            lights.FireStarShell();
            return "star shell";
        }

        /// <summary>Rain on the field, 0..1 (0 off).</summary>
        public static string Rain(float amount)
        {
            var sky = Object.FindFirstObjectByType<TW.Presentation.Terrain.Atmosphere>();
            if (sky == null) return "no Atmosphere";
            sky.Rain = Mathf.Clamp01(amount);
            return "rain " + sky.Rain.ToString("0.0");
        }

        /// <summary>Something done `seconds` from now (real time in Play), its answer logged.</summary>
        public static string Later(float seconds, System.Func<string> act)
        {
            float due = Time.time + seconds;
            void Tick()
            {
                if (!Application.isPlaying) { UnityEditor.EditorApplication.update -= Tick; return; }
                if (Time.time < due) return;
                UnityEditor.EditorApplication.update -= Tick;
                Debug.Log("VfxLab.Later: " + act());
            }
            UnityEditor.EditorApplication.update += Tick;
            return "in " + seconds.ToString("0.#") + " s";
        }

        /// <summary>A whole staging by name at (x, z), the effect's centre.</summary>
        public static string Scene(string name, float x = 100f, float z = 120f)
        {
            switch (name)
            {
                case "shell": return Later(0.8f, () => Shell(x, z));                                   // one shell on open ground, seen before it lands
                case "barrage": return Later(0.5f, () => Call((int)OffMapAbilityId.HeBarrage, x, z));  // the HE barrage: shells over seconds
                case "cookoff": return Wreck(VehicleArchetype.Maw, x, z);                               // a big machine dies: cook-off, then its wreck burns
                case "burning": return Burning(VehicleArchetype.Tusk, x, z);                            // alight and alive: deck fire and smoke column
                case "gas": return Later(0.5f, () => Call((int)OffMapAbilityId.ChlorineGas, x, z));
                case "smoke": return Later(0.5f, () => Call((int)OffMapAbilityId.SmokeScreen, x, z));
                case "beam": return Later(0.5f, () => Call((int)OffMapAbilityId.Beam, x - 10f, z, AbilityArgs.Pack(90, 0, 0)));   // walks east along z
                case "strafe": return Later(0.5f, () => Call((int)OffMapAbilityId.StrafeRun, x - 20f, z, AbilityArgs.Pack(90, 0, 0)));
                case "fire": return Men(5, x - 3f, z, 1) + "; " + Later(0.8f, () => Fire(x, z, 5f));    // an incendiary on a row: men alight
                case "flare": return Flare();
                case "rain": return Rain(1f);
                default: return "unknown scene " + name;
            }
        }
    }
}
