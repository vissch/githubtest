// Phase: deaths (2026-09-28, tooling) — depends on: SimHost, TankCapture, DeathGags, BlastSystem
// Editor-only helpers to stage and film the absurd deaths (DeathGags) in Play, driven from the command line (tw eval).
// Every death here is made by the sim's own systems inside a tick (a queued shell or incendiary burst, a support call,
// an enemy's gun, a machine driven over a row), never by a Despawn inside WriteWorlds: the next Step clears the events
// raised there, so the picture would never see the Death and draw no body.
//  - Absurd(x): pin how absurd (DeathGags.Pin; a negative value hands it back to the fx.deathAbsurd knob);
//  - Row(n, x, z, team, spacing, hp): n men in a line along x, facing +z, with hp each (1 = the first hit kills);
//  - Shell(x, z, radius, damage) / Fire(x, z, radius): one burst into every world's BlastSystem, bursting next tick;
//  - Call(ability, x, z, args): a support call for player 0 (silver topped up first): 1 HE, 3 chlorine, 11 the beam;
//  - Disarm(slot): a machine's guns out of action, so it runs men down rather than shelling them;
//  - DriveAt(slot, x, z): a machine driven straight at a point;
//  - Parts(x, z): one of each of a man's parts dropped in a row, to look at them close;
//  - Later(seconds, act): something done a moment from now (the "machine" scene's shell);
//  - Machine(archetype, x, z): a machine held where it stands and shelled to death a moment later (its absurd death);
//  - Scene(name, x, z): a whole staging by name (shot, mg, shell, heap, gas, fire, beam, crush, parts, and the machines:
//    machine (a Tusk), maw, salvo, skimmer, walker (a Pincer)), then film it with TankCapture.Follow/Shot or CaptureRig.
// None of this is part of the game; it exists for the capture-and-critique loop.
using System.Text;
using Unity.Mathematics;
using UnityEngine;
using TW.Sim;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Presentation;
using TW.Presentation.Tactical;

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
        public static string Row(int n = 6, float x = 100f, float z = 120f, int team = 0, float spacing = 1.6f, float hp = 1f, float maxHp = -1f, byte archetype = 0)
        {
            var h = Host; if (h == null) return "no SimHost";
            var sb = new StringBuilder("slots");
            for (int k = 0; k < n; k++)
            {
                string s = TankCapture.Spawn(team, archetype, x + k * spacing, z, 0f);
                if (!s.StartsWith("slot ")) return s;
                int slot = int.Parse(s.Substring(5));
                h.WriteWorlds(m => { m.World.Hp[slot] = hp; m.World.Speed[slot] = 0f; if (maxHp > 0f) m.World.MaxHp[slot] = maxHp; });
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

        /// <summary>A support call for player 0 at a point (silver topped up first); `args` is SimCommand.B
        /// (AbilityArgs: a line ability's heading, pattern and length; 0 its plain form, up the field).</summary>
        public static string Call(int ability, float x, float z, int args = 0)
        {
            var h = Host; if (h == null) return "no SimHost";
            TankCapture.Silver();
            h.Issue(new SimCommand { Tick = h.Local.World.Tick, Type = CommandType.SupportFire, A = ability, B = args, Pos = new float3(x, 0f, z) });
            return "called " + (OffMapAbilityId)ability + " at " + x + ", " + z;
        }

        /// <summary>A whole staging by name at (x, z): a row of men and what kills them.</summary>
        public static string Scene(string name, float x = 100f, float z = 120f)
        {
            switch (name)
            {
                case "shot": return Row(6, x, z, 0) + "; " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 4f, z + 30f, 180f) + " " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 6f, z + 30f, 180f);
                case "mg": return Row(6, x, z, 0) + "; " + TankCapture.Spawn(1, InfantryArchetype.Machinegunner, x + 4f, z + 30f, 180f);
                // hit and not killed (CombatFx.HitBlood, the blood on the uniform): men tough enough to take many hits, their
                // hp and max hp raised together so the stain grows with what they lose, a machine gun and two rifles on them
                case "wounds": return Row(6, x, z, 0, 1.6f, 900f, 900f) + "; " + TankCapture.Spawn(1, InfantryArchetype.Machinegunner, x + 4f, z + 30f, 180f) + " " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 2f, z + 30f, 180f) + " " + TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 6f, z + 30f, 180f);
                case "shell": return Row(6, x, z, 0, 3f, 100f) + "; " + Shell(x + 7.5f, z - 2f, 8f, 600f);
                case "heap": return Row(8, x, z, 0, 0.7f, 100f) + "; " + Shell(x + 2.5f, z, 6f, 800f);
                // play test (test/frog-deaths): the frogs under a shell, and the frogs' side standing
                case "frogheap": return Row(8, x, z, 0, 0.7f, 100f, archetype: InfantryArchetype.Frog) + "; " + Later(0.8f, () => Shell(x + 2.5f, z, 6f, 800f));   // the row seen standing first
                case "frogshell": return Row(6, x, z, 0, 3f, 100f, archetype: InfantryArchetype.Frog) + "; " + Later(0.8f, () => Shell(x + 7.5f, z - 2f, 8f, 600f));
                case "frogshot": return Row(6, x, z, 0, 1.6f, 1f, archetype: InfantryArchetype.Frog) + "; " + Tough(TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 4f, z + 18f, 180f)) + " " + Tough(TankCapture.Spawn(1, InfantryArchetype.Rifle, x + 6f, z + 18f, 180f));   // 18 m: at 30 the bank hid them (round 1)
                case "frogmg": return Row(6, x, z, 0, 1.6f, 900f, 900f, InfantryArchetype.Frog) + "; " + Tough(TankCapture.Spawn(1, InfantryArchetype.Machinegunner, x + 4f, z + 18f, 180f));
                case "croaker": return Machine(VehicleArchetype.Croaker, x, z, 0);   // the frog mech: two legs
                case "hopper": return Machine(VehicleArchetype.Hopper, x, z, 0);     // the frog gunship: drawn flying, a dead one falls
                case "frogs": return Row(6, x, z, 0, 1.6f, 100f, archetype: InfantryArchetype.Frog) + "; " + TankCapture.Spawn(0, VehicleArchetype.Croaker, x + 2f, z + 9f, 0f) + " " + TankCapture.Spawn(0, VehicleArchetype.Hopper, x + 9f, z + 9f, 0f);
                case "gas": return Row(6, x, z, 1, 1.6f, 30f) + "; " + Call((int)OffMapAbilityId.ChlorineGas, x + 4f, z);   // the enemy's men: player 0 calls it
                case "fire": return Row(6, x, z, 0, 1.6f, 40f) + "; " + Fire(x + 4f, z, 6f);
                case "beam": return Row(6, x, z, 1, 1.6f, 100f) + "; " + Call((int)OffMapAbilityId.Beam, x - 10f, z, AbilityArgs.Pack(90, 0, 0));   // it walks east from there along the row (x >= 14 or the call is off the map)
                case "parts": return Parts(x, z);
                case "machine": return Machine(VehicleArchetype.Tusk, x, z);      // a turret, tracks
                case "maw": return Machine(VehicleArchetype.Maw, x, z);           // a cupola, road wheels, tracks
                case "salvo": return Machine(VehicleArchetype.Salvo, x, z);       // a rack of rockets on a turret, tyres
                case "skimmer": return Machine(VehicleArchetype.Skimmer, x, z);   // a fan astern, a cushion
                case "walker": return Machine(VehicleArchetype.Pincer, x, z);     // six legs
                case "crush":
                {
                    string row = Row(6, x, z, 1, 1.2f, 100f);
                    string tank = TankCapture.Spawn(0, VehicleArchetype.Maw, x + 3f, z - 15f, 0f);
                    if (!tank.StartsWith("slot ")) return row + "; " + tank;
                    int slot = int.Parse(tank.Substring(5));
                    return row + "; " + tank + "; " + Disarm(slot) + "; " + DriveAt(slot, x + 3f, z + 12f);   // unarmed, or it shells the row before it gets there
                }
                default: return "scenes: shot, mg, shell, heap, gas, fire, beam, crush, parts, machine, maw, salvo, skimmer, walker";
            }
        }

        /// <summary>A machine of the enemy's held where it stands, facing east, and a shell that obliterates it once the
        /// picture draws it there (a machine killed the moment it appears is drawn blending in from the map's corner).</summary>
        public static string Machine(int archetype, float x, float z, int team = 1)
        {
            string tank = TankCapture.Spawn(team, archetype, x + 4f, z, 90f);
            if (!tank.StartsWith("slot ")) return tank;
            RiderLab.Stop(int.Parse(tank.Substring(5)));
            return tank + "; " + Later(1.5f, () => Shell(x + 4f, z, 4f, 50000f));
        }

        /// <summary>One of each of a man's parts (DebrisRenderer.Figure, cut from his figure) dropped in a row from (x, z)
        /// along x, a metre apart and a metre up, in the first side's uniform, to lie there two minutes: a close look.</summary>
        public static string Parts(float x, float z)
        {
            var d = DebrisRenderer.Instance; var h = Host;
            if (d == null || !d.Ready || h == null || h.Local == null) return "no DebrisRenderer";
            var rng = new DebrisRng(new Vector3(x, 0f, z), 5u);
            var kinds = new[] { DebrisRenderer.Piece.Head, DebrisRenderer.Piece.Helm, DebrisRenderer.Piece.Torso, DebrisRenderer.Piece.Pelvis, DebrisRenderer.Piece.Arm,
                                DebrisRenderer.Piece.Leg, DebrisRenderer.Piece.Boot, DebrisRenderer.Piece.UpperHalf, DebrisRenderer.Piece.LowerHalf, DebrisRenderer.Piece.Pack };
            for (int k = 0; k < kinds.Length; k++)
            {
                float px = x + k * 1.1f;
                var at = new Vector3(px, RenderGround.Sample(h.Local.Map, px, z) + 1f, z);
                d.Throw(kinds[k], at, Vector3.up * 0.5f, 1f, new Color(0.60f, 0.53f, 0.33f), ref rng, 120f);
            }
            return kinds.Length + " parts dropped from " + x + ", " + z;
        }

        /// <summary>A machine driven straight at a point (VehicleKinematicsSystem.DriveStraight), not along a flow field (the
        /// crush stills' Maw sat still on a field goal 15 m short of its row).</summary>
        public static string DriveAt(int slot, float x, float z)
        {
            var h = Host; if (h == null || h.Local == null) return "no SimHost";
            float speed = h.Local.World.Units.Roster[h.Local.World.Archetype[slot]].Speed;
            h.WriteWorlds(m =>
            {
                m.Vehicles.Drive[slot] = TW.Sim.Nav.VehicleKinematicsSystem.DriveStraight;
                m.Vehicles.DriveTarget[slot] = new float3(x, 0f, z);
                if (m.World.Speed[slot] <= 0f) m.World.Speed[slot] = speed;
            });
            return $"slot {slot} driving at {x:0.0}, {z:0.0}";
        }

        /// <summary>`act` once, `seconds` of play from now (a frame at a time, from the editor's update).</summary>
        /// <summary>A spawned shooter the row cannot kill in a scene's time (frog round 3: the frogs shot their two riflemen
        /// in a second and the scene had nothing left to show). Takes TankCapture.Spawn's answer, returns it.</summary>
        public static string Tough(string spawned)
        {
            var h = Host;
            if (h == null || !spawned.StartsWith("slot ")) return spawned;
            int slot = int.Parse(spawned.Substring(5));
            h.WriteWorlds(m => { m.World.Hp[slot] = 50000f; m.World.MaxHp[slot] = 50000f; });
            return spawned;
        }

        public static string Later(float seconds, System.Func<string> act)
        {
            float due = Time.time + seconds;
            void Tick()
            {
                if (!Application.isPlaying) { UnityEditor.EditorApplication.update -= Tick; return; }
                if (Time.time < due) return;
                UnityEditor.EditorApplication.update -= Tick;
                Debug.Log("DeathLab.Later: " + act());
            }
            UnityEditor.EditorApplication.update += Tick;
            return "in " + seconds.ToString("0.#") + " s";
        }

        /// <summary>A machine's guns put out of action (GunHealth 0, as a knocked-out gun) once its gunnery has taken the slot
        /// on: the first tick after a spawn sets every gun sound, so this waits for it, a frame at a time, for up to 600.</summary>
        public static string Disarm(int slot)
        {
            int frames = 0;
            void Tick()
            {
                var h = Host;
                if (h == null || h.Local == null || ++frames > 600) { UnityEditor.EditorApplication.update -= Tick; return; }
                var g = h.Local.World.GetSystem<TankGunnerySystem>();
                if (g == null || g.Gen[slot] != h.Local.World.Generation[slot]) return;
                if (h.WriteWorlds(m =>
                {
                    var gm = m.World.GetSystem<TankGunnerySystem>();
                    for (int k = 0; k < TankGunnerySystem.Guns; k++) gm.GunHealth[slot * TankGunnerySystem.Guns + k] = 0f;
                })) UnityEditor.EditorApplication.update -= Tick;
            }
            UnityEditor.EditorApplication.update += Tick;
            return "slot " + slot + " disarmed once its guns are up";
        }
    }
}
