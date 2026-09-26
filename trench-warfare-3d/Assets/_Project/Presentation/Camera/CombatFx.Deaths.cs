// Phase: B1 / docs/21 phase 4 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared
// pools): what a Death leaves on the field. The controller decided how he dies (AnimationController.Death.cs) and
// wrote it to its ring; this reads that record by slot and tick, never State[slot], because the event is handled
// after every tick of the frame ran and the slot may already hold another man. A blast death that flies high loses
// limbs (Gibs, more of them in a heap); a man who died alight is doused, leaves a pool of burning fuel, lies charred
// and smoulders for a while (a small smoke card every SmoulderEvery seconds). UnitAlight from the sim's BurningSystem
// lights and douses the drawn torch (Flamethrower), which puts the controller on Clip.Burning.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Presentation;
using TW.Presentation.Units;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        struct Smoulder { public Vector3 At; public float Until, Next, Seed; }
        readonly List<Smoulder> smoulders = new List<Smoulder>(48);
        public const int MaxSmoulders = 48;
        public const float SmoulderSeconds = 8f, SmoulderEvery = 0.45f;

        /// <summary>The sim's BurningSystem: a man caught fire (b = 1, scalar = seconds) or it went out (b = 0).</summary>
        void OnAlight(SimEvent e)
        {
            if (e.A < 0) return;
            if (e.B == 1) flames.Ignite(e.A, Mathf.Max(0.5f, e.Scalar));
            else flames.Douse(e.A);
        }

        void OnDeath(SimEvent e)
        {
            var w = Host.Local.World;
            // a tank leaves a wreck (TankRenderer), not a body. Its Death comes just before its VehicleDestroyed, while
            // the tank view still has the slot; the archetype would be a later tenant's if the slot was refilled
            if (e.A >= 0 && e.A < w.HighWater && (SceneHooks.IsTankSlot != null ? SceneHooks.IsTankSlot(e.A) : VehicleArchetype.IsTank(w.Archetype[e.A]))) return;
            if (bodies.Count >= MaxBodies) bodies.RemoveAt(0);
            Vector3 p = Host.Presenter != null && e.A >= 0 ? (Vector3)Host.Presenter.Drawn(e.A) : (Vector3)e.Pos;   // where he was drawn, so the corpse does not hop
            p.y = RenderGround.Sample(Host.Local.Map, p.x, p.z) + 0.02f;
            byte team = e.A >= 0 && e.A < w.Team.Length ? w.Team[e.A] : (byte)0;
            float fellYaw = Mathf.Atan2(e.Dir.x, e.Dir.z) * Mathf.Rad2Deg;
            int death = (Mathf.FloorToInt(p.x * 13f) ^ Mathf.FloorToInt(p.z * 29f)) & 3;
            bool burning = e.B == (int)DeathCause.Burning || e.B == (int)DeathCause.Beam;
            // he goes down as the figure he was (VATRenderer plays the death and holds it); without it, a still box figure
            if (units != null && units.Ready)
            {
                // the controller's record of this death: the clip it chose for his stance, gait, cause and the side it came
                // from, how far a shell throws him, how many went down beside him. By slot and tick, not State[slot].
                var anim = Host != null ? Host.Animation : null;
                DeathRecord rec = default;
                bool controlled = anim != null && Host.UseAnimationController && e.A >= 0 && anim.TryDeath(e.A, e.Tick, out rec);
                Clip deathClip = controlled ? rec.Clip : Clip.None;
                float yaw = controlled ? rec.Yaw : e.A >= 0 && e.A < w.HighWater ? w.Yaw[e.A] : fellYaw * Mathf.Deg2Rad;
                // the clip he was hit in fades into the death (the controller's own cross-fade, carried into the fallen buffer)
                Clip from = controlled ? rec.PrevClip : Clip.None; float fromPhase = 0f, fade = 0f;
                if (from != Clip.None)
                {
                    var prev = Clips.Table[(int)from]; float pp = prev.Seconds > 0f ? rec.PrevFrame / prev.Seconds : 0f;
                    fromPhase = prev.Loop ? pp - Mathf.Floor(pp) : Mathf.Min(pp, 1f); fade = Mathf.Max(rec.Fade, 0.2f);
                }
                Vector3 fly = controlled ? new Vector3(rec.ThrowX, rec.ThrowUp, rec.ThrowZ) : Vector3.zero;
                int density = controlled ? rec.Density : 0;
                // a shell that threw him high takes him apart: the sim's Death says a blast did it and how hard, the
                // figure loses the limbs (a bit each, read by the VAT shader), and they fly off with his helmet and rifle
                int gib = e.B == (int)DeathCause.Blast && e.Scalar > 0f && fly.y > 0.6f ? Gibs(e.A, p, yaw, team, fly, density) : 0;
                float grime = controlled ? rec.Grime : anim != null && e.A >= 0 && e.A < anim.Grime.Length ? anim.Grime[e.A] : 0f;   // he goes down in the mud he wore
                int chr = controlled ? rec.Char : burning ? 3 : 0;
                flames.Douse(e.A);   // whatever killed him, the torch on his slot goes out with him: the next tenant is not alight
                if (burning)
                {
                    // the fuel he carried burns on under the body, and he smoulders
                    flames.Spill(p, 1.2f, 6f);
                    AddSmoulder(p);
                }
                units.AddFallen(new Vector3(p.x, p.y - 0.02f, p.z), yaw, team, death, deathClip, e.A >= 0 && e.A < w.HighWater ? w.Archetype[e.A] : 0, from, fromPhase, fade, fly, gib, grime, density, chr);
            }
            else
            {
                flames.Douse(e.A);
                if (burning) AddSmoulder(p);
                bodies.Add(new Body { Pos = p, Rot = Lie(p.x, p.z, fellYaw, 0.6f), Born = Time.time, Team = team, Variant = (byte)death });
            }
            // his helmet comes off as he goes down and rolls a step away
            if (!(units != null && units.Ready) && Near(p, 60f) && chunks.Count < 700)   // the animated figure keeps his helmet on
                chunks.Add(new Chunk { Pos = p + Vector3.up * 1.2f, Vel = Quaternion.Euler(0f, fellYaw + Mathf.Lerp(-70f, 70f, Hash01(p.x, p.z, 1)), 0f) * Vector3.forward * Mathf.Lerp(1.2f, 2.4f, Hash01(p.x, p.z, 2)) + Vector3.up * 1.6f,
                    Born = Time.time, Life = 4f, Size = 1f, Kind = 6 });
        }

        /// <summary>The cosmetic dice: a hash of a position and a salt in 0..1, so two peers throw the same helmet and
        /// the same wisps (decisions.md: presentation only, seeded so replays agree).</summary>
        static float Hash01(float x, float z, int salt)
        {
            uint h = (uint)Mathf.FloorToInt(x * 37f) * 73856093u ^ (uint)Mathf.FloorToInt(z * 37f) * 19349663u ^ (uint)salt * 83492791u;
            h ^= h >> 13; h *= 0x5bd1e995u; h ^= h >> 15;
            return (h & 0xFFFFFF) / 16777216f;
        }

        void AddSmoulder(Vector3 at)
        {
            if (smoulders.Count >= MaxSmoulders) smoulders.RemoveAt(0);
            smoulders.Add(new Smoulder { At = at, Until = Time.time + SmoulderSeconds, Next = Time.time + 0.2f, Seed = Hash01(at.x, at.z, 3) });
        }

        /// <summary>A thin thread of smoke off every charred body for a while after it fell.</summary>
        void TickSmoulders(float now)
        {
            if (smoulders.Count == 0) return;
            bool draw = books != null && books.Ready;
            for (int i = smoulders.Count - 1; i >= 0; i--)
            {
                var s = smoulders[i];
                if (now > s.Until) { smoulders.RemoveAt(i); continue; }
                if (now < s.Next) continue;
                s.Next = now + SmoulderEvery * (0.8f + 0.4f * s.Seed);
                smoulders[i] = s;
                if (!draw) continue;
                float left = Mathf.Clamp01((s.Until - now) / SmoulderSeconds);
                books.Add(FlipbookFx.Book.Smoke, s.At + new Vector3(Mathf.Sin(s.Seed * 6.28f + now) * 0.15f, 0.35f, Mathf.Cos(s.Seed * 6.28f + now) * 0.15f),
                    0.8f * (0.6f + 0.4f * left), 3f, FlipbookFx.Kind.None, velocity: Vector3.up * 0.6f, grow: 0.5f, alpha: 0.35f * (0.4f + 0.6f * left));
            }
        }
    }
}
