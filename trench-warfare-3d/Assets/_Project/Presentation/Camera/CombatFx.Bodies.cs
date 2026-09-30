// Phase: B1 / C4 (implemented) — part of CombatFx (see CombatFx.cs for the event dispatch and the shared pools): what
// comes off a man and a tree: gibs and kit on a blast death, a tree crown breaking, and where a man's muzzle and
// chest are drawn (for tracers and hits), all at the drawn figure scale.
using System.Collections.Generic;
using UnityEngine;
using TW.Sim;
using TW.Sim.Match;
using TW.Presentation;

namespace TW.Presentation.Tactical
{
    public sealed partial class CombatFx
    {
        /// <summary>How big a man is drawn right now (VATRenderer grows him with the zoom), so what comes off him matches.</summary>
        float FigureScale()
        {
            if (units == null) return 1f;
            var cam = Camera.main;
            float zoom = cam != null && cam.TryGetComponent<IZoomSource>(out var source) ? source.CurrentZoom : 0f;
            return units.UnitScale * Mathf.Clamp(zoom / Mathf.Max(1f, units.GrowFromZoom), 1f, units.MaxGrow);
        }

        Color ClothOf(int slot)
        {
            var w = Host.Local.World;
            return slot >= 0 && slot < w.Team.Length && w.Team[slot] == 1 ? ClothB : ClothA;
        }

        /// <summary>
        /// A shell has taken a man apart: which limbs he loses (bits 1 head, 2 left arm, 3 right arm, 4 left leg, 5 right
        /// leg; the VAT shader cuts them from the figure at the root), and the same limbs, his helmet and his rifle thrown
        /// from where he stood on the shell's own throw plus a scatter. Seeded from the place, so a replay agrees. Nothing
        /// with DebrisRenderer.Gore at 0. density: how many died beside him this moment (AnimationController's death
        /// record): in a heap more comes off, and fewer come down whole. With fx.deathAbsurd above 0 GibPlan decides it
        /// and what flies is his own arm, leg, head and helmet, cut from his figure (DebrisRenderer.Figure), maybe both
        /// his halves (GibPlan.TornBit in what comes back: lay no corpse); at 0 a tube, a clod and the kit's helmet, as ever.
        /// </summary>
        int Gibs(int slot, Vector3 at, float yaw, int team, Vector3 fly, int density = 0, float delay = 0f, byte archetype = 0)
        {
            if (DeathGags.Intensity > 0f) return OwnGibs(slot, at, yaw, team, fly, density, delay, archetype);   // fx.deathAbsurd: his own parts
            if (DebrisRenderer.Gore <= 0f || debris == null || !debris.Ready) return 0;
            var rng = new DebrisRng(at, 0x6B1u + (uint)slot);
            if (rng.Next() < 0.3f / (1f + density)) return 0;   // most men thrown by a shell alone come down whole; in a heap, few
            float scale = FigureScale();
            Color cloth = team == 1 ? ClothB : ClothA;
            Vector3 chest = at + Vector3.up * (1.2f * scale);
            Vector3 carry = new Vector3(fly.x, 0f, fly.z) * 0.9f + Vector3.up * (2.5f + fly.y * 2f);   // the shell's throw, and up
            int mask = 0, limbs = (rng.Next() < 0.35f ? 2 : 1) + Mathf.Min(density, 2);
            for (int k = 0; k < limbs; k++)
            {
                int limb = 2 + (int)(rng.Next() * 3.999f);   // an arm or a leg
                if ((mask & (1 << limb)) != 0) continue;
                mask |= 1 << limb;
                Vector3 vel = carry + rng.OnSphere() * 3.5f; vel.y = Mathf.Abs(vel.y) + 2f;
                debris.Throw(DebrisRenderer.Piece.Limb, chest + rng.OnSphere() * (0.3f * scale), vel, (limb >= 4 ? 0.85f : 0.62f) * scale, cloth, ref rng, 30f);
            }
            if (rng.Next() < 0.22f + 0.12f * density)
            {
                mask |= 1 << 1;   // his head: the helmet goes one way, the head another
                Vector3 vel = carry + rng.OnSphere() * 3f; vel.y = Mathf.Abs(vel.y) + 3f;
                debris.Throw(DebrisRenderer.Piece.Clod, chest + Vector3.up * (0.4f * scale), vel, 0.24f * scale, Skin, ref rng, 30f);
            }
            Vector3 helmetVel = carry + rng.OnSphere() * 4f; helmetVel.y = Mathf.Abs(helmetVel.y) + 4f;
            debris.Throw(DebrisRenderer.Piece.Helmet, chest + Vector3.up * (0.5f * scale), helmetVel, 0.32f * scale, Steel, ref rng, 60f);
            if (rng.Next() < 0.6f)
            {
                Vector3 vel = carry + rng.OnSphere() * 3f; vel.y = Mathf.Abs(vel.y) + 2.5f;
                debris.Throw(DebrisRenderer.Piece.Rifle, chest, vel, scale, Bark, ref rng, 60f);
            }
            int lumps = Mathf.RoundToInt(5f * DebrisRenderer.Gore * (1f + 0.5f * density));
            for (int k = 0; k < lumps; k++)
            {
                Vector3 vel = carry * 0.8f + rng.OnSphere() * 4.5f; vel.y = Mathf.Abs(vel.y) + 1.5f;
                debris.Throw(DebrisRenderer.Piece.Clod, chest, vel, rng.Range(0.07f, 0.14f) * scale, Gore, ref rng, 8f);
            }
            return mask;
        }

        /// <summary>A lump of him at fx.deathAbsurd above 0: blood red, not today's near-black (critic round 6: a swarm of
        /// dark lumps read as clods of earth and hid his parts).</summary>
        static readonly Color GoreRed = new Color(0.55f, 0.04f, 0.04f);
        readonly List<DebrisRenderer.Piece> gibPieces = new List<DebrisRenderer.Piece>(12);

        /// <summary>Gibs at fx.deathAbsurd above 0: GibPlan decides what he loses, and exactly that flies, cut from his own
        /// figure; torn in two, the upper half goes where his body would have landed (so its blood is there) and the lower
        /// half a shorter way. Kit flies at GORE 0.</summary>
        int OwnGibs(int slot, Vector3 at, float yaw, int team, Vector3 fly, int density, float delay, byte archetype)
        {
            if (debris == null || !debris.Ready) return 0;
            uint seed = (uint)Mathf.FloorToInt(at.x * 37f) * 73856093u ^ (uint)Mathf.FloorToInt(at.z * 37f) * 19349663u ^ (uint)slot * 83492791u;
            var plan = GibPlan.Decide(seed, DeathGags.Intensity, DebrisRenderer.Gore, density);
            if (plan.Whole) return 0;
            // his parts leave with him: a heap's men go a beat apart (DeathGags' delay), and theirs with them (critic
            // round 9: every part left the burst in one clump while the bodies were staggered)
            if (delay > 0.02f) pendingGibs.Add(new PendingGibs { Plan = plan, Slot = slot, At = at, Yaw = yaw, Team = team, Fly = fly, Due = Time.time + delay, Archetype = archetype });
            else ThrowGibs(plan, slot, at, yaw, team, fly, archetype);
            return plan.Mask | (plan.Torn ? GibPlan.TornBit : 0);
        }

        struct PendingGibs { public GibPlan Plan; public int Slot, Team; public Vector3 At, Fly; public float Yaw, Due; public byte Archetype; }
        readonly List<PendingGibs> pendingGibs = new List<PendingGibs>(32);

        /// <summary>Once a frame: the parts whose men leave now (OwnGibs).</summary>
        void DueGibs(float now)
        {
            for (int k = pendingGibs.Count - 1; k >= 0; k--)
            {
                var g = pendingGibs[k];
                if (now < g.Due) continue;
                pendingGibs.RemoveAt(k);
                if (debris != null && debris.Ready) ThrowGibs(g.Plan, g.Slot, g.At, g.Yaw, g.Team, g.Fly, g.Archetype);
            }
        }

        void ThrowGibs(in GibPlan plan, int slot, Vector3 at, float yaw, int team, Vector3 fly, byte archetype)
        {
            bool frog = archetype == InfantryArchetype.Frog;   // a frog's parts are cut from the frog (DebrisRenderer.FrogOf)
            var rng = new DebrisRng(at, 0x61B5u + (uint)slot);
            float figure = FigureScale(), scale = figure * GibPlan.PartScale(DeathGags.Intensity);
            Color cloth = team == 1 ? ClothB : ClothA;
            Vector3 chest = at + Vector3.up * (1.2f * figure);
            Vector3 facing = new Vector3(Mathf.Sin(yaw), 0f, Mathf.Cos(yaw));
            // his parts ride his own arc (the gag's throw, already grown): today's 2.5 + 2 x height sent them 25 m/s up and
            // out of the frame at the new heights (critic round 5 found no parts to point at)
            Vector3 carry = Reaching(fly, 0.8f, 0.8f);
            GibPlan.Pieces(plan, gibPieces);
            foreach (var piece in gibPieces)
            {
                Vector3 vel;
                var drawn = frog ? DebrisRenderer.FrogOf(piece) : piece;
                switch (piece)
                {
                    case DebrisRenderer.Piece.Arm:
                    case DebrisRenderer.Piece.Leg:
                        vel = carry + rng.OnSphere() * 3.5f; vel.y = Mathf.Abs(vel.y) + 2f;
                        debris.Throw(drawn, chest + rng.OnSphere() * (0.3f * scale), Topped(vel), scale, cloth, ref rng, 30f);
                        break;
                    case DebrisRenderer.Piece.Head:
                        vel = carry + rng.OnSphere() * 3f; vel.y = Mathf.Abs(vel.y) + 3f;
                        debris.Throw(drawn, chest + Vector3.up * (0.4f * scale), Topped(vel), scale, Skin, ref rng, 30f);
                        break;
                    case DebrisRenderer.Piece.UpperHalf:
                        debris.Throw(drawn, chest, Topped(Reaching(fly, 1f, 1f) + rng.OnSphere() * 0.8f), figure, cloth, ref rng, 40f);   // life-size: at PartScale half a man lay there twice as big as a whole one (frog critic round 6)
                        break;
                    case DebrisRenderer.Piece.LowerHalf:
                        debris.Throw(drawn, at + Vector3.up * (0.6f * figure), Topped(Reaching(fly, 0.45f, 0.6f) + rng.OnSphere() * 0.8f), figure, cloth, ref rng, 40f);
                        break;
                    case DebrisRenderer.Piece.Helm:
                        vel = carry + rng.OnSphere() * 4f; vel.y = Mathf.Abs(vel.y) + 4f;
                        if (frog) debris.Throw(DebrisRenderer.Piece.Helmet, chest + Vector3.up * (0.5f * scale), Topped(vel), 0.32f * scale, Steel, ref rng, 60f);
                        else debris.Throw(piece, chest + Vector3.up * (0.5f * scale), Topped(vel), scale, Steel, ref rng, 60f);
                        break;
                    case DebrisRenderer.Piece.Rifle:
                        vel = carry + rng.OnSphere() * 3f; vel.y = Mathf.Abs(vel.y) + 2.5f;
                        debris.Throw(piece, chest, Topped(vel), scale, Bark, ref rng, 60f);
                        break;
                    case DebrisRenderer.Piece.Pack:
                        vel = carry + rng.OnSphere() * 3f; vel.y = Mathf.Abs(vel.y) + 2.5f;
                        debris.Throw(piece, chest - facing * (0.25f * scale), Topped(vel), scale, cloth, ref rng, 60f);
                        break;
                }
            }
            for (int k = 0; k < plan.Lumps; k++)
            {
                Vector3 vel = carry * 0.8f + rng.OnSphere() * 4.5f; vel.y = Mathf.Abs(vel.y) + 1.5f;
                debris.Throw(DebrisRenderer.Piece.Clod, chest, Topped(vel), rng.Range(0.07f, 0.14f) * figure, GoreRed, ref rng, 8f);
            }
            // round 9: at 2.6 m, and none for a man who only lost a limb, the heap's aftermath showed two small splats for eight men
            float splat = plan.Torn || plan.Apart ? 3.4f : (plan.Mask & GibPlan.AllLimbs) != 0 ? 1.8f : 0f;
            if (splat > 0f && DebrisRenderer.Gore > 0f)
                AddGagMark(at, rng.Range(0f, 360f), new Vector2(splat, splat) * (Mathf.Sqrt(DebrisRenderer.Gore) * figure), SceneTints.Now.Frozen ? BloodLifeSnow : BloodLife, 3, 0.05f);
        }

        /// <summary>The launch that carries a thrown piece `far` of the throw's way and `high` of its height under the
        /// debris' gravity: the body's own arc (VATRenderer throws the corpse the same distance and height).</summary>
        /// <summary>A part's launch with its rise capped at GibPlan.PartTop (critic round 9: riding a fountain's arc under the
        /// debris' lighter gravity, parts hung in the air 3 s and more, above the frame, after their men had landed).</summary>
        static Vector3 Topped(Vector3 vel)
        {
            float most = Mathf.Sqrt(2f * DebrisMath.Gravity * GibPlan.PartTop);
            if (vel.y > most) vel.y = most;
            return vel;
        }

        static Vector3 Reaching(Vector3 fly, float far, float high)
        {
            float up = Mathf.Sqrt(2f * DebrisMath.Gravity * Mathf.Max(0.3f, fly.y * high));
            float air = 2f * up / DebrisMath.Gravity;
            return new Vector3(fly.x * far / air, up, fly.z * far / air);
        }

        /// <summary>
        /// A tree the sim has worn down (PropChanged): a standing tree loses its top, which hinges off the break and falls
        /// away from the newest burst; a broken one is shattered to the stump. The sim swaps the drawn prop the same
        /// tick (BattlefieldProps recomposes) to a snag with no top, so the crown on the ground is the only one there.
        /// </summary>
        void TreeBreaks(SimEvent e, Vector3 foot)
        {
            var props = Host.Local.Map.Props;
            if (e.A < 0 || e.A >= props.Length) return;
            var def = props[e.A];
            float s = def.Scale > 0f ? def.Scale : 0.85f + 0.3f * ((e.A * 37) % 100) / 100f;   // as BattlefieldComposer sizes it
            var kind = (TW.Sim.Terrain.PropKind)e.B;
            Vector3 away = Time.time - lastBlastAt < 0.5f ? foot - lastBlast : new Vector3(Mathf.Sin(def.Yaw + 1.1f), 0f, Mathf.Cos(def.Yaw + 1.1f));
            away.y = 0f;
            switch (kind)
            {
                case TW.Sim.Terrain.PropKind.BrokenTree:
                {
                    Vector3 pivot = foot + Vector3.up * (2.7f * s);   // the snag the kit leaves standing is 2.7 m
                    debris.Topple(DebrisRenderer.Piece.Crown, pivot, Quaternion.Euler(0f, def.Yaw * Mathf.Rad2Deg, 0f), away, 1.3f, s, Bark, 4.8f);   // falls for 1.3 s, lies a few seconds on the snag's side, then sinks
                    debris.Burst(DebrisRenderer.Piece.Shard, pivot, 10, 6f, 0.35f * s, Bark, 25f, 0f, 1.6f, away.normalized * 0.4f, e.Tick);
                    break;
                }
                case TW.Sim.Terrain.PropKind.Stump:
                    debris.Burst(DebrisRenderer.Piece.Shard, foot + Vector3.up * (1.3f * s), 14, 8f, 0.5f * s, Charred, 25f, 0f, 1.6f, away.normalized * 0.5f, e.Tick);
                    debris.Burst(DebrisRenderer.Piece.Clod, foot + Vector3.up * 0.2f, 5, 5f, 0.18f, Mud, 20f, 0f, 1.8f, default, e.Tick + 3u);
                    break;
                case TW.Sim.Terrain.PropKind.Log:   // a tree gone under a vehicle
                    debris.Burst(DebrisRenderer.Piece.Shard, foot + Vector3.up * 0.8f, 8, 5f, 0.4f * s, Bark, 25f, 0f, 1.4f, default, e.Tick);
                    break;
            }
        }

        /// <summary>
        /// Where a man's muzzle is when his figure has no sockets to say: out in front of him at the height his (drawn)
        /// stance holds a rifle, the barrel turned towards the man he fired at.
        /// </summary>
        void EstimateMuzzle(int shooter, int target, float scale, out Vector3 muzzle, out Vector3 barrel)
        {
            var w = Host.Local.World;
            Vector3 at = Host.Presenter != null && shooter >= 0 ? (Vector3)Host.Presenter.Drawn(shooter) : (Vector3)w.Position[Mathf.Max(0, shooter)];
            Vector3 aim = target >= 0 && target < w.HighWater ? (Vector3)w.Position[target] - at : Vector3.forward;
            aim.y = 0f; barrel = aim.sqrMagnitude > 1e-4f ? aim.normalized : Vector3.forward;
            bool vehicle = shooter >= 0 && shooter < w.HighWater && (w.Flags[shooter] & (uint)UnitFlags.Vehicle) != 0;
            var anim = Host.Animation;
            var stance = shooter >= 0 && shooter < w.HighWater ? (Stance)(anim != null && Host.UseAnimationController ? anim.State[shooter].Stance : w.StanceOf[shooter]) : Stance.Standing;
            float height = vehicle ? 1.6f : stance == Stance.Prone || stance == Stance.Pinned ? 0.3f : stance == Stance.Crouch ? 1.0f : 1.4f;
            muzzle = new Vector3(at.x, RenderGround.Sample(Host.Local.Map, at.x, at.z) + height * scale, at.z) + barrel * ((vehicle ? 2.4f : 0.75f) * scale);
            if (vehicle && SceneHooks.VehicleGunPort != null)
            {
                var port = SceneHooks.VehicleGunPort(shooter);   // the Maw's mouth, beside the Tusk's gun
                if (port.w > 0.5f) muzzle = new Vector3(port.x, port.y, port.z) + barrel * 0.3f;
            }
        }

        /// <summary>A man's chest when his figure has no sockets to say, from his (drawn) stance.</summary>
        Vector3 EstimateChest(int slot, float scale)
        {
            var w = Host.Local.World;
            Vector3 at = Host.Presenter != null ? (Vector3)Host.Presenter.Drawn(slot) : (Vector3)w.Position[slot];
            bool vehicle = (w.Flags[slot] & (uint)UnitFlags.Vehicle) != 0;
            var anim = Host.Animation;
            var stance = (Stance)(anim != null && Host.UseAnimationController && !vehicle ? anim.State[slot].Stance : w.StanceOf[slot]);
            float chest = vehicle ? 1.4f : stance == Stance.Prone || stance == Stance.Pinned ? 0.3f : stance == Stance.Crouch ? 0.8f : 1.2f;
            return new Vector3(at.x, RenderGround.Sample(Host.Local.Map, at.x, at.z) + chest * scale, at.z);
        }
    }
}
