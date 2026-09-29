// Phase: deaths (2026-09-28, implemented) — part of TankRenderer: a machine's death made absurd (owner, 2026-09-28:
// slapstick; fx.deathAbsurd, DeathGags.Intensity, 0 = today's death exactly: nothing here runs). On top of Wreckify:
//  - the turret (or the cupola) leaps straight up at 17-21 m/s, turning end over end about the hull's side axis in
//    whole flips, and comes down with two bounces within 1.5 hull lengths (a cook-off's throw is taken over);
//  - up to four of a machine's road wheels roll away 8-14 m along its length, spreading out, then topple flat;
//  - a track comes off and pays out flat on the ground beside the hull, its links running as it goes;
//  - the Skimmer's fan leaves astern like a frisbee: it lies over flat, spinning, and glides 18-27 m before it lands;
//  - the Salvo's last rockets fizz off out of the rack (TankRenderer.Fizzers): harmless, looping, popping in the air;
//  - the hull hops a metre and comes down with a bump; a hover machine's cushion goes and it drops onto its skirt; a
//    walker holds its death pose a beat, then belly-flops: a pop, its legs splayed flat out, down on its belly.
// The flight is VehicleGags' (pure, tested); the dice are DebrisRng's, seeded by where it died, never UnityEngine.Random.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public sealed partial class TankRenderer
    {
        sealed partial class View
        {
            /// <summary>The body's drop (VehicleGags.Drop): when it leaves, its launch speed, the heights it leaves from and
            /// lands at, and how much of it Heave carries now (others may move Heave meanwhile: a wreck's stages).</summary>
            public bool Dropping, DropLanded; public float DropAt, DropUp, DropFrom, DropTo, Dropped;
            /// <summary>A walker's belly-flop: its legs splay out (0 as they stood, 1 flat out, PartLocal reads it), its
            /// tilt as it died, which the flop lays flat, and its belly's height in its own frame.</summary>
            public bool Flops; public float Splay, DropPitch, DropRoll, Belly;
        }

        sealed partial class Debris
        {
            /// <summary>The share of its fall speed it keeps on each of its next Bounces landings (then FlyDebris' 0.25).</summary>
            public float Bounce = 0.25f; public int Bounces;
            /// <summary>Seconds a wheel still rolls on its rim before it topples (0: it flies as any piece), and seconds
            /// left of its topple onto its face (critic round 4: left to FlyDebris, wheels came to rest on edge).</summary>
            public float Roll, Topple;
            /// <summary>Seconds a fan may still glide (0: it flies as any piece), and the way its path bends (rad/s).</summary>
            public float Glide, Curve;
            /// <summary>A track paying out: seconds since it began (-1: not), from and to, as it stood and as it lies.</summary>
            public float Spool = -1f; public Vector3 SpoolFrom, SpoolTo; public Quaternion SpoolRot0, SpoolRot1;
        }

        /// <summary>How a cook-off pop lights its wreck: today (intensity 0) a full flash, which washes the whole hull and
        /// every piece thrown off it pale cream for a fifth of a second a pop, so through a cook-off the wreck read as a
        /// pale blob in four stills of ten (critic round 9); above 0 a glint, the pop's own flipbook flash doing the rest.</summary>
        static float PopFlash(float flash) => DeathGags.Intensity > 0f ? Mathf.Max(flash, VehicleGags.PopGlint) : 1f;

        /// <summary>The absurd death, once, as the machine becomes a wreck (Wreckify, fx.deathAbsurd above 0).</summary>
        void DeathGag(View v, float now)
        {
            float a = DeathGags.Intensity;
            var rng = new DebrisRng(v.Pos, 0xDEADu + (uint)Mathf.Max(0, v.Slot));
            var lod = v.Model.Lods[0];
            var parts = lod.Parts;
            Vector3 fwd = new Vector3(Mathf.Sin(v.Yaw), 0f, Mathf.Cos(v.Yaw)), right = new Vector3(fwd.z, 0f, -fwd.x);
            float hullLength = v.Model.HalfLength * 2f;

            // the turret: straight up, end over end, two bounces
            // (a rack of rockets stays on its truck: its last rockets fizz off it, TankRenderer.Fizzers; critic round 1
            // found the rack perched on a wall and the fizzers gone with it)
            int top = -1;
            for (int i = 1; i < parts.Count && top < 0; i++) if (parts[i].Role == TankPartRole.Turret) top = i;
            bool turret = top >= 0;
            for (int i = 1; i < parts.Count && top < 0; i++) if (parts[i].Role == TankPartRole.Cupola) top = i;
            if (v.Model.IsRack)
            {
                // even a cook-off leaves the rack on its truck now (critic round 2): its rockets fizz off it instead
                if (top >= 0 && v.Off[top])
                    for (int k = v.Pieces.Count - 1; k >= 0; k--)
                        if (v.Pieces[k].Part == top) { debris.Remove(v.Pieces[k]); v.Pieces.RemoveAt(k); v.Off[top] = false; }
                top = -1;
            }
            if (top >= 0)
            {
                Debris d = null;
                if (v.Off[top]) { foreach (var p in v.Pieces) if (p.Part == top) d = p; }   // the cook-off threw it: take it over
                else d = Detach(v, top, v.World[top]);
                if (d != null)
                {
                    float turn = rng.Range(0f, 2f * Mathf.PI);
                    var leap = VehicleGags.TurretLeap(a, hullLength, ClearWay(v.Pos, turn, hullLength), right, rng.Next(), rng.Next(), rng.Next());
                    d.Vel = leap.Vel; d.Spin = leap.Spin; d.Resting = false;
                    d.Bounce = VehicleGags.TurretBounce; d.Bounces = VehicleGags.TurretBounces;
                    d.Burn = Mathf.Max(d.Burn, v.Burn);
                    GunOff(v, d, right);
                }
            }

            // the wheels: a few roll away along its length, spreading out
            int rollers = 0;
            for (int i = 1; i < parts.Count && rollers < VehicleGags.MaxRollers; i++)
            {
                if (parts[i].Role != TankPartRole.Wheel || v.Off[i]) continue;
                if (rng.Next() < 0.5f && rollers >= 2) continue;   // two at least, where it has them
                var d = Detach(v, i, v.World[i]);
                Vector3 at = (Vector3)v.World[i].GetColumn(3) - v.Pos;
                float side = Vector3.Dot(at, right) >= 0f ? 1f : -1f, ahead = Vector3.Dot(at, fwd) >= 0f ? 1f : -1f;
                Vector3 dir = (fwd * ahead + right * (side * rng.Range(0.2f, 0.5f))).normalized;
                float distance = VehicleGags.RollDistance(rng.Next(), hullLength);   // a Maw's roll further
                d.Vel = dir * VehicleGags.RollSpeed(distance);
                d.Roll = VehicleGags.RollSeconds(distance) + 0.5f;
                rollers++;
            }

            // what came after the first film (its own dice, so the turret and the wheels above go as they were filmed)
            var more = new DebrisRng(v.Pos, 0xF1A7u + (uint)Mathf.Max(0, v.Slot));
            if (!turret) Sponsons(v, ref more, a, right);   // a machine with no turret (the Maw) throws its gun sponsons
            PayOut(v, ref more, a, fwd, right);
            FanOff(v, ref more, a, fwd);
            Fizzers(v, top, ref more, a, now);   // TankRenderer.Fizzers

            // the body: a hop; a hover machine drops onto its skirt; a walker holds a beat, then belly-flops
            v.Dropping = true; v.DropLanded = false; v.Dropped = 0f;
            v.DropFrom = v.DropTo = v.Heave.Value;
            v.DropUp = VehicleGags.HopSpeed(a);
            v.DropAt = now;
            if (v.Hover) v.DropTo = Mathf.Min(v.DropFrom, Ground(v.Pos.x, v.Pos.z));
            if (v.Model.LegCount > 0)
            {
                v.Flops = true;
                v.Belly = Belly(v);
                // a little into the snow: its claws hang lower than its belly (critic round 5: it sat up on them)
                v.DropTo = Mathf.Min(v.DropFrom, Ground(v.Pos.x, v.Pos.z) - v.Belly - VehicleGags.BellySink * v.Model.Height);
                v.DropUp = VehicleGags.FlopUp * Mathf.Sqrt(Mathf.Min(a, VehicleGags.HopCap));
                v.DropAt = now + VehicleGags.WalkerFreeze;
                v.DropPitch = v.Pitch.Value; v.DropRoll = v.Roll.Value;
            }
        }

        /// <summary>Which way a leaping turret drifts: of eight bearings from the dice's, the one whose landing spot (a hull
        /// length out) is furthest from any prop or blocked ground (a ruin, a bunker), so it comes down on open ground, not
        /// on a wall (critic rounds 3 and 4: the ruined wall it kept landing on is blocked ground, not a prop).</summary>
        Vector3 ClearWay(Vector3 at, float turn, float reach)
        {
            Vector3 best = new Vector3(Mathf.Sin(turn), 0f, Mathf.Cos(turn));
            var map = Host != null && Host.Local != null ? Host.Local.Map : null;
            if (map == null || !map.Props.IsCreated) return best;
            float bestGap = -1f;
            for (int k = 0; k < 8; k++)
            {
                float b = turn + k * Mathf.PI * 0.25f;
                var dir = new Vector3(Mathf.Sin(b), 0f, Mathf.Cos(b));
                Vector3 spot = at + dir * reach;
                float gap = 30f;
                for (int i = 0; i < map.Props.Length; i++)
                {
                    var q = map.Props[i];
                    float dx = q.Pos.x - spot.x, dz = q.Pos.z - spot.z;
                    if (dx * dx + dz * dz > 900f) continue;
                    gap = Mathf.Min(gap, Mathf.Sqrt(dx * dx + dz * dz) - 1.5f * (q.Scale > 0f ? q.Scale : 1f));
                }
                // the scenery the map does not hold (critic round 7: a stone ruin), along the stretch it lands and bounces on
                if (SceneHooks.Standing != null)
                    for (float along = 0.9f; along <= 1.5f; along += 0.3f)
                    {
                        Vector3 on = at + dir * (reach * along);
                        gap = Mathf.Min(gap, SceneHooks.Standing(on.x, on.z, 30f) - 1f);
                    }
                if (map.StaticCover.IsCreated)
                    for (int i = 0; i < map.StaticCover.Length; i++)
                    {
                        var cv = map.StaticCover[i];   // a ruin, a wall: what men take cover behind
                        if (cv.OwnerSlot >= 0) continue;
                        float dx = cv.Center.x - spot.x, dz = cv.Center.z - spot.z;
                        if (dx * dx + dz * dz > 900f) continue;
                        gap = Mathf.Min(gap, Mathf.Sqrt(dx * dx + dz * dz) - 0.5f * cv.Radius - 1f);
                    }
                for (int ring = 0; ring <= 2; ring++)
                    for (int n = 0; n < (ring == 0 ? 1 : 8); n++)
                    {
                        float c = n * Mathf.PI * 0.25f, r = ring * 1.5f;
                        var layer = map.LayerAt(new Unity.Mathematics.float3(spot.x + Mathf.Sin(c) * r, 0f, spot.z + Mathf.Cos(c) * r));
                        if ((layer & (TW.Sim.Terrain.NavLayer.Blocked | TW.Sim.Terrain.NavLayer.Bunker)) != 0) gap = Mathf.Min(gap, r - 1.5f);
                    }
                if (gap > bestGap + 0.5f) { bestGap = gap; best = dir; }   // the dice's own bearing wins a near tie
            }
            return best;
        }

        /// <summary>Once a frame, after the wrecks smoulder: the bodies still dropping, a walker's legs splaying, the
        /// fizzers in the air.</summary>
        void GagsFrame(float dt, float now)
        {
            foreach (var v in wrecks)
            {
                if (!v.Dropping) continue;
                float t = now - v.DropAt;
                if (t < 0f) continue;
                float h = VehicleGags.Drop(v.DropFrom, v.DropTo, v.DropUp, t) - v.DropFrom;
                v.Heave.Value += h - v.Dropped; v.Dropped = h;
                if (v.Flops)
                {
                    v.Splay = VehicleGags.Splay(t);
                    v.Pitch.Value = v.DropPitch * (1f - v.Splay); v.Roll.Value = v.DropRoll * (1f - v.Splay);
                }
                if (!v.DropLanded && t >= VehicleGags.DropFirst(v.DropFrom, v.DropTo, v.DropUp)) { v.DropLanded = true; Landing(v); }
                if (t >= VehicleGags.DropSeconds(v.DropFrom, v.DropTo, v.DropUp))
                {
                    v.Heave.Value += (v.DropTo - v.DropFrom) - v.Dropped; v.Dropped = v.DropTo - v.DropFrom;
                    v.Dropping = false;
                }
            }
            FizzersFrame(dt, now);
        }

        /// <summary>The body comes down: dust, a bump, and a walker's belly throws plates.</summary>
        void Landing(View v)
        {
            float size = v.Flops ? 2.2f : 1.4f;
            if (books != null && books.Ready)
            {
                books.Add(FlipbookFx.Book.Puff, v.Pos + Vector3.up * 0.3f, v.Model.HalfLength * size, 1.1f, FlipbookFx.Kind.Upright, velocity: Vector3.up * 0.4f, grow: 0.8f, alpha: 0.6f);
                if (v.Flops) books.Add(FlipbookFx.Book.Wings, new Vector3(v.Pos.x, Ground(v.Pos.x, v.Pos.z), v.Pos.z), v.Model.HalfLength * 3f, 1.0f, FlipbookFx.Kind.Upright | FlipbookFx.Kind.Anchored, grow: 0.5f, alpha: 0.55f);
                // a ring of dust pushed out from under its rim (critic round 9: the one puff read as haze, not a ring), grey
                // smoke cards: white puffs vanished on snow (round 11 film); a hull landing its hop too (round 13: no thump)
                if (v.Flops || !v.Hover)
                    for (int k = 0; k < VehicleGags.FlopRing; k++)
                    {
                        float b = v.Yaw + k * 2f * Mathf.PI / VehicleGags.FlopRing;
                        var out1 = new Vector3(Mathf.Sin(b), 0f, Mathf.Cos(b));
                        Vector3 at = v.Pos + out1 * (v.Model.HalfLength * 1.35f);   // outside the rim: over the body the cards made the hull look glassy (critic round 15)
                        at.y = Ground(at.x, at.z) + 0.25f;
                        books.Add(FlipbookFx.Book.Smoke, at, v.Model.HalfLength * 0.55f, 1.0f, velocity: out1 * 3.5f + Vector3.up * 0.2f, grow: 1.1f, alpha: 0.45f);
                    }
            }
            if (v.Flops) Scrap(v.Pos + Vector3.up * 0.5f, 5, 5f, 0.3f, v.Burn * 0.5f, 20f, Vector3.zero, (uint)v.Slot);
            CameraShake.Add(v.Pos, v.Flops ? 6f : 3f);
        }

        /// <summary>A walker's belly: the lowest corner of its body's box, in the frame its heave is the height of.</summary>
        static float Belly(View v)
        {
            var body = v.Model.Lods[0].Parts[0];
            if (body.Mesh == null) return 0f;
            var b = body.Mesh.bounds;
            float low = float.MaxValue;
            for (int k = 0; k < 8; k++)
            {
                Vector3 c = b.center + Vector3.Scale(b.extents, new Vector3((k & 1) != 0 ? 1f : -1f, (k & 2) != 0 ? 1f : -1f, (k & 4) != 0 ? 1f : -1f));
                low = Mathf.Min(low, (body.Local + body.LocalRot * c).y);
            }
            return low;
        }

        /// <summary>A leg of a walker that belly-flopped (View.Splay above 0; PartLocal asks): the hip turned so the leg
        /// lies flat out from the body with its toe on the ground, what hangs below the hip as it was modelled, and while
        /// it splays, the way it stood blended into that.</summary>
        Matrix4x4 Splayed(View v, TankModel.Part p, int index)
        {
            var target = Matrix4x4.TRS(p.Local, p.LocalRot, Vector3.one);
            var legs = v.Model.Lods[0].Legs;
            if ((p.Role == TankPartRole.Leg || p.Role == TankPartRole.Thigh) && legs != null && p.Leg >= 0 && p.Leg < legs.Length && legs[p.Leg] != null)
            {
                var rig = legs[p.Leg];
                float down = Mathf.Clamp(Mathf.Max(0f, rig.Hip.y - v.Belly) / Mathf.Max(0.1f, rig.Reach), 0f, VehicleGags.SplayDown);
                Vector3 flat = new Vector3(rig.Outward.x, 0f, rig.Outward.z);
                flat = flat.sqrMagnitude > 1e-6f ? flat.normalized : Vector3.right;
                Vector3 want = flat * Mathf.Sqrt(1f - down * down) + Vector3.down * down;
                Quaternion turn = Quaternion.FromToRotation(rig.Rest.normalized, want);   // in the body's frame
                turn = Quaternion.Inverse(rig.ParentRot) * turn * rig.ParentRot;          // in the hip's parent's
                target = Matrix4x4.TRS(p.Local, turn * p.LocalRot, Vector3.one);
            }
            if (v.Splay >= 1f || v.LegSolved == null || index < 0 || index >= v.LegSolved.Length || !v.LegSolved[index]) return target;
            var from = v.LegLocal[index];
            return Matrix4x4.TRS(Vector3.Lerp(from.GetColumn(3), target.GetColumn(3), v.Splay), Quaternion.Slerp(from.rotation, target.rotation, v.Splay), Vector3.one);
        }

        /// <summary>A track comes off and pays out flat beside the hull; one the sim already threw (its module broke in the
        /// killing shot, as it usually does) is taken over, as the turret's leap takes over a cook-off's throw (critic round
        /// 3: the Maw's two were always thrown first, so none paid out). At ludicrous both go.</summary>
        void PayOut(View v, ref DebrisRng rng, float a, Vector3 fwd, Vector3 right)
        {
            var lod = v.Model.Lods[0];
            int l = lod.Find("Track_L"), r = lod.Find("Track_R");
            if (l < 0 && r < 0) return;
            bool left = rng.Next() < 0.5f;
            // the side with open ground, where the scenery says (critic round 9: the Maw's belt ran into a ruin); the dice's
            // side when both are clear
            if (SceneHooks.Standing != null && l >= 0 && r >= 0)
            {
                float reach = v.Model.HalfGauge + 3f;
                Vector3 back = -fwd * 2f;
                float openL = SceneHooks.Standing(v.Pos.x - right.x * reach + back.x, v.Pos.z - right.z * reach + back.z, 8f);
                float openR = SceneHooks.Standing(v.Pos.x + right.x * reach + back.x, v.Pos.z + right.z * reach + back.z, 8f);
                if (Mathf.Abs(openL - openR) > 1f && Mathf.Min(openL, openR) < 6f) left = openL > openR;
            }
            int first = left ? l : r, second = left ? r : l;
            if (first < 0) { first = second; second = -1; }
            if (first < 0) return;
            StartPayOut(v, first, fwd, right);
            if (a >= 1.5f && second >= 0) StartPayOut(v, second, fwd, right);
        }

        void StartPayOut(View v, int i, Vector3 fwd, Vector3 right)
        {
            var p = v.Model.Lods[0].Parts[i];
            Debris d = null;
            if (v.Off[i]) { foreach (var q in v.Pieces) if (q.Part == i) d = q; }
            else d = Detach(v, i, v.World[i]);
            if (d == null) return;
            d.Thrown = false; d.Resting = false;   // no longer a track to be put back: it is paying out
            // a wheel that already rolled off is its own piece: it does not ride the belt as well
            var own = new System.Collections.Generic.List<int>();
            foreach (var c in d.Local.Keys) if (v.Off[c]) own.Add(c);
            foreach (int c in own) d.Local.Remove(c);
            float side = p.Side < 0 ? -1f : 1f;
            Vector3 from = d.World.GetColumn(3);
            // where it lies is measured from the meshes, not the pivots: the Maw's track pivots sit on its centre line,
            // and a belt laid out from them lay under its own hull (critic round 2 saw no track)
            var bounds = p.Mesh != null ? p.Mesh.bounds : new Bounds(Vector3.zero, Vector3.one);
            var body = v.Model.Lods[0].Parts[0].Mesh;
            float hullHalf = body != null ? body.bounds.extents.x : v.Model.HalfGauge;
            Quaternion rot1 = Quaternion.AngleAxis(v.Yaw * Mathf.Rad2Deg, Vector3.up) * p.LocalRot;
            Vector3 centre = new Vector3(v.Pos.x, 0f, v.Pos.z)
                             + right * (side * (Mathf.Max(hullHalf, v.Model.HalfGauge) + bounds.extents.x * VehicleGags.UnspoolOut))
                             - fwd * (bounds.extents.z * (VehicleGags.UnspoolStretch - 1f) * 0.5f);   // half its new length behind: beside the hull, not off its tail
            Vector3 scaled = new Vector3(bounds.center.x, bounds.center.y * VehicleGags.UnspoolFlat, bounds.center.z * VehicleGags.UnspoolStretch);
            Vector3 to = centre - rot1 * scaled;
            to.y = Ground(centre.x, centre.z) - bounds.min.y * VehicleGags.UnspoolFlat + 0.03f;
            d.SpoolFrom = from; d.SpoolTo = to;
            d.SpoolRot0 = d.World.rotation;
            d.SpoolRot1 = rot1;
            d.Spool = 0f; d.Vel = Vector3.zero; d.Spin = Vector3.zero;
        }

        /// <summary>The Skimmer's fan, off astern like a frisbee: its ring (FanRing) with it, which is what reads from the
        /// camera (critic round 1: the blades alone left and the ring stayed, so nothing seemed to go).</summary>
        void FanOff(View v, ref DebrisRng rng, float a, Vector3 fwd)
        {
            var parts = v.Model.Lods[0].Parts;
            int fan = -1, ring = v.Model.Lods[0].Find("FanRing");
            for (int i = 1; i < parts.Count && fan < 0; i++) if (parts[i].Role == TankPartRole.Fan && !v.Off[i]) fan = i;
            if (fan < 0) return;
            var glide = VehicleGags.FanThrow(a, -fwd, rng.Next(), rng.Next(), rng.Next());
            bool fanUnderRing = false;
            for (int k = fan; k >= 0; k = parts[k].Parent) if (k == ring) fanUnderRing = true;
            if (ring >= 0 && !v.Off[ring])
            {
                var r = Detach(v, ring, v.World[ring]);
                r.Vel = glide.Vel; r.Curve = glide.Curve; r.Glide = VehicleGags.FanGlideCap; r.Burn = Mathf.Max(r.Burn, v.Burn * 0.5f);
            }
            if (fanUnderRing) return;   // it rides its ring
            var d = Detach(v, fan, v.World[fan]);
            d.Vel = glide.Vel; d.Curve = glide.Curve; d.Glide = VehicleGags.FanGlideCap;
            d.Burn = Mathf.Max(d.Burn, v.Burn * 0.5f);
        }

        /// <summary>The leaping turret's gun snaps off and cartwheels away on its own (critic round 15: a turret going up
        /// gun first, the barrel pointing down at the hull, read as a turret on a stalk). Its own dice, so the leap filmed
        /// before goes as it went.</summary>
        void GunOff(View v, Debris turret, Vector3 right)
        {
            var parts = v.Model.Lods[0].Parts;
            int gun = -1;
            foreach (var c in turret.Local.Keys) if (parts[c].Role == TankPartRole.Gun && (gun < 0 || c < gun)) gun = c;
            if (gun < 0) return;
            var rng = new DebrisRng(v.Pos, 0x6A77u + (uint)Mathf.Max(0, v.Slot));
            var d = Detach(v, gun, v.World[gun]);
            // what hung off the gun goes with it, not with the turret
            var gone = new System.Collections.Generic.List<int>();
            foreach (var c in turret.Local.Keys)
                for (int k = c; k >= 0; k = parts[k].Parent) if (k == gun) { gone.Add(c); break; }
            foreach (int c in gone) turret.Local.Remove(c);
            float side = rng.Next() < 0.5f ? -1f : 1f;
            d.Vel = turret.Vel * VehicleGags.GunKeeps + right * (side * rng.Range(VehicleGags.GunOutMin, VehicleGags.GunOutMax));
            d.Spin = rng.OnSphere() * rng.Range(8f, 12f);
            d.Bounce = VehicleGags.TurretBounce; d.Bounces = 1;
            d.Burn = Mathf.Max(d.Burn, turret.Burn);
        }

        /// <summary>A machine with no turret throws its gun sponsons off its sides, up and out, tumbling.</summary>
        void Sponsons(View v, ref DebrisRng rng, float a, Vector3 right)
        {
            var parts = v.Model.Lods[0].Parts;
            float k = Mathf.Sqrt(Mathf.Min(a, 2f));
            for (int i = 1; i < parts.Count; i++)
            {
                if (parts[i].Role != TankPartRole.Sponson || v.Off[i]) continue;
                var d = Detach(v, i, v.World[i]);
                float side = parts[i].Side < 0 ? -1f : 1f;
                d.Vel = (right * (side * rng.Range(4f, 6.5f)) + Vector3.up * rng.Range(8f, 11f)) * k;
                d.Spin = rng.OnSphere() * rng.Range(3f, 6f);
                d.Bounce = VehicleGags.TurretBounce; d.Bounces = 1;
                d.Burn = Mathf.Max(d.Burn, v.Burn);
            }
        }

        /// <summary>A piece on a gag's own path this frame (FlyDebris asks first): true if it was moved here.</summary>
        bool FlyGag(Debris d, float dt)
        {
            if (d.Roll > 0f) { RollWheel(d, dt); return true; }
            if (d.Topple > 0f) { ToppleWheel(d, dt); return true; }
            if (d.Glide > 0f) { GlideFan(d, dt); return true; }
            if (d.Spool >= 0f) { PayingOut(d, dt); return true; }
            return false;
        }

        /// <summary>What a landing piece keeps of its fall speed (FlyDebris): its own share for its next few bounces.</summary>
        static float BounceOf(Debris d)
        {
            if (d.Bounces <= 0) return 0.25f;
            d.Bounces--;
            return d.Bounce;
        }

        /// <summary>A wheel on its rim: it rolls along, slowing, turning about its axle; when it runs out it topples over.</summary>
        void RollWheel(Debris d, float dt)
        {
            var model = d.Owner.Model;
            var p = model.Lods[0].Parts[d.Part];
            Vector3 pos = d.World.GetColumn(3);
            Quaternion rot = d.World.rotation;
            Vector3 flat = new Vector3(d.Vel.x, 0f, d.Vel.z);
            float speed = flat.magnitude;
            d.Roll -= dt;
            if (speed < 0.5f || d.Roll <= 0f)
            {
                // out of roll: over onto its face, a quarter turn about the way it was going (ToppleWheel)
                Vector3 dir = speed > 1e-3f ? flat / speed : Vector3.forward;
                d.Roll = 0f;
                d.Topple = VehicleGags.ToppleSeconds;
                d.Vel = dir * (speed * 0.5f);
                d.Spin = dir * (0.5f * Mathf.PI / VehicleGags.ToppleSeconds);
                return;
            }
            float slowed = Mathf.Max(0.01f, speed - VehicleGags.RollDecel * dt);
            flat *= slowed / speed;
            d.Vel = flat;
            pos += flat * dt;
            float radius = Mathf.Max(0.15f, model.WheelRadius);
            rot = Quaternion.AngleAxis(slowed / radius * Mathf.Rad2Deg * dt, Vector3.Cross(Vector3.up, flat / slowed)) * rot;
            Vector3 centre = pos + rot * p.Center;
            pos.y += Ground(centre.x, centre.z) + radius - centre.y;   // on its rim
            d.World = Matrix4x4.TRS(pos, rot, Vector3.one);
        }

        /// <summary>A wheel toppling onto its face: a quarter turn about the way it rolled, its rim kept on the ground as
        /// it goes over; then it lies there.</summary>
        void ToppleWheel(Debris d, float dt)
        {
            var model = d.Owner.Model;
            var p = model.Lods[0].Parts[d.Part];
            Vector3 pos = d.World.GetColumn(3);
            Quaternion rot = d.World.rotation;
            float step = Mathf.Min(dt, d.Topple);
            d.Topple -= step;
            if (d.Spin.sqrMagnitude > 1e-6f) rot = Quaternion.AngleAxis(d.Spin.magnitude * Mathf.Rad2Deg * step, d.Spin.normalized) * rot;
            pos += d.Vel * step;
            float radius = Mathf.Max(0.15f, model.WheelRadius);
            Vector3 axle = rot * Vector3.right;   // a wheel turns about its own x (PartLocal)
            float up = Mathf.Abs(axle.y);
            float low = radius * Mathf.Sqrt(Mathf.Max(0f, 1f - up * up)) + radius * 0.2f * up;   // its lowest point below its middle
            Vector3 centre = pos + rot * p.Center;
            pos.y += Ground(centre.x, centre.z) + low - centre.y;
            d.World = Matrix4x4.TRS(pos, rot, Vector3.one);
            if (d.Topple > 0f) return;
            d.Topple = 0f; d.Resting = true; d.Vel = Vector3.zero; d.Spin = Vector3.zero;
            if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, centre + Vector3.up * 0.1f, radius * 2.4f, 0.8f, velocity: Vector3.up * 0.3f, grow: 0.8f, alpha: 0.5f);
        }

        /// <summary>The fan in the air: it lies over flat (its hub's axis, local +Z, to the vertical), spins about that
        /// axis and glides (VehicleGags.GlideStep); where it meets the ground, or its glide runs out, FlyDebris takes it.</summary>
        void GlideFan(Debris d, float dt)
        {
            var p = d.Owner.Model.Lods[0].Parts[d.Part];
            Vector3 pos = d.World.GetColumn(3);
            Quaternion rot = d.World.rotation;
            VehicleGags.GlideStep(ref pos, ref d.Vel, d.Curve, dt);
            Vector3 hub = rot * Vector3.forward;
            Vector3 flatUp = Vector3.Dot(hub, Vector3.up) >= 0f ? Vector3.up : Vector3.down;
            Vector3 lean = Vector3.RotateTowards(hub, flatUp, Mathf.PI * 0.5f * dt / VehicleGags.FanTilt, 0f);
            rot = Quaternion.FromToRotation(hub, lean) * rot;
            rot = Quaternion.AngleAxis(VehicleGags.FanSpin * Mathf.Rad2Deg * dt, lean) * rot;
            d.Glide -= dt;
            Vector3 centre = pos + rot * p.Center;
            // it lands on what is drawn there: a frozen pond's ice, not the bed under it (critic round 6: it glided out
            // over the pond, FlyDebris set it down on the bed and it vanished under the ice)
            float ground = Ground(centre.x, centre.z);
            var map = Host != null && Host.Local != null ? Host.Local.Map : null;
            if (map != null && SceneTints.Now.Frozen && map.WaterLevel > ground) ground = map.WaterLevel;
            float half = Mathf.Min(p.Radius, 1.2f) * 0.2f;
            if (centre.y - half < ground || d.Glide <= 0f)
            {
                // down: flat on its face where it came down, and it lies there (it glided in flat; no bounce to lose it)
                d.Glide = 0f; d.Resting = true; d.Vel = Vector3.zero; d.Spin = Vector3.zero;
                pos.y += ground + half - centre.y;
                rot = Quaternion.FromToRotation(rot * Vector3.forward, Vector3.Dot(rot * Vector3.forward, Vector3.up) >= 0f ? Vector3.up : Vector3.down) * rot;
                if (books != null && books.Ready) books.Add(FlipbookFx.Book.Puff, new Vector3(centre.x, ground + 0.2f, centre.z), 2.2f, 0.9f, velocity: Vector3.up * 0.4f, grow: 0.9f, alpha: 0.6f);
            }
            d.World = Matrix4x4.TRS(pos, rot, Vector3.one);
        }

        /// <summary>A track paying out (VehicleGags.Unspool): off the hull, a little up, out and down onto the ground,
        /// lying flatter and longer as it goes, its links running; at the end it lies there.</summary>
        void PayingOut(Debris d, float dt)
        {
            var v = d.Owner; var p = v.Model.Lods[0].Parts[d.Part];
            d.Spool += dt;
            float k = VehicleGags.Unspool(d.Spool);
            Vector3 pos = Vector3.Lerp(d.SpoolFrom, d.SpoolTo, k) + Vector3.up * (Mathf.Sin(k * Mathf.PI) * 0.4f);
            Quaternion rot = Quaternion.Slerp(d.SpoolRot0, d.SpoolRot1, k);
            var scale = new Vector3(1f, Mathf.Lerp(1f, VehicleGags.UnspoolFlat, k), Mathf.Lerp(1f, VehicleGags.UnspoolStretch, k));
            float links = VehicleGags.UnspoolLinks * dt * (1f - k);
            if (p.Side < 0) v.TreadL = Mathf.Repeat(v.TreadL + links, 1000f); else v.TreadR = Mathf.Repeat(v.TreadR + links, 1000f);
            d.World = Matrix4x4.TRS(pos, rot, scale);
            if (d.Spool < VehicleGags.UnspoolSeconds) return;
            d.Spool = -1f; d.Resting = true; d.Vel = Vector3.zero; d.Spin = Vector3.zero;
            if (books != null && books.Ready)
            {
                Vector3 along = rot * Vector3.forward;
                float half = p.Mesh != null ? p.Mesh.bounds.extents.z * VehicleGags.UnspoolStretch : 2f;
                for (int k2 = -1; k2 <= 1; k2++)
                    books.Add(FlipbookFx.Book.Puff, pos + along * (half * 0.6f * k2) + Vector3.up * 0.2f, 1.3f, 0.9f, velocity: Vector3.up * 0.4f, grow: 0.8f, alpha: 0.5f, delay: 0.05f * (k2 + 1));
            }
        }
    }
}
