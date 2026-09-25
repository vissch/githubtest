// Phase: A5b / C4 (implemented) — depends on: TankModel.LegRig (built from Tools/crabsplit.py's parts), a ground
// height function (TankRenderer hands it RenderGround.Sample, so craters and deformation are included)
// A walker's legs are not animated. They are SOLVED, and the difference is the whole point.
//
// The old way swung each leg on a sine wave about its shoulder. That reads as a toy the moment you look at the feet:
// they slide along the ground the entire time they are supposed to be carrying the machine, they pass through a
// parapet instead of over it, and the body floats at a height the legs have no say in.
//
// Here each foot is planted on an actual point of ground and STAYS on it while the body walks over it. When the body
// has outrun a foot far enough, that leg lifts, arcs over whatever lies between, and plants again ahead. Nothing
// skates, because a planted foot is a fixed world position, not a phase. Three things then come out for free:
//   - Ground. A step's target is sampled off the terrain, and its arc is raised to clear the highest thing between
//     the old foot and the new one, so a machine crossing a trench lip steps ONTO the lip.
//   - Carriage. The body's height, pitch and roll are read off the feet that are down, so the machine is tilted by
//     the ground it is actually standing on rather than by a probe under its belly.
//   - Damage. A lost leg simply stops taking part. The others step more often because the body still moves, the
//     support under one corner is gone, and the machine leans into the hole. None of that is special-cased.
using UnityEngine;
using TW.Sim.Nav;

namespace TW.Presentation.Tactical
{
    /// <summary>One walker's legs: where its feet are, what its body is doing on top of them, and the rotations that
    /// put the modelled parts there. One of these per drawn walker, kept between frames — the planted feet ARE the
    /// state, so it cannot be recomputed from scratch without the machine skating.</summary>
    public sealed class WalkerGait
    {
        /// <summary>How far a foot may fall behind where it would like to stand, as a share of the leg's reach,
        /// before that leg takes a step.</summary>
        public const float TriggerShare = 0.32f;
        /// <summary>A foot this far behind (as a share of reach) has waited long enough: it steps whatever its
        /// neighbours are doing. Without this a leg can be held back by one neighbour after another while the body
        /// walks on, and by the time its turn comes its foot is further behind than the leg is long.</summary>
        public const float DesperateShare = 0.52f;
        /// <summary>A leg also steps once it has used up this much of its length, however little its foot has
        /// drifted.
        ///
        /// Distance travelled is the wrong thing to wait for on its own. A foot left behind stretches its leg, and
        /// because the ride height is capped by whatever the most-stretched leg can still reach, a machine whose
        /// feet are allowed to trail simply SINKS as it walks — correctly, in that the hips must come down for that
        /// leg to reach the ground at all, and wrongly, in that nothing with legs walks like that. A real one picks
        /// the foot up before it runs out of leg. Measuring what the leg has LEFT, rather than how far the foot has
        /// gone, is what keeps the body up. It sits below StepSafety so the leg always moves before the cap bites,
        /// and Frantic is just under the cap, where a leg goes whatever its neighbours are doing.</summary>
        public const float ExtendAt = 0.955f, Frantic = 0.968f;
        /// <summary>A step in the air, walking and running, in seconds. A fast machine picks its feet up quicker.
        ///
        /// These grow by the SQUARE ROOT of the machine's size, which is neither of the two obvious answers.
        /// Leaving them alone is wrong: a step is triggered by a share of the leg, so a leg two and a half times
        /// longer trails two and a half times further before it lifts and the stride grows to match, and covering
        /// two and a half times the ground in the same fifth of a second is a flick, not a step. Multiplying them
        /// by the size is wrong the other way, since large animals do not move their legs in slow motion.
        /// A swinging leg is a pendulum and a pendulum's period goes as the square root of its length, so that is
        /// what this uses: a foot that still moves faster than it used to, but visibly weighs something.
        ///
        /// It is also the safe direction. The body travels the same metres per second whatever size the machine
        /// is, so the gap between steps grew by the full 2.5 while the step itself grew by 1.58: more of the
        /// cycle is spent with the foot down than before, and more feet are down at once.</summary>
        public static readonly float SwingSlow = 0.36f * Mathf.Sqrt(VehicleSize.Walker),
                                    SwingFast = 0.12f * Mathf.Sqrt(VehicleSize.Walker);
        /// <summary>A step is planted this much of its own flight ahead of where the foot wants to be, so the foot
        /// lands where the body is going rather than where it was.</summary>
        public const float LeadShare = 0.60f;
        /// <summary>How high an ordinary step goes, as a share of reach.</summary>
        public const float ArcShare = 0.26f;

        /// <summary>The daylight left over whatever a step has to cross. This one is in metres, so it grows with
        /// the machine: 20 cm over a parapet is a careful step for a leg a metre long and a scrape for one two and
        /// a half times that. ArcShare above needs no such treatment, being a share of reach already.</summary>
        public static readonly float Clearance = 0.20f * VehicleSize.Walker;
        /// <summary>A body movement further than this in one frame is a teleport, not a walk: re-plant everything.</summary>
        public static readonly float JumpMetres = 6f * VehicleSize.Walker;

        public struct Foot
        {
            public Vector3 At;        // where it is this frame
            public Vector3 Anchor;    // the ground it is standing on (equal to At while it is down)
            public Vector3 Target;    // the ground it is stepping to
            public float Swing;       // -1 planted, otherwise 0..1 through the step
            public float Arc;         // how high this particular step has to go
            public bool Lost;         // the damage system has taken this leg off
        }

        public Foot[] Feet = System.Array.Empty<Foot>();
        /// <summary>The body, in the world: how high it rides, and how it is tilted by the feet that are down. Pitch
        /// and roll are in radians and in the same sense TankRenderer.Settle uses (nose up positive, left up
        /// positive), so HullRotation does not need to know which one produced them.</summary>
        public float Height, Pitch, Roll;
        /// <summary>0 while it walks properly, toward 1 as its legs go.</summary>
        public float Limp;
        /// <summary>A bit per leg that planted a foot this frame: the renderer kicks dust off these.</summary>
        public int Landed;
        /// <summary>0 while it is working, toward 1 as a dead one goes down. A walker that has been killed does not
        /// stand there: it settles onto the ground, and because its legs cannot fold they slide out from under it.
        /// That sliding is the point — a machine whose feet stayed put as its body sank would be doing something
        /// its own legs make impossible, and it is the one moment where a foot is SUPPOSED to move on the ground.</summary>
        public float Down;
        /// <summary>How long a killed walker takes to go down.</summary>
        public const float CollapseSeconds = 1.25f;
        /// <summary>How far down it ends up, as a share of how high its hips stood.</summary>
        public const float BellyShare = 0.62f;
        public bool Ready;

        Vector3 was;
        float stand; bool set;
        float[] homeFlat = System.Array.Empty<float>();
        Vector2[] splay = System.Array.Empty<Vector2>();
        float hips;                               // mean hip height as modelled, for the collapse
        int perSide = 1;                          // leg numbers per side, from the model
        float fitPitch, fitRoll;                  // the last plane the feet made, kept apart from what we output
        readonly Quaternion[] bone = new Quaternion[4];
        // set while a step is being chosen, so the choice does not allocate
        readonly int[] order = new int[8];

        /// <summary>Put every foot back on the ground under it. Used on the first frame and after a teleport.</summary>
        public void Plant(TankModel model, Vector3 pos, float yaw, System.Func<float, float, float> ground)
        {
            var rigs = model.Lods[0].Legs;
            if (rigs == null) return;
            if (Feet.Length != rigs.Length) Feet = new Foot[rigs.Length];
            perSide = Mathf.Max(1, model.LegsPerSide);
            Landed = 0;
            Stand(rigs);                     // works out the stance ring before any foot is placed on it
            var body = Quaternion.AngleAxis(yaw * Mathf.Rad2Deg, Vector3.up);
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null) continue;
                Vector3 home = Home(rigs[i], body, pos, i);
                home.y = ground(home.x, home.z);
                Feet[i].Anchor = Feet[i].At = Feet[i].Target = home;
                Feet[i].Swing = -1f;
            }
            was = pos;
            float under = 0f; int n = 0;
            for (int i = 0; i < rigs.Length; i++) if (rigs[i] != null) { under += Feet[i].Anchor.y; n++; }
            Height = (n > 0 ? under / n : ground(pos.x, pos.z)) + Stand(rigs);
            Pitch = Roll = 0f;
            Ready = true;
        }

        /// <summary>How the body is actually sitting: which way it faces AND how the feet have tilted it. This is
        /// the same rotation TankRenderer.HullRotation builds, and it has to be, because a hip a metre and a quarter
        /// up moves a good part of a foot when the machine leans. Working a leg's reach out against a hip placed by
        /// yaw alone quietly misplaces every hip by that much.</summary>
        public Quaternion Carriage(float yaw)
            => Quaternion.AngleAxis(yaw * Mathf.Rad2Deg, Vector3.up)
             * Quaternion.AngleAxis(-Pitch * Mathf.Rad2Deg, Vector3.right)
             * Quaternion.AngleAxis(-Roll * Mathf.Rad2Deg, Vector3.forward);

        /// <summary>Where one hip actually is in the world, with the machine sitting as it is sitting.</summary>
        public Vector3 HipAt(TankModel.LegRig rig, Vector3 pos, float yaw)
            => new Vector3(pos.x, Height, pos.z) + Carriage(yaw) * rig.Hip;

        /// <summary>Where a leg wants to stand, on the flat: out from its own hip along the way that leg sticks
        /// out, at the radius worked out in Stand.</summary>
        Vector3 Home(TankModel.LegRig rig, Quaternion body, Vector3 pos, int i)
        {
            float r = i >= 0 && i < homeFlat.Length ? homeFlat[i] : 0f;
            Vector2 s = i >= 0 && i < splay.Length ? splay[i] : Vector2.right;
            return pos + body * new Vector3(rig.Hip.x + s.x * r, 0f, rig.Hip.z + s.y * r);
        }

        /// <summary>How high the body rides over the ground its feet are on.
        ///
        /// This cannot be an average of how the legs were modelled, because they were not modelled standing on
        /// anything: crabsplit drops each machine so its LOWEST point sits at zero, which leaves the others hanging.
        /// Nor can it be simply the highest ride at which every leg still reaches, because that has a ceiling and no
        /// floor — one foot drifting wide drags the body down, a low body asks the other legs to fold, and a leg in
        /// one piece cannot fold. The body then sinks through its own feet.
        ///
        /// So both bounds are worked out. The most restricted leg sets how high it may ride; the leg that can fold
        /// the least sets how low. Between them it rides as high as it may.</summary>
        public float Stand(TankModel.LegRig[] rigs)
        {
            if (set) return stand;
            set = true;
            stand = 0f;
            if (rigs == null) return stand;

            // down until the shortest-reaching leg stands at the wanted angle — that leg decides for all of them
            float best = float.MaxValue; int n = 0;
            for (int i = 0; i < rigs.Length; i++)
            {
                var r = rigs[i];
                if (r == null) continue;
                best = Mathf.Min(best, MaxSpan(r) * StepSafety * Mathf.Sqrt(1f - Lean * Lean) - r.Hip.y);
                n++;
            }
            stand = n == 0 ? 0f : best;

            // then each foot stands in the MIDDLE of the ring of ground it can be put on at that height, so it has
            // as much room to trail behind as to reach ahead. Standing a foot at the outer edge of its ring, which
            // is the obvious thing to do, leaves the leg already at full stretch before the machine has moved.
            homeFlat = new float[rigs.Length];
            splay = new Vector2[rigs.Length];
            for (int i = 0; i < rigs.Length; i++)
            {
                var r = rigs[i];
                if (r == null) { splay[i] = Vector2.right; continue; }
                float drop = stand + r.Hip.y;
                homeFlat[i] = (Rise(MaxSpan(r) * StepSafety, drop) + Rise(MinSpan(r), drop)) * 0.5f;
                // out along the way the leg sticks out from the body's middle. The HIP says that honestly; Rest
                // does not, because a limb posed across the body points the wrong way entirely.
                var v = new Vector2(r.Hip.x, r.Hip.z);
                splay[i] = v.sqrMagnitude > 1e-4f ? v.normalized
                         : new Vector2(r.Outward.x, r.Outward.z).sqrMagnitude > 1e-4f
                           ? new Vector2(r.Outward.x, r.Outward.z).normalized : Vector2.right;
            }
            return stand;
        }

        /// <summary>One frame of walking. `vel` is the body's planar velocity, `lost` the damage system's bit per
        /// leg, and `ground` answers how high the terrain is at a point.</summary>
        public void Step(TankModel model, Vector3 pos, float yaw, Vector3 vel, float yawRate,
                         byte lost, bool dead, float dt, System.Func<float, float, float> ground)
        {
            var rigs = model.Lods[0].Legs;
            if (rigs == null || rigs.Length == 0) return;
            perSide = Mathf.Max(1, model.LegsPerSide);
            if (Feet.Length != rigs.Length) { Feet = new Foot[rigs.Length]; Ready = false; }
            if (!Ready || (pos - was).sqrMagnitude > JumpMetres * JumpMetres) { Plant(model, pos, yaw, ground); return; }
            was = pos;
            Landed = 0;
            Down = dead ? Mathf.Min(1f, Down + dt / CollapseSeconds) : 0f;

            var body = Quaternion.AngleAxis(yaw * Mathf.Rad2Deg, Vector3.up);
            float pace = new Vector2(vel.x, vel.z).magnitude;
            float swingTime = Mathf.Lerp(SwingSlow, SwingFast, Mathf.Clamp01(pace / 3f));

            int alive = 0, gone = 0;
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null) continue;
                if ((lost & (1 << i)) != 0) gone++; else alive++;
            }
            Limp = alive + gone > 0 ? (float)gone / (alive + gone) : 0f;

            // ---- carry each foot forward -------------------------------------------------
            int swinging = 0, want = 0;
            for (int i = 0; i < rigs.Length; i++)
            {
                var rig = rigs[i];
                if (rig == null) continue;
                bool off = (lost & (1 << i)) != 0;
                Feet[i].Lost = off;
                if (off) { Feet[i].Swing = -1f; continue; }

                if (Feet[i].Swing >= 0f)
                {
                    Feet[i].Swing += dt / Mathf.Max(0.05f, swingTime);
                    if (Feet[i].Swing >= 1f)
                    {
                        Feet[i].Swing = -1f;
                        Feet[i].Anchor = Feet[i].At = Feet[i].Target;
                        Landed |= 1 << i;
                    }
                    else
                    {
                        // the foot leaves and arrives along the ground and rides an arch in between
                        float t = Feet[i].Swing;
                        Vector3 flat = Vector3.Lerp(Feet[i].Anchor, Feet[i].Target, t * t * (3f - 2f * t));
                        Feet[i].At = flat + Vector3.up * (Mathf.Sin(t * Mathf.PI) * Feet[i].Arc);
                        swinging++;
                    }
                }
                if (Feet[i].Swing < 0f)
                {
                    // a shell may have dug the ground out from under a planted foot since it was put there
                    Feet[i].Anchor.y = ground(Feet[i].Anchor.x, Feet[i].Anchor.z);
                    Feet[i].At = Feet[i].Anchor;
                    if (!dead && want < order.Length) order[want++] = i;
                }
            }

            // ---- decide which of them steps ----------------------------------------------
            // A leg steps when its foot has fallen far enough behind where it wants to stand. Legs are not on a
            // timetable: at a crawl one foot moves at a time, at a run the whole side comes through together, and a
            // machine turning on the spot steps because the stance point swings round it. The only rules are that
            // not too many may be off the ground at once and that two legs beside each other may not both be up —
            // which is what makes six legs fall into the alternating tripod a crab walks with, without being told.
            if (!dead && want > 0)
            {
                int allowance = Mathf.Max(1, Mathf.Min(rigs.Length >= 6 ? 3 : 2, alive / 2));
                // worst offender first
                for (int a = 0; a < want; a++)
                    for (int b = a + 1; b < want; b++)
                        if (Urgency(rigs[order[b]], body, pos, yaw, order[b]) > Urgency(rigs[order[a]], body, pos, yaw, order[a]))
                            { int t = order[a]; order[a] = order[b]; order[b] = t; }

                for (int k = 0; k < want && swinging < allowance; k++)
                {
                    int i = order[k];
                    var rig = rigs[i];
                    float drift = Drift(rig, body, pos, i);
                    float span = MaxSpan(rig);
                    float now = Vector3.Distance(Feet[i].Anchor, HipAt(rig, pos, yaw));
                    float stretched = span > 1e-4f ? now / span : 0f;
                    // A leg must leave the ground before it runs out, not once it has. It is off the ground for a
                    // whole swing, and the body keeps walking the entire time — Kettle at 2.6 m/s travels 0.39 m
                    // while one foot is in the air — so a leg that waits until it is at full stretch has already
                    // lost by the time it lands. What matters is where this foot WILL be when it could next be
                    // put down, and that is the distance it has plus the ground the body will cover meanwhile.
                    // with a little in hand, because the swing itself is not the only delay: the leg may also be
                    // held a frame or two waiting for a neighbour to come down
                    bool runningOut = now + pace * swingTime >= span * (StepSafety - 0.05f);
                    if (drift < span * TriggerShare && stretched < ExtendAt && !runningOut) continue;
                    // a leg that has waited too long, or is nearly out of length, goes now whatever its neighbours
                    // are doing: being held back by one neighbour after another is what let the body sink
                    if (drift < span * DesperateShare && stretched < Frantic && !runningOut && Beside(i)) continue;

                    Vector3 home = Home(rig, body, pos, i);
                    // lead the step: out to where this foot will want to be by the time it lands, including the
                    // sweep a turn puts on a leg out at the end of the body
                    Vector3 hipOut = home - pos; hipOut.y = 0f;
                    Vector3 sweep = Vector3.Cross(Vector3.up, hipOut) * yawRate;
                    Vector3 target = home + (vel + sweep) * (swingTime * LeadShare);
                    target.y = ground(target.x, target.z);

                    // keep the step somewhere the leg can actually put a foot: not further out than it reaches,
                    // and not so far in that it would have to fold past what it is able to. Most of a leg's reach
                    // is spent getting down to the ground, so both bounds are taken on the flat at the height the
                    // foot will actually land.
                    Vector3 hip = pos + body * rig.Hip + Vector3.up * (Height - pos.y);
                    float restFlat = i < homeFlat.Length ? homeFlat[i] : new Vector2(rig.Rest.x, rig.Rest.z).magnitude;
                    for (int pass = 0; pass < 2; pass++)
                    {
                        Vector3 flat = target - hip; flat.y = 0f;
                        // against the HIGHEST the body is allowed to ride, not where it happens to be sitting now.
                        // The ride height moves after a foot is planted, and a foot placed for a low body is out of
                        // reach once the body comes back up — which is how a foot ended up 1.05 m from a hip whose
                        // leg is 0.90 m long. A taller hip means a longer drop and so a tighter bound, so taking
                        // the taller of the two is the safe way round.
                        float drop = Mathf.Max(hip.y - target.y, Stand(rigs) + rig.Hip.y);
                        float outer = Rise(MaxSpan(rig) * StepSafety, drop);
                        float inner = Rise(MinSpan(rig), drop);
                        // if it cannot get down to that ground at all, stand it at its natural offset and let the
                        // ride height sort itself out, rather than collapsing the foot under the hip
                        if (outer <= inner) { outer = Mathf.Max(restFlat, inner); }
                        float reachable = Mathf.Clamp(flat.magnitude, inner, outer);
                        if (Mathf.Abs(reachable - flat.magnitude) < 1e-4f) break;
                        Vector3 dir = flat.sqrMagnitude > 1e-6f ? flat.normalized : new Vector3(rig.Outward.x, 0f, rig.Outward.z).normalized;
                        target = hip + dir * reachable;
                        target.y = ground(target.x, target.z);
                    }

                    Feet[i].Target = target;
                    Feet[i].Arc = ArcFor(Feet[i].Anchor, target, rig.Reach, rig.Chain.Length == 1, ground);
                    Feet[i].Swing = 0f;
                    swinging++;
                }
            }

            Carry(rigs, body, pos, yaw, lost, dt, ground, yawRate, pace);

            // Carry has just settled the ride height, and every hip moved with it. A foot in the air is held inside
            // what its leg can actually span against where the hips have ENDED UP, not where they were at the top
            // of the frame: the body keeps moving while a foot is out, so the start of a long step can otherwise
            // finish further behind its hip than the leg is long. A leg at full stretch should look like a leg at
            // full stretch, rather than be asked for the impossible and drawn somewhere else entirely.
            for (int i = 0; i < rigs.Length; i++)
            {
                var rig = rigs[i];
                if (rig == null || Feet[i].Lost) continue;

                if (Down > 0f)
                {
                    // going down: the body is sinking and a rigid leg can only keep its foot on the ground by
                    // pushing it further out. The foot skids outward, which is what a thing with stiff legs does
                    // when it stops holding itself up.
                    Vector3 hipNow = HipAt(rig, pos, yaw);
                    Vector3 off = Feet[i].At - hipNow;
                    float drop = hipNow.y - Feet[i].At.y;
                    float straight = MaxSpan(rig) * StepSafety;
                    float wantFlat = Mathf.Sqrt(Mathf.Max(0.04f, straight * straight - drop * drop));
                    Vector2 flat = new Vector2(off.x, off.z);
                    Vector2 dir = flat.sqrMagnitude > 1e-4f ? flat.normalized
                                : new Vector2(rig.Outward.x, rig.Outward.z).normalized;
                    Vector3 skid = new Vector3(hipNow.x + dir.x * wantFlat, 0f, hipNow.z + dir.y * wantFlat);
                    skid.y = ground(skid.x, skid.z);
                    Feet[i].At = Vector3.Lerp(Feet[i].At, skid, 1f - Mathf.Exp(-dt * 7f));
                    Feet[i].Anchor = Feet[i].At;
                    Feet[i].Swing = -1f;
                    continue;
                }

                if (Feet[i].Swing < 0f) continue;

                // A foot in the air must stay inside what its leg can span — but WHICH WAY it is brought back
                // inside matters more than that it is. Lifting a foot brings it closer to the hip, and a leg that
                // cannot fold has to answer that somehow. Pushing the foot outward to make up the distance is the
                // wrong answer and it is catastrophic: the foot lands a metre wide of where the step was aimed,
                // a wide foot lowers the ride height, a lower body makes the next foot fold tighter still, and the
                // machine splays itself flat within a second of setting off. What a rigid leg actually limits is
                // how high the foot comes UP, so the lift is capped and the step still lands where it was sent.
                Vector3 hip = HipAt(rig, pos, yaw);
                float dx = Feet[i].At.x - hip.x, dz = Feet[i].At.z - hip.z;
                float flatDist = Mathf.Sqrt(dx * dx + dz * dz);
                float ceilingY = hip.y - Rise(MinSpan(rig), flatDist);   // no higher than the fold allows
                if (Feet[i].At.y > ceilingY)
                    Feet[i].At.y = Mathf.Max(ground(Feet[i].At.x, Feet[i].At.z), ceilingY);

                // the far side still clamps by reach, because there the foot genuinely cannot get that far out
                Vector3 span = Feet[i].At - hip;
                float len = span.magnitude, hi = MaxSpan(rig);
                if (len > hi && len > 1e-4f) Feet[i].At = hip + span * (hi / len);
            }
        }

        /// <summary>How badly a leg needs to step: the worse of how far its foot has fallen behind and how much of
        /// its length it has used up, both as a share of reach so the two can be compared.</summary>
        float Urgency(TankModel.LegRig rig, Quaternion body, Vector3 pos, float yaw, int i)
        {
            if (rig.Reach <= 1e-4f || MaxSpan(rig) <= 1e-4f) return 0f;
            float span = Mathf.Max(1e-4f, MaxSpan(rig));
            float drift = Drift(rig, body, pos, i) / span;
            float stretched = Vector3.Distance(Feet[i].Anchor, HipAt(rig, pos, yaw)) / span;
            return Mathf.Max(drift, stretched);
        }

        /// <summary>How far a planted foot has fallen from where it wants to stand, on the flat.</summary>
        float Drift(TankModel.LegRig rig, Quaternion body, Vector3 pos, int i)
        {
            Vector3 home = Home(rig, body, pos, i), at = Feet[i].Anchor;
            float dx = home.x - at.x, dz = home.z - at.z;
            return Mathf.Sqrt(dx * dx + dz * dz);
        }

        /// <summary>Is a leg next to this one already off the ground? Legs are numbered down the left side and then
        /// down the right (TankModel.NumberLegs), so neighbours are adjacent numbers within a side.</summary>
        bool Beside(int i)
        {
            int side = i / perSide, k = i % perSide;
            for (int j = 0; j < Feet.Length; j++)
            {
                if (j == i || Feet[j].Swing < 0f || Feet[j].Lost) continue;
                if (j / perSide != side) continue;
                if (Mathf.Abs(j % perSide - k) == 1) return true;
            }
            return false;
        }

        /// <summary>How high this step must go: its ordinary height, or enough to clear whatever stands between the
        /// foot and where it is going — a parapet, a sandbag, the far lip of a trench.</summary>
        static float ArcFor(Vector3 from, Vector3 to, float reach, bool rigid, System.Func<float, float, float> ground)
        {
            // a leg built in one piece cannot fold, so lifting its foot high pulls the toe up off the ground it is
            // aiming at: it picks its feet up less, which is also how a thing with stiff legs actually moves
            float arc = reach * (rigid ? ArcShare * 0.55f : ArcShare);
            float basis = Mathf.Min(from.y, to.y);
            float high = Mathf.Max(from.y, to.y);
            for (int k = 1; k <= 3; k++)
            {
                float t = k * 0.25f;
                Vector3 at = Vector3.Lerp(from, to, t);
                high = Mathf.Max(high, ground(at.x, at.z));
            }
            return Mathf.Max(arc, high - basis + Clearance);
        }

        /// <summary>What the feet that are down do to the body: how high it rides and how it is tilted. A plane is
        /// fitted through them, so the machine lies on the ground it is standing on. With fewer than three feet down
        /// the fit is degenerate and the body simply keeps its level, and a machine missing legs is pulled over
        /// toward the corner that has nothing under it any more.</summary>
        void Carry(TankModel.LegRig[] rigs, Quaternion body, Vector3 pos, float yaw, byte lost, float dt,
                   System.Func<float, float, float> ground, float yawRate, float pace)
        {
            Vector3 fwd = body * Vector3.forward, right = body * Vector3.right;
            float sumY = 0f, sumF = 0f, sumR = 0f; int n = 0;
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null || Feet[i].Lost || Feet[i].Swing >= 0f) continue;
                Vector3 d = Feet[i].Anchor - pos;
                sumY += Feet[i].Anchor.y; sumF += Vector3.Dot(d, fwd); sumR += Vector3.Dot(d, right); n++;
            }
            if (n == 0) return;
            float meanY = sumY / n, meanF = sumF / n, meanR = sumR / n;

            float sff = 0f, srr = 0f, sfr = 0f, sfy = 0f, sry = 0f;
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null || Feet[i].Lost || Feet[i].Swing >= 0f) continue;
                Vector3 d = Feet[i].Anchor - pos;
                float f = Vector3.Dot(d, fwd) - meanF, r = Vector3.Dot(d, right) - meanR, y = Feet[i].Anchor.y - meanY;
                sff += f * f; srr += r * r; sfr += f * r; sfy += f * y; sry += r * y;
            }
            float det = sff * srr - sfr * sfr;
            if (n >= 3 && Mathf.Abs(det) > 1e-4f)
            {
                float a = (sfy * srr - sry * sfr) / det;    // d(height) / d(forward)
                float b = (sry * sff - sfy * sfr) / det;    // d(height) / d(right)
                fitPitch = Mathf.Atan(a);
                fitRoll = -Mathf.Atan(b);                   // Settle's sense: left up is positive
            }
            // too few feet down to say anything about the ground: keep the last plane they made rather than the
            // last answer we gave, or the lean below compounds on itself
            float wantPitch = fitPitch, wantRoll = fitRoll;

            // the legs it has lost pull it over: nothing holds that corner up any more
            Vector3 hole = Vector3.zero;
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null || (lost & (1 << i)) == 0) continue;
                hole += new Vector3(rigs[i].Hip.x + rigs[i].Rest.x, 0f, rigs[i].Hip.z + rigs[i].Rest.z).normalized;
            }
            if (hole.sqrMagnitude > 1e-4f)
            {
                hole = hole.normalized * Mathf.Min(1f, Limp * 2f);
                // hole points at the missing legs. Forward is +z, the machine's left is -x, and roll is positive
                // left-up — so losing the front drops the nose and losing the left drops the left.
                wantPitch -= hole.z * 0.13f;
                wantRoll += hole.x * 0.16f;
            }

            // A machine going round a corner leans into it. Nothing in this file did that: `yawRate` reached it
            // only as a sweep on the step target, and `Roll` came from the foot plane and the lost-leg hole and
            // nowhere else, so a walker rounding a corner stood as square as one walking straight.
            //
            // The lean is the centripetal acceleration taken against gravity: a machine turning at 0.5 rad/s at
            // 1.2 m/s pulls 0.6 m/s^2 sideways, which is atan(0.6 / 9.81) = 3.5 degrees of honest bank. The
            // coefficient below gives that, and it is deliberately a fraction of the true angle rather than the
            // whole of it, because these are 300-tonne machines on legs and not motorcycles.
            //
            // Roll is positive left-up and Unity's yaw is positive clockwise from above, so a right turn
            // (yawRate > 0) lifts the left side, which is banking INTO the turn. It rides through the same clamp
            // and the same filter as everything else here, and it moves only what is drawn - no foot is placed
            // from Roll.
            wantRoll += yawRate * pace * TurnLean;

            // The tilt is settled FIRST, because how far each hip is from its foot depends on it. A hip a metre and
            // a quarter above the body's middle swings a good part of a foot when the machine leans, and reaches
            // worked out against hips placed by yaw alone are wrong by exactly that much.
            float k2 = 1f - Mathf.Exp(-dt * 9f);
            Pitch = Mathf.Lerp(Pitch, Mathf.Clamp(wantPitch, -0.42f, 0.42f), k2);
            Roll = Mathf.Lerp(Roll, Mathf.Clamp(wantRoll, -0.38f, 0.38f), k2);

            float height = meanY + Stand(rigs);
            // and now the two bounds against the feet as they actually lie, with the hips where the lean has
            // actually put them: no higher than the leg that is stretched furthest allows, and no lower than the
            // leg that can fold least allows. Taking only the ceiling lets one wide foot drag the machine down
            // through the rest of them.
            var tilt = Carriage(yaw);
            float ceiling = float.MaxValue, floor = float.MinValue;
            for (int i = 0; i < rigs.Length; i++)
            {
                if (rigs[i] == null || Feet[i].Lost || Feet[i].Swing >= 0f) continue;
                Vector3 arm = tilt * rigs[i].Hip;          // the hip relative to the body's middle, leaning and all
                float dx = Feet[i].Anchor.x - (pos.x + arm.x), dz = Feet[i].Anchor.z - (pos.z + arm.z);
                float flat = Mathf.Sqrt(dx * dx + dz * dz);
                ceiling = Mathf.Min(ceiling, Feet[i].Anchor.y + Rise(MaxSpan(rigs[i]) * StepSafety, flat) - arm.y);
                floor = Mathf.Max(floor, Feet[i].Anchor.y + Rise(MinSpan(rigs[i]), flat) - arm.y);
            }
            float means = meanY + Stand(rigs);
            if (ceiling < float.MaxValue) height = Mathf.Min(height, ceiling);
            // the floor only holds a machine that is still holding itself up
            if (floor > float.MinValue && Down <= 0f) height = Mathf.Max(height, floor);
            // and it does not squat below the height it means to stand at, however far its feet have trailed
            // and it never rides HIGHER than the stance it worked out for itself either. Every step is chosen
            // against that height, so a body that floats above it stretches feet that were placed correctly for
            // it — which is how a foot came to be planted 1.73 m from a hip whose leg is 1.62 m long. The stance
            // is a ceiling as well as a target; the ground may only ever push it down.
            if (Down <= 0f) height = Mathf.Clamp(height, means - MaxSink, means);

            // A step in the ground the feet have NOT reached yet — the bank of a parapet, the far lip of a trench —
            // is invisible to a ride height derived only from the feet already planted, and MaxSink cannot catch it:
            // it is clamped against `means`, computed from the same falling mean it is meant to restrain. Measured
            // on a Pincer mounting a parapet: the hull lost 1.4 m, squatted onto the bank and ploughed through it.
            //
            // So the dirt under the hull gets a vote. It gets a BOUNDED vote, and the bound is the whole lesson of
            // the first attempt at this (cycle A6): lifting the body invalidates step targets that were chosen for
            // a lower one. `Step` picks each foot against the height the body had when it picked it; raise the body
            // afterwards by an arbitrary amount and the leg cannot reach the foot it was sent to — Banner's rear
            // leg missed by 0.093 m against a 0.015 m tolerance, intermittently, which is worse than a scraped
            // belly because a detached foot is visible and a slightly low hull is not.
            //
            // Two bounds, therefore. It may not rise above what the currently planted legs can reach (`ceiling`),
            // and it may not rise more than MaxSink above the stance the machine chose for itself — the same
            // distance it is allowed to sink, used symmetrically, so the lift can never exceed the margin the
            // step-target clamp already carries.
            if (ground != null && Down <= 0f)
            {
                float under = ground(pos.x, pos.z);
                for (int i = 0; i < rigs.Length; i++)
                {
                    if (rigs[i] == null) continue;
                    Vector3 h = body * rigs[i].Hip;      // part-way out to each hip: under the hull, not out at the feet
                    under = Mathf.Max(under, ground(pos.x + h.x * 0.6f, pos.z + h.z * 0.6f));
                }
                float belly = Mathf.Min(under + Clearance, means + MaxSink);
                if (ceiling < float.MaxValue) belly = Mathf.Min(belly, ceiling);
                if (belly > height) height = belly;
            }
            if (Down > 0f) height = Mathf.Lerp(height, meanY - Hips(rigs) * BellyShare, Down);

            Height = Mathf.Lerp(Height, height, k2);
            // and the SMOOTHED value is bounded too, not just the target it is heading for. The body may lag its
            // feet — that is where the weight comes from — but the legs are rigid and cannot, so a lag that leaves
            // the hips nearer a foot than the leg can fold has to be refused outright rather than eased out of.
            if (ceiling < float.MaxValue) Height = Mathf.Min(Height, ceiling);
            if (floor > float.MinValue && Down <= 0f) Height = Mathf.Max(Height, floor);
            if (Down <= 0f) Height = Mathf.Clamp(Height, means - MaxSink, means);
        }

/// <summary>How far a leg built in one piece may stretch or shorten to keep its toe on the ground.</summary>
        public const float StretchMin = 0.80f, StretchMax = 1.18f;
        /// <summary>Stand is deliberately the tighter margin, so a foot standing at home is comfortably inside
        /// what a step will allow and its own clamp never pulls it in.</summary>
        /// <summary>The most of its length a leg is ever asked for.</summary>
        public const float StepSafety = 0.97f;

        /// <summary>How far out a leg stands, as a share of its length: the sine of its angle from straight down.
        ///
        /// A walker's stance cannot be read off the model, because five of the six were never modelled standing.
        /// Censer's four legs hang straight down with the feet directly under the hips; Redoubt's front pair point
        /// forward with the toe only 24 cm below the hip, so they never touched the ground at all; Kettle's front
        /// feet cross to the far side of the body. Using `Rest` as the place a leg wants to stand — which is all it
        /// records, where the sculptor happened to leave that limb — gives Censer a stance ring of radius zero and
        /// nowhere to put a foot.
        ///
        /// A leg is a rod of known length hanging from a hip of known height, and that is enough. The body settles
        /// until the shortest-reaching leg stands at this angle, and then every foot is placed out along the way
        /// its own leg sticks out from the body. The pose the model was left in is not consulted.</summary>
        public const float Lean = 0.45f;

        /// <summary>How hard a machine banks into a turn. The lean is `yawRate * pace * TurnLean` radians, so at
        /// 0.5 rad/s and 1.2 m/s this gives 2.06 degrees — about 59% of the 3.5 degrees the true centripetal
        /// angle atan(v*w/g) would ask for, which is deliberate: these are 300-tonne machines on legs, not
        /// motorcycles. Measured (cycle A9) by driving the solver tick by tick with
        /// yawRate 0.5 and speed 1.2: the roll settles at 2.06 degrees and holds, against 0.00 on an otherwise
        /// identical straight walk.</summary>
        public const float TurnLean = 0.06f;

        /// <summary>The furthest the feet may pull the body below the height it means to stand at.
        ///
        /// Without this the machine walks itself into the ground, and the reason is worth keeping. A planted foot
        /// stays where it was put while the body walks over it, so it falls steadily behind; a trailing foot
        /// stretches its leg; and the ride height is capped by whatever the most-stretched leg can still reach, so
        /// the hips come down to meet it. Lower hips shrink the ring of ground the next foot may be put on, so the
        /// next step lands worse, and within a second the machine is flat on its belly with its legs splayed.
        /// Every step of that is correct in isolation.
        ///
        /// The escape is to refuse the last part. A leg that has run out of length is drawn slightly stretched
        /// instead, which nobody sees, rather than the whole machine being lowered, which everybody sees. Holding
        /// the body up also keeps the feet in their proper ring, so a firm ride height needs LESS faking than a
        /// soft one: uncapped, the worst leg is drawn at 1.25 of its length; capped here, at 1.13.</summary>
        public static readonly float MaxSink = 0.30f * VehicleSize.Walker;

        /// <summary>How high the hips stand above the body's middle, as modelled: what a collapse has to lose.</summary>
        float Hips(TankModel.LegRig[] rigs)
        {
            if (hips > 0f) return hips;
            float sum = 0f; int n = 0;
            for (int i = 0; i < rigs.Length; i++) if (rigs[i] != null) { sum += rigs[i].Hip.y; n++; }
            hips = n > 0 ? sum / n : 1f;
            return hips;
        }

        /// <summary>The closest a leg can bring its toe to its hip. A leg with joints folds; a leg built in one
        /// piece (Pincer's) cannot fold AT ALL, and asking one to put its foot half a bone-length away is as
        /// impossible as asking it to reach twice its length. This is the bound that was missing.</summary>
        public static float MinSpan(TankModel.LegRig r)
        {
            if (r.Chain.Length == 1) return r.Reach * StretchMin;
            float upper = r.Bone[0], lower = 0f;
            for (int k = 1; k < r.Bone.Length; k++) lower += r.Bone[k];
            return Mathf.Abs(upper - lower) + 0.02f;
        }

        /// <summary>The furthest a leg can put its toe from its hip.
        ///
        /// This is NOT `Reach`. Reach is the distance from hip to toe in the pose the leg was modelled in, which for
        /// a leg modelled bent is a good deal less than the leg is long: Banner's rear pair are two bones of 0.655
        /// and 0.247 — 0.902 m of leg — recorded with a reach of 0.757 because the sculpt had them folded. Working
        /// the bounds out from Reach therefore asked the wrong question, and every answer was wrong by however far
        /// that leg happened to be bent. It showed up as a foot planted 1.050 m from a hip that could reach 0.902.
        ///
        /// A leg in one piece can only put its toe at exactly its own length, so that is its span. A leg with
        /// joints can straighten, so its span is its bones end to end.</summary>
        public static float MaxSpan(TankModel.LegRig r)
        {
            if (r.Chain.Length == 1) return r.Reach;
            float sum = 0f;
            for (int k = 0; k < r.Bone.Length; k++) sum += r.Bone[k];
            return sum;
        }

        /// <summary>How high a hip must sit above a foot that is `flat` metres away from it horizontally, for a span
        /// of `span`. Zero when the foot is further out than the span reaches.</summary>
        static float Rise(float span, float flat) => Mathf.Sqrt(Mathf.Max(0f, span * span - flat * flat));

        /// <summary>Turn the feet into part transforms. `local` is filled for every part of a leg that has a
        /// solution; `solved` says which. The body's own transform is passed so a foot in the world can be brought
        /// back into the frame the parts are accumulated in.
        ///
        /// A leg with joints reaches its foot exactly, because the joints take up the difference. A leg modelled in
        /// ONE piece (Pincer's are) cannot: turning it aims the toe correctly but leaves it hanging short or driven
        /// into the ground by however much the body has moved. Those legs are allowed to stretch a little along
        /// their own length to make up the difference, which is invisible at a few per cent and is the difference
        /// between a foot that is on the ground and a foot that is nearly on it. It is safe because a leg in one
        /// piece has nothing hanging off it to be skewed by the scale.</summary>
        public void Solve(TankModel.Lod lod, Matrix4x4 bodyToWorld, Matrix4x4[] local, bool[] solved)
        {
            var rigs = lod.Legs;
            if (rigs == null) return;
            for (int i = 0; i < solved.Length; i++) solved[i] = false;
            Matrix4x4 toBody = bodyToWorld.inverse;

            for (int i = 0; i < rigs.Length && i < Feet.Length; i++)
            {
                var rig = rigs[i];
                if (rig == null || Feet[i].Lost) continue;

                Vector3 want = toBody.MultiplyPoint3x4(Feet[i].At);
                Vector3 d = want - rig.Hip;
                float dist = d.magnitude;
                if (dist < 1e-4f) continue;
                Vector3 dn = d / dist;
                int n = rig.Chain.Length;
                float grow = 1f;                 // how far a jointed leg has to be drawn long to reach its foot

                if (n == 1)
                {
                    bone[0] = Quaternion.FromToRotation(rig.RestDir[0], dn) * rig.RestRot[0];
                }
                else
                {
                    // two links: the first bone, and everything past it taken as one. The knee is put on the side
                    // that raises it, which is the way a crab's leg is built — up and out, then down to the toe.
                    float upper = rig.Bone[0], lower = 0f;
                    for (int k = 1; k < n; k++) lower += rig.Bone[k];
                    // A leg with joints was the only kind that could not answer being asked for more than it has.
                    // A one-piece leg is drawn a little long and the toe lands where it was sent; a jointed one
                    // clamped its links and left the toe hanging short instead. Banner and Kettle are given speeds
                    // their legs cannot sustain — four legs, two of them off the ground at a time, so a leg gets
                    // asked past full stretch however early it decides to lift — and the visible result was a foot
                    // detached from its leg rather than a leg reaching. Both kinds now stretch, and a leg drawn
                    // seven per cent long is not a thing anyone sees.
                    float total = upper + lower;
                    grow = total > 1e-4f ? Mathf.Clamp(dist / total, 1f, StretchMax) : 1f;
                    upper *= grow; lower *= grow;
                    float reach = Mathf.Clamp(dist, Mathf.Abs(upper - lower) + 1e-3f, upper + lower - 1e-3f);
                    float cos = Mathf.Clamp((upper * upper + reach * reach - lower * lower) / (2f * upper * reach), -1f, 1f);
                    float alpha = Mathf.Acos(cos) * Mathf.Rad2Deg;

                    Vector3 axis = Vector3.Cross(dn, Vector3.up);
                    if (axis.sqrMagnitude < 1e-4f) axis = Vector3.Cross(dn, rig.Outward);
                    if (axis.sqrMagnitude < 1e-4f) axis = Vector3.right;
                    axis.Normalize();

                    Vector3 up0 = Quaternion.AngleAxis(alpha, axis) * dn;
                    Vector3 up1 = Quaternion.AngleAxis(-alpha, axis) * dn;
                    Vector3 upperDir = (rig.Hip + up0 * upper).y > (rig.Hip + up1 * upper).y ? up0 : up1;

                    Vector3 knee = rig.Hip + upperDir * upper;
                    Vector3 lowerDir = want - knee;
                    lowerDir = lowerDir.sqrMagnitude > 1e-6f ? lowerDir.normalized : dn;

                    bone[0] = Quaternion.FromToRotation(rig.RestDir[0], upperDir) * rig.RestRot[0];
                    for (int k = 1; k < n && k < bone.Length; k++)
                        bone[k] = Quaternion.FromToRotation(rig.RestDir[k], lowerDir) * rig.RestRot[k];
                }

                for (int k = 0; k < n && k < bone.Length; k++)
                {
                    int part = rig.Chain[k];
                    if (part < 0 || part >= local.Length) continue;
                    Quaternion parent = k == 0 ? rig.ParentRot : bone[k - 1];
                    Quaternion q = Quaternion.Inverse(parent) * bone[k];
                    // scaling the hip part scales the links hanging off it as well, so the chain stays joined
                    var m = Matrix4x4.TRS(lod.Parts[part].Local, q,
                        k == 0 && n > 1 && grow > 1f ? Vector3.one * grow : Vector3.one);
                    if (n == 1 && rig.Bone[0] > 1e-3f)
                    {
                        float stretch = Mathf.Clamp(dist / rig.Bone[0], StretchMin, StretchMax);
                        if (Mathf.Abs(stretch - 1f) > 1e-3f)
                        {
                            // the bone points along dn once it is turned, so in the part's own posed frame its
                            // length runs along this axis; scale along that and nothing else
                            Vector3 axis = Quaternion.Inverse(bone[0]) * dn;
                            Quaternion a = Quaternion.FromToRotation(Vector3.forward, axis);
                            m *= Matrix4x4.Rotate(a) * Matrix4x4.Scale(new Vector3(1f, 1f, stretch)) * Matrix4x4.Rotate(Quaternion.Inverse(a));
                        }
                    }
                    local[part] = m;
                    solved[part] = true;
                }
            }
        }
    }
}
