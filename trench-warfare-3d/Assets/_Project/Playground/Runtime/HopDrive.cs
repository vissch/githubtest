// Phase: Playground (2026-09-28, lane/show/playground) — a machine that hops, on VehicleRig
// Hops a VehicleRig whose tank3.json says "hopper" (Tools/mechsplit.py TW_KIND=gatling: the Bullfrog, a toad whose body,
// head and legs Tripo welded into one piece, so it cannot step: it hops, the way a toad goes). One hop is a crouch, a
// flight and a landing that squats and comes back up; a toad lands flat on all four feet and throws dust from each.
// `walk <speed>` sends it hopping round a circle of Radius (`walk <speed> 1` hops on the spot); standing, it breathes.
// Knocked out it slumps over onto the side of the gun it has lost (its own seed's side when it has lost none), nose
// down, the corner it goes onto dug 5 cm into the ground: sinking it 0.22 m put its feet through the floor (critic g1).
//
// The legs are welded to the body, so the whole toad is one rigid piece: whenever it is on the ground its four feet stay
// on it (the pose is raised by whatever the pitch pushed a foot under), and a crouch or a landing reads as the nose
// dipping over planted feet, not the feet sinking. (Sunk 0.12 m and pitched 5 degrees, the front feet went 0.24 m under.)
// Rigid legs cannot bend, so the body squashes and stretches instead (the hull's scale, volume kept: taller is thinner):
// it squashes as it gathers, stretches as it springs and reaches for the ground, squashes flat on landing and overshoots
// back. Planted, the crouch and the landing squat had nothing left to show (the lift on the ground is only what keeps the
// feet on it), and the crouch, the top of the hop and the landing looked alike in stills (critic g6). The turret gets
// the inverse scale: the hull's scale is uniform across its width and the turret turns only about the hull's up, so the
// two cancel exactly and the saddle and guns never shear.
// The legs move although Tripo welded them to the body: they are skinned (LegRig, bones and weights from
// Tools/legrig.py) on the rig's own copy of each hull mesh and posed through the hop by Drives: the hind legs unfold and
// swing to trail straight back off the take-off, the forelegs sweep back under it and then reach forward and down for
// the landing, and knocked out all four sprawl (tucked in the sitting pose all the way, it read as a statue lifted,
// critic g8). The body stands on the skinned feet and on everything that follows a leg, so no foot, knee or heel goes
// into the ground, and the unfolding legs give the take-off its push.
// A hit jolts it away from the blow (a quick squash, the legs taking it). Between hops it settles and sits a moment,
// and sitting its throat swells twice every few seconds, the way a toad's does. Knocked out its legs sprawl wide so it
// drops onto its belly, then a hind leg kicks a few times, weaker each time; the cook-off throws the body up off them.
// The saddle rides the body on a spring: it sags as the body springs up and bounces as it lands. Firing, the body
// shudders with the guns and leans back against them. (Critic g6: a rigid statue that bounced; firing, nothing moved.)
// Only the hull moves in the rig's frame (its height, pitch and squash), and the rig's root along the circle, as WalkerDrive
// moves it: a clock that only dt advances, so three copies at three LODs hop identically (VehicleRig's rule).
using UnityEngine;

namespace TW.Playground
{
    public sealed class HopDrive : MonoBehaviour
    {
        public float Speed;                        // m/s along the circle (the "walk" command); 0 stands and breathes
        public float Radius = 18f;
        public bool InPlace;                       // hops on the spot (to look at)
        public float Period = 1.15f;               // seconds per hop: crouch, flight, landing
        public float Height = 1.3f;                // metres the body rises at the top of a hop, at 2 m/s (more with pace; 0.9 hardly read at the battle's 78 m, g7)
        public const float Crouch = 0.18f, Flight = 0.5f;   // shares of a hop; the landing takes the rest
        /// <summary>Seconds it sits between hops (settled, breathing), on top of Period: hop after hop without a pause read
        /// as a machine bouncing, not a toad hopping. The pace is kept: each hop goes further.</summary>
        public float Rest = 0.3f;
        public float T { get; private set; }        // its own clock
        public Vector3 HopPos { get; private set; } // metres from where it was built, in its build frame
        public float HopYaw { get; private set; }   // radians turned since it was built
        public int Landings { get; private set; }   // hops landed since it was built (dust, and the tests)
        /// <summary>How far the hull stands above its modelled rest this frame, metres (negative in a crouch).</summary>
        public float Lift { get; private set; }
        /// <summary>The hull's height scale this frame (1 at rest; under 1 squashed, over 1 stretched; its width 1/sqrt).</summary>
        public float Stretch { get; private set; } = 1f;

        VehicleRig rig;
        VehicleRig.Part hull;
        Vector3 startPos; Quaternion startRot; bool started;
        float phase, slump;
        bool wasAir;
        Vector3[] toes;   // the hull's lowest LOD0 vertices, in its frame (rig units): what it stands on
        VehicleRig.Part saddle;
        Mesh[] legMesh; LegRig legs; int[] standOn, belly, legPts;
        float deadSplay = 1f, deadDy = float.NaN;
        readonly int[] thigh = { -1, -1 }, shin = { -1, -1 }, foot = { -1, -1 }, arm = { -1, -1 }, fore = { -1, -1 }, hand = { -1, -1 };
        /// <summary>The legs' drives this frame (0..1; see Drives): the hind legs unfolding, the forelegs tucked, reaching.</summary>
        public float Extend { get; private set; }
        public float Tuck { get; private set; }
        public float ReachDrive { get; private set; }
        /// <summary>In the flight of a hop this frame (its feet off the ground by the hop's clock).</summary>
        public bool InAir { get; private set; }
        /// <summary>The throat's swell this frame (0..1), and how hard the last hit jolted it (0..1, decaying).</summary>
        public float Throat { get; private set; }
        public float Jolt { get; private set; }
        /// <summary>Metres the body is let down over its planted feet this frame (the crouch, the landing, a hit, firing).</summary>
        public float Sink { get; private set; }
        float flinchAt = -99f, flinchStrength; Vector3 flinchDir; float deadAt = -1f, cookAt = -1f;

        /// <summary>A hit: away is the direction the blow pushes, in the rig's frame; strength about damage / 40.</summary>
        public void Flinch(Vector3 away, float strength)
        {
            away.y = 0f;
            flinchDir = away.sqrMagnitude > 1e-6f ? away.normalized : Vector3.back;
            flinchStrength = Mathf.Clamp(Mathf.Max(strength, T - flinchAt < 0.1f ? flinchStrength : 0f), 0f, 1.2f);
            flinchAt = T;
        }

        /// <summary>The jolt's envelope: up in 0.05 s, gone over 0.2 s.</summary>
        float JoltNow() { float k = T - flinchAt; return k < 0f ? 0f : flinchStrength * (k < 0.05f ? k / 0.05f : Mathf.Exp(-(k - 0.05f) / 0.2f)); }
        /// <summary>Skinned legs (a legs file matched this hull), not a rigid body.</summary>
        public bool HasLegs => legs != null;
        /// <summary>Hold the legs at these drives (extend, tuck, reach, splay) whatever the hop does ("legpose"; null lets go).</summary>
        public Vector4? LegOverride;
        float springX, springV, prevLift, prevLiftV, fireLean; int liftFrames;
        public const float SaddleHz = 2.2f, SaddleDamping = 0.3f, SaddleTravel = 0.12f;   // the spring; metres at most
        /// <summary>How far the saddle rides above (+) or below its seat on the body this frame, metres.</summary>
        public float SaddleOffset => springX;

        /// <summary>Put every hop at phase u (0..1: the crouch to 0.18, the flight to 0.68, then the landing): with time
        /// frozen, a still of a chosen moment ("hopphase"; a still taken on a clock caught whatever moment its delay hit).</summary>
        public void SetPhase(float u) { phase = u * Period / (Period + Rest); wasAir = false; }

        public HopDrive Init(VehicleRig r, string legsJson = null)
        {
            rig = r; hull = r.Find("Hull"); saddle = r.Find("Turret");
            if (!started) { started = true; startPos = transform.position; startRot = transform.rotation; }
            // what it stands on: the hull's lowest LOD0 vertices (its feet and belly), all of them. The box's bottom
            // corners stood further out than any foot, so tipped over the body floated 8 cm off the ground on them; one
            // in every few missed the lowest foot of a tilt and left it 9 cm under
            var b = hull.Box;
            var mesh = hull.Lods != null && hull.Lods.Length > 0 ? hull.Lods[0] : null;
            var feet = new System.Collections.Generic.List<Vector3>();
            if (mesh != null && mesh.isReadable)
                foreach (var v in mesh.vertices) if (v.y < b.min.y + 0.12f * b.size.y) feet.Add(v);
            if (feet.Count == 0) for (int k = 0; k < 4; k++) feet.Add(new Vector3(k % 2 == 0 ? b.min.x : b.max.x, b.min.y, k < 2 ? b.min.z : b.max.z));
            toes = feet.ToArray();
            Legs(legsJson);
            return this;
        }

        /// <summary>The rig's own copy of each hull LOD mesh (the legs are skinned on it), with bounds grown so a leg
        /// reaching out is never culled, and the legs' bones and weights on them (Tools/legrig.py; none: they stay still).</summary>
        void Legs(string json)
        {
            if (legMesh != null || hull.Lods == null) return;
            foreach (var m in hull.Lods) if (m == null || !m.isReadable) return;
            int n = hull.Lods.Length;
            legMesh = new Mesh[n];
            for (int k = 0; k < n; k++)
            {
                var src = hull.Lods[k];
                var copy = Instantiate(src); copy.name = src.name + " (legs)";
                var bb = src.bounds; bb.Expand(0.3f * hull.Box.size.magnitude); copy.bounds = bb;
                legMesh[k] = copy;
                if (hull.F != null && hull.F.sharedMesh == src) hull.F.sharedMesh = copy;
                hull.Lods[k] = copy;
            }
            legs = LegRig.Parse(json, legMesh, rig.name);
            if (legs == null) return;
            // it stands on its lowest vertices and on everything that follows a leg: an unfolding leg's knee or heel can
            // come lower than the feet it started on
            var stand = new System.Collections.Generic.List<int>();
            var v0 = legMesh[0].vertices; var box = hull.Box;
            for (int i = 0; i < v0.Length; i++) if (v0[i].y < box.min.y + 0.12f * box.size.y || legs.LegShare(0, i) > 0.05f) stand.Add(i);
            standOn = stand.ToArray();
            // knocked out it lies on its belly: the body's own underside (no leg in it), and the legs' own points
            var bel = new System.Collections.Generic.List<int>(); var lp = new System.Collections.Generic.List<int>();
            for (int i = 0; i < v0.Length; i++)
            {
                float share = legs.LegShare(0, i);
                if (share < 0.02f && v0[i].y < box.min.y + 0.45f * box.size.y) bel.Add(i);
                if (share > 0.05f) lp.Add(i);
            }
            belly = bel.ToArray(); legPts = lp.ToArray();
            legs.Floor = box.min.y;
            string[] sides = { "L", "R" };
            for (int s = 0; s < 2; s++)
            {
                thigh[s] = legs.Bone("Thigh_" + sides[s]); shin[s] = legs.Bone("Shin_" + sides[s]); foot[s] = legs.Bone("Foot_" + sides[s]);
                arm[s] = legs.Bone("Arm_" + sides[s]); fore[s] = legs.Bone("Fore_" + sides[s]); hand[s] = legs.Bone("Hand_" + sides[s]);
            }
            // how high the push stands it at take-off (stretched, level): the flight starts from there, not from the ground
            PoseLegs(Push, 0.6f, 0f, 0f, 0f, 0f, 0f, 0f);   // the pose at the end of the crouch (Drives)
            float w = 1f / Mathf.Sqrt(Spring);
            Lowest(Quaternion.identity, new Vector3(w, Spring, w), out float low, out float rest);
            takeoff = Mathf.Max(0f, (rest - low) * rig.Size);
            PoseLegs(0f, 0f, 0f, 0f, 0f, 0f, 0f, 0f);
        }

        void OnDestroy()
        {
            if (legMesh == null) return;
            foreach (var m in legMesh) if (m != null) { if (Application.isPlaying) Destroy(m); else DestroyImmediate(m); }
        }

        /// <summary>The legs through a hop at phase u, as five drives (0..1): extend, the hind legs unfolding (straight
        /// out in the first tenth of the flight, folding back up by two thirds of it); trail, those legs swung from
        /// pushing down to trailing straight back behind (propped on them from below, the body rose 2.5 m, twice the hop,
        /// before it left the ground); tuck, the forelegs swept back under it leaving the ground; reach, the forelegs out
        /// forward and down for the landing; absorb, the hind knees folding as it gathers and lands.</summary>
        public static void Drives(float u, out float extend, out float trail, out float open, out float tuck, out float reach, out float absorb)
        {
            extend = trail = open = tuck = reach = absorb = 0f;
            if (u < Crouch)
            {
                float c = u / Crouch;
                absorb = 0.5f * Mathf.Sin(Mathf.Min(c / 0.6f, 1f) * Mathf.PI);   // gathers
                // and pushes off: the hind legs start to unfold with the feet still on the ground, which stands the body
                // up on them (unfolding only in the air, it rose with its legs still folded, critic g10)
                extend = Push * Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.6f, 1f, c));
                // leaning back as it pushes, so it leaves along a diagonal instead of standing up on stilts first (g11)
                trail = 0.6f * Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.75f, 1f, c));
            }
            else if (u < Crouch + Flight)
            {
                float k = (u - Crouch) / Flight;
                // the swing back leads the rest of the unfold: unfolding first, the legs propped it up to 1.9 m
                // (and waits for it: still unfolding while it swung, the legs stood it 0.7 m over the arc at k = 0.04)
                extend = Mathf.Lerp(Push, 1f, Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.07f, 0.2f, k))) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.35f, 0.7f, k)));
                trail = Mathf.Lerp(0.6f, 1f, Mathf.SmoothStep(0f, 1f, k / 0.07f));
                // the knee opens from its push to its trail after the swing: opening while it swung dropped the ankle
                open = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.07f, 0.2f, k));
                tuck = Mathf.SmoothStep(0f, 1f, k / 0.15f) * (1f - Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.3f, 0.5f, k)));
                // from 0.3, over the end of the tuck: from 0.45 the legs sat at rest mid-flight (g10)
                reach = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.3f, 0.8f, k));
            }
            else
            {
                float q = (u - Crouch - Flight) / (1f - Crouch - Flight);
                reach = 1f - Mathf.SmoothStep(0f, 1f, q / 0.4f);
                absorb = Mathf.Sin(Mathf.Min(q / 0.5f, 1f) * Mathf.PI);
            }
        }

        /// <summary>Pose the bones from the drives (and splay: knocked out, the legs sprawl; brace: firing, the forelegs
        /// set forward). Pitch turns about the body's right (+ swings a bone's far end from forward to down to back),
        /// splay about its forward, outward on each side.</summary>
        void PoseLegs(float extend, float trail, float open, float tuck, float reach, float absorb, float splay, float brace, float kickL = 0f, float kickR = 0f, float sink = 0f, float shift = 0f)
        {
            if (legs == null) return;
            if (LegOverride is Vector4 o) { extend = o.x; trail = open = 1f; tuck = o.y; reach = o.z; splay = o.w; absorb = 0f; brace = 0f; }
            Extend = extend; Tuck = tuck; ReachDrive = reach;
            for (int s = 0; s < 2; s++)
            {
                // sprawled 45 degrees out (20 left it sitting up, g10)
                float out_ = (s == 0 ? -1f : 1f) * 45f * splay;
                float ex = Mathf.Min(1f, extend + (s == 0 ? kickL : kickR));
                if (s == 1) Extend = Mathf.Max(extend, Mathf.Min(1f, extend + Mathf.Max(kickL, kickR)));
                // the roll outside the pitch: a sprawled leg folds and kicks in its own plane, flat along the ground (the
                // pitch outside swung a sprawled leg's kick straight up over the back like a tail, critic g13)
                Quaternion R(float pitch, float roll = 0f) => Quaternion.AngleAxis(roll, legs.Forward) * Quaternion.AngleAxis(pitch, legs.Right);
                // (the forelegs do not fold as it gathers or lands: turned about the shoulder the hands left the ground and
                // the body stood up on the elbows, +0.2 m in the crouch)
                // trailing, the thighs ride up 32 degrees: the body leaves nose up, and at 20 that swung the trailing feet
                // into the ground, which stood it 0.7 m over its arc
                // trailing, the leg opens nearly straight behind (knee 100 degrees; at 65 it zig-zagged, the knee up and the
                // foot down, g10); pushing, it presses down; absorbing, the knee folds (5 degrees did not show)
                Set(thigh[s], R(ex * Mathf.Lerp(-22f, 25f, trail) + 12f * absorb, out_));
                Set(shin[s], R(ex * Mathf.Lerp(55f, 100f, open) - 15f * absorb + 20f * splay));
                Set(foot[s], R(ex * Mathf.Lerp(25f, 60f, open)));
                Set(arm[s], R(-38f * reach + 24f * tuck - 15f * brace - 10f * splay, out_));
                Set(fore[s], R(-12f * reach + 20f * tuck));
                Set(hand[s], R(20f * reach - 10f * tuck));   // (35: the palm behind the wrist sheared)
            }
            legs.Solve();
            // the body sinking (and shifting forward) over its planted feet: each leg folds to take it, and the belly
            // flattens on the ground under it
            legs.Squash = Mathf.Max(0f, sink);
            if (sink == 0f && shift == 0f) return;
            for (int s = 0; s < 2; s++) { Fold(thigh[s], shin[s], foot[s], -shift, sink); Fold(arm[s], fore[s], hand[s], -shift, sink); }
            legs.Solve();
        }

        /// <summary>Two-bone IK in the body's side plane (forward, up): turn the upper and middle bones so the end bone's
        /// joint moves by (df, dy) hull units against the body, the end bone keeping its angle (the foot or hand stays
        /// flat). The clamp then stands the body on the moved feet, so a leg folded up by d lowers the body by d over
        /// feet that stay where they were: planted, a crouch or a landing had no dip (the lift was only what kept the feet
        /// on the ground) and the body only ever rose (critic g13). Exact while the legs have no roll (only knocked out).</summary>
        void Fold(int top, int mid, int end, float df, float dy)
        {
            if (top < 0 || mid < 0 || end < 0) return;
            Vector2 P(Vector3 p) => new Vector2(Vector3.Dot(p, legs.Forward), p.y);
            Vector2 h = P(legs.Joint(top)), k = P(legs.Joint(mid)), a = P(legs.Joint(end));
            float l1 = (k - h).magnitude, l2 = (a - k).magnitude;
            if (l1 < 1e-4f || l2 < 1e-4f) return;
            var t = a + new Vector2(df, dy) - h;
            float d = Mathf.Clamp(t.magnitude, Mathf.Abs(l1 - l2) + 1e-3f, l1 + l2 - 1e-3f);
            // the knee stays on the side of the hip-to-ankle line it is on
            var ha = a - h; var hk = k - h;
            float side = Mathf.Sign(ha.x * hk.y - ha.y * hk.x); if (side == 0f) side = 1f;
            float alpha = Mathf.Acos(Mathf.Clamp((l1 * l1 + d * d - l2 * l2) / (2f * l1 * d), -1f, 1f));
            float top1 = Mathf.Atan2(t.y, t.x) + side * alpha, top0 = Mathf.Atan2(hk.y, hk.x);
            var k1 = h + l1 * new Vector2(Mathf.Cos(top1), Mathf.Sin(top1)); var e1 = h + t - k1; var e0 = a - k;
            float d1 = Mathf.DeltaAngle(top0 * Mathf.Rad2Deg, top1 * Mathf.Rad2Deg);
            float d2 = Mathf.DeltaAngle(Mathf.Atan2(e0.y, e0.x) * Mathf.Rad2Deg, Mathf.Atan2(e1.y, e1.x) * Mathf.Rad2Deg);
            // a turn up in the side plane (counter-clockwise, forward to up) is a negative pitch about the body's right
            legs.Pose[top] = Quaternion.AngleAxis(-d1, legs.Right) * legs.Pose[top];
            legs.Pose[mid] = Quaternion.AngleAxis(-(d2 - d1), legs.Right) * legs.Pose[mid];
            legs.Pose[end] = Quaternion.AngleAxis(d2, legs.Right) * legs.Pose[end];
        }

        void Set(int b, Quaternion q) { if (b >= 0) legs.Pose[b] = q; }

        /// <summary>The lowest point of what it stands on, under the hull's turn and scale, in its frame's units; and the
        /// same at rest (the feet as modelled).</summary>
        /// <summary>Where the ground is in the rig's frame, relative to the hull at rest: under its modelled feet.</summary>
        float RestY() { float rest = float.MaxValue; foreach (var t in toes) rest = Mathf.Min(rest, (hull.RestRot * t).y); return rest; }

        /// <summary>The lowest of these skinned points under the hull's turn and scale, in its frame's units.</summary>
        float LowestOf(int[] set, Quaternion turn, Vector3 scale)
        {
            float low = float.MaxValue;
            foreach (int i in set) low = Mathf.Min(low, (hull.RestRot * turn * Vector3.Scale(scale, legs.Skin(0, i))).y);
            return low;
        }

        void Lowest(Quaternion turn, Vector3 scale, out float low, out float rest)
        {
            low = float.MaxValue; rest = float.MaxValue;
            if (legs != null && standOn != null)
            {
                foreach (int i in standOn)
                {
                    low = Mathf.Min(low, (hull.RestRot * turn * Vector3.Scale(scale, legs.Skin(0, i))).y);
                }
                foreach (var t in toes) rest = Mathf.Min(rest, (hull.RestRot * t).y);
                return;
            }
            foreach (var t in toes) { low = Mathf.Min(low, (hull.RestRot * turn * Vector3.Scale(scale, t)).y); rest = Mathf.Min(rest, (hull.RestRot * t).y); }
        }

        /// <summary>How wide the legs sprawl knocked out (PoseLegs' splay): the least that lays them out no lower than the
        /// belly (within a centimetre), at the slump's full tilt and flattening.</summary>
        float Sprawl(Quaternion tilt, float size)
        {
            float w = 1f / Mathf.Sqrt(0.9f); var sc = new Vector3(w, 0.9f, w);
            if (legs != null) legs.Squash = 0.26f / size;
            float bellyLow = LowestOf(belly, tilt, sc), sp = 1f;
            for (; sp < 2f; sp += 0.1f)
            {
                PoseLegs(0.3f, 0.5f, 1f, 0f, 0f, 0f, sp, 0f);
                if (LowestOf(legPts, tilt, sc) >= bellyLow - 0.01f / size) break;
            }
            return sp;
        }

        public const float Squash = 0.84f, Spring = 1.12f;   // the hull's height at the flattest landing, and at take-off
        public const float Push = 0.4f;   // how far the hind legs unfold on the ground, pushing off
        float takeoff;   // metres the push stands the body up at take-off: the flight's arc starts there and eases into its own

        /// <summary>The hop's shape at phase u (0..1): lift in metres per metre of Height, pitch in degrees (nose up +),
        /// and stretch, the hull's height scale.</summary>
        public static void Shape(float u, out float lift, out float pitch, out float stretch)
        {
            if (u < Crouch)
            {
                // gathers itself: rear down, nose up, squashing (nose down it looked like the landing, critic g3), then in
                // the last third springs up out of the squash, taller than it stands, as its feet leave the ground
                float c = u / Crouch;
                float k = Mathf.Sin(c * Mathf.PI);
                lift = -0.12f * k; pitch = 5f * k;
                stretch = c < 0.65f ? Mathf.Lerp(1f, 0.88f, Mathf.SmoothStep(0f, 1f, c / 0.65f))
                                    : Mathf.Lerp(0.88f, Spring, Mathf.SmoothStep(0f, 1f, (c - 0.65f) / 0.35f));
            }
            else if (u < Crouch + Flight)
            {
                float k = (u - Crouch) / Flight;
                lift = 4f * k * (1f - k);           // a parabola, feet off the ground the whole way
                pitch = 10f * (1f - 2f * k);        // nose up leaving, nose down arriving
                // stretched leaving, round at the top, a little long again reaching down for the ground
                stretch = k < 0.5f ? Mathf.Lerp(Spring, 1f, Mathf.SmoothStep(0f, 1f, k / 0.5f)) : Mathf.Lerp(1f, 1.05f, (k - 0.5f) / 0.5f);
            }
            else
            {
                // lands: squashes flat, springs back past its height, settles (a damped bounce over the landing's 0.37 s)
                float k = (u - Crouch - Flight) / (1f - Crouch - Flight);
                lift = -0.16f * Mathf.Sin(k * Mathf.PI) * (1f - 0.4f * k); pitch = -3f * Mathf.Sin(k * Mathf.PI);
                // flattest a tenth of the way in, back through its height at 0.3, 4 % over at 0.5, settled by the end
                float land = Mathf.SmoothStep(0f, 1f, k / 0.1f);   // from the reaching 1.05 into the squash
                float bounce = (Squash - 1f) * Mathf.Cos(Mathf.PI * (k - 0.1f) / 0.4f) * Mathf.Exp(-3.5f * Mathf.Max(0f, k - 0.1f));
                stretch = Mathf.Lerp(1.05f, 1f + bounce, land);
            }
        }

        /// <summary>One frame, after VehicleRig's pose.</summary>
        public void Drive(float dt)
        {
            if (rig == null || hull == null || hull.Loose) return;
            T += dt;
            float size = rig.Size;
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            bool free = !dead; foreach (var p in rig.Parts) if (p.Loose && p.Name != "Barrels_L" && p.Name != "Barrels_R") { free = false; break; }
            if (dead)
            {
                // slumps onto its belly, over onto the side it has lost (its own seed when it has lost nothing)
                slump = Mathf.MoveTowards(slump, 1f, dt / 0.8f);
                if (deadAt < 0f) deadAt = T;
                if (rig.State == VehicleRig.Stage.CookedOff && cookAt < 0f) cookAt = T;
                float roll = 0f;
                foreach (var p in rig.Parts) if (p.Loose && p.Name.StartsWith("Gun_")) roll = p.Name.EndsWith("_L") ? 11f : -11f;
                if (roll == 0f) roll = (rig.Seed & 1) == 0 ? 8f : -8f;
                float s = Mathf.SmoothStep(0f, 1f, slump);
                var tilt = Quaternion.Euler(4f * s, 0f, roll * s);
                // and flattens as it goes down, a belly-flop (settles at 0.9 of its height), its legs sprawled out wide so
                // it lies on its belly; then a hind leg kicks, one side then the other, weaker each time
                SetStretch(1f - 0.1f * s);
                float td = T - deadAt;
                float Kick(float at, float amp) => td > at && td < at + 0.35f ? amp * Mathf.Sin(Mathf.PI * (td - at) / 0.35f) : 0f;
                float kickL = Kick(1.1f, 0.6f) + Kick(2.6f, 0.25f), kickR = Kick(1.8f, 0.4f);
                Throat = 0f; if (legs != null) legs.Throat = 0f;
                if (legs != null && td == 0f) deadSplay = Sprawl(Quaternion.Euler(4f, 0f, roll), size);
                // the legs roll out first and stretch after: rolling and stretching together, a hind leg's shin swung down
                // through the ground on the way (0.46 m under, half way through the slump)
                float rollOut = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0f, 0.6f, slump)), stretchOut = Mathf.SmoothStep(0f, 1f, Mathf.InverseLerp(0.5f, 1f, slump));
                PoseLegs(0.3f * stretchOut, 0.5f, 1f, 0f, 0f, 0f, deadSplay * rollOut, 0f, kickL, kickR);
                if (legs != null) legs.Squash = 0.26f * s / size;   // and it flops, its belly spreading on the ground
                // it lies on its belly, not on its legs: laid out flat (Sprawl) they no longer reach lower than it. Stood on
                // everything, the sprawled feet propped it 0.44 m above its standing height, a table (critic g13). The
                // height is kept once it is down, so the kicks move only the legs (they dropped the body 0.21 m and back)
                float dy;
                if (!float.IsNaN(deadDy)) dy = deadDy;
                else
                {
                    Lowest(tilt, hull.T.localScale, out float deadLow, out float deadRest);
                    if (legs != null) deadLow = LowestOf(belly, tilt, hull.T.localScale);
                    dy = (deadRest - deadLow) - 0.05f * s / size;
                    if (slump >= 1f) deadDy = dy;
                }
                // the cook-off throws it up off its belly and drops it back (0.45 s)
                if (cookAt >= 0f && T - cookAt < 0.45f) dy += 0.55f / size * Mathf.Sin(Mathf.PI * (T - cookAt) / 0.45f);
                hull.T.localPosition = hull.RestLocal + Vector3.up * dy;
                hull.T.localRotation = hull.RestRot * tilt;
                if (legs != null)
                {
                    // the ground in the hull's frame, for the legs to lie on (LegRig.HasGround)
                    var up = Quaternion.Inverse(hull.RestRot * tilt) * Vector3.up; var dsc = hull.T.localScale;
                    legs.HasGround = true;
                    legs.GroundN = Vector3.Scale(dsc, up);
                    legs.GroundDir = Vector3.Scale(new Vector3(1f / dsc.x, 1f / dsc.y, 1f / dsc.z), up);
                    legs.GroundDir /= Vector3.Dot(legs.GroundN, legs.GroundDir);
                    legs.GroundC = RestY() - dy;
                }
                springX = springV = 0f; liftFrames = 0;
                legs?.Apply(rig.Lod);
                if (saddle != null && !saddle.Loose) saddle.T.localPosition = saddle.RestLocal;
                Lift = dy * size;
                return;
            }
            slump = 0f; deadAt = cookAt = -1f; deadDy = float.NaN;
            if (legs != null) legs.HasGround = false;
            float v = free ? Speed : 0f;
            float lift, pitch, stretch = 1f, extend = 0f, trail = 0f, open = 0f, tuck = 0f, reach = 0f, absorb = 0f, sink = 0f, shift = 0f; bool inAir = false, hopping = false;
            if (v > 0.05f || (InPlace && Speed > 0.05f))
            {
                float pace = Mathf.Max(Speed, 0.05f); hopping = true;
                float cycle = Period + Rest;
                phase += dt / cycle;
                float u = (phase - Mathf.Floor(phase)) * cycle / Period;   // past 1: sitting between hops
                bool sitting = u >= 1f; if (sitting) u = 0f;
                Shape(u, out lift, out pitch, out stretch);
                Drives(u, out extend, out trail, out open, out tuck, out reach, out absorb);
                if (sitting) { lift = 0f; pitch = 0f; stretch = 1f; absorb = 0.12f; }
                lift *= Height * Mathf.Clamp(pace / 2f, 0.6f, 1.4f);
                bool air = u >= Crouch && u < Crouch + Flight; inAir = air; InAir = air;
                // on the ground the crouch and the landing sink it over its planted feet (the legs fold, Fold)
                // (1.7 times the shape's dip: the crouch's nose-up pitch about the middle pushes the rear feet down, and the
                // clamp gave back more than half the sink, 0.076 m)
                if (!air) sink = 1.7f * Mathf.Max(0f, -lift);
                // a parabola from the push's height down to the ground: peaks near the middle (an arc plus a fading
                // offset peaked at a quarter and then sank, floating, then falling, g11)
                if (air) lift += takeoff * (1f - (u - Crouch) / Flight);
                // it only goes forward in the air: a hop's distance is the pace times the period, covered in the flight
                if (air && !InPlace)
                {
                    float step = v * (Period + Rest) / (Flight * Period) * dt, yawRate = v / Mathf.Max(1f, Radius);
                    HopYaw += yawRate * (Period + Rest) / (Flight * Period) * dt;
                    HopPos += Quaternion.AngleAxis(HopYaw * Mathf.Rad2Deg, Vector3.up) * Vector3.forward * step;
                    transform.SetPositionAndRotation(startPos + startRot * HopPos, startRot * Quaternion.AngleAxis(HopYaw * Mathf.Rad2Deg, Vector3.up));
                }
                if (wasAir && !air) Land();
                wasAir = air;
            }
            else
            {
                // standing: it breathes (the throat and belly, a slow swell)
                lift = Mathf.Sin(T * 2.1f) * 0.015f; pitch = Mathf.Sin(T * 2.1f + 1f) * 0.4f; stretch = 1f + Mathf.Sin(T * 2.1f) * 0.008f;
                absorb = 0.1f + 0.08f * Mathf.Sin(T * 0.9f);   // and shifts its weight on its haunches
                phase = 0f; wasAir = false; InAir = false;
            }
            // firing: leans back against the guns over a quarter second and shudders with them
            fireLean = Mathf.MoveTowards(fireLean, rig.Firing ? 1f : 0f, dt / 0.25f);
            float shake = 0f;
            if (fireLean > 0f)
            {
                // (2 degrees and 0.4 of shake hardly showed, critic g13) and it squats into the recoil, pushed back on its feet
                pitch += fireLean * (4f + 1.2f * Mathf.Sin(T * 2f * Mathf.PI * 11f));
                shake = fireLean * 1.5f * Mathf.Sin(T * 2f * Mathf.PI * 9.3f);
                if (!inAir) { sink += fireLean * (0.05f + 0.02f * Mathf.Sin(T * 2f * Mathf.PI * 11f)); shift -= fireLean * 0.04f; }
            }
            // a hit: the body rocks away from the blow, squashes, and the legs take it
            Jolt = JoltNow();
            if (Jolt > 1e-3f)
            {
                var d = Quaternion.Inverse(hull.RestRot) * flinchDir;
                pitch -= 7f * Jolt * d.z; shake -= 7f * Jolt * d.x; stretch *= 1f - 0.08f * Jolt; absorb += 0.6f * Jolt;
                // and on the ground it drops and is shoved back on its feet (a hit moved the body only up, critic g13)
                if (!inAir && legs != null) { sink += 0.25f * Jolt; shift += 0.15f * Jolt * Vector3.Dot(d, legs.Forward); }
            }
            // sitting still, the throat swells twice every 3.2 s (the pulses 0.45 s each: 0.3 s went by unseen, g15); firing,
            // it pumps with the burst
            float tt = Mathf.Repeat(T, 3.2f);
            Throat = fireLean > 0.01f ? fireLean * (0.45f + 0.35f * Mathf.Sin(T * 2f * Mathf.PI * 4f))
                   : !hopping && tt < 0.9f ? Mathf.Pow(Mathf.Sin(Mathf.PI * tt / 0.45f), 2f) : 0f;
            if (legs != null) legs.Throat = Throat;
            var turn = Quaternion.Euler(-pitch, 0f, shake);
            SetStretch(stretch);
            var sc = hull.T.localScale;
            // what the pitch and the squash push below the ground (or lift off it): on the ground the body is moved by
            // exactly that (its feet planted), in the air lifted by at least that (arriving nose down, the lift reaches 0
            // while the nose is still 10 degrees down: the front feet dug in 0.23 m before it landed)
            float low = float.MaxValue, rest = float.MaxValue;
            // firing, the haunches sink and pulse with the shudder and the forelegs brace (standing, they did nothing, g10)
            PoseLegs(extend, trail, open, tuck, reach, absorb + fireLean * (0.3f + 0.15f * Mathf.Sin(T * 2f * Mathf.PI * 11f)), 0f, fireLean, 0f, 0f, sink / size, shift / size);
            Sink = sink;
            Lowest(turn, sc, out low, out rest);
            legs?.Apply(rig.Lod);
            float clear = (rest - low) * size;
            lift = inAir ? Mathf.Max(lift, clear) : clear;
            Lift = lift;
            hull.T.localPosition = hull.RestLocal + Vector3.up * (lift / size);
            hull.T.localRotation = hull.RestRot * turn;
            Saddle(lift, hopping, dt);
        }

        /// <summary>The saddle on its spring, driven by the body's vertical acceleration while it hops (from the lift, so
        /// only dt moves it and copies agree). Standing it only settles: the firing shudder moved the lift a centimetre at
        /// 11 Hz and rang the spring 0.1 m, the gun block jumping 10-20 px between stills (critic g8). A frame with no
        /// time (a frozen still) leaves it where it is.</summary>
        void Saddle(float lift, bool hopping, float dt)
        {
            if (saddle == null || saddle.Loose) return;
            if (dt > 1e-5f)
            {
                float v = liftFrames > 0 ? (lift - prevLift) / dt : 0f;
                float a = liftFrames > 1 && hopping ? Mathf.Clamp((v - prevLiftV) / dt, -60f, 60f) : 0f;
                prevLift = lift; prevLiftV = v; liftFrames = Mathf.Min(liftFrames + 1, 2);
                float w = 2f * Mathf.PI * SaddleHz;
                // the body accelerating up leaves the saddle behind (down); slowing on landing throws it on down, then back
                springV += (-w * w * springX - 2f * SaddleDamping * w * springV - a) * dt;
                springX += springV * dt;
                if (Mathf.Abs(springX) > SaddleTravel) { springX = Mathf.Sign(springX) * SaddleTravel; springV = 0f; }
            }
            // firing, the gun block is driven back along its guns and chatters with the rounds (3-4.5 cm)
            var back = Vector3.zero;
            if (fireLean > 0f)
            {
                var f = saddle.T.localRotation * Vector3.forward; f.y = 0f;
                if (f.sqrMagnitude > 1e-6f) back = -f.normalized * (fireLean * (0.03f + 0.015f * Mathf.Sin(T * 2f * Mathf.PI * 12f)) / rig.Size);
            }
            saddle.T.localPosition = saddle.RestLocal + back + Vector3.up * (springX / rig.Size / Mathf.Max(0.5f, Stretch));
        }

        /// <summary>The hull's height scale s, its width 1/sqrt(s) (volume kept); each part hung on the hull gets the inverse.</summary>
        void SetStretch(float s)
        {
            Stretch = s;
            float w = 1f / Mathf.Sqrt(s);
            hull.T.localScale = new Vector3(w, s, w);
            var inv = new Vector3(1f / w, 1f / s, 1f / w);
            foreach (var p in rig.Parts) if (!p.Loose && p.T.parent == hull.T) p.T.localScale = inv;
        }

        void Land()
        {
            Landings++;
            if (rig.Fx == null) return;
            foreach (var k in new[] { "Socket_Toe_FL", "Socket_Toe_FR", "Socket_Toe_RL", "Socket_Toe_RR" })
            {
                var at = rig.Socket(k); var c = rig.Centre;
                // dust off the ground at each foot (the hovercraft's spray read as glass bubbles past the feet, critic g13)
                rig.Fx.Dust(new Vector3(at.x, rig.GroundY + 0.05f * rig.Size, at.z), new Vector3(at.x - c.x, 0f, at.z - c.z).normalized, 1.2f * rig.Size);
            }
        }
    }
}
