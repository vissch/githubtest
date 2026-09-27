// Phase: Playground (2026-09-27, lane/show/playground) — a two-legged walker on the game's own gait
// Walks a VehicleRig whose tank3.json says "walker" (Tools/mechsplit.py: Hull > Thigh > Shin > Foot, Claw > Jaw, Turret >
// Gun). Where the feet go, how high the body rides and how it tilts are the battle's own WalkerGait, fed a leg rig built
// from the manifest's pivots. The legs are then solved here, not by WalkerGait.Solve: the game's solver puts a knee on
// the side that RAISES it (a crab's leg, up and out, then down), and a biped's knee has to go forward, which is what the
// frog mech was modelled with (hip 2.9 m, knee 1.7 m and 0.45 m forward, ankle 0.5 m and 0.3 m back).
//
// Everything is worked in the walker's own frame (where it has walked to from where it was built, and its turn), never
// from world matrices, so three copies at three LODs anywhere in the world are posed identically (VehicleRig's rule).
using TW.Presentation.Tactical;
using UnityEngine;

namespace TW.Playground
{
    public sealed class WalkerDrive : MonoBehaviour
    {
        public float Speed;                        // m/s forward (the "walk" command); it walks a circle of Radius
        public float Radius = 22f;
        public bool InPlace;                       // a treadmill: it steps as if walking and stays where it was built (to look at)
        public float StrideArms = 30f;
        public float Rise = 0.12f;                 // metres the body rises as a foot passes under it (rig units)
        public float Sway = 4f;                    // degrees it leans over the foot it stands on
        public float HipTurn = 5f;                 // degrees the hips turn with the stride             // degrees of arm swing per metre the feet are apart along the body
        public Vector3 WalkPos { get; private set; }   // metres from where it was built, in its build frame
        public float WalkYaw { get; private set; }     // radians turned since it was built

        VehicleRig rig;
        readonly WalkerGait gait = new WalkerGait();
        public WalkerGait Gait => gait;                 // read by tests and the gait probe
        TankModel model;
        Vector3 startPos; Quaternion startRot;
        VehicleRig.Part hull, turret;
        readonly VehicleRig.Part[] thigh = new VehicleRig.Part[2], shin = new VehicleRig.Part[2], foot = new VehicleRig.Part[2], claw = new VehicleRig.Part[2], jaw = new VehicleRig.Part[2];
        Vector3 hullPivot;
        readonly Vector3[] hip0 = new Vector3[2], knee0 = new Vector3[2], ankle0 = new Vector3[2], toe0 = new Vector3[2];
        readonly float[] l1 = new float[2], l2 = new float[2];
        static readonly string[] Side = { "L", "R" };

        public WalkerDrive Init(VehicleRig r)
        {
            rig = r;
            startPos = transform.position; startRot = transform.rotation;
            hull = r.Find("Hull"); turret = r.Find("Turret");
            hullPivot = hull.RestLocal;
            var legs = new TankModel.LegRig[2];
            for (int s = 0; s < 2; s++)
            {
                thigh[s] = r.Find("Thigh_" + Side[s]); shin[s] = r.Find("Shin_" + Side[s]); foot[s] = r.Find("Foot_" + Side[s]);
                claw[s] = r.Find("Claw_" + Side[s]); jaw[s] = r.Find("Jaw_" + Side[s]);
                // rest pivots in the rig's own frame: each part's RestLocal is relative to its parent's pivot
                hip0[s] = hullPivot + thigh[s].RestLocal;
                knee0[s] = hip0[s] + shin[s].RestLocal;
                ankle0[s] = knee0[s] + foot[s].RestLocal;
                toe0[s] = r.SocketLocal("Socket_Toe_" + Side[s]);   // the middle of the sole (mechsplit.py)
                l1[s] = (knee0[s] - hip0[s]).magnitude; l2[s] = (ankle0[s] - knee0[s]).magnitude;
                // the gait's leg: ONE piece from hip to toe as modelled, so its ride height and stance come out as the
                // sculpt stands (a jointed leg is taken at full stretch and stood the frog up straight-legged)
                Vector3 rest = (toe0[s] - hip0[s]) * r.Size;
                legs[s] = new TankModel.LegRig
                {
                    Chain = new[] { thigh[s].Index }, Hip = hip0[s] * r.Size, Toe = Vector3.zero, Rest = rest,
                    Outward = new Vector3(Mathf.Sign(hip0[s].x), 0f, 0f), Reach = rest.magnitude, Drop = Mathf.Max(0.05f, -rest.y),
                    Bone = new[] { rest.magnitude }, RestRot = new[] { Quaternion.identity }, RestDir = new[] { rest.normalized },
                    ParentRot = Quaternion.identity,
                };
            }
            model = new TankModel { Name = r.name, LegCount = 2, LegsPerSide = 1 };
            model.Lods[0] = new TankModel.Lod { Legs = legs };
            model.Lods[1] = model.Lods[0];
            gait.Plant(model, Vector3.zero, 0f, Ground);
            return this;
        }

        static float Ground(float x, float z) => 0f;

        /// <summary>One frame: walk on (while whole: a part that has come off flies in the rig's frame, so the rig stays
        /// put from then on), step the gait, pose the parts. Called by VehicleRig after its own pose.</summary>
        public void Drive(float dt)
        {
            if (rig == null || hull.Loose) return;
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            bool free = !dead; foreach (var p in rig.Parts) if (p.Loose) { free = false; break; }
            float v = free ? Speed : 0f, yawRate = v / Mathf.Max(1f, Radius);
            if (InPlace) yawRate = 0f;
            WalkYaw += yawRate * dt;
            var face = Quaternion.AngleAxis(WalkYaw * Mathf.Rad2Deg, Vector3.up);
            Vector3 vel = face * Vector3.forward * v;
            WalkPos += vel * dt;
            if (free && !InPlace) transform.SetPositionAndRotation(startPos + startRot * WalkPos, startRot * face);
            byte lost = 0;
            for (int s = 0; s < 2; s++) if (thigh[s].Loose || shin[s].Loose || foot[s].Loose) lost |= (byte)(1 << s);
            if (dt > 0f) gait.Step(model, WalkPos, WalkYaw, vel, yawRate, lost, dead, dt, Ground);
            Pose(face);
        }

        void Pose(Quaternion face)
        {
            var inv = Quaternion.Inverse(face);
            float size = rig.Size;
            // the body: up by how high the gait rides it, tilted by the plane its feet make
            float dy = gait.Height / size - 0f;
            // a biped carries its weight: the body rises as the swinging foot passes under it, leans over the foot it
            // stands on and turns its hips with the stride. WalkerGait rides a crab level on the mean of its feet, and
            // on two legs that read as a statue sliding (critic loop 2 r30: the hull at 1.25 m in both walk frames)
            float lift = 0f, lean = 0f, stride = 0f;
            for (int s = 0; s < 2 && s < gait.Feet.Length; s++)
            {
                if (gait.Feet[s].Swing < 0f || gait.Feet[s].Lost) continue;
                float k = Mathf.Sin(gait.Feet[s].Swing * Mathf.PI);
                lift = Mathf.Max(lift, k); lean += (s == 0 ? -1f : 1f) * k;   // left foot up: lean onto the right
            }
            for (int s = 0; s < 2 && s < gait.Feet.Length; s++) stride += (s == 0 ? 1f : -1f) * (inv * (gait.Feet[s].At - WalkPos)).z / size;
            dy += Rise * lift;
            var tilt = Quaternion.AngleAxis(-gait.Pitch * Mathf.Rad2Deg, Vector3.right) * Quaternion.AngleAxis(-gait.Roll * Mathf.Rad2Deg + Sway * lean, Vector3.forward)
                     * Quaternion.AngleAxis(Mathf.Clamp(stride, -1.5f, 1.5f) * HipTurn, Vector3.up);
            hull.T.localPosition = hullPivot + new Vector3(0f, dy, 0f);
            hull.T.localRotation = tilt;
            Vector3 hullAt = hull.T.localPosition;
            float along = 0f;
            for (int s = 0; s < 2; s++)
            {
                if (thigh[s].Loose || gait.Feet.Length <= s) continue;
                var f = gait.Feet[s];
                // the foot's toe in the rig's frame; a swinging foot tips its toe down a little
                Vector3 toe = inv * (f.At - WalkPos) / size;
                float swing = f.Swing >= 0f ? Mathf.Sin(f.Swing * Mathf.PI) : 0f;
                var footRot = Quaternion.Euler(18f * swing, 0f, 0f);
                Vector3 ankle = toe + footRot * (ankle0[s] - toe0[s]);
                Vector3 hip = hullAt + tilt * (hip0[s] - hullPivot);
                // two bones, the knee forward
                Vector3 d = ankle - hip; float dist = d.magnitude; if (dist < 1e-4f) continue;
                Vector3 n = d / dist;
                float reach = Mathf.Clamp(dist, Mathf.Abs(l1[s] - l2[s]) + 1e-3f, l1[s] + l2[s] - 1e-3f);
                float a = (l1[s] * l1[s] + reach * reach - l2[s] * l2[s]) / (2f * reach);
                float h = Mathf.Sqrt(Mathf.Max(0f, l1[s] * l1[s] - a * a));
                Vector3 pole = Vector3.forward - n * Vector3.Dot(Vector3.forward, n);
                pole = pole.sqrMagnitude > 1e-6f ? pole.normalized : Vector3.forward;
                Vector3 knee = hip + n * a + pole * h;
                var rThigh = Turn(knee0[s] - hip0[s], knee - hip);
                var rShin = Turn(ankle0[s] - knee0[s], (hip + n * reach) - knee);
                if (!shin[s].Loose) { thigh[s].T.localRotation = Quaternion.Inverse(tilt) * rThigh; }
                if (!shin[s].Loose) shin[s].T.localRotation = Quaternion.Inverse(rThigh) * rShin;
                if (!foot[s].Loose) foot[s].T.localRotation = Quaternion.Inverse(rShin) * footRot;
                along += (s == 0 ? 1f : -1f) * toe.z;
            }
            // the arms swing against the legs: a foot forward, the other side's arm forward
            for (int s = 0; s < 2; s++)
            {
                if (claw[s] == null || claw[s].Loose) continue;
                float swing = Mathf.Clamp((s == 0 ? -along : along) * StrideArms, -35f, 35f);
                claw[s].T.localRotation = Quaternion.Euler(-swing, 0f, 0f);
                if (jaw[s] != null && !jaw[s].Loose) jaw[s].T.localRotation = Quaternion.Euler(-0.4f * Mathf.Abs(swing), 0f, 0f);
            }
        }

        /// <summary>The turn that carries a bone modelled along `from` to lie along `to`, keeping its bend facing forward
        /// (a from-to rotation alone lets a nearly straight leg spin about itself).</summary>
        static Quaternion Turn(Vector3 from, Vector3 to)
        {
            if (from.sqrMagnitude < 1e-8f || to.sqrMagnitude < 1e-8f) return Quaternion.identity;
            return Frame(to) * Quaternion.Inverse(Frame(from));
        }

        static Quaternion Frame(Vector3 dir)
        {
            dir.Normalize();
            Vector3 up = Vector3.forward - dir * Vector3.Dot(Vector3.forward, dir);
            if (up.sqrMagnitude < 1e-6f) up = Vector3.up - dir * Vector3.Dot(Vector3.up, dir);
            return Quaternion.LookRotation(dir, up.normalized);
        }
    }
}
