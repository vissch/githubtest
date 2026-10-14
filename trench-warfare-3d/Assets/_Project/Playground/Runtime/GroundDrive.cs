// Phase: Playground (2026-10-14, lane/show/review-fixes) — a tracked or wheeled machine that can be driven
// Code review 2026-10-04 Y3: a machine whose tank3.json says neither "walker" nor "flyer" could not be driven at all,
// so its wheels, belts and the way it sits on the move were never scored. This is WalkerDrive's shape without the legs:
// `walk <speed>` drives it round a circle of Radius (or on the spot), each Wheel_* part turns on its axle at
// speed / radius as TankRenderer turns them (TankPartRole.Wheel, about the part's x axis), and each Track_* belt runs
// at the same rate through the "_Tread" offset the game's shader reads.
//
// Everything is worked in the machine's own build frame (where it has driven to, and its turn), never from a world
// matrix, so three copies at three LODs are driven identically (VehicleRig's rule).
using TW.Presentation.Tactical;
using UnityEngine;

namespace TW.Playground
{
    public sealed class GroundDrive : MonoBehaviour
    {
        public float Speed;                        // m/s forward (the "walk" command); it drives a circle of Radius
        public float Radius = 22f;
        public bool InPlace;                       // a rolling road: the wheels turn and it stays where it was built
        public Vector3 WalkPos { get; private set; }   // metres from where it was built, in its build frame
        public float WalkYaw { get; private set; }     // radians turned since it was built
        public float WheelTurn { get; private set; }   // radians the wheels have turned
        public float Tread { get; private set; }       // metres of belt that have run past

        VehicleRig rig;
        VehicleRig.Part[] wheels = new VehicleRig.Part[0], tracks = new VehicleRig.Part[0];
        float wheelR = 0.4f;
        Vector3 startPos; Quaternion startRot; bool started;

        public GroundDrive Init(VehicleRig r)
        {
            rig = r;
            wheels = System.Array.FindAll(r.Parts.ToArray(), p => p.Name.StartsWith("Wheel"));
            tracks = System.Array.FindAll(r.Parts.ToArray(), p => p.Name.StartsWith("Track"));
            // the rolling radius as modelled: half the wheel's height, in metres (the box is in rig units)
            if (wheels.Length > 0) wheelR = Mathf.Max(0.05f, wheels[0].Box.extents.y * r.Size);
            if (!started) { started = true; startPos = transform.position; startRot = transform.rotation; }
            return this;
        }

        /// <summary>The belt offset of this part, or 0: read by VehicleRig.Push when it fills the property block.</summary>
        public float TreadOf(VehicleRig.Part p) => p != null && p.Name.StartsWith("Track") ? Tread : 0f;

        /// <summary>One frame, after VehicleRig's pose. A machine that has lost a part or its crew stands still, as a
        /// walker does; the wheels wind down with it.</summary>
        public void Drive(float dt)
        {
            if (rig == null) return;
            bool dead = rig.State >= VehicleRig.Stage.KnockedOut;
            bool free = !dead;
            foreach (var p in rig.Parts) if (p.Loose) { free = false; break; }
            float v = free ? Speed : 0f, yawRate = InPlace ? 0f : v / Mathf.Max(1f, Radius);
            WalkYaw += yawRate * dt;
            var face = Quaternion.AngleAxis(WalkYaw * Mathf.Rad2Deg, Vector3.up);
            WalkPos += face * Vector3.forward * v * dt;
            if (free && !InPlace) transform.SetPositionAndRotation(startPos + startRot * WalkPos, startRot * face);
            // the running gear turns whether or not the machine moves (on the spot it is a rolling road)
            float roll = (free ? Speed : 0f) * dt;
            WheelTurn = Mathf.Repeat(WheelTurn + roll / wheelR, Mathf.PI * 2f);
            Tread = Mathf.Repeat(Tread + roll, 100f);
            foreach (var w in wheels)
            {
                if (w.Loose) continue;
                w.T.localRotation = w.RestRot * Quaternion.AngleAxis(WheelTurn * Mathf.Rad2Deg, Vector3.right);
            }
            foreach (var t in tracks) if (!t.Loose) t.T.localRotation = t.RestRot;   // a belt does not turn; its texture runs
        }
    }
}
