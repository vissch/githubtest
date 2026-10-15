// Phase: infantry-firing (2026-10-15) — the tracer starts at the gun. The review of the units in the game (2026-10-12)
// read the rounds as "orange shapes over empty ground with no shooter behind them": TracerLook centred every layer on a
// point travelling from muzzle to target, so at k = 0 a streak 1.15 x its own length long had its tail 0.075 of that
// length BEHIND the muzzle, and CombatFx then multiplied in Matrix4x4.Scale(1, 1, streak), which stretched the bar about
// that same centre — a machinegunner's tracer round (Streak 1.2) began about a metre behind his gun and the whole 7 m bar
// existed on its first frame. Every shape is now anchored at `from`. These are the pure-function claims of that: the
// drawn box's tail is never behind the gun, at k = 0 it is AT the gun, and no layer is drawn past the target.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class TracerMuzzleTests
    {
        static readonly Vector3 From = new Vector3(12f, 1.3f, 40f);
        // the weapon's own streak scale (CombatFx reads it off the tracer): an SMG's short round, a rifle's, the MG's long
        // tracer round (ArmsLook)
        static readonly float[] Scales = { 0.5f, 1f, 1.2f };

        /// <summary>Where the drawn box begins and ends along the shot, measured from the muzzle.</summary>
        static (float tail, float head) Span(Matrix4x4 m, Vector3 along)
        {
            Vector3 c = m.GetColumn(3);
            float mid = Vector3.Dot(c - From, along), half = m.lossyScale.z * 0.5f;
            return (mid - half, mid + half);
        }

        [Test]
        public void NoTracerIsEverDrawnBehindTheGunOrPastTheTarget()
        {
            foreach (var to in new[] { new Vector3(80f, 1.1f, 95f), new Vector3(15f, 1.2f, 44f), new Vector3(12.6f, 1.3f, 40.5f) })
            foreach (float scale in Scales)
            foreach (float shape in new[] { 0f, 0.5f, 1f })
            foreach (float k in new[] { 0f, 0.2f, 1f })
            foreach (bool night in new[] { false, true })
            for (int side = 0; side < (night ? 3 : 1); side++)
            {
                Vector3 d = to - From, along = d.normalized; float len = d.magnitude;
                var (tail, head) = Span(TracerLook.Matrix(From, d, len, k, night, side, 0f, shape, scale), along);
                string what = $"scale {scale}, shape {shape}, k {k}, night {night}, side {side}, len {len:0.00}";
                Assert.GreaterOrEqual(tail, -0.02f, "behind the gun: " + what);
                Assert.LessOrEqual(head, len + 1e-3f, "past the target: " + what);
                Assert.Greater(head - tail, 0f, "nothing to see: " + what);
            }
        }

        [Test]
        public void OnItsFirstFrameTheRoundIsAtTheMuzzle()
        {
            // 0.10 m is well inside the gun's own length: the rifle's muzzle is 0.875 m ahead of the grip (VATBaker), so a
            // tail this close reads as leaving the barrel. Before, a day round was 0.45 m behind it and the MG's tracer
            // round about 1.1 m.
            // the round itself: the day streak, and by night the white-hot core. The night halo above shape 0 is C104's
            // head glow, a short bright box hung on the core's tip, so its own tail sits at the core's front by design
            // (KnobsTests guards that); what matters for it is that it is never behind the gun, which the test above says.
            foreach (var to in new[] { new Vector3(80f, 1.1f, 95f), new Vector3(15f, 1.2f, 44f) })
            foreach (float scale in Scales)
            foreach (float shape in new[] { 0f, 0.5f, 1f })
            foreach (bool night in new[] { false, true })
            {
                int side = night ? 2 : 0;
                Vector3 d = to - From, along = d.normalized; float len = d.magnitude;
                var (tail, _) = Span(TracerLook.Matrix(From, d, len, 0f, night, side, 0f, shape, scale), along);
                Assert.That(Mathf.Abs(tail), Is.LessThanOrEqualTo(0.10f),
                            $"the round starts {tail:0.00} m from the muzzle: scale {scale}, shape {shape}, night {night}, side {side}");
                // and the old wide halo is the whole round, so by day and at shape 0 it starts at the gun too
                if (night && shape == 0f)
                {
                    var (haloTail, _) = Span(TracerLook.Matrix(From, d, len, 0f, true, 0, 0f, 0f, scale), along);
                    Assert.That(Mathf.Abs(haloTail), Is.LessThanOrEqualTo(0.10f), $"the old halo starts {haloTail:0.00} m from the muzzle");
                }
            }
        }

        [Test]
        public void ALongerRoundIsALongerBarAndTheRoundTravels()
        {
            Vector3 d = new Vector3(80f, 1.1f, 95f) - From, along = d.normalized; float len = d.magnitude;
            foreach (float shape in new[] { 0f, 1f })
            {
                float shortRound = TracerLook.Matrix(From, d, len, 0f, true, 2, 0f, shape, 0.5f).lossyScale.z;
                float longRound = TracerLook.Matrix(From, d, len, 0f, true, 2, 0f, shape, 1.2f).lossyScale.z;
                Assert.Greater(longRound, shortRound * 2f, "the MG's tracer round is the longer bar (shape " + shape + ")");
                // the same round later in its life is further down the shot, and its length has not changed
                var early = Span(TracerLook.Matrix(From, d, len, 0f, true, 2, 0f, shape, 1f), along);
                var late = Span(TracerLook.Matrix(From, d, len, 0.6f, true, 2, 0f, shape, 1f), along);
                Assert.Greater(late.tail, early.tail + 1f, "it travels (shape " + shape + ")");
                Assert.AreEqual(early.head - early.tail, late.head - late.tail, 1e-3f, "and keeps its length (shape " + shape + ")");
            }
        }
    }
}
