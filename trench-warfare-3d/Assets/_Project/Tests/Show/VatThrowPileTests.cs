// Phase: V4 (code review 2026-10-04) — VATRenderer.Fallen.cs:136,159-162: AddFallen solved the thrown arc against the
// landing ground height and only then let the heap lift him (PileStep * UnitScale * min(stacked, PileMax)), so the
// landing frame snapped up 0.28-0.95 m. The arc is now solved against where he finally lies.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Units;

namespace TW.Tests
{
    public sealed class VatThrowPileTests
    {
        // [V4] the worked example: gravity 14, two men already down on his cell (PileStep 0.28, UnitScale 1 -> rest y 0.56),
        // fly.y 2, thrown from y 0. Solved against the heap's rest height the arc tops out at 2.56 and lasts 1.139265 s;
        // the old code solved against y 0 (top 2, flight 1.069045) and ended 0.56 m under him.
        [Test]
        public void The_Arc_Is_Solved_Against_Where_He_Finally_Lies()
        {
            VATRenderer.SolveThrow(0f, 0.56f, 2f, 14f, out float up, out float top, out float flight);
            Assert.AreEqual(2.56f, top, 1e-4f, "[V4] the top of the arc must clear the heap he lands on, not the bare ground");
            Assert.AreEqual(0.604743f, up, 1e-4f, "[V4] time to the top of a 2.56 m arc under gravity 14");
            Assert.AreEqual(1.139265f, flight, 1e-4f, "[V4] the whole flight: up, then down only to 0.56 m, not to 0");
        }

        // [V4] the snap itself: the last frame of the flight must be the resting place the renderer draws after it.
        [Test]
        public void The_Landing_Frame_Is_Where_He_Lies()
        {
            Vector3 from = new Vector3(10f, 0f, 20f), rest = new Vector3(13f, 0.56f, 24f);
            VATRenderer.SolveThrow(from.y, rest.y, 2f, 14f, out float up, out float top, out float flight);
            Vector3 at = VATRenderer.ThrownAt(from, rest, up, top, flight, 14f, flight);
            Assert.AreEqual(rest.y, at.y, 0.001f, "[V4] the landing frame must not be below his resting height: the next frame snapped him up");
            Assert.AreEqual(rest.x, at.x, 0.001f, "[V4] xz ends where he lies");
            Assert.AreEqual(rest.z, at.z, 0.001f, "[V4] xz ends where he lies");
        }

        // [V4] and he tops out where the solve says, so the arc still reads as a blow.
        [Test]
        public void He_Is_In_The_Air_Through_The_Flight()
        {
            VATRenderer.SolveThrow(0f, 0.56f, 2f, 14f, out float up, out float top, out float flight);
            Vector3 at = VATRenderer.ThrownAt(Vector3.zero, new Vector3(0f, 0.56f, 0f), up, top, flight, 14f, up);
            Assert.AreEqual(top, at.y, 1e-3f, "[V4] at the time to the top he is at the top");
        }
    }
}
