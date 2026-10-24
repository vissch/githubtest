// Phase: VFX pass (the flame-jet seam, round 7, 2026-10-24) - FlipbookFx.Spied/SpiedCards, the capture switch that
// hands a still's harness the renderer's own answer to "which pixels are this card": a per-book record in Pack's
// own packed form, not a guess about colour. CaptureRig.CardMask turns that record into a per-card stencil with the
// shader's own billboard math (Flipbook_URP.shader:87-99).
//
// What is asserted here is the contract of the new branch - a spied book's card is recorded, an unspied book's is
// not, hidden wins over spied (a dropped card never exists to spy on), the switch is off in the game - and that
// CardMask's area matches the camera's own lens, not a guess about scale.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Editor;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class FlipbookSpyTests
    {
        static FlipbookFx NewFx(out Material[] mats)
        {
            var fx = new FlipbookFx();
            mats = (Material[])typeof(FlipbookFx).GetField("mats", BindingFlags.NonPublic | BindingFlags.Instance).GetValue(fx);
            return fx;
        }

        static void Dispose(Material[] mats)
        {
            foreach (var m in mats) if (m != null) Object.DestroyImmediate(m);   // DestroyImmediate: Dispose's Object.Destroy is not allowed in edit mode
        }

        [Test]
        public void ASpiedBooksCardIsRecorded_AndAnUnspiedBooksIsNot()
        {
            var fx = NewFx(out var mats);
            ulong was = FlipbookFx.Spied;
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");

                FlipbookFx.Spied = 1UL << (int)FlipbookFx.Book.JetCore;
                fx.Add(FlipbookFx.Book.JetCore, Vector3.zero, 2f, 1f);
                fx.Add(FlipbookFx.Book.Pool, Vector3.zero, 2f, 1f);
                FlipbookFx.SpiedCards.Clear();
                fx.Draw(Time.time + 0.01f, new Bounds(Vector3.zero, Vector3.one * 1000f));

                Assert.AreEqual(1, FlipbookFx.SpiedCards.Count, "only the spied book's card is recorded");
            }
            finally
            {
                FlipbookFx.Spied = was;
                FlipbookFx.SpiedCards.Clear();
                Dispose(mats);
            }
        }

        [Test]
        public void AHiddenAndSpiedBookIsNeverRecorded()
        {
            var fx = NewFx(out var mats);
            ulong wasHidden = FlipbookFx.Hidden, wasSpied = FlipbookFx.Spied;
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");

                FlipbookFx.Hidden = 1UL << (int)FlipbookFx.Book.JetCore;
                FlipbookFx.Spied = 1UL << (int)FlipbookFx.Book.JetCore;
                fx.Add(FlipbookFx.Book.JetCore, Vector3.zero, 2f, 1f);   // dropped in Add before it exists: Hidden wins
                FlipbookFx.SpiedCards.Clear();
                fx.Draw(Time.time + 0.01f, new Bounds(Vector3.zero, Vector3.one * 1000f));

                Assert.AreEqual(0, FlipbookFx.SpiedCards.Count, "a hidden card is never stored, so there is nothing left to spy on");
            }
            finally
            {
                FlipbookFx.Hidden = wasHidden;
                FlipbookFx.Spied = wasSpied;
                FlipbookFx.SpiedCards.Clear();
                Dispose(mats);
            }
        }

        [Test]
        public void SpiedZeroLeavesTheListEmpty()
        {
            var fx = NewFx(out var mats);
            ulong was = FlipbookFx.Spied;
            try
            {
                Assume.That(fx.Ready, "the flipbook shader and sheets load in the editor");

                FlipbookFx.Spied = 0UL;
                fx.Add(FlipbookFx.Book.JetCore, Vector3.zero, 2f, 1f);
                FlipbookFx.SpiedCards.Clear();
                fx.Draw(Time.time + 0.01f, new Bounds(Vector3.zero, Vector3.one * 1000f));

                Assert.AreEqual(0, FlipbookFx.SpiedCards.Count, "Spied == 0 in the game: nothing is recorded");
            }
            finally
            {
                FlipbookFx.Spied = was;
                FlipbookFx.SpiedCards.Clear();
                Dispose(mats);
            }
        }

        [Test]
        public void CardMaskCoversTheAreaTheLensSays()
        {
            var go = new GameObject("spy test camera");
            try
            {
                var cam = go.AddComponent<Camera>();
                cam.orthographic = false;
                cam.fieldOfView = 60f;
                cam.transform.SetPositionAndRotation(new Vector3(0f, 0f, -10f), Quaternion.identity);   // looking down +z

                var card = FlipbookFx.Pack(Vector3.zero, 2f, 1f, 0f, 1f, 1f, 0f, FlipbookFx.Kind.None);
                int w = 1600, h = 900;
                var mask = CaptureRig.CardMask(new[] { card }, cam, w, h);

                int covered = 0;
                for (int i = 0; i < mask.Length; i++) if (mask[i] != 0) covered++;

                // the half-height at 10 m is 10*tan(30deg); px/m follows from spreading that over half the image
                float pxPerM = (h * 0.5f) / (10f * Mathf.Tan(30f * Mathf.Deg2Rad));
                float expected = 2f * pxPerM * (1f * pxPerM);
                Assert.That(covered, Is.EqualTo(expected).Within(expected * 0.02f),
                            "CardMask's area comes from the lens (fieldOfView, distance), not from a guess about scale");
            }
            finally
            {
                Object.DestroyImmediate(go);
            }
        }
    }
}
