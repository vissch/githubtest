// Phase: review 3 (2026-10-15) — the rocket trail's break-up. Each point of a ribbon keeps its own share of the alpha so
// the smoke looks puffy and does not flicker; that share was keyed on the point's place in the list, which shifts every
// time the oldest point expires off the front. The rig is the renderer itself: a Ribbon made by reflection, put in the
// private ribbons list and stepped through RibbonsFrame as LateUpdate does, read back off the private colour list.
using System;
using System.Collections.Generic;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class SalvoTrailTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        static object Field(object o, string name)
        {
            var f = o.GetType().GetField(name, Any);
            Assert.NotNull(f, o.GetType().Name + " has no field " + name);
            return f.GetValue(o);
        }

        /// <summary>[K6] the oldest points expiring must not move the break-up pattern along the ribbon: a point keeps
        /// its own lump for its whole life. The finding, not a run, says what is right here.</summary>
        [Test]
        public void A_Points_Break_Up_Share_Does_Not_Jump_When_The_Oldest_Points_Expire()
        {
            var go = new GameObject("salvo trail rig");
            try
            {
                var r = go.AddComponent<TankRenderer>();
                var type = r.GetType();
                var ribbonType = type.GetNestedType("Ribbon", BindingFlags.NonPublic);
                Assert.NotNull(ribbonType, "TankRenderer.Ribbon");
                object rb = Activator.CreateInstance(ribbonType, true);

                // six points two metres apart, laid 0.7 s apart; its rocket is down, so the last point is drawn too
                const float Life = 4f, Alpha = 0.3f;
                float now = 100f;
                var times = new float[6];
                var pp = (List<Vector3>)Field(rb, "P");
                var tt = (List<float>)Field(rb, "T");
                for (int k = 0; k < 6; k++)
                {
                    times[k] = now - (3.5f - 0.7f * k);   // 96.5..100: the oldest is 3.5 s old, so none has expired yet
                    pp.Add(new Vector3(2f * k, 12f, 0f)); tt.Add(times[k]);
                }
                ribbonType.GetField("Live", Any).SetValue(rb, false);
                var ribbons = (System.Collections.IList)Field(r, "ribbons");
                ribbons.Add(rb);

                var frame = type.GetMethod("RibbonsFrame", Any);
                Assert.NotNull(frame, "TankRenderer.RibbonsFrame");

                frame.Invoke(r, new object[] { now });
                var before = Lumps((List<Color>)Field(r, "rC"), times, 0, now, Life, Alpha);
                Assert.AreEqual(6, before.Length, "[K6] six points drawn on the first frame");

                // 1.5 s on: the first two points are past Life and come off the front of the list
                float later = now + 1.5f;
                frame.Invoke(r, new object[] { later });
                var after = Lumps((List<Color>)Field(r, "rC"), times, 2, later, Life, Alpha);
                Assert.AreEqual(4, after.Length, "[K6] the two oldest points expired, four are left");

                for (int k = 0; k < after.Length; k++)
                    Assert.AreEqual(before[k + 2], after[k], 0.002f,
                        $"[K6] point {k + 2}'s break-up share jumped when the oldest points expired");
            }
            finally { UnityEngine.Object.DestroyImmediate(go); }
        }

        /// <summary>Each drawn point's lump, undone out of the middle vertex's alpha (the two edges are clear).</summary>
        static float[] Lumps(List<Color> rC, float[] times, int firstPoint, float now, float life, float alpha)
        {
            int n = rC.Count / 3;
            var lumps = new float[n];
            for (int k = 0; k < n; k++)
            {
                float age = Mathf.Clamp01((now - times[firstPoint + k]) / life);
                lumps[k] = rC[3 * k + 1].a / (alpha * (1f - age));
            }
            return lumps;
        }
    }
}
