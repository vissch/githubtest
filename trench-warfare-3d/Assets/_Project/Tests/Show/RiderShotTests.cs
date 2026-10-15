// Phase: review 2 (2026-10-15) — a rider's shot when the VAT renderer cannot say where his muzzle is. MuzzleOf leaves
// the muzzle at the man's drawn position and returns false (no clip atlas, no sockets); the flash and the tracer must
// then not fire at all, rather than from whatever MuzzleOf left behind. Driven through TankRenderer.Emit by reflection,
// with a bare VATRenderer (its clip tables answer SecondsFor and Loops without an atlas).
using System;
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;
using TW.Presentation.Units;

namespace TW.Tests
{
    public class RiderShotTests
    {
        const BindingFlags Any = BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic;

        /// <summary>[K9] a failed MuzzleOf fires nothing: no flash, no shot mark, no tracer (they came off the world origin).</summary>
        [Test]
        public void A_Shot_Whose_Muzzle_Is_Unknown_Fires_No_Flash_And_No_Tracer()
        {
            var go = new GameObject("rider shot rig");
            var vatGo = new GameObject("vat");
            var wasFlash = SceneHooks.Flash;
            try
            {
                var r = go.AddComponent<TankRenderer>();   // no Host, no FlipbookFx, no CombatFx
                var vat = vatGo.AddComponent<VATRenderer>();
                r.GetType().GetField("vat", Any).SetValue(r, vat);

                int flashes = 0;
                SceneHooks.Flash = (at, c, a, b, d) => flashes++;

                var riderType = r.GetType().GetNestedType("Rider", BindingFlags.Public);
                Assert.NotNull(riderType, "TankRenderer.Rider");
                object man = Activator.CreateInstance(riderType);
                riderType.GetField("State", Any).SetValue(man, TankRenderer.RiderState.Seated);
                riderType.GetField("Pos", Any).SetValue(man, new Vector3(12f, 3f, -7f));
                riderType.GetField("ShotAt", Any).SetValue(man, 0f);
                riderType.GetField("Flashed", Any).SetValue(man, false);
                riderType.GetField("Aim", Any).SetValue(man, -1);

                var emit = r.GetType().GetMethod("Emit", Any);
                Assert.NotNull(emit, "TankRenderer.Emit");
                object[] args = { man, 10f, 1f, 0 };
                emit.Invoke(r, args);

                Assert.IsTrue((bool)riderType.GetField("Flashed", Any).GetValue(man), "[K9] the shot was not spent");
                Assert.AreEqual(0, flashes, "[K9] a flash fired although the muzzle is unknown");
                Assert.AreEqual(0, r.RiderShots, "[K9] a shot was counted although the muzzle is unknown");
                var marks = (System.Collections.ICollection)r.GetType().GetField("shotMarks", Any).GetValue(r);
                Assert.AreEqual(0, marks.Count, "[K9] a shot mark was left although the muzzle is unknown");
            }
            finally
            {
                SceneHooks.Flash = wasFlash;
                UnityEngine.Object.DestroyImmediate(go);
                UnityEngine.Object.DestroyImmediate(vatGo);
            }
        }
    }
}
