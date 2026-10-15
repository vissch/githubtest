// Phase: code review fixes (2026-10) — [X5] at a standstill a bomb in the air filled the chunk pool: its Life=0 lump
// is never pruned by the age test (0 > 0 is false) and a fuse spark came with it every frame. [X8] the clod mesh was
// built in Start with HideAndDontSave and never destroyed. [X9] the early return for unbaked flipbooks also skipped
// the water, smoke and flash material tints, every frame.
using System.Reflection;
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.Tactical;

namespace TW.Tests
{
    public class CombatFxReviewTests
    {
        const BindingFlags Any = BindingFlags.NonPublic | BindingFlags.Instance;

        static object Call(CombatFx fx, string method) =>
            typeof(CombatFx).GetMethod(method, Any).Invoke(fx, null);

        static int ChunkCount(CombatFx fx) =>
            (int)((System.Collections.ICollection)typeof(CombatFx).GetField("chunks", Any).GetValue(fx)).Count;

        /// <summary>Put one bomb, born now and with a long flight ahead of it, into the private bombs list.</summary>
        static void PlantBomb(CombatFx fx)
        {
            var bombType = typeof(CombatFx).GetNestedType("Bomb", BindingFlags.NonPublic);
            object b = System.Activator.CreateInstance(bombType);
            bombType.GetField("From").SetValue(b, Vector3.zero);
            bombType.GetField("Velocity").SetValue(b, Vector3.up * 0.1f);
            bombType.GetField("Born").SetValue(b, SimClock.Seconds(null));
            bombType.GetField("Air").SetValue(b, 1e6f);
            bombType.GetField("Size").SetValue(b, 0.1f);
            var bombs = (System.Collections.IList)typeof(CombatFx).GetField("bombs", Any).GetValue(fx);
            bombs.Add(b);
        }

        /// <summary>[X5] With the clock standing still (Host null, so SimNow is Time.time, which does not move inside
        /// one test) a bomb in the air used to add a lump and a fuse spark every frame and the age prune never dropped
        /// either. Fails on old code (20 chunks after ten frames, and the sweep method does not exist), passes once
        /// DropSpentChunks ends the frame and the fuse spark is only made when the sim clock moved.</summary>
        [Test]
        public void A_Bomb_In_The_Air_At_A_Standstill_Does_Not_Fill_The_Pool()
        {
            var go = new GameObject("CombatFx");
            try
            {
                var fx = go.AddComponent<CombatFx>();
                PlantBomb(fx);
                for (int frame = 0; frame < 10; frame++) { Call(fx, "DrawBombs"); Call(fx, "DropSpentChunks"); }
                Assert.That(ChunkCount(fx), Is.LessThanOrEqualTo(2), "[X5] a bomb in the air at a standstill fills the chunk pool");
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>[X5] The same ten frames with no sweep at all: at most the ten lumps plus the one fuse spark of the
        /// first frame, because the sim clock does not move. Old code: two a frame, so 20.</summary>
        [Test]
        public void A_Paused_Bomb_Sputters_No_New_Sparks()
        {
            var go = new GameObject("CombatFx");
            try
            {
                var fx = go.AddComponent<CombatFx>();
                PlantBomb(fx);
                for (int frame = 0; frame < 10; frame++) Call(fx, "DrawBombs");
                Assert.That(ChunkCount(fx), Is.LessThanOrEqualTo(11), "[X5] a paused bomb sputters a new fuse spark every frame");
            }
            finally { Object.DestroyImmediate(go); }
        }

        /// <summary>[X9] The water, smoke and flash materials are the biome's whether or not the flipbooks are baked.
        /// Fails on old code (the early return leaves them at the colour the test painted), passes after the reorder.
        /// The tint epoch must still be behind, so ApplyTints runs again when the books arrive.</summary>
        [Test]
        public void The_Water_Smoke_And_Flash_Tints_Are_Painted_Without_The_Books()
        {
            var go = new GameObject("CombatFx");
            Material water = null, smoke = null, flash = null;
            try
            {
                var fx = go.AddComponent<CombatFx>();
                var shader = Shader.Find("Universal Render Pipeline/Unlit");
                water = new Material(shader) { color = Color.magenta };
                smoke = new Material(shader) { color = Color.magenta };
                flash = new Material(shader) { color = Color.magenta };
                typeof(CombatFx).GetField("waterMat", Any).SetValue(fx, water);
                typeof(CombatFx).GetField("smokeMat", Any).SetValue(fx, smoke);
                typeof(CombatFx).GetField("flashMat", Any).SetValue(fx, flash);
                typeof(CombatFx).GetField("books", Any).SetValue(fx, null);
                typeof(CombatFx).GetField("tintEpoch", Any).SetValue(fx, -1);

                Call(fx, "ApplyTints");

                var t = SceneTints.Now;
                Assert.AreEqual(t.Splash, water.color, "[X9] the water material kept its old tint while the books were unbaked");
                Assert.AreEqual(new Color(t.Smoke.r, t.Smoke.g, t.Smoke.b, 0.36f), smoke.color, "[X9] the smoke material kept its old tint while the books were unbaked");
                Assert.AreNotEqual(Color.magenta, flash.color, "[X9] the flash material kept its old tint while the books were unbaked");
                Assert.AreEqual(-1, (int)typeof(CombatFx).GetField("tintEpoch", Any).GetValue(fx), "[X9] the tint epoch was stamped although the books are not painted yet");
            }
            finally
            {
                if (water != null) Object.DestroyImmediate(water);
                if (smoke != null) Object.DestroyImmediate(smoke);
                if (flash != null) Object.DestroyImmediate(flash);
                Object.DestroyImmediate(go);
            }
        }

        /// <summary>[X8] The clod mesh is built with HideAndDontSave and so outlives the component unless it is freed.
        /// Fails on old code (there is no FreeMeshes, and OnDestroy never touched clod), passes once FreeMeshes frees
        /// every mesh the component builds.</summary>
        [Test]
        public void The_Clod_Mesh_Is_Freed_With_The_Rest()
        {
            var go = new GameObject("CombatFx");
            Mesh clod = null;
            try
            {
                var fx = go.AddComponent<CombatFx>();
                var clodField = typeof(CombatFx).GetField("clod", Any);
                clod = new Mesh { name = "Clod", hideFlags = HideFlags.HideAndDontSave };
                clodField.SetValue(fx, clod);

                var free = typeof(CombatFx).GetMethod("FreeMeshes", Any);
                Assert.IsNotNull(free, "[X8] CombatFx must free the clod mesh it builds");
                free.Invoke(fx, null);

                Assert.IsNull(clodField.GetValue(fx), "[X8] the clod mesh was left behind");
                clod = null;
            }
            finally
            {
                if (clod != null) Object.DestroyImmediate(clod);
                Object.DestroyImmediate(go);
            }
        }
    }
}
