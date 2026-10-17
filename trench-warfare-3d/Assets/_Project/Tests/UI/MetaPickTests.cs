// Phase: B6 (implemented) — MetaPick's collider naming and MetaKeyLight's save/restore, the two helpers shared by
// the diorama and strategic map views. No scene, no Mouse, no MonoBehaviour.
using NUnit.Framework;
using UnityEngine;
using TW.Presentation.Meta;

namespace TW.Tests
{
    public class MetaPickTests
    {
        GameObject go;
        Light light;

        [SetUp]
        public void SetUp()
        {
            go = new GameObject("key");
            light = go.AddComponent<Light>();
        }

        [TearDown]
        public void TearDown()
        {
            if (go != null) Object.DestroyImmediate(go);
        }

        [Test]
        public void NameFor_And_IdOf_Round_Trip()
        {
            Assert.AreEqual("pick:alpha", MetaPick.NameFor("alpha"));
            Assert.AreEqual("alpha", MetaPick.IdOf("pick:alpha"));
        }

        [Test]
        public void IdOf_Is_Null_For_A_Non_Pick_Collider()
        {
            Assert.IsNull(MetaPick.IdOf(""));
            Assert.IsNull(MetaPick.IdOf("plate"));
            Assert.IsNull(MetaPick.IdOf(null));
        }

        [Test]
        public void Take_Saves_Then_ReTints_On_A_Second_Take()
        {
            light.color = Color.red; light.intensity = 2f;
            var keyLight = new MetaKeyLight();

            keyLight.Take(light, Color.green, 0.5f);
            Assert.AreEqual(Color.green, light.color);
            Assert.AreEqual(0.5f, light.intensity);
            Assert.IsTrue(keyLight.Held);

            keyLight.Take(light, Color.blue, 0.8f);
            Assert.AreEqual(Color.blue, light.color);
            Assert.AreEqual(0.8f, light.intensity);

            keyLight.Release();
            Assert.AreEqual(Color.red, light.color);
            Assert.AreEqual(2f, light.intensity);
            Assert.IsFalse(keyLight.Held);
        }
    }
}
