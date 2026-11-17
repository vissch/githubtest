// Phase: CelFx flamethrower late-ember radius contract (2026-11-17).
using NUnit.Framework;
using UnityEngine;
using TW.Presentation;
using TW.Presentation.VFX.Cel;
namespace TW.Tests
{
    public sealed class FlameEmberCapTests
    {
        [TestCase(FxTier.High, 159, 36, 9, 158.98002f)]
        [TestCase(FxTier.Epic, 161, 45, 8, 160.72265f)]
        public void ActualMake_Seed1917_UsesEffectiveCountAndDocumentedRejoin(FxTier tier, int protectedFrame, int expectedCount, int expectedLive, float expectedCross)
        {
            var recipe = FlamethrowerRecipe.Recipe;
            int index = recipe.Layers.FindIndex(l => l.Name == "late embers");
            Assert.That(index, Is.EqualTo(27), "Layer-slot changes invalidate the seed guarantee");
            var layer = recipe.Layers[index];
            Assert.That(layer.Kind, Is.EqualTo("FlameEmber"));
            Assert.That(layer.Close, Is.EqualTo(36));
            Assert.That(layer.Start, Is.EqualTo(3.5f));
            Assert.That(layer.Duration, Is.EqualTo(1f));
            Assert.That(layer.Life, Is.EqualTo(2f));
            Assert.That(layer.LifeJitter, Is.EqualTo(.2f));
            Assert.That(layer.TimeJitter, Is.Zero);
            Assert.That(layer.Size.From, Is.EqualTo(.09f));
            Assert.That(layer.Size.To, Is.EqualTo(.015f));
            Assert.That(layer.Size.Pow, Is.EqualTo(2f));
            var rng = new CelRng(1917u * 31u + (uint)index * 7919u + 1u);
            int count = layer.CountAt(0f, CelFx.QualityShare(tier));
            Assert.That(count, Is.EqualTo(expectedCount));
            int liveProtected = 0; float latestCross = 0f;
            for (int j = 0; j < count; j++)
            {
                // Non-degenerate sphere/radial vectors; these values do not change RNG consumption.
                CelFx.Make(layer, j, count, Vector3.zero, Vector3.forward, Vector3.right,
                    Vector3.up, Vector3.forward * 10f, 7f / 24f, ref rng, out CelRecord r);
                float crossK = Mathf.Sqrt((r.Size.x - r.Size.x * FlameEmberKind.CapFraction) / (r.Size.x - r.Size.y));
                latestCross = Mathf.Max(latestCross, (r.Born + crossK * r.Life) * 24f);
                for (int f = protectedFrame; f < 192; f++)
                {
                    float age = f / 24f - r.Born;
                    if (age < 0f || age > r.Life) continue;
                    if (f == protectedFrame) liveProtected++;
                    float k = Mathf.Clamp01(age / Mathf.Max(r.Life, 1e-4f));
                    float original = Mathf.Lerp(r.Size.x, r.Size.y, Mathf.Pow(k, r.Size.z));
                    Assert.That(original, Is.LessThanOrEqualTo(r.Size.x * FlameEmberKind.CapFraction), "Capped survivor j=" + j + " f=" + f);
                    Assert.That(Mathf.Min(original, r.Size.x * FlameEmberKind.CapFraction), Is.EqualTo(original));
                }
            }
            Assert.That(liveProtected, Is.EqualTo(expectedLive), "Detect RNG/capture assumptions drifting");
            Assert.That(latestCross, Is.LessThan((float)protectedFrame));
            Assert.That(latestCross, Is.EqualTo(expectedCross).Within(.002f));
        }
        [Test]
        public void ShaderKeeperAndMapping_MatchLocalGlowContract()
        {
            var kind = new FlameEmberKind();
            Assert.That(kind.Order, Is.EqualTo(new GlowKind().Order));
            Assert.That(kind.Capacity, Is.EqualTo(128));
            var shader = kind.FindShader();
            Assert.That(shader, Is.Not.Null);
            var keeper = Resources.Load<Material>("ShaderKeep/KeepCelFlameEmber");
            Assert.That(keeper, Is.Not.Null);
            Assert.That(keeper.shader, Is.EqualTo(shader));

        }
    }
}
