// Phase: B2 (implemented) — installs the screen-space ink pass (Shaders/InkLines_URP.shader) on the project's URP
// renderer: creates Settings/InkLines.mat and adds a Full Screen Pass renderer feature that runs it before the
// transparents. Idempotent; menu TW/Look/Install Ink Lines. Remove the feature on TW-Renderer to switch it off.
using System.Linq;
using UnityEditor;
using UnityEngine;
using UnityEngine.Rendering.Universal;

namespace TW.Editor
{
    public static class InkLinesSetup
    {
        const string RendererPath = "Assets/_Project/Settings/TW-Renderer.asset";
        const string MaterialPath = "Assets/_Project/Settings/InkLines.mat";
        const string FeatureName = "Ink Lines";

        [MenuItem("TW/Look/Install Ink Lines")]
        public static string Install()
        {
            var renderer = AssetDatabase.LoadAssetAtPath<ScriptableRendererData>(RendererPath);
            if (renderer == null) return "no renderer at " + RendererPath;
            var shader = Shader.Find("TW/Ink Lines (URP)");
            if (shader == null) return "ink shader not found";
            var material = AssetDatabase.LoadAssetAtPath<Material>(MaterialPath);
            if (material == null) { material = new Material(shader); AssetDatabase.CreateAsset(material, MaterialPath); }
            var feature = renderer.rendererFeatures.OfType<FullScreenPassRendererFeature>().FirstOrDefault(f => f != null && f.name == FeatureName);
            bool added = feature == null;
            if (added)
            {
                feature = ScriptableObject.CreateInstance<FullScreenPassRendererFeature>();
                feature.name = FeatureName;
                AssetDatabase.AddObjectToAsset(feature, renderer);
                renderer.rendererFeatures.Add(feature);
            }
            feature.passMaterial = material;
            feature.passIndex = 0;
            feature.injectionPoint = FullScreenPassRendererFeature.InjectionPoint.BeforeRenderingTransparents;
            feature.requirements = ScriptableRenderPassInput.Depth;
            feature.fetchColorBuffer = true;
            feature.bindDepthStencilAttachment = false;
            // the renderer keeps a parallel list of local file ids; let it rebuild that from the features
            var validate = typeof(ScriptableRendererData).GetMethod("ValidateRendererFeatures", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
            validate?.Invoke(renderer, null);
            EditorUtility.SetDirty(feature); EditorUtility.SetDirty(renderer);
            AssetDatabase.SaveAssets();
            return (added ? "installed" : "updated") + " ink lines on " + RendererPath;
        }
    }
}
