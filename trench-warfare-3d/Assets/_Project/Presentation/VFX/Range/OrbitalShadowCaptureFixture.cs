// Phase: CelFx orbital shadow lab prototype (2026-11-14). Range option orbitalshadows=1.
using System;
using System.IO;
using UnityEngine;
using TW.Presentation.VFX.Cel;

namespace TW.Presentation.VFX
{
    public sealed class OrbitalShadowCaptureFixture : MonoBehaviour
    {
        Camera owner;
        public void Configure(Camera camera)
        { owner = camera; OrbitalShadowDepth.Configure(camera, true); }
        [Serializable] sealed class Report
        {
            public string status, failure;
            public int resolution, shadowedStrikeLimit, renderedCameras, peakCasters, peakProps, peakDraws, displacedStrikes;
            public bool gpuMillisecondsMeasured = false, shaderOrientationValidated = false;
        }
        public bool TryWriteReport(string directory)
        {
            bool ok = OrbitalShadowDepth.Failure == null && OrbitalShadowDepth.RenderedCameras > 0
                && OrbitalShadowDepth.PeakProps > 0;
            var report = new Report { status = ok ? "rendered_unvalidated" : "failed",
                failure = OrbitalShadowDepth.Failure, resolution = 256, shadowedStrikeLimit = 1,
                renderedCameras = OrbitalShadowDepth.RenderedCameras, peakCasters = OrbitalShadowDepth.PeakCasters,
                peakProps = OrbitalShadowDepth.PeakProps, peakDraws = OrbitalShadowDepth.PeakDraws,
                displacedStrikes = OrbitalShadowDepth.DisplacedStrikes };
            File.WriteAllText(Path.Combine(directory, "orbital_shadow_probe.json"), JsonUtility.ToJson(report, true));
            if (!ok) Debug.LogError("Requested orbital shadows produced no complete dummy+prop shadow pass.");
            return ok;
        }
        public void Release() { OrbitalShadowDepth.Disable(owner); owner = null; }
        void OnDisable() => Release();
        void OnDestroy() => Release();
    }
}
