using UnityEngine;
using System.IO;

public static class VisualCapture
{
    public static string SurfaceReport()
    {
        var view = Object.FindFirstObjectByType<TW.Presentation.Terrain.GreyboxTerrainView>();
        var surface = view.Surface;
        var watch = System.Diagnostics.Stopwatch.StartNew(); surface.RefreshHollows(); watch.Stop();
        int links = 0;
        foreach (var edge in surface.Edges) if (edge.Link) links++;
        var props = Object.FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
        string sites = "";
        foreach (var site in props.Sites) sites += $"; {site.Blueprint.Name} trench={site.Trench} at={site.Position}";
        return $"Boundary edges={surface.Edges.Count}, link edges={links}, classified hollows={surface.Hollows.Count}, hollow refresh CPU={watch.Elapsed.TotalMilliseconds:F2}ms (this machine only), compositions={props.CompositionCount}{sites}";
    }

    public static string PrepareArt()
    {
        PrepareStress();
        var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        host.StressUnits = 24; host.PeerDeployEveryTicks = 100000; host.TimeScale = 4f;
        return "Unsaved art scene: 24 per side. Reopen the scene after review.";
    }
    public static string PrepareStress()
    {
        if (Application.isPlaying) return "Stop Play before preparing stress.";
        var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        host.StressUnits = 1500; host.StressAdvanceDelayTicks = 100000; host.TimeScale = 8f;
        host.ScriptedPeer = true; host.PeerAttacks = false; host.PeerUsesSupport = false; host.BombardmentPerMinute = 0f;
        return "Unsaved stress scene: 1500 per side, no advance/bombardment. Reopen the scene after review.";
    }

    public static string Report()
    {
        var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        var units = Object.FindFirstObjectByType<TW.Presentation.Units.VATRenderer>();
        var props = Object.FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
        int alive = 0;
        for (int i = 0; i < host.Local.World.HighWater; i++) if ((host.Local.World.Flags[i] & (uint)TW.Sim.UnitFlags.Alive) != 0) alive++;
        return $"Alive={alive}, desync={host.Desync}, drawn={units.DrawnInfantry}, near={units.DrawnNear}, far={units.DrawnFar}, unit vertices including shadow={units.VerticesThisFrame}, unit shadows={units.ShadowsThisFrame}, prop base vertices={props.SubmittedVertices}, prop submissions={props.DrawCalls}";
    }

    public static string Effects()
    {
        var fx = Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
        var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        var method = typeof(TW.Presentation.Tactical.CombatFx).GetMethod("OnSimEvent", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic);
        // Presentation-only synthetic explosions, never inserted into the simulation event stream.
        foreach (var p in new[] { new Unity.Mathematics.float3(37f, 0f, 43f), new Unity.Mathematics.float3(53f, 0f, 48f) })
            method.Invoke(fx, new object[] { new TW.Sim.SimEvent { Type = TW.Sim.SimEventType.Explosion, Pos = p, Scalar = 5f } });
        Time.timeScale = 0.01f;
        return "Two presentation-only explosions staged; real-time scale slowed for capture.";
    }

    public static async System.Threading.Tasks.Task<string> EffectsPeak()
    {
        Effects(); Time.timeScale = 1f;
        await System.Threading.Tasks.Task.Delay(180);
        Time.timeScale = 0f;
        var fx = Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
        var host = Object.FindFirstObjectByType<TW.Presentation.SimHost>();
        int nearest = -1, enemy = -1; float distance = float.MaxValue;
        for (int i = 0; i < host.Local.World.HighWater; i++)
        {
            if (!host.Local.World.IsAlive(i)) continue;
            if (host.Local.World.Team[i] != 0) { if (enemy < 0) enemy = i; continue; }
            float d = Unity.Mathematics.math.distancesq(host.Local.World.Position[i], new Unity.Mathematics.float3(38f, 0f, 36f));
            if (d < distance) { nearest = i; distance = d; }
        }
        if (nearest >= 0 && enemy >= 0)
            typeof(TW.Presentation.Tactical.CombatFx).GetMethod("OnSimEvent", System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic)
                .Invoke(fx, new object[] { new TW.Sim.SimEvent { Type = TW.Sim.SimEventType.Shot, Pos = host.Local.World.Position[nearest], A = nearest, B = enemy } });
        return "Plumes frozen at 180ms with a fresh muzzle flash; stop Play to restore normal time.";
    }

    public static string Hud()
    {
        Directory.CreateDirectory("Temp/VisualReview");
        ScreenCapture.CaptureScreenshot("Temp/VisualReview/hud.png");
        return "HUD capture scheduled for end of frame.";
    }

    public static async System.Threading.Tasks.Task<string> VerifyEffectLifetimes()
    {
        var fx = Object.FindFirstObjectByType<TW.Presentation.Tactical.CombatFx>();
        int Count(string field) => ((System.Collections.ICollection)typeof(TW.Presentation.Tactical.CombatFx)
            .GetField(field, System.Reflection.BindingFlags.Instance | System.Reflection.BindingFlags.NonPublic).GetValue(fx)).Count;
        Time.timeScale = 1f;
        await System.Threading.Tasks.Task.Delay(1100);
        int shortEffects = Count("bursts") + Count("flashes") + Count("tracers");
        await System.Threading.Tasks.Task.Delay(6100);
        int debris = Count("chunks");
        if (shortEffects != 0 || debris != 0) throw new System.Exception($"Expired effects retained: short={shortEffects}, debris={debris}");
        return "PASS: bursts/flashes/tracers cleared after 1.1s; all debris and smoke cleared after 7.2s.";
    }

    public static string Stage()
    {
        var cam = Camera.main;
        cam.GetComponent<TW.Presentation.Tactical.TacticalCamera>().enabled = false;
        Object.FindFirstObjectByType<TW.Presentation.SimHost>().TimeScale = 0f;
        cam.fieldOfView = 25f;
        cam.transform.rotation = Quaternion.Euler(25f, 291f, 0f);
        cam.transform.position = new Vector3(38f, 0f, 34f) - cam.transform.forward * (30f * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(12.5f * Mathf.Deg2Rad));
        return "Standard camera staged; simulation paused for reproducible art review.";
    }
    public static string Standard()
    {
        var cam = Camera.main;
        var position = cam.transform.position;
        var rotation = cam.transform.rotation;
        float fov = cam.fieldOfView;
        cam.fieldOfView = 25f;
        cam.transform.rotation = Quaternion.Euler(25f, 291f, 0f);
        cam.transform.position = new Vector3(38f, 0f, 34f) - cam.transform.forward * (30f * Mathf.Tan(30f * Mathf.Deg2Rad) / Mathf.Tan(12.5f * Mathf.Deg2Rad));
        var previous = cam.targetTexture;
        var active = RenderTexture.active;
        var rt = RenderTexture.GetTemporary(1600, 900, 24);
        var tex = new Texture2D(1600, 900, TextureFormat.RGB24, false);
        try
        {
            cam.targetTexture = rt;
            cam.Render();
            RenderTexture.active = rt;
            tex.ReadPixels(new Rect(0, 0, 1600, 900), 0, 0);
            tex.Apply();
            Directory.CreateDirectory("Temp/VisualReview");
            File.WriteAllBytes("Temp/VisualReview/standard.png", tex.EncodeToPNG());
            var props = Object.FindFirstObjectByType<TW.Presentation.Terrain.BattlefieldProps>();
            return $"Temp/VisualReview/standard.png; camera={cam.transform.position}, angles={cam.transform.eulerAngles}, fov={cam.fieldOfView}; GPU={SystemInfo.graphicsDeviceName}; environment instances={props.VisibleInstances}, vertices={props.SubmittedVertices}, batches={props.DrawCalls}";
        }
        finally
        {
            cam.targetTexture = previous;
            cam.transform.SetPositionAndRotation(position, rotation);
            cam.fieldOfView = fov;
            RenderTexture.active = active;
            RenderTexture.ReleaseTemporary(rt);
            Object.DestroyImmediate(tex);
        }
    }
}
