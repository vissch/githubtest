using UnityEngine;
using System.IO;

public static class VisualCapture
{
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
