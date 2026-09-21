// Phase: B3 (implemented)
// What the unit renderer needs from the camera, kept in Core so TW.Presentation.Units does not reference the camera
// assembly: the width of ground in view, which picks the render tier.
namespace TW.Presentation
{
    public interface IZoomSource
    {
        float CurrentZoom { get; }
    }
}
