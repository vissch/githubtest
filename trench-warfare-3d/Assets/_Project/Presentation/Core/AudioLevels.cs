// Phase: B6 (implemented) — the four volume levels the settings screen sets, for whoever plays sound.
// Master goes straight to AudioListener.volume (SettingsApplier). The three buses are statics for B7's
// EventAudioRouter to multiply into its sources; today only Storm's thunder exists and may read Ambience.
namespace TW.Presentation
{
    public static class AudioLevels
    {
        public static float Master = 0f;   // muted until the player says otherwise; SettingsApplier overwrites it
        public static float Ambience = 0.8f;
        public static float Sfx = 1f;
        public static float Music = 0.8f;
    }
}
