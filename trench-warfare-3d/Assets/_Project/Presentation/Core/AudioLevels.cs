// Phase: B6 (implemented) — the three volume BUSES, for whoever plays sound. Master is deliberately not here.
//
// Master is AudioListener.volume and nothing else. It used to be duplicated into a static here as well, and
// Storm multiplied by both, so thunder came out scaled by Master SQUARED — a 6 dB error at the midpoint that
// nobody caught because the case everyone tested was 0, and 0 squared is still 0. A level becomes attenuation
// in exactly one place or this happens again.
//
// The buses below are multiplied into a source's own volume by whoever plays it. That is a convention, not an
// enforcement: nothing stops a new AudioSource ignoring them. When B7 builds EventAudioRouter it should put
// these on an AudioMixer and assign outputAudioMixerGroup instead, so that obeying is cheaper than not.
namespace TW.Presentation
{
    public static class AudioLevels
    {
        // taken from the settings tree rather than typed again, so the two cannot drift apart
        public static float Ambience = GameSettings.Defaults().Audio.Ambience;
        public static float Sfx = GameSettings.Defaults().Audio.Sfx;
        public static float Music = GameSettings.Defaults().Audio.Music;
    }
}
