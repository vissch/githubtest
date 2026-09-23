// Phase: B6 (implemented) — the speaker strip above the roster bar (was the MissionDialogue stub): a portrait in a
// recessed window, the speaker's name, and a line. While a line is "being said" the portrait flaps between the
// talking frame and the line's mood frame, then settles on the mood; the strip fades out after the line's time.
// Lines queue, so a tutorial tip and a battlefield report never talk over each other. The same strip carries the
// status reports: a mood of Wounded or Critical shows the unit's hurt portrait with the report.
using System.Collections.Generic;
using UnityEngine;
using UnityEngine.UIElements;

namespace TW.UI
{
    public sealed class HudDialogue
    {
        public const float FlapHz = 7f, CharsPerSecond = 22f, MinSeconds = 3.5f, MaxQueue = 4;

        struct Line { public string Speaker, Name, Text; public Mood Mood; public float Seconds; }

        readonly VisualElement strip, portrait;
        readonly Label name, text;
        readonly Queue<Line> queue = new Queue<Line>();
        Line current; bool showing; float age, talkFor; int lastFrame = -1;

        public bool Showing => showing;
        public int Queued => queue.Count;
        public string CurrentText => showing ? current.Text : null;

        public HudDialogue(VisualElement root)
        {
            strip = root?.Q("dialogue"); portrait = root?.Q("dialogue-portrait");
            name = root?.Q<Label>("dialogue-name"); text = root?.Q<Label>("dialogue-text");
            if (strip != null) strip.style.display = DisplayStyle.None;
        }

        /// <summary>Queue a line. speaker = the art name (UnitArt), display = the plate's name (upper case).</summary>
        public void Say(string speaker, string display, string line, Mood mood = Mood.Neutral, float seconds = 0f)
        {
            if (queue.Count >= MaxQueue) queue.Dequeue();   // a flood of reports keeps the newest
            float talk = Mathf.Max(1.2f, line.Length / CharsPerSecond);
            queue.Enqueue(new Line { Speaker = speaker, Name = display, Text = line, Mood = mood, Seconds = seconds > 0f ? seconds : Mathf.Max(MinSeconds, talk + 2f) });
        }

        public void Clear() { queue.Clear(); showing = false; if (strip != null) strip.style.display = DisplayStyle.None; }

        /// <summary>Advance by real (unscaled) seconds: a paused war still lets the sergeant finish his sentence.</summary>
        public void Tick(float dt)
        {
            if (!showing)
            {
                if (queue.Count == 0) return;
                current = queue.Dequeue(); showing = true; age = 0f; lastFrame = -1;
                talkFor = Mathf.Max(1.2f, current.Text.Length / CharsPerSecond);
                if (name != null) name.text = current.Name;
                if (text != null) text.text = current.Text;
                if (strip != null) { strip.style.display = DisplayStyle.Flex; strip.EnableInClassList("is-hurt", current.Mood == Mood.Wounded || current.Mood == Mood.Critical); }
            }
            age += dt;
            // 0/1 alternate while talking, 2 = settled on the mood
            int frame = age < talkFor ? ((int)(age * FlapHz) & 1) : 2;
            if (frame != lastFrame)
            {
                lastFrame = frame;
                var m = frame == 0 ? Mood.Talking : current.Mood;
                // a hurt speaker keeps the hurt face while talking: the talking frame is the healthy one
                if (frame == 0 && (current.Mood == Mood.Wounded || current.Mood == Mood.Critical)) m = current.Mood;
                var tex = UnitArt.State(current.Speaker, m);
                if (portrait != null) portrait.style.backgroundImage = tex != null ? new StyleBackground(tex) : new StyleBackground(StyleKeyword.None);
            }
            if (age >= current.Seconds)
            {
                showing = false;
                if (queue.Count == 0 && strip != null) strip.style.display = DisplayStyle.None;
            }
        }
    }
}
