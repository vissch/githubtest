// Phase: B6 (implemented) — who says what on the speaker strip. The Sergeant opens the match with two tips (the
// tutorial for now), cheers a trench taken and shouts when one is lost; a unit with a face speaks up the first time
// it is deployed; and the battlefield status reports arrive in the hurt faces: a type of ours that has lost men
// reports Wounded at LossesWounded and Critical at LossesCritical, a lost machine reports Critical, a stalled one
// Wounded. Only our side (team 0) talks. Everything is read from SimEvents, like ObjectiveTracker and MatchStats.
using System;
using TW.Presentation;
using TW.Sim;

namespace TW.UI
{
    public sealed class HudCommentary : IDisposable
    {
        public const int LossesWounded = 5, LossesCritical = 15;

        readonly SimHost host;
        readonly HudDialogue say;
        readonly bool[] introduced = new bool[32];
        readonly int[] losses = new int[32];

        public HudCommentary(SimHost host, HudDialogue dialogue, bool tips = true)
        {
            this.host = host; say = dialogue;
            if (host != null && host.Events != null) host.Events.OnEvent += OnEvent;
            if (tips)
            {
                Sergeant(HudText.TipDeploy, Mood.Talking);
                Sergeant(HudText.TipPause, Mood.Neutral);
            }
        }

        public void Dispose() { if (host != null && host.Events != null) host.Events.OnEvent -= OnEvent; }

        void Sergeant(string line, Mood mood) => say.Say(UnitArt.Narrator, HudText.SergeantName, line, mood);

        bool Ours(int slot, out byte archetype)
        {
            archetype = 0;
            var w = host != null && host.Local != null ? host.Local.World : null;
            if (w == null || slot < 0 || slot >= w.Team.Length) return false;
            archetype = w.Archetype[slot];
            return (w.Team[slot] & 1) == 0;
        }

        /// <summary>A loss of ours in that slot, and whose type fell. Events reach us once a frame: at 2x-8x a later
        /// tick may already have deployed a new man into the dead man's slot, and then the slot describes the new man,
        /// so the loss would be counted against his type. A refilled slot tells us nothing, so nothing is counted
        /// (DeathMarks.OnEvent guards the same way). Public and static so a test can reach it without a host.</summary>
        public static bool OurLoss(SimWorld w, int slot, out byte archetype)
        {
            archetype = 0;
            if (w == null || slot < 0 || slot >= w.Team.Length || w.IsAlive(slot)) return false;
            archetype = w.Archetype[slot];
            return (w.Team[slot] & 1) == 0;
        }

        bool OurLoss(int slot, out byte archetype) =>
            OurLoss(host != null && host.Local != null ? host.Local.World : null, slot, out archetype);

        /// <summary>Public so a test can feed events without a running sim.</summary>
        public void OnEvent(SimEvent ev)
        {
            switch (ev.Type)
            {
                case SimEventType.UnitSpawned:
                {
                    if (!Ours(ev.A, out byte a) || a >= introduced.Length || introduced[a]) break;
                    introduced[a] = true;
                    string art = UnitArt.NameOf(a);
                    if (UnitArt.HasFace(art)) say.Say(art, HudText.Name(a).ToUpperInvariant(), HudText.Bark(a), Mood.Shouting);
                    break;
                }
                case SimEventType.Death:
                {
                    if (!OurLoss(ev.A, out byte a) || a >= losses.Length) break;
                    int n = ++losses[a];
                    string art = UnitArt.NameOf(a);
                    if (!UnitArt.HasFace(art)) break;
                    if (n == LossesWounded) say.Say(art, HudText.Name(a).ToUpperInvariant(), HudText.LossReport(a, n, false), Mood.Wounded);
                    else if (n == LossesCritical) say.Say(art, HudText.Name(a).ToUpperInvariant(), HudText.LossReport(a, n, true), Mood.Critical);
                    break;
                }
                case SimEventType.VehicleDestroyed:
                {
                    if (!OurLoss(ev.A, out byte a)) break;
                    string art = UnitArt.NameOf(a);
                    if (UnitArt.HasFace(art)) say.Say(art, HudText.Name(a).ToUpperInvariant(), HudText.MachineLost, Mood.Critical);
                    else Sergeant(HudText.LostMachine(a), Mood.Shouting);
                    break;
                }
                case SimEventType.VehicleStalled:
                {
                    if (ev.B != 1 || !Ours(ev.A, out byte a)) break;
                    string art = UnitArt.NameOf(a);
                    if (UnitArt.HasFace(art)) say.Say(art, HudText.Name(a).ToUpperInvariant(), HudText.MachineStalled, Mood.Wounded);
                    break;
                }
                case SimEventType.TrenchCaptured:
                    if (ev.B == 0) Sergeant(HudText.TrenchTaken, Mood.Cheering); else Sergeant(HudText.TrenchLost, Mood.Shouting);
                    break;
                case SimEventType.MatchEnded:
                    Sergeant(ev.A == 0 ? HudText.WonLine : HudText.LostLine, ev.A == 0 ? Mood.Cheering : Mood.Critical);
                    break;
            }
        }
    }
}
