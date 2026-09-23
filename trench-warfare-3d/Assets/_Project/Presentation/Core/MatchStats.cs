// Phase: B6 (implemented) — what the debrief says about the match, counted as it happens.
// Subscribes to the event pump and tallies per team: men and machines lost, men fielded, trenches and objectives
// taken, abilities fired, and when the match ended; the rest of the report (kills, shots, silver, duration, what is
// held now) is read from the sim at the end. Losses are counted from Death / VehicleDestroyed events with the team
// read at dispatch, which is right unless a slot died and was refilled inside one frame's ticks; SimWorld counters
// would be exact and are the follow-up once the sim owner agrees to three lines in Despawn and StepEconomy.
using System;
using UnityEngine;
using TW.Sim;

namespace TW.Presentation
{
    [Serializable]
    public sealed class MatchReport
    {
        public int Winner = -1;
        public float DurationSeconds;
        public string EndReason = "";
        public int[] MenLost = new int[2], VehiclesLost = new int[2], MenFielded = new int[2], Kills = new int[2], Shots = new int[2];
        public int[] SilverNow = new int[2], SilverStart = new int[2];
        public int[] TrenchesTaken = new int[2], TrenchesHeld = new int[2], ObjectivesTaken = new int[2], ObjectivesHeld = new int[2];
        public int[] AbilitiesFired = new int[2];
        public string Title = "", Difficulty = "", Seed = "";

        public float Accuracy(int team) => Shots[team] > 0 ? 100f * Kills[team] / Shots[team] : 0f;
        public static string Clock(float seconds) { int s = Mathf.Max(0, Mathf.FloorToInt(seconds)); return $"{s / 60:00}:{s % 60:00}"; }
    }

    public sealed class MatchStats : IDisposable
    {
        readonly SimHost host;
        readonly int[] menLost = new int[2], vehiclesLost = new int[2], fielded = new int[2], trenches = new int[2], objectives = new int[2], abilities = new int[2];
        uint endedTick; bool ended;
        readonly int[] silverStart = new int[2];

        public MatchStats(SimHost host)
        {
            this.host = host;
            var w = host.Local.World;
            silverStart[0] = w.Silver[0]; silverStart[1] = w.Silver[1];
            host.Events.OnEvent += OnEvent;
        }

        public void Dispose() { if (host != null) host.Events.OnEvent -= OnEvent; }

        /// <summary>The stats for the host's GameObject, attached once.</summary>
        public static MatchStats Attach(SimHost host)
        {
            var holder = host.GetComponent<MatchStatsHolder>() ?? host.gameObject.AddComponent<MatchStatsHolder>();
            return holder.Stats ??= new MatchStats(host);
        }

        int TeamOf(int slot)
        {
            var w = host.Local.World;
            return slot >= 0 && slot < w.Team.Length ? w.Team[slot] & 1 : 0;
        }

        void OnEvent(SimEvent ev)
        {
            switch (ev.Type)
            {
                case SimEventType.Death: menLost[TeamOf(ev.A)]++; break;
                case SimEventType.VehicleDestroyed: vehiclesLost[TeamOf(ev.A)]++; break;
                case SimEventType.UnitSpawned: fielded[TeamOf(ev.A)]++; break;
                case SimEventType.TrenchCaptured: trenches[ev.B & 1]++; break;
                case SimEventType.ObjectiveCaptured: objectives[ev.B & 1]++; break;
                case SimEventType.AbilityFired: abilities[ev.B & 1]++; break;
                case SimEventType.MatchEnded: if (!ended) { ended = true; endedTick = ev.Tick; } break;
            }
        }

        public MatchReport Report(MatchLaunch.Request request, string endReason = null)
        {
            var w = host.Local.World;
            var r = new MatchReport
            {
                Winner = w.WinnerTeam,
                DurationSeconds = (ended ? endedTick : w.Tick) * w.Config.TickSeconds,
                EndReason = endReason ?? (w.WinnerTeam < 0 ? "" : w.WinnerTeam == 0 ? "ENEMY HQ TAKEN" : "OUR HQ HAS FALLEN"),
                Title = request?.Title ?? "", Difficulty = request?.Difficulty ?? "", Seed = (request?.BattlefieldSeed ?? host.BattlefieldSeed).ToString(),
            };
            for (int t = 0; t < 2; t++)
            {
                r.MenLost[t] = menLost[t]; r.VehiclesLost[t] = vehiclesLost[t]; r.MenFielded[t] = fielded[t];
                r.TrenchesTaken[t] = trenches[t]; r.ObjectivesTaken[t] = objectives[t]; r.AbilitiesFired[t] = abilities[t];
                r.SilverNow[t] = w.Silver[t]; r.SilverStart[t] = silverStart[t];
                if (host.Local.Fire != null) { r.Kills[t] = host.Local.Fire.Kills[t]; r.Shots[t] = host.Local.Fire.Shots[t]; }
            }
            var fields = host.Local.Fields;
            for (int i = 0; i < fields.Trenches.Length; i++) { byte o = fields.Trenches[i].OwnerTeam; if (o < 2) r.TrenchesHeld[o]++; }
            var sectors = host.Local.Sectors;
            if (sectors != null && sectors.States.IsCreated)
                for (int i = 0; i < sectors.States.Length; i++) { byte o = sectors.States[i].Owner; if (o < 2) r.ObjectivesHeld[o]++; }
            return r;
        }
    }

    /// <summary>Keeps one MatchStats alive on the SimHost's GameObject for the match's lifetime.</summary>
    public sealed class MatchStatsHolder : MonoBehaviour
    {
        public MatchStats Stats;
        void OnDestroy() { Stats?.Dispose(); Stats = null; }
    }
}
