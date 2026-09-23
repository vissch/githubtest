// Phase: tooling (perf pass, 2026-09-23) — every SimHost the PlayMode gate stands up runs the determinism canary.
// Single player runs one world now (owner decision, 2026-09-23), so an ordinary Play session no longer compares two
// worlds' hashes every tick. The gate still should: this turns the canary on for the whole PlayMode run (namespace
// scope), so a change that desyncs lockstep shows up as a DESYNC error in the gate instead of in a first online match.
using NUnit.Framework;
using TW.Presentation;

namespace TW.Tests
{
    [SetUpFixture]
    public class CanaryFixture
    {
        [OneTimeSetUp] public void CanaryOn() => SimHost.CanaryOverride = true;
        [OneTimeTearDown] public void CanaryOff() => SimHost.CanaryOverride = null;
    }
}
