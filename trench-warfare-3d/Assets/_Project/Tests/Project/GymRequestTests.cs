// Phase: tooling (2026-11-09) - ordinary Play must not start a gym, while empty options still queue one.
using System.Reflection;
using NUnit.Framework;
using UnityEditor;
using UnityEngine;
using UnityEngine.TestTools;
using TW.Editor;

namespace TW.Tests
{
    public sealed class GymRequestTests
    {
        const string Request = "TW.Gym.Request";
        const BindingFlags PrivateStatic = BindingFlags.Static | BindingFlags.NonPublic;
        string previous;

        [SetUp]
        public void SetUp()
        {
            previous = SessionState.GetString(Request, "");
            SessionState.EraseString(Request);
        }

        [TearDown]
        public void TearDown()
        {
            SessionState.EraseString(Request);
            if (!string.IsNullOrEmpty(previous)) SessionState.SetString(Request, previous);
        }

        static object Call(string name, params object[] args)
        {
            var method = typeof(Gym).GetMethod(name, PrivateStatic);
            Assert.That(method, Is.Not.Null, name);
            return method.Invoke(null, args);
        }

        static bool Take(out string options)
        {
            var args = new object[] { null };
            bool taken = (bool)Call("TakeRequest", args);
            options = (string)args[0];
            return taken;
        }

        [Test]
        public void OrdinaryPlayDoesNotStartAGymWithoutARequest()
        {
            int before = Object.FindObjectsByType<GymRun>(FindObjectsInactive.Include, FindObjectsSortMode.None).Length;
            Call("OnPlayMode", PlayModeStateChange.EnteredPlayMode);
            Assert.That(Object.FindObjectsByType<GymRun>(FindObjectsInactive.Include, FindObjectsSortMode.None).Length, Is.EqualTo(before));
            LogAssert.NoUnexpectedReceived();
            Assert.That(Take(out _), Is.False);
        }

        [TestCase(null)]
        [TestCase("")]
        [TestCase("tabs=clips filter=Fire max=2")]
        public void RequestedOptionsSurviveAndAreConsumedOnce(string requested)
        {
            Call("QueueRequest", requested);
            Assert.That(Take(out string options), Is.True);
            Assert.That(options, Is.EqualTo(requested ?? ""));
            Assert.That(Take(out _), Is.False, "a consumed request must not start another run after a reload");
        }

        [Test]
        public void LeavingEditModeDoesNotConsumeThePendingRequest()
        {
            Call("QueueRequest", "tabs=clips max=1");
            Call("OnPlayMode", PlayModeStateChange.ExitingEditMode);
            Assert.That(Take(out string options), Is.True);
            Assert.That(options, Is.EqualTo("tabs=clips max=1"));
            LogAssert.NoUnexpectedReceived();
        }
    }
}
