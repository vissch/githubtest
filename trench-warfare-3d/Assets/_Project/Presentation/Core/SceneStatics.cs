// Phase: B6 (implemented) — the statics that outlive a scene load or a Play session, put back.
//
// Two different moments, two entry points, and mixing them up breaks the game:
//
// Reset() runs on every scene load (ShellRouter's sceneLoaded, MatchLaunch, Quit to menu). A Single LoadScene
// destroys every object but not a static. The owners clear their own (SimHost.OnDestroy, VATRenderer,
// GreyboxTerrainView, TankRenderer, Storm's Thaw); these are the ones nobody owns: the debug bombardment override
// the TestPanel presets set, the engine time scale Storm's lightning may be holding, and the shell's input focus.
// It must NOT clear SceneHooks, nor HudBridge's PointerOverUi / WheelClaimed: sceneLoaded fires after the new scene's
// components have already wired theirs, and HudBootstrap builds the HUD in its own sceneLoaded handler, which Unity
// may run before or after ShellRouter's. Their owners clear them in OnDisable; ResetSession clears them too.
//
// A per-scene registration (RegisterPerScene) is for a static that caches something the scene owned: a view, a
// renderer, a mesh that the Single LoadScene destroyed. Reset() runs those at its end, so the cache is forgotten
// before any code in the new scene can hand out the dead reference (MetaServices did: the destroyed strategic map
// view read as "not null" behind its interface and threw MissingReferenceException on the first campaign DEPLOY).
//
// ResetSession() runs when no scene is live: when the editor leaves Play (Editor/PlayModeStaticsReset.cs) and
// before tests that read these statics. The editor reloads the domain on entering Play but not on leaving it,
// so without this an EditMode test run after a Play session sees the last frame of that session — CameraShake's
// look point made BlastReactionTests fail in a live editor and pass in the batch gate. Anything outside this
// assembly registers its own reset with Register (CameraShake does, from its static constructor).
//
// StaticLifecycleTests fails when a type in TW.Presentation.* or TW.UI gains a mutable static and neither registers
// a reset here nor is listed there with the reason it is safe.
using System;
using System.Collections.Generic;
using UnityEngine;

namespace TW.Presentation
{
    public static class SceneStatics
    {
        static readonly List<KeyValuePair<string, Action>> session = new List<KeyValuePair<string, Action>>();
        static readonly List<KeyValuePair<string, Action>> perScene = new List<KeyValuePair<string, Action>>();

        /// <summary>Per scene load: statics nobody owns, then every per-scene registration. Leaves SceneHooks alone
        /// (see the file header).</summary>
        public static void Reset()
        {
            SimHost.BombardmentOverride = -1f;
            Time.timeScale = 1f;
            InputFocus.Reset();
            for (int i = 0; i < perScene.Count; i++) perScene[i].Value();
        }

        /// <summary>Register what to put back when a Play session ends. One entry per owner: registering the same
        /// owner again replaces its reset (a static constructor runs again after a domain reload).</summary>
        public static void Register(string owner, Action reset)
        {
            for (int i = 0; i < session.Count; i++)
                if (session[i].Key == owner) { session[i] = new KeyValuePair<string, Action>(owner, reset); return; }
            session.Add(new KeyValuePair<string, Action>(owner, reset));
        }

        /// <summary>Register what to forget on every scene load: a cache of things the ending scene owned. Same
        /// replace-by-owner rule as Register. These also run when a Play session ends (ResetSession calls Reset).</summary>
        public static void RegisterPerScene(string owner, Action reset)
        {
            for (int i = 0; i < perScene.Count; i++)
                if (perScene[i].Key == owner) { perScene[i] = new KeyValuePair<string, Action>(owner, reset); return; }
            perScene.Add(new KeyValuePair<string, Action>(owner, reset));
        }

        /// <summary>Who forgets its scene's things on every scene load (the tests read it).</summary>
        public static IEnumerable<string> PerSceneRegistered
        {
            get { foreach (var kv in perScene) yield return kv.Key; }
        }

        /// <summary>Who has registered a reset (StaticLifecycleTests reads it).</summary>
        public static IEnumerable<string> Registered
        {
            get { foreach (var kv in session) yield return kv.Key; }
        }

        /// <summary>No scene is live: put back everything a Play session may have left behind.</summary>
        public static void ResetSession()
        {
            Reset();
            SceneHooks.Reset();
            HudBridge.PointerOverUi = null;
            HudBridge.WheelClaimed = null;
            for (int i = 0; i < session.Count; i++) session[i].Value();
        }
    }
}
