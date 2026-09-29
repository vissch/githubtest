// Phase: A5c (2026-09-29, the Dust Front lessons: vehicle weight) — depends on: TankModel.
// One answer to "where is this socket". The tanks' splitter (Tools/tanksplit.py) writes numbered pairs, Socket_Exhaust0/1
// and Socket_Fire0/1; the walkers' (Tools/crabsplit.py) writes one of each, Socket_Exhaust and Socket_Fire. TankRenderer
// asks for the numbered names, so the walkers never smoked from their exhaust, never burned at their fire socket, and
// their fire burst, burning smoke and bail-out puff were drawn at world y = 0, under the ground. Asked for the first of a
// numbered pair, a model with the plain name answers with it; the second of a pair is never invented, so one exhaust
// stays one puff.
using UnityEngine;

namespace TW.Presentation.Tactical
{
    public static class MachineSockets
    {
        /// <summary>The socket <paramref name="name"/> on <paramref name="model"/>, or the plain one a walker has for
        /// the first of a numbered pair. No allocation.</summary>
        public static bool TryResolve(TankModel model, string name, out (int part, Vector3 local) socket)
        {
            socket = default;
            if (model == null || name == null) return false;
            if (model.Sockets.TryGetValue(name, out socket)) return true;
            string plain = PlainName(name);
            return plain != null && model.Sockets.TryGetValue(plain, out socket);
        }

        /// <summary>The plain name a walker's exporter writes for the first of a numbered pair, or null.</summary>
        public static string PlainName(string name)
            => name == "Socket_Exhaust0" ? "Socket_Exhaust" : name == "Socket_Fire0" ? "Socket_Fire" : null;
    }
}
