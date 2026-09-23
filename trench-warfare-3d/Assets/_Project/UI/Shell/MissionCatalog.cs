// Phase: B6 (implemented) — the mission select's list, in order; the first card is the default (today's map).
// Its own file: Unity binds a ScriptableObject asset to its script only when the file is named after the class, so
// a catalog declared inside MissionCard.cs saved with no script reference and loaded with no cards.
using UnityEngine;

namespace TW.UI
{
    [CreateAssetMenu(menuName = "TW/Mission Catalog", fileName = "MissionCatalog")]
    public sealed class MissionCatalog : ScriptableObject
    {
        public MissionCard[] Cards = System.Array.Empty<MissionCard>();
    }
}
