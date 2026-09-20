// Phase: C2 (implemented generator) — creates the British and German slice rosters plus the off-map abilities as
// ScriptableObject assets from the tables in docs/06-units-and-factions.md and docs/07-abilities.md.
// Menu: TW → Data → Create Slice Definitions. Re-running overwrites values, keeping asset GUIDs.
using System.IO;
using UnityEditor;
using UnityEngine;
using TW.Data;
using TW.Sim.Combat;
using TW.Sim.Match;
using TW.Sim.Units;

namespace TW.Editor
{
    public static class SliceDefinitions
    {
        const string Root = "Assets/_Project/Data/Definitions";

        [MenuItem("TW/Data/Create Slice Definitions")]
        public static void Create()
        {
            Directory.CreateDirectory($"{Root}/Weapons"); Directory.CreateDirectory($"{Root}/British");
            Directory.CreateDirectory($"{Root}/German"); Directory.CreateDirectory($"{Root}/French"); Directory.CreateDirectory($"{Root}/Abilities");
            AssetDatabase.Refresh(); // folders must be imported before CreateAsset can target them

            // ---- weapons ----
            var smle = Weapon(1, "SMLE Mk III", 25, 60, 0.8f, acc: 0.6f);
            var g98 = Weapon(2, "Gewehr 98", 25, 60, 0.8f, acc: 0.6f);
            var lewis = Weapon(3, "Lewis Gun", 18, 80, 8f, acc: 0.45f, mag: 47, reload: 4f, supp: 12f);
            var mg08 = Weapon(4, "MG 08/15", 18, 80, 8f, acc: 0.45f, mag: 100, reload: 5f, supp: 12f);
            var webley = Weapon(5, "Webley + cut-down SMLE", 12, 25, 4f, acc: 0.5f, burst: 5);
            var mp18 = Weapon(6, "MP 18", 12, 25, 6f, acc: 0.45f, burst: 6);
            var mills = Weapon(7, "Mills bomb", 80, 20, 0.17f, mode: FireMode.Indirect, blast: 4f, supp: 40f, v0: 12f);
            var stiel = Weapon(8, "Stielhandgranate", 80, 22, 0.17f, mode: FireMode.Indirect, blast: 4f, supp: 40f, v0: 12f);
            var scopedSmle = Weapon(9, "Scoped SMLE", 150, 80, 0.25f, acc: 0.85f, supp: 8f);
            var scopedG98 = Weapon(10, "Scoped Gewehr 98", 150, 80, 0.25f, acc: 0.85f, supp: 8f);
            var tankgewehr = Weapon(11, "Mauser Tankgewehr", 1400, 70, 0.2f, acc: 0.7f, pen: 20f);
            var mg08Sentry = Weapon(12, "MG 08 (Sentry)", 18, 60, 8f, acc: 0.4f, mag: 250, reload: 6f, supp: 12f);
            var flammen = Weapon(13, "Flammenwerfer", 40, 12, 10f, mode: FireMode.Cone, burning: true, supp: 20f);
            var sixPdr = Weapon(14, "6-pdr sponson", 300, 90, 0.33f, acc: 0.55f, pen: 40f, blast: 3f, crater: 15);
            var vickers = Weapon(15, "Vickers (tank)", 18, 70, 8f, acc: 0.4f, mag: 250, reload: 5f, supp: 12f);
            var kwk57 = Weapon(16, "5.7 cm Maxim-Nordenfelt", 350, 90, 0.3f, acc: 0.55f, pen: 45f, blast: 3f, crater: 15);
            var webleyPistol = Weapon(17, "Webley (officer)", 20, 30, 1f, acc: 0.5f);
            var pomPom = Weapon(18, "2-pdr pom-pom", 120, 120, 2f, acc: 0.5f, blast: 2.5f, pen: 15f);
            var soixanteQuinze = Weapon(19, "Canon de 75", 1500, 100, 0.25f, acc: 0.6f, pen: 30f, blast: 4f, crater: 20);
            var hotchkiss = Weapon(20, "Hotchkiss M1914", 18, 80, 8f, acc: 0.45f, mag: 30, reload: 3f, supp: 12f);

            // ---- British ----
            var bRifle = Unit(100, "Rifleman", Faction.British, UnitClass.Rifleman, 25, 100, 3.0f, smle, "British");
            var bRaider = Unit(101, "Trench Raider", Faction.British, UnitClass.Assault, 40, 90, 4.2f, webley, "British", secondary: mills, grenade: 20f);
            var bLewis = Unit(102, "Lewis Gunner", Faction.British, UnitClass.Machinegunner, 60, 110, 2.2f, lewis, "British");
            var bSniper = Unit(103, "Sniper", Faction.British, UnitClass.Sniper, 90, 80, 3.0f, scopedSmle, "British", cd: 200, trenchHp: 1f, trenchAcc: 0.25f, trenchRange: 0.4f, prio: 1);
            var bOfficer = Unit(104, "Officer", Faction.British, UnitClass.Officer, 110, 100, 3.0f, webleyPistol, "British", cd: 300, aura: 15f);
            var bAt = Unit(105, "Anti-Tank Rifleman", Faction.British, UnitClass.AntiTankRifle, 100, 90, 2.8f, tankgewehr, "British", cd: 240);
            var markIvMale = Unit(110, "Mark IV (Male)", Faction.British, UnitClass.Tank, 350, 10000, 1.6f, sixPdr, "British", secondary: vickers, cd: 600, armor: new[] { 12f, 8f, 6f, 6f }, vehicle: true);
            var markIvFemale = Unit(111, "Mark IV (Female)", Faction.British, UnitClass.Tank, 300, 10000, 1.6f, vickers, "British", cd: 600, armor: new[] { 12f, 8f, 6f, 6f }, vehicle: true);
            var markV = Unit(112, "Mark V", Faction.British, UnitClass.Tank, 380, 11000, 2.0f, sixPdr, "British", secondary: vickers, cd: 600, armor: new[] { 14f, 12f, 8f, 8f }, vehicle: true);
            var pierceArrow = Unit(113, "Pierce-Arrow AA Lorry", Faction.British, UnitClass.Lorry, 220, 3000, 5.0f, pomPom, "British", cd: 400, armor: new[] { 3f, 3f, 3f, 3f }, vehicle: true);
            FactionAsset(Faction.British, "British Empire", new[] { bRifle, bRaider, bLewis, bSniper, markIvMale }, new[] { bOfficer, bAt }, new[] { markIvFemale, markV, pierceArrow },
                new[] { "Khaki Service Dress", "CEF", "Sikh Sepoys", "ANZAC Gallipoli", "Gordon Highlanders", "Napoleonic Redcoats", "Hejaz Irregulars", "SAS" });

            // ---- German ----
            var gRifle = Unit(200, "Infanterist", Faction.German, UnitClass.Rifleman, 25, 100, 3.0f, g98, "German");
            var gSturm = Unit(201, "Sturmtruppen", Faction.German, UnitClass.Assault, 40, 90, 4.2f, mp18, "German", secondary: stiel, grenade: 22f);
            var gMg = Unit(202, "MG 08/15 Schütze", Faction.German, UnitClass.Machinegunner, 60, 110, 2.2f, mg08, "German");
            var gAt = Unit(203, "Tankgewehr Schütze", Faction.German, UnitClass.AntiTankRifle, 100, 90, 2.8f, tankgewehr, "German", cd: 240);
            var gSentry = Unit(204, "Grabenposten (Sentry)", Faction.German, UnitClass.Sentry, 125, 300, 1.8f, mg08Sentry, "German", cd: 300, armor: new[] { 8f, 2f, 0f, 0f }, shieldArc: 60f);
            var gFlame = Unit(205, "Flammenwerfer-Trupp", Faction.German, UnitClass.Flamethrower, 120, 100, 3.0f, flammen, "German", cd: 300);
            var gSniper = Unit(206, "Scharfschütze", Faction.German, UnitClass.Sniper, 90, 80, 3.0f, scopedG98, "German", cd: 200, trenchHp: 1f, trenchAcc: 0.25f, trenchRange: 0.4f, prio: 1);
            var a7v = Unit(210, "A7V Sturmpanzerwagen", Faction.German, UnitClass.Tank, 400, 12000, 1.8f, kwk57, "German", secondary: mg08, cd: 700, armor: new[] { 30f, 20f, 20f, 6f }, vehicle: true);
            var ehrhardt = Unit(211, "Ehrhardt E-V/4", Faction.German, UnitClass.ArmouredCar, 250, 4000, 4.0f, mg08, "German", cd: 400, armor: new[] { 7f, 7f, 7f, 7f }, vehicle: true);
            var motorcycle = Unit(212, "Kradschützen MG", Faction.German, UnitClass.Motorcycle, 90, 600, 8.0f, mg08, "German", cd: 200, vehicle: true);
            FactionAsset(Faction.German, "German Empire", new[] { gRifle, gSturm, gMg, gAt, a7v }, new[] { gSentry, gFlame, gSniper }, new[] { ehrhardt, motorcycle },
                new[] { "Stahlhelm 1916-18", "1914 Pickelhaube", "Seebataillon", "Prussian Guard", "Alpenkorps", "Asienkorps", "WW2 Wehrmacht", "GSG-9" });

            // ---- French vehicles for M3 enemy variety ----
            Unit(310, "Saint-Chamond", Faction.French, UnitClass.Tank, 400, 15000, 2.0f, soixanteQuinze, "French", secondary: hotchkiss, cd: 700, armor: new[] { 17f, 17f, 17f, 5f }, vehicle: true);
            Unit(311, "Schneider CA1", Faction.French, UnitClass.Tank, 300, 8000, 2.2f, soixanteQuinze, "French", secondary: hotchkiss, cd: 600, armor: new[] { 11f, 11f, 11f, 5f }, vehicle: true);
            Unit(312, "Renault FT", Faction.French, UnitClass.Tank, 220, 5000, 3.0f, hotchkiss, "French", cd: 500, armor: new[] { 16f, 16f, 8f, 8f }, vehicle: true);

            // ---- off-map abilities ----
            Ability(OffMapAbilityId.HeBarrage, "HE Barrage", 150, 1200, 80, AbilityTargetMode.Area, radius: 25f, shells: 12, dmg: 150f, r: 8f, spread: 120, wire: true);
            Ability(OffMapAbilityId.CreepingBarrage, "Creeping Barrage", 250, 2400, 120, AbilityTargetMode.Heading, length: 60f, shells: 40, dmg: 120f, r: 8f, spread: 800, wire: true);
            Ability(OffMapAbilityId.ChlorineGas, "Chlorine Gas", 120, 1800, 60, AbilityTargetMode.Point, radius: 10f, conc: 40f, persist: 1200);
            Ability(OffMapAbilityId.MustardGas, "Mustard Gas", 180, 2400, 60, AbilityTargetMode.Area, radius: 20f, conc: 25f, persist: 1800);
            Ability(OffMapAbilityId.BomberRun, "Bomber Run", 200, 2400, 100, AbilityTargetMode.Heading, length: 60f, shells: 6, dmg: 400f, r: 6f, spread: 40, trench: true, bunker: true);
            Ability(OffMapAbilityId.SmokeScreen, "Smoke Screen", 60, 600, 40, AbilityTargetMode.Line, length: 40f, conc: 30f, persist: 600);
            Ability(OffMapAbilityId.MortarSalvo, "Off-map Mortar Salvo", 0, 900, 80, AbilityTargetMode.Point, radius: 5f, shells: 4, dmg: 120f, r: 5f, spread: 40);
            Ability(OffMapAbilityId.ReconFlight, "Recon Flight", 40, 1200, 0, AbilityTargetMode.Line, length: 800f);
            Ability(OffMapAbilityId.ReinforcementSurge, "Reinforcement Surge", 100, 1800, 0, AbilityTargetMode.None);

            AssetDatabase.SaveAssets();
            AssetDatabase.Refresh();
            Debug.Log("TW: slice definitions created under " + Root);
        }

        static T Load<T>(string path) where T : ScriptableObject
        {
            var a = AssetDatabase.LoadAssetAtPath<T>(path);
            if (a == null) { a = ScriptableObject.CreateInstance<T>(); AssetDatabase.CreateAsset(a, path); }
            EditorUtility.SetDirty(a);
            return a;
        }

        static WeaponDefinition Weapon(short id, string name, float dmg, float range, float rof, FireMode mode = FireMode.Direct, float acc = 0.6f,
            int burst = 1, int mag = 0, float reload = 0f, float supp = 4f, float pen = 0f, float blast = 0f, float v0 = 0f, byte crater = 0, bool burning = false)
        {
            var w = Load<WeaponDefinition>($"{Root}/Weapons/{id:000}_{Sanitize(name)}.asset");
            w.Id = id; w.DisplayName = name; w.Mode = mode; w.Damage = dmg; w.RangeMax = range; w.RoundsPerSecond = rof; w.Accuracy = acc;
            w.BurstRounds = burst; w.MagazineRounds = mag; w.ReloadSeconds = reload; w.SuppressionPerShot = supp; w.PenetrationMm = pen;
            w.BlastRadius = blast; w.MuzzleVelocity = v0; w.CraterRadiusDm = crater; w.SetsBurning = burning;
            return w;
        }

        static UnitDefinition Unit(short id, string name, Faction faction, UnitClass cls, int cost, float hp, float speed, WeaponDefinition primary, string folder,
            WeaponDefinition secondary = null, float grenade = 0f, int cd = 0, float[] armor = null, bool vehicle = false, float aura = 0f, float shieldArc = 0f,
            float trenchHp = 0f, float trenchAcc = 0f, float trenchRange = 0f, byte prio = 0)
        {
            var u = Load<UnitDefinition>($"{Root}/{folder}/{id:000}_{Sanitize(name)}.asset");
            u.Id = id; u.DisplayName = name; u.Faction = faction; u.Class = cls; u.Cost = cost; u.Hp = hp; u.Speed = speed;
            u.PrimaryWeapon = primary; u.SecondaryWeapon = secondary; u.GrenadeRange = grenade; u.DeployCooldownTicks = cd;
            if (armor != null) { u.ArmorFrontMm = armor[0]; u.ArmorSideMm = armor[1]; u.ArmorRearMm = armor[2]; u.ArmorTopMm = armor[3]; }
            u.IsVehicle = vehicle; u.CanEnterTrench = !vehicle; u.AuraRadius = aura; u.ShieldArcHalfWidthDeg = shieldArc;
            u.TrenchHpBonus = trenchHp; u.TrenchAccuracyBonus = trenchAcc; u.TrenchRangeBonus = trenchRange; u.TargetPriorityProfile = prio;
            u.VatArchetype = vehicle ? "vehicle" : cls == UnitClass.Machinegunner || cls == UnitClass.Sentry ? "infantry_mg" : "infantry_rifle";
            return u;
        }

        static void FactionAsset(Faction f, string name, UnitDefinition[] slots, UnitDefinition[] altSpecials, UnitDefinition[] altVehicles, string[] skins)
        {
            var a = Load<FactionDefinition>($"{Root}/{f}.asset");
            a.Faction = f; a.DisplayName = name; a.DefaultSlots = slots;
            a.AlternateSpecials.Clear(); a.AlternateSpecials.AddRange(altSpecials);
            a.AlternateVehicles.Clear(); a.AlternateVehicles.AddRange(altVehicles);
            a.UniformSkins.Clear(); a.UniformSkins.AddRange(skins);
        }

        static void Ability(OffMapAbilityId id, string name, int cost, int cd, int warm, AbilityTargetMode target, float radius = 0f, float length = 0f,
            int shells = 0, float dmg = 0f, float r = 0f, int spread = 0, float conc = 0f, int persist = 0, bool wire = false, bool trench = false, bool bunker = false)
        {
            var a = Load<AbilityDefinition>($"{Root}/Abilities/{(int)id:00}_{Sanitize(name)}.asset");
            a.Id = id; a.DisplayName = name; a.Cost = cost; a.CooldownTicks = cd; a.WarmupTicks = warm; a.Target = target; a.Radius = radius; a.Length = length;
            a.Shells = shells; a.ShellDamage = dmg; a.ShellRadius = r; a.SpreadTicks = spread; a.Concentration = conc; a.PersistTicks = persist;
            a.BreachesWire = wire; a.CollapsesTrench = trench; a.DestroysBunker = bunker;
        }

        static string Sanitize(string s)
        {
            foreach (var c in Path.GetInvalidFileNameChars()) s = s.Replace(c, '_');
            return s.Replace(' ', '_').Replace('/', '_');
        }
    }
}
