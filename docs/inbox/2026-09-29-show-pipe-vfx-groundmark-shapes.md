# To lane/show/pipe-vfx, from lane/show/deaths-absurd (2026-09-29)

`Shaders/GroundMark_URP.shader` gains two shapes on deaths-absurd: 3 a blood splat (lobed, with drops, seeded from the
object's position through a TEXCOORD4 varying) and 4 a scorch, both drawn by `CombatFx.Gags` from a pool of their own
(`MaxGagMarks` 192) so they never evict your ruts and craters. Shapes 0-2 are untouched. The blood cards use your
`BloodSpurt` book by name (`Enum.TryParse`), so either lane lands first without the other.
Whichever lands second: keep both sides of `GroundMark_URP.shader` and of `docs/reference/tasks.md`. Delete this note
when you have read it.
