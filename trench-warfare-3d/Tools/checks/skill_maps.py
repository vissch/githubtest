import re

WHY = 'A big reference file needs a section map in its SKILL.md, or a leg burns context reading all of it before work starts.'


def run(ctx):
    """Every .claude/skills/*/references/*.md over 15,000 bytes must have a '## Section map — `<path>`' line in its
    skill's SKILL.md, followed by a markdown table whose first cells match the reference's '## ' headings exactly
    (after stripping backticks and collapsing whitespace). The threshold of 15,000 comes from the files as they are:
    the three targets are 61,126 / 21,431 / 21,376 bytes and the next biggest reference is 9,669
    (tw-bug-catcher/.../determinism-and-verdicts.md), so exactly these three are covered, with headroom both ways."""
    skills_dir = ctx.root.parent / '.claude/skills'
    if not skills_dir.exists():
        return []

    def norm(s):
        return re.sub(r'\s+', ' ', s.replace('`', '')).strip()

    errors = []
    for skill_dir in sorted(skills_dir.glob('*')):
        skill_md = skill_dir / 'SKILL.md'
        refs_dir = skill_dir / 'references'
        if not refs_dir.exists():
            continue
        for ref in sorted(refs_dir.glob('*.md')):
            if ref.stat().st_size <= 15000:
                continue
            rel = ref.relative_to(skill_dir).as_posix()
            if not skill_md.exists():
                errors.append(f'{skill_dir.name}: {rel} is over 15,000 bytes but there is no SKILL.md')
                continue
            skill_text = ctx.text(skill_md)
            map_line = None
            for line in skill_text.splitlines():
                m = re.match(r'^##\s+Section map\s+—\s+(.*)$', line.strip())
                if m and rel in m.group(1):
                    map_line = line
                    break
            if map_line is None:
                errors.append(f'{skill_dir.name}/SKILL.md: missing "## Section map — `{rel}`" for the '
                               f'{ref.stat().st_size}-byte reference {rel}')
                continue

            ref_headings = {norm(h) for h in re.findall(r'^##\s+(.+)$', ctx.text(ref), re.M)}

            lines = skill_text.splitlines()
            start = lines.index(map_line) + 1
            rows = []
            header_skipped = False
            for line in lines[start:]:
                stripped = line.strip()
                if not stripped.startswith('|'):
                    if rows or header_skipped:
                        break
                    continue
                if re.match(r'^\|\s*-+\s*\|', stripped):
                    continue
                if not header_skipped:
                    header_skipped = True
                    continue
                cell = stripped.split('|')[1]
                rows.append(norm(cell))

            if not rows:
                errors.append(f'{skill_dir.name}/SKILL.md: "## Section map — `{rel}`" has no table rows')
                continue

            for cell in rows:
                if cell not in ref_headings:
                    nearest = ', '.join(sorted(ref_headings)[:5])
                    errors.append(f'{skill_dir.name}: section map cell "{cell}" matches no "## " heading in {rel} '
                                  f'(have, e.g.: {nearest})')

    return errors
