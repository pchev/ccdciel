#!/usr/bin/env python3
"""
Merge the extra catalogs (Lynds LBN/LDN and MBM) into deep_sky.csv (option B).

For every extra row whose primary designation (LBN<n> / LDN<n> / MBM<n>)
already exists in deep_sky.csv, the existing row is REPLACED in place: the
authoritative catalog position, size and orientation take over, and any
additional designations the old row carried are kept as aliases (subject to
the parser's 3-slot limit). Rows with no existing match are appended.

Overflow rows from ldn.csv (Barnard designations that did not fit in the
3 alias slots of the parent LDN row) are appended only when at least one of
their designations is not already reachable in deep_sky.csv.

MBM<n> designations do not exist in deep_sky.csv at all, so every mbm.csv row
is appended. The key machinery still covers them so that a later re-run stays
idempotent.

The merge is idempotent: running it again over an already-merged file changes
nothing but the object count in the header line.

Usage:
    python3 merge_extras.py <deep_sky.csv> <extra_dir> [--write]
Without --write the merge is reported but not written.
"""

import math
import os
import re
import sys

CATRE = re.compile(r'^(LBN|LDN|MBM)([0-9]+)$')
BARNRE = re.compile(r'^B[0-9]+[A-Z]?$')
MAX_ALIASES = 3
# deep_sky.csv units: RA is deg*2400, Dec is deg*3600. A row whose stored
# position is further than this from the authoritative VII/7A / VII/9 position
# carries a mis-identification, not a coarse position: HNSKY attached the
# LBN/LDN number to a neighbouring object. Moving that row would teleport a
# real named object, so the number is detached from it instead.
DETAG_SEP = 1.0  # degrees


def split_row(line):
    f = line.rstrip('\n').split(',')
    while len(f) < 6:
        f.append('')
    return f


def names(field):
    """Designations in a name field, whitespace-stripped.

    deep_sky.csv space-pads the name field on rows that carry size fields
    (e.g. 'B100/LDN443      ,160,100'), so every alias must be stripped before
    it is compared or used as a catalog key.
    """
    return [a.strip() for a in field.split('/') if a.strip()]


def cat_keys(aliases):
    keys = []
    for a in aliases:
        m = CATRE.match(a)
        if m:
            keys.append((m.group(1), int(m.group(2))))
    return keys


def separation_deg(f_old, f_new):
    """Angular separation of two rows, both in deep_sky.csv integer units."""
    try:
        ra1, de1 = int(f_old[0]) / 2400.0, int(f_old[1]) / 3600.0
        ra2, de2 = int(f_new[0]) / 2400.0, int(f_new[1]) / 3600.0
    except ValueError:
        return 1e9
    r1, d1, r2, d2 = (x * math.pi / 180.0 for x in (ra1, de1, ra2, de2))
    c = (math.sin(d1) * math.sin(d2)
         + math.cos(d1) * math.cos(d2) * math.cos(abs(r1 - r2)))
    return math.degrees(math.acos(max(-1.0, min(1.0, c))))


def main():
    if len(sys.argv) < 3:
        sys.exit(__doc__)
    deep_path, extra_dir = sys.argv[1], sys.argv[2]
    do_write = '--write' in sys.argv[3:]

    with open(deep_path, 'r', encoding='utf-8-sig', newline='') as fh:
        lines = fh.read().split('\n')
    header1, header2 = lines[0], lines[1]
    data = lines[2:]
    trailing_blank = data and data[-1] == ''
    if trailing_blank:
        data = data[:-1]

    # index existing rows by catalog key; a key may appear on several rows
    # (deep_sky.csv has pre-existing mis-tagged duplicates), so keep them all
    # and pick the best match later
    key2rows = {}
    dup_keys = 0
    dup_keys_seen = set()
    for idx, line in enumerate(data):
        if not line.strip():
            continue
        for k in cat_keys(names(split_row(line)[2])):
            if k in key2rows:
                dup_keys += 1
                dup_keys_seen.add('%s%d' % (k[0], k[1]))
                key2rows[k].append(idx)
            else:
                key2rows[k] = [idx]
    n_existing = len(data)

    # every designation currently reachable as naam2/naam3/naam4.
    # Recomputed after the replacement pass: replacing a row can evict an
    # alias that was reachable before the merge, and those designations then
    # need the extra/ldn.csv overflow rows to stay findable.
    def reachable_now():
        s = set()
        for line in data:
            if line.strip():
                s.update(names(split_row(line)[2])[:MAX_ALIASES])
        return s

    new_rows = []
    missing = []
    for name in ('lbn.csv', 'ldn.csv', 'mbm.csv'):
        path = os.path.join(extra_dir, name)
        if not os.path.isfile(path):
            # an absent extra file just means that catalog is not being merged;
            # it lets a packager opt out of one catalog by deleting its source
            missing.append(name)
            continue
        with open(path, 'r', encoding='utf-8-sig') as fh:
            for i, line in enumerate(fh):
                if i < 2:
                    continue
                line = line.rstrip('\n')
                if line.strip():
                    new_rows.append(split_row(line))

    replaced = appended = skipped_overflow = detagged = 0
    consumed = set()
    overflow_rows = []
    far_cases = []
    # every number that has an authoritative row in the extra catalogs; a
    # sibling number on an existing row is dropped in favour of that row
    new_keys = set()
    for f in new_rows:
        a = names(f[2])
        if a and CATRE.match(a[0]):
            new_keys.add(a[0])
    for f in new_rows:
        aliases = names(f[2])
        keys = cat_keys(aliases)
        primary = keys[0] if keys and CATRE.match(aliases[0]) else None

        if primary is None:
            overflow_rows.append(f)
            continue

        if primary in key2rows:
            # a single existing row can carry two catalog numbers (deep_sky.csv
            # has rows like B252/LDN1698/LDN1699); once one number has claimed
            # that row the other number must get its own row instead of
            # overwriting the first merge
            cands = [i for i in key2rows[primary] if i not in consumed]
            if not cands:
                appended += 1
                data.append(','.join(f[:6]))
                continue

            def score(idx):
                f0 = split_row(data[idx])
                try:
                    sep = separation_deg(f0, f)
                except ValueError:
                    sep = 1e9
                # positional proximity decides which existing row this number
                # really belongs to; alias overlap only breaks near ties
                return (round(sep, 3), -len(set(aliases) & set(names(f0[2]))))

            cands = sorted(cands, key=score)
            idx = cands[0]
            consumed.add(idx)
            old = split_row(data[idx])
            old_aliases = names(old[2])
            # keep every other designation the old row had, including sibling
            # catalog numbers (deep_sky.csv has rows like B47/LDN1791/LDN1792),
            # except the number being merged and any sibling that gets its own
            # authoritative row from the extra catalogs
            old_other = [a for a in old_aliases
                         if a != aliases[0] and a not in new_keys]
            new_other = [a for a in aliases if not CATRE.match(a)]
            sep = separation_deg(old, f)
            shared = bool(set(old_other) & set(new_other))

            if sep > DETAG_SEP and not shared:
                # The two rows agree on nothing but the number itself and sit
                # degrees apart: HNSKY attached this number to a different
                # object. Keep the old row and its own names where they are
                # and drop the LBN/LDN tag; append the authoritative row.
                if old_other:
                    data[idx] = ','.join(
                        [old[0], old[1], '/'.join(old_other)] + old[3:6])
                    detagged += 1
                    far_cases.append('%s%d@%.1fdeg' % (
                        primary[0], primary[1], sep))
                    appended += 1
                    data.append(','.join(f[:6]))
                    continue
                # old row was only a coarse placeholder for this number

            # Inherit the old row's designations before the new cross-ref:
            # they are already reachable today, so dropping one would be a
            # regression, whereas a new cross-ref is only extra reachability.
            merged = [aliases[0]] + old_other
            # deep_sky.csv has a few lower-case designations (B67a). The parser
            # compares naam2/3/4 against an upper-cased query, so those are
            # unreachable; normalise them to the form the converter emits.
            upper_new = {a.upper(): a for a in new_other}
            merged = [upper_new.get(a.upper(), a) for a in merged]
            for a in new_other:
                if a not in merged and len(merged) < MAX_ALIASES:
                    merged.append(a)
            merged = merged[:MAX_ALIASES]
            data[idx] = '%s,%s,%s,%s,%s,%s' % (
                f[0], f[1], '/'.join(merged), f[3], f[4], f[5])
            replaced += 1
            # drop the catalog tag from the losing duplicate rows: it is a
            # mis-identification, and leaving it would keep two rows claiming
            # the same designation. Their other designations are untouched.
            for other in cands[1:]:
                f0 = split_row(data[other])
                keep = [a for a in names(f0[2]) if a != aliases[0]]
                if keep:
                    data[other] = ','.join(
                        [f0[0], f0[1], '/'.join(keep)] + f0[3:6])
                    detagged += 1
        else:
            appended += 1
            data.append(','.join(f[:6]))

    # overflow rows are decided against the post-replacement alias set
    reachable = reachable_now()
    for f in overflow_rows:
        aliases = names(f[2])
        if not any(a not in reachable for a in aliases):
            skipped_overflow += 1
            continue
        appended += 1
        data.append(','.join(f[:6]))
        reachable.update(aliases)

    new_header1 = (
        'ASTAP/CCDCIEL DEEPSKY DATABASE (extract from HNSKY database), '
        '%d objects. Based on SAC81, Wolfgang Steinicke\'s REV NGC&IC, Leda, '
        'Sh2,vdB,HCG,LND,PK.DWB, Barnard\'s DN. GX>=1_arcmin. IAU named stars '
        'included. GC of M31, M33 added. Lynds Dark Nebulae and Lynds Bright '
        'Nebulae positions from CDS VizieR VII/7A (Lynds 1962) and VII/9 '
        '(Lynds 1965), precessed B1950->J2000. MBM high-latitude molecular '
        'clouds from CDS VizieR J/ApJ/786/29, J/ApJS/256/46, J/AJ/168/203 and '
        'J/A+A/383/631. Version 2024-05-17, Lynds catalogs added 2026-10-07, '
        'MBM added 2026-10-08.' % len(data))

    print('existing data rows : %d' % n_existing)
    if missing:
        print('extra files absent : %s (not merged)' % ' '.join(missing))
    print('new rows in extras : %d' % len(new_rows))
    print('replaced in place  : %d' % replaced)
    print('mis-tags removed   : %d' % detagged)
    if far_cases:
        print('  detached (>%.1f deg): %s' % (DETAG_SEP, ' '.join(far_cases)))
    print('appended           : %d' % appended)
    print('overflow skipped   : %d' % skipped_overflow)
    print('duplicate keys seen: %d %s' % (dup_keys, sorted(dup_keys_seen)))
    print('total after merge  : %d' % len(data))
    if len(data) >= 50000:
        print('WARNING: >= 50000 rows, load_deep/load_hyperleda heuristic breaks')

    if not do_write:
        print('\n(dry run: pass --write to apply)')
        return

    out = '\n'.join([new_header1, header2] + data + ([''] if trailing_blank else []))
    with open(deep_path, 'w', encoding='utf-8-sig', newline='') as fh:
        fh.write(out)
    print('written: %s' % deep_path)


if __name__ == '__main__':
    main()
