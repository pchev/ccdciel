#!/usr/bin/env python3
"""
Build CCDCiel deep-sky catalog rows for Lynds' Bright Nebulae (LBN) and
Lynds' Dark Nebulae (LDN).

Sources (fixed-width ASCII, coordinates in B1950):
  LDN : CDS VizieR VII/7A  https://cdsarc.cds.unistra.fr/ftp/cats/VII/7A/ldn
        Lynds B.T., 1962ApJS....7....1L
  LBN : CDS VizieR VII/9   https://cdsarc.cds.unistra.fr/ftp/cats/VII/9/catalog.dat
        Lynds B.T., 1965ApJS...12..163L

Output is the CCDCiel deep_sky.csv 6-field format:
    RA[0..864000], DEC[-324000..324000], name(s), length[0.1 arcmin],
    width[0.1 arcmin], orientation[degrees]
with RA = degrees*2400 and DEC = degrees*3600.

Coordinates are precessed from B1950 to J2000 using the same Lieske (1977)
formula as PrecessionFK5 in src/u_utils.pas, so positions match the rest of
deep_sky.csv.

Usage:
    python3 make_lynds.py <indir> <outdir>
where <indir> contains the downloaded 'ldn' and 'catalog.dat' files.
"""

import math
import os
import re
import sys

PI2 = 2.0 * math.pi
DEG2RAD = math.pi / 180.0
JD1950 = 2433190.5
JD2000 = 2451545.0

MAX_ALIASES = 3  # deep_sky.csv parses naam2, naam3, naam4 only
# VII/7A carries up to 8 Barnard cross-identifications per cloud, so a single
# row cannot hold them all. The LDN row keeps the first (MAX_ALIASES-1) and the
# remainder are emitted as extra rows at the same coordinates, each holding up
# to MAX_ALIASES Barnard designations. Duplicated geometry is harmless: the
# ellipse is drawn twice in exactly the same place.
LDN_BARN_FIRST = MAX_ALIASES - 1
LDN_BARN_OVERFLOW = MAX_ALIASES


def precession_fk5(ti, tf, ra, de):
    """Lieske 1977 precession, identical to PrecessionFK5 in src/u_utils.pas.
    ra, de in radians; returns J2000 radians with RA normalised to [0, 2pi)."""
    if abs(ti - tf) < 0.01:
        return ra, de
    i1 = (ti - JD2000) / 36525.0
    i2 = (tf - ti) / 36525.0
    i3 = DEG2RAD * ((2306.2181 + 1.39656 * i1 - 1.39e-4 * i1 * i1) * i2
                    + (0.30188 - 3.44e-4 * i1) * i2 * i2
                    + 1.7998e-2 * i2 * i2 * i2) / 3600.0
    i4 = DEG2RAD * ((2306.2181 + 1.39656 * i1 - 1.39e-4 * i1 * i1) * i2
                    + (1.09468 + 6.6e-5 * i1) * i2 * i2
                    + 1.8203e-2 * i2 * i2 * i2) / 3600.0
    i5 = DEG2RAD * ((2004.3109 - 0.85330 * i1 - 2.17e-4 * i1 * i1) * i2
                    - (0.42665 + 2.17e-4 * i1) * i2 * i2
                    - 4.1833e-2 * i2 * i2 * i2) / 3600.0
    i6 = math.cos(de) * math.sin(ra + i3)
    i7 = math.cos(i5) * math.cos(de) * math.cos(ra + i3) - math.sin(i5) * math.sin(de)
    i1v = math.sin(i5) * math.cos(de) * math.cos(ra + i3) + math.cos(i5) * math.sin(de)
    i1v = max(-1.0, min(1.0, i1v))
    new_de = math.asin(i1v)
    new_ra = (math.atan2(i6, i7) + i4 + PI2) % PI2
    return new_ra, new_de


def to_int(deg, scale):
    """scale=2400 for RA (deg*2400), scale=3600 for Dec (deg*3600)."""
    return int(round(deg * scale))


def clean_name(name):
    """CCDCiel names contain no spaces; underscores replace them."""
    return name.replace(' ', '_')


def map_crossref(raw):
    """Map a VII/9 cross-reference designation to CCDCiel naming style.
    'S 17' -> 'Sh2-17', 'C 15' -> 'Ced15', 'NGC 6960' -> 'NGC6960',
    'IC 443' -> 'IC443', 'DG 10' -> 'DG10'."""
    raw = raw.strip()
    if not raw:
        return None
    m = re.match(r'^([A-Za-z]+)\s*([0-9]+)$', raw)
    if not m:
        return clean_name(raw)
    prefix, number = m.group(1), m.group(2)
    if prefix == 'S':
        return 'Sh2-' + number
    if prefix == 'C':
        return 'Ced' + number
    if prefix in ('NGC', 'IC', 'DG'):
        return prefix + number
    return clean_name(prefix) + number


def size_from_area(area_sqdeg):
    """LDN has no dimensions, only cloud area in square degrees.
    Assume a 2:1 axis ratio ellipse: Area = pi*a*b with b = a/2,
    so a = sqrt(2*Area/pi) degrees. Returns (length, width) in 0.1 arcmin."""
    if area_sqdeg <= 0.0:
        return 0, 0
    a_deg = math.sqrt(2.0 * area_sqdeg / math.pi)
    b_deg = a_deg / 2.0
    return int(round(a_deg * 60.0 * 10.0)), int(round(b_deg * 60.0 * 10.0))


def parse_ldn(path):
    """VII/7A/ldn, Lrecl 92.
    Bytes: 1-4 LDN, 6-7 RAh, 9-12 RAm, 16 DE sign, 17-18 DEd, 20-21 DEm,
    23-28 GLON, 30-35 GLAT, 37-43 Area, 45 Opacity, 47-49 ID,
    51-54 Seq, 56-59 Lynds2, 61-92 Barnard numbers."""
    rows = []
    for line in open(path, 'r', errors='replace'):
        line = line.rstrip('\n')
        if len(line) < 59:
            continue
        if line[0:4].strip() == '':
            continue  # the 4 unnamed objects added after the published version
        try:
            ldn = int(line[0:4])
            rah = int(line[5:7])
            ram = float(line[8:12])
            sign = -1.0 if line[15] == '-' else 1.0
            ded = int(line[16:18])
            dem = int(line[19:21])
            area = float(line[36:43])
        except ValueError:
            continue
        ra = (rah + ram / 60.0) * 15.0 * DEG2RAD
        de = sign * (ded + dem / 60.0) * DEG2RAD
        ra, de = precession_fk5(JD1950, JD2000, ra, de)
        length, width = size_from_area(area)
        # Barn column is 8A4 (bytes 61-92): eight 4-char fields, and adjacent
        # fields can run together with no separator (e.g. "119A117A323"), so
        # slice by width rather than splitting on whitespace.
        # Designations are "a number followed by a letter" per the ReadMe, so
        # keep any letter suffix: 67A is not 67.
        barns = []
        for i in range(60, min(92, len(line)), 4):
            m = re.fullmatch(r'0*([0-9]+)([A-Za-z]?)', line[i:i + 4].strip())
            if m:
                barns.append('B' + m.group(1) + m.group(2).upper())
        ra_i = to_int(ra * 180.0 / math.pi, 2400)
        de_i = to_int(de * 180.0 / math.pi, 3600)
        rows.append((ra_i, de_i, ['LDN%d' % ldn] + barns[:LDN_BARN_FIRST],
                     length, width, 999))
        rest = barns[LDN_BARN_FIRST:]
        while rest:
            rows.append((ra_i, de_i, rest[:LDN_BARN_OVERFLOW], length, width, 999))
            rest = rest[LDN_BARN_OVERFLOW:]
    return rows


def parse_lbn(path):
    """VII/9/catalog.dat, Lrecl 68.
    Bytes: 2-5 Seq(=LBN number), 7-12 GLON, 14-19 GLAT, 21-22 RAh, 24-25 RAm,
    28 DE sign, 29-30 DEd, 32-33 DEm, 36-39 Diam1[arcmin], 41-43 Diam2[arcmin],
    45-51 Area, 53 Color, 55 Bright, 57-59 ID, 61-68 Other name."""
    rows = []
    for line in open(path, 'r', errors='replace'):
        line = line.rstrip('\n')
        if len(line) < 33:
            continue
        if line[1:5].strip() == '':
            continue
        try:
            seq = int(line[1:5])
            rah = int(line[20:22])
            ram = int(line[23:25])
            sign = -1.0 if line[27] == '-' else 1.0
            ded = int(line[28:30])
            dem = int(line[31:33])
            diam1 = int(line[35:39])
            diam2 = int(line[40:43])
        except ValueError:
            continue
        ra = (rah + ram / 60.0) * 15.0 * DEG2RAD
        de = sign * (ded + dem / 60.0) * DEG2RAD
        ra, de = precession_fk5(JD1950, JD2000, ra, de)
        # VII/9 does not order Diam1/Diam2, but every numeric row in
        # deep_sky.csv has width <= length, so normalise here.
        if diam2 > diam1:
            diam1, diam2 = diam2, diam1
        names = ['LBN%d' % seq]
        crossref = map_crossref(line[60:68] if len(line) >= 61 else '')
        if crossref:
            names.append(crossref)
        rows.append((to_int(ra * 180.0 / math.pi, 2400),
                     to_int(de * 180.0 / math.pi, 3600),
                     names, diam1 * 10, diam2 * 10, 999))
    return rows


def write_csv(path, header, rows):
    with open(path, 'w') as out:
        out.write(header + '\n')
        out.write('RA[0..864000], DEC[-324000..324000], name(s), '
                  'length [0.1 min], width[0.1 min], orientation[degrees]\n')
        for ra, de, names, length, width, pa in rows:
            out.write('%d,%d,%s,%d,%d,%d\n' % (ra, de, '/'.join(names), length, width, pa))


def main():
    indir = sys.argv[1] if len(sys.argv) > 1 else '.'
    outdir = sys.argv[2] if len(sys.argv) > 2 else '.'
    ldn_path = os.path.join(indir, 'ldn')
    lbn_path = os.path.join(indir, 'catalog.dat')
    if not os.path.isfile(ldn_path) or not os.path.isfile(lbn_path):
        sys.exit('missing input: need %s and %s' % (ldn_path, lbn_path))

    ldn_header = ('CCDCIEL LBN/LDN EXTRA DATABASE - Lynds\' Catalogue of Dark '
                  'Nebulae. Source: CDS VizieR VII/7A, Lynds B.T., '
                  'Astrophys. J. Suppl. Ser. 7, 1 (1962) [1962ApJS....7....1L]. '
                  'Positions precessed B1950->J2000. Size derived from Cloud_Area '
                  '(2:1 axis ratio). Dec range +90..-33 (Palomar plates).')
    lbn_header = ('CCDCIEL LBN/LDN EXTRA DATABASE - Lynds\' Catalogue of Bright '
                  'Nebulae. Source: CDS VizieR VII/9, Lynds B.T., '
                  'Astrophys. J. Suppl. Ser. 12, 163 (1965) [1965ApJS...12..163L]. '
                  'Positions precessed B1950->J2000. Dec range +90..-33 '
                  '(Palomar plates).')

    ldn_rows = parse_ldn(ldn_path)
    lbn_rows = parse_lbn(lbn_path)
    write_csv(os.path.join(outdir, 'ldn.csv'), ldn_header, ldn_rows)
    write_csv(os.path.join(outdir, 'lbn.csv'), lbn_header, lbn_rows)
    print('ldn.csv: %d rows' % len(ldn_rows))
    print('lbn.csv: %d rows' % len(lbn_rows))


if __name__ == '__main__':
    main()
