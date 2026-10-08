#!/usr/bin/env python3
"""
Build CCDCiel deep-sky catalog rows for the Magnani-Blitz-Mundy (MBM) catalog
of high-latitude molecular clouds.

There is no single machine-readable MBM catalog: the original catalog
(Magnani, Blitz & Mundy 1984ApJ...282L...9M; Magnani et al. 1985ApJ...295..402M)
and its extension (Magnani, Smith & Kubat 1996ApJS..106..447M) are not in
VizieR.  The catalog is reconstructed here from the four VizieR tables that
carry MBM designations:

  1. Schlafly, Finkbeiner, M & D 2014ApJ...786...29S
     https://cdsarc.cds.unistra.fr/ftp/cats/J/ApJ/786/29/mbmcloud.dat
     107 MBM clouds, J2000 galactic l/b + distance moduli.  Backbone: these
     are the MBM catalog positions redone with modern astrometry.
  2. Sun, Reich, Wolleben et al. 2021ApJS..256...46S
     https://cdsarc.cds.unistra.fr/ftp/cats/J/ApJS/256/46/table1.dat
     66 MBM clouds, J2000 galactic l/b.
  3. Sun, Reich, Wolleben et al. 2024AJ....168..203S
     https://cdsarc.cds.unistra.fr/ftp/cats/J/AJ/168/203/table2.dat
     52 rows; the GLONM/GLATM/angRadM columns are the *original MBM catalog*
     centre and angular radius (degrees).  Only source of MBM radii.
  4. Dutra & Bica 2002A&A...383..631D (catalogue of dust clouds)
     https://cdsarc.cds.unistra.fr/ftp/cats/J/A+A/383/631/cdn.dat
     20 MBM cross-identifications, J2000 galactic l/b + a/b in arcmin.

Position priority 1 > 2 > 3 > 4.  Size priority 3 > 4, else no size.

Frame check (done, not assumed): sources 1-3 agree with each other to
0.004-0.014 deg, and source 1 agrees with source 4 to 0.058 deg median.  All
four are J2000 galactic, verified because Schlafly's l/b for MBM 13
(161.59, -35.89) converts to RA 44.849 Dec +17.201, 6 arcmin from the
precessed VII/9 position of LBN 762, which is the same object.
Source 4's printed RA/Dec columns are NOT usable (they disagree with its own
galactic columns by tens of degrees); only its l/b are read.

MBM rows carry no cross-identification aliases on purpose.  Many MBM clouds
share a number-space with LDN clouds already in deep_sky.csv; putting an LDN
designation on an MBM row would let find_object resolve that LDN name to the
MBM position instead of its authoritative VII/7A row.

Output is the CCDCiel deep_sky.csv 6-field format:
    RA[0..864000], DEC[-324000..324000], name(s), length[0.1 arcmin],
    width[0.1 arcmin], orientation[degrees]
with RA = degrees*2400 and DEC = degrees*3600.  A length of 0 makes
plot_deepsky draw a small four-dot marker plus the label, which is the
correct fallback for a cloud with no tabulated angular size.

Usage:
    python3 make_mbm.py <indir> <outdir>
where <indir> contains mbmcloud.dat, J_ApJS_256_46_table1.dat,
aj2024_t2.dat and cdn.dat.
"""

import math
import os
import re
import sys

# J2000 galactic frame: north pole and galactic centre, both equatorial J2000.
GC_POLE_RA, GC_POLE_DEC = 192.85948, 27.12825
GC_CENTRE_RA, GC_CENTRE_DEC = 266.4051, -28.9362


def _unit(ra, de):
    ra, de = math.radians(ra), math.radians(de)
    return (math.cos(de) * math.cos(ra),
            math.cos(de) * math.sin(ra),
            math.sin(de))


def _cross(u, v):
    return (u[1] * v[2] - u[2] * v[1],
            u[2] * v[0] - u[0] * v[2],
            u[0] * v[1] - u[1] * v[0])


# Galactic basis vectors expressed in equatorial J2000 components.
_ZG = _unit(GC_POLE_RA, GC_POLE_DEC)
_XG = _unit(GC_CENTRE_RA, GC_CENTRE_DEC)
_YG = _cross(_ZG, _XG)
_BASIS = (_XG, _YG, _ZG)


def gal2eq(l, b):
    """Galactic (l, b) degrees -> equatorial (ra, dec) degrees, same frame."""
    l, b = math.radians(l), math.radians(b)
    v = (math.cos(b) * math.cos(l), math.cos(b) * math.sin(l), math.sin(b))
    x = sum(v[j] * _BASIS[j][0] for j in range(3))
    y = sum(v[j] * _BASIS[j][1] for j in range(3))
    z = sum(v[j] * _BASIS[j][2] for j in range(3))
    return math.degrees(math.atan2(y, x)) % 360.0, math.degrees(math.asin(z))


def num(field):
    field = field.strip()
    if not field or field.startswith('.'):
        return None
    try:
        return float(field)
    except ValueError:
        return None


def mbm_number(text):
    m = re.fullmatch(r'MBM\s*0*([0-9]+)', text.strip())
    return int(m.group(1)) if m else None


def parse_schlafly(path):
    """J/ApJ/786/29/mbmcloud.dat, pipe-delimited.
    A cloud can appear on several sight lines; keep the one with the largest
    sight-line count N (column 15), which is the best-sampled position."""
    best = {}
    for line in open(path):
        p = line.split('|')
        n = mbm_number(p[0])
        if n is None or len(p) < 3:
            continue
        l, b = num(p[1]), num(p[2])
        if l is None or b is None:
            continue
        nsl = int(num(p[14]) or 0) if len(p) > 14 else 0
        if n not in best or nsl > best[n][2]:
            best[n] = (l, b, nsl)
    return {n: (v[0], v[1]) for n, v in best.items()}


def parse_sun2021(path):
    """J/ApJS/256/46/table1.dat, free format: MBM <n> <l> <b> <d1> <d2> <d3>."""
    out = {}
    for line in open(path):
        m = re.match(r'^MBM\s*([0-9]+)\s+([0-9.]+)\s+(-?[0-9.]+)', line)
        if m:
            out[int(m.group(1))] = (float(m.group(2)), float(m.group(3)))
    return out


def parse_sun2024(path):
    """J/AJ/168/203/table2.dat, pipe-delimited:
    Cloud | MBM | GLON | GLAT | angRad | GLONM | GLATM | angRadM | flag
    The M-suffixed columns are the original MBM catalog centre and angular
    radius in degrees; angRadM is blank for clouds MBM listed without a radius."""
    out = {}
    for line in open(path):
        p = [x.strip() for x in line.split('|')]
        if len(p) < 8:
            continue
        n = mbm_number(p[1])
        if n is None:
            continue
        l, b, r = num(p[5]), num(p[6]), num(p[7])
        if l is None or b is None:
            continue
        out.setdefault(n, (l, b, r))
    return out


def parse_cdn(path):
    """J/A+A/383/631/cdn.dat, fixed width:
    GLON GLAT RA Dec a[arcmin] b[arcmin] V_LSR Name(s)
    Only the galactic columns and a/b are used; the RA/Dec columns of this
    table are inconsistent with its own galactic columns."""
    out = {}
    for line in open(path):
        m = re.match(r'^\s*([0-9.]+)\s+(-?[0-9.]+)\s+'
                     r'\d{1,2}:\d{2}:\d{2}\s+[+-]?\d{1,2}:\d{2}:\d{2}\s+'
                     r'([0-9.]+)\s+([0-9.]+)\s*\d?\s+(.*)$', line.rstrip('\n'))
        if not m:
            continue
        l, b = float(m.group(1)), float(m.group(2))
        a, bb = num(m.group(3)), num(m.group(4))
        for token in m.group(5).split(','):
            n = mbm_number(token)
            if n is not None:
                out.setdefault(n, (l, b, a, bb))
    return out


def main():
    indir = sys.argv[1] if len(sys.argv) > 1 else '.'
    outdir = sys.argv[2] if len(sys.argv) > 2 else '.'
    need = ('mbmcloud.dat', 'J_ApJS_256_46_table1.dat', 'aj2024_t2.dat', 'cdn.dat')
    for f in need:
        if not os.path.isfile(os.path.join(indir, f)):
            sys.exit('missing input: %s' % os.path.join(indir, f))

    schlafly = parse_schlafly(os.path.join(indir, need[0]))
    sun2021 = parse_sun2021(os.path.join(indir, need[1]))
    sun2024 = parse_sun2024(os.path.join(indir, need[2]))
    cdn = parse_cdn(os.path.join(indir, need[3]))

    numbers = sorted(set(schlafly) | set(sun2021) | set(sun2024) | set(cdn))

    rows = []
    stats = {'schlafly': 0, 'sun2021': 0, 'sun2024': 0, 'cdn': 0,
             'size_radius': 0, 'size_cdn': 0, 'size_none': 0}
    for n in numbers:
        if n in schlafly:
            l, b = schlafly[n]
            stats['schlafly'] += 1
        elif n in sun2021:
            l, b = sun2021[n]
            stats['sun2021'] += 1
        elif n in sun2024:
            l, b, _ = sun2024[n]
            stats['sun2024'] += 1
        else:
            l, b = cdn[n][0], cdn[n][1]
            stats['cdn'] += 1

        ra, de = gal2eq(l, b)
        ra_i = int(round(ra * 2400.0))
        de_i = int(round(de * 3600.0))

        radius = sun2024[n][2] if n in sun2024 else None
        if radius:
            # MBM's own angular radius -> a circle of twice that diameter.
            d = int(round(2.0 * radius * 60.0 * 10.0))
            length, width = d, d
            stats['size_radius'] += 1
        elif n in cdn and cdn[n][2] and cdn[n][3]:
            length = int(round(cdn[n][2] * 10.0))
            width = int(round(cdn[n][3] * 10.0))
            if width > length:
                length, width = width, length
            stats['size_cdn'] += 1
        else:
            length, width = 0, 0
            stats['size_none'] += 1

        rows.append((ra_i, de_i, ['MBM%d' % n], length, width, 999))

    header = ('CCDCIEL MBM EXTRA DATABASE - Magnani-Blitz-Mundy catalogue of '
              'high-latitude molecular clouds. The original catalog '
              '(Magnani, Blitz & Mundy 1984ApJ...282L...9M; 1985ApJ...295..402M) '
              'and Magnani, Smith & Kubat 1996ApJS..106..447M are not in VizieR; '
              'positions are reconstructed from CDS VizieR J/ApJ/786/29 '
              '(Schlafly et al. 2014), J/ApJS/256/46 and J/AJ/168/203 (Sun et al. '
              '2021, 2024) and J/A+A/383/631 (Dutra & Bica 2002), all J2000 '
              'galactic. Angular radii from J/AJ/168/203, fallback axes from '
              'J/A+A/383/631; clouds with no tabulated size are drawn as markers.')

    outpath = os.path.join(outdir, 'mbm.csv')
    with open(outpath, 'w') as out:
        out.write(header + '\n')
        out.write('RA[0..864000], DEC[-324000..324000], name(s), '
                  'length [0.1 min], width[0.1 min], orientation[degrees]\n')
        for ra, de, names, length, width, pa in rows:
            out.write('%d,%d,%s,%d,%d,%d\n' % (ra, de, '/'.join(names),
                                               length, width, pa))

    print('mbm.csv: %d rows' % len(rows))
    print('positions: Schlafly %d, Sun2021 %d, Sun2024 %d, Dutra %d'
          % (stats['schlafly'], stats['sun2021'], stats['sun2024'], stats['cdn']))
    print('sizes: MBM radius %d, Dutra axes %d, none %d'
          % (stats['size_radius'], stats['size_cdn'], stats['size_none']))
    print('MBM number range %d..%d' % (numbers[0], numbers[-1]))


if __name__ == '__main__':
    main()
