from functools import lru_cache

###############################################################################
# Napisite funkcijo [najdaljse_narascajoce_podazporedje], ki sprejme seznam in
# poisce najdaljse (ne strogo) narascajoce podzaporedje stevil v seznamu.
#
# Na primer: V seznamu [2, 3, 6, 8, 4, 4, 6, 7, 12, 8, 9] je najdaljse naj vrne
# rezultat [2, 3, 4, 4, 6, 7, 8, 9].
###############################################################################


def najdaljse_narascajoce_podzaporedje(sez):
    if sez == []:
        return []
    return []

###############################################################################
# Nepreviden študent je pustil robotka z umetno inteligenco nenadzorovanega.
# Robotek želi pobegniti iz laboratorija, ki ga ima v pomnilniku
# predstavljenega kot matriko števil:
#   - ničla predstavlja prosto pot
#   - enica predstavlja izhod iz laboratorija
#   - katerikoli drugi znak označuje oviro, na katero robotek ne more zapeljati

# Robotek se lahko premika le gor, dol, levo in desno, ter ima omejeno količino
# goriva. Napišite funkcijo [pobeg], ki sprejme matriko, ki predstavlja sobo,
# začetno pozicijo in pa število korakov, ki jih robotek lahko naredi z
# gorivom, in izračuna ali lahko robotek pobegne. Soba ima vedno vsaj eno
# polje.
#
# Na primer za laboratorij:
# [[0, 1, 0, 0, 2],
#  [0, 2, 2, 0, 0],
#  [0, 0, 2, 2, 0],
#  [2, 0, 0, 2, 0],
#  [0, 2, 2, 0, 0],
#  [0, 0, 0, 2, 2]]
#
# robotek iz pozicije (3, 1) pobegne čim ima vsaj 5 korakov, iz pozicije (5, 0)
# pa v nobenem primeru ne more, saj je zagrajen.
###############################################################################

soba = [[0, 1, 0, 0, 2],
        [0, 2, 2, 0, 0],
        [0, 0, 2, 2, 0],
        [2, 0, 0, 2, 0],
        [0, 2, 2, 0, 0],
        [0, 0, 0, 2, 2]]


def pobeg(soba, pozicija, koraki):
    @lru_cache(maxsize=None)
    def min_korak(pozicija):
        max_vod = len(soba[0])
        max_nav = len(soba)
        vodoravno = pozicija[1]
        navpicno = pozicija[0]
        if soba[navpicno][vodoravno] == 1:
            return 0
        mozna = []
        for dx in [-1, 0, 1]:
            for dy in [-1, 0, 1]:
                if abs(dx) == abs(dy):
                    pass
                elif navpicno + dx >= max_nav or vodoravno + dy >= max_vod or navpicno + dx < 0 or vodoravno + dy < 0:
                    pass
                elif soba[navpicno + dx][vodoravno + dy] != 0 and soba[navpicno + dx][vodoravno + dy] != 1:
                    pass 
                else:
                    mozna.append((navpicno + dx, vodoravno + dy))
        return 1 + min([min_korak(par) for par in mozna])
    return koraki >= min_korak(pozicija)

#pobeg(sob, (3, 1), 5)   
pobeg([[1,0,0]], (0, 2), 5)
