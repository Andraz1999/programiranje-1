###############################################################################
# Želimo definirati pivotiranje na mestu za tabelo [a]. Ker bi želeli
# pivotirati zgolj dele tabele, se omejimo na del tabele, ki se nahaja med
# indeksoma [start] in [end].
#
# Primer: za [start = 0] in [end = 8] tabelo
#
# [10, 4, 5, 15, 11, 2, 17, 0, 18]
#
# preuredimo v
#
# [0, 2, 5, 4, 10, 11, 17, 15, 18]
#
# (Možnih je več različnih rešitev, pomembno je, da je element 10 pivot.)
#
# Sestavi funkcijo [pivot(a, start, end)], ki preuredi tabelo [a] tako, da bo
# element [ a[start] ] postal pivot za del tabele med indeksoma [start] in
# [end]. Funkcija naj vrne indeks, na katerem je po preurejanju pristal pivot.
# Funkcija naj deluje v času O(n), kjer je n dolžina tabele [a].
#
# Primer:
#
#     >>> a = [10, 4, 5, 15, 11, 2, 17, 0, 18]
#     >>> pivot(a, 1, 7)
#     3
#     >>> a
#     [10, 2, 0, 4, 11, 15, 17, 5, 18]
###############################################################################



def pivot(table, start, end):
    left_i = start 
    right_i = end
    pivot = table[start]
    while left_i < right_i:
        if table[left_i + 1] < pivot:
            left_i += 1
        elif table[right_i] >= pivot:
            right_i -= 1
        else: table[left_i + 1], table[right_i] = table[right_i], table[left_i + 1]
    table[right_i], table[start]  = pivot, table[right_i]
    return right_i 




a = [10, 4, 5, 15, 11, 2, 17, 0, 18]


###############################################################################
# V tabeli želimo poiskati vrednost k-tega elementa po velikosti.
#
# Primer: Če je
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#
# potem je tretji element po velikosti enak 5, ker so od njega manši elementi
#  2, 3 in 4. Pri tem štejemo indekse od 0 naprej, torej je "ničti" element 2.
#
# Sestavite funkcijo [kth_element(a, k)], ki v tabeli [a] poišče [k]-ti
# element po velikosti. Funkcija sme spremeniti tabelo [a]. Cilj naloge je, da
# jo rešite brez da v celoti uredite tabelo [a].
###############################################################################

def kth_element(table, k, start= 0, end= None):
    if end is None:
        end = len(table) - 1
    x = pivot(table, start, end)
    if x == k: 
        return table[k]
    elif x > k:
        return kth_element(table, k, start, x)
    else:
        return kth_element(table, k - x, x, end)

a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
###############################################################################
# Tabelo a želimo urediti z algoritmom hitrega urejanja (quicksort).
#
# Napišite funkcijo [quicksort(a)], ki uredi tabelo [a] s pomočjo pivotiranja.
# Poskrbi, da algoritem deluje 'na mestu', torej ne uporablja novih tabel.
#
# Namig: Definirajte pomožno funkcijo [quicksort_part(a, start, end)], ki
#        uredi zgolj del tabele [a].
#
#     >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
#     >>> quicksort(a)
#     [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def quicksort(a, start= 0, end= None):
    if end is None:
        end = len(a) - 1
    if start >= end:
        return a 
    else:
        x = pivot(a, start, end)
        quicksort(a, start, x - 1)
        quicksort(a, x + 1, end)


###############################################################################
# Če imamo dve urejeni tabeli, potem urejeno združeno tabelo dobimo tako, da
# urejeni tabeli zlijemo. Pri zlivanju vsakič vzamemo manjšega od začetnih
# elementov obeh tabel. Zaradi učinkovitosti ne ustvarjamo nove tabele, ampak
# rezultat zapisujemo v že pripravljeno tabelo (ustrezne dolžine).
# 
# Funkcija naj deluje v času O(n), kjer je n dolžina tarčne tabele.
# 
# Sestavite funkcijo [zlij(target, begin, end, list_1, list_2)], ki v del 
# tabele [target] med start in end zlije tabeli [list_1] in [list_2]. V primeru, 
# da sta elementa v obeh tabelah enaka, naj bo prvi element iz prve tabele.
# 
# Primer:
#  
#     >>> list_1 = [1,3,5,7,10]
#     >>> list_2 = [1,2,3,4,5,6,7]
#     >>> target = [-1 for _ in range(len(list_1) + len(list_2))]
#     >>> zlij(target, 0, len(target), list_1, list_2)
#     >>> target
#     [1,1,2,3,3,4,5,5,6,7,7,10]
#
###############################################################################
def zlij(target, begin, end, list_1, list_2):
    if begin == end:
        return target
    elif list_1 == [] and list_2 == []:
        return 'Failwith'
    elif list_1 == []:
        target[begin] = list_2[0]
        return zlij(target, begin + 1, end, list_1, list_2[1:])
    elif  list_2 == []:
        target[begin] = list_1[0]
        return zlij(target, begin + 1, end, list_1[1:], list_2)
    elif list_1[0] <= list_2[0]:
        target[begin] = list_1[0]
        return zlij(target, begin + 1, end, list_1[1:], list_2)
    else:
        target[begin] = list_2[0]
        return zlij(target, begin + 1, end, list_1, list_2[1:])


###############################################################################
# Tabelo želimo urediti z zlivanjem (merge sort). 
# Tabelo razdelimo na polovici, ju rekurzivno uredimo in nato zlijemo z uporabo
# funkcije [zlij].
#
# Namig: prazna tabela in tabela z enim samim elementom sta vedno urejeni.
#
# Napišite funkcijo [mergesort(a)], ki uredi tabelo [a] s pomočjo zlivanja.
# Za razliko od hitrega urejanja tu tabele lahko kopirate, zlivanje pa je 
# potrebno narediti na mestu.
#
# >>> a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
# >>> mergesort(a)
# [2, 3, 4, 5, 10, 11, 15, 17, 18]
###############################################################################

def mergesort(a):
    if len(a) <= 1:
        return a
    else:
        target = [0 for _ in range(len(a))]
        pol = round(len(a) / 2)
        levi = a[:pol]
        desni = a[pol:]
        return zlij(target, 0, len(target), mergesort(levi), mergesort(desni))  
a = [10, 4, 5, 15, 11, 3, 17, 2, 18]
mergesort(a)