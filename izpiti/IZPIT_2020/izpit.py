from functools import lru_cache


# @lru_cache(maxsize=None)
# def korito(n, m, l, zac=True):
#     if n < (l * m) + (m - 1):
#         mult = 0
#     else:
#         mult = 1
#     moznosti = []
#     if n <= 0:
#         return 0
#     if zac:
#         i = 0
#     else:
#         i = 1
#     while i + l <= n:
#         moznosti.append(korito((n - i - l), (m - 1), l, False))
#         i += 1
#     return mult * len(moznosti)

# def korito(n, m, l):
#     counter = 0
#     def pomoc(n, m, l, ali):
#         if n == l and m == 1:
#             return 1



    
# # @lru_cache(maxsize=None)
# def k(n, m, l):
#     stevec = 0
#     def korito(n, m, l, stevec, zac=True):
#         if n < (l * m) + (m - 1):
#             return 
#         if zac:
#             i = 0
#         else:
#             i = 1
#         while n - l - i >= (l * m) + (m - 1):
#             if m == 1:
#                 stevec += 1
#             korito((n - i - l), (m - 1), l, stevec, False)
#             i += 1
#         return stevec
#     return korito(n, m, l, stevec)
        


# korito(9, 3, 2)
# k(9, 3, 2)




def tezja_korita(n, sez):
    @lru_cache(maxsize=None)
    def pomozna(n, zac=True):
    if sez == []:
        return 1
    elif n < sum(sez) + len(sez) - 1:
        return 0
    elif len(sez) == 1 and zac and n == sez[0]:
        return 1
    else:
        moznosti = []
        if zac:
            i = 0
        else:
            i = 1
        while sez[0] + i <= n:
            moznosti.append(pomozna(n, sez[1:], False))# Tukaj nisem imel vec casa, da bi popravil za lru_cache, moral bi narediti stevec, ki se premika naprej, ne pa sez[1:]...
    return sum(moznosti) 


def korito(n, m, l):
    sez = [l for i in range(m)]
    return tezja_korita(n, sez, zac=True)



        