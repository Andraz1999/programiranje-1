from functools import lru_cache

test_matrix = [
    [1, 2, 0],
    [2, 4, 5],
    [7, 0, 1]
]

def max_cheese(cheese_matrix, x=0, y=0):
    if cheese_matrix == []:
        return 0
    sirina = len(cheese_matrix[0])
    dolzina = len(cheese_matrix)
    if x == sirina - 1 and y == dolzina - 1:
        return cheese_matrix[y][x]
    if x == sirina - 1:
        return cheese_matrix[y][x] + max_cheese(cheese_matrix, x, y + 1)
    if y == dolzina - 1:
        return cheese_matrix[y][x] + max_cheese(cheese_matrix, x + 1, y)
    return cheese_matrix[y][x] + max(max_cheese(cheese_matrix, x+1, y), max_cheese(cheese_matrix, x, y+1))

# druga možnost
def max_sir(matrix):
    max_row = len(matrix)
    try:
        max_col = len(matrix[0])
    except IndexError:
        return 0
    @lru_cache(maxsize=None)
    def max_index(row, col):
        if row == max_row or col == max_col: 
            return 0
        return matrix[row][col] + max(max_index(row + 1, col), max_index(row, col + 1))
    return max_index(0,0)

import sys
sys.setrecursionlimit(10**9)
print(max_sir(
[[j for j in range(2000)] for _ in range(20)]
))