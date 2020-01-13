ocaml = [
    '05-funkcijsko-programiranje',
    '06-definicije-tipov',
    '07-ucinki-in-cistost',
    '09-modularnost',
]
psa = [
    '08-iskalna-drevesa',
    '10-spremenljive-podatkovne-strukture',
    '11-deli-in-vladaj',
    '12-dinamicno-programiranje',
    '13-memoizacija',
]
import random
while True:
    input('Si pripravljen na vprašanje iz funkcijskega programiranja?')
    print(random.choice(ocaml))
    a = input('Si pripravljen na vprašanje iz podatkovnih struktur in algoritmov?')
    print(random.choice(psa))
    if a == 'Znam':
        print('Dobil si 10')
      
