def numeros_pares(numeros):
    return [num for num in numeros if num % 2 == 0]


def numeros_pares_suma(numeros):
    pares = [num for num in numeros if num % 2 == 0]
    return pares, sum(pares)


def cadena_mas_larga(cadenas):
    return max(cadenas, key = lambda x: len(x))


def longs(cadenas):
    return [len(x) for x in cadenas]


def eliminar(l1, l2):
    elementos = set(l2)
    return [num for num in l1 if num not in elementos]


def combinar(la, lb):
    # Creo que no se podía usar sort...
    return sorted(la + lb)

assert numeros_pares([1, 2, 3, 4]) == [2, 4]
assert numeros_pares([1, 3]) == []

assert numeros_pares_suma([1, 2, 3, 4]) == ([2, 4], 6)
assert numeros_pares_suma([1, 3]) == ([], 0)

assert cadena_mas_larga(["hola", "mundo", "muy", "cruel!"]) == "cruel!"
assert longs(["hola", "mundo"]) == [4, 5]

assert eliminar([1,2,3], [2,3]) == [1]

assert(combinar([1,2,3], [4, 5])) == [1,2,3,4,5]
