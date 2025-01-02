def contar_letras(palabra: str, letra: str):
    cont = 0
    for char in palabra:
        if char == letra:
            cont += 1
    return cont


def contar_palabras(cadena: str):
    cont = 0
    for word in cadena.split(' '):
        cont += 1

    return cont

def anagrama(palabra1: str, palabra2: str):
    def count_chars(word):
        count = {}
        for char in word:
            if char in count.keys():
                count[char] += 1
            else:
                count[char] = 1
        return count

    count1 = count_chars(palabra1)
    count2 = count_chars(palabra2)
    if len(count1.keys()) != len(count2.keys()):
        return False
    for key, value in count1.items():
        if key not in count2.keys() or value != count2[key]:
            return False
    return True


def todas_las_letras(palabra: str, letras: str):
    for char in letras:
        palabra = palabra.replace(char, '', 1)

    return len(palabra) == 0


def suma_digitos(cad):
    cad = str(cad)
    count = 0
    for char in cad:
        if char.isdigit():
            count += int(char)
        else:
            count += 0
    return count


assert contar_letras("perro", "r") == 2
assert contar_letras("perro", "e") == 1
assert contar_palabras("hola mundo") == 2
assert anagrama("marta", "trama")
assert not anagrama("marta", "tramas")
assert suma_digitos("123") == 6
assert suma_digitos(1) == 1
assert todas_las_letras("perro", "perro")
assert not todas_las_letras("perro", "pero")
