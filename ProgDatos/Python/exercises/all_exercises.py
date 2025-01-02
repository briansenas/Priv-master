# startregion exercise_1
def contar_letras(palabra, letra):
    return sum([char == letra for char in palabra])


assert contar_letras("hola", "a") == 1
assert contar_letras("holo", "a") == 0
assert contar_letras("hola mundo muy cruel", "o") == 2


# endregion exercise_1
# startregion exercise_2
def eliminar_palabras(palabra, letra):
    return "".join([char for char in palabra if char != letra])


assert eliminar_palabras("hola", "a") == "hol"
assert eliminar_palabras("hola", "b") == "hola"
assert eliminar_palabras("hola mundo muy cruel", "o") == "hla mund muy cruel"


# endregion exercise_2
# startregion exercise_3
def vocales(palabra):
    return set(palabra).intersection({"a", "e", "i", "o", "u"})


assert vocales("hola") == {"a", "o"}
assert not vocales("bcdfgh")


# endregion exercise_3
# startregion exercise_4
def es_inversa(palabra1, palabra2):
    if len(palabra1) != len(palabra2):
        return False
    return sum(
        [char1 == char2 for char1, char2 in zip(palabra1[::-1], palabra2)]
    ) == len(palabra1)


assert es_inversa("hola", "aloh")
assert not es_inversa("hola", "alob")
assert not es_inversa("hola", "adios")
assert es_inversa("", "")
# endregion exercise_4
""  # startregion exercise_5


def comunes(palabra1, palabra2):
    caracteres = set(palabra2)
    return "".join([char1 for char1 in palabra1 if char1 in caracteres])


assert comunes("hola", "mundo") == "o"
assert comunes("hola", "aloha") == "hola"


# endregion exercise_5
# startregion exercise_6
def palindromo(frase):
    return es_inversa(frase, frase)


assert palindromo("aba") == True
assert palindromo("saippuakivikauppias") == True


# endregion exercise_6
# startregion exercise_7
def orden_alfabetico(palabra):
    return all([char < char2 for char, char2 in zip(palabra, palabra[1::])])


assert orden_alfabetico("abcd") == True
assert orden_alfabetico("abcda") == False


# endregion exercise_7
# startregion exercise_8
def todas_las_letras(palabra, letras):
    counter = {}
    for char in letras:
        if char in counter.keys():
            counter[char] += 1
        else:
            counter[char] = 1
    for char in palabra:
        if char in counter.keys():
            counter[char] -= 1
            if counter[char] < 0:
                return False
    return sum(counter.values()) == 0


assert todas_las_letras("hola", "a") == True
assert todas_las_letras("hola", "b") == False
assert todas_las_letras("hola", "hola") == True
assert todas_las_letras("hola", "hola mundo muy cruel") == False


# endregion exercise_8
# startregion exercise_9
def anagrama(palabra1, palabra2):
    def counter(word):
        res = {}
        for char in word:
            if char in res:
                res[char] += 1
            res[char] = 1
        return res

    count1 = counter(palabra1)
    count2 = counter(palabra2)
    return sum(count1.values()) == sum(count2.values())


assert anagrama("New York Times", "monkeys write")
assert anagrama("Church of Scientology", "rich-chosen goofy cult")
assert not anagrama("hola mundo", "muy cruel")


# endregion exercise_9
# startregion exercise_10
def suma_digitos(cad):
    if isinstance(cad, int):
        temp = cad
        res = 0
        while True:
            rem = temp % 10
            div = temp // 10
            res += rem
            temp = div
            if div <= 0:
                break
        return res
    else:
        return sum([int(char) for char in cad])


assert suma_digitos("123") == 6
assert suma_digitos(1) == 1
assert suma_digitos(103) == 4


# endregion exercise_10
# startregion exercise_11
def contar_numeros_impares(numeros):
    return sum(1 for num in numeros if num % 2 == 1)


assert contar_numeros_impares([1, 2, 3, 4, 5]) == 3
assert contar_numeros_impares([2, 4, 6]) == 0


# endregion exercise_11
# startregion exercise_12
def contar_letras(palabra):
    res = {}
    for char in palabra:
        if char in res:
            res[char] += 1
        res[char] = 1
    return list(res.items())


assert contar_letras("hola") == [("h", 1), ("o", 1), ("l", 1), ("a", 1)]


# endregion exercise_12
# startregion exercise_13
def mezclar(lista1, lista2):
    i = j = 0
    res = []
    while i < len(lista1) and j < len(lista2):
        if lista1[i] <= lista2[j]:
            res.append(lista1[i])
            i += 1
        else:
            res.append(lista2[j])
            j += 1
    if i < len(lista1):
        res += lista1[i:]
    if j < len(lista2):
        res += lista2[j:]
    return res


assert mezclar([1, 2, 3], [2, 3, 4]) == [1, 2, 2, 3, 3, 4]
assert mezclar([1, 2, 3], [4, 5, 6]) == [1, 2, 3, 4, 5, 6]
assert mezclar([1, 2, 3, 4, 5], [1, 2, 3]) == [1, 1, 2, 2, 3, 3, 4, 5]
assert mezclar([1, 2, 3], [1, 2, 3, 4, 5]) == [1, 1, 2, 2, 3, 3, 4, 5]


# endregion exercise_13
# startregion exercise_14
def transpuesta(mat):
    """
    1 2 3
    1 2 3
    1 2 3
    =
    1 1 1
    2 2 2
    3 3 3
    """
    res = []
    rows = len(mat)
    if rows > 0:
        cols = len(mat[0])
    else:
        return []
    for j in range(cols):
        for i in range(rows):
            if i >= len(res):
                res.append([])
            res[i].append(mat[j][i])

    return res


assert transpuesta(
    [
        [1, 2, 3],
        [1, 2, 3],
        [1, 2, 3],
    ]
) == [
    [1, 1, 1],
    [2, 2, 2],
    [3, 3, 3],
]
# endregion exercise_14
# startregion exercise_14b
import numpy as np


def transpuesta_numpy(mat: np.ndarray):
    return mat.T


# endregion exercise_14b
# startregion exercise_15
def dispersa(v):
    res = {}
    for i, val in enumerate(v):
        if val != 0:
            res[i] = val
    return (list(res.items()), len(v))


assert dispersa([0, 0, 1]) == ([(2, 1)], 3)
assert dispersa([0, 0, 0]) == ([], 3)


# endregion exercise_15
# startregion exercise_16
def create_dispersa(v):
    pairs, length = v
    res = [0 for _ in range(length)]
    for key, value in pairs:
        res[key] = value
    return res


assert create_dispersa(([(2, 1)], 3)) == [0, 0, 1]
assert create_dispersa(([], 3)) == [0, 0, 0]


# endregion exercise_16
# startregion exercise_17
def suma_columna(nom_fichero):
    def sum_readlines(file):
        res = 0
        while (line := file.readline()) != "":
            res += int(line)
        return res

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            res = sum_readlines(file)
    else:
        res = sum_readlines(nom_fichero)
    return res


import io

assert suma_columna(io.StringIO("1\n2\n3")) == 6


# endregion exercise_17
# startregion exercise_18
def leer_columna(nom_fichero, col):
    def leer_datos(file, col):
        datos = []
        while line := file.readline():
            datos.append(line.strip("\n").split(",")[col])
        return datos

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            datos = leer_datos(file, col)
    else:
        datos = leer_datos(nom_fichero, col)
    return datos


assert leer_columna(io.StringIO("1,2\n1,2\n1,2"), 1) == ["2", "2", "2"]
assert leer_columna(io.StringIO("1,2\n1,2\n1,2"), 0) == ["1", "1", "1"]


# endregion exercise_18
# startregion exercise_19
def contar_palabras(nom_fichero, palabra):
    def contar(file, palabra):
        res = 0
        while line := file.readline():
            words = line.split(" ")
            res += sum(word.strip("\n") == palabra for word in words)
        return res

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            datos = contar(file, palabra)
    else:
        datos = contar(nom_fichero, palabra)
    return datos


assert contar_palabras(io.StringIO("hola hola\nmundo hola"), "hola") == 3
assert contar_palabras(io.StringIO("hola hola\nmundo hola"), "adios") == 0


# endregion exercise_19
# startregion exercise_20
def copiar_archivos(nom_fichero):
    def copiar(file1):
        file2 = io.StringIO()
        while line := file1.readline():
            file2.write(line)
        return file2

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            datos = copiar(file)
    else:
        datos = copiar(nom_fichero)
    return datos


assert (
    copiar_archivos(io.StringIO("hola\nmundo")).getvalue()
    == io.StringIO("hola\nmundo").getvalue()
)


# endregion exercise_20
# startregion exercise_21
def buscar_texto(nom_fichero, texto):
    def buscar(file):
        res = []
        cont = 1
        while line := file.readline():
            if texto in line:
                res.append([cont, line.strip("\n")])
            cont += 1
        return res

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            datos = buscar(file)
    else:
        datos = buscar(nom_fichero)
    return datos


assert buscar_texto(io.StringIO("hola\nmundo\nadios hola"), "hola") == [
    [1, "hola"],
    [3, "adios hola"],
]


# endregion exercise_21
# startregion exercise_22
def contar_palabras(nom_fichero):
    def contar(file):
        res = {}
        while line := file.readline():
            words = line.strip("\n").split(" ")
            for word in words:
                if word in res:
                    res[word] += 1
                else:
                    res[word] = 1
        return res

    if isinstance(nom_fichero, str):
        with open(nom_fichero, "r") as file:
            datos = contar(file)
    else:
        datos = contar(nom_fichero)
    return datos


assert contar_palabras(io.StringIO("hola\nmundo\nmuy cruel")) == {
    "hola": 1,
    "mundo": 1,
    "muy": 1,
    "cruel": 1,
}
# endregion exercise_22
# startregion exercise_23
import numpy as np


def suma_columnas(v):
    return np.sum(v, axis=1)


assert all(suma_columnas(np.asarray([[1, 2, 3], [1, 2, 3]])) == np.asarray([6, 6]))


# endregion exercise_23
# startregion exercise_24
def filtro(v, x):
    return v[v > x]


assert all(filtro(np.asarray([1, 2, 3, 4, 5]), 2) == np.asarray([3, 4, 5]))


# endregion exercise_24
# startregion exercise_25
def stats(v: np.ndarray):
    mean = v.mean()
    sd = v.std()
    v.sort()
    median = v[len(v) // 2]
    modals, counts = np.unique(v, return_counts=True)
    index = np.argmax(counts)
    mode = modals[index]
    return [mean, sd, median, mode]


# endregion exercise_25
# startregion exercise_26
def funny_order(v, x):
    res = np.zeros(shape=v.shape)
    res[v < v[x,]] = -1
    res[v > v[x,]] = 1
    return res


# endregion exercise_26
# startregion exercise_27
def erastostenes(n):
    mask = np.zeros(shape=(n,))
    mask[0] = True
    for i in range(1, n // 2 + 1):
        mask[range(i + i + 1, n, i + 1)] = True
    return np.where(mask == 0)[0] + 1


# print(erastostenes(1000000))
# endregion exercise_27
# startregion exercise_28
# endregion exercise_28
# startregion exercise_29
import scipy
import math


def func(x):
    return [
        x[0] ** 2 + x[1] ** 2 - 10,
        x[0] * math.exp(2) - 1,
    ]


root = scipy.optimize.fsolve(func, [1, 1])
# endregion exercise_29