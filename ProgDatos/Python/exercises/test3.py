def suma_columnas(nom_fichero: str):
    total = 0
    with open(nom_fichero, "r") as file:
        while n := file.readline():
            total += int(n)

    return total


def num_lineas(nom_fichero: str):
    lineas = 0
    with open(nom_fichero, "r") as file:
        lineas = sum(1 for line in file)

    return lineas


def escribe_coordenadas(nom_fich, c):
    with open(nom_fich, "w") as file:
        for x, y in c:
            file.write(f"{x},{y}\n")


def punto_medio(nom_fichero):
    """ Suponemos un archivo con coordenadas separadas por ',' """
    with open(nom_fichero, "r") as file:
        mx = my = n = 0
        for line in file:
            x, y = line.split(',')
            x, y = int(x), int(y)
            mx += x
            my += y
            n += 1

        return mx / n, my / n


def decodificar(fich_texto, fich_posiciones):
    decodificados = []
    with open(fich_texto, "r") as textos, open(fich_posiciones, "r") as posiciones:
        for texto, posicion in zip(textos, posiciones):
            cadena = "".join(texto[int(x)] for x in posicion.split(" "))
            decodificados.append(cadena)

    return decodificados


print(suma_columnas("sumar_1234.txt"))
print(num_lineas("sumar_1234.txt"))
escribe_coordenadas("test3_pares.txt", [(1, 2), (3, 4)])
print(punto_medio("test3_pares.txt"))
print(decodificar("test3_fich_texto.txt", "test3_fich_posiciones.txt"))
