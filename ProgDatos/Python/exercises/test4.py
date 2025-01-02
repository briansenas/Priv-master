import numpy as np
from sklearn import datasets


def mask_x(v, x):
    # dependiendo de si el valor v[i,j] es, respectivamente, menor, igual o mayor que el valor de v[x,j]
    greater = np.where(v[:, :] > v[x,:], 1, -1)
    greater[v[:, :] == v[x, :]] = 0
    return greater


def eratostenes(N):
    """
    1. generar un ndarray con los valores entre 1 y N. En nuestro caso, pr ejemplo, 10 millones
    2. eliminar los valores de 2 en 2, luego de 3 en 3, y así sucesivamente. Mientras sea posible.
    Los que queden serán los números primos menores que N
    """
    arr = np.arange(2, N)
    old = len(arr)
    i = 2
    cont = 0
    while True:
        old = len(arr)
        indexes = np.arange(0, len(arr), i)
        arr = np.delete(arr, indexes, axis = 0)
        i += 1
        if len(arr) == old and cont != 0:
            break
        cont += 1
    return arr



def get_iris_info():
    iris = datasets.load_iris()
    # Para obtener la media:
    mean_data = np.mean(iris.data, axis=0)
    # Para normalizar los valores a 0 y 1
    maxi = np.max(iris.data, axis = 0)
    mini = np.min(iris.data, axis = 0)
    normalized = (iris.data - mini) / (maxi - mini)
    # Para filtrar el tipo 1
    flor_1 = iris.data[iris.target == 1]
    return mean_data, normalized, flor_1


print(eratostenes(10))
