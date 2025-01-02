import pandas as pd
import numpy as np


def temp():
    pass


from sklearn.datasets import load_digits
from sklearn.preprocessing import StandardScaler
from sklearn.decomposition import PCA
from sklearn.svm import SVC
from sklearn.pipeline import Pipeline
from sklearn.model_selection import GridSearchCV

X, y = load_digits(return_X_y=True)

scoring = ['precision_macro', 'recall_macro', 'f1_macro']

param_grid = {
    # aquí definiriamos los parametros
    'svc__C': [1e4, 1e3, 1e2],
    'svc__gamma': [0.0001, 0.001, 0.01]
}
pipe = Pipeline(
    [
        ("scaler", StandardScaler()),
        ("pca", PCA()),
        ("svc", SVC(random_state=0)),
    ]
)
clf = GridSearchCV(
    pipe,
    param_grid,
    cv=5
)

cinco = np.array([
    [0,  9, 12, 12, 12, 12,  7,  0],
    [0, 11,  0,  0,  0,  0,  0,  0],
    [0, 11, 12, 12, 11,  9,  0,  0],
    [0,  0,  0,  0,  0, 13,  3,  0],
    [0,  0,  0,  0,  0, 11,  6,  0],
    [0,  0,  0,  0,  0, 10,  7,  0],
    [0,  7,  9,  8, 11, 12,  2,  0],
    [0,  2, 10, 12, 10,  2,  0,  0]
])
cinco = cinco.reshape(1, 64)
print(cinco.shape)
clf.fit(X, y)
pred = clf.predict(cinco)
print(pred)
