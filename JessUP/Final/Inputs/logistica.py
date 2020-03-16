# -*- coding: utf-8 -*-
"""logistica.ipynb.txt

Automatically generated by Colaboratory.

Original file is located at
    https://colab.research.google.com/drive/1P7cUGqD_-oboXGa3opSFSS4TCeKkvUw9

# Regresión Logística
Se muestran dos ejemplos de regresión logística: clasificación binaria y clasificación multi-clase.

### (1) Regresión Logística Binaria
Se utiliza el conjunto de datos "Titanic" provisto originalmente en una competencia Kaggle:
https://www.kaggle.com/c/titanic/

#### Importación de librerías
"""

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

"""#### Datos"""

datos = pd.read_csv("titanic_train.csv")
datos.head(8)

"""#### Limpieza del conjunto de datos
* Los valores NaN de la edad se reemplazan por la media de la edad
* Se elimina la columna de la cabina
* Se eliminan los registros con valores faltantes
"""

datos['Age'] = datos['Age'].fillna(int(datos['Age'].mean()))
datos.drop("Cabin",inplace=True,axis=1)
datos.dropna(inplace=True)

datos.head(8)

"""Se cambian los valores categóricos a valores numéricos (binarización)"""

sex = pd.get_dummies(datos["Sex"],drop_first=True)
embarked = pd.get_dummies(datos["Embarked"],drop_first=True)
pclass = pd.get_dummies(datos["Pclass"],drop_first=True)

datos = pd.concat([datos,pclass,sex,embarked],axis=1)

datos.head(8)

"""Se eliminan las columnas que no se utilizarán en el modelo"""

datos.drop(["PassengerId","Pclass","Name","Sex","Ticket","Embarked"],axis=1,inplace=True)

datos.head(8)

"""#### Creación del modelo"""

x = datos.drop("Survived",axis=1)
y = datos["Survived"]

from sklearn.model_selection import train_test_split
x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state = 0)

# Crear modelo de regresión logística

from sklearn.linear_model import LogisticRegression

modelo = LogisticRegression(max_iter = 1000)
modelo.fit(x_train, y_train)

"""#### Evaluación del modelo
Como referencia, se puede utilizar el recurso siguiente:
https://towardsdatascience.com/multi-class-metrics-made-simple-part-i-precision-and-recall-9250280bddc2
"""

# Estimar la clase
y_pred = modelo.predict(x_test)
print(y_pred)

# Estimar la probabilidad de clase
p_pred = modelo.predict_proba(x_test)
print(p_pred)

# Mostrar las estadísticas del desempeño
from sklearn.metrics import classification_report

print(classification_report(y_test, y_pred))

# Mostrar la matriz de confusión

import seaborn as sns

score = modelo.score(x_test,y_test)

sns.heatmap(cm, annot=True, fmt=".1f", linewidths=2, square = True, cmap = 'Blues_r');
plt.ylabel('Actual label');
plt.xlabel('Predicted label');
all_sample_title = 'Accuracy Score: {0}'.format(score)
plt.title(all_sample_title, size = 15);

"""### (2) Regresión Logística Multi-clase
Se utiliza el conjunto de datos de dígitos escritos a mano MNIST

#### Datos
"""

from sklearn.datasets import load_digits
digitos = load_digits()

import numpy as np 
import matplotlib.pyplot as plt

plt.figure(figsize=(20,4))

for i, (imagen, etiqueta) in enumerate(zip(digitos.data[0:5], digitos.target[0:5])):
    plt.subplot(1, 5, i + 1)
    plt.imshow(np.reshape(imagen, (8,8)), cmap = plt.cm.gray)
    plt.title('Training: %i\n' % etiqueta, fontsize = 20)

"""#### Creación del modelo"""

from sklearn.model_selection import train_test_split
x_train, x_test, y_train, y_test = train_test_split(digitos.data, digitos.target, test_size=0.25, random_state=0)

# Crear modelo de regresión logística

"""#### Evaluación del modelo
Como referencia, se puede utilizar el recurso siguiente:
https://towardsdatascience.com/multi-class-metrics-made-simple-part-i-precision-and-recall-9250280bddc2
"""

# Estimar la clase

# Estimar la probabilidad de clase

from sklearn.metrics import classification_report

print(classification_report(y_test,y_pred))

from sklearn.metrics import confusion_matrix
cm = confusion_matrix(y_test,y_pred)

print(cm)

import seaborn as sns

score = modelo.score(x_test,y_test)

sns.heatmap(cm, annot=True, fmt=".1f", linewidths=2, square = True, cmap = 'Blues_r');
plt.ylabel('Actual label');
plt.xlabel('Predicted label');
all_sample_title = 'Accuracy Score: {0}'.format(score)
plt.title(all_sample_title, size = 15);

# Índice en el subconjunto de datos de prueba x_test
indice = 9

plt.imshow(np.reshape(x_test[indice], (8,8)), cmap = plt.cm.gray)
plt.title('Etiqueta: %i\nPrediccion: %i\n' % (y_test[indice],y_pred[indice]), fontsize = 20)

