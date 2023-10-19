import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error
import statsmodels.api as sm

dataset = pd.read_csv("Python/Estadistica Multivariada/salarios.txt")

x, y = dataset.iloc[:, :1].values, dataset.iloc[:, 1].values

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state=0)

lr = LinearRegression()
lr.fit(x_train, y_train)

ypred_test = lr.predict(x_test)
ypred_train = lr.predict(x_train)

ECM = mean_squared_error(y_test, ypred_test)
EAM = mean_absolute_error(y_test, ypred_test)
print("ECM: ", ECM)
print("EAM: ", EAM)

"""plt.scatter(x_train, y_train, color = "red")
plt.plot(x_train, lr.predict(x_train), color = "blue")
plt.title("Salario vs Años de experiencia (Conjunto de entrenamiento)")
plt.xlabel("Años de experiencia")
plt.ylabel("Salario")
plt.show()"""

b1 = lr.coef_
b0 = lr.intercept_
print("b1: ", b1)
print("b0: ", b0)

# y = b0 + b1*x

mean_residuo = np.mean(ypred_test - y_test)
print("Media de los residuos: ", mean_residuo)

"""plt.scatter(y_train, y_train - lr.predict(x_train), color = "red")
plt.title("Residuos vs Valores ajustados")
plt.xlabel("Valores ajustados")
plt.ylabel("Residuos")
plt.show()"""

residuos = ypred_train - y_train
sm.qqplot(residuos, line = "45", fit=True)
plt.show()