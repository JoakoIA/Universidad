import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import mean_squared_error
from sklearn.metrics import mean_absolute_error
from sklearn.preprocessing import StandardScaler
import statsmodels.api as sm

dataset = pd.read_csv("Python/Estadistica Multivariada/bodyfat.csv")

x, y = dataset.iloc[:, 3:].values, dataset.iloc[:, 1].values

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = 0.3, random_state=0)

"""std_scalaer = StandardScaler()
x_train = std_scalaer.fit_transform(x_train)
x_test = std_scalaer.transform(x_test)
"""
x_stat = sm.add_constant(x_train)
model = sm.OLS(y_train, x_stat)
result = model.fit()
print(result.summary())