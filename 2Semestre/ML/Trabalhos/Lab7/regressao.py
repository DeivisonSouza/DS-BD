#!/usr/bin/python

#!/usr/bin/python

import numpy as np
import seaborn as sns
import pandas
from sklearn import linear_model
from sklearn.linear_model import LinearRegression
from keras.models import Sequential
from keras.layers import Dense
from sklearn.metrics import mean_squared_error
from sklearn.neighbors import KNeighborsRegressor
from sklearn.tree import DecisionTreeRegressor
from sklearn import svm
from sklearn.svm import SVR
from sklearn import tree
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.ensemble import RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn import preprocessing
import matplotlib.pyplot as plt


# load dataset
print ("Loading Data")
data = pandas.read_csv("usina72.csv")

feature_cols = ['f5', 'f6', 'f7', 'f8', 'f9', 'f10', 'f11', 'f12']
X = data.loc[:,feature_cols]
#label_col = ['f3']
label_col = ['f4']
Y = data.loc[:,label_col]

## split the data into testing and training
X_train, X_test, y_train, y_test = train_test_split(X,Y,test_size=0.5, random_state=2)

print ("Count_X_train:", len(X_train))
print ("Count_y_test:", len(X_test))


### Normalizacao
scaler = preprocessing.MinMaxScaler()
scaler.fit(X_train)
X_train = scaler.transform(X_train)
X_test = scaler.transform(X_test)

#scaler.fit(y_train)
#y_train = scaler.transform(y_train)
#y_test = scaler.transform(y_test)


print ("Fitting Regressor")
lm = LinearRegression(fit_intercept = True)
svr = SVR(kernel='linear', C=1e3)
knn = KNeighborsRegressor(n_neighbors=5, metric='minkowski')
dtr = DecisionTreeRegressor(max_depth=5)
rf = RandomForestRegressor(n_estimators=100)
gbr = GradientBoostingRegressor(n_estimators=500, max_depth=4, learning_rate=0.01)


#### .ravel() converte uma coluna num array 1d
lm.fit(X_train, y_train.values.ravel())
svr.fit(X_train, y_train.values.ravel())
knn.fit(X_train, y_train.values.ravel())
dtr.fit(X_train, y_train.values.ravel())
rf.fit(X_train, y_train.values.ravel())
gbr.fit(X_train, y_train.values.ravel())

#vet_test = np.array(y_test.values.ravel())
y_pred_lm = lm.predict(X_test)
y_pred_svr = svr.predict(X_test)
y_pred_knn = knn.predict(X_test)
y_pred_dtr = dtr.predict(X_test)
y_pred_rf = rf.predict(X_test)
y_pred_gbr = gbr.predict(X_test)



MSE_lm = mean_squared_error(y_test.values.ravel(), y_pred_lm)
MSE_svr = mean_squared_error(y_test.values.ravel(), y_pred_svr)
MSE_knn = mean_squared_error(y_test.values.ravel(), y_pred_knn)
MSE_dtr = mean_squared_error(y_test.values.ravel(), y_pred_dtr)
MSE_rf = mean_squared_error(y_test.values.ravel(), y_pred_rf)
MSE_gbr = mean_squared_error(y_test.values.ravel(), y_pred_gbr)

print("MSE_lm:", MSE_lm)
print("MSE_svr:", MSE_svr)
print("MSE_knn:", MSE_knn)
print("MSE_dtr:", MSE_dtr)
print("MSE_rf:", MSE_rf)
print("MSE_gbr:", MSE_gbr)

#----------------------------------------------------------------------------------------------------------
#### Neural Network\n",
#model = Sequential()
#model.add(Dense(9, input_dim=8, kernel_initializer='normal', activation='relu'))
#model.add(Dense(10, activation='relu'))
#model.add(Dense(1, kernel_initializer='normal'))

#model.compile(loss='mean_squared_error', optimizer='adam')

#model.fit(X_train, y_train, epochs=100, validation_split=0.33, verbose=1)
#y_pred = model.predict(X_test)
#MSE = mean_squared_error(y_test, y_pred)
#print(MSE)
#----------------------------------------------------------------------------------------------------------

vet_test = np.array(y_test.values.ravel())


vet_pred_lm = np.array(y_pred_lm)
vet_pred_svr = np.array(y_pred_svr)
vet_pred_knn = np.array(y_pred_knn)
vet_pred_dtr = np.array(y_pred_dtr)
vet_pred_rf = np.array(y_pred_rf)
vet_pred_gbr = np.array(y_pred_gbr)


vet_res_lm = vet_test - vet_pred_lm
vet_res_svr = vet_test - vet_pred_svr
vet_res_knn = vet_test - vet_pred_knn
vet_res_dtr = vet_test - vet_pred_dtr
vet_res_rf = vet_test - vet_pred_rf
vet_res_gbr = vet_test - vet_pred_gbr


#mean_sd = np.mean(vet_res_lm)*np.std(vet_res_lm)
#print ("mean+-sd_lm: ", mean_sd)
#count = len([i for i in abs(vet_res_lm) if i <= mean_sd])
#print ("count_lm: ", count)

print ("Erro Médio (LM): ", np.mean(vet_res_lm))
print ("Erro Médio (SVR): ", np.mean(vet_res_svr))
print ("Erro Médio (KNN): ", np.mean(vet_res_knn))
print ("Erro Médio (DTR): ", np.mean(vet_res_dtr))
print ("Erro Médio (RF): ", np.mean(vet_res_rf))
print ("Erro Médio (GBR): ", np.mean(vet_res_gbr))

print ("Desvio (LM):", np.std(vet_res_lm))
print ("Desvio (SVR):", np.std(vet_res_svr))
print ("Desvio (KNN):", np.std(vet_res_knn))
print ("Desvio (DTR):", np.std(vet_res_dtr))
print ("Desvio (RF):", np.std(vet_res_rf))
print ("Desvio (GBR):", np.std(vet_res_gbr))


# plot histograma dos resíduos

fig = plt.figure(figsize=(20, 6))

# sintaxe para add subplots
ax1 = fig.add_subplot(231)      #(linha, coluna, indice)
ax2 = fig.add_subplot(232)
ax3 = fig.add_subplot(233)
ax4 = fig.add_subplot(234)
ax5 = fig.add_subplot(235)
ax6 = fig.add_subplot(236)

# Subplot 1 - Linear Regression (LR)
n, bins, patches = ax1.hist(vet_res_lm, 64, density=True, stacked=True)
ax1.axvline(x=np.mean(vet_res_lm), color='black', linestyle='--')
ax1.axvline(x= + np.std(vet_res_lm), color='r', linestyle='--')
ax1.axvline(x= - np.std(vet_res_lm), color='r', linestyle='--')
ax1.set_title('Linear Regression \n (fit_intercept = True)')
ax1.set_ylabel('Density')
ax1.set_xlabel('Residual')


# Subplot 2 - Support Vector Regression (SVR)
n, bins, patches = ax2.hist(vet_res_svr, 64, density=True, color='green', stacked=True)
ax2.axvline(x=np.mean(vet_res_svr), color='black', linestyle='--')
ax2.axvline(x= + np.std(vet_res_svr), color='r', linestyle='--')
ax2.axvline(x= - np.std(vet_res_svr), color='r', linestyle='--')
ax2.set_title('Support Vector Regression (SVR) \n (kernel=linear, C=1e3)')
ax2.set_ylabel('Density')
ax2.set_xlabel('Residual')


# Subplot 3 - K-Neighbors Regressor (KNN)
n, bins, patches = ax3.hist(vet_res_knn, 64, density=True, color='gray', stacked=True)
ax3.axvline(x=np.mean(vet_res_knn), color='black', linestyle='--')
ax3.axvline(x= + np.std(vet_res_knn), color='r', linestyle='--')
ax3.axvline(x= - np.std(vet_res_knn), color='r', linestyle='--')
ax3.set_title('K-Neighbors Regressor (KNN) \n (n_neighbors=5, metric=minkowski)')
ax3.set_ylabel('Density')
ax3.set_xlabel('Residual')


# Subplot 4 - Decision Tree Regressor (DTR)
n, bins, patches = ax4.hist(vet_res_dtr, 64, density=True, color='blue', stacked=True)
ax4.axvline(x=np.mean(vet_res_dtr), color='black', linestyle='--')
ax4.axvline(x= + np.std(vet_res_dtr), color='r', linestyle='--')
ax4.axvline(x= - np.std(vet_res_dtr), color='r', linestyle='--')
ax4.set_title('Decision Tree Regressor (DTR) \n (max_depth=5)')
ax4.set_ylabel('Density')
ax4.set_xlabel('Residual')


# Subplot 5 - Random Forest Regressor (RF)
n, bins, patches = ax5.hist(vet_res_rf, 64, density=True, color='yellow', stacked=True)
ax5.axvline(x=np.mean(vet_res_rf), color='black', linestyle='--')
ax5.axvline(x= + np.std(vet_res_rf), color='r', linestyle='--')
ax5.axvline(x= - np.std(vet_res_rf), color='r', linestyle='--')
ax5.set_title('Random Forest Regressor (RF) \n (n_estimators=100)')
ax5.set_ylabel('Density')
ax5.set_xlabel('Residual')


# Subplot 6 - Gradient Boosting Regressor (GBR)
n, bins, patches = ax6.hist(vet_res_gbr, 64, density=True, color='orange', stacked=True)
ax6.axvline(x=np.mean(vet_res_gbr), color='black', linestyle='--')
ax6.axvline(x= + np.std(vet_res_gbr), color='r', linestyle='--')
ax6.axvline(x= - np.std(vet_res_gbr), color='r', linestyle='--')
ax6.set_title('Gradient Boosting Regressor (GBR) \n (n_estimators=500, max_depth=4, learning_rate=0.01)')
ax6.set_ylabel('Density')
ax6.set_xlabel('Residual')
fig.tight_layout()
plt.show()


# Analisar a quantidade de desvio no intervalo de 1sd. Quanto mais desvios neste intervalo melhor o modelo...Tenho que implementar isso....

