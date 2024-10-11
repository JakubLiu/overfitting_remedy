import numpy as np
import statsmodels.api as sm
data = np.zeros((1000, 5), dtype = np.float16)

for i in range(0, data.shape[1]):
    data[:,i] = np.random.normal(0,1,1000)

"""
    X1         X2        X3       X4         Y
[[ 1.002     2.79     -0.836     1.446     0.7085  ]
 [-1.489     0.412    -0.2808   -2.055    -0.4902  ]
 [ 0.07446   0.0528   -0.332    -2.28      1.227   ]
 ...
 [ 0.07446   0.0528   -0.332    -2.28      1.227   ]]
"""

def Fit(n_samples, X, Y):
    coefs = np.zeros((X.shape[1]+1, n_samples), dtype = np.float16)
    means = np.zeros(X.shape[1]+1, dtype = np.float16)
    variances = np.zeros(X.shape[1]+1, dtype = np.float16)
    final_coefs = np.zeros(means.shape[0], dtype = np.float16)

    for i in range(0, n_samples):
        subX = X[list(np.random.choice(range(0, X.shape[0]),X.shape[0])), :]
        subY = Y[list(np.random.choice(range(0, X.shape[0]),X.shape[0]))]
        subX = sm.add_constant(subX)
        model = sm.OLS(subY, subX).fit()
        coefficients = model.params

        for j in range(0, X.shape[1]+1):
            coefs[j,i] = coefficients[j]
    

    for i in range(0, coefs.shape[0]):
        means[i] = np.mean(coefs[i,:])
        variances[i] = np.var(coefs[i,:])

    for i in range(0, final_coefs.shape[0]):
        final_coefs[i] = np.random.normal(means[i], variances[i], size = 1)
    
    return final_coefs



def Predict(X, coefficients):
    Y_pred = np.zeros(X.shape[0], dtype = np.float16)
    k = 0
    for i in range(0, Y_pred.shape[0]):
        Y_pred[i] = coefficients[0]
        for j in range(1, coefficients.shape[0]):
            Y_pred[i] = Y_pred[i] + X[i,j-1]
    
    return Y_pred

X = data[:, [0,1]]
Y = data[:,-1]
final_coefficients = Fit(100, X, Y)
Y_hat = Predict(X, final_coefficients)
print(Y_hat.shape, X.shape)
