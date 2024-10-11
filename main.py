from coefs_from_dist import DistParam_LinReg
import numpy as np

# linear regression test __________________________________________________________________________---
# create data
data = np.zeros((1000, 5), dtype = np.float16)
for i in range(0, data.shape[1]):
    data[:,i] = np.random.normal(0,1,1000)
X = data[:, [0,1,2,3]]
Y = data[:,-1]
"""
    X1         X2        X3       X4         Y
[[ 1.002     2.79     -0.836     1.446     0.7085  ]
 [-1.489     0.412    -0.2808   -2.055    -0.4902  ]
 [ 0.07446   0.0528   -0.332    -2.28      1.227   ]
 ...
 [ 0.07446   0.0528   -0.332    -2.28      1.227   ]]
"""


dist = DistParam_LinReg(X,Y)  # create instance
fitted, dist_params = dist.fit(10, return_dist_params = True)  # choose coefficients, estimare coefficeint distribution parameters
y_pred = dist.predict(X, fitted)  # make predictions
print(y_pred)