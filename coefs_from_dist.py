

# linear regression_____________________________________________________________________________
import numpy as np
import statsmodels.api as sm
from sklearn.linear_model import LogisticRegression


class DistParam_LinReg:
    def __init__(self, X, Y):
        self.X = X
        self.Y = Y
    
    def fit(self, n_samples, return_dist_params = False):
        coefs = np.zeros((self.X.shape[1]+1, n_samples), dtype = np.float16)
        means = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        variances = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        final_coefs = np.zeros(means.shape[0], dtype = np.float16)

        for i in range(0, n_samples):
            subX = self.X[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0])), :]
            subY = self.Y[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0]))]
            subX = sm.add_constant(subX)
            model = sm.OLS(subY, subX).fit()
            coefficients = model.params

            for j in range(0, self.X.shape[1]+1):
                coefs[j,i] = coefficients[j]
        

        for i in range(0, coefs.shape[0]):
            means[i] = np.mean(coefs[i,:])
            variances[i] = np.var(coefs[i,:])

        for i in range(0, final_coefs.shape[0]):
            final_coefs[i] = np.random.normal(means[i], variances[i], size = 1)
        

        if return_dist_params == True:
            return final_coefs, np.array([means, variances], dtype = np.float16)
        else:
            return final_coefs
    
    def predict(self, X_new, final_coefs):
        X_new = sm.add_constant(X_new)
        Y_pred = np.zeros(X_new.shape[0], dtype = np.float16)
        k = 0
        for i in range(0, Y_pred.shape[0]):
            Y_pred[i] = final_coefs[0]
            for j in range(1, final_coefs.shape[0]):
                Y_pred[i] = Y_pred[i] + self.X[i,j-1]
        
        return Y_pred
    


# logistic regression_____________________________________________________________________________________________________________
class DistParam_LogReg:
    def __init__(self, X, Y):
        self.X = X
        self.Y = Y
    
    def fit(self, n_samples, return_dist_params = False):
        coefs = np.zeros((self.X.shape[1]+1, n_samples), dtype = np.float16)
        means = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        variances = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        final_coefs = np.zeros(means.shape[0], dtype = np.float16)

        for i in range(0, n_samples):
            subX = self.X[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0])), :]
            subY = self.Y[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0]))]
            subX = sm.add_constant(subX)
            model = sm.Logit(subY, subX).fit()
            coefficients = model.params

        for j in range(0, self.X.shape[1]+1):
                coefs[j,i] = coefficients[j]
        

        for i in range(0, coefs.shape[0]):
            means[i] = np.mean(coefs[i,:])
            variances[i] = np.var(coefs[i,:])

        for i in range(0, final_coefs.shape[0]):
            final_coefs[i] = np.random.normal(means[i], variances[i], size = 1)
        

        if return_dist_params == True:
            return final_coefs, np.array([means, variances], dtype = np.float16)
        else:
            return final_coefs
        
    
    def predict(self, X_new, final_coefs, threshold = 0.5):
        X_new = sm.add_constant(X_new)

        def logistic_function(x):
            return 1 / (1 + np.exp(-x))
        
        linear_combination = np.dot(X_new, final_coefs)
        y_pred_raw = logistic_function(linear_combination)
        y_pred = (y_pred_raw > threshold).astype(np.int8)
        return y_pred
    

    
# polynomial regression_______________________________________________________________________________________________________
class DistParam_PolyReg:
    def __init__(self,X,Y, degree):
        """
        Enter with a single X vector, then depending on the value of degree,
        the consecutive powers of X will be appended in the next columns.
        """
        self.X = X
        self.Y = Y
        self.degree = degree
        
        
    def fit(self, n_samples, return_dist_params = False):

        def poly(X, degree):
            X_poly = np.zeros((X.shape[0],len(range(0,degree))), dtype = np.float16)
                
            for i in range(0, degree):
                X_poly[:,i] = X**(i+1)

            return X_poly

        coefs = np.zeros((self.X.shape[1]+1, n_samples), dtype = np.float16)
        means = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        variances = np.zeros(self.X.shape[1]+1, dtype = np.float16)
        final_coefs = np.zeros(means.shape[0], dtype = np.float16)

        for i in range(0, n_samples):
            subX = self.X[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0])), :]
            subX = poly(subX, degree = self.degree)
            subY = self.Y[list(np.random.choice(range(0, self.X.shape[0]),self.X.shape[0]))]
            subX = sm.add_constant(subX)
            model = sm.OLS(subY, subX).fit()
            coefficients = model.params

        for j in range(0, self.X.shape[1]+1):
                coefs[j,i] = coefficients[j]
            

        for i in range(0, coefs.shape[0]):
            means[i] = np.mean(coefs[i,:])
            variances[i] = np.var(coefs[i,:])

        for i in range(0, final_coefs.shape[0]):
            final_coefs[i] = np.random.normal(means[i], variances[i], size = 1)
            

        if return_dist_params == True:
            return final_coefs, np.array([means, variances], dtype = np.float16)
        else:
            return final_coefs
    
    def predict(self, X_new):
        Y_pred = np.zeros(X_new.shape[0], dtype = np.float16)

        if np.unique(X_new[:,0]) != 1.0:
            X_new = sm.add_constant(X_new)

        for i in range(0, Y_pred.shape[0]):
            Y_pred[i] = self.final_coefs[0]
            for j in range(1, self.final_coefs.shape[0]):
                Y_pred[i] = Y_pred[i] + self.final_coefs[j]*X_new[i,j]
            
        return Y_pred