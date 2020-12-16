## convert 'npy/scaler_y_model_NI.pkl' to 'npy/scaler_y_model_NI.npy'

import pickle
import numpy as np

with open('npy/scaler_y_model_NI.pkl', 'rb') as file:
    scaler = pickle.load(file)

   
np.save(file='npy/scaler_y_model_NI.npy', arr=np.vstack([scaler.mean_, scaler.var_]))
    
    
