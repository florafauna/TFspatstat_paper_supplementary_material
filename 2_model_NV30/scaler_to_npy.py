## convert 'npy/scaler_y_model_NV30.pkl' to 'npy/scaler_y_model_NV30.npy'

import pickle
import numpy as np

with open('npy/scaler_y_model_NV30.pkl', 'rb') as file:
    scaler = pickle.load(file)

   
np.save(file='npy/scaler_y_model_NV30.npy', arr=np.vstack([scaler.mean_, scaler.var_]))
    
    
