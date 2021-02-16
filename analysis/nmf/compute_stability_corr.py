import os
import glob
import pandas as pd
import numpy as np
import scipy
import scipy.io
import pickle
import hdf5storage
import sys
import sklearn
from sklearn.metrics.pairwise import cosine_similarity


n_splits = 10
g = sys.argv[1]

out_dir = "stability_correlations/"
stab_dir = "stability_res/"
options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')

cols = ["Granularity","Iteration","Euclidean_mean","Euclidean_median","Euclidean_std","Corr_mean","Corr_median","Corr_std","Recon_errorA","Recon_errorB"]
df = pd.DataFrame(columns = cols)
for i in range(0,n_splits):
    
    #load split input, run nmf for each split
    fname = stab_dir + "k" + str(g) + "/a_" + str(i) + "_k" + str(g) + ".mat" 
    resA = hdf5storage.loadmat(fname)
    Wa = resA['W']
    ea = resA['err']
        
    fname = stab_dir + "k" + str(g) + "/b_" + str(i) + "_k" + str(g) + ".mat" 
    resB = hdf5storage.loadmat(fname)
    Wb = resB['W']
    eb = resB['err']
    del resA, resB

    #assess correlation of identified parcel component scores - which parcels vary together?
    #c_Wa = cosine_similarity(Wa[0:100,:])
    #c_Wb = cosine_similarity(Wb[0:100,:])
    c_Wa = cosine_similarity(Wa)
    c_Wb = cosine_similarity(Wb)
        
    euclid_dist = np.zeros((1,np.shape(c_Wa)[0]))
    corr = np.zeros((1,np.shape(c_Wa)[0]))

    for parcel in range(0,np.shape(c_Wa)[0]):
        euclid_dist[0,parcel] = scipy.spatial.distance.euclidean(c_Wa[parcel,:], c_Wb[parcel,:])
        corr[0,parcel] = np.corrcoef(c_Wa[parcel,:],c_Wb[parcel,:])[0,1]

    df_curr = pd.DataFrame(data = [[g, i+1, np.mean(euclid_dist), np.median(euclid_dist),np.std(euclid_dist),
                                    np.mean(corr),np.median(corr),np.std(corr),ea,eb]], columns = cols)
    print(i, np.mean(corr))
    df = df.append(df_curr)
    fname = out_dir + "temp_stability_corr_k" + str(g) + ".csv"
    df.to_csv(fname)
    del Wa,Wb,ea,eb
    
fname = out_dir + "stability_corr_k" + str(g) + ".csv"
print(fname)
df.to_csv(fname)
del df, df_curr

