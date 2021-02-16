##imports
import os
import numpy as np
import scipy
import scipy.stats
import hdf5storage
import sys
import sklearn
import pandas as pd
from sklearn.decomposition import NMF
from sklearn import preprocessing

input_fname = sys.argv[1]
compnum = int(sys.argv[2])
res_dir = sys.argv[3]
res_base = sys.argv[4]  
options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')
X = hdf5storage.loadmat(input_fname)['X']

#run nmf
model = NMF(n_components=compnum, init='nndsvd', max_iter=1000000)
#model = NMF(n_components=compnum, init='nndsvd', max_iter=10000)
model.fit(np.asmatrix(X))
W_vox = model.fit_transform(np.asmatrix(X)) # spatial components - each col of W is a component
H_subj = model.components_ #subject-metric weights, describing patterns of each comp

print(compnum, np.shape(W_vox), model.reconstruction_err_, model.n_iter_)

res={}
res['W']=W_vox
res['H']=H_subj
res['err']=model.reconstruction_err_
res['iter']=model.n_iter_
res['comp']=compnum
#save results
fname=res_dir + "/" + res_base + ".mat"
hdf5storage.savemat(fname, res, format='7.3', options=options)
