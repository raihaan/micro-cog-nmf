#this script builds input matrices for each split of data to be used in stability analysis
#for splits 1 to 10, define an A input and B input
#nmf is run on each of A and B, and spatial outputs compared for stability
#the indices (ie subjects) for the A and B matrices in each split are predefined
#computed in the define_splits_n10.ipynb notebook

#load them here, load residualized data and build input matrices

## LOAD MODULES/SOFTWARE
import os
import glob
import pandas as pd
import numpy as np

import sys
import pickle
import hdf5storage
import scipy
import scipy.stats

options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')

#load the residual data, store in dictionary with hemisphere/metric description as key
#this data is residualized, but not yet z scored or shifted
leftinput_list=["left_ct","left_sa","left_md","left_fa","left_rd"]
rightinput_list=["right_ct","right_sa","right_md","right_fa","right_rd"]
left_dict = {}
for file in leftinput_list:
    fname = "../../../../derivatives/extract_metrics/" + file + "_resid.mat"
    left_dict[file] = np.asarray(hdf5storage.loadmat(fname)['X'])

right_dict = {}
for file in rightinput_list:
    fname = "../../../../derivatives/extract_metrics/" + file + "_resid.mat"
    right_dict[file] = np.asarray(hdf5storage.loadmat(fname)['X'])
    

#now build the A and B matrices for each split
#take all rows, but only the columns (subjects) specified by the predefined indices

#reload saved indices, which were previously generated and checked 
fname="Asplits_indices.mat"
Asplits_indices=hdf5storage.loadmat(fname)
fname="Bsplits_indices.mat"
Bsplits_indices=hdf5storage.loadmat(fname)

n_splits = 10
#for each split, build required indices
#because of metric "chunks", if a split has index 0, get index 0 (ct), 398 (sa), 796 (md) etc
#ie get idx, idx + m*n_subjects, for m = 1:5
for split in range(0, n_splits):
    
    #get left ct data
    metric=leftinput_list[0]
    data_all = left_dict[metric]
    #get data_a and data_b, containing ct data for A indicies and B indices
    data_a = data_all[:,Asplits_indices[str(split)]]; data_b = data_all[:,Bsplits_indices[str(split)]]
    #z score each 
    a_mx_left = scipy.stats.zscore(data_a,axis=1)
    b_mx_left = scipy.stats.zscore(data_b,axis=1)
    
    #repeat for each left metric
    for metric in leftinput_list[1:]:
        data_all = left_dict[metric]
        data_a = data_all[:,Asplits_indices[str(split)]]; data_b = data_all[:,Bsplits_indices[str(split)]]
        data_a_z = scipy.stats.zscore(data_a,axis=1) #zscore
        data_b_z = scipy.stats.zscore(data_b,axis=1)
        a_mx_left = np.concatenate((a_mx_left,data_a_z),axis=1) #append z scored data for this metric to the rest
        b_mx_left = np.concatenate((b_mx_left,data_b_z),axis=1)
    
    #repeat for right side
    #get right ct data
    metric=rightinput_list[0]
    data_all = right_dict[metric]
    #get data_a and data_b, containing ct data for A indicies and B indices
    data_a = data_all[:,Asplits_indices[str(split)]]; data_b = data_all[:,Bsplits_indices[str(split)]]
    #z score each 
    a_mx_right = scipy.stats.zscore(data_a,axis=1)
    b_mx_right = scipy.stats.zscore(data_b,axis=1)
    
    #repeat for each right metric
    for metric in rightinput_list[1:]:
        data_all = right_dict[metric]
        data_a = data_all[:,Asplits_indices[str(split)]]; data_b = data_all[:,Bsplits_indices[str(split)]]
        data_a_z = scipy.stats.zscore(data_a,axis=1) #zscore
        data_b_z = scipy.stats.zscore(data_b,axis=1)
        a_mx_right = np.concatenate((a_mx_right,data_a_z),axis=1) #append z scored data for this metric to the rest
        b_mx_right = np.concatenate((b_mx_right,data_b_z),axis=1)
    
    #concat left and right mx to get whole brain data
    a_mx_wb = np.concatenate((a_mx_left,a_mx_right),axis=0)
    b_mx_wb = np.concatenate((b_mx_left,b_mx_right),axis=0)
    #shift each to be non negative
    a_mx_shift_wb = a_mx_wb - np.min(a_mx_wb)
    b_mx_shift_wb = b_mx_wb - np.min(b_mx_wb)
    
    #write out
    fname = "a_" + str(split) + ".mat"
    print(fname, np.min(a_mx_shift_wb), np.shape(a_mx_shift_wb))
    hdf5storage.savemat(fname, {'X': a_mx_shift_wb}, format='7.3', options=options)
    fname = "b_" + str(split) + ".mat"
    print(fname, np.min(b_mx_shift_wb), np.shape(b_mx_shift_wb))
    hdf5storage.savemat(fname, {'X': b_mx_shift_wb}, format='7.3', options=options)
    
    
    