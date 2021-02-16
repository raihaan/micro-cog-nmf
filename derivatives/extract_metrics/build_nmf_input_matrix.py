#this python script loads in residulized data for each metric and hemisphere
#zscores data for each metric across subjects, per vertex
#then concatenates and writes out matrices for nmf usage
#Usage: python build_nmf_input_matrix.py

## LOAD MODULES/SOFTWARE
import os
import glob
import pandas as pd
import numpy as np

import sys
import pickle
import hdf5storage
import scipy
from scipy import stats

options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')

leftinput_list=["left_ct","left_sa","left_md","left_fa","left_rd"]
rightinput_list=["right_ct","right_sa","right_md","right_fa","right_rd"]

#for each metric, load in residualized data, zscore, and shift
#note the unshifted data of each metric is concateanted and all data shifted by same amoutn later
#but each metric shifted and written out in case needed later as well
left_z_dict = {}
for file in leftinput_list:
    fname = file + "_resid.mat"
    res = hdf5storage.loadmat(fname) #load residualized data
    #x_z = scipy.stats.zscore(res['X'], axis = 0)
    x_z = np.asarray(stats.zscore(res['X'],axis=1)) #zscore, per vertex, across subjects
    left_z_dict[file] = x_z 
    x_z_s = x_z - np.min(x_z) #shift so all positive
    fname = "z_shift_" + file + ".mat" #write out shifted
    print(fname)
    hdf5storage.savemat(fname, {'X': x_z_s}, format='7.3', options=options)

#repeat for right
right_z_dict = {}
for file in rightinput_list:
    fname = file + "_resid.mat"
    res = hdf5storage.loadmat(fname) #load residualized data
    #x_z = scipy.stats.zscore(res['X'], axis = 0)
    x_z = np.asarray(stats.zscore(res['X'],axis=1)) #zscore, per vertex, across subjects
    right_z_dict[file] = x_z 
    x_z_s = x_z - np.min(x_z) #shift so all positive
    fname = "z_shift_" + file + ".mat" #write out shifted
    print(fname)
    hdf5storage.savemat(fname, {'X': x_z_s}, format='7.3', options=options)
    
#for each hemisphere, concatenate each metric matrix together
#forms vertex X subject*5 (5 metrics) matrix for each of left and right
file=leftinput_list[0]
print(file)
left_all = left_z_dict[file]
for file in leftinput_list[1:]:
    print(file)
    left_all = np.concatenate((left_all, left_z_dict[file]),axis=1)

#shift and write out the left side in case needed
left_all_shift = left_all - np.min(left_all)
fname = "z_shift_left_ct-sa-md-fa-rd.mat"
print(fname, np.shape(left_all_shift), np.min(left_all_shift))
hdf5storage.savemat(fname, {'X': left_all_shift}, format='7.3', options=options)
del left_all_shift

#repeat for right
file=rightinput_list[0]
print(file)
right_all = right_z_dict[file]
for file in rightinput_list[1:]:
    print(file)
    right_all = np.concatenate((right_all, right_z_dict[file]),axis=1)

right_all_shift = right_all - np.min(right_all)
fname = "z_shift_right_ct-sa-md-fa-rd.mat"
print(fname, np.shape(right_all_shift), np.min(right_all_shift))
hdf5storage.savemat(fname, {'X': right_all_shift}, format='7.3', options=options)
del right_all_shift

#Concatenate the left and right hemisphere data to get whole brain data
wb_all = np.concatenate((left_all,right_all),axis=0)
wb_all_shift = wb_all - np.min(wb_all)

#write out z scored, un shifted data for stability analysis (to be shifted within each split later)
fname = "z_wb_ct-sa-md-fa-rd.mat"
print(fname, np.shape(wb_all), np.min(wb_all))
hdf5storage.savemat(fname, {'X': wb_all}, format='7.3', options=options)

#write out z scored, shifted data fow whole group nmf analysis
fname = "z_shift_wb_ct-sa-md-fa-rd.mat"
print(fname, np.shape(wb_all_shift), np.min(wb_all_shift))
hdf5storage.savemat(fname, {'X': wb_all_shift}, format='7.3', options=options)
