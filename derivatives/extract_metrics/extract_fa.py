#this script loads in raw fa .txt files from each subject and:
#1) concatenates each file to build a vertex X subj matrix of raw fa, for both left and right hemisphers
#2) runs a fa ~ intercept + scanner regression to remove scanner effects
#3) builds matrix of vertex X subj of residualized fa 
#4) writes out .mat files for both raw and resid, left and right separately
#Usage python extract_fa.py

## LOAD MODULES/SOFTWARE
import os
import glob
import pandas as pd
import numpy as np

import sys
import pickle
import hdf5storage
from sklearn import linear_model
sys.path.append('../../analysis/bin/python_nmf_micro/')
import utils as ox

#Read in csv with subject demographics 
df_sorted = pd.read_csv('../../raw_data/sheets/07-04-20-McGillData_WH_Exprodo-Report_IncExc_CR_CRmed_cham_CRtopfdemeduc_civetpass_slopes_sorted.csv')
options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')

#LOAD CIVET MASK TO IDENTIFY MIDLINE/ CORPOS COLLOSUM REGION
#IDENTIFY 'VALID VERTICES' - IE VERTICES NOT IN THIS REGION
left_mask = np.loadtxt('../surfsamp/mask_files/CIVET_2.0_mask_left_short.txt')
left_valid = np.where(left_mask==1) # list of valid indices in civet .txt file
left_invalid = np.where(left_mask==0) #list of invalid indices in civet .txt file

right_mask = np.loadtxt('../surfsamp/mask_files/CIVET_2.0_mask_right_short.txt')
right_valid = (np.where(right_mask==1))
right_invalid = (np.where(right_mask==0))
#38561 valid vertices

n_subjects=df_sorted.shape[0] #num rows in spreadsheet
n_vertex=40962

#Load left thicknesses into matrix left_fa
row = df_sorted['oxmg_id'].tolist()[0] #do first row to create matrix, concat from there
fname="../surfsamp/outputs/oxmg_" + str(row) + '/surfsamp/oxmg_' + str(row) + "_mid_left_FA_masked_smooth_clean_rsl.txt"
left_fa = np.loadtxt(fname).reshape(1,n_vertex)
print(np.shape(left_fa)) #1 x 40962

#load thickness file for rest of subjects and concatenate to make subjects X vertices matrix
for row in df_sorted['oxmg_id'].tolist()[1:]:
    fname="../surfsamp/outputs/oxmg_" + str(row) + '/surfsamp/oxmg_' + str(row) + "_mid_left_FA_masked_smooth_clean_rsl.txt"
    x = np.loadtxt(fname).reshape(1,n_vertex)
    left_fa = np.concatenate((left_fa, x), axis=0)
print("raw left has", np.shape(left_fa)[0], "subjects", np.shape(left_fa)[1], "vertices")

#Repeat for right side
row = df_sorted['oxmg_id'].tolist()[0] #do first row to create matrix, concat from there
fname="../surfsamp/outputs/oxmg_" + str(row) + '/surfsamp/oxmg_' + str(row) + "_mid_right_FA_masked_smooth_clean_rsl.txt"
right_fa = np.loadtxt(fname).reshape(1,n_vertex)
print(np.shape(right_fa)) #1 x 40962
#load thickness file for rest of subjects and concatenate to make subjects X vertices matrix
for row in df_sorted['oxmg_id'].tolist()[1:]:
    fname="../surfsamp/outputs/oxmg_" + str(row) + '/surfsamp/oxmg_' + str(row) + "_mid_right_FA_masked_smooth_clean_rsl.txt"
    x = np.loadtxt(fname).reshape(1,n_vertex)
    right_fa = np.concatenate((right_fa, x), axis=0)
print("raw right has", np.shape(right_fa)[0], "subjects", np.shape(right_fa)[1], "vertices")

#valid ct matrices have dimensions subj x validvertices, masked midline vertices removed
n_rows = np.shape(left_fa)[0] #same for right side
n_cols = np.shape(left_valid)[1] #same for right side

#take all rows (subjects), but only columns which are valid vertices
left_fa_valid = left_fa[:, left_valid].reshape(n_rows,n_cols)
right_fa_valid = right_fa[:, right_valid].reshape(n_rows,n_cols)


#write out in .mat format
#nmf will want vertex X subj, so transpose
out_matrix = np.transpose(np.asmatrix(left_fa_valid))
fname = "left_fa_raw.mat"
hdf5storage.savemat(fname, {'X': out_matrix}, format='7.3', options=options)
del out_matrix

out_matrix = np.transpose(np.asmatrix(right_fa_valid))
fname = "right_fa_raw.mat"
hdf5storage.savemat(fname, {'X': out_matrix}, format='7.3', options=options)

#Use residualize_mx fcn to regress out scanner effects at each vertex
#write out data
#define vector of 0s and 1s denoting scanner status
scanner = df_sorted["prisma_bool"].values.reshape(n_subjects,1) #0 for verio, 1 for prisma
#residualize_mx takes in a matrix of vertex data, then regresses out scanner variable
left_fa_valid_resid = ox.residualize_mx(left_fa_valid,scanner)
out_matrix = np.transpose(np.asmatrix(left_fa_valid_resid))
fname = "left_fa_resid.mat"
hdf5storage.savemat(fname, {'X': out_matrix}, format='7.3', options=options)
del out_matrix

right_fa_valid_resid = ox.residualize_mx(right_fa_valid,scanner)
out_matrix = np.transpose(np.asmatrix(right_fa_valid_resid))
fname = "right_fa_resid.mat"
hdf5storage.savemat(fname, {'X': out_matrix}, format='7.3', options=options)
del out_matrix
