#this script spits out commands that can be used to create figs to visualize components
#figures are created by civet_create_image scripts, but exact inputs to that script can vary
#this script automates this by spitting out the exact commands to run
#can pipe commands to job list to execute

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

script_dir=sys.path[0]
mask_dir = os.path.dirname(os.path.dirname(script_dir)) + "/derivatives/surfsamp/mask_files/"

#LOAD CIVET MASK TO IDENTIFY MIDLINE/ CORPOS COLLOSUM REGION
#IDENTIFY 'VALID VERTICES' - IE VERTICES NOT IN THIS REGION
left_mask = np.loadtxt(mask_dir + "CIVET_2.0_mask_left_short.txt")
left_valid = np.where(left_mask==1) # list of valid indices in civet .txt file
left_invalid = np.where(left_mask==0) #list of invalid indices in civet .txt file

right_mask = np.loadtxt(mask_dir + "CIVET_2.0_mask_right_short.txt")
right_valid = (np.where(right_mask==1))
right_invalid = (np.where(right_mask==0))

fname = sys.argv[1]
threshold = int(sys.argv[2])
out_dir=sys.argv[3]
if not os.path.exists(out_dir):
    os.mkdir(out_dir)
    
options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')
W = hdf5storage.loadmat(fname)

compnum=np.shape(W['W'])[1]
left_W=W['W'][0:38561,:]
right_W=W['W'][38561:,:]

left_outarray = np.zeros((np.shape(left_mask)[0],compnum))
for comp in range(0,compnum):
    valid_idx = 0
    for idx in range(0,np.size(left_mask)):
        if left_mask[idx] == 1:
            left_outarray[idx,comp] = left_W[valid_idx,comp]
            valid_idx +=1
left_statmap = out_dir + "/left_k" + str(compnum) + ".txt"
np.savetxt(left_statmap,left_outarray)

right_outarray = np.zeros((np.shape(right_mask)[0],compnum))
for comp in range(0,compnum):
    valid_idx = 0
    for idx in range(0,np.size(right_mask)):
        if right_mask[idx] == 1:
            right_outarray[idx,comp] = right_W[valid_idx,comp]
            valid_idx +=1
right_statmap = out_dir + "/right_k" + str(compnum) + ".txt"
np.savetxt(right_statmap,right_outarray)

base = script_dir+ "/create_white_thumbnail_whitehall_rdpu_25p.sh "
thumb_base = script_dir + "/create_white_thumbnail_separatehemi_whitehall_rdpu_25p.sh "
left_obj = mask_dir + "CIVET_2.0_icbm_avg_mid_sym_mc_left.obj "
right_obj = mask_dir + "CIVET_2.0_icbm_avg_mid_sym_mc_right.obj "
for comp in range(0,compnum):
    left_thresh = np.percentile(W['W'][:,comp],threshold); right_thresh = np.percentile(W['W'][:,comp],threshold)
    fig_name = out_dir + "/box_whitebg_comp" + str(comp+1) + "_k" + str(compnum) + "-" + str(threshold) + "p.png"
    separatehemi_fig_box_name = out_dir + "/separatehemi_box_whitebg_comp" + str(comp+1) + "_k" + str(compnum) + "-" + str(threshold) + "p"
    
    cmd = base + left_obj + left_statmap + " " + str(comp+1) + " " + str(left_thresh) + \
    " " + right_obj + right_statmap + " " + str(comp+1) + " " + str(right_thresh) + " " + fig_name
    
    print(cmd)

    cmd = thumb_base + left_obj + left_statmap + " " + str(comp+1) + " " + str(left_thresh) + \
    " " + right_obj + right_statmap + " " + str(comp+1) + " " + str(right_thresh) + " " + separatehemi_fig_box_name
    print(cmd)
