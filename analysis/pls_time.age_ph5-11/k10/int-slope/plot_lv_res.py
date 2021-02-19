import os
import glob
import pandas as pd
import numpy as np

import sys
import pickle
import hdf5storage
import matplotlib as mpl
from matplotlib import cm
from matplotlib.ticker import MultipleLocator, FormatStrFormatter, FixedFormatter, FixedLocator


from matplotlib import pyplot as plt

#load in an lv, set behavs and components text once, will be same for all
lv_fname = sys.argv[1]
out_base = sys.argv[2]
lv_ = hdf5storage.loadmat(lv_fname)
components_list = []
for idx in range(0,len(lv_['components'].tolist()[0])):
    components_list.append( str(lv_['components'].tolist()[0][idx][0]) )

    behavs_list = []
for idx in range(0,len(lv_['behavs_text'].tolist()[0])):
    behavs_list.append( str(lv_['behavs_text'].tolist()[0][idx][0]) )
    
    
mycolors = ['b','b','olivedrab','olivedrab','maroon','maroon','y','y',
           'darkviolet','darkviolet','c','c','g','g']

err = np.concatenate( (lv_['lower'], lv_['upper']))

fig, ax = plt.subplots(1,1, figsize = (7,13),dpi = 500)
plt.subplots_adjust(wspace = 0.3)

y_pos = np.arange(len(lv_['lvcorrs'].tolist()[0]))
p = ax.barh(y_pos,lv_['lvcorrs'].tolist()[0], xerr = err, align = 'center', color = mycolors)
ax.yaxis.set_major_formatter(FixedFormatter(behavs_list))
ax.yaxis.set_major_locator(FixedLocator(y_pos))
ax.tick_params(axis='both', labelsize=20)
ax.set_xlabel("Correlation",fontsize=20)
ax.grid(which='both', color='#CCCCCC')
ax.set_facecolor('white')
ax.set_title(lv_fname.split(".")[0], fontsize=20)
plt.savefig(out_base + "_behavs.png",bbox_inches='tight', dpi = 'figure')



fig, ax = plt.subplots(1,2, figsize = (10,10),dpi = 500)
plt.subplots_adjust(wspace = 0.3)

y_pos = np.arange(len(lv_['bsr'].tolist()[0][0:25]))
ax[0].barh(y_pos,lv_['bsr'].tolist()[0][0:25], align = 'center')
ax[0].yaxis.set_major_formatter(FixedFormatter(components_list[0:25]))
ax[0].yaxis.set_major_locator(FixedLocator(y_pos))
ax[0].tick_params(axis='x', labelsize=10)
ax[0].tick_params(axis='y', labelsize=10)
if( np.max(lv_['bsr']) > 1.9):
    ax[0].axvline(x=2.58, color = 'black', ymin=0.03, ymax = 0.97)
    ax[0].axvline(x=1.96, color = 'blue', ymin=0.03, ymax = 0.97)
if( np.min(lv_['bsr']) < -1.9):
    ax[0].axvline(x=-2.58, color = 'black', ymin=0.03, ymax = 0.97)
    ax[0].axvline(x=-1.96, color = 'blue', ymin=0.03, ymax = 0.97)
ax[0].grid(which='both', color='#CCCCCC')
ax[0].set_facecolor('white')
y_pos = np.arange(len(lv_['bsr'].tolist()[0][25:50]))
ax[1].barh(y_pos,lv_['bsr'].tolist()[0][25:50], align = 'center')
ax[1].yaxis.set_major_formatter(FixedFormatter(components_list[25:50]))
ax[1].yaxis.set_major_locator(FixedLocator(y_pos))
ax[1].tick_params(axis='x', labelsize=10)
ax[1].tick_params(axis='y', labelsize=10)
if( np.max(lv_['bsr']) > 1.9):
    ax[1].axvline(x=2.58, color = 'black', ymin=0.03, ymax = 0.97)
    ax[1].axvline(x=1.96, color = 'blue', ymin=0.03, ymax = 0.97)
if( np.min(lv_['bsr']) < -1.9):
    ax[1].axvline(x=-2.58, color = 'black', ymin=0.03, ymax = 0.97)
    ax[1].axvline(x=-1.96, color = 'blue', ymin=0.03, ymax = 0.97)
ax[1].grid(which='both', color='#CCCCCC')
ax[1].set_facecolor('white')
plt.savefig(out_base + "_bsr.png",bbox_inches='tight', dpi = 'figure')
