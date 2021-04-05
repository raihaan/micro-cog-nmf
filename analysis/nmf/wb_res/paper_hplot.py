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
sys.path.append('../../bin/python_nmf_micro/')
import utils as ox
import seaborn as sns

import matplotlib as mpl
from matplotlib import cm
from matplotlib.ticker import MultipleLocator, FormatStrFormatter, FixedFormatter, FixedLocator
from matplotlib import pyplot as plt

plt.rcParams['font.family'] = 'serif'
plt.rcParams['font.serif'] = ['Times New Roman'] + plt.rcParams['font.serif']

#Read in csv with subject demographics 
df_sorted=pd.read_csv('../../../raw_data/sheets/07-04-20-McGillData_WH_Exprodo-Report_IncExc_CR_CRmed_cham_CRtopfdemeduc_civetpass_slopes_sorted.csv')
options = hdf5storage.Options(oned_as = 'column', matlab_compatible = True, action_for_matlab_incompatible = 'error')


fname=sys.argv[1]
out_dir=sys.argv[2]

def heatmapping_h(data,minn,maxx,cbar_tix,fig_width,fig_height,ylabels,title='',fname=''):

    plt.rc('figure', titlesize=30, dpi=500)  # fontsize of the figure title
    plt.rcParams['xtick.top'] = plt.rcParams['xtick.labeltop'] = False
  
    viridis = cm.get_cmap('viridis', 256)
    newcolors = viridis(np.linspace(0, 1, 256))
    cmap = mpl.colors.ListedColormap(newcolors)
    img = plt.imshow(data,interpolation='nearest', \
    cmap = cmap, origin='upper',vmin=minn,vmax=maxx)
    #Set the axis of the plot so it isn't a long rectangle
    ax = plt.gca()
    ax.set_aspect('auto') #use 'auto' for automatic aspect
    ax.set_xlabel('')
    ax.set_ylabel('Components', fontsize=12)
    
    xmajorLocator = FixedLocator([210, 630, 1040,1480,1900])
    xmajorFormatter = FixedFormatter(['CT','SA','MD','FA','RD'])
    ax.xaxis.set_major_locator(xmajorLocator)
    ax.xaxis.set_major_formatter(xmajorFormatter)
    plt.setp(ax.get_xmajorticklabels(), rotation='horizontal', fontsize=12)
    ymajorLocator = FixedLocator(list(np.arange(0,np.shape(data)[0],1)))
    ymajorFormatter = FixedFormatter(ylabels)
    ax.yaxis.set_major_locator(ymajorLocator)
    ax.yaxis.set_major_formatter(ymajorFormatter)
    plt.setp(ax.get_ymajorticklabels(), fontsize=8)
    ax.set_ylabel('Components', fontsize=12)
    ax.tick_params(axis='y',size=2, pad=50)
    ax.grid(False)
    fig = plt.gcf()
    fig.set_size_inches(fig_width,fig_height)
    #Generate a color bar
    cbar = plt.colorbar(img,cmap=cmap)
    cbar.set_label('Normalized Weight', fontsize=8)
    n_metrics = 5
    n_subj = np.shape(data)[1]/5
    for x in range(1,n_metrics):
        plt.axvline(x=(n_subj*x),c='w',linewidth=0.5)
    
    cbar.set_ticks(np.arange(minn, maxx, cbar_tix))
    cbar.ax.tick_params(labelsize=5)
    if title:
        plt.title(title, fontsize=12)
    if fname:
        plt.savefig(fname, bbox_inches='tight')
        plt.close(fig)
        return
    plt.show()


h=np.asarray(hdf5storage.loadmat(fname)['H'])
n_comps=np.shape(h)[0]
n_subjects=398

ylabels=['Fronto-temporal','Motor','Visual','Parietal','Inferior Frontal',
        'Anterior Frontal','Cingulate','Postcentral','Right Lateralized','Temporal Pole']

#plot and save H weightings
#minn=np.percentile(h.flatten(),1); maxx=np.percentile(h.flatten(),99.5)
fname= out_dir + "/k" + str(n_comps) + "_H_zscore.png"
heatmapping_h(scipy.stats.zscore(h,axis=1),-2.5,2.5,1,6.1,3.5,ylabels=ylabels,fname=fname)

h_norm=np.zeros_like(h)
for r in range(0,np.shape(h)[0]):
    row_avg = np.mean(h[r,:])
    for c in range(0,np.shape(h)[1]):
        h_norm[r,c] = h[r,c]/row_avg
        
fname= out_dir + "/k" + str(n_comps) + "_H_meannormalize.png"
heatmapping_h(h_norm,0,2.01,1,6.1,3.5,ylabels=ylabels, fname=fname)