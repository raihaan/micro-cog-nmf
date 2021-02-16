import os
import glob
import pandas as pd
import numpy as np
import sys


#plot stability and error gradient on same plot 

df_stab = pd.read_csv(sys.argv[1])


split_corr = []
for g in np.arange(2,21,2):
    split_corr.append(df_stab.loc[df_stab['Granularity'] == g][['Corr_mean']].values)
    
plt_arr = np.zeros((1,np.shape(split_corr)[0]))
plt_std_arr = np.zeros((1,np.shape(split_corr)[0]))
for g in range(0,9):
    plt_arr[0,g] = np.mean(split_corr[g])
    plt_std_arr[0,g] = np.std(split_corr[g])
    

dict_errorgrad = {'Granularity' : np.arange(4,21,2).flatten()}
dict_errorgrad['Index'] = np.arange(0, np.shape(dict_errorgrad['Granularity'])[0], 1)
for iter in range(1,11):
    dict_errorgrad["A_iter" + str(iter)] = np.diff(df_stab.loc[df_stab['Iteration'] == iter][['Recon_errorA']].values.flatten(), axis=0).tolist()
    dict_errorgrad["B_iter" + str(iter)] = np.diff(df_stab.loc[df_stab['Iteration'] == iter][['Recon_errorB']].values.flatten(), axis=0).tolist()
df_errorgrad = pd.DataFrame(data=dict_errorgrad, index = np.arange(1,10).flatten())

error_grad_arr = np.zeros((1,np.shape(np.arange(4,21,2))[0]))
error_grad_std_arr = np.zeros((1,np.shape(np.arange(4,21,2))[0]))
for idx in range(0,9):
    error_grad_arr[0,idx] = np.mean(df_errorgrad.iloc[idx,2:])
    error_grad_std_arr[0,idx] = np.std(df_errorgrad.iloc[idx,2:])


stab_x = np.arange(2,21,2)
grad_err_x = np.arange(4,21,2)
fig, ax1 = plt.subplots(figsize=(16, 8), dpi=100)

color = 'tab:red'
ax1.set_xlabel('Number of Components', fontsize = 25)
ax1.xaxis.set_major_locator(matplotlib.ticker.MultipleLocator(5))
ax1.set_ylabel('Stability Coefficient', color=color, fontsize = 25)
ax1.errorbar(stab_x,plt_arr.flatten(),yerr=plt_std_arr.flatten(), c=color, marker=".", lw=2, ms = 10)

ax1.tick_params(axis='y', labelcolor=color, labelsize=20)
ax1.tick_params(axis='x', labelsize=20)

ax2 = ax1.twinx()  # instantiate a second axes that shares the same x-axis

color = 'tab:blue'
ax2.set_ylabel('Gradient(Reconstruction Error)', color=color, fontsize = 25)  # we already handled the x-label with ax1
ax2.errorbar(grad_err_x,error_grad_arr.flatten(),yerr=error_grad_std_arr.flatten(), c=color, marker=".", lw=2, ms = 10)

ax2.tick_params(axis='y', labelcolor=color, labelsize=20)
fig.tight_layout()  # otherwise the right y-label is slightly clipped
plt.title('NMF Stability', fontsize=30)
plt.savefig('stability.png', dpi='figure', bbox_inches='tight')
#plt.show()
