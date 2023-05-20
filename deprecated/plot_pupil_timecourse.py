import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd
from scipy import stats
import scipy
import statsmodels.stats.multitest as mul
from itertools import combinations
import matplotlib.gridspec as gridspec
from array import array
import subprocess
import csv
#import os.path as op



path = '/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table.csv'

def plot_stat_comparison(comp1, comp2, comp1_stderr, comp2_stderr, p_val, p_fdr, p_mul, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    assert(len(comp1) == len(comp2) == len(time))
#    ax1 = globals()['ax'+str(num)]
    #number of pics in fig
    #num = 1
    #ax = fig.add_subplot(gs[num])
    fig, ax = plt.subplots()
    ax.set_xlim(time[0], time[-1])
    #ax.set_xticks([y + 1 for y in range(1,2)],
    #              labels=[''])
    ax.set_ylim(p_mul-3, p_mul+3)
#    ax.set_xlabel('time (ms)')
    #axis for feedback
    ax.plot([20, 20.001], [-1000, 1000], color='k', linewidth=1, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [-1000, 1000], color='k', linewidth=1, linestyle='--', zorder=1)
    ax.plot([-5000, 5000], [0, 0.001], color='k', linewidth=1, linestyle='--', zorder=1)
    ax.plot(time, comp1, color=comp1_color, linewidth=2, label=comp1_label)
    ax.fill_between(time, comp1-comp1_stderr, comp1+comp1_stderr, alpha=.2, facecolor = comp1_color)
    ax.plot(time, comp2, color=comp2_color, linewidth=2, label=comp2_label)
    ax.fill_between(time, comp2-comp2_stderr, comp2+comp2_stderr, alpha=.2, facecolor = comp2_color)
    
    ax.fill_between(time, y1 = p_mul+3, y2 = p_mul-3, where = (p_fdr < 0.05), facecolor = 'm', alpha = 0.46, step = 'pre')
    ax.fill_between(time, y1 = p_mul+3, y2 = p_mul-3, where = (p_val < 0.05), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 9)
    ax.legend(loc='upper left', fontsize = 12)
    ax.set_title(title, fontsize = 20)
#    plt.close()
        
    path = '/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s' %(comp1_label, comp2_label) + '.png', transparent=False)
    

comp1 = 'risk'
comp2 = 'norisk'
comp1_label = comp1 + '_AT'
comp1_label = comp2 + '_AT'
#omp2_label = comp1 + '_NT'
#parameter3 = 'positive'
#parameter4 = 'positive'

parameter3 = None
parameter4 = None

#df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table.csv')
#remove rows
with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        #if row[1] != "P034" and row[1] != "P334" and  row[1] != "P328":
        if row[1] != "P318" and  row[1] != "P326":
            writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')
#print(df)
AT = df[df['group'] == 'autists']
risk_AT = AT[AT['trial_type4'] == 'risk']
norisk_AT = AT[AT['trial_type4'] == 'norisk']
#print(risk_AT['fname'])
NT = df[df['group'] == 'normals']  
risk_NT =NT[NT['trial_type4'] == 'postrisk']
# dropping passed values
#for risk remove P034
#print(risk_NT.iloc[6,1])
#print(dff['fname'])
#idex = risk_NT.columns.get_loc('Avg_block_V_2151_2200')
#print(idex)
#idex = risk_NT.columns.get_loc('Avg_block_V_m499_m450')
#print(idex)
#decision -400 0 ms to 2200 ms
risk_NT_mean = risk_NT.iloc[:, 4:57].mean(axis = 0)
risk_NT = risk_NT.iloc[:, 0:57]
risk_AT_mean = risk_AT.iloc[:, 4:57].mean(axis = 0)
norisk_AT_mean = norisk_AT.iloc[:, 4:57].mean(axis = 0)


risk_AT = risk_AT.iloc[:, 0:57]
norisk_AT = norisk_AT.iloc[:, 0:57]

#print(risk_NT)
#print(risk_AT)
#print('risk_AT', risk_AT['fname'])
#print('risk_NT', risk_NT.shape)
#print('norisk_AT fname', norisk_AT)
print('norisk_AT', norisk_AT.shape)
#print('risk_AT fname', risk_AT['fname'])
print('risk_AT', risk_AT.shape)
#print('risk_NT mean shape', risk_NT_mean.shape)
#print('risk_AT mean', risk_AT_mean.shape)

print('riskAT', risk_AT)
risk_AT_stderr = scipy.stats.sem(risk_AT, axis=0)
risk_NT_stderr = scipy.stats.sem(risk_NT, axis=0)
print('risk_AT_stderr', risk_AT_stderr.shape)

#t_stat, p_val = stats.ttest_ind(risk_AT, risk_NT, axis=0)
t_stat, p_val = stats.ttest_rel(risk_AT, norisk_AT, axis=0)

p_fdr = mul.fdrcorrection(p_val)[1]
print('p val shape', p_val.shape)
print('p val fdr shape', p_fdr.shape)

time = np.arange(-10,43)
print(time)
p_mul = 0
exit()
print('time', time.shape)
fig = plt.figure(figsize=(8, 5))

#for subplot set num of rows and cols
#gs = gridspec.GridSpec(nrows=2, ncols=2)
fig.suptitle('Pupil trained,2-step averaging, ttest', fontsize=25, fontweight='bold')

plot_stat_comparison(comp1=risk_AT_mean, comp2=risk_NT_mean, comp1_stderr=risk_AT_stderr, comp2_stderr=risk_NT_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label}',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='salmon', comp2_color='b')
