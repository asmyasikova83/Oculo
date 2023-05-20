import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd
from scipy import stats
import scipy
import statsmodels.stats.multitest as mul
#from itertools import combinations
import matplotlib.gridspec as gridspec
#from array import array
#import subprocess
import csv
#import os.path as op
import math



def plot_stat_comparison(group, comp1, comp2, comp1_stderr, comp2_stderr, p_val, p_fdr, p_mul, time, title, folder,
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
    ax.set_ylim(p_mul-2, p_mul+2)
    #ax.set_ylim(p_mul-5, p_mul)
#    ax.set_xlabel('time (ms)')
    #axis for feedback
    ax.plot([20, 20.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot([-5000, 5000], [0, 0.001], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot(time, comp1, color=comp1_color, linewidth=3, label=comp1_label)
    ax.fill_between(time, comp1-comp1_stderr, comp1+comp1_stderr, alpha=.4, facecolor = comp1_color)
    ax.plot(time, comp2, color=comp2_color, linewidth=3, label=comp2_label)
    ax.fill_between(time, comp2-comp2_stderr, comp2+comp2_stderr, alpha=.4, facecolor = comp2_color)
    
    ax.fill_between(time, y1 = p_mul+2, y2 = p_mul-2, where = (p_fdr < 0.05), facecolor = 'm', alpha = 0.46, step = 'pre')
    ax.fill_between(time, y1 = p_mul+2, y2 = p_mul-2, where = (p_val < 0.05), facecolor = 'g', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul, y2 = p_mul-5, where = (p_val < 0.05), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul, y2 = p_mul-5, where = (p_val < 0.05), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 19)
    ax.legend(loc='upper left', fontsize = 22)
    ax.set_title(title, fontsize = 40)
    #    plt.close()
        
    path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/long_timecourse/{group}/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s' %(comp1_label, comp2_label) + '.png', transparent=False)
    

comp1 = 'prerisk'
comp2 = 'norisk'
#parameter3 = '_positive'
#parameter4 = '_positive'
#parameter3 = '_negative'
#parameter4 = '_positive'
parameter3 = ''
parameter4 = ''

untrained = False

#df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table.csv')
with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_trained_no_aver.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')

df = df[df['train'] == 'trained']

if comp1 == 'prerisk':
    subj_list = ['P001', 'P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 'P058', 'P059', 'P061', 'P063', 'P064', 'P065', 'P301', 'P304', 'P307', 'P312', 'P313', 'P314', 'P320', 'P321', 'P322', 'P324', 'P325', 'P326', 'P327', 'P328', 'P329', 'P334', 'P335', 'P338', 'P341']
if comp1 == 'risk':
    subj_list = ['P001', 'P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 'P059', 'P063', 'P064', 'P065',
       'P301', 'P304', 'P307', 'P312', 'P313', 'P314', 'P320', 'P321', 'P322', 'P324', 'P325', 'P326', 'P327', 'P329', 'P335', 'P338', 'P341']
if comp1 == 'postrisk':
    subj_list = ['P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 
        'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P304', 'P307', 'P312', 'P313', 'P314','P320', 'P321', 'P322', 
        'P324', 'P325', 'P326', 'P327', 'P329', 'P333', 'P334', 'P335', 'P338', 'P341']

#P001, P004, P019, P021, P022, P032, P034, P035, P039, P040, P044, P047, P048, P053, P055, P058, P059, P060, P061, P063, P064, P065]
#P301, P304, P307, P312, P313, P314, P316, P320, P321, P322, P323, P324, P325, P326, P327, P328, P329, P333, P334, P335, P338, P341

#p333 <- p3334
#P060 <- P061
#subj_list = ['P001', 'P004', 'P019', 'P022', 'P032', 'P039', 'P040', 'P047', 'P053', 'P055', 'P059', 'P060', 'P063', 'P064', 'P065',
#      'P301', 'P304', 'P307', 'P313', 'P314', 'P321', 'P322', 'P324', 'P326', 'P327', 'P329', 'P333', 'P335', 'P338', 'P341']

#65 = 0, 105 = 2000, 46 = m1000
times_len = df.iloc[:, 46:105].shape

choice_types = [comp2, comp1]

runs = [1,2,3,4,5]

fbs = [True]

contr = np.zeros((len(subj_list), len(choice_types), 1, times_len[1]))

for idx, choice in enumerate(choice_types):
    subj_choice_list_mean = np.empty((0, 1, times_len[1]))
    for subj in subj_list:
        print(subj)
        choice_list = np.empty((0, 1, times_len[1]))
        for run in runs:
            try:
                df_name = df[df['fname'] == subj]
                df_choice = df_name[df_name['trial_type4'] == choice]
                df_block = df_choice[df_choice['block'] == run]
                #df_fb = df_block[df_block['rew'] == True]
                #over trials
                #df_to_vstack = df_fb.iloc[:, 26:125].mean(axis = 0)
                df_to_vstack = df_block.iloc[:, 46:105].mean(axis = 0)
                df_to_vstack = df_to_vstack[np.newaxis, :]
                #print(df_to_vstack.shape)
                if pd.isna(df_to_vstack).any():
                    continue
                else:
                    choice_list = np.vstack([choice_list, df_to_vstack[:,np.newaxis]])
            except (OSError):
                print('This file not exist')

        choice_list_mean = choice_list.mean(axis = 0)
        #if pd.isna(choice_list_mean).any():
        #    print('subj na', subj)
        #    continue
        subj_choice_list_mean =  np.vstack([subj_choice_list_mean, choice_list_mean[:,np.newaxis]]) 
    contr[:, idx, :]  = subj_choice_list_mean

#prepare stst and plotting

N = int(len(subj_list)/2)
print('N', N)

group = 'autists'

if group == 'normals':
    start = 0
    end = N
    comp1_label = comp1 + parameter3 + '_NT'
    comp2_label = comp2 + parameter4 + '_NT'

if group == 'autists':
    start = N+1
    end = 2*N
    comp1_label = comp1 + parameter3 + '_AT'
    comp2_label = comp2 + parameter4 + '_AT'

norisk_NT = contr[start:end, 0, :]
risk_NT = contr[start:end, 1, :]

norisk_NT_mean = norisk_NT.mean(axis=0)
risk_NT_mean = risk_NT.mean(axis=0)

norisk_NT_mean = np.squeeze(norisk_NT_mean, axis=0)
risk_NT_mean = np.squeeze(risk_NT_mean, axis=0)

norisk_NT_stderr = scipy.stats.sem(contr[start:end, 0, :], axis=0)
risk_NT_stderr = scipy.stats.sem(contr[start:end, 1, :], axis=0)

norisk_NT_stderr = np.squeeze(norisk_NT_stderr, axis=0)
risk_NT_stderr = np.squeeze(risk_NT_stderr, axis=0)


#t_stat, p_val = stats.ttest_ind(risk_negative_AT, risk_negative_NT, axis=0)
t_stat, p_val = stats.ttest_rel(norisk_NT, risk_NT, axis=0)

p_val = np.squeeze(p_val, axis=0)
p_fdr = mul.fdrcorrection(p_val)[1]


#!!!! -1000:2000
time = np.arange(-19,40)
p_mul = 0

fig = plt.figure(figsize=(8, 5))
fig.suptitle('Pupil trained, ttest', fontsize=25, fontweight='bold')

plot_stat_comparison(group=group, comp1=risk_NT_mean, comp2=norisk_NT_mean, comp1_stderr=risk_NT_stderr, comp2_stderr=norisk_NT_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label}',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='salmon', comp2_color='b')
