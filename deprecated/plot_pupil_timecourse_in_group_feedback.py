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
    #for trial type
    #ax.set_ylim(p_mul, p_mul-5)
    #for feedback
    ax.set_ylim(p_mul+3, p_mul-3)
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
    #for trial type 
    #ax.fill_between(time, y1 = p_mul, y2 = p_mul-5, where = (p_fdr < 0.05), facecolor = 'm', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul, y2 = p_mul-5, where = (p_val < 0.05), facecolor = 'g', alpha = 0.2, step = 'pre')
    #for trial type 
    ax.fill_between(time, y1 = p_mul+3, y2 = p_mul-3, where = (p_fdr < 0.05), facecolor = 'm', alpha = 0.46, step = 'pre')
    ax.fill_between(time, y1 = p_mul+3, y2 = p_mul-3, where = (p_val < 0.05), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 9)
    ax.legend(loc='upper left', fontsize = 12)
    ax.set_title(title, fontsize = 20)
#    plt.close()
        
    path = '/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/untrained/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s' %(comp1_label, comp2_label) + '.png', transparent=False)
    

comp1 = 'risk'
#comp2 = 'norisk'
#parameter3 = '_positive'
#parameter4 = '_positive'
parameter3 = '_negative'
parameter4 = '_positive'
comp1_label = comp1 + parameter3 + '_NT'
comp2_label = comp1 + parameter4 + '_NT'


untrained = True

#df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table.csv')
#remove rows
with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/pupil_table_feedback_untrained.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        #if row[1] != "P034" and row[1] != "P334" and  row[1] != "P328":
        #untrained and risk
        if untrained and row[1] != "P019":
        # untrained and prerisk
        #if untrained and row[1] != "P019" and row[1] != "P021" and  row[1] != "P032" and row[1] != "P034" and row[1] != "P040" and row[1] != "P048" and row[1] != "P053" and row[1] != "P059" and row[1] != "P065":
        # untrained and postrisk
        #if untrained and row[1] != "P021" and row[1] != "P032" and  row[1] != "P053" and row[1] != "P058" and row[1] != "P059" and row[1] != "P061" and row[1] != "P064" and row[1] != "P065":
        #negative fb
        #prerisk NT
        #if  row[1] != "P040" and row[1] != "P044" and row[1] != "P053"  and row[1] != "P058" and row[1] != "P061":# and row[1] != "P301" and  row[1] != "P313" and row[1] != "P316":
        #risk NT
        #if row[1] != "P044"  and row[1] != "P048" and row[1] != "P058" and row[1] != "P061":
        #norisk
        #f row[1] != "P040"  and row[1] != "P061" and row[1] != "P044" and row[1] != "P058":
            print('..In step1')
            #for postrisk AT
            #if row[1] != "P301" and row[1] != "P316" and row[1] != "P326" and row[1] != "P338" and row[1] != "P341" and row[1] != "P314" and row[1] != "P320": 
            #for prerisk AT
            #if row[1] != "P301" and row[1] != "P304" and row[1] != "P316" and row[1] != "P322" and row[1] != "P326" and row[1] != "P333":
            #for risk, norisk AT
            if   row[1] != "P301" and row[1] != "P312" and row[1] != "P313" and row[1] != "P316" and row[1] != "P322" and row[1] != "P326" and row[1] != "P338":
                writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')
AT = df[df['group'] == 'normals']
risk_AT =AT[AT['trial_type4'] == 'risk']
risk_negative_AT = risk_AT[risk_AT['feedback_prev'] == 'negative']
#risk_negative_AT = risk_AT[risk_AT['feedback_prev'] == 'positive']
print('risk_AT neg', risk_negative_AT.shape)
NT = df[df['group'] == 'normals']  
#NT = df[df['group'] == 'normals']  
risk_NT =NT[NT['trial_type4'] == 'risk']
#risk_negative_NT = risk_NT[risk_NT['feedback_prev'] == 'negative']
risk_negative_NT = risk_NT[risk_NT['feedback_prev'] == 'positive']
print('risk_NT neg', risk_negative_NT.shape)
print(risk_negative_NT['fname'])
print(risk_negative_AT['fname'])
idx = risk_negative_NT.columns.get_loc('Avg_block_V_m499_m450')
print(idx)
idx = risk_negative_NT.columns.get_loc('Avg_block_V_2151_2200')
print(idx)
print('(risk_negative_AT shape', risk_negative_AT.shape)
print('(risk_positive_AT shape', risk_negative_NT.shape)
#for feedback table indices indicating time intervals are 5 Avg_block_V_m499_m450 and 58 Avg_block_V_2151_2200
risk_negative_NT_mean = risk_negative_NT.iloc[:, 5:58].mean(axis = 0)
risk_negative_NT = risk_negative_NT.iloc[:, 5:58]
risk_negative_AT_mean = risk_negative_AT.iloc[:, 5:58].mean(axis = 0)
risk_negative_AT = risk_negative_AT.iloc[:, 5:58]

risk_AT_stderr = scipy.stats.sem(risk_negative_AT, axis=0)
risk_NT_stderr = scipy.stats.sem(risk_negative_NT, axis=0)
print('risk_AT_stderr', risk_AT_stderr.shape)

#t_stat, p_val = stats.ttest_ind(risk_negative_AT, risk_negative_NT, axis=0)
t_stat, p_val = stats.ttest_rel(risk_negative_AT, risk_negative_NT, axis=0)

p_fdr = mul.fdrcorrection(p_val)[1]
print('p val shape', p_val.shape)
print('p val fdr shape', p_fdr.shape)

time = np.arange(-10,43)
p_mul = 0

fig = plt.figure(figsize=(8, 5))
fig.suptitle('Pupil trained feedback,2-step averaging, ttest', fontsize=25, fontweight='bold')

plot_stat_comparison(comp1=risk_negative_AT_mean, comp2=risk_negative_NT_mean, comp1_stderr=risk_AT_stderr, comp2_stderr=risk_NT_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label}',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='salmon', comp2_color='b')
