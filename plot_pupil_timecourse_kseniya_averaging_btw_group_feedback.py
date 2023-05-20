import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd
from scipy import stats
import scipy
import statsmodels.stats.multitest as mul
import matplotlib.gridspec as gridspec
import csv
import math



def plot_stat_comparison(group, comp1, comp2, comp1_stderr, comp2_stderr, p_val, p_fdr, p_mul, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    assert(len(comp1) == len(comp2) == len(time))
    fig, ax = plt.subplots()
    ax.set_xlim(time[0], time[-1])
    ax.set_ylim(p_mul-2, p_mul+2)
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
    
    ax.tick_params(labelsize = 19)
    ax.legend(loc='upper left', fontsize = 22)
    ax.set_title(title, fontsize = 40)
        
    path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/long_timecourse/feedback/{group}/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s' %(comp1_label, comp2_label) + '.png', transparent=False)
    

comp1 = 'norisk'
comp2 = 'norisk'
#parameter3 = '_positive'
#parameter4 = '_positive'
parameter3 = '_negative'
parameter4 = '_positive'

untrained = False

with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_trained_no_aver.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')

df = df[df['train'] == 'trained']

group = 'autists'

if group == 'normals':
    #start = 0
    #end = N
    comp1_label = comp1 + parameter3 + '_NT'
    comp2_label = comp1 + parameter4 + '_NT'
    subj_list = ['P001', 'P004', 'P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039', 'P040', 'P044', 'P047', 'P048', 'P053', 'P055', 'P058', 'P059', 
        'P060', 'P061', 'P063', 'P064', 'P065']

if group == 'autists':
    #start = N+1
    #end = 2*N
    comp1_label = comp1 + parameter3 + '_AT'
    comp2_label = comp1 + parameter4 + '_AT'
    subj_list = ['P301', 'P304', 'P307', 'P312', 'P313', 'P314', 'P316', 'P320', 'P321', 'P322', 'P323', 'P324', 'P325', 'P326', 'P327', 'P328', 'P329',
            'P333', 'P334', 'P335', 'P338', 'P341']


#if comp1 == 'prerisk':
#    subj_list = ['P001', 'P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 'P058', 'P059', 'P061', 'P063', 'P064', 'P065', 'P301', 'P304', 'P307', 'P312', 'P313', 'P314', 'P320', 'P321', 'P322', 'P324', 'P325', 'P326', 'P327', 'P328', 'P329', 'P334', 'P335', 'P338', 'P341']
#if comp1 == 'risk':
#    subj_list = ['P001', 'P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 'P059', 'P063', 'P064', 'P065',
#       'P301', 'P304', 'P307', 'P312', 'P313', 'P314', 'P320', 'P321', 'P322', 'P324', 'P325', 'P326', 'P327', 'P329', 'P335', 'P338', 'P341']
#if comp1 == 'postrisk':
#    subj_list = ['P004', 'P019', 'P021', 'P022', 'P032', 'P035', 'P039', 'P040', 'P047', 'P048', 'P053', 'P055', 
#        'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P304', 'P307', 'P312', 'P313', 'P314','P320', 'P321', 'P322', 
#        'P324', 'P325', 'P326', 'P327', 'P329', 'P333', 'P334', 'P335', 'P338', 'P341']


#65 = 0, 105 = 2000, 46 = m1000
times_len = df.iloc[:, 46:105].shape

choice_types = [comp1]

runs = [1,2,3,4,5]

#runs = [2]

#for fd 2 dimensions

for idx, choice in enumerate(choice_types):
    subj_choice_positive_list_mean = np.empty((0, 1, times_len[1]))
    subj_choice_negative_list_mean = np.empty((0, 1, times_len[1]))
    for subj in subj_list:
        choice_positive_list = np.empty((0, 1, times_len[1]))
        choice_negative_list = np.empty((0, 1, times_len[1]))
        for run in runs:
            df_name = df[df['fname'] == subj]
            df_choice = df_name[df_name['trial_type4'] == choice]
            df_block = df_choice[df_choice['block'] == run]
            df_fb_positive = df_block[df_block['prev_rew'] == True]
            #over trials
            #df_to_vstack = df_fb.iloc[:, 26:125].mean(axis = 0)
            df_fb_positive_to_vstack = df_fb_positive.iloc[:, 46:105].mean(axis = 0)
            df_fb_positive_to_vstack = df_fb_positive_to_vstack[np.newaxis, :]
            if pd.isna(df_fb_positive_to_vstack).any():
                print('isna mean interval')
                print(df_fb_positive_to_vstack)
                continue
            else:
                choice_positive_list = np.vstack([choice_positive_list, df_fb_positive_to_vstack[:,np.newaxis]])

        if len(choice_positive_list) > 0:
            choice_positive_list_mean = choice_positive_list.mean(axis = 0)
        else:
            continue
        #subj_choice_positive_list_mean =  np.vstack([subj_choice_positive_list_mean, choice_positive_list_mean[:,np.newaxis]]) 

        for run in runs:
            df_name = df[df['fname'] == subj]
            df_choice = df_name[df_name['trial_type4'] == choice]
            df_block = df_choice[df_choice['block'] == run]

            df_fb_negative = df_block[df_block['prev_rew'] == False]
            #over trials
            #df_to_vstack = df_fb.iloc[:, 26:125].mean(axis = 0)
            df_fb_negative_to_vstack = df_fb_negative.iloc[:, 46:105].mean(axis = 0)
            df_fb_negative_to_vstack = df_fb_negative_to_vstack[np.newaxis, :]
            if pd.isna(df_fb_negative_to_vstack).any():
                print('isna mean interval')
                print(df_fb_negative_to_vstack)
                continue
            else:
                choice_negative_list = np.vstack([choice_negative_list, df_fb_negative_to_vstack[:,np.newaxis]])

        if len(choice_negative_list) > 0:
            choice_negative_list_mean = choice_negative_list.mean(axis = 0)
        else:
            continue
        if len(choice_positive_list) > 0 and len(choice_negative_list) > 0:
            print('in both', subj)
            subj_choice_negative_list_mean =  np.vstack([subj_choice_negative_list_mean, choice_negative_list_mean[:,np.newaxis]])
            subj_choice_positive_list_mean =  np.vstack([subj_choice_positive_list_mean, choice_positive_list_mean[:,np.newaxis]])
            le = len(subj_choice_negative_list_mean)
            print('le', le)
            #print('subj_choice_positive_list_mean', subj_choice_positive_list_mean.shape) 
            #print('subj_choice_negative_list_mean', subj_choice_negative_list_mean.shape) 
            contr = np.zeros((int(le), 2, 1, times_len[1]))
    contr[:, 1, :]  = subj_choice_positive_list_mean
    contr[:, 0, :]  = subj_choice_negative_list_mean
    #print(subj_choice_positive_list_mean)
#prepare stst and plotting

#N = int(len(subj_list)/2)
#print('N', N)
#print('subj list N', subj_list[N])

feedback_negative = contr[:, 0, :]
feedback_positive = contr[:, 1, :]

feedback_negative_mean = feedback_negative.mean(axis=0)
feedback_positive_mean = feedback_positive.mean(axis=0)

feedback_negative_mean = np.squeeze(feedback_negative_mean, axis=0)
feedback_positive_mean = np.squeeze(feedback_positive_mean, axis=0)

feedback_negative_stderr = scipy.stats.sem(contr[:, 0, :], axis=0)
feedback_positive_stderr = scipy.stats.sem(contr[:, 1, :], axis=0)

feedback_negative_stderr = np.squeeze(feedback_negative_stderr, axis=0)
feedback_positive_stderr = np.squeeze(feedback_positive_stderr, axis=0)


t_stat, p_val = stats.ttest_ind(feedback_negative, feedback_positive, axis=0)

p_val = np.squeeze(p_val, axis=0)
p_fdr = mul.fdrcorrection(p_val)[1]

#!!!! -1000:2000
time = np.arange(-19,40)
p_mul = 0

fig = plt.figure(figsize=(8, 5))
fig.suptitle('Pupil trained, ttest', fontsize=25, fontweight='bold')

plot_stat_comparison(group, comp1=feedback_negative_mean, comp2=feedback_positive_mean, comp1_stderr=feedback_negative_stderr, comp2_stderr=feedback_positive_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label}, {le} subjects',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='salmon', comp2_color='b')
