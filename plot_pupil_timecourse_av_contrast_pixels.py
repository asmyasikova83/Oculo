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
from array import array


def plot_stat_comparison(Tuk, show_contrast, train, comp1, comp2, comp1_stderr, comp2_stderr, contrast, p_mul, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    #assert(len(comp1) == len(comp2) == len(time))
    title = '%s_%s' %(train, fname)
    fig, ax = plt.subplots()
    ax.set_xlim(time[0], time[-1])
    ax.set_ylim(p_mul+8000, p_mul+9000)
    ax.set_xlabel(title, fontsize=40)
    ax.set_ylabel('Pupil size pixels', fontsize=40)
    #ax.set_title(title, fontsize = 40)
    ## x-label as blank
    #axis for feedback
    ax.plot([20, 20.001], [8000, 9000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([40, 40.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [8000, 9000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([-5000, 5000], [0, 0.001], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot(time, comp1, color=comp1_color, linewidth=7, label=comp1_label)
    ax.fill_between(time, comp1-comp1_stderr, comp1+comp1_stderr, alpha=.8, facecolor = comp1_color)
    ax.plot(time, comp2, color=comp2_color, linewidth=7, label=comp2_label)
    ax.fill_between(time, comp2-comp2_stderr, comp2+comp2_stderr, alpha=.8, facecolor = comp2_color)
    if show_contrast:
        ax.plot(time, contrast, color='blue', linestyle = 'dotted',linewidth=7, label='LP-HP')
    #ax.fill_between(time, y1 = p_mul+1.15, y2 = p_mul-1.15, where = (p_fdr < 0.05), facecolor = 'blue', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+1.2, y2 = p_mul-1.2, where = (p_val < 0.05), facecolor = 'g', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 40)
    ax.legend(loc='upper left', fontsize = 30)
    path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/individual/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s_%s_%s' %(comp1_label, comp2_label, train, fname) + '.png', transparent=False)
    

###########################################################################
#norisk_autists_pixels.csv
df_norisk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals_pixels.csv')

#from -500 ms to 2200 ms
times_len = df_norisk_normals.iloc[:, 56:109].shape

#risk_autists_pixels.csv
df_risk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals_pixels.csv')

comp1 = 'norisk'
comp2 = 'risk'
train = 'trained'
fname = 'P066'
df_norisk_normals = df_norisk_normals[df_norisk_normals['train'] == train]
df_norisk_normals = df_norisk_normals[df_norisk_normals['fname'] == fname]
df_risk_normals = df_risk_normals[df_risk_normals['train'] == train]
df_risk_normals = df_risk_normals[df_risk_normals['fname'] == fname]

parameter3 = ''
parameter4 = ''

runs = [1,2,3,4,5]

norisk_NT = df_norisk_normals.iloc[:, 56:110]
#to get the overall figure representing mean in the interval
norisk_NT_mean = df_norisk_normals.iloc[:, 56:110].mean(axis = 0)

risk_NT = df_risk_normals.iloc[:, 56:110]
#to get the overall figure representing mean in the interval
risk_NT_mean = df_risk_normals.iloc[:, 56:110].mean(axis = 0)

#illustrate contrasts betw conditions
contrast = risk_NT_mean - norisk_NT_mean

risk_NT_stderr = scipy.stats.sem(risk_NT, axis=0)
norisk_NT_stderr = scipy.stats.sem(norisk_NT, axis=0)
norisk_NT_mean = norisk_NT_mean[np.newaxis, :]
risk_NT_mean = risk_NT_mean[np.newaxis, :]

time = np.arange(-9, 45)
p_mul = 0

comp1_label = 'HP'
comp2_label = 'LP'

fig = plt.figure(figsize=(8, 5))

norisk_NT_mean = norisk_NT_mean.mean(axis = 0)
risk_NT_mean = risk_NT_mean.mean(axis = 0)


Tuk = False

show_contrast = False

plot_stat_comparison(Tuk, show_contrast=show_contrast, train=train, comp1=norisk_NT_mean, comp2=risk_NT_mean, comp1_stderr=norisk_NT_stderr, comp2_stderr=risk_NT_stderr, contrast = contrast, p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label} {train} {fname}',  folder='comparison', comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='green', comp2_color='red')
