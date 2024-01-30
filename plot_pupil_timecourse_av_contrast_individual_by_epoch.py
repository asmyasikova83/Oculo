import numpy as np
import matplotlib.pyplot as plt
import os
import pandas as pd
from scipy import stats
import scipy
import statsmodels.stats.multitest as mul
import matplotlib.gridspec as gridspec
import csv
from array import array


def plot_stat_comparison(Tuk, show_contrast, fname, comp1, comp2, p_mul, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    #assert(len(comp1) == len(comp2) == len(time))
    size_norisk = comp1.shape
    size_hp_epoch = size_norisk[0]
    size_hp_time = size_norisk[1]
    size_risk = comp2.shape
    size_lp_epoch = size_risk[0]
    size_lp_time = size_risk[1]
    fig, ax = plt.subplots()
    title = '%s_vs_%s_%s by epoch' %(comp1_label, comp2_label, fname)
    ax.set_xlim(time[0], time[-1])
    ax.set_ylim(p_mul-4.80, p_mul+3.0)
    ax.set_xlabel(title, fontsize=40)
    ax.set_ylabel('Pupil size [Z]', fontsize=40)
    #axis for feedback
    ax.plot([20, 20.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([40, 40.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    for sn in range(size_hp_epoch):
        for sr in range(size_lp_epoch):
            ax.plot(time, comp1.iloc[sn,:], color=comp1_color, linewidth=7, label='')
            ax.plot(time, comp2.iloc[sr,:], color=comp2_color, linewidth=7, label='')
    
    ax.tick_params(labelsize = 40)
    ax.legend(loc='upper left', fontsize = 30)
    if Tuk:
        path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/individual/z/by_epoch/'
    else:
        path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/individual/z/by_epoch/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s_%s' %(comp1_label, comp2_label, fname) + 'epoch.png', transparent=False)
    

###########################################################################
    
#with open('/home/asmyasnikova83/Oculo/tables/pupil_table_autists_norisk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv', 'w') as out:
with open('/home/asmyasnikova83/Oculo/tables/pupil_table_normals_norisk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df_norisk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv')

#from -500 ms to 2200 ms
times_len = df_norisk_normals.iloc[:, 56:109].shape

#with open('/home/asmyasnikova83/Oculo/tables/pupil_table_autists_risk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv', 'w') as out:
with open('/home/asmyasnikova83/Oculo/tables/pupil_table_normals_risk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df_risk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv')

comp1 = 'norisk'
comp2 = 'risk'
train = 'trained'

fname = 'P063'
df_norisk_normals = df_norisk_normals[df_norisk_normals['fname'] == fname]
df_norisk_normals = df_norisk_normals[df_norisk_normals['train'] == train]
df_risk_normals = df_risk_normals[df_risk_normals['fname'] == fname]
df_risk_normals = df_risk_normals[df_risk_normals['train'] == train]

parameter3 = ''
parameter4 = ''

runs = [1,2,3,4,5]


norisk_NT = df_norisk_normals.iloc[:, 56:110]

risk_NT = df_risk_normals.iloc[:, 56:110]

time = np.arange(-9, 45)
p_mul = 0

comp1_label = 'HP'
comp2_label = 'LP'

fig = plt.figure(figsize=(8, 5))


Tuk = False
show_contrast = False

plot_stat_comparison(Tuk, show_contrast=show_contrast, fname=fname, comp1=norisk_NT, comp2=risk_NT, p_mul=p_mul, time=time, title=f'{comp1_label} vs {comp2_label} {train}',  folder='comparison', comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='turquoise', comp2_color='salmon')
