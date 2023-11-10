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
import subprocess

def tfce (df1,df2): #модифицированный пример из гитхаба Платона
    # create random data arrays
    A = df1
    A = A.transpose()
    B = df2
    B = B.transpose()
    subject_count = A.shape[0]
    data_length = A.shape[1]
    # check that python outputs data to binary files with expected byte count
    assert array("B", [0]).itemsize == 1
    assert array("I", [0]).itemsize == 4
    assert array("d", [0]).itemsize == 8


# write input data to file
    data_file = open("data.bin", "wb")
    array("I", [subject_count]).tofile(data_file)
    for s in range(subject_count):
        array("I", [data_length]).tofile(data_file)
        array("d", A[s]).tofile(data_file)
        array("I", [data_length]).tofile(data_file)
        array("d", B[s]).tofile(data_file)
    data_file.close()

    # call libtfce binary
    subprocess.call([
        "/home/asmyasnikova83/Oculo/libtfce",
    #    "--explore",
        "-e", "0.66",
        #"-e", "1",
        "-h", "2",
        "--input-file", "data.bin",
        "--output-file", "result.bin",
        "--permutation-count", "1000",
        "--type", "1d"])

    # read result back
    result_file = open("result.bin", "rb")
    result_size = array("I", [])
    result_size.fromfile(result_file, 1)
    result = array("B", [])
    result.fromfile(result_file, result_size[0])
    result = result.tolist()
    result_file.close()
#    print(result)
    return (result)

def plot_stat_comparison(Tuk, group, train, comp1, comp2, comp1_stderr, comp2_stderr, p_val, p_fdr, p_mul, res_tfce, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    print('comp1 color', comp1_color)
    print('comp1 label', comp1_label)

    #assert(len(comp1) == len(comp2) == len(time))
    fig, ax = plt.subplots()
    ax.set_xlim(time[0], time[-1])
    ax.set_ylim(p_mul-0.85, p_mul+0.85)
    ax.set_xlabel('Time ms', fontsize=40)
    ax.set_ylabel('Pupil size [Z]', fontsize=40)
    title = '%s_vs_%s_%s_%s' %(comp1_label, comp2_label, train, group)
    ax.set_title(title, fontsize = 40)
    ## x-label as blank
    #axis for feedback
    ax.plot([20, 20.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([40, 40.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([-5000, 5000], [0, 0.001], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot(time, comp1, color=comp1_color, linewidth=7, label=comp1_label)
    ax.fill_between(time, comp1-comp1_stderr, comp1+comp1_stderr, alpha=.8, facecolor = comp1_color)
    ax.plot(time, comp2, color=comp2_color, linewidth=7, label=comp2_label)
    ax.fill_between(time, comp2-comp2_stderr, comp2+comp2_stderr, alpha=.8, facecolor = comp2_color)
    
    ax.fill_between(time, y1 = p_mul+0.85, y2 = p_mul-0.85, where = (p_fdr < 0.05), facecolor = 'blue', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+1.2, y2 = p_mul-1.2, where = (p_val < 0.05), facecolor = 'g', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 40)
    ax.legend(loc='upper left', fontsize = 40)
    if Tuk:
        path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/averaged_chain_Tuk_fdr_article/'
    else:
        path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/averaged_chain_fdr/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s_%s_%s' %(comp1_label, comp2_label, train, group) + '.png', transparent=False)
    

###########################################################################
    

with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_autists_norisk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv', 'w') as out:
#with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_normals_norisk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df_norisk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/norisk_normals.csv')

#fom -500 ms to 2200 ms
times_len = df_norisk_normals.iloc[:, 46:109].shape

with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_autists_risk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv', 'w') as out:
#with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_normals_risk_only.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df_risk_normals = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/risk_normals.csv')
#from zero -buttonn press

comp1 = 'norisk'
comp2 = 'risk'
train = 'not_trained'
df_norisk_normals = df_norisk_normals[df_norisk_normals['train'] == train]
df_risk_normals = df_risk_normals[df_risk_normals['train'] == train]

parameter3 = ''
parameter4 = ''

runs = [1,2,3,4,5]


norisk_NT = df_norisk_normals.iloc[:, 46:110]
print('shape norisk_NT', norisk_NT)
norisk_NT_mean = df_norisk_normals.iloc[:, 46:110].mean(axis = 0)

risk_NT = df_risk_normals.iloc[:, 46:110]

risk_NT_mean = df_risk_normals.iloc[:, 46:110].mean(axis = 0)


risk_NT_stderr = scipy.stats.sem(risk_NT, axis=0)
norisk_NT_stderr = scipy.stats.sem(norisk_NT, axis=0)
norisk_NT_mean = norisk_NT_mean[np.newaxis, :]
risk_NT_mean = risk_NT_mean[np.newaxis, :]

res_tfce =  np.zeros((1, 64)) 

time = np.arange(-19, 45)
p_mul = 0

comp1_label = 'HP'
comp2_label = 'LP'

group = 'AT'

fig = plt.figure(figsize=(8, 5))

norisk_NT_mean = norisk_NT_mean.mean(axis = 0)
risk_NT_mean = risk_NT_mean.mean(axis = 0)

print('risk_NT_mean final', risk_NT_mean)
#p_val is just dummy here
p_val = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_nt_not_trained_choice.csv')

#choose post hoc Tukey or jast factor Choice type (trial type)

Tuk = False
if group == 'NT' and train == 'not_trained':
    if Tuk:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_Tuk_nt_not_trained_choice.csv')
    else:    
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_nt_not_trained_choice.csv')
if group == 'NT' and train == 'trained':
    if Tuk:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_Tuk_nt_trained_choice.csv')
    else:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_nt_trained_choice.csv')
if group == 'AT' and train == 'not_trained':
    if Tuk:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_Tuk_at_not_trained_choice.csv')
    else:   
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_at_not_trained_choice.csv')
if group == 'AT' and train == 'trained':
    if Tuk:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_Tuk_at_trained_choice.csv')
    else:
        p = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/p_vals_factor_significance_pupil_permut_at_trained_choice.csv')



if Tuk:
     p_fdr = p.iloc[:, 4]
     path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/averaged_chain_Tuk_fdr/'
else:
     p_fdr = p.iloc[:, 3]
     path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/averaged_chain_fdr/'

plot_stat_comparison(Tuk, group=group, train=train, comp1=norisk_NT_mean, comp2=risk_NT_mean, comp1_stderr=norisk_NT_stderr, comp2_stderr=risk_NT_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, res_tfce=res_tfce, time=time, title=f'{comp1_label} vs {comp2_label} {train} {group} av 50ms',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='yellow', comp2_color='salmon')
