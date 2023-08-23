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

def plot_stat_comparison(group, train, comp1, comp2, comp1_stderr, comp2_stderr, p_val, p_fdr, p_mul, res_tfce, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    print('comp1 color', comp1_color)
    print('comp1 label', comp1_label)
    #assert(len(comp1) == len(comp2) == len(time))
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
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'g', alpha = 0.2, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+500, y2 = p_mul-500, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 19)
    ax.legend(loc='upper left', fontsize = 22)
    ax.set_title(title, fontsize = 40)
        
    path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/revised_samples_tmcrs_test/{group}/'
    os.makedirs(path, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s_%s_%s' %(comp1_label, comp2_label, train, group) + '.png', transparent=False)
    

###########################################################################
    

with open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/tables/pupil_table_for_tfce.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')

times_len = df.iloc[:, 46:105].shape

comp1 = 'risk'
comp2 = 'risk'
train = 'trained_vs_not_trained'
df1 = df[df['train'] == 'trained']
#df1 = df1[df1['trial_type4'] == comp1]

df2 = df[df['train'] == 'not_trained']
#df2 = df2[df2['trial_type4'] == comp2]

group = 'normals'
if group == 'normals':
    subj_list = ['P001','P004', 'P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066']
if group == 'autists':
    subj_list = ['P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
         'P328','P329','P333', 'P334','P335','P338','P341', 'P342']

#parameter3 = '_positive'
#parameter4 = '_positive'
#parameter3 = '_negative'
#parameter4 = '_positive'
parameter3 = ''
parameter4 = ''

runs = [1,2,3,4,5]
#runs = [1]

contr = np.zeros((len(subj_list), 2, 1, times_len[1]))


print('comp1', comp1)
for j,subj in enumerate(subj_list):
    temp_cond1 = np.empty((0, 1, times_len[1]))
    temp_cond2 = np.empty((0, 1, times_len[1]))
    epochs_all_fb1=np.empty((0, 1,times_len[1])) #arrays for epochs of both feedbacks from one block cond1
    epochs_all_fb2=np.empty((0, 1,times_len[1])) #arrays for epochs of both feedbacks from one block cond2
    for run in runs:
        df1_name = df1[df1['fname'] == subj]
        df_choice1= df1_name[df1_name['trial_type4'] == comp1]
        df_block1 = df_choice1[df_choice1['block'] == run]
        df2_name = df2[df2['fname'] == subj]
        df_choice2= df2_name[df2_name['trial_type4'] == comp2]
        df_block2 = df_choice2[df_choice2['block'] == run]
        if len(df_block1) > 0:
            #over trials
            df_to_vstack = df_block1.iloc[:, 46:105].mean(axis = 0)
            df_to_vstack = df_to_vstack[np.newaxis, np.newaxis, :]
            epochs_all_fb1 = np.vstack([epochs_all_fb1, df_to_vstack])

        if len(df_block2) > 0:
            df_to_vstack_cond2 = df_block2.iloc[:, 46:105].mean(axis = 0)
            df_to_vstack_cond2 = df_to_vstack_cond2[np.newaxis, np.newaxis, :]
            epochs_all_fb2 = np.vstack([epochs_all_fb2, df_to_vstack_cond2])

    #average over blocks
    if len(epochs_all_fb1) > 0:
        temp_cond1 = epochs_all_fb1.mean(axis = 0)
    if len(epochs_all_fb2) > 0:
        temp_cond2 = epochs_all_fb2.mean(axis = 0)
   

    if len(temp_cond1) > 0:
        contr[j, 0, :]  = temp_cond1.mean(axis=0)
    if len(temp_cond2) > 0:
        contr[j, 1, :]  = temp_cond2.mean(axis=0)

#prepare stst and plotting


if group == 'normals':
    comp1_label = comp1 + 'train' + '_NT'
    comp2_label = comp2 + 'not_train' + '_NT'

if group == 'autists':
    comp1_label = comp1 + '_train' + '_AT'
    comp2_label = comp2 + '_not_train' + '_AT'

norisk_NT = np.empty((1, times_len[1]))
risk_NT = np.empty((1, times_len[1]))

print('risk nt shape', risk_NT.shape)

for j,subj in enumerate(subj_list):
    if contr[j, 0, :].all() != 0 and contr[j, 1, :].all() != 0:
        print('j', j)
        print('j no zero norisk', j)
        print('j no zero norisk', contr[j, 0, :])
        norisk_NT = np.vstack([norisk_NT, contr[j, 0, :]])
        print('norisk_NT', norisk_NT)
        risk_NT = np.vstack([risk_NT, contr[j, 1, :]])
        


print('norisk_NT shape', norisk_NT.shape)
print('norisk_NT', norisk_NT)
norisk_NT_mean = norisk_NT.mean(axis=0)
risk_NT_mean = risk_NT.mean(axis=0)


norisk_NT_stderr = scipy.stats.sem(contr[:, 0, :], axis=0)
risk_NT_stderr = scipy.stats.sem(contr[:, 1, :], axis=0)

norisk_NT_stderr = np.squeeze(norisk_NT_stderr, axis=0)
risk_NT_stderr = np.squeeze(risk_NT_stderr, axis=0)


t_stat, p_val = stats.ttest_rel(norisk_NT, risk_NT, axis=0)

p_fdr = mul.fdrcorrection(p_val)[1]

#prepare data for tfce
norisk_NT = np.transpose(norisk_NT)
risk_NT = np.transpose(risk_NT)

#print('risk NT after transpose', risk_NT)
#exit()
res_tfce = np.array(tfce(norisk_NT,risk_NT))

print('res tfce', res_tfce)
#!!!! -1000:2000
time = np.arange(-19,40)
p_mul = 0

fig = plt.figure(figsize=(8, 5))
fig.suptitle('Pupil train vs not_trained, ttest', fontsize=25, fontweight='bold')

plot_stat_comparison(group=group, train=train, comp1=norisk_NT_mean, comp2=risk_NT_mean, comp1_stderr=norisk_NT_stderr, comp2_stderr=risk_NT_stderr, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, res_tfce=res_tfce, time=time, title=f'{comp1_label} vs {comp2_label} {train} {group}',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='b', comp2_color='salmon')