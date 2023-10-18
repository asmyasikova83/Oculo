import numpy as np
import mne
import matplotlib.pyplot as plt
import os
import pandas as pd
from scipy import stats
import scipy
import statsmodels.stats.multitest as mul
import csv
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
    return (result)

def plot_stat_comparison(group, train, comp1, comp2, comp1_stderr, comp2_stderr, f, p_val, p_fdr, p_mul, res_tfce, time, title, folder,
                         comp1_label, comp2_label, comp1_color, comp2_color):
    print('comp1 color', comp1_color)
    print('comp1 label', comp1_label)
    print('time', time)
    print('comp1', comp1)
    #assert(len(comp1) == len(comp2) == len(time))
    fig, ax = plt.subplots()
    ax.set_xlim(time[0], time[-1])
    ax.set_ylim(p_mul-10.0, p_mul+10.0)
    #axis for feedback
    #ax.plot([20, 20.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot([1000, 1000.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #axis for response
    ax.plot([0, 0.001], [-1000, 1000], color='k', linewidth=3, linestyle='--', zorder=1)
    #ax.plot([-5000, 5000], [0, 0.001], color='k', linewidth=3, linestyle='--', zorder=1)
    ax.plot(time, comp1, color=comp1_color, linewidth=3, label=comp1_label)
    ax.fill_between(time, comp1-comp1_stderr, comp1+comp1_stderr, alpha=.4, facecolor = comp1_color)
    ax.plot(time, comp2, color=comp2_color, linewidth=3, label=comp2_label)
    ax.fill_between(time, comp2-comp2_stderr, comp2+comp2_stderr, alpha=.4, facecolor = comp2_color)
    
    #ax.fill_between(time, y1 = p_mul+2, y2 = p_mul-2, where = (p_fdr < 0.05), facecolor = 'm', alpha = 0.46, step = 'pre')
    #ax.fill_between(time, y1 = p_mul+2, y2 = p_mul-2, where = (p_val < 0.05), facecolor = 'g', alpha = 0.46, step = 'pre')
    #plot tfce stat
    ax.fill_between(time, y1 = p_mul, y2 = p_mul, where = ((p_val < 0.05)*(res_tfce==0)), facecolor = 'blue', alpha = 0.2, step = 'pre')
    ax.fill_between(time, y1 = p_mul, y2 = p_mul, where = (res_tfce == 1), facecolor = 'crimson', alpha = 0.2, step = 'pre')
    
    ax.tick_params(labelsize = 19)
    ax.legend(loc='upper left', fontsize = 22)
    ax.set_title(title, fontsize = 40)
        
    path = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/filter/'
    os.makedirs(path, exist_ok = True)
    path_group = f'/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/full_tmcrs/filter/{group}/'
    os.makedirs(path_group, exist_ok = True)
    fig.set_size_inches((20, 10), forward=False)
    plt.savefig(path+'%s_vs_%s_%s_%s_%f high Hz' %(comp1_label, comp2_label, train, group, f) + '.png', transparent=False)
    

###########################################################################
    

with open('/home/asmyasnikova83/Oculo/tables/normals_table.csv', 'r') as inp, open('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv', 'w') as out:
    writer = csv.writer(out)
    for row in csv.reader(inp):
        writer.writerow(row)
df = pd.read_csv('/net/server/data/Archive/prob_learn/asmyasnikova83/Pupil/edit.csv')

#print(df.iloc[:,20])
#print(df.iloc[:,4219])


comp1 = 'norisk'
comp2 = 'risk'
train = 'not_trained'
df = df[df['train'] == train]

group = 'normals'
if group == 'normals':
    subj_list = ['P067','P004', 'P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066']
if group == 'autists':
    subj_list = ['P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
         'P328','P329','P333', 'P334','P335','P338','P341', 'P342']

parameter3 = ''
parameter4 = ''

runs = [1,2,3,4,5]
#runs = [1]

times_len = 3200
contr = np.zeros((len(subj_list), 2, 1, times_len))


sampling_freq = 1000  # in Hertz

#cutoff frequency
f = 0.3

for j,subj in enumerate(subj_list):
    temp_cond1 = np.empty((0, 1, times_len))
    temp_cond2 = np.empty((0, 1, times_len))
    epochs_all_fb1=np.empty((0, 1,times_len)) #arrays for epochs of both feedbacks from one block cond1
    epochs_all_fb2=np.empty((0, 1,times_len)) #arrays for epochs of both feedbacks from one block cond2
    for run in runs:
        df_name = df[df['fname'] == subj]
        df_choice1= df_name[df_name['trial_type4'] == comp1]
        df_block1 = df_choice1[df_choice1['block'] == run]
        df_choice2= df_name[df_name['trial_type4'] == comp2]
        df_block2 = df_choice2[df_choice2['block'] == run]
        if len(df_block1) > 0:
            print('df_block1.iloc[:, 1019:4219]',df_block1.iloc[:, 1000] )
            sh = df_block1.iloc[:, 1019:4219].shape
            # Create some dummy metadata
            n_channels = sh[0]
            ch_names=["Pupil size"]
            ch_types=["misc"]
            info = mne.create_info(n_channels, sfreq=sampling_freq)
            raw1 = mne.io.RawArray(df_block1.iloc[:, 1019:4219], info)
            #over trials
            raw1_filt = raw1.filter(picks = 'misc', l_freq=None, h_freq=f)
            df_to_vstack = raw1_filt.get_data().mean(axis = 0)
            print('df_to_vstack shape', df_to_vstack.shape)
            df_to_vstack = df_to_vstack[np.newaxis, np.newaxis, :]
            epochs_all_fb1 = np.vstack([epochs_all_fb1, df_to_vstack])
             
        if len(df_block2) > 0:
            sh = df_block2.iloc[:, 1019:4219].shape
            n_channels = sh[0]
            info = mne.create_info(n_channels, sfreq=sampling_freq)
            raw2 = mne.io.RawArray(df_block2.iloc[:, 1019:4219], info)
            #over trials
            raw2_filt = raw2.filter(picks = 'misc', l_freq=None, h_freq=f)
            df_to_vstack_cond2 = raw2_filt.get_data().mean(axis = 0)
            df_to_vstack_cond2 = df_to_vstack_cond2[np.newaxis, np.newaxis, :]
            epochs_all_fb2 = np.vstack([epochs_all_fb2, df_to_vstack_cond2])

    #average over blocks
    if len(epochs_all_fb1) > 0:
        temp_cond1 = epochs_all_fb1.mean(axis = 0)
    if len(epochs_all_fb2) > 0:
        temp_cond2 = epochs_all_fb2.mean(axis = 0)
  
    #subjects
    if len(temp_cond1) > 0:
        contr[j, 0, :]  = temp_cond1.mean(axis=0)
    if len(temp_cond2) > 0:
        contr[j, 1, :]  = temp_cond2.mean(axis=0)

#prepare stst and plotting


if group == 'normals':
    comp1_label = comp1 + parameter3 + '_NT'
    comp2_label = comp2 + parameter4 + '_NT'

if group == 'autists':
    comp1_label = comp1 + parameter3 + '_AT'
    comp2_label = comp2 + parameter4 + '_AT'

norisk_NT = np.empty((1, times_len))
risk_NT = np.empty((1, times_len))

print('risk nt shape', risk_NT.shape)

for j,subj in enumerate(subj_list):
    if contr[j, 0, :].all() != 0 and contr[j, 1, :].all() != 0:
        norisk_NT = np.vstack([norisk_NT, contr[j, 0, :]])
        risk_NT = np.vstack([risk_NT, contr[j, 1, :]])
        
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

res_tfce = np.array(tfce(norisk_NT,risk_NT))

print('res tfce', res_tfce)
#!!!! -1000:2000

time = np.arange(-1000,2200)
p_mul = 0

fig = plt.figure(figsize=(8, 5))

plot_stat_comparison(group=group, train=train, comp1=norisk_NT_mean, comp2=risk_NT_mean, comp1_stderr=norisk_NT_stderr, comp2_stderr=risk_NT_stderr, f = f, p_val=p_val, p_fdr=p_fdr,p_mul=p_mul, res_tfce=res_tfce, time=time, title=f'{comp1_label} vs {comp2_label} {train} {group} filtered {f} Hz',  folder='comparison',
                         comp1_label=comp1_label, comp2_label=comp2_label, comp1_color='b', comp2_color='salmon')
