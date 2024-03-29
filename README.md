Scripts for analysis and plots of pupillometric data obtained from normals and autists

lists of participants:

autism_all.txt
normals_all.txt 

plot_RT_raw.R - script for preprocessing and plotting raw response time over 
                choice types and groups (normals, autists) options for splitting
                into feedback prev.

plot_RT_raw_by_choice.R - same as plot_RT_raw.R with LMM statistics

plot_RT_Z_ANOVA.R - script for preprocessing and plotting Z-transformed 
                response time with ANOVA and 2 choice types (risk, postrisk)
                analysis od normality of distributions is included.

plot_RT_raw_repetitive_risks.R - script for analysis of triple risks (a chain of risks)
plot_RT_Z_repetitive_risks.R

LMEM_oculo_graph_group_by_choice.R - script for preprocessing and plotting Z-transformed
                pupillometric data. Feedback cur in the model can be replaced with
                feedback prev.

LMEM_oculo_graph_group_cleaned.R -script for preprocessing and plotting Z-transformed
                pupillometric data. Plots for the article on NT, AT. Plots in Rus 
LMEM_oculo_graph_group_cleaned_rus.R -script for preprocessing and plotting Z-transformed
                pupillometric data. Plots for the article on NT, AT. Plots in Rus 

LMEM_oculo_graph_group_feedback_cleaned.R - script for preprocessing and plotting Z-transformed
                pupillometric data. Choice type split by Feedback Prev. Plots for the article on Nt, AT
LMEM_oculo_graph_group_feedback_cleaned_rus.R - script for preprocessing and plotting Z-transformed
                pupillometric data. Choice type split by Feedback Prev. Plots in Rus

LMEM_oculo_graph_group_cleaned_singleLP_repet_LP.R - script for preprocessing and plotting Z-transformed
                pupillometric data. Contrasts of single and repetitive LP. Plots in Rus

LMEM_oculo_traits_fin.R - correlations between traits of character and pupil Z

LMM_oculo_feedback_boxplot.R - boxplot diagrams representing Pupillary responses in Group split
by prev. feedback emmeans

LMEM_oculo_raw_1100_2200.R - LMM analysis on raw data with plotting emmeans 

LMEM_oculo_graph_group_1500_2200.R - interval from peaked value (1500 ms) with separate analysis in no learning/after learning cond
LMEM_oculo_graph_group_cur_feedback_1500_2200.R - interval from peaked value (1500 ms) with current fb in the LMM model

Pupil_diff_graph_correlation.R - correlate pupil size in HP: trained vs not_trained

plot_pupil_timecourse_kseniya_averaging_btw_group.py - timecourses and t-statistics betw 
               normals and autists

plot_pupil_timecourse_kseniya_averaging_in_group.py - timecourses and t-statistics betw
               in group
plot_pupil_timecourse_full_choice.py - plot full timecourses split by choice with tfce (or tstat)

plot_pupil_timecourse_full_feedback.py - plot full timecourses split by feedback with tfce (or tstat)

plot_pupil_timecourse_full_train.py -  plot full timecourses split by train with tfce (or tstat)
autists_table.csv is very big, cannot upload it. libtfce has been uploaded.

plot_pupil_timecourse_av.py - plot correctly averaged timecourses with permutations  

################################################
timecourse_permut_PERM_choice.R       -   timecourses with p-val permutations, fdr
timecourse_permut_PERM_Tukey_choice.R -   timecourses with posthoc Tukey, p-val permutations, fdr

RT 

plot_RT_raw_eyetracker.R - log RT stat HP vs LP 
plot_RT_raw_eyetracker_negative.R - log RT stat HP vs LP with LP preceding losses
plot_RT_raw_eyetracker_positive.R - log RT stat HP vs LP with LP preceding gains

plot_RT_feedback_fin.R - log RT in HP and LP split by prev. fb, groups combined
plot_RT_LMM_fin.R - log RT for betw group analyses

proportion_risks_RT_slowing_corr.R - associations of LP share and diff btwn RT LP and RT HP

LMM_RT_feedback_boxplot.R - boxplot diagrams representing RT in choice type split be prev. feedback,
emmeans

RT_diff_graph_correlation.R - correlate RT in HP: trained vs not_trained

General

general_stat_table1.R - stat for Table 1 in the article
proportion_risks_fin.R - proportions of risks for article
failed_count.R
proportion_risks_gains_corr.R
learning_trials.R
repetitive_risks_stat.R 
transitions - estimate Win_Stay, Lose-Shift, not completed
plot_age_sex.R - plot demographics, non-verbal IQ test 

Preprocessing

* use OS (C:) \Program Files\SR Research\edfconverter (https://www.sr-research.com/support/thread-7674.html)
EDF to ASCII Conversion / EDF2ASC (GUI interface)
to convert Eyetracker files into ASC

pupil_preprocessing_full_timecourse_corrected.R - blink identification, learning and trial type identification
merge_asc.R in deprecated - combines asc files into a table, redundant 
