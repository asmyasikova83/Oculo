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

LMEM_oculo_traits.R - correlations between traits of character and pupil Z

plot_pupil_timecourse_kseniya_averaging_btw_group.py - timecourses and t-statistics betw 
               normals and autists

plot_pupil_timecourse_kseniya_averaging_in_group.py - timecourses and t-statistics betw
               in group

RT 

plot_RT_raw_eyetracker.R - log RT stat HP vs LP 
plot_RT_raw_eyetracker_negative.R - log RT stat HP vs LP with LP preceding losses

proportion_risks_RT_slowing_corr.R - associations of LP share and diff btwn RT LP and RT HP

General

failed_count.R
proportion_risks_gains_corr.R
learning_trials.R
repetitive_risks_stat.R  
