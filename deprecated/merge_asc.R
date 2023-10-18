
library(data.table)
#rm(list = ls())

at <- c('P301', 'P304','P307', 'P312','P313','P314','P316','P318','P321','P322','P323','P324','P325','P326','P327',
        'P328','P329','P333', 'P334','P335','P338','P341', 'P342')
#nt <- c('P001','P004','P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
#        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066')

nt <- c('P067','P004','P019', 'P021', 'P022', 'P032', 'P034', 'P035', 'P039','P040', 'P044','P047','P048', 'P053',
        'P055', 'P058', 'P059', 'P060', 'P061', 'P063', 'P064', 'P065', 'P066')

temp <- read.csv('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/asci/P301/resp_P301_table.csv') #donor of colnames for empty datatable 
#temp$V1 <- NULL
length(colnames(temp))
pupil <- setNames(data.table(matrix(nrow = 0, ncol = length(colnames(temp)))), c(colnames(temp)))

group <- 'autists'

if (group == 'normals'){
  subj_list <- nt
}

if (group == 'autists'){
  subj_list <- at
}

for(subject in subj_list){
  fname <- paste0('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/asci/', subject, '/','resp_', subject, '_table.csv')
  print(paste("Reading ",fname, sep=""))
  fname_csv <- as.data.table(read.csv(fname))
  print(nrow(fname_csv))
  pupil <- rbind(pupil, fname_csv, fill = TRUE)
}

pupil <- na.omit(pupil)

pupil$group <- as.character(group)

pupil$fname <- gsub ("_1", "", pupil$fname)
pupil$trial_type <-gsub ("_", "", pupil$trial_type)

#2
#RT filter (for all intervals)
pupil <- pupil[RT>300 & RT<4000]
pupil[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#3
#Z for pupil

ncol_min <- grep('V_m999',colnames(pupil))
ncol_max <- grep('V_3000',colnames(pupil))
col <- c(colnames(pupil)[ncol_min:ncol_max])

df_filt_m <- data.table(melt(pupil[blink==F], id.vars = c('fname','time'), measure.vars = col))
Zmeans <- df_filt_m[, mean(value), by=fname]
setnames(Zmeans,'V1','Zmean_2')
Zsds <- df_filt_m[, sd(value), by=fname]
setnames(Zsds,'V1','Zsd_2')

pupil <- merge(pupil,Zmeans,by='fname',all.x = TRUE)
pupil <- merge(pupil,Zsds,by='fname',all.x = TRUE)

#apply Z for pupil
cols <- colnames(pupil)[grep('resp_|V_|BL_|inter_trial',colnames(pupil))]
for (j in cols) set(pupil, j = j, value = (pupil[[j]]-pupil$Zmean_2)/pupil$Zsd_2)

pupil$trial_type4 <- pupil$trial_type
pupil[prev_risk==0 & risk==1 & next_risk==0, trial_type4:= 'risk']
pupil[prev_risk==0 & risk==0 & next_risk==0, trial_type4:= 'norisk']

pupil <- pupil[blink == FALSE]
unique(pupil$blink)
pupil[trained == FALSE, train:='not_trained'][trained== TRUE, train:='trained']
unique(pupil$trained)

fname <- paste0('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/asci/', group, '_table.csv')
write.csv(pupil,file=fname,row.names=F)
c <- read.csv(fname)
