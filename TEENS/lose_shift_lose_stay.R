
library(data.table)
library(stringi)

#rm(list = ls())
path_teens <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
path_adults <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/"

#eyetracker
rt_teens <- read.csv(paste0(path_teens,"resp_merged_teens_table_general_stat_w_blinks.csv"))
rt_teens <- as.data.table(rt_teens)
rt_adults <- fread(paste0(path_adults,"norma.txt"))
rt_adults <- as.data.table(rt_adults[,!c(21:25)])
rt_adults <- rt_adults[block!=6]
colnames(rt_teens)
colnames(rt_adults)

rt <- rbind(rt_teens, rt_adults)

#rename
rt$fname <- gsub('_1','',rt$fname)
rt$fname <- gsub('^p','P',rt$fname)

#rt <- rt[grep('P[0-9]',rt$fname)]
#rt <- rt[!(rt$RT< 300 | rt$RT> 4000),]
#rt <- rt[block != 6]
rt[prev_rew == 1, feedback_prev:='positive'][prev_rew == 0, feedback_prev:='negative']

#lose-shift, lose-stay
rt$lose_shift<- FALSE
rt[feedback_prev=='negative' & risk != prev_risk]$lose_shift <- TRUE

rt$lose_stay<- FALSE
rt[feedback_prev=='negative' & prev_risk == risk]$lose_stay <- TRUE

######################################TEENS###############################################

#problem info 'P719 '
rt_teens<- filter(rt, fname %in% c("P701", "P702", "P705", "P706", "P707", "P708", "P709", "P710", "P711", "P712",
                             "P713", "P714","P715", "P716",  "P718",  "P723", "P726", "P727", "P728", "P729",
                             "P730"))
#"P717", "P731","P703",
q_shift<- rt_teens %>%
  group_by(fname, lose_shift)%>%summarise(N = n())

q_shift <- as.data.table(unique(q_shift))
q_shift_yes <- q_shift[lose_shift==TRUE]
q_shift_no <- q_shift[lose_shift==FALSE]
q_shift_yes$not_lose_shift_N <-q_shift_no$N 
q_shift_yes$proportion_lose_shift<- as.numeric(q_shift_yes$N)/(as.numeric(q_shift_yes$N) + as.numeric(q_shift_no$N))

q_stay<- rt_teens %>%
  group_by(fname, lose_stay)%>%summarise(N = n())

q_stay <- as.data.table(unique(q_stay))

q_stay <- as.data.table(unique(q_stay))
q_stay_yes <- q_stay[lose_stay==TRUE]
q_stay_no <- q_stay[lose_stay==FALSE]
q_stay_yes$not_lose_stay_N <-q_stay_no$N 
q_stay_yes$proportion_lose_stay<- as.numeric(q_stay_yes$N)/(as.numeric(q_stay_yes$N) + as.numeric(q_stay_no$N))

out_path <- "C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/"
#write.csv(q_shift_yes,paste0(out_path, "teens_lose_shift.csv"))
#write.csv(q_stay_yes,paste0(out_path, "teens_lose_stay.csv"))

#############################ADULTS ####################################
#############################lose shift ################################

rt_adults<- filter(rt, fname %in% c("P001", "P002", "P003", "P004", "P005",
                                    "P006", "P007", "P008", "P009", "P010",
                                    "P011", "P012", "P043", "P044", "P045",
                                    "P046", "P047", "P048", "P049", "P050" ))
unique(rt_adults$fname)
q_adults_shift<- rt_adults %>%
  group_by(fname, lose_shift)%>%summarise(N = n())

q_adults_shift <- as.data.table(unique(q_adults_shift))
q_adults_shift_yes <- q_adults_shift[lose_shift==TRUE]
q_adults_shift_no <- q_adults_shift[lose_shift==FALSE]
q_adults_shift_yes$not_lose_shift_N <-q_adults_shift_no$N 
q_adults_shift_yes$proportion_lose_shift<- as.numeric(q_adults_shift_yes$N)/(as.numeric(q_adults_shift_yes$N) + as.numeric(q_adults_shift_no$N))

#compare proportions of lose shift betw adults and teens
MAT = rbind(q_adults_shift_yes$proportion_lose_shift, q_shift_yes$proportion_lose_shift)
chisq.test(MAT, cor=F, sim=T, B = 10000)
##########################################lose_stay####################
q_adults_stay<- rt_adults %>%
  group_by(fname, lose_stay)%>%summarise(N = n())

q_adults_stay <- as.data.table(unique(q_adults_stay))

q_adults_stay <- as.data.table(unique(q_adults_stay))
q_adults_stay_yes <- q_adults_stay[lose_stay==TRUE]
q_adults_stay_no <- q_adults_stay[lose_stay==FALSE]
q_adults_stay_yes$not_lose_stay_N <-q_adults_stay_no$N 
q_adults_stay_yes$proportion_lose_stay<- as.numeric(q_adults_stay_yes$N)/(as.numeric(q_adults_stay_yes$N) + as.numeric(q_adults_stay_no$N))

MAT = rbind(q_adults_stay_yes$proportion_lose_stay, q_stay_yes$proportion_lose_stay)
#compare proportions
chisq.test(MAT, cor=F, sim=T, B = 10000)
