
#install.packages('intervals')
library('intervals')
library(data.table)
source('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/readEL.R')
#rm(list = ls())  

t1 <- -2000 #starting epoch latency
t2 <- 3000  #finish epoch latency
t3 <- -500
blink_threshold <- 400

blink_find <- function(dataset,diff_threshold=10,time_threshold=20){
  L <- length(dataset$ps) #number of rows: to simplify the code
  
  DT <- as.data.table(dataset[,1:5]) #removing extra columns
  DT$diff <- 0 #initializing "deriative" colums by zeros
  DT[1:L-1,]$diff <- abs(DT[-1,]$ps-DT[-L,]$ps) #calculating "deriative" values
  
  #DT2: taking only "bad sectors of data"
  
  L1 <- is.na(DT$ps) #vector of boolean: true=missing values in pupil size
  L2 <- DT$diff>diff_threshold #vector of boolean: true=deriative above threshold
  L1[L2] <- T #summarizing the two vectors
  DT2 <- DT[L1] #DT2 contains only "bad" data sectors
  
  #DT3: marking "start" and "end" points of "bad" segments
  
  DT2$time_diff <- 0 #this code block is for 
  L_DT2 <- length(DT2$time_diff)
  DT2[-L_DT2,]$time_diff <- DT2[-1,]$time-DT2[-L_DT2,]$time
  DT2$type <- 0
  DT2[c(TRUE,DT2[-L_DT2]$time_diff>time_threshold)]$type <- 1
  DT2[DT2$time_diff>time_threshold]$type <- 2
  DT2[L_DT2]$type <- 2
  DT3 <- DT2[DT2$type>0]
  
  #DT4: a table with 1 row for 1 "bad" segment
  
  DT3$single_point <- c((DT3[1,]$type==2),as.logical((DT3[-1]$type==2)*(DT3[-length(DT3$time)]$type==2)))
  DT3[DT3$type==1]$single_point <- NA
  DT3$start_time <- 0
  DT3[DT3$single_point]$start_time <- DT3[DT3$single_point==TRUE]$time-1
  logical_vector <- c((!DT3$single_point)[-1],FALSE)
  DT3[!DT3$single_point]$start_time <- DT3[logical_vector]$time-1
  DT4 <- DT3[DT3$type==2]
  
  DT4$NAs <- 0
  for(b_idx in 1:length(DT4$time)){
    if(!DT4[b_idx,]$single_point){
      n1 <- match(DT4[b_idx,]$start_time+1,DT2$time)
      n2 <- match(DT4[b_idx,]$time,DT2$time)
      DT_temp <- DT2[n1:n2,]
      DT4[b_idx,]$NAs <- sum(is.na(DT_temp$ps))
    }
    if(DT4[b_idx,]$single_point){
      DT4[b_idx,]$NAs <- is.na(DT4[b_idx,]$ps)
    }
  } 
  DT4$duration <- DT4$time-DT4$start_time
  DT4$bad <- as.logical((DT4$duration>20)+(DT4$NAs>0))
  
  if(DT4[1,]$time==DT[1,]$time){ #removing last string of output in case of NAs in end of file
    DT4 <- DT4[-1,]
  }
  
  if(DT4[length(DT4$bad),]$time==DT[L,]$time){ #removing last string of output in case of NAs in end of file
    DT4 <- DT4[-length(DT4$bad),]
  }
  DT5 <- DT4[DT4$bad]
  data.table(t1=DT5$start_time,t2=DT5$time,duration=DT5$duration)
  
}

print("3 steps pupil processor. Step 1: data import")
full_table <- NULL
blink_table <- NULL
folder_out <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/'

f <- 'C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/data/links/'
teens <- list.files(f)
teens <- c("P701", "P702", "P703", "P705", "P706", "P707", "P708", "P709", "P710", "P711", "P712",
           "P713", "P714","P715", "P716", "P717", "P718",  "P723", "P726", "P727", "P728", "P729",
           "P730", "P731")

#problem info 'P719 '
#problem blinks P726 block 5
for(subject in teens){
    
    print(subject)
    #folder <- paste0('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/asci/', subject, '/')
    folder <- paste0('C:/Users/trosh/OneDrive/jobs_Miasnikova/Oculo/TEENS/data/links/', subject, '/')
    print(folder)
    path <- setwd(folder)
    list_of_files <- list.files(path=folder)
    length(list_of_files)

    subject_idx <- 1
    print(paste("Reading ",list_of_files[subject_idx],sep=""))
    
    fn <- paste(folder,list_of_files[subject_idx],sep="")
    response_logs <- NULL
    fn
    if(file.exists(fn)){
      D <- read.asc(fn)
      D$raw[,is.na(colnames(D$raw))] <- NULL
    }else{
      print('no fnames in folder')
      next
    }

    #this section is for merging multiple asc files into one
    if(grepl('1.asc',list_of_files[subject_idx])){
      print('merging server file')
      time_check_vector <- rep(0,5)
      time_check_vector[1] <- D$raw[1,]$time
      for(next_file_idx in 2:5){
        S <- paste(as.character(next_file_idx),'.asc',sep='')
        next_file <- str_replace(list_of_files[subject_idx],'1.asc',S)
        fn_temp <- paste(folder,next_file,sep="")

        if(file.exists(fn_temp)){
          print(paste('part',as.character(next_file_idx)))
          D_temp <- read.asc(fn_temp)
          D_temp$raw[,is.na(colnames(D_temp$raw))] <- NULL
          colnames(D$raw)
          colnames(D_temp$raw)
          length(colnames(D_temp$raw))
        } else {
          print(fn_temp)
          print('Does not exist')
          next
        }
        
        N_delay <- 400
        
        L <- length(D$raw$time)
        
        N <- D_temp$raw[1,]$time-D$raw[L,]$time-1
        
        time_finish <- D$raw[L,]$time
      
        raw_filler <- data.frame(time=time_finish+(1:N_delay),
                               xp=rep(NA,N_delay),
                               yp=rep(NA,N_delay),
                               ps=rep(NA,N_delay),
                               cr.info=rep('...',N_delay),
                               block=rep(-1,N_delay))
        
        raw_filler 
        D_temp$msg$time <- D_temp$msg$time-(D_temp$raw[1,]$time-time_finish)+N_delay+1
        D_temp$raw$time <- D_temp$raw$time-(D_temp$raw[1,]$time-time_finish)+N_delay+1
      
        D_temp$sacc$stime <- D_temp$sacc$stime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
        D_temp$sacc$etime <- D_temp$sacc$etime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
      
        D_temp$fix$stime <- D_temp$fix$stime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
        D_temp$fix$etime <- D_temp$fix$etime-(D_temp$raw[1,]$time-time_finish)+N_delay+1
      
       
        D$raw <- rbind(D$raw,raw_filler)
        D$raw <- rbind(D$raw,D_temp$raw)
        D$msg <- rbind(D$msg,D_temp$msg)
        D$sacc <- rbind(D$sacc,D_temp$sacc)
        D$fix <- rbind(D$fix,D_temp$fix)
      
      }
      print(paste("t-cons:",as.character(time_check_vector)))
    }
    subject_name <- sub('.asc','',list_of_files[subject_idx])
  
    D$raw <- as.data.table(D$raw[,1:5])
    D$msg <- as.data.table(D$msg)
    tau_const <- D$raw[1,]$time
    fix_cross <- as.data.table(D$msg[grep(pattern='FIX_CROSS',D$msg$text),])
    stim      <- as.data.table(D$msg[grep(pattern='STIM',D$msg$text),])
  
   
    D$msg[grep('ERROR MESSAGES LOST',D$msg$text)]$text <- 'RESPONSE_1_1'
    D$msg$text <- sub('RESPONSE_[NP]','RESPONSE',D$msg$text)
  
    #D$msg <- sub('COR_VAL','REW_VAL')
  
  
    responses <- D$msg[grep('RESPONSE',D$msg$text)]
    responses$fname <- subject_name
  
    responses$info <- sub('FIX_CROSS_|FIXATION_CROSS_','',D$msg[grep('FIX',D$msg$text)]$text)
    responses$block <- as.numeric(substring(responses$info,1,1))
    responses$RT <- responses$time-stim$time
    responses$fix_cross_time <- fix_cross[1:length(responses$time)]$time
  
    if(!grepl('_1',subject_name)){
      responses <- responses[block<max(block)]}
    #responses$info <- sub('RESPONSE+[[:space:]_]+(LOSE|REW)+[[:space:]_]+(VAL|INV)','',responses$text)
  
    responses$text <- sub('COR[[:space:]_]VAL','REW_VAL',responses$text)
    responses$text <- sub('INC[[:space:]_]VAL','LOSE_VAL',responses$text)
    responses$text <- sub('COR[[:space:]_]INV','LOSE_INV',responses$text)
    responses$text <- sub('INC[[:space:]_]INV','REW_INV',responses$text)
  
  
    responses$risk <- grepl(pattern='REW INV|LOSE VAL|REW_INV|LOSE_VAL',responses$text)
    responses$rew <- grepl(pattern='REW',responses$text)
  
    L <- length(responses$rew)
    responses$prev_rew <- F
    responses[-1]$prev_rew <- responses[-L]$rew
    #responses[1]$prev_rew <- NA
  
    responses$next_risk <- F
    responses[-L]$next_risk <- responses[-1]$risk
    #responses[L]$next_risk <- NA
  
    responses$prev_risk <- F
    responses[-1]$prev_risk <- responses[-L]$risk
    #responses[1]$prev_risk <- NA
  
  
    responses$learning <- F
    responses$critical <- F
    responses$trained <- F
    responses$last_trial <- F
  
    for(block_idx in unique(responses$block)){
    block_link <- responses$block==block_idx
    responses_temp <- responses[block_link]
    trained_bool <- F
    last_trial <- length(responses_temp$time)
    
    trial_idx <- 1
    while(!trained_bool & trial_idx<last_trial-5){
      V <- (trial_idx+4):last_trial
      cor_percentage <- 1-mean(responses_temp[V]$risk)
      if(prod(!responses_temp[trial_idx+(0:3)]$risk) & cor_percentage>0.65){
        trained_bool <- T
        responses_temp[1:(trial_idx-1)]$learning <- T
        responses_temp[trial_idx+(0:3)]$critical <- T
        responses_temp[V]$trained <- T
        responses_temp[length(responses_temp$time)]$last_trial <- T
        
        responses[block_link] <- responses_temp
      }
      trial_idx <- trial_idx+1
    }
    
    }
  
    responses$trial_type <- 'unused'
    responses[trained==T & risk==T & last_trial==F]$trial_type <- 'risk'
    responses[trained==T & risk==F & prev_risk==F & next_risk==F & last_trial==F]$trial_type <- 'no_risk'
    responses[trained==T & risk==F & prev_risk==F & next_risk==T & last_trial==F]$trial_type <- 'pre_risk'
    responses[trained==T & risk==F & prev_risk==T & next_risk==F & last_trial==F]$trial_type <- 'post_risk'
    responses[trained==T & risk==F & prev_risk==T & next_risk==T & last_trial==F]$trial_type <- 'between_risk'
    responses[trained==T & last_trial==T]$trial_type <- 'final'
    responses[learning==T]$trial_type <- 'learning'
    responses[critical==T]$trial_type <- 'criterion'
  
    #this section is for eye movement interpolation
    BI <- blink_find(D$raw)
    print('Interpolating blinks')
    responses$blink <- F
    responses$blink_time <- -1
    responses$blink_duration <- 0
    #for(blink_idx in 1:length(BI$t1)) {
    #  row_idx1 <- BI[blink_idx,]$t1-tau_const-1
    #  row_idx2 <- BI[blink_idx,]$t2-tau_const+1
    #  if(row_idx1>=2 && row_idx2<=length(D$raw$time)-1 && row_idx2-row_idx1-2<=blink_threshold){
    #    #print(c(blink_idx,row_idx1,row_idx2))
    #    ps1 <- D$raw[row_idx1-1,]$ps #????????? ????? ????????????
    #    ps2 <- D$raw[row_idx2+1,]$ps #???????? ????? ????????????
    #  
    #    D$raw[row_idx1:row_idx2,]$ps <- seq(from=ps1,to=ps2,length.out=row_idx2-row_idx1+1)
    #    #print(c(row_idx1,row_idx2))
    #  }
    
    #  #replacing bad intervals with NA
    #  if(row_idx1>=2 && row_idx2<=length(D$raw$time)-1 && row_idx2-row_idx1-2 > blink_threshold){
    #    #print(c(blink_idx,row_idx1,row_idx2))
    #    ps1 <- D$raw[row_idx1-1,]$ps #????????? ????? ????????????
    #    ps2 <- D$raw[row_idx2+1,]$ps #???????? ????? ????????????
    #  
    #    D$raw[row_idx1:row_idx2,]$ps <- NA
    #    #print(c(row_idx1,row_idx2))
    #  }
    #}
    #"blink" boolean
    for(trial_idx in 1:L){
      tt1 <- responses[trial_idx,]$time+t1 #epoch start
      tt2 <- responses[trial_idx,]$time+t2 #epoch end
    
      v1 <- as.logical((BI$t1>=tt1)*(BI$t1<=tt2)) #blinks started within the epoch
      v2 <- as.logical((BI$t2>=tt1)*(BI$t2<=tt2)) #blinks started within the epoch
      v3 <- as.logical((BI$t1<tt1)*(BI$t2>tt2))   #blinks started before epoch and ended after it
      v_total <- as.logical(v1+v2+v3)
      LOCAL_BLINKS <- BI[v_total] #dataset of totan blinks
      LOCAL_BLINKS <- LOCAL_BLINKS[LOCAL_BLINKS$duration>=blink_threshold]
      if(length(LOCAL_BLINKS$t1)>0) {
        responses[trial_idx,]$blink <- T
        responses[trial_idx,]$blink_time <- LOCAL_BLINKS[1,]$t1
        responses[trial_idx,]$blink_duration <- LOCAL_BLINKS[1,]$duration
      
      
      } 
    }
  
    varname <- function(x){gsub('-','m',paste('V_',x*50-2049,'_',x*50-2000,sep=''))}
    
    for(i in 1:100){
      eval(parse(text=paste('responses$',varname(i),'<-0',sep='')))
    }
    ##!!!!!
    # put NAs within non-interpolated blinks (done?)
    ##
    n_col <- length(responses[1,])
    avg_matrix <- matrix(ncol=100,nrow=length(responses$time),data=0)
    for(trial_idx in 1:length(responses$time)){
      row_idx <- responses[trial_idx,]$time-tau_const+1
      trial_signal <- D$raw[row_idx+(-1999:3000)]$ps
      trial_avg <- colMeans(matrix(trial_signal,ncol=100))
      avg_matrix[trial_idx,] <- trial_avg
    }
    
    for(col_idx in 1:100){
      responses[,n_col-100+col_idx] <- avg_matrix[,col_idx]
    }

    full_table <- rbind(full_table,responses)
    fname <- paste0(folder_out, 'resp_merged_teens_table_general_stat_w_blinks.csv')
    write.csv(full_table,file=fname,row.names=F)
    rm(list=ls(all=TRUE)[sapply(mget(ls(all=TRUE)), class) == "list"])
  }

c <- read.csv(fname)