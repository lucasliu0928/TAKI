

##########################################################################################
#4.CRRT/HD Start = End indexes
##########################################################################################
raw_time_df$RRT_START_EQ_END <- 0
rrt_start_eq_End_indexes <- which(raw_time_df$CRRT_START_TIME == raw_time_df$CRRT_END_TIME | 
                                  raw_time_df$HD_START_TIME == raw_time_df$HD_END_TIME)
raw_time_df$RRT_START_EQ_END[rrt_start_eq_End_indexes]<-1
table(raw_time_df$RRT_START_EQ_END)

##########################################################################################
#5. Check CRRT overlapped with HD
##########################################################################################
raw_time_df$CRRT_HD_OVERLAP <- 0
CRRT_HD_OVERLAP_indexes <- (which(ymd_hms(raw_time_df[,"CRRT_START_TIME"]) <= ymd_hms(raw_time_df[,"HD_END_TIME"]) &
                                  ymd_hms(raw_time_df[,"CRRT_END_TIME"]) >= ymd_hms(raw_time_df[,"HD_START_TIME"])))
raw_time_df[CRRT_HD_OVERLAP_indexes,"CRRT_HD_OVERLAP"] <- 1

##########################################################################################
#CRRT and ICU time
#1.CRRT start 24 hours before ICU start
#2.CRRT END 24 hours after ICU END 
##########################################################################################
CRRT_ICU_24h_indexes <- which(ymd_hms(raw_time_df[,"CRRT_START_TIME"]) < ymd_hms(raw_time_df[,"ICU_ADMSN_TIME"]) - hours(24))
CRRT_ICU_24h_indexes <- which(ymd_hms(raw_time_df[,"CRRT_END_TIME"]) > ymd_hms(raw_time_df[,"ICU_DISCH_TIME"]) + hours(24))

length(CRRT_ICU_24h_indexes)
