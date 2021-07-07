#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# DIURNAL FLUX VARIABILITY
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


## This script does the following:
##
##
##  1) Imports raw soil temperature and soil water content data. Raw soil tmperature data did
##     not require cleaning, while soil water content data did. A cleaned data set of soil
##     water content is therefore imported and substitutes the raw soil water content data (
##     data cleaning process not shown)
##  
##  2) Syncronizes soil water content and soil temperature data with fluxes (i.e. for each flux
##     it selects soil water content and soil temperature data which are closest in time)
##
##  3) Averages fluxes by fertilizer treatmetns (i.e. by spatial replicates)
##     
##     
##  4) Calculates daily flux ranges and makes barplots with relative frequencies
##  
##  5) Identifies maximum, minimum and (closest to) mean daily flux and makes barplots with relative
##     frequencies for maximum, minimum and (closest to) mean daily flux falling into each of the
##     24 hours of the day

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# Import and prepare ICOS soil temperature and water content data for synchronizatio with clean
# fluxes 9 and 8 points 
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")



#=====================================================================================================
# Import and check MEAN soil temperature and water content data
#=====================================================================================================

#Import

clean.soil.tw.2015 <- read.csv("./Data/Cleaned_data/Soil_T_water/ICOS_TempMoist_2015_clean.csv")
clean.soil.tw.2016 <- read.csv("./Data/Cleaned_data/Soil_T_water/ICOS_TempMoist_2016_clean.csv")


#Set colnames and rbind 2015 and 2016

colnames(clean.soil.tw.2015)
colnames(clean.soil.tw.2016)
colnames(clean.soil.tw.2016)  <- colnames(clean.soil.tw.2015)

colnames(clean.soil.tw.2015) == colnames(clean.soil.tw.2016)

clean.soil.tw <- rbind(clean.soil.tw.2015, clean.soil.tw.2016)

clean.soil.tw <- clean.soil.tw[ , -(1)]


#TEMPERATURE==========================================================================================

clean.soil.t.untidy  <- clean.soil.tw[ ,colnames(clean.soil.tw) == "TIMESTAMP" |
                                    substr(colnames(clean.soil.tw), 1, 2)  == "TS"]

#Make tidy (from wide to long format for ggplot)

library(reshape2)

clean.soil.t <- 
melt(clean.soil.t.untidy, id.vars = "TIMESTAMP",
     measure.vars = colnames(clean.soil.t.untidy)[2: ncol(clean.soil.t.untidy)],
     variable.name = "ID", value.name = "Soil.C")


clean.soil.t$Rep <- as.numeric(substr(clean.soil.t$ID, 3, 3))



pb <- txtProgressBar(min = 1, max = nrow(clean.soil.t), style = 3)

for(i in 1:nrow(clean.soil.t)){

    clean.soil.t$Depth.cm[i] = (substr(clean.soil.t$ID[i], 
                                         unlist(gregexpr(pattern ='_', clean.soil.t$ID[i]))+1,
                                         nchar(as.character(clean.soil.t$ID[i]))-2))

    setTxtProgressBar(pb, i)

    
    }


clean.soil.t <- clean.soil.t[ , c("TIMESTAMP", "ID", "Depth.cm", "Rep", "Soil.C")]

## write.csv(clean.soil.t, "./Data/Ancillary_data/ICOS_soil_temp_water_raw_clean/Tmp/4_clean.soil.t.csv", row.names = FALSE)
## clean.soil.t <- read.csv("./Data/Ancillary_data/ICOS_soil_temp_water_raw_clean/Tmp/4_clean.soil.t.csv")

clean.soil.t$TIMESTAMP  <-  as.POSIXct(clean.soil.t$TIMESTAMP)
clean.soil.t$Depth.cm  <-  factor(clean.soil.t$Depth.cm, levels = c("2", "5", "10", "30", "50"))
clean.soil.t$Rep  <-  as.factor(clean.soil.t$Rep)


#Soil t (plot, clean, average 5cm)


clean.soil.t.5cm <- clean.soil.t[clean.soil.t$Depth.cm == 5, ]



#calculate means and medians

clean.soil.t.5cm.spli <- split(clean.soil.t.5cm, clean.soil.t.5cm$TIMESTAMP)

summarize.soil.t <- function(x) {

    data.frame(
        Timestamp = x$TIMESTAMP[1],
        Mean.soil.t.C = mean(x$Soil.C),
        Median.soil.t.C = median(x$Soil.C)
        )
    }

summ.soil.t <- do.call(rbind, lapply(clean.soil.t.5cm.spli, summarize.soil.t))
colnames(summ.soil.t)[colnames(summ.soil.t) == "Timestamp"]  <- "ICOS.Timestamp"


write.csv(summ.soil.t, "./Data/Cleaned_data/Soil_T_water/summ_soil_t_5cm.csv",
          row.names = FALSE)


#END TEMPERATURE=====================================================================================


#WATER===============================================================================================


clean.soil.w.untidy  <- clean.soil.tw[ ,colnames(clean.soil.tw) == "TIMESTAMP" |
                                    substr(colnames(clean.soil.tw), 1, 2)  == "SM"]


clean.soil.w <- 
melt(clean.soil.w.untidy, id.vars = "TIMESTAMP",
     measure.vars = colnames(clean.soil.w.untidy)[2: ncol(clean.soil.w.untidy)],
     variable.name = "ID", value.name = "WFPS.perc")


clean.soil.w$Rep <- as.numeric(substr(clean.soil.w$ID, 3, 3))



pb <- txtProgressBar(min = 1, max = nrow(clean.soil.w), style = 3)

for(i in 1:nrow(clean.soil.w)){

    clean.soil.w$Depth.cm[i] = (substr(clean.soil.w$ID[i], 
                                         unlist(gregexpr(pattern ='_', clean.soil.w$ID[i]))+1,
                                         nchar(as.character(clean.soil.w$ID[i]))-2))

    setTxtProgressBar(pb, i)

    
    }

clean.soil.w <- clean.soil.w[ , c("TIMESTAMP", "ID", "Depth.cm", "Rep", "WFPS.perc")]

## write.csv(clean.soil.w, "./Data/Ancillary_data/ICOS_soil_temp_water_raw_clean/Tmp/4_clean.soil.w.csv",
##           row.names = FALSE)
## clean.soil.w <- read.csv("./Data/Ancillary_data/ICOS_soil_temp_water_raw_clean/Tmp/4_clean.soil.w.csv")

clean.soil.w$TIMESTAMP  <-  as.POSIXct(clean.soil.w$TIMESTAMP)
clean.soil.w$Depth.cm  <- as.character(clean.soil.w$Depth.cm)
clean.soil.w$Depth.cm[clean.soil.w$Depth.cm == "6"] <- as.character("06")
clean.soil.w$Depth.cm  <-  factor(clean.soil.w$Depth.cm, levels = c("06", "5", "10", "30", "50"))
clean.soil.w$Rep  <-  as.factor(clean.soil.w$Rep)





#Soil w (plot, clean, average 5cm)



clean.soil.w.5cm <- clean.soil.w[clean.soil.w$Depth.cm == "5", ]


#calculate means and medians

clean.soil.w.5cm.spli <- split(clean.soil.w.5cm, clean.soil.w.5cm$TIMESTAMP)

summarize.soil.w <- function(x) {

    data.frame(
        Timestamp = x$TIMESTAMP[1],
        Mean.soil.WFPS = mean(x$WFPS.perc),
        Median.soil.WFPS = median(x$WFPS.perc)
        )
    }

summ.soil.w <- do.call(rbind, lapply(clean.soil.w.5cm.spli, summarize.soil.w))
colnames(summ.soil.w)[colnames(summ.soil.w) == "Timestamp"]  <- "ICOS.Timestamp"
    
#END WATER============================================================================================

#=====================================================================================================
#END Import and check MEAN soil temperature and water content data
#=====================================================================================================


#=====================================================================================================
# Soil water content data at 5 cm depth required some cleaning. These cleaned data are impored here
#=====================================================================================================


#Assign new name to swc data that will be susbstituted (commented out for safety)
old.swc.data <- summ.soil.w

#Import cleaned swc data
summ.soil.w <- read.csv("./Data/Cleaned_data/Soil_T_water/Moist_5cm_Magdalena_20200220.csv")
summ.soil.w$Timestamp <- as.POSIXct(summ.soil.w$Timestamp)




# set data structure as old one

summ.soil.w <- summ.soil.w[
  ,
    -(which(names(summ.soil.w) %in% c("X", paste0("SM", 1:4, "_5cm"))))
        ]

colnames(summ.soil.w)[colnames(summ.soil.w) == "Timestamp"]  <- colnames(old.swc.data)[1]
colnames(summ.soil.w)[colnames(summ.soil.w) == "mean_5cm"]  <- colnames(old.swc.data)[2]
colnames(summ.soil.w)[colnames(summ.soil.w) == "median_5cm"]  <- colnames(old.swc.data)[3]


#=====================================================================================================
# END Soil water content data at 5 cm depth required some cleaning. These cleaned data are impored
# here
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# Import and prepare ICOS soil temperature and water content data for synchronizatio with clean
# fluxes 9 and 8 points 
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# Sychronize MEAN soil temperature and water content data and fluxes
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


fluxes <- read.csv("./Data/Processed_data/Fluxes_9_8_p_linear_regr.csv")

fluxes <- fluxes[ , !(colnames(fluxes) %in% (c("DateTime.start", "DateTime.stop", "g.N2O.N.min",
                                                "R2", "NRMSE", "Lower.95.limit.slope",
                                               "Upper.95.limit.slope", "Confint95.amplitude",
                                               "Abs.rel.confint.amplitude")))]

fluxes$Assigned.DateTime  <- as.POSIXct(fluxes$Assigned.DateTime)




#sychronize soil temperatures


soil.tm.5cm <- merge(summ.soil.t, summ.soil.w, by = "ICOS.Timestamp", all = TRUE)


soil.tm.5cm <- soil.tm.5cm[ ,colnames(soil.tm.5cm) %in% c("ICOS.Timestamp", "Mean.soil.t.C", "Mean.soil.WFPS" )]


soil.tm.5cm$Difftime.min <- as.numeric(NA)


dummy <- cbind(fluxes[1,], soil.tm.5cm[1,])
dummy$Difftime.min <- as.numeric(NA)


#set progress bar

pb <- txtProgressBar(min = 1, max = nrow(fluxes), style = 3)


for (i in 1:nrow(fluxes)) {



soil.tm.5cm$Difftime.min <- as.numeric(difftime(soil.tm.5cm$ICOS.Timestamp, fluxes$Assigned.DateTime[i], units = "mins"))

    

dummy[i, ] <- cbind(fluxes[i, ], soil.tm.5cm[which.min(abs(soil.tm.5cm$Difftime.min)), ])


#update progress bar

setTxtProgressBar(pb, i)

}




fluxes <- dummy

rm(dummy,soil.tm.5cm)

write.csv(fluxes, "./Data/Processed_data/Fluxes_9_8_linear_ICOS_T_W.csv", row.names = FALSE)

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END SySychronize MEAN soil temperature and water content data and fluxes
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# Flux and temperature ranges by chamber
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

fluxes <- read.csv("./Data/Processed_data/Fluxes_9_8_linear_ICOS_T_W.csv")

fluxes$Assigned.DateTime  <- as.POSIXct(fluxes$Assigned.DateTime)

fluxes$Assigned.DateTime  <- format(fluxes$Assigned.DateTime + 3600, tz = "UTC")

fluxes.spli <- split(fluxes, list(fluxes$ChambN, substr(fluxes$Assigned.DateTime, 1, 10)), drop = TRUE)

unlist(lapply(fluxes.spli, function(x) {nrow(x) == 24}))

daily.var.funct <-

    function(x) {

        x$Day  <- substr(x$Assigned.DateTime, 1, 10)
       
        x$N.flux.day <- nrow(x)
        
        x$Flux.range <- max(x$g.N2O.N.ha.day) - min(x$g.N2O.N.ha.day)
        #x$Flux.range2 <- if (max(x$g.N2O.N.ha.day) > 0 & min(x$g.N2O.N.ha.day) > 0)
                           # max(x$g.N2O.N.ha.day) - min(x$g.N2O.N.ha.day)#ok

                        #else if (max(x$g.N2O.N.ha.day) < 0 & min(x$g.N2O.N.ha.day) < 0)
                            #abs(min(x$g.N2O.N.ha.day)) - abs(max(x$g.N2O.N.ha.day))#only 7 cases in which only one (negative) flux per day was available. 

                        #else if (max(x$g.N2O.N.ha.day) > 0 & min(x$g.N2O.N.ha.day) < 0)
                            #abs(max(x$g.N2O.N.ha.day)) + abs(min(x$g.N2O.N.ha.day))#ok

                        #else 666
        #x$Flux.range3 <- max(max(x$g.N2O.N.ha.day), min(x$g.N2O.N.ha.day)) -
            #min(max(x$g.N2O.N.ha.day), min(x$g.N2O.N.ha.day))

                                    
        x$Flux.rank <- rank(x$g.N2O.N.ha.day)
        x$Mean.flux.day  <- mean(x$g.N2O.N.ha.day)

        for (i in 1:nrow(x)) {
        x$Dist.from.mean[i] <- max(x$g.N2O.N.ha.day[i] , mean(x$g.N2O.N.ha.day)) -
            min(x$g.N2O.N.ha.day[i] , mean(x$g.N2O.N.ha.day))
        }

        x$Closeness.to.mean.rank <- rank(x$Dist.from.mean)

        x$Temp.range <- max(x$Mean.soil.t.C) - min(x$Mean.soil.t.C)
        #x$Temp.range2  <- max(max(x$Mean.soil.t.C), min(x$Mean.soil.t.C)) -
            #min(max(x$Mean.soil.t.C), min(x$Mean.soil.t.C))
        
        x$Mean.Temp <- mean(x$Mean.soil.t.C)

        x$Moist.range <- max(x$Mean.soil.WFPS) - min(x$Mean.soil.WFPS)
        x$Mean.moist <- mean(x$Mean.soil.WFPS)
                  
        return(x)

 
        }

fluxes.var.spli <- lapply(fluxes.spli, daily.var.funct)

fluxes.var <- do.call(rbind, fluxes.var.spli)


pb <- txtProgressBar(min = 1, max = nrow(fluxes.var), style = 3)
for (i in 1:nrow(fluxes.var)) {

    if(fluxes.var$ChambN[i] %in% c(2, 5)) {fluxes.var$Treat[i] <- "Control"}#ok
    if(fluxes.var$ChambN[i] %in% c(4, 9)) {fluxes.var$Treat[i] <- "Normal_N"}#ok
    if(fluxes.var$ChambN[i] %in% c(3, 8)) {fluxes.var$Treat[i] <- "High_N"}#ok
    if(fluxes.var$ChambN[i] %in% c(6, 10)) {fluxes.var$Treat[i] <- "Pig_slurry"}#ok
    if(fluxes.var$ChambN[i] %in% c(1, 7)) {fluxes.var$Treat[i] <- "Biogas_digestate"}#ok

    if(fluxes.var$ChambN[i] %in% c(2, 4, 3, 6, 1)) {fluxes.var$Rep[i] <- 1}#ok
    if(fluxes.var$ChambN[i] %in% c(5, 9, 8, 10, 7)) {fluxes.var$Rep[i] <- 2}#ok

    #insert astronomical seasons here

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "03" & 
       substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:31)) {fluxes.var$Season[i] <- "Spring"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "03" & 
       !(substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:31))) {fluxes.var$Season[i] <-
                                                                                        "Winter"}
    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) %in% c("04", "05")) {fluxes.var$Season[i] <- "Spring"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "06" & 
       substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:30)) {fluxes.var$Season[i] <- "Summer"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "06" & 
       !(substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:30))) {fluxes.var$Season[i] <-
                                                                                        "Spring"}
                                                                 
    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) %in% c("07", "08")) {fluxes.var$Season[i] <- "Summer"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "09" & 
       substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:30)) {fluxes.var$Season[i] <- "Autumn"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "09" & 
       !(substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:30))) {fluxes.var$Season[i] <-
                                                                                        "Summer"}
                    
    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) %in% c("10", "11")) {fluxes.var$Season[i] <- "Autumn"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "12" & 
       substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:31)) {fluxes.var$Season[i] <- "Winter"}

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) == "12" & 
       !(substr(fluxes.var$Assigned.DateTime[i], 9, 10) %in% as.character(21:31))) {fluxes.var$Season[i] <-
                                                                                        "Autumn"}
                                              
    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) %in% c("01", "02")) {fluxes.var$Season[i] <- "Winter"}

    #season assignment checked!!

    #insert plant status (growth/no-growth) here

    if(substr(fluxes.var$Assigned.DateTime[i], 6, 7) %in% c("04", "05", "06", "07", "08"))
    {fluxes.var$Plant.status[i] <- "Growth"}

    else
    {fluxes.var$Plant.status[i] <- "No-growth"}

    #plant status assignment checked!!

    #update progress bar

    setTxtProgressBar(pb, i)

    
}


fluxes.var[as.integer(runif(50, 1, nrow(fluxes.var))), c("ChambN", "Rep")]#ok

unique(substr(fluxes.var[fluxes.var$Plant.status == "Growth", "Assigned.DateTime"], 6, 7))
#OK, growth period -> April, May, June, July, August

unique(substr(fluxes.var[fluxes.var$Plant.status == "No-growth", "Assigned.DateTime"], 6, 7))
#OK, growth period -> September, October, November, December, January, February, March

fluxes.var <- 
fluxes.var[ ,c("Assigned.DateTime", "ChambN",  "Treat", "Rep", "Season", "Plant.status", "N.conc",
               "P.coeff1.N2O", "g.N2O.N.ha.day", "Lower.95.confint.flux", "Upper.95.confint.flux",
               "ICOS.Timestamp", "Mean.soil.t.C", "Mean.soil.WFPS", "Difftime.min", "Day",  "N.flux.day",
               "Flux.range", "Flux.rank", "Mean.flux.day", "Dist.from.mean", "Closeness.to.mean.rank",
               "Temp.range", "Mean.Temp", "Moist.range", "Mean.moist")]

unique(fluxes.var[ , c("Day", "Season", "Plant.status")])


#set factor levels order


fluxes.var <- within(fluxes.var, {
    Rep = factor(Rep, levels = c("1","2"))
    Treat = factor(Treat, levels = c("Control", "Normal_N", "High_N", "Pig_slurry", "Biogas_digestate"))
    Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
    Plant.status = factor(Plant.status, levels = c("Growth", "No-growth"))
}

    )




fluxes.var <- fluxes.var[with(fluxes.var, order(Treat, Rep, Day)), ]

write.csv(fluxes.var, "./Data/Processed_data/Fluxes_soil_daily_var.csv", row.names = FALSE)


#=====================================================================================================
# Plots at maximum data disaggregation (i.e. chamber + season)
#=====================================================================================================

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END Flux and temperature ranges by chamber
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# Diurnal flux variability (fluxes AVERAGED by TREATMENT, n flux day = 24)
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
# Average fluxes by treatment
#=====================================================================================================


fluxes <- read.csv("./Data/Processed_data/Fluxes_9_8_linear_ICOS_T_W.csv",)

fluxes <- within(fluxes, {

    Assigned.DateTime  = as.POSIXct(Assigned.DateTime)
    ICOS.Timestamp = as.POSIXct(Assigned.DateTime)
    
    }
    )

fluxes$DateTime.UTC.plus.1  <- format(fluxes$Assigned.DateTime + 3600, tz = "UTC")

fluxes <- fluxes[ , c(
    "Assigned.DateTime", "DateTime.UTC.plus.1", "ChambN", "N.conc", "P.coeff1.N2O", "g.N2O.N.ha.day",
    "Lower.95.confint.flux", "Upper.95.confint.flux", "ICOS.Timestamp", "Mean.soil.t.C",   
    "Mean.soil.WFPS", "Difftime.min")]          


#insert treatment, astronomical season and plant status variables

pb <- txtProgressBar(min = 1, max = nrow(fluxes), style = 3)
for (i in 1:nrow(fluxes)) {

    if(fluxes$ChambN[i] %in% c(2, 5)) {fluxes$Treat[i] <- "Control"}#ok
    if(fluxes$ChambN[i] %in% c(4, 9)) {fluxes$Treat[i] <- "Normal_N"}#ok
    if(fluxes$ChambN[i] %in% c(3, 8)) {fluxes$Treat[i] <- "High_N"}#ok
    if(fluxes$ChambN[i] %in% c(6, 10)) {fluxes$Treat[i] <- "Pig_slurry"}#ok
    if(fluxes$ChambN[i] %in% c(1, 7)) {fluxes$Treat[i] <- "Biogas_digestate"}#ok

    if(fluxes$ChambN[i] %in% c(2, 4, 3, 6, 1)) {fluxes$Rep[i] <- 1}#ok
    if(fluxes$ChambN[i] %in% c(5, 9, 8, 10, 7)) {fluxes$Rep[i] <- 2}#ok

    #insert astronomical seasons here

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "03" & 
       substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:31)) {fluxes$Season[i] <- "Spring"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "03" & 
       !(substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:31))) {fluxes$Season[i] <-
                                                                                        "Winter"}
    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) %in% c("04", "05")) {fluxes$Season[i] <- "Spring"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "06" & 
       substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:30)) {fluxes$Season[i] <- "Summer"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "06" & 
       !(substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:30))) {fluxes$Season[i] <-
                                                                                        "Spring"}
                                                                 
    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) %in% c("07", "08")) {fluxes$Season[i] <- "Summer"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "09" & 
       substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:30)) {fluxes$Season[i] <- "Autumn"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "09" & 
       !(substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:30))) {fluxes$Season[i] <-
                                                                                        "Summer"}
                    
    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) %in% c("10", "11")) {fluxes$Season[i] <- "Autumn"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "12" & 
       substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:31)) {fluxes$Season[i] <- "Winter"}

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) == "12" & 
       !(substr(fluxes$DateTime.UTC.plus.1[i], 9, 10) %in% as.character(21:31))) {fluxes$Season[i] <-
                                                                                        "Autumn"}
                                              
    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) %in% c("01", "02")) {fluxes$Season[i] <- "Winter"}

    #season assignment checked!!

    #insert plant status (growth/no-growth) here

    if(substr(fluxes$DateTime.UTC.plus.1[i], 6, 7) %in% c("04", "05", "06", "07", "08"))
    {fluxes$Plant.status[i] <- "Growth"}

    else
    {fluxes$Plant.status[i] <- "No-growth"}

    #plant status assignment checked!!

    #update progress bar

    setTxtProgressBar(pb, i)

    
}


fluxes <- 
fluxes[ ,c("Assigned.DateTime", "DateTime.UTC.plus.1", "ChambN",  "Treat", "Rep", "Season", "Plant.status",
           "N.conc", "P.coeff1.N2O", "g.N2O.N.ha.day", "Lower.95.confint.flux", "Upper.95.confint.flux",
           "ICOS.Timestamp", "Mean.soil.t.C", "Mean.soil.WFPS", "Difftime.min")]


fluxes.spli <- split(fluxes, list(fluxes$Treat, substr(fluxes$DateTime.UTC.plus.1, 1, 13)), drop = TRUE)


summ.funct <- function(x){

    data.frame(
        DateTime = as.POSIXct(paste0(unique(substr(x$DateTime.UTC.plus.1, 1, 14)), "00:00")),
        ChambN = paste0(x$ChambN, collapse = ","),
        Treat = unique(x$Treat),
        Season = unique(x$Season),
        Plant.status = unique(x$Plant.status),
        N.conc = paste0(x$N.conc, collapse = ","),
        Mean.g.N2O.N.ha.day = mean(x$g.N2O.N.ha.day),
        ICOS.Timestamp = mean(x$ICOS.Timestamp),
        Mean.soil.t.C = mean(x$Mean.soil.t.C), 
        Mean.soil.WFPS = mean(x$Mean.soil.WFPS)
        )

    }

mean.fluxes <- do.call(rbind, lapply(fluxes.spli, summ.funct))
row.names(mean.fluxes) <- 1:nrow(mean.fluxes)

write.csv(mean.fluxes, "./Data/Processed_data/Fluxes_averaged_by_treatment.csv", row.names = FALSE)


#=====================================================================================================
# END Average fluxes by treatment
#=====================================================================================================



#=====================================================================================================
# Calculate daily variability variables for mean fluxes (complete sets only: n flux day = 24)
#=====================================================================================================
#mean fluxes from 81074 indivudual fluxes

mean.fluxes.spli <-
    split(mean.fluxes, with(mean.fluxes,list(Treat, substr(DateTime, 1, 10), drop = TRUE)))

nrowvect <- do.call(rbind, lapply(mean.fluxes.spli, function(x){nrow(x)}))

mean.fluxes.spli <-
    mean.fluxes.spli[-(which(do.call(rbind, lapply(mean.fluxes.spli, function(x){nrow(x) == 0}))))]


daily.var.funct <-

    function(x) {

        x$Day  <- substr(x$DateTime, 1, 10)
       
        x$N.flux.day <- nrow(x)
        
        x$Flux.range <- max(x$Mean.g.N2O.N.ha.day) - min(x$Mean.g.N2O.N.ha.day)
                                    
        x$Flux.rank <- rank(x$Mean.g.N2O.N.ha.day)
        x$Mean.flux.day  <- mean(x$Mean.g.N2O.N.ha.day)

        for (i in 1:nrow(x)) {
        x$Dist.from.mean[i] <- max(x$Mean.g.N2O.N.ha.day[i] , mean(x$Mean.g.N2O.N.ha.day)) -
            min(x$Mean.g.N2O.N.ha.day[i] , mean(x$Mean.g.N2O.N.ha.day))
        }

        x$Closeness.to.mean.rank <- rank(x$Dist.from.mean)

        x$Temp.range <- max(x$Mean.soil.t.C) - min(x$Mean.soil.t.C)

        x$Freezing <- if (min(x$Mean.soil.t.C) < 0) "Days with freezing"
                      else
                          "Days without freezing"

        x$Mean.Temp <- mean(x$Mean.soil.t.C)

        x$Moist.range <- max(x$Mean.soil.WFPS) - min(x$Mean.soil.WFPS)
        x$Mean.moist <- mean(x$Mean.soil.WFPS)
                  
        return(x)

 
        }

mean.fluxes.spli.var <- lapply(mean.fluxes.spli, daily.var.funct)

mean.fluxes.var <- do.call(rbind, mean.fluxes.spli.var)
row.names(mean.fluxes.var) <- 1:nrow(mean.fluxes.var)
#mean fluxes from 81073 indivudual fluxes


#Limit analysis to complete sets of data (n flux day = 24)
flux.var.24 <- mean.fluxes.var[mean.fluxes.var$N.flux.day == 24, ]

#limit analysis to day for which all treatmens are available
flux.var.24.spli <- split(flux.var.24, flux.var.24$Day)
index.5.treat <- do.call(rbind, lapply(flux.var.24.spli, function(x){length(unique(x$Treat)) == 5}))
flux.var.24.spli <- flux.var.24.spli[index.5.treat]

flux.var.24<- do.call(rbind, flux.var.24.spli)


flux.var.24$Treat <-
    factor(flux.var.24$Treat,
           levels = c("Control", "Normal_N", "High_N", "Pig_slurry", "Biogas_digestate"))

flux.var.24$Freezing <-
    factor(flux.var.24$Freezing,
           levels = c("Days without freezing", "Days with freezing"))



#Generate df with max, mean and min daily fluxes only

flux.var.24.spli <- split(flux.var.24, with(flux.var.24, list(Day, Treat)), drop = TRUE)


extr.max.min.mean <- function(x){

    tmp <- data.frame(
        Day = unique(x$Day),
        Season = unique(x$Season),
        Treat = unique(x$Treat),
        Freezing = unique(x$Freezing),
        Flux.range = unique(x$Flux.range),
        Max.flux.rank = x$Flux.rank[which.max(x$Flux.rank)],
        Hour.max = as.numeric(format(x$DateTime[which.max(x$Flux.rank)], "%H")),
        Min.closeness.to.mean.rank = x$Closeness.to.mean.rank[which.min(x$Closeness.to.mean.rank)],
        Hour.mean = as.numeric(format(x$DateTime[which.min(x$Closeness.to.mean.rank)], "%H")),
        Min.flux.rank = x$Flux.rank[which.min(x$Flux.rank)],
        Hour.min  = as.numeric(format(x$DateTime[which.min(x$Flux.rank)], "%H"))
        )
   
    }

hour.max.mean.min <- do.call(rbind,lapply(flux.var.24.spli, extr.max.min.mean))
row.names(hour.max.mean.min)  <- 1:nrow(hour.max.mean.min)


#=====================================================================================================
# END Calculate daily variability variables for mean fluxes (complete sets only: n flux day = 24)
#=====================================================================================================




#=====================================================================================================
# Diurnal flux range by treat with separation by freezing/no freezing
#=====================================================================================================


library(ggplot2)

X11(24/2.54, 16/2.54)

png("./Figures/Flux_daily_variability/By_treat_and_higher_agg/Flux_range_hist.png", width = 24, height = 16, units = "cm", res = 600)


ggplot(hour.max.mean.min)+
    facet_wrap(~ Freezing + Treat , ncol = 5, nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 70, hjust = 0.8))+
    xlab(expression(paste("Daily flux range [","g ", N[2], "O-N ", ha^{-1}, d^{-1},"]")))+
    ylab("Relative frequency")+
    geom_histogram(binwidth = 10, aes(x=Flux.range,y=10*..density..), col = "black", boundary = 0)+
    #coord_cartesian(ylim = c(0, 0.1))+
    scale_x_continuous(breaks=seq(0, 250, 20))+
    scale_y_continuous(breaks=seq(0, 0.6, 0.1))

graphics.off()



#=====================================================================================================
# Diurnal flux range by treat with separation by freezing/no freezing
#=====================================================================================================

#=====================================================================================================
# Probability distribution of Max Mean Min
#=====================================================================================================

#Max

X11(16/2.54, 24/2.54)

png("./Figures/Flux_daily_variability/By_treat_and_higher_agg/Max_flux_prob_fnf_hist.png", width = 16, height = 24, units = "cm", res = 600)


ggplot(hour.max.mean.min)+
    #facet_wrap(~Freezing, nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.title = element_blank(),
           axis.text.x = element_text(angle = 70, vjust = 0.5, size = 16),
           axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 18))+
    scale_x_continuous(breaks = seq(0, 23, 1), labels = c(rbind(as.character(seq(0, 23, 2)), rep("", 12))),
                        expand = c(0.03,0.03))+
    scale_y_continuous(breaks = seq(0, 0.1, 0.01), expand = c(0.0007,0.0007))+
    coord_cartesian(ylim = c(0,0.08))+
    ## ylab("Relative frequency")+
    ## xlab("Hour of day (0-23)")+
    ggtitle("Diurnal probability distribution of MAXIMUM fluxes")+
    geom_bar(aes(x = Hour.max, y = (..count..)/sum(..count..)), width = 1, col = "black")

graphics.off()



#Mean

X11(16/2.54, 24/2.54)

png("./Figures/Flux_daily_variability/By_treat_and_higher_agg/Mean_flux_prob_fnf_hist.png", width = 16, height = 24, units = "cm", res = 600)


ggplot(hour.max.mean.min)+
    #facet_wrap(~Freezing, nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 70, vjust = 0.5, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 18))+
    scale_x_continuous(breaks = seq(0, 23, 1), labels = c(rbind(as.character(seq(0, 23, 2)), rep("", 12))),
                        expand = c(0.03,0.03))+
    scale_y_continuous(breaks = seq(0, 0.1, 0.01), expand = c(0.0007,0.0007))+
    coord_cartesian(ylim = c(0,0.08))+
    ## ylab("Relative frequency")+
    ## xlab("Hour of day (0-23)")+
    ggtitle("Diurnal probability distribution of MEAN fluxes")+
    geom_bar(aes(x = Hour.mean, y = (..count..)/sum(..count..)), width = 1, col = "black")

graphics.off()


#Min

X11(16/2.54, 24/2.54)

png("./Figures/Flux_daily_variability/By_treat_and_higher_agg/Min_flux_prob_fnf_hist.png", width = 16, height = 24, units = "cm", res = 600)


ggplot(hour.max.mean.min)+
    #facet_wrap(~Freezing, nrow = 2)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          axis.title = element_blank(),
          axis.text.x = element_text(angle = 70, vjust = 0.5, size = 16),
          axis.text.y = element_text(size = 16),
          plot.title = element_text(size = 18))+
    scale_x_continuous(breaks = seq(0, 23, 1), labels = c(rbind(as.character(seq(0, 23, 2)), rep("", 12))),
                       expand = c(0.03,0.03))+
    scale_y_continuous(breaks = seq(0, 0.1, 0.01), expand = c(0.0007,0.0007))+
    coord_cartesian(ylim = c(0,0.08))+
    ## ylab("Relative frequency")+
    ## xlab("Hour of day (0-23)")+
    ggtitle("Diurnal probability distribution of MINIMUM fluxes")+
    geom_bar(aes(x = Hour.min, y = (..count..)/sum(..count..)), width = 1, col = "black")

graphics.off()



#Merge max,mean and min plots


png.list <- list.files("./Figures/Flux_daily_variability/By_treat_and_higher_agg", patt = "*prob_fnf*")
png.list <- paste0("./Figures/Flux_daily_variability/By_treat_and_higher_agg/", png.list)

library("png")
library("grid")
library("gridExtra")


plots <- lapply(png.list, function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}
)


ggsave(
    "./Figures/Flux_daily_variability/By_treat_and_higher_agg/Max_mean_min_fnf.png",
    width=24, height=12, marrangeGrob(grobs = plots, nrow=1, ncol=3,top=NULL,
            bottom = textGrob("Hour of day (0-23)", gp = gpar(fontsize = 25, font = 8)),
            left = textGrob("Relative frequency",
                    gp = gpar(fontsize = 25, font = 8), rot = 90))
)


#=====================================================================================================
# END Probability distribution of Max Mean Min
#=====================================================================================================

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END Diurnal flux variability (fluxes AVERAGED by TREATMENT, n flux day = 24)
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END DIURNAL FLUX VARIABILITY
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


