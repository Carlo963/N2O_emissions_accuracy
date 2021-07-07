#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# EXPECTED ERROR IN CUMULATIVE FLUX ESTIMATES AS FUNCTION OF MEASURING FREQUENCY
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")



fluxes <- read.csv("./Data/Processed_data/Fluxes_9_8_p_linear_regr.csv")


#insert year col

fluxes$Year <- substr(fluxes$Assigned.DateTime, 1, 4)

fluxes <- fluxes[ ,c("Assigned.DateTime", "DateTime.start", "DateTime.stop","Year", "ChambN", "N.conc", "g.N2O.N.min", "R2", "NRMSE", "g.N2O.N.ha.day")]



fluxes <- 
within(fluxes,{
               Assigned.DateTime  <- as.POSIXct(Assigned.DateTime)
               DateTime.start <- as.POSIXct(DateTime.start)
               DateTime.stop <- as.POSIXct(DateTime.stop)
               Year <- as.factor(Year)
               ChambN <- as.factor(ChambN)
               }
       )





#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# DEFINITION OF PARAMETERS AND FUNCTIONS FOR CALUCALTIONS OF EXPECTED ERROR
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

# Define simulated measuring frequncies for wich error will be calculated


# Simulation of automatic system (interval between susequent measurements in hours)
auto <- c(1,2,3,4,6,8,12)

# Simulation of manual system(interval between susequent measurements in hours)
manual <- seq(24, 168, 24)


#Virtual start date-hour for subsampling simulation(i.e. start date of virtual sampling
#times for different measuring frequencies; all start at same hour; missing data from virtual
#start to first measurement will be consdiered gap)

lapply(

split(fluxes, fluxes$ChambN), 


function(x) {

    first.flux <- min(x$Assigned.DateTime)

    }
)#All 10 time series start in afternoon of 2015-05-19.

#Virtual start will be 2015-05-19 00:00:00.
virt.start <- as.POSIXct("2015-05-19 00:00:00")


#Sampling hours for manual simulation (only hours from 9 to 12)

samp.hours.man <- 2:4

samp.hours.man.ABMTs <- data.frame(

    Hours = 9:11,
    Source = "ABMTs"
      
    )

samp.hours.man.EDBMTs <- data.frame(

    Hours = 2:4,
    Source = "EDBMTs"
      
    )

ABMTs.EDBMTs.list <- list(samp.hours.man.ABMTs, samp.hours.man.EDBMTs)



#Modified accumualtion function (omit integreation if gap larger than sampling frequncy)


accumulation.function.no.gap.integration <- function(x) {


x <- x[order(x$Assigned.DateTime), ]

x$Difftime.hours <- NA
x$Difftime.hours[1] <- 0

x$Trapezoid.area  <- NA
x$Trapezoid.area[1]  <- 0

x$Cum.gN2O.N.ha.total  <- NA
x$Cum.gN2O.N.ha.total[1] <- 0    
    


for(i in 2:nrow(x)) {


    t0 <- x$Assigned.DateTime[i-1]
    t1 <- x$Assigned.DateTime[i]
    flux0 <- x$g.N2O.N.ha.day[i-1]
    flux1 <- x$g.N2O.N.ha.day[i]


     x$Difftime.hours[i] <-  difftime(t1,t0, units = "hours")
    
     x$Trapezoid.area[i] <- mean(c(flux0, flux1)) * difftime(t1,t0, units = "days")


    if(difftime(t1,t0, units = "hours") > unique(x$Samp.freq)) {

        x$Cum.gN2O.N.ha.total[i]  <- x$Cum.gN2O.N.ha.total[i-1] 

        }
    
    else {

     x$Cum.gN2O.N.ha.total[i] <- mean(c(flux0, flux1)) * difftime(t1,t0, units = "days") + x$Cum.gN2O.N.ha.total[i -1]
        }



   
                            }

    return(x)
    
}


#Modified accumualtion function (omit integreation if gap larger than sampling frequncy and calculate total
# integrated time)


accumulation.function.no.gap.integration.acc.time <- function(x) {


x <- x[order(x$Assigned.DateTime), ]

x$Difftime.hours <- NA
x$Difftime.hours[1] <- 0

x$Integrated.time.hours <- NA
x$Integrated.time.hours[1] <- 0    

x$Trapezoid.area  <- NA
x$Trapezoid.area[1]  <- 0

x$Cum.gN2O.N.ha.total  <- NA
x$Cum.gN2O.N.ha.total[1] <- 0    
    


for(i in 2:nrow(x)) {


    t0 <- x$Assigned.DateTime[i-1]
    t1 <- x$Assigned.DateTime[i]
    flux0 <- x$g.N2O.N.ha.day[i-1]
    flux1 <- x$g.N2O.N.ha.day[i]


     x$Difftime.hours[i] <-  difftime(t1,t0, units = "hours")
    
     x$Trapezoid.area[i] <- mean(c(flux0, flux1)) * difftime(t1,t0, units = "days")


    if(difftime(t1,t0, units = "hours") > unique(x$Samp.freq)) {

        x$Cum.gN2O.N.ha.total[i]  <- x$Cum.gN2O.N.ha.total[i-1]
        x$Integrated.time.hours[i]  <- x$Integrated.time.hours[i-1] 

        }
    
    else {

     x$Cum.gN2O.N.ha.total[i] <-
         mean(c(flux0, flux1)) * difftime(t1,t0, units = "days") + x$Cum.gN2O.N.ha.total[i - 1]

     x$Integrated.time.hours[i]  <- difftime(t1,t0, units = "hours") + x$Integrated.time.hours[i-1]       
        }



   
                            }

    return(x)
    
}




# Prepare the gap filled flux dataset for assessing errors in cumfluxes

fluxes.gap.filled <-
    read.csv("./Data/Processed_data/Gap_filling/Final/Gap_filled_flux_df_COMPLETE/ALL_chambers.csv")



fluxes.gap.filled <- fluxes.gap.filled[ , c("Assigned.DateTime", "ChambN", "g.N2O.N.ha.day")]

fluxes.gap.filled$Assigned.DateTime <- as.POSIXct(fluxes.gap.filled$Assigned.DateTime) 


# LIMIT TO EXACTLY ONE YEAR FROM FIRST MEASUREMENT!!!

fluxes.gap.filled <-
    fluxes.gap.filled[fluxes.gap.filled$Assigned.DateTime < as.POSIXct("2016-05-18 16:00:00"), ]

difftime(max(fluxes.gap.filled$Assigned.DateTime), min(fluxes.gap.filled$Assigned.DateTime), units = "days")




fluxes.gap.filled$ChambN <- as.factor(fluxes.gap.filled$ChambN)


fluxes.gap.filled$Round.hour <- round(fluxes.gap.filled$Assigned.DateTime, "hours")

fluxes.gap.filled$Year <- as.factor(substr(fluxes.gap.filled$Round.hour, 1, 4))

fluxes.gap.filled <- fluxes.gap.filled[, c("Assigned.DateTime", "Round.hour", "Year", "ChambN", "g.N2O.N.ha.day")]


#=====================================================================================================
# Average the gap filled flux dataset by treatment for assessing errors in cumfluxes
#=====================================================================================================


#Insert treatment column


for(i in 1:nrow(fluxes.gap.filled)){

    if(fluxes.gap.filled$ChambN[i] %in% c(2, 5)) {fluxes.gap.filled$Treat[i] <- "Control"}#ok
    if(fluxes.gap.filled$ChambN[i] %in% c(4, 9)) {fluxes.gap.filled$Treat[i] <- "Normal_N"}#ok
    if(fluxes.gap.filled$ChambN[i] %in% c(3, 8)) {fluxes.gap.filled$Treat[i] <- "High_N"}#ok
    if(fluxes.gap.filled$ChambN[i] %in% c(6, 10)) {fluxes.gap.filled$Treat[i] <- "Pig_slurry"}#ok
    if(fluxes.gap.filled$ChambN[i] %in% c(1, 7)) {fluxes.gap.filled$Treat[i] <- "Biogas_digestate"}#ok

    }

fluxes.gap.filled <- fluxes.gap.filled[ ,
                                       c("Assigned.DateTime", "Round.hour", "Year", "Treat", "g.N2O.N.ha.day")]



fluxes.gap.filled.spli <- split(
    fluxes.gap.filled, with(fluxes.gap.filled, list(Treat, substr(Assigned.DateTime, 1, 13)), drop = TRUE))



summ.funct <- function(x){

    data.frame(
        Assigned.DateTime = mean(x$Assigned.DateTime),
        Round.hour = unique(x$Round.hour),
        Year = unique(x$Year),
        Treat = unique(x$Treat),
        g.N2O.N.ha.day = mean(x$g.N2O.N.ha.day)
        )

    }

mean.gap.filled.fluxes <- do.call(rbind, lapply(fluxes.gap.filled.spli, summ.funct))
row.names(mean.gap.filled.fluxes) <- 1:nrow(mean.gap.filled.fluxes)

mean.gap.filled.fluxes$Treat <- factor(
    mean.gap.filled.fluxes$Treat,
    levels = c("Control", "Normal_N", "High_N", "Pig_slurry","Biogas_digestate"))              
      


mean.gap.filled.fluxes <- mean.gap.filled.fluxes[
    with(mean.gap.filled.fluxes, order(Treat, Assigned.DateTime)),
                                                      ]

fluxes <- mean.gap.filled.fluxes

#=====================================================================================================
# Average the gap filled flux dataset by treatment for assessing errors in cumfluxes
#=====================================================================================================

    
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END DEFINITION OF PARAMETERS AND FUNCTIONS FOR CALUCALTIONS OF EXPECTED ERROR
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================




#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FULL AUTO EXPECTED ERROR IN CUMFLUX FOR DIFFERENT SAMPLING FREQUENCIES BY TREAT 
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


# Split flux df by chamber

flux.list <- split(fluxes, fluxes$Treat)

library(ggplot2)


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FULL AUTO CALCATION & INDIVIDUAL PLOTS
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

#START FULL AUTO FUNCTION

fun.apply.ABMTs.EDBMTs <- function(w){


full.auto.cumflux.error <- function(x){



# Extract fluxes for one chamber
dummy.1 <- x

dummy <- dummy.1


# Generate vector with rounded hour for facilitating calculations of difftimes
dummy <- dummy[order(dummy$Assigned.DateTime), ]
dummy$Round.hour <- as.POSIXct(paste0(substr(dummy$Assigned.DateTime, 1, 13), ":00:00"))

dummy <- dummy[ , c("Assigned.DateTime", "Round.hour", "Year", "Treat", "g.N2O.N.ha.day")]




#=====================================================================================================
#=====================================================================================================
# Select all theoretical measuring times simulating automatic and manual system
#=====================================================================================================
#=====================================================================================================

subsamp.list  <- list()

for(i in auto){

    
    for(z in 1:i) {

        df.sampfreq  = data.frame(

        Treat = unique(dummy$Treat),
        Samp.freq = i,
        Forward.shift = z - 1,
        Start = virt.start,
        First.meas = dummy$Round.hour[1],
        DateTime = seq(
            from = virt.start + (z-1) * 3600, to = max(dummy$Round.hour) + 24*3600, by = paste0(i, " hours")
        )

        )

        #print(df.sampfreq)

        name = paste("Treat", unique(dummy$Treat), "Forw.shift", z-1, "Sampfreq", i, sep = ".")

        subsamp.list[[name]] = df.sampfreq
 
        
        }

        
    }




# Select theoretical measuring times simulating manual system


for(i in manual){

    for(q in seq(0, i-24, 24)){

        for(z in w$Hours) {

            df.sampfreq  = data.frame(

                Treat = unique(dummy$Treat),
                Samp.freq = i,
                Forward.shift = z+q,
                Start = virt.start,
                First.meas = dummy$Round.hour[1],
                DateTime = seq(
                    from = virt.start + z * 3600 + q * 3600,
                    to = max(dummy$Round.hour) + 8*24*3600,
                    by = paste0(i, " hours")
                )

            )

            #print(df.sampfreq)
            name = paste("Treat", unique(dummy$Treat), "Forw.shift", q, "Hour", z, "Sampfreq", i, sep = ".")
            subsamp.list[[name]] = df.sampfreq
        }

        }
  
    }

all.subs.no.gaps <- do.call(rbind, subsamp.list)
row.names(all.subs.no.gaps)  <- 1:nrow(all.subs.no.gaps)

#=====================================================================================================
#=====================================================================================================
# END Select all theoretical measuring times simulating automatic and manual system
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# For each tehoretical time series (each instance of each measuring frequency)
# extract available fluxes
#=====================================================================================================
#=====================================================================================================

subsamp.spli <- split(all.subs.no.gaps, with(all.subs.no.gaps, list(Samp.freq, Forward.shift)), drop = TRUE)

#Each sf in subsamp.spli contains one time series


extr.sampfreq.meas <- function(x){

    subsamp <- dummy[dummy$Round.hour %in% x$DateTime, ]

    subsamp$Samp.freq  <- unique(x$Samp.freq)

    subsamp$Forward.shift  <- unique(x$Forward.shift)
    
    return(subsamp)


    }


subsamp.list <-lapply(subsamp.spli, extr.sampfreq.meas)


#=====================================================================================================
#=====================================================================================================
# END For each tehoretical time series (each instance of each measuring frequency)
# extract available fluxes
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# Calculate cumfluxes for each subsampled timeseries
#=====================================================================================================
#=====================================================================================================


subsamp.list <- lapply(subsamp.list, accumulation.function.no.gap.integration.acc.time)
                                     


subsamp <- do.call(rbind, subsamp.list)

write.csv(subsamp,
          paste0(
         "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Accumulation_timeseries_subsamples/",
              "Accumulation_timeseries_subsampling_Treat_", unique(subsamp$Treat), "_", unique(w$Source), ".csv"),
          row.names = FALSE
          )


#=====================================================================================================
#=====================================================================================================
# END Calculate cumfluxes for each subsampled timeseries
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# Extract final cumfluxes
#=====================================================================================================
#=====================================================================================================

extr.final.cumflux <- function(x){


    last.cumflux <-
        x[
        nrow(x),
        c("Assigned.DateTime", "Treat", "Samp.freq", "Forward.shift", "Cum.gN2O.N.ha.total",
          "Integrated.time.hours")
                      ]
    last.cumflux$DateTime.first = x$Assigned.DateTime[1]
    colnames(last.cumflux)[colnames(last.cumflux) == "Assigned.DateTime"] = as.character("DateTime.last")
    last.cumflux = last.cumflux[, c("DateTime.first", "DateTime.last", "Treat", "Samp.freq", "Forward.shift",
                                    "Cum.gN2O.N.ha.total", "Integrated.time.hours")]

    return(last.cumflux)

    }

last.cumflux.subsamp <- do.call(rbind, lapply(subsamp.list, extr.final.cumflux))


last.cumflux.subsamp.list <- split(last.cumflux.subsamp, last.cumflux.subsamp$Samp.freq)


calc.mean.last.cumfl.freq <- function(x){

    x$Mean.cumflux <- mean(x$Cum.gN2O.N.ha.total)
    x$Bias.abs  <- NA
    x$Bias.perc  <- NA
    x$CV.perc <- sd(x$Cum.gN2O.N.ha.total) / mean(x$Cum.gN2O.N.ha.total)*100
    x$Mean.integrated.t.h <- mean(x$Integrated.time.hours)
    x$Meas.times <- unique(w$Source)
    
    return(x)


    }


last.cumflux.subsamp <- do.call(rbind, lapply(last.cumflux.subsamp.list, calc.mean.last.cumfl.freq))



#This calculates exact biases (reference cumfluxes refer to exactly same interval as the subsample)

 for (i in 1:nrow(last.cumflux.subsamp)){
                                      
    last.ref.cumflux =
      subsamp[subsamp$Samp.freq == 1 &
              subsamp$Forward.shift == 0 &
              subsamp$Treat == unique(last.cumflux.subsamp$Treat) &        
              subsamp$Assigned.DateTime == last.cumflux.subsamp$DateTime.last[i]
                            , "Cum.gN2O.N.ha.total"]


    first.ref.cumflux =
      subsamp[subsamp$Samp.freq == 1 &
              subsamp$Forward.shift == 0 &
              subsamp$Treat == unique(last.cumflux.subsamp$Treat) &        
              subsamp$Assigned.DateTime == last.cumflux.subsamp$DateTime.first[i]
                            , "Cum.gN2O.N.ha.total"]
     

     ref.cumflux = last.ref.cumflux - first.ref.cumflux
     
     last.cumflux.subsamp$ref.cumflux[i] = ref.cumflux
     last.cumflux.subsamp$Bias.abs[i] = (last.cumflux.subsamp$Mean.cumflux[i] - ref.cumflux) 
     last.cumflux.subsamp$Bias.perc[i] =
         (last.cumflux.subsamp$Mean.cumflux[i] - ref.cumflux) / ref.cumflux  * 100

     }

                                      


write.csv(last.cumflux.subsamp,
          paste0(
              "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Last_cumflux_subsamples/",
              "Total_cumflux_subsampling_Treat_", unique(last.cumflux.subsamp$Treat), "_", unique(w$Source), ".csv"),
          row.names = FALSE
          )
#Set max y depending on treat

     if(unique(last.cumflux.subsamp$Treat) == "Control") maxy <- 1100
     if(unique(last.cumflux.subsamp$Treat) == "Normal_N") maxy <- 1400
     if(unique(last.cumflux.subsamp$Treat) == "High_N") maxy <-  2600
     if(unique(last.cumflux.subsamp$Treat) == "Pig_slurry") maxy <-  2900
     if(unique(last.cumflux.subsamp$Treat) == "Biogas_digestate") maxy <-  3200
    
   
if(unique(last.cumflux.subsamp$Treat) == "Biogas_digestate"){    
#Biogas digestate ( x scale)

png(paste0(
    "./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/Tot_cumflux_by_freq_Treat_",
    unique(last.cumflux.subsamp$Treat), "_", unique(w$Source), ".png"),
    width = 37, height = 34, units = "cm", res = 300)


         print(
ggplot()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 60),
          axis.text.y = element_blank(),
          axis.title = element_blank())+
    geom_hline(yintercept = last.cumflux.subsamp$Mean.cumflux[1], linetype = "dotted")+
    annotate("text", x = 82,
             y = maxy,
             label = paste0(unique(last.cumflux.subsamp$Treat), " (", unique(w$Source), ")"),
             size = 22)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Cum.gN2O.N.ha.total), pch = 1, size = 15)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Mean.cumflux), col = "red", size = 15)+
    scale_x_continuous(breaks = seq(0, 170, 24))+
    coord_cartesian(ylim = c(0, maxy), xlim = c(24, 170))+
    scale_y_continuous(
                       breaks =
                           seq(from = 0,
                               to = maxy,
                               by = maxy/5
                               ))


)
    graphics.off()
   

    }

    else {

        png(paste0(
    "./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/Tot_cumflux_by_freq_Treat_",
    unique(last.cumflux.subsamp$Treat), "_", unique(w$Source), ".png"),
    width = 37, height = 34, units = "cm", res = 300)

#All cases except biogas digestate (no x scale)
        print(
ggplot()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          axis.title = element_blank())+
    geom_hline(yintercept = last.cumflux.subsamp$Mean.cumflux[1], linetype = "dotted")+
    annotate("text", x = 65,
             y = maxy,
             label = paste0(unique(last.cumflux.subsamp$Treat), " (", unique(w$Source), ")"),
             size = 22)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Cum.gN2O.N.ha.total),  pch = 1, size = 15)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Mean.cumflux), col = "red", size = 15)+
    scale_x_continuous(breaks = seq(0, 170, 24))+
    coord_cartesian(ylim = c(0, maxy), , xlim = c(24, 170))+
    scale_y_continuous(
                       breaks =
                           seq(from = 0,
                               to = maxy,
                               by = maxy/5
                               ))


)


        }



if(unique(last.cumflux.subsamp$Treat) == "Biogas_digestate"){    
#Biogas digestate ( x scale)


png(paste0(
    "./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/Tot_cumflux_by_freq_Treat_",
    unique(last.cumflux.subsamp$Treat),"_AUTO.png"),
    width = 37, height = 34, units = "cm", res = 300)


         print(
ggplot()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 60),
          axis.text.y = element_text(size = 60),
          axis.title = element_blank())+
    geom_hline(yintercept = last.cumflux.subsamp$Mean.cumflux[1], linetype = "dotted")+
    annotate("text", x = 5.7,
             y = maxy,
             label = paste0(unique(last.cumflux.subsamp$Treat), " (AUTO)"),
             size = 22)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Cum.gN2O.N.ha.total), pch = 1, size = 15)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Mean.cumflux), col = "red", size = 15)+
    scale_x_continuous(breaks = c(1:4, 6, 8, 12))+
    coord_cartesian(ylim = c(0, maxy), xlim = c(1, 12))+
    scale_y_continuous(
                       breaks =
                           seq(from = 0,
                               to = maxy,
                               by = maxy/5
                               ))

)
    graphics.off()

        }

    else {
#All cases except biogas digestate (no x scale)

            png(paste0(
    "./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/Tot_cumflux_by_freq_Treat_",
    unique(last.cumflux.subsamp$Treat),"_AUTO.png"),
    width = 37, height = 34, units = "cm", res = 300)

         print(
ggplot()+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 60),
          axis.title = element_blank())+
    geom_hline(yintercept = last.cumflux.subsamp$Mean.cumflux[1], linetype = "dotted")+
    annotate("text", x = 4.3,
             y = maxy,
             label = paste0(unique(last.cumflux.subsamp$Treat), " (AUTO)"),
             size = 22)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Cum.gN2O.N.ha.total), pch = 1, size = 15)+
    geom_point(data = last.cumflux.subsamp, aes(x = Samp.freq , y = Mean.cumflux), col = "red", size = 15)+
    scale_x_continuous(breaks = seq(1, 12, 1))+
    coord_cartesian(ylim = c(0, maxy), xlim = c(1, 12))+
    scale_y_continuous(
                       breaks =
                           seq(from = 0,
                               to = maxy,
                               by = maxy/5
                               ))

)

            }
   

graphics.off()    



#=====================================================================================================
#=====================================================================================================
# END Extract final cumfluxes
#=====================================================================================================
#=====================================================================================================

    
    }



lapply(flux.list, full.auto.cumflux.error)

}

#END FULL AUTO FUNCTION

lapply(ABMTs.EDBMTs.list, fun.apply.ABMTs.EDBMTs)

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END FULL AUTO CALCATION & INDIVIDUAL PLOTS
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================




#=====================================================================================================
#=====================================================================================================
# MAKE COMPOSITE PLOTS
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
# Generate lists of plots  
#=====================================================================================================

#AUTO
png.list.AUTO <- list.files("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved",
                       patt = "*AUTO*")

png.list.AUTO <- png.list.AUTO[c(2, 4, 3, 5, 1)]

png.list.AUTO <- paste0("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/", png.list.AUTO)



#ABMTs
png.list.ABMTs <- list.files("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved",
                       patt = "*ABMTs*")

png.list.ABMTs <- png.list.ABMTs[c(2, 4, 3, 5, 1)]


png.list.ABMTs <- paste0("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/", png.list.ABMTs)


#EDBMTs
png.list.EDBMTs <- list.files("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved",
                       patt = "*EDBMTs*")

png.list.EDBMTs <- png.list.EDBMTs[c(2, 4, 3, 5, 1)]

png.list.EDBMTs <- paste0("./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/", png.list.EDBMTs)



#=====================================================================================================
# END Generate lists of plots  
#=====================================================================================================



#=====================================================================================================
# Generate composite plot
#=====================================================================================================



png.list <- c(png.list.AUTO, png.list.ABMTs, png.list.EDBMTs)


library("png")
library("grid")
library("gridExtra")



plots <- lapply(png.list, function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}
)



ggsave(
   "./Figures/Cumulative_flux_calculation/Expected_error/Aggregation_by_treatment_improved/Big_cumflux_uncertainty_plot.png",
    width=16, height=24,
    marrangeGrob(grobs = plots, nrow=5, ncol=3,
                top = NULL,
                bottom = textGrob("Interval between measurements [hours]", gp = gpar(fontsize = 25, font = 8)),
                left = textGrob(
                    expression(paste("Total cumulative emissions [g ", N[2], "O-N ", ha^{-1}, "]")),
                    gp = gpar(fontsize = 25, font = 8), rot = 90)) 
   )


#=====================================================================================================
# END Generate composite plot
#=====================================================================================================





#=====================================================================================================
#=====================================================================================================
# END MAKE COMPOSITE PLOTS
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# MAKE BIAS & UNCERTAINTY TABLE
#=====================================================================================================
#=====================================================================================================

#=====================================================================================================
# Extract cumfluxes, absoulute biases, relative biases and CVs
#=====================================================================================================


#import data

dat <- list.files(
 "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Last_cumflux_subsamples")



dat <-
    paste0("./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Last_cumflux_subsamples/",
           dat)

dat <- as.list(dat)

cumflux.dat <- do.call(rbind, lapply(dat, read.csv))

plot(with(cumflux.dat, (Bias.perc / 100 * ref.cumflux)) ~ cumflux.dat$Bias.abs)
graphics.off()

all(round(with(cumflux.dat, (Bias.perc / 100 * ref.cumflux)), 8) == round(cumflux.dat$Bias.abs, 8))
# OK, the absolute biases calculated above from Bias.perc and ref.cumflux correspond to those calculated
# in the function fun.apply.ABMTs.EDBMTs.

cumflux.dat.spli <- split(cumflux.dat, with(cumflux.dat, list(Treat, Samp.freq, Meas.times)))


tab.dat.all <- do.call(rbind,
lapply(cumflux.dat.spli, function(x){
    data.frame(
        Treat = unique(x$Treat),
        Samp.freq = unique(x$Samp.freq),
        Cum.flux = round(mean(x$ref.cumflux), 0),
        Bias.abs = round(mean(x$Bias.abs), 2),
        Bias.perc = round(mean(x$Bias.perc), 2),
        CV.perc = round(unique(x$CV.perc), 2),
        Meas.times = unique(x$Meas.times)
        )
    }
    )
)

row.names(tab.dat.all) <- 1:nrow(tab.dat.all)


tab.dat.all$Treat <- factor(tab.dat.all$Treat, levels = c(
                                                     "Control", "Normal_N",  "High_N",
                                                     "Pig_slurry", "Biogas_digestate" 
                                                 ))


tab.dat.long.highfreq <- tab.dat.all[with(tab.dat.all,{
                            Samp.freq > 1 & Samp.freq < 24 & Meas.times == "ABMTs"
                            }
                            )
                       , ]


tab.dat.long.lowfreq <- tab.dat.all[with(tab.dat.all,{
                            Samp.freq > 12
                            }
                            )
                       , ]



tab.dat.long.highfreq.bias <- tab.dat.long.highfreq[ , c("Treat", "Samp.freq", "Cum.flux",
                                                         "Bias.abs", "Bias.perc")]
#tab.dat.long.highfreq.bias$Bias.perc  <- round(tab.dat.long.highfreq.bias$Bias.perc ,2)

tab.dat.long.highfreq.cv <- tab.dat.long.highfreq[ , c("Treat", "Samp.freq",  "CV.perc")]
#tab.dat.long.highfreq.cv$CV.perc  <- round(tab.dat.long.highfreq.cv$CV.perc ,2)

tab.dat.long.lowfreq.bias <- tab.dat.long.lowfreq[ , c("Treat", "Samp.freq", "Meas.times", "Cum.flux",
                                                         "Bias.abs", "Bias.perc")]
#tab.dat.long.lowfreq.bias$Bias.perc  <- round(tab.dat.long.lowfreq.bias$Bias.perc, 2)

tab.dat.long.lowfreq.cv <- tab.dat.long.lowfreq[ , c("Treat", "Samp.freq", "Meas.times", "CV.perc")]
#tab.dat.long.lowfreq.cv$CV.perc  <- round(tab.dat.long.lowfreq.cv$CV.perc, 2)


#=====================================================================================================
# END Extract cumfluxes, absoulute biases, relative biases and CVs
#=====================================================================================================

#=====================================================================================================
#Long to wide format
#=====================================================================================================

library("reshape2")



tab.dat.wide.highfreq.bias  <- dcast(
    tab.dat.long.highfreq.bias , Treat  ~ Samp.freq, value.var= c("Bias.perc")
    )

tab.dat.wide.lowfreq.bias  <- dcast(
    tab.dat.long.lowfreq.bias , Treat  ~ Samp.freq + Meas.times, value.var= "Bias.perc"
    )


tab.dat.wide.highfreq.cv  <- dcast(
    tab.dat.long.highfreq.cv , Treat  ~ Samp.freq, value.var= "CV.perc"
    )


tab.dat.wide.lowfreq.cv  <- dcast(
    tab.dat.long.lowfreq.cv , Treat  ~ Samp.freq + Meas.times, value.var= "CV.perc"
)


#=====================================================================================================
# END Long to wide format
#=====================================================================================================

#=====================================================================================================
# Relative bias table
#=====================================================================================================

tab.bias <- merge(tab.dat.wide.highfreq.bias, tab.dat.wide.lowfreq.bias, by = "Treat")
tab.bias <- tab.bias[order(tab.bias$Treat), ]
tab.bias$Treat <- as.character(tab.bias$Treat)
tab.bias <- tab.bias[, c("Treat", "2", "3", "4", "6", "8", "12", "24_ABMTs", "48_ABMTs", "72_ABMTs", "96_ABMTs", "120_ABMTs", "144_ABMTs", "168_ABMTs", "24_EDBMTs", "48_EDBMTs", "72_EDBMTs", "96_EDBMTs", "120_EDBMTs", "144_EDBMTs", "168_EDBMTs")]

for(i in 1:nrow(tab.bias)){

    if(tab.bias$Treat[i] == "Normal_N") {tab.bias$Treat[i] = "Normal N"}
    if(tab.bias$Treat[i] == "High_N") {tab.bias$Treat[i] = "High N"}
    if(tab.bias$Treat[i] == "Pig_slurry") {tab.bias$Treat[i] = "Pig slurry"}
    if(tab.bias$Treat[i] == "Biogas_digestate") {tab.bias$Treat[i] = "Biogas digestate"}


    }


for(i in 1:length(colnames(tab.bias))){

    if(substr(colnames(tab.bias)[i], 3, 3) == "_") {substr(colnames(tab.bias)[i], 3, 3) = " "}
    if(substr(colnames(tab.bias)[i], 4, 4) == "_") {substr(colnames(tab.bias)[i], 4, 4) = " "}
    
    }

tab.bias <- t(tab.bias)
tab.bias <- tab.bias[-1, ]
tab.bias <- cbind(row.names(tab.bias), tab.bias)
colnames(tab.bias)[1] <- "Measuring mode"

tab.bias[c(),
         ]

write.csv(
    tab.bias,
    file = "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Bias_uncertainty/Bias.csv",
    row.names = FALSE)

#=====================================================================================================
# END Relative bias table
#=====================================================================================================



#=====================================================================================================
# CV table
#=====================================================================================================

tab.cv <- merge(tab.dat.wide.highfreq.cv, tab.dat.wide.lowfreq.cv, by = "Treat") 
tab.cv <- tab.cv[order(tab.cv$Treat), ]
tab.cv$Treat <- as.character(tab.cv$Treat)
tab.cv <- tab.cv[, c("Treat", "2", "3", "4", "6", "8", "12", "24_ABMTs", "48_ABMTs", "72_ABMTs", "96_ABMTs", "120_ABMTs", "144_ABMTs", "168_ABMTs", "24_EDBMTs", "48_EDBMTs", "72_EDBMTs", "96_EDBMTs", "120_EDBMTs", "144_EDBMTs", "168_EDBMTs")]

for(i in 1:nrow(tab.cv)){

    if(tab.cv$Treat[i] == "Normal_N") {tab.cv$Treat[i] = "Normal N"}
    if(tab.cv$Treat[i] == "High_N") {tab.cv$Treat[i] = "High N"}
    if(tab.cv$Treat[i] == "Pig_slurry") {tab.cv$Treat[i] = "Pig slurry"}
    if(tab.cv$Treat[i] == "Biogas_digestate") {tab.cv$Treat[i] = "Biogas digestate"}


    }


for(i in 1:length(colnames(tab.cv))){

    if(substr(colnames(tab.cv)[i], 3, 3) == "_") {substr(colnames(tab.cv)[i], 3, 3) = " "}
    if(substr(colnames(tab.cv)[i], 4, 4) == "_") {substr(colnames(tab.cv)[i], 4, 4) = " "}
    
    }

tab.cv <- t(tab.cv)
tab.cv <- tab.cv[-1, ]
tab.cv <- cbind(row.names(tab.cv), tab.cv)
colnames(tab.cv)[1] <- "Measuring mode"




write.csv(
    tab.cv,
    file = "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Bias_uncertainty/Uncertainty.csv",
    row.names = FALSE)


#=====================================================================================================
# END CV table
#=====================================================================================================


#=====================================================================================================
# BIG bias table (mean absolute bias, mean relative bias)
#=====================================================================================================

bias.big.dat <- tab.dat.all[order(tab.dat.all$Treat), ]

#eliminate hourly measurements and keep only needed columns

bias.big.dat <- bias.big.dat[bias.big.dat$Samp.freq != 1,
                             c("Treat", "Samp.freq", "Bias.abs", "Bias.perc", "Meas.times")]

# Subdaily measurements duplicated. Eliminate one copy

bias.big.dat <- bias.big.dat[!(bias.big.dat$Samp.freq <= 12 & bias.big.dat$Meas.time == "EDBMTs"), ]


#Generate column measuring mode
bias.big.dat$Meas.mode <- 
ifelse(bias.big.dat$Samp.freq <= 12, bias.big.dat$Samp.freq,
ifelse(bias.big.dat$Samp.freq > 12, with(bias.big.dat, paste(Samp.freq, Meas.times)), NA
       ))

# Final columns in final sequence
bias.big.dat <- bias.big.dat[ , c("Meas.mode", "Treat", "Bias.abs", "Bias.perc")]

#Long to wide

#Load library to use setDT function.
library(data.table)

# Setting data frame to data table (function setDT) required for dcast with two value.var
bias.big.tab <- dcast(setDT(bias.big.dat), Meas.mode ~ Treat, value.var=c("Bias.abs","Bias.perc"))


# Set table structure

bias.big.tab <-
bias.big.tab[
    c("2","3","4","6","8","12",
      "24 ABMTs", "48 ABMTs", "72 ABMTs", "96 ABMTs", "120 ABMTs", "144 ABMTs", "168 ABMTs",
      "24 EDBMTs", "48 EDBMTs", "72 EDBMTs", "96 EDBMTs", "120 EDBMTs", "144 EDBMTs", "168 EDBMTs"
      ),
    c("Meas.mode",
      "Bias.abs_Control", "Bias.perc_Control", "Bias.abs_Normal_N", "Bias.perc_Normal_N",
      "Bias.abs_High_N", "Bias.perc_High_N", "Bias.abs_Pig_slurry", "Bias.perc_Pig_slurry",
      "Bias.abs_Biogas_digestate", "Bias.perc_Biogas_digestate")
    ]

write.csv(bias.big.tab, "./Data/Processed_data/Expected_error_cumfluxes/Aggregation_by_treat/Bias_uncertainty/Bias_abs_rel.csv", row.names = FALSE)

#=====================================================================================================
# END BIG bias table  mean absolute bias, mean relative bias)
#=================================================================================================

#=====================================================================================================
#=====================================================================================================
# END MAKE BIAS & UNCERTAINTY TABLE
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FULL AUTO EXPECTED ERROR IN CUMFLUX FOR DIFFERENT SAMPLING FREQUENCIES BY TREAT
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================




#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END EXPECTED ERROR IN CUMULATIVE FLUX ESTIMATES AS FUNCTION OF MEASURING FREQUENCY
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


