#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# GAP FILLING
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

## This script uses a mean imputation procedure to fill the gaps in the flux time series. The flux
## time series from each chamber is gap-filled independently from each other


setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")


#=====================================================================================================
# Import flux data
#=====================================================================================================


# Fluxes above and below 0C

fluxes <- read.csv("./Data/Processed_data/Fluxes_soil_daily_var.csv")

fluxes  <- within(fluxes, {

    Assigned.DateTime = as.POSIXct(fluxes$Assigned.DateTime)
    Day               = as.Date(fluxes$Day)
    ChambN            = as.factor(ChambN)  
    Treat             = as.factor(Treat)
    Rep               = as.factor(Rep)
    Season            = as.factor(Season)
    ICOS.Timestamp    = as.POSIXct(ICOS.Timestamp)

}

)





fluxes <- within(fluxes, {
    Rep = factor(Rep, levels = c("1","2"))
    Treat = factor(Treat, levels = c("Control", "Normal_N", "High_N", "Pig_slurry", "Biogas_digestate"))
    Season = factor(Season, levels = c("Spring", "Summer", "Autumn", "Winter"))
}

)

fluxes <- fluxes [with(fluxes , order(Treat, Rep, Assigned.DateTime)),]


rownames(fluxes) <- 1:nrow(fluxes)



# import data for construncting lookup tables for fluxes below 0C
fluxes.be0.ttrends  <-  read.csv("./Data/Processed_data/Fluxes_below_0C_t_trend.csv")
fluxes.be0.ttrends$Assigned.DateTime  <- as.POSIXct(fluxes.be0.ttrends$Assigned.DateTime)

#Temperature trends for 1 hour prior to flux measurement, Assigned.DateTime and ChambN only
fluxes.be0.ttrends <- fluxes.be0.ttrends[ , c(1:7, 10)]


# Merge dataframe with temperature trends 1 hour before all bluxes below 0C fluxes into df with
# all fluxes
fluxes <- 
    merge(x = fluxes, y = fluxes.be0.ttrends,
          by = c("Assigned.DateTime", "ChambN", "Treat",  "Rep",  "Season", "Plant.status",  "g.N2O.N.ha.day"),
          all = TRUE)


#=====================================================================================================
# END Import flux data
#=====================================================================================================


#=====================================================================================================
# Import soil temperature and water content data (necessary only once, not once for each chamber)
#=====================================================================================================

summ.soil.t <- read.csv("./Data/Cleaned_data/Soil_T_water/summ_soil_t_5cm.csv")


#Import new swc data
summ.soil.w <- read.csv("./Data/Cleaned_data/Soil_T_water/Moist_5cm_Magdalena_20200220.csv")
summ.soil.w$Timestamp <- as.POSIXct(summ.soil.w$Timestamp)

#calculate medians
summ.soil.w$median_5cm <-
    apply(summ.soil.w[ , c("SM1_5cm", "SM2_5cm", "SM3_5cm", "SM4_5cm")], MARGIN = 1, median)



#adapt data structure 

summ.soil.w <- summ.soil.w[
   ,
    -(which(names(summ.soil.w) %in% c("X", paste0("SM", 1:4, "_5cm"))))
]

colnames(summ.soil.w)[colnames(summ.soil.w) == "Timestamp"]  <- "ICOS.Timestamp"
colnames(summ.soil.w)[colnames(summ.soil.w) == "mean_5cm"]  <- "Mean.soil.WFPS"
colnames(summ.soil.w)[colnames(summ.soil.w) == "median_5cm"]  <- "Median.soil.WFPS"


summ.soil.w$ICOS.Timestamp  <- as.factor(summ.soil.w$ICOS.Timestamp)


#=====================================================================================================


soil.tm.5cm <- merge(summ.soil.t, summ.soil.w, by = "ICOS.Timestamp", all = TRUE)
soil.tm.5cm <- soil.tm.5cm[ ,colnames(soil.tm.5cm) %in% c("ICOS.Timestamp", "Mean.soil.t.C", "Mean.soil.WFPS" )]

soil.tm.5cm$ICOS.Timestamp <- as.POSIXct(soil.tm.5cm$ICOS.Timestamp)
soil.tm.5cm$Difftime.min <- as.numeric(NA)


#=====================================================================================================
# END Import soil temperature and water content data (necessary only once, not once for each chamber)
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# GAP FILLING FULL AUTO (ALL chambers)
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


fluxes.spli <- split(fluxes, fluxes$ChambN)

#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# DEFINE FULL AUTO GAP FILLING FUNCTION
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


gap.fill.full.auto <- function(x) {


    
    fluxes.1 <- x


    #=====================================================================================================
    #=====================================================================================================
    # GENERATE LOOKUP TABLES FOR GAPS ABOVE 0C WITH FLUXES MEASURED ABOVE 0C
    #=====================================================================================================
    #=====================================================================================================

    #=====================================================================================================
    # Insert group labels (soil temp decile, soil water decile, days since ferilization) in flux df
    #=====================================================================================================

    #Select fluxes above zero C

    fluxes.ab0 <- fluxes.1[fluxes.1$Mean.soil.t.C > 0, ]




    #insert soil t decile vector

    library("StatMeasures")



    #insert soil t decile vector

    fluxes.ab0$Soil.T.decile <- decile(fluxes.ab0$Mean.soil.t.C)



    #insert soil w decile vector

    fluxes.ab0$Soil.WFPS.decile <- decile(fluxes.ab0$Mean.soil.WFPS)




    #insert vector with days from N input================================================================


    #Assign exact datetime for N input (treatment specific)

    date.1 <- "2015-05-18"
    if(unique(fluxes.ab0$ChambN) == 1) time.1  <-  "10:20:00"
    if(unique(fluxes.ab0$ChambN) %in% c(2:5, 8, 9)) time.1  <-  "10:00:00"
    if(unique(fluxes.ab0$ChambN) == 6) time.1  <-  "11:45:00"
    if(unique(fluxes.ab0$ChambN) == 7) time.1  <-  "15:30:00"
    if(unique(fluxes.ab0$ChambN) == 10) time.1  <-  "13:40:00"
    fert.day.1 <- as.POSIXct(paste(date.1, time.1))


    fert.day.2 <- as.POSIXct("2016-04-22 08:00:00")


    date.3 <- "2016-05-26"
    if(unique(fluxes.ab0$ChambN) == 1) time.3  <-  "08:30:00"
    if(unique(fluxes.ab0$ChambN) %in% c(2:5, 8, 9)) time.3  <-  "08:00:00"
    if(unique(fluxes.ab0$ChambN) == 6) time.3 <-  "10:50:00"
    if(unique(fluxes.ab0$ChambN) == 7) time.3  <-  "08:30:00"
    if(unique(fluxes.ab0$ChambN) == 10) time.3  <-  "10:50:00"
    fert.day.3 <- as.POSIXct(paste(date.3, time.3))

    # Checked fertilization date times for chambers 1, 2, 3, 4, 5, 6, 7, 8, 9, 10

    # END Assign exact datetime for N input (treatment specific)


    # 0-3 days = 1
    cat.1 <- c(
        seq(fert.day.1, fert.day.1 + 60*60*72, by = "min"),
        seq(fert.day.2, fert.day.2 + 60*60*72, by = "min"),
        seq(fert.day.3, fert.day.3 + 60*60*72, by = "min")
    )

    # 3-6 days = 2
    cat.2 <- c(
        seq(fert.day.1 + 60*60*72 + 60, fert.day.1 + 60*60*144, by = "min"),
        seq(fert.day.2 + 60*60*72 + 60, fert.day.2 + 60*60*144, by = "min"),
        seq(fert.day.3 + 60*60*72 + 60, fert.day.3 + 60*60*144, by = "min")

    )


    # 6-9 days = 3
    cat.3 <- c(
        seq(fert.day.1 + 60*60*144 + 60, fert.day.1 + 60*60*216, by = "min"),
        seq(fert.day.2 + 60*60*144 + 60, fert.day.2 + 60*60*216, by = "min"),
        seq(fert.day.3 + 60*60*144 + 60, fert.day.3 + 60*60*216, by = "min")
    )


    # 9-12 days = 4
    cat.4 <- c(
        seq(fert.day.1 + 60*60*216 + 60, fert.day.1 + 60*60*288, by = "min"),
        seq(fert.day.2 + 60*60*216 + 60, fert.day.2 + 60*60*288, by = "min"),
        seq(fert.day.3 + 60*60*216 + 60, fert.day.3 + 60*60*288, by = "min")
    )


    # 12-15 days = 5
    cat.5 <- c(
        seq(fert.day.1 + 60*60*288 + 60, fert.day.1 + 60*60*360, by = "min"),
        seq(fert.day.2 + 60*60*288 + 60, fert.day.2 + 60*60*360, by = "min"),
        seq(fert.day.3 + 60*60*288 + 60, fert.day.3 + 60*60*360, by = "min")
    )

    # > 15 days = 6

    all.mins <- seq(min(fluxes.ab0$Assigned.DateTime), max(fluxes.ab0$Assigned.DateTime), by = "min")
    mins.cat.1.5 <- c(cat.1, cat.2, cat.3, cat.4, cat.5)

    cat.6 <- all.mins[!(substr(all.mins, 1, 17) %in% substr(mins.cat.1.5, 1, 17))]

    #check cat 1:5


    start.stop.cats <- 
        lapply(list(
            #cat1
            seq(fert.day.1, fert.day.1 + 60*60*72, by = "min"),
            seq(fert.day.2, fert.day.2 + 60*60*72, by = "min"),
            seq(fert.day.3, fert.day.3 + 60*60*72, by = "min"),
            #cat2
            seq(fert.day.1 + 60*60*72 + 60, fert.day.1 + 60*60*144, by = "min"),
            seq(fert.day.2 + 60*60*72 + 60, fert.day.2 + 60*60*144, by = "min"),
            seq(fert.day.3 + 60*60*72 + 60, fert.day.3 + 60*60*144, by = "min"),
            #cat3
            seq(fert.day.1 + 60*60*144 + 60, fert.day.1 + 60*60*216, by = "min"),
            seq(fert.day.2 + 60*60*144 + 60, fert.day.2 + 60*60*216, by = "min"),
            seq(fert.day.3 + 60*60*144 + 60, fert.day.3 + 60*60*216, by = "min"),
            #cat4
            seq(fert.day.1 + 60*60*216 + 60, fert.day.1 + 60*60*288, by = "min"),
            seq(fert.day.2 + 60*60*216 + 60, fert.day.2 + 60*60*288, by = "min"),
            seq(fert.day.3 + 60*60*216 + 60, fert.day.3 + 60*60*288, by = "min"),
            #cat5
            seq(fert.day.1 + 60*60*288 + 60, fert.day.1 + 60*60*360, by = "min"),
            seq(fert.day.2 + 60*60*288 + 60, fert.day.2 + 60*60*360, by = "min"),
            seq(fert.day.3 + 60*60*288 + 60, fert.day.3 + 60*60*360, by = "min")
            
        ), range)

    start.stop.cats <- lapply(start.stop.cats, as.data.frame)
    start.stop.cats <- lapply(start.stop.cats, t)
    start.stop.cats <- as.data.frame(do.call(rbind, start.stop.cats))


    #Insert appropriate labels into flux dataframe

    fluxes.ab0$Days.N.input.cat <- as.integer(NA)

    print(unique(fluxes.ab0$ChambN))

    pb <- txtProgressBar(min = 1, max = nrow(fluxes.ab0), style = 3)

    for (i in 1:nrow(fluxes.ab0)) {
        
        #all controls to cat 6 (more than 15 days)
        if (unique(fluxes.ab0$Treat) == "Control") {
            
            fluxes.ab0$Days.N.input.cat = 6
        }
        
        else {

            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.1, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(1)}
            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.2, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(2)}
            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.3, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(3)}
            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.4, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(4)}
            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.5, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(5)}
            if(substr(fluxes.ab0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.6, 1, 17)) {fluxes.ab0$Days.N.input.cat[i] = as.integer(6)}
            
        }
        
        setTxtProgressBar(pb, i)

    }

    #END insert vector with days from N input=============================================================


    fluxes.ab0$Lookup.label <- with(fluxes.ab0, paste0("Ab0_", sprintf("%02d", ChambN), "_", sprintf("%02d", Soil.T.decile), "_", sprintf("%02d", Soil.WFPS.decile), "_", Days.N.input.cat))


    #END Check days from N input labels===================================================================

    #=====================================================================================================
    # END Insert group labels (soil temp decile, soil water decile, days since ferilization) in flux df
    #=====================================================================================================



    #=====================================================================================================
    # Generate lookup table above 0C
    #=====================================================================================================

    # GENERAL STRATEGY: the df with all fluxes will be splitted into a list of dfs. Each df in the list
    # will contain fluxes all labelled with one of the 600 possible Soil.T.decile, Soil.WFPS.decile,
    # Days.N.input.cat permutations.



    fluxes.ab0.list <- split(fluxes.ab0, fluxes.ab0$Lookup.label)


    # Define lookup table generating function

    lookup.gen <- function(x){

        data.frame(
            ChambN = x$ChambN[1],
            Treat = x$Treat[1],
            Rep = x$Rep[1],
            Soil.T.decile = x$Soil.T.decile[1],
            Soil.WFPS.decile = x$Soil.WFPS.decile[1],
            Days.N.input.cat = x$Days.N.input.cat[1],
            Lookup.label = x$Lookup.label[1],
            N.fluxes.Lookup.table.ID = nrow(x),
            Mean.g.N2O.N.ha.day = mean(x$g.N2O.N.ha.day)
        )
    }




    # generate lookup table
    lookup.table.ab0 <- do.call(rbind,lapply(fluxes.ab0.list, lookup.gen))
    lookup.table.ab0$Lookup.label  <- as.character(lookup.table.ab0$Lookup.label)

    lookup.table.ab0 <- lookup.table.ab0[ , c("ChambN","Treat", "Rep", "Soil.T.decile", "Soil.WFPS.decile",
                                              "Days.N.input.cat", "Lookup.label", "N.fluxes.Lookup.table.ID",
                                              "Mean.g.N2O.N.ha.day")]

    #write lookup table to disk
    write.csv(lookup.table.ab0, file = paste0(
                                    "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Above_0C/Ab0_ChambN_",
                                    fluxes.ab0$ChambN[1],"_", fluxes.ab0$Treat[1], "_Rep_", fluxes.ab0$Rep[1], ".csv"),
              row.names = FALSE)

    

    #=====================================================================================================
    # END Generate lookup table above 0C
    #=====================================================================================================




    #=====================================================================================================
    #=====================================================================================================
    # END GENERATE LOOKUP TABLES FOR GAPS ABOVE 0C WITH FLUXES MEASURED ABOVE 0C
    #=====================================================================================================
    #=====================================================================================================




    #=====================================================================================================
    #=====================================================================================================
    # GENERATE LOOKUP TABLES FOR GAPS BELOW 0C WITH FLUXES MEASURED BELOW 0C
    #=====================================================================================================
    #=====================================================================================================


    ## These lookup table will be based on 3 labels:
    ##
    ## 1) Time elapsed from N input (all fluxes should belong to category 6, but this has to be verified)
    ## 2) Soil temperature decile
    ## 3) Soil temperature trend decile in the last hour


    # Extract fluxes below 0C

    fluxes.be0 <- fluxes.1[fluxes.1$Mean.soil.t.C <= 0, ]

    #=====================================================================================================
    # Insert group labels (soil temp decile, soil temperature trend decile,
    # days since ferilization) in flux df
    #=====================================================================================================

    #insert soil t decile vector

    fluxes.be0$Soil.T.decile <- decile(fluxes.be0$Mean.soil.t.C)


    #insert soil temperature rate of change decile vector

    fluxes.be0$C.min_1.decile <- decile(fluxes.be0$C.min_1)


    #Insert appropriate time elapsed from last N input labels into flux dataframe
    
    fluxes.be0$Days.N.input.cat <- as.integer(NA)

    print(unique(fluxes.be0$ChambN))

    pb <- txtProgressBar(min = 1, max = nrow(fluxes.be0), style = 3)

    for (i in 1:nrow(fluxes.be0) ) {
        
        #all controls to cat 6 (more than 15 days)
        if (unique(fluxes.be0$Treat) == "Control") {
            
            fluxes.be0$Days.N.input.cat = 6
        }
        
        else {

            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.1, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(1)}
            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.2, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(2)}
            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.3, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(3)}
            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.4, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(4)}
            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.5, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(5)}
            if(substr(fluxes.be0$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.6, 1, 17)) {fluxes.be0$Days.N.input.cat[i] = as.integer(6)}
            
            

        }
        
        setTxtProgressBar(pb, i)

    }


    #END insert vector with days from N input=============================================================


    # Insert label vector into df for fluxes below zero 

    fluxes.be0$Lookup.label <- with(fluxes.be0, paste0("Be0_", sprintf("%02d", ChambN), "_", sprintf("%02d", Soil.T.decile), "_", sprintf("%02d", C.min_1.decile), "_", Days.N.input.cat))


    #=====================================================================================================
    # END Insert group labels (soil temp decile, soil temperature trend decile,
    # days since ferilization) in flux df
    #=====================================================================================================





    #=====================================================================================================
    # Generate lookup table below 0C
    #=====================================================================================================

    # GENERAL STRATEGY: the df with all fluxes will be splitted into a list of dfs. Each df in the list
    # will contain fluxes all labelled with one of the 600 possible Soil.T.decile, Soil.WFPS.decile,
    # Days.N.input.cat permutations.



    fluxes.be0.list <- split(fluxes.be0, fluxes.be0$Lookup.label)


    # Define lookup table generating function

    lookup.gen <- function(x){

        data.frame(
            ChambN = x$ChambN[1],
            Treat = x$Treat[1],
            Rep = x$Rep[1],
            Soil.T.decile = x$Soil.T.decile[1],
            C.min_1.decile = x$C.min_1.decile[1],
            Days.N.input.cat = x$Days.N.input.cat[1],
            Lookup.label = x$Lookup.label[1],
            N.fluxes.Lookup.table.ID = nrow(x),
            Mean.g.N2O.N.ha.day = mean(x$g.N2O.N.ha.day)
        )
    }




    # generate lookup table
    lookup.table.be0 <- do.call(rbind,lapply(fluxes.be0.list, lookup.gen))
    lookup.table.be0$Lookup.label  <- as.character(lookup.table.be0$Lookup.label)

    lookup.table.be0 <- lookup.table.be0[ , c("ChambN","Treat", "Rep", "Soil.T.decile", "C.min_1.decile",
                                              "Days.N.input.cat", "Lookup.label", "N.fluxes.Lookup.table.ID",
                                              "Mean.g.N2O.N.ha.day")]

    #write lookup table to disk
    write.csv(lookup.table.be0, file = paste0(
                                    "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Below_0C/Be0_ChambN_",
                                    fluxes.be0$ChambN[1],"_", fluxes.be0$Treat[1], "_Rep_", fluxes.be0$Rep[1], ".csv"),
              row.names = FALSE)



    #=====================================================================================================
    # END Generate lookup table below 0C
    #=====================================================================================================



    #=====================================================================================================
    #=====================================================================================================
    # END GENERATE LOOKUP TABLES FOR GAPS BELOW 0C WITH FLUXES MEASURED BELOW 0C
    #=====================================================================================================
    #=====================================================================================================




    #=====================================================================================================
    #=====================================================================================================
    # GENERATE GAP DF READY FOR FLUX IMPUTATION
    #=====================================================================================================
    #=====================================================================================================



    #=====================================================================================================
    # Generate template with hours without flux measurements
    #=====================================================================================================

    #Add new required columns to flux df
    fluxes.ab0$Source  <- as.character("Measure")
    fluxes.ab0$Lookup.table.ID <- as.character(NA)
    fluxes.ab0$N.fluxes.Lookup.table.ID <- as.integer(NA)

    fluxes.be0$Source  <- as.character("Measure")
    fluxes.be0$Lookup.table.ID <- as.character(NA)
    fluxes.be0$N.fluxes.Lookup.table.ID <- as.integer(NA)



    fluxes.ab0$C.min_1.decile  <- as.integer(NA)
    fluxes.be0$Soil.WFPS.decile  <- as.integer(NA)

    fluxes.ab0 <- fluxes.ab0[ , c("Assigned.DateTime", "Day", "ChambN", "Treat", "Rep", "Season", "Plant.status", "N.conc", "g.N2O.N.ha.day", "Mean.soil.t.C", "Mean.soil.WFPS", "C.min_1", "Soil.T.decile", "Soil.WFPS.decile", "C.min_1.decile", "Days.N.input.cat", "Lookup.label", "Source", "Lookup.table.ID", "N.fluxes.Lookup.table.ID")]

    fluxes.be0 <- fluxes.be0[ , c("Assigned.DateTime", "Day", "ChambN", "Treat", "Rep", "Season", "Plant.status", "N.conc", "g.N2O.N.ha.day", "Mean.soil.t.C", "Mean.soil.WFPS", "C.min_1", "Soil.T.decile", "Soil.WFPS.decile", "C.min_1.decile", "Days.N.input.cat", "Lookup.label", "Source", "Lookup.table.ID", "N.fluxes.Lookup.table.ID")]

    colnames(fluxes.ab0) == colnames(fluxes.be0)

    # Bind dataframes with labelled fluxes (above and below 0C)

    flux.lab <- rbind(fluxes.ab0, fluxes.be0)

    flux.lab <- flux.lab[order(flux.lab$Assigned.DateTime, decreasing = FALSE),]



    #Generate df template for gap filled fluxes===========================================================

    #Generate vector with all hours from first to last measurement

    flux.lab <- flux.lab[order(flux.lab$Assigned.DateTime, decreasing = FALSE), ]

    all.hours.monit.period <-
        seq(flux.lab$Assigned.DateTime[1], flux.lab$Assigned.DateTime[length(flux.lab$Assigned.DateTime)],
            by = "1 hour")

    tmp <- as.numeric(NA)
    
    for(i in 1:(length(all.hours.monit.period)-1)){
        tmp[i] = as.numeric(difftime(all.hours.monit.period[i+1], all.hours.monit.period[i], unit = "hours"))
    }

    all(tmp == 1)


    #Generate vector with hours missing in time series from chamber x

    missing.hours.monit.period <- all.hours.monit.period[
        !(substr(all.hours.monit.period, 1, 13) %in% substr(flux.lab$Assigned.DateTime, 1, 13))
    ]


    #use measured fluxes df as template for gap filled fluxes

    gaps  <- data.frame(

        Assigned.DateTime = as.POSIXct(missing.hours.monit.period),
        Day = as.Date(substr(missing.hours.monit.period, 1, 10)),
        ChambN = factor(flux.lab$ChambN[1], levels = levels(flux.lab$ChambN)),
        Treat = factor(flux.lab$Treat[1], levels = levels(flux.lab$Treat)),
        Rep = factor(flux.lab$Rep[1], levels = levels(flux.lab$Rep)),
        Season = factor(NA, levels = levels(flux.lab$Season)),
        Plant.status = factor(NA, levels = levels(flux.lab$Plant.status)),
        N.conc = as.integer(NA),
        g.N2O.N.ha.day = as.numeric(NA),
        C.min_1 = as.numeric(NA),
        Soil.T.decile = as.integer(NA),
        Soil.WFPS.decile = as.integer(NA),
        C.min_1.decile = as.integer(NA),
        Days.N.input.cat = as.integer(NA),
        Lookup.label = as.character(NA),
        Source = as.character(NA),
        Lookup.table.ID = as.character(NA),
        N.fluxes.Lookup.table.ID = as.integer(NA),
        stringsAsFactors = FALSE

    )


    #=====================================================================================================
    # END Generate template with hours without flux measurements
    #=====================================================================================================


    #=====================================================================================================
    # Insert Season and  Plant status variables
    #=====================================================================================================


    #Season

    pb <- txtProgressBar(min = 1, max = nrow(gaps), style = 3)
    for (i in 1:nrow(gaps)) {

        #insert astronomical seasons here

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "03" & 
           substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:31)) {gaps$Season[i] <- "Spring"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "03" & 
           !(substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:31))) {gaps$Season[i] <-
                                                                                      "Winter"}
        if(substr(gaps$Assigned.DateTime[i], 6, 7) %in% c("04", "05")) {gaps$Season[i] <- "Spring"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "06" & 
           substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:30)) {gaps$Season[i] <- "Summer"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "06" & 
           !(substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:30))) {gaps$Season[i] <-
                                                                                      "Spring"}
        
        if(substr(gaps$Assigned.DateTime[i], 6, 7) %in% c("07", "08")) {gaps$Season[i] <- "Summer"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "09" & 
           substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:30)) {gaps$Season[i] <- "Autumn"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "09" & 
           !(substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:30))) {gaps$Season[i] <-
                                                                                      "Summer"}
        
        if(substr(gaps$Assigned.DateTime[i], 6, 7) %in% c("10", "11")) {gaps$Season[i] <- "Autumn"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "12" & 
           substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:31)) {gaps$Season[i] <- "Winter"}

        if(substr(gaps$Assigned.DateTime[i], 6, 7) == "12" & 
           !(substr(gaps$Assigned.DateTime[i], 9, 10) %in% as.character(21:31))) {gaps$Season[i] <-
                                                                                      "Autumn"}
        
        if(substr(gaps$Assigned.DateTime[i], 6, 7) %in% c("01", "02")) {gaps$Season[i] <- "Winter"}


        #insert plant status here

        if(substr(gaps$Assigned.DateTime[i], 6, 7) %in% c("04", "05", "06", "07", "08"))
        {gaps$Plant.status[i] <- "Growth"}

        else
        {gaps$Plant.status[i] <- "No-growth"}


        #update progress bar

        setTxtProgressBar(pb, i)

        
    }

    #=====================================================================================================
    # END Insert Season and  Plant status variables
    #=====================================================================================================


    #=====================================================================================================
    # Insert soil temperature and water content variables (measures only)
    #=====================================================================================================


    #Mean.soil.t. & Mean.soil.WFP

    #sychronize soil temperatures


    dummy <- cbind(gaps[1,], soil.tm.5cm[1,])
    dummy$Difftime.min <- as.numeric(NA)


    #set progress bar

    pb <- txtProgressBar(min = 1, max = nrow(gaps), style = 3)


    for (i in 1:nrow(gaps)) {



        soil.tm.5cm$Difftime.min <- as.numeric(difftime(soil.tm.5cm$ICOS.Timestamp, gaps$Assigned.DateTime[i], units = "mins"))

        

        dummy[i, ] <- cbind(gaps[i, ], soil.tm.5cm[which.min(abs(soil.tm.5cm$Difftime.min)), ])


        #update progress bar

        setTxtProgressBar(pb, i)

    }




    gaps <- dummy

    #=====================================================================================================
    # END Insert soil temperature and water content variables (measures only)
    #=====================================================================================================




    #=====================================================================================================
    # Insert Days.N.input.cat
    #=====================================================================================================

    
    print(unique(gaps$ChambN))

    pb <- txtProgressBar(min = 1, max = nrow(gaps), style = 3)

    for (i in 1:nrow(gaps)) {
        
        #all controls to cat 6 (more than 15 days)
        if (unique(gaps$Treat) == "Control") {
            
            gaps$Days.N.input.cat = 6
        }
        
        else {

            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.1, 1, 17)) {gaps$Days.N.input.cat[i] = 1}
            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.2, 1, 17)) {gaps$Days.N.input.cat[i] = 2}
            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.3, 1, 17)) {gaps$Days.N.input.cat[i] = 3}
            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.4, 1, 17)) {gaps$Days.N.input.cat[i] = 4}
            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.5, 1, 17)) {gaps$Days.N.input.cat[i] = 5}
            if(substr(gaps$Assigned.DateTime[i], 1, 17) %in%
               substr(cat.6, 1, 17)) {gaps$Days.N.input.cat[i] = 6}
            

        }
        
        setTxtProgressBar(pb, i)

    }


    #=====================================================================================================
    # END Insert Days.N.input.cat
    #=====================================================================================================



    #=====================================================================================================
    #  Insert soil temperature and water content  deciles (separately above and below 0C!!!)
    #=====================================================================================================

    #ABOVE 0C

    gaps.ab0 <- gaps[gaps$Mean.soil.t.C > 0, ]


    #Soil.T.decile

    gaps.ab0$Soil.T.decile <- decile(gaps.ab0$Mean.soil.t.C)


    #Soil.WFPS.decile

    gaps.ab0$Soil.WFPS.decile <- decile(gaps.ab0$Mean.soil.WFPS)


    #Source label

    gaps.ab0$Source <- as.character("Lookup.table.ab0")




    #BELOW 0C

    gaps.be0 <- gaps[gaps$Mean.soil.t.C <= 0, ]

    #Soil.T.decile

    gaps.be0$Soil.T.decile <- decile(gaps.be0$Mean.soil.t.C)


    #Soil.WFPS.decile

    gaps.be0$Soil.WFPS.decile <- decile(gaps.be0$Mean.soil.WFPS)

    #Source label

    gaps.be0$Source <- as.character("Lookup.table.be0")


    #=====================================================================================================
    # END Insert soil temperature and water content  deciles (separately above and below 0C!!!)
    #=====================================================================================================


    #=====================================================================================================
    # Insert soil temperature rates of change and rates of change deciles (gaps below 0C only)
    #=====================================================================================================

    #import (mean) ICOS soil temp 5cm depth

    icos.data <- read.csv("./Data/Cleaned_data/Soil_T_water/summ_soil_t_5cm.csv")
    icos.data$ICOS.Timestamp  <- as.POSIXct(icos.data$ICOS.Timestamp)

    gaps.be0$Temps.in.trend  <- as.integer(NA)
    gaps.be0$First.last.time  <- as.character(NA)


    pb.1 <- txtProgressBar(min = 1, max = nrow(gaps.be0), style = 3)
    
    for(i in 1:nrow(gaps.be0)) {

        index.1 =  
            which(
                as.numeric(
                    difftime(gaps.be0[i,"Assigned.DateTime"], icos.data[ ,"ICOS.Timestamp"], units = "hours")) >= 0
                &


                
                as.numeric(
                    difftime(gaps.be0[i,"Assigned.DateTime"] - 13*60,
                             icos.data[ ,"ICOS.Timestamp"], units = "hours")) <= 1

            )
        

        

        gaps.be0[i, "Temps.in.trend"] = length(index.1)

        gaps.be0[i, "First.last.time"] = paste(as.character(icos.data[index.1[length(index.1)],"ICOS.Timestamp"]),
                                               as.character(icos.data[index.1[1], "ICOS.Timestamp"]), sep = "_")

        trend.length.mins = as.numeric(difftime(icos.data$ICOS.Timestamp[index.1],
                                                icos.data$ICOS.Timestamp[index.1][1], units = "mins"))

        temps.in.trend = icos.data$Mean.soil.t.C[index.1]

        if(length(trend.length.mins) > 1 & length(temps.in.trend) > 1) {

            gaps.be0[i, "C.min_1"] = lm(temps.in.trend ~ trend.length.mins)$coef[2]

        }
        
        else {
            
            gaps.be0[i, "C.min_1"] = NA

        }

        setTxtProgressBar(pb.1, i)  
        
    }

    
    gaps.be0 <- gaps.be0[ , !(colnames(gaps.be0) %in% c("Temps.in.trend", "First.last.time"))]


    #insert temperature rate of change decile

    gaps.be0$C.min_1.decile <- decile(gaps.be0$C.min_1)

    #=====================================================================================================
    # END Insert soil temperature rates of change and rates of change deciles (gaps below 0C only)
    #=====================================================================================================




    #=====================================================================================================
    # Insert gap label column
    #=====================================================================================================

    #Above 0C

    gaps.ab0$Lookup.label <- with(gaps.ab0,
                                  paste0("Ab0_", sprintf("%02d", ChambN), "_", sprintf("%02d", Soil.T.decile), "_",
                                         sprintf("%02d", Soil.WFPS.decile), "_", Days.N.input.cat)
                                  )

    gaps.ab0$N.fluxes.Lookup.table.ID  <- as.integer(NA)
    gaps.ab0$Lookup.table.ID  <- as.character(NA)
    gaps.ab0 <- gaps.ab0[ , colnames(flux.lab)]


    #Below 0C

    gaps.be0$Lookup.label <- with(
        gaps.be0, paste0("Be0_", sprintf("%02d", ChambN), "_", sprintf("%02d", Soil.T.decile), "_",
                         sprintf("%02d",C.min_1.decile), "_", Days.N.input.cat)
    )

    gaps.be0$N.fluxes.Lookup.table.ID  <- as.integer(NA)
    gaps.be0$Lookup.table.ID  <- as.character(NA)
    gaps.be0 <- gaps.be0[ , colnames(flux.lab)]


    #=====================================================================================================
    # END 
    #=====================================================================================================


    #=====================================================================================================
    #=====================================================================================================
    # END GENERATE GAP DF READY FOR FLUX IMPUTATION
    #=====================================================================================================
    #=====================================================================================================





    #=====================================================================================================
    #=====================================================================================================
    # GAP FILLING
    #=====================================================================================================
    #=====================================================================================================


    

    #=====================================================================================================
    # Above 0C
    #=====================================================================================================

    library(dplyr)

    

    for(i in 1:nrow(gaps.ab0)) {


        # Select lookup table entries with same category for time elapsed since N input
        same.Days.N.input.cat =
            lookup.table.ab0[lookup.table.ab0$Days.N.input.cat == gaps.ab0$Days.N.input.cat[i], ]

        # Order by ST and SWC decile (if equidistant deciles, smaller one first)
        same.Days.N.input.cat$Rank.st = abs(same.Days.N.input.cat$Soil.T.decile - gaps.ab0$Soil.T.decile[i])
        same.Days.N.input.cat$Rank.swc = abs(same.Days.N.input.cat$Soil.WFPS.decile - gaps.ab0$Soil.WFPS.decile[i])



        same.Days.N.input.cat = same.Days.N.input.cat[
            with(same.Days.N.input.cat, order(Rank.st, Soil.T.decile, Rank.swc, Soil.WFPS.decile, decreasing = FALSE)),
            ]
        


        tmp <- (same.Days.N.input.cat[, c(4:5, 7:11)])
        


        

        # Empty df to which selecte entries will be added in while loop
        select.entr <- tmp[0, ]



        #Loop selecting entries until N fluxes > 10
        while (nrow(tmp) > 0)
            

    {

        # START LOOP

        # Select entries with lowest Rank.swc
        tmp.min.swc <- tmp[which(tmp$Rank.swc == min(tmp$Rank.swc)), ]

        # Within lowest Rank.swc select lowest Rank.st
        tmp.min.swc.st <- tmp.min.swc[which(tmp.min.swc$Rank.st == min(tmp.min.swc$Rank.st)), ]

        # Add entires above to selected entries
        select.entr <- rbind(select.entr, tmp.min.swc.st)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.swc.st)

        # Now select entries with lowest Rank.st
        tmp.min.st <- tmp[which(tmp$Rank.st == min(tmp$Rank.st)), ]

        # Now within lowest Rank.st select lowest Rank.swc
        tmp.min.st.swc <- tmp.min.st[which(tmp.min.st$Rank.swc == min(tmp.min.st$Rank.swc)), ]

        # Append to selected entries
        select.entr <- rbind(select.entr, tmp.min.st.swc)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.st.swc)

        # END LOOP
        

    }

        
        # Calculate weighted mean flux, N.fluxes(again) and list selected table entry labels

        for(z in 1:nrow(select.entr)) {

            n.flux = sum(select.entr$N.fluxes.Lookup.table.ID[1:z])  
            
            gap.value = with(select.entr, {sum(
                                               Mean.g.N2O.N.ha.day[1:z] * N.fluxes.Lookup.table.ID[1:z] /
                                               sum(N.fluxes.Lookup.table.ID[1:z]))
            }
            )
            
            label = paste(select.entr$Lookup.label[1:z], collapse = ", ")
            

        }

        gaps.ab0$g.N2O.N.ha.day[i] = gap.value
        gaps.ab0$Lookup.table.ID[i] = label
        gaps.ab0$N.fluxes.Lookup.table.ID[i] = n.flux
        
    }



    gaps.ab0[
        gaps.ab0$Lookup.label != gaps.ab0$Lookup.table.ID ,
        c("Lookup.label", "Lookup.table.ID", "N.fluxes.Lookup.table.ID")
    ]


    #=====================================================================================================
    # END Above 0C
    #=====================================================================================================


    #=====================================================================================================
    # Below 0C
    #=====================================================================================================

    
    for(i in 1:nrow(gaps.be0)) {

        # Select lookup table entries with same category for time elapsed since N input
        same.Days.N.input.cat =
            lookup.table.be0[lookup.table.be0$Days.N.input.cat == gaps.be0$Days.N.input.cat[i], ]

        # Order by STRC and ST decile (if equidistant deciles, smaller one first)
        same.Days.N.input.cat$Rank.strc = abs(same.Days.N.input.cat$C.min_1.decile - gaps.be0$C.min_1.decile[i])
        same.Days.N.input.cat$Rank.st = abs(same.Days.N.input.cat$Soil.T.decile - gaps.be0$Soil.T.decile[i])
        
        

        same.Days.N.input.cat = same.Days.N.input.cat[
            with(same.Days.N.input.cat, order(Rank.strc, C.min_1.decile, Rank.st, Soil.T.decile,  decreasing = FALSE)),
            ]
        


        tmp <- (same.Days.N.input.cat[, c(4:5, 7:11)])
        


        
        # Empty df to which selecte entries will be added in while loop
        select.entr <- tmp[0, ]



        #Loop selecting entries until N fluxes > 10
        while (nrow(tmp) > 0)
            

    {

        # START LOOP

        # Select entries with lowest Rank.st
        tmp.min.st <- tmp[which(tmp$Rank.st == min(tmp$Rank.st)), ]

        # Within lowest Rank.st select lowest Rank.strc
        tmp.min.st.strc <- tmp.min.st[which(tmp.min.st$Rank.strc == min(tmp.min.st$Rank.strc)), ]

        # Add entires above to selected entries
        select.entr <- rbind(select.entr, tmp.min.st.strc)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.st.strc)

        # Now select entries with lowest Rank.strc
        tmp.min.strc <- tmp[which(tmp$Rank.strc == min(tmp$Rank.strc)), ]

        # Now within lowest Rank.strc select lowest Rank.st
        tmp.min.strc.st <- tmp.min.strc[which(tmp.min.strc$Rank.st == min(tmp.min.strc$Rank.st)), ]

        # Append to selected entries
        select.entr <- rbind(select.entr, tmp.min.strc.st)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.strc.st)

        # END LOOP
        

    }


        # Calculate weighted mean flux, N.fluxes(again) and list selected table entry labels

        for(z in 1:nrow(select.entr)) {

            n.flux = sum(select.entr$N.fluxes.Lookup.table.ID[1:z])  
            
            gap.value = with(select.entr, {sum(
                                               Mean.g.N2O.N.ha.day[1:z] * N.fluxes.Lookup.table.ID[1:z] /
                                               sum(N.fluxes.Lookup.table.ID[1:z]))
            }
            )
            
            label = paste(select.entr$Lookup.label[1:z], collapse = ", ")
            

        }

        gaps.be0$g.N2O.N.ha.day[i] = gap.value
        gaps.be0$Lookup.table.ID[i] = label
        gaps.be0$N.fluxes.Lookup.table.ID[i] = n.flux
        
    }



    gaps.be0[, c("Lookup.label", "Lookup.table.ID", "N.fluxes.Lookup.table.ID")]


    gaps.be0[
        gaps.be0$Lookup.label != gaps.be0$Lookup.table.ID ,
        c("Lookup.label", "Lookup.table.ID", "N.fluxes.Lookup.table.ID")
    ]


    #=====================================================================================================
    # END Below 0C
    #=====================================================================================================


    
    #=====================================================================================================
    #=====================================================================================================
    # END GAP FILLING
    #=====================================================================================================
    #=====================================================================================================



    #=====================================================================================================
    #=====================================================================================================
    # MERGE MEASURED AND ESTIMATED FLUXES 
    #=====================================================================================================
    #=====================================================================================================




    # Unite mesured and gap filled fluxes=================================================================

    flux.lab.gapfill <- rbind(flux.lab, gaps.ab0, gaps.be0)

    flux.lab.gapfill <- flux.lab.gapfill[order(flux.lab.gapfill$Assigned.DateTime, decreasing = TRUE), ]

    flux.lab.gapfill$Source <- as.factor(flux.lab.gapfill$Source)


    #=====================================================================================================
    #=====================================================================================================
    # END MERGE MEASURED AND ESTIMATED FLUXES 
    #=====================================================================================================
    #=====================================================================================================

    return(flux.lab.gapfill)    
    
}



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END DEFINE FULL AUTO GAP FILLING FUNCTION
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================




#=====================================================================================================
# APPLY FULL AUTO GAP FILLING FUNCTION!
#=====================================================================================================

fluxes.gap.filled <- do.call(rbind, lapply(fluxes.spli, gap.fill.full.auto))

fluxes.gap.filled <- fluxes.gap.filled[with(fluxes.gap.filled, order(ChambN, Assigned.DateTime)), ]




#=====================================================================================================
# END APPLY FULL AUTO GAP FILLING FUNCTION!
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# Gaps organic treatments from 2016-04-22 to 2016-05-26
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
# Generate averaged (weighted by flux N) lookup table from two NM lookup tables
#=====================================================================================================

#Import lookup tables for 2 NM replicates

NM.1 <- read.csv(
    "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Above_0C/Ab0_ChambN_4_Normal_N_Rep_1.csv")
NM.1$TWN.labs  <- substr(NM.1$Lookup.label, 8, 14)

NM.2 <- read.csv(
    "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Above_0C/Ab0_ChambN_9_Normal_N_Rep_2.csv")
NM.2$TWN.labs  <- substr(NM.2$Lookup.label, 8, 14)

nrow(NM.1) == nrow(NM.2)
all(NM.1$TWN.labs == NM.2$TWN.labs)

#merge two tables

NM.12 <- merge(NM.1, NM.2, by = "TWN.labs")

all(NM.12$Soil.T.decile.x == NM.12$Soil.T.decile.y)
all(NM.12$Soil.WFPS.decile.x == NM.12$Soil.WFPS.decile.y)
all(NM.12$Days.N.input.cat.x == NM.12$Days.N.input.cat.y)
NM.12$N.fluxes.Lookup.table.ID.x == NM.12$N.fluxes.Lookup.table.ID.y

#calculate weighted mean fluxes

NM.12$N.fluxes.Lookup.table.ID.z <- with(NM.12, {

    N.fluxes.Lookup.table.ID.x + N.fluxes.Lookup.table.ID.y

}
)

NM.12$Mean.g.N2O.N.ha.day.z <- with(NM.12, {
    (Mean.g.N2O.N.ha.day.x * N.fluxes.Lookup.table.ID.x / NM.12$N.fluxes.Lookup.table.ID.z) +
        (Mean.g.N2O.N.ha.day.y * N.fluxes.Lookup.table.ID.y / NM.12$N.fluxes.Lookup.table.ID.z)
}
)

NM.12$Lookup.label.z  <- paste0("Ab0_mean1.2_", NM.12$TWN.labs)

# All required calculations done. Format new lookup table and write to disk

NM.12 <- NM.12[ ,!(substr(colnames(NM.12), nchar(colnames(NM.12))-1, nchar(colnames(NM.12))) == ".x")]
NM.12 <- NM.12[ , !(colnames(NM.12) %in%
                    c("TWN.labs", "Lookup.label.y", "N.fluxes.Lookup.table.ID.y",
                      "Mean.g.N2O.N.ha.day.y"))]

NM.12$ChambN.y  <- 49
NM.12$Rep.y  <- 12

colnames(NM.12)  <- substr(colnames(NM.12), 1, nchar(colnames(NM.12))-2)

NM.12 <- NM.12[ ,colnames(NM.1)[1:9]]

colnames(NM.12) == colnames(read.csv(
                       "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Above_0C/Ab0_ChambN_9_Normal_N_Rep_2.csv"))


write.csv(NM.12, file = "./Data/Processed_data/Gap_filling/Final/Lookup_tables_by_chamber/Above_0C/Ab0_ChambN_49_Normal_N_Rep_12.csv", row.names = FALSE)

#=====================================================================================================
# END Generate averaged (weighted by flux N) lookup table from two NM lookup tables
#=====================================================================================================


#=====================================================================================================
# Select data to be sustituted
#=====================================================================================================

fluxes.gap.filled$ChambN <- as.integer(fluxes.gap.filled$ChambN)

fluxes.BD.PS  <- fluxes.gap.filled[fluxes.gap.filled$ChambN %in% c(1, 7, 6, 10), ]

fluxes.spli <- split(fluxes.BD.PS, fluxes.BD.PS$ChambN)

#define data selectin function

start <- as.POSIXct("2016-04-22 08:00:00")
stop  <- as.POSIXct("2016-04-22 08:00:00") + 60*60*360

subs.gaps.extr <- function(x) {

    tmp = x[
        x$ChambN %in% c(1, 7, 6, 10) &
        x$Assigned.DateTime >= start &
        x$Assigned.DateTime < stop &
        x$Source != "Measure" , ] 

}

# Extract data to be modified
subs.gaps <- do.call(rbind, lapply(fluxes.spli, subs.gaps.extr))

#Eliminate/change variables to be substituted/changed for these flux estimates
subs.gaps$g.N2O.N.ha.day  <- as.numeric(NA)
subs.gaps$Lookup.table.ID  <- as.character(NA)
subs.gaps$Source  <- as.character("Lookup.table.ab0.NM.1.2")
subs.gaps$Lookup.label  <-  paste0("Ab0_mean1.2_" , substr(subs.gaps$Lookup.label, 8, 14))
subs.gaps$N.fluxes.Lookup.table.ID  <- as.integer(NA)


#Eliminate data to be modified from main df

fluxes.gap.filled.short <- fluxes.gap.filled[!(
    fluxes.gap.filled$Assigned.DateTime %in% subs.gaps$Assigned.DateTime &
    fluxes.gap.filled$ChambN %in% subs.gaps$ChambN), ]



#=====================================================================================================
# END Select data to be sustituted
#=====================================================================================================


#=====================================================================================================
# Gap fill with entries from new lookup table
#=====================================================================================================


# Split gaps to be substituted by chamber
subs.gaps.spli <- split(subs.gaps, subs.gaps$ChambN)


# Define gap filling function

fill.gaps.organic.20160422 <- function(x){

    gaps.ab0 <- x

    for(i in 1:nrow(gaps.ab0)) {

        # Select lookup table entries with same category for time elapsed since N input
        same.Days.N.input.cat =
            NM.12[NM.12$Days.N.input.cat == gaps.ab0$Days.N.input.cat[i], ]

        # Order by ST and SWC decile (if equidistant deciles, smaller one first)
        same.Days.N.input.cat$Rank.st = abs(same.Days.N.input.cat$Soil.T.decile - gaps.ab0$Soil.T.decile[i])
        same.Days.N.input.cat$Rank.swc = abs(same.Days.N.input.cat$Soil.WFPS.decile - gaps.ab0$Soil.WFPS.decile[i])



        same.Days.N.input.cat = same.Days.N.input.cat[
            with(same.Days.N.input.cat, order(Rank.st, Soil.T.decile, Rank.swc, Soil.WFPS.decile, decreasing = FALSE)),
            ]
        


        tmp <- (same.Days.N.input.cat[, c(4:5, 7:11)])
        


        

        # Empty df to which selecte entries will be added in while loop
        select.entr <- tmp[0, ]



        #Loop selecting entries until N fluxes > 10
        while (nrow(tmp) > 0)
            

    {

        # START LOOP

        # Select entries with lowest Rank.swc
        tmp.min.swc <- tmp[which(tmp$Rank.swc == min(tmp$Rank.swc)), ]

        # Within lowest Rank.swc select lowest Rank.st
        tmp.min.swc.st <- tmp.min.swc[which(tmp.min.swc$Rank.st == min(tmp.min.swc$Rank.st)), ]

        # Add entires above to selected entries
        select.entr <- rbind(select.entr, tmp.min.swc.st)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.swc.st)

        # Now select entries with lowest Rank.st
        tmp.min.st <- tmp[which(tmp$Rank.st == min(tmp$Rank.st)), ]

        # Now within lowest Rank.st select lowest Rank.swc
        tmp.min.st.swc <- tmp.min.st[which(tmp.min.st$Rank.swc == min(tmp.min.st$Rank.swc)), ]

        # Append to selected entries
        select.entr <- rbind(select.entr, tmp.min.st.swc)

        #calculate number of underlying fluxes
        N.fluxes  <- sum(select.entr$N.fluxes.Lookup.table.ID)
        if (N.fluxes > 10) break

        # Remove already selected entries from ordered lookup table. This step is ESSENTIAL!!!
        tmp <- setdiff(tmp, tmp.min.st.swc)

        # END LOOP
        

    }


        # Calculate weighted mean flux, N.fluxes(again) and list selected table entry labels

        for(z in 1:nrow(select.entr)) {

            n.flux = sum(select.entr$N.fluxes.Lookup.table.ID[1:z])  
            
            gap.value = with(select.entr, {sum(
                                               Mean.g.N2O.N.ha.day[1:z] * N.fluxes.Lookup.table.ID[1:z] /
                                               sum(N.fluxes.Lookup.table.ID[1:z]))
            }
            )
            
            label = paste(select.entr$Lookup.label[1:z], collapse = ", ")
            

        }

        gaps.ab0$g.N2O.N.ha.day[i] = gap.value
        gaps.ab0$Lookup.table.ID[i] = label
        gaps.ab0$N.fluxes.Lookup.table.ID[i] = n.flux

    }

    return(gaps.ab0)
}

gaps.subs <- do.call(rbind, lapply(subs.gaps.spli, fill.gaps.organic.20160422))

#=====================================================================================================
# END Gap fill with entries from new lookup table
#=====================================================================================================

#=====================================================================================================
# Put newly filled gaps back into main dataframe
#=====================================================================================================

fluxes.gap.filled.1 <- rbind(fluxes.gap.filled.short, gaps.subs)

fluxes.gap.filled.1 <-
    fluxes.gap.filled.1[with(fluxes.gap.filled.1, order(Treat, Rep, Assigned.DateTime)), ]

#=====================================================================================================
# END Put newly filled gaps back into main dataframe
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# END Gaps organic treatments from 2016-04-22 to 2016-05-26
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
# Write all 10 gap filled time series to disk
#=====================================================================================================


write.csv(fluxes.gap.filled.1,
          "./Data/Processed_data/Gap_filling/Final/Gap_filled_flux_df_COMPLETE/ALL_chambers.csv",
          row.names = FALSE)


#=====================================================================================================
# END Write all 10 gap filled time series to disk
#=====================================================================================================



#=====================================================================================================
# Plots
#=====================================================================================================

fluxes.gap.filled  <- read.csv("./Data/Processed_data/Gap_filling/Final/Gap_filled_flux_df_COMPLETE/ALL_chambers.csv")


fluxes.gap.filled$Assigned.DateTime <- as.POSIXct(fluxes.gap.filled$Assigned.DateTime)
fluxes.gap.filled$Rep  <- as.factor(fluxes.gap.filled$Rep)
fluxes.gap.filled$Treat  <- as.factor(fluxes.gap.filled$Treat)

#All 10 time series in one plot



id.code <- data.frame(
    Treat = rep(levels(fluxes.gap.filled$Treat), each =2),
    Rep = rep(levels(fluxes.gap.filled$Rep), 5),
    Source = rep("Lookup.table", 10)
)


library(ggplot2)

X11(24/2.54, 16/2.54)

png("./Figures/Gap_filling/Final/All_chambers.png",
    width = 24, height = 16, units = "cm", res = 600)


ggplot(
    fluxes.gap.filled, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, aes(fill = Source)))+
    scale_colour_manual(values = c("red", "violet","blue", "black"))+
    scale_fill_manual(values = c("red", "violet","blue", "black"))+
    facet_wrap(~Treat+Rep, nrow = 5)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          strip.background =element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 5),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size=8),
          legend.position = "top", legend.margin=margin(0,0,0,0),
          legend.title=element_text(size=5), legend.text=element_text(size=5),
          legend.key.size = unit(0.2, "lines"), legend.box.margin=margin(-1,-10,-10,-10),
          panel.spacing = unit(0.1, "lines"), axis.title.x=element_blank())+
    geom_bar(aes(col= Source, fill = Source), size = 0.1, stat="identity")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_datetime(expand = c(0,0), date_breaks = "10 days")+
    geom_text(data = id.code, x = as.POSIXct("2015-07-07 00:00:00"), y = 400, aes(label = Treat),
              col = "black", size = 3)+
    geom_text(data = id.code, x = as.POSIXct("2015-07-07 00:00:00"), y = 340, aes(label = Rep),
              col = "black", size = 3)


graphics.off()



#One time series in one plot (controls and mineral treats)

fluxes.gap.filled.cont.min <-
    fluxes.gap.filled[fluxes.gap.filled$Treat %in% c("Control", "Normal_N", "High_N"), ]

fluxes.gap.filled.cont.min.spli <-
    split(fluxes.gap.filled.cont.min, with(fluxes.gap.filled.cont.min, list(Treat, Rep)), drop = TRUE)


plotting.fun.cont.min <- function(a){


    time.series <- 

    ggplot(
        a, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, aes(fill = Source)))+
    scale_colour_manual(values = c("red", "blue", "black"))+
    scale_fill_manual(values = c("red", "blue", "black"))+
    #facet_wrap(~Treat+Rep, nrow = 5)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          strip.background =element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size=8),
          legend.position = "top", legend.margin=margin(0,0,0,0),
          legend.title=element_text(size=5), legend.text=element_text(size=5),
          legend.key.size = unit(0.2, "lines"), legend.box.margin=margin(-1,-10,-10,-10),
          axis.title.x=element_blank())+
    geom_bar(aes(col= Source, fill = Source), size = 0.1, stat="identity")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_datetime(expand = c(0,0), date_breaks = "10 days")+
    ggtitle(label = paste0(a$Treat, "_", a$Rep))


    png(paste0(getwd(), "/Figures/Gap_filling/Final/", a$Treat, "_", a$Rep, ".png"),
        width = 24, height = 16, units = "cm", res = 600)    

    print(time.series)

    dev.off()

}


lapply(fluxes.gap.filled.cont.min.spli, plotting.fun.cont.min)


#One time series in one plot (organig treats)

fluxes.gap.filled.org <-
    fluxes.gap.filled[fluxes.gap.filled$Treat %in% c("Pig_slurry", "Biogas_digestate"), ]

fluxes.gap.filled.org.spli <-
    split(fluxes.gap.filled.org, with(fluxes.gap.filled.org, list(Treat, Rep)), drop = TRUE)


plotting.fun.org <- function(a){


    time.series <- 

    ggplot(
        a, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, aes(fill = Source)))+
    scale_colour_manual(values = c("red", "violet","blue", "black"))+
    scale_fill_manual(values = c("red", "violet","blue", "black"))+
    #facet_wrap(~Treat+Rep, nrow = 5)+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          strip.background =element_blank(), strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 7),
          axis.text.y = element_text(size = 7),
          axis.title = element_text(size=8),
          legend.position = "top", legend.margin=margin(0,0,0,0),
          legend.title=element_text(size=5), legend.text=element_text(size=5),
          legend.key.size = unit(0.2, "lines"), legend.box.margin=margin(-1,-10,-10,-10),
          axis.title.x=element_blank())+
    geom_bar(aes(col= Source, fill = Source), size = 0.1, stat="identity")+
    scale_y_continuous(expand = c(0,0))+
    scale_x_datetime(expand = c(0,0), date_breaks = "10 days")+
    ggtitle(label = paste0(a$Treat, "_", a$Rep))


    png(paste0(getwd(), "/Figures/Gap_filling/Final/", a$Treat, "_", a$Rep, ".png"),
        width = 24, height = 16, units = "cm", res = 600)    

    print(time.series)

    dev.off()

}



lapply(fluxes.gap.filled.org.spli, plotting.fun.org)

#=====================================================================================================
# END Plots
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END GAP FILLING FULL AUTO (ALL chambers)
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
# Calculate mean gap filled fluxes by treat
#=====================================================================================================

fluxes <- read.csv(
    "./Data/Processed_data/Gap_filling/Final/Gap_filled_flux_df_COMPLETE/ALL_chambers.csv")


fluxes.spli <- split(fluxes, list(fluxes$Treat, substr(fluxes$Assigned.DateTime, 1, 13)), drop = TRUE)


summ.funct <- function(x){

    data.frame(
        DateTime = as.POSIXct(paste0(unique(substr(x$Assigned.DateTime, 1, 14)), "00:00")),
        ChambN = paste0(x$ChambN, collapse = ","),
        Treat = unique(x$Treat),
        Season = unique(x$Season),
        Plant.status = unique(x$Plant.status),
        N.conc = paste0(x$N.conc, collapse = ","),
        Mean.g.N2O.N.ha.day = mean(x$g.N2O.N.ha.day),
        Mean.soil.t.C = mean(x$Mean.soil.t.C), 
        Mean.soil.WFPS = mean(x$Mean.soil.WFPS)
    )

}

mean.fluxes <- do.call(rbind, lapply(fluxes.spli, summ.funct))

#check time series of fluxes, soil T and soil w

library("ggplot2")

png("./Figures/Gap_filling/Final/Averaged_by_treatment.png",
    width = 24, height = 16, units = "cm", res = 600)

ggplot(data = mean.fluxes, aes(x = DateTime, y = Mean.g.N2O.N.ha.day, col = Treat))+
    #facet_wrap(~Treat, nrow = 5)+
    scale_y_continuous(breaks = seq(0, 400, by = 20))+
    geom_line()

graphics.off()




#=====================================================================================================
# END Calculate mean gap filled fluxes by treat
#=====================================================================================================






#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END  GAP FILLING
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
