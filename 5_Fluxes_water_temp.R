#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FLUXES ~ SOIL TEMPERATURE + SOIL WATER CONTENT
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

## This script
##
## 1) Extract fluxes for soil temperatures at 5cm depth lower than 0°C
## 2) Calculates the linear temperature trends [°C/min] for different time intervals prior to flux
##    measurement
## 3) Plots fluxes for the Control and Normal_N treatments against i) soil temperature and ii) soil
##    temperature rate of change in the hour prior to flux measurement 




setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")


fluxes.var <- read.csv("./Data/Processed_data/Fluxes_soil_daily_var.csv")



#=====================================================================================================
#=====================================================================================================
#  Plots (medium disaggregation) for no-growth season below 0C with temperature and temperature trend
#  as predictors
#=====================================================================================================
#=====================================================================================================

# Extract fluxes at tmeperatures below zero (they are all during no-growth periods)

fluxes.nog.bel0 <- fluxes.var[fluxes.var$Plant.status == "No-growth" &  fluxes.var$Mean.soil.t.C < 0, ]

#Set factor levels and order

fluxes.nog.bel0 <- within(fluxes.nog.bel0, {
    Rep = factor(Rep, levels = c("1","2"))
    Treat = factor(Treat, levels = c("Control", "Normal_N", "High_N", "Pig_slurry", "Biogas_digestate")) 
}

    )


fluxes.nog.bel0 <- fluxes.nog.bel0[with(fluxes.nog.bel0, order(Rep, Treat)), ]
   

# Freezing/thawing history============================================================================



#slim down df

flux.ttrend <- fluxes.nog.bel0[ , c("Assigned.DateTime", "ChambN", "Treat", "Rep", "Season",
                                    "Plant.status", "g.N2O.N.ha.day", "Mean.soil.t.C",
                                    "Mean.soil.WFPS")]

flux.ttrend$Assigned.DateTime  <- as.POSIXct(flux.ttrend$Assigned.DateTime)

#import (mean) ICOS data 5cm depth

icos.data <- read.csv("./Data/Cleaned_data/Soil_T_water/summ_soil_t_5cm.csv")
icos.data$ICOS.Timestamp  <- as.POSIXct(icos.data$ICOS.Timestamp)


difft <- as.numeric(NA)

for(i in 2:nrow(icos.data)){

    difft[1] <- as.numeric(NA)

    difft[i] <- as.numeric(difftime(icos.data$ICOS.Timestamp[i], icos.data$ICOS.Timestamp[i-1],
                                        units = "hours"))
   
    }

table(difft)#ok, the difftime between successive temperature measurements is .5 hours in all cases
# except one (1 hour). Ideal data for calculating temperature trends

#prepare flux  df

difftimes <- c(1, 2, 4, 8, 16, 32, 64, 128)

tmp <- data.frame(matrix(ncol = 24, nrow = (nrow(flux.ttrend))))

colnames(tmp)[seq(1, 24, by = 3)] <- paste("C.min", difftimes, sep = "_")
colnames(tmp)[seq(2, 24, by = 3)] <- paste("Temp.N", difftimes, sep = "_")
colnames(tmp)[seq(3, 24, by = 3)] <- paste("First.last.time", difftimes, sep = "_")


tmp <- within(tmp, {

    First.last.time_1  <- as.character(NA)
    First.last.time_2  <- as.character(NA)
    First.last.time_4  <- as.character(NA)
    First.last.time_8  <- as.character(NA)
    First.last.time_16  <- as.character(NA)
    First.last.time_32  <- as.character(NA)
    First.last.time_64  <- as.character(NA)
    First.last.time_128  <- as.character(NA)

    }
    )





flux.ttrend <- cbind(flux.ttrend, tmp)


flux.ttrend.spli <- split(flux.ttrend, with(flux.ttrend, list(Treat, Rep)))


full.auto.t.trend.fun <- function(x){

    

for(z in which(substr(colnames(x), 1, 6) == "C.min_")) {

    pb.1 <- txtProgressBar(min = 1, max = nrow(x), style = 3)
    
    for(i in 1:nrow(x)) {

        index.1 =  
        which(
            as.numeric(
                difftime(x[i,"Assigned.DateTime"], icos.data[ ,"ICOS.Timestamp"], units = "hours")) >= 0
            &
            as.numeric(
                  difftime(x[i,"Assigned.DateTime"] - 13*60,
                           icos.data[ ,"ICOS.Timestamp"], units = "hours")) <=
              as.numeric(
                  substr(colnames(x)[z],as.numeric(gregexpr(pattern = "_", colnames(x)[z])) + 1,
                      nchar(colnames(x)[z]))
              )
              )
        

        

        x[i, z+1] = length(index.1)

        x[i, z+2] = paste(as.character(icos.data[index.1[length(index.1)],"ICOS.Timestamp"]),
                                   as.character(icos.data[index.1[1], "ICOS.Timestamp"]), sep = "_")

        trend.length.mins = as.numeric(difftime(icos.data$ICOS.Timestamp[index.1],
                                                icos.data$ICOS.Timestamp[index.1][1], units = "mins"))

        temps.in.trend = icos.data$Mean.soil.t.C[index.1]

        if(length(trend.length.mins) > 1 & length(temps.in.trend) > 1) {

            x[i, z] = lm(temps.in.trend ~ trend.length.mins)$coef[2]

            }
        
        else {
                
               x[i, z] = NA

            }

        setTxtProgressBar(pb.1, i)  
            
    }
           

    }

    return(x)
    

}

flux.ttrend <- do.call(rbind, lapply(flux.ttrend.spli, full.auto.t.trend.fun))
rownames(flux.ttrend) <- 1:nrow(flux.ttrend)


write.csv(flux.ttrend, file = "./Data/Processed_data/Fluxes_below_0C_t_trend.csv",  row.names = FALSE)

#=====================================================================================================
# Plots for supplemetary material "Measuring frequncy and accuracy of annual .... estimates"
#=====================================================================================================


flux.ttrend.spli <- split(flux.ttrend, with(flux.ttrend, list(Treat, Rep)), drop = TRUE)

#define plotting function

library(latticeExtra)

cols<-function(n) {
    colorRampPalette(c("grey90", "black"))(60)
                  }



plot.fluxes.water.temp.5 <- function(x){

    for(i in c(150)) {
        

png(paste0("./Figures/Fluxes_soil_T_H2O/Individual_plots/", x$Plant.status[1], "_", x$Treat[1], "_", x$Rep[1], "_", i, ".png"), width = 24, height = 24, units = "cm", res = 200)


print(
    cloud(
        g.N2O.N.ha.day ~ C.min_1 + Mean.soil.t.C , x, panel.3d.cloud=panel.3dbars,
        zlim = c(min(x$g.N2O.N.ha.day ), max(x$g.N2O.N.ha.day)),
        xlim = c(min(x$C.min_1), max(x$C.min_1)),
        ylim = c(-0.00325, -7), 
        #par.settings=my.settings,
        main = paste0( x$Treat[1], "_", x$Rep[1]), 
        col.facet = level.colors(x$g.N2O.N.ha.day,
                                 at = do.breaks(c(min(x$g.N2O.N.ha.day ), max(x$g.N2O.N.ha.day)), 10),
                                 col.regions = cols,colors = TRUE),
        #colorkey = list(labels=list(at=c(seq(-20, 110, by = 10), 103),
                        #labels=c(as.character(seq(-20, 90, by = 10)), expression(bold("Flux")))),
                        #col = cols, at = do.breaks(seq(-20, 90, by = 10)),
        screen = list(z = i, x = -55),
        xbase=0.00025,
        ybase=0.1,
        xlab = list(expression(paste("Temp trend [°C/min]")), rot = 0, cex = 1.5),
        ylab = list("Soil temperature [°C]", rot = 0, cex = 1.5),
        zlab =list(expression(paste("Flux [g ", N[2], "O-N ", ha^{-1}, d^{-1}, "]")), rot = 90, cex = 1.5),
        scales=list(arrows=FALSE, col=1, relation="free")
        #par.settings = list(axis.line = list(col = "transparent"))

        
        
        
    )
)

dev.off()

        }

}


lapply(flux.ttrend.spli, plot.fluxes.water.temp.5)

### Composite plot

# plot list

plot.list <-
    list.files("./Figures/Fluxes_soil_T_H2O/Individual_plots")


plot.list <- plot.list[substr(plot.list, 11, 14) %in% c("Cont", "High")]

plot.list <- paste(".", "/Figures/Fluxes_soil_T_H2O/Individual_plots/", plot.list, sep = "")

plot.list <- as.list(plot.list)



# Define plotting function


library("png")
library("grid")
library("gridExtra")


plots <- lapply(plot.list, function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}

)


library("ggplot2")

ggsave(
    "./Figures/Fluxes_soil_T_H2O/Composite_Control_Normal_N.png",
        width=20, height=20, marrangeGrob(grobs = plots, nrow=2, ncol=2, top = NULL)
)





#=====================================================================================================
# END Plots for supplemetary material "Measuring frequncy and accuracy of annual .... estimates"
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#  ENS Plots (medium disaggregation) for no-growth season below 0C with temperature and temperature
#  trend as predictors
#=====================================================================================================
#=====================================================================================================




#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END FLUXES ~ SOIL TEMPERATURE + SOIL WATER CONTENT
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

