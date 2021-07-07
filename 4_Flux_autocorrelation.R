#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FLUX PARTIAL AUTOCORRELATION
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

## This script calculates the partial autocorrelation of fluxes. Analysis focuses on not-gap filled
## time series averavged by treatment (i.e. by spatial replicate)


setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# AGGREGATION BY TREATMENT
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#Import fluxes averaged by treatment in script 5 and split by treatment

flux.av <- read.csv("./Data/Processed_data/Fluxes_averaged_by_treatment.csv")

flux.av <- flux.av[ , c("DateTime", "Treat", "Mean.g.N2O.N.ha.day")]

flux.av <- within(flux.av, {
    
    DateTime = as.POSIXct(DateTime)
    Treat = factor(Treat, levels = c("Control", "Normal_N", "High_N", "Pig_slurry", "Biogas_digestate" ))

    }
    )

flux.av <- flux.av[with(flux.av, order(Treat, DateTime)), ]

flux.av.spli <- split(flux.av, flux.av$Treat)




# Genrate PACF plots for each treatment

plot.pacf <- function(x){

    png(paste0(
        "./Figures/Flux_autocorrelation/Partial_autocorrelation/One_treat_", unique(x$Treat),
        ".png"), width = 26*0.7, height = 8 * 0.7, units = "cm", res = 400)

    # crop white space bottom, left, top,right

    if (unique(x$Treat) == "Biogas_digestate"){
        
        par(mar = c(2, 2, 0.1, 0.1))

    }

    else {

        #par(mar = c(0.6, 2, 0.1, 0.1))
        par(mar = c(2, 2, 0.1, 0.1))

    }

    pacf(x$Mean.g.N2O.N.ha.day, lag.max = 72,  xlim = c(0, 72), ylim = c(-0.05, 0.9), xaxt="n", main = "",
         xlab = "", ylab = "", ci.col = "red", lwd = 4)

    title(unique(x$Treat), line = -2) 

    if(unique(x$Treat) == "Biogas_digestate"){
        
        axis(side = 1, at = seq(1, 72, by = 1), labels = seq(1, 72, by = 1), cex.axis=1)        

    }

    else {

        #axis(side = 1, at = seq(1, 72, by = 1), labels=FALSE)
        axis(side = 1, at = seq(1, 72, by = 1), labels = seq(1, 72, by = 1), col.axis = "white", cex.axis=1)
         

    }

    axis(side = 2, at = seq(0, 0.9, by = 0.2))


    graphics.off()

}

lapply(flux.av.spli, plot.pacf)




# Genearate composite PACF plot containting all treatments

png.list <-
    list.files("./Figures/Flux_autocorrelation/Partial_autocorrelation/",
               patt = "*One_treat_*")

png.list <- png.list[c(2, 4, 3, 5, 1)]


png.list <- paste("./Figures/Flux_autocorrelation/Partial_autocorrelation/", png.list, sep = "")

library("ggplot2")
library("png")
library("grid")
library("gridExtra")


plots <- lapply(png.list, function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}
)




ggsave(
    "./Figures/Flux_autocorrelation/Partial_autocorrelation/All_treat.png",
    width=16, height=24,
    marrangeGrob(grobs = plots, nrow=5, ncol=1, padding = unit(3, "line"), 
                 top = NULL,
                 bottom = textGrob("Lag [hours]", gp = gpar(fontsize = 30, font = 8)),
                 left = textGrob(
                     expression("Partial ACF"),
                     gp = gpar(fontsize = 30, font = 8), rot = 90)
                 ) 
)





#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END AGGREGATION BY TREATMENT
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END FLUX PARTIAL AUTOCORRELATION
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================



