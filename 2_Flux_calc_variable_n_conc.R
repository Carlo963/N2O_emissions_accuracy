#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# FLUX CALCULATION VARIABLE N2O CONCENTRATIONS
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================


## This scripts does the following:
##
## 1) calcuates fluxes for all chosen chamber sampling methods
## 2) applies linear regression models to compare fluxes for different chamber samplign methods with
##    the reference fluxes (i.e. 9 N2O concentrations per flux)
## 3) draws all plots


setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")

#=====================================================================================================
#=====================================================================================================
# Import clean flux dataset calculated with 9 points and extract underlying N2O concentrations
#=====================================================================================================
#=====================================================================================================


#import clean flux data set
clean.9.8 <- read.csv("./Data/Processed_data/Fluxes_9_8_p_linear_regr.csv")

clean.9 <- clean.9.8[clean.9.8$N.conc == 9, ]
clean.9$DateTime.start  <- as.POSIXct(clean.9$DateTime.start)


#import all N2O concentrations during closure
n2o.closed <- read.csv("./Data/Cleaned_data/N2O_ppm_air_T_p/Cham_closed_N2O_icos.csv")
n2o.closed$dateTime <- as.POSIXct(n2o.closed$dateTime)


#split by chamber, date, full hour
n2o.closed.spli <- split(n2o.closed, with(n2o.closed, {list(chambN, substr(dateTime, 1 ,10), substr(dateTime, 12, 13))}), drop = TRUE)


#extract N2O concentrations underlying clean flux data set of fluxes calculated with 9 pints

extract.conc <- function(x){
    
    x <- x[order(x$dateTime), ]
    if(x$dateTime[1] %in% clean.9$DateTime.start) {return(x)}

    }
        

conc.1 <- do.call(rbind, lapply(n2o.closed.spli, extract.conc))

conc.spli.1 <- split(conc.1, with(conc.1, {list(chambN, substr(dateTime, 1 ,10), substr(dateTime, 12, 13))}), drop = TRUE)


#insert min after closure time and number of concentrations per flux


min.closed.n.conc <- function(x) {
                                    
                                    x$Seq.n.conc.flux <- 1:as.integer(nrow(x))
                                    x$Tot.n.conc.flux <- as.integer(nrow(x))
                                    x$min.closure <- as.numeric(
                                        difftime(x$dateTime,
                                                 x$dateTime[1], units = "mins"))
                                    
                                    return(x)

                                                                        
                                    }





library("pbapply")
conc.spli <- pblapply(conc.spli.1, min.closed.n.conc)

conc <- do.call(rbind, conc.spli)

table(conc$Seq.n.conc.flux)
table(conc$Tot.n.conc.flux)
table(conc$min.closure)
#=====================================================================================================
#=====================================================================================================
# END Import clean flux dataset calculated with 9 points and extract underlying N2O concentrations
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# Generate concentrations subsamples
#=====================================================================================================
#=====================================================================================================

# Generate indexes of concentrations to be selected for each closure

index.list <- list()

for(i in 2:9){

    name <- paste0(seq(from = 1, to = i), ",", collapse = "")
    name <- substr(name, 1, nchar(name) - 1)
    
    index.list[[name]] <- seq(from = 1, to = i, by = 1)
        }


index.list[["1,3,5,7,9"]] <- seq(1, 9, 2)
index.list[["1,5,9"]] <-c(1, 5, 9)
index.list[["1,9"]] <-c(1, 9)
index.list[["1,2,8,9"]] <-c(1, 2, 8, 9)
index.list[["1,2,3,7,8,9"]] <-c(1:3, 7:9)


# Generate list of concentrations subsamples

extr.conc.subsets <- function(x){

    flux.versions <- list()

    for(i in 1:length(index.list)){

        x$Flux.version.nominal <- names(index.list[i])
        x$Flux.version.actual <- paste0(x[index.list[[i]], "Seq.n.conc.flux"], ",", collapse = "")
        x$Flux.version.actual  <- substr(x$Flux.version.actual, 1, nchar(x$Flux.version.actual) - 1)
        
        flux.versions[[names(index.list[i])]] <- x[index.list[[i]], ]

        }

    return(flux.versions)
    }


conc.subsets.list.of.lists <- lapply(conc.spli, extr.conc.subsets)


#flatten list
conc.subsets.list <- unlist(conc.subsets.list.of.lists, recursive = FALSE)

#=====================================================================================================
# insert data for 1 concentration only here (background 328.92 ppb (Mean 2015-2016 according to EEA
# and point 9)
#=====================================================================================================

subs.9 <- conc.subsets.list[
    substr(names(conc.subsets.list), nchar(names(conc.subsets.list)) -2, nchar(names(conc.subsets.list))) == "1,9"]


# Data for ideal gas law application
cham.vol.m3 <- 0.268
R <- 8.314
N2O.mol.weight.g <- 14.007*2 + 15.999
N.ratio <- 14.007*2/N2O.mol.weight.g



ins.back.n2o <- function(x){

    if(unique(x$Flux.version.actual) != "1,9")

        print("Error!")
    

    else

        N2O.vol.m3 <- cham.vol.m3 / 1e6 * 328.92 / 1e3 
    moles.N2O <- (N2O.vol.m3 * x$hPa[2] * 100)/(R*(273.15 + x$degC.1[2]))
    g.N2O <- moles.N2O * N2O.mol.weight.g
    g.N2O.N <- g.N2O*N.ratio

    
        

        x$ppmv.N2O[1] <- 328.92
        x$icos.dateTime[1]  <- NA
        x$hPa[1]  <- NA
        x$degC.1[1]  <- NA
        x$g.N2O.N[1]  <- g.N2O.N
        x$Seq.n.conc.flux[1]  <- 0
        x$Flux.version.nominal  <- "9"
        x$Flux.version.actual  <- "9"



    return(x)


    }

subs.9.list <- lapply(subs.9, ins.back.n2o)


# Check assigned background N2O-N masses

backg.mass <- do.call(rbind, lapply(subs.9.list, function(x){

    tmp <- data.frame(

        date  = x$dateTime[2],
        mass.1 = x$g.N2O.N[1],
        mass.2 = x$g.N2O.N[2]
        )
    
    return(tmp)
    
    }
    )
    )



plot(data = backg.mass, mass.2 ~ date)
points(data = backg.mass, mass.1 ~ date, col = "red")
#It seems the conversion from concentrations to masses is correct


#Concatenate lists

conc.subsets.list <- c(conc.subsets.list, subs.9.list)

#=====================================================================================================
# END insert data for 1 concentration only here (background 328.92 ppb (Mean 2015-2016 according to
# EEA  and point 9)
#=====================================================================================================


conc.subsets <- do.call(rbind, conc.subsets.list)

row.names(conc.subsets)  <- 1:nrow(conc.subsets)

all(with(conc.subsets, Flux.version.nominal == Flux.version.actual))

unique(conc.subsets$Flux.version.actual)

write.csv(conc.subsets,
          "./Data/Processed_data/Different_chamber_samplings/Flux_versions_N2O_conc.csv", row.names = FALSE)

rm(conc.subsets)
gc()


#=====================================================================================================
#=====================================================================================================
# END Generate concentrations subsamples
#=====================================================================================================
#=====================================================================================================

 
#=====================================================================================================
#=====================================================================================================
# Calculate each version (i.e. each concentrations subsample) of each flux
#=====================================================================================================
#=====================================================================================================

flux.calc.linear <- function (x) {
    data.frame(
        Flux.version.nominal = unique(x$Flux.version.nominal),
        Flux.version.actual = unique(x$Flux.version.actual),
        DateTime.start  = x$dateTime[1],
        DateTime.stop   = x$dateTime[nrow(x)],
        ChambN          = x$chambN[1],
        N.conc          = x$Tot.n.conc.flux[1], 
        g.N2O.N.ha.day  = lm(g.N2O.N ~ min.closure, x)$coef[2] / 0.44^2 * 1e4 * 60 * 24,
        R2              = as.numeric(summary(lm(x$g.N2O.N ~ x$min.closure, x))[8]),
        NRMSE           = {resid <- as.numeric(resid(lm(x$g.N2O.N ~ x$min.closure,x)))
                           sqrt(sum(resid^2)/length(resid)) / (max(x$g.N2O.N, na.rm =
                           TRUE)-min(x$g.N2O.N, na.rm = TRUE))
                           },
        Lower.95.limit.slope = (confint(lm(x$g.N2O.N ~ x$min.closure, x, na.action = na.omit), level
                                        =0.95)[2]),
        Upper.95.limit.slope = (confint(lm(x$g.N2O.N ~ x$min.closure, x, na.action = na.omit), level
                                        =0.95)[4])
        
        )
    }
    
fluxes.all.versions <- do.call(rbind,lapply(conc.subsets.list, flux.calc.linear))
row.names(fluxes.all.versions) <- 1:nrow(fluxes.all.versions)

all(fluxes.all.versions$Flux.version.nominal == fluxes.all.versions$Flux.version.actual)

write.csv(fluxes.all.versions,
          "./Data/Processed_data/Different_chamber_samplings/Flux_versions.csv", row.names = FALSE)




#insert flux version column for plots

fluxes.all.versions <- read.csv("./Data/Processed_data/Different_chamber_samplings/Flux_versions.csv")

fluxes.all.versions$Flux.version.plots <- as.character(NA)

fluxes.all.versions <- fluxes.all.versions[ , c(
    "Flux.version.nominal", "Flux.version.actual", "Flux.version.plots",  "DateTime.start",
    "DateTime.stop", "ChambN", "N.conc", "g.N2O.N.ha.day", "R2", "NRMSE", "Lower.95.limit.slope",
    "Upper.95.limit.slope"  )]

#fluxes.all.versions <- fluxes.all.versions[1:100, ]

for(i in 1:nrow(fluxes.all.versions)){

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4,5,6,7,8,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20  min"}
    
    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4,5,6,7,8"){
        fluxes.all.versions$Flux.version.plots[i] = "17.5  min"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4,5,6,7"){
        fluxes.all.versions$Flux.version.plots[i] = "15  min"}
    
    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4,5,6"){
        fluxes.all.versions$Flux.version.plots[i] = "12.5  min"}
    
    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4,5"){
        fluxes.all.versions$Flux.version.plots[i] = "10  min"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,4"){
        fluxes.all.versions$Flux.version.plots[i] = "7.5  min"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3"){
        fluxes.all.versions$Flux.version.plots[i] = "5  min"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2"){
        fluxes.all.versions$Flux.version.plots[i] = "2.5  min"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,3,5,7,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20 min, 5 conc, even"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,5,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20 min, 3 conc, even"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20 min, 2 conc, even"}

    if(fluxes.all.versions$Flux.version.actual[i] == "9"){
        fluxes.all.versions$Flux.version.plots[i] = "9"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,8,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20 min, 4 conc, clustered"}

    if(fluxes.all.versions$Flux.version.actual[i] == "1,2,3,7,8,9"){
        fluxes.all.versions$Flux.version.plots[i] = "20 min, 6 conc, clustered"}  


}



write.csv(fluxes.all.versions,
          "./Data/Processed_data/Different_chamber_samplings/Flux_versions.csv", row.names = FALSE)

#=====================================================================================================
#=====================================================================================================
# END Calculate each version (i.e. each concentrations subsample) of each flux
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
# Order data frame and assign flux version names for plotting
#=====================================================================================================
#=====================================================================================================

fluxes.a.v <- read.csv("./Data/Processed_data/Different_chamber_samplings/Flux_versions.csv")

 
#order data frame=====================================================================================

index.list <- list()

for(i in 9:2){

    name <- paste0(seq(from = 1, to = i), ",", collapse = "")
    name <- substr(name, 1, nchar(name) - 1)
    
    index.list[[name]] <- seq(from = 1, to = i, by = 1)
        }


index.list[["1,3,5,7,9"]] <- seq(1, 9, 2)
index.list[["1,5,9"]] <-c(1, 5, 9)
index.list[["1,9"]] <-c(1, 9)
index.list[["9"]] <-9
index.list[["1,2,8,9"]] <-c(1, 2, 8, 9)
index.list[["1,2,3,7,8,9"]] <-c(1:3, 7:9)

index.list <- index.list[c(1, 9:14, 2:8)]

fluxes.a.v$Flux.version.actual  <- factor(fluxes.a.v$Flux.version.actual, levels = names(index.list))

fluxes.a.v <- fluxes.a.v[with(fluxes.a.v, order(Flux.version.actual, ChambN,DateTime.start)), ]

#END order data frame=================================================================================

# Insert flux version names for plotting==============================================================

index.list.2 <- list()

for(i in 9:2){

    name <- paste0(seq(from = 1, to = i), ",", collapse = "")
    name <- substr(name, 1, nchar(name) - 1)
    
    index.list.2[[name]] <-  paste(as.character(2.5 * (i-1)), " min")
        }


index.list.2[["1,3,5,7,9"]] <-
    expression(paste(1^{st}, ", ", 3^{rd}, ", ", 5^{th}, ", ", 7^{th}, ", ", 9^{th}," conc."))
index.list.2[["1,5,9"]] <-
    expression(paste(1^{st}, ", ", 5^{th}, ", ", 9^{th}," conc."))
index.list.2[["1,9"]] <-
     expression(paste(1^{st}, ", ", 9^{th}," conc."))
index.list.2[["9"]] <- "9"
index.list.2[["1,2,8,9"]] <-
    expression(paste(1^{st}, ", ", 2^{nd}, ", ", 8^{th}, ", ", 9^{th}, " conc."))
index.list.2[["1,2,3,7,8,9"]] <-
    expression(paste(1^{st}, ", ", 2^{nd}, ", ", 3^{rd},", ", 7^{th}, ", ", 8^{th}, ", ", 9^{th}, " conc."))

index.list.2 <- index.list.2[c(1, 9:14, 2:8)]



fluxes.a.v$Flux.version.plots <- factor(fluxes.a.v$Flux.version.plots, levels = names(index.list.2))

fluxes.a.v <- fluxes.a.v[with(fluxes.a.v, order(Flux.version.plots, ChambN,DateTime.start)), ]


# END Insert flux version names for plotting==========================================================


#=====================================================================================================
#=====================================================================================================
# END Order data frame and assign flux version names for plotting
#=====================================================================================================
#=====================================================================================================


#=====================================================================================================
#=====================================================================================================
# Plots and linear models hexbins
#=====================================================================================================
#=====================================================================================================


library("ggplot2")
library("hexbin")

for(i in 2:length(names(index.list.2))){#length(names(index.list.2))

    xaxis <- fluxes.a.v[fluxes.a.v$Flux.version.actual == "1,2,3,4,5,6,7,8,9", ]
    yaxis <- fluxes.a.v[fluxes.a.v$Flux.version.actual == names(index.list.2)[i], ]

    mod <- lm(yaxis$g.N2O.N.ha.day ~ xaxis$g.N2O.N.ha.day)
    sum.mod  <-  summary(mod)

    png(paste0(
        "./Figures/Flux_calculation/Fluxes_different_chamber_samplings/Flux_version_",
        paste0(names(index.list.2)[[i]], collapse = "_"),  "_hexbin.png"),
        width = 20, height = 20, units = "cm", res = 600)

 print(
    ggplot(x = xaxis$g.N2O.N.ha.day, y = yaxis$g.N2O.N.ha.day)+
    theme_bw()+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.title = element_blank(), axis.text = element_text(size = 20),
          plot.title = element_text(size = 25),
          legend.title = element_text(size=18), legend.text = element_text(size=14, angle=0))+
    #geom_point(aes(x = xaxis$g.N2O.N.ha.day, y = yaxis$g.N2O.N.ha.day), shape = 20, size = 0.4, alpha = 0.05)+
    geom_hex(aes(x = xaxis$g.N2O.N.ha.day, y = yaxis$g.N2O.N.ha.day, fill = stat(log(count))), bins = 40)+
    geom_abline(intercept = 0, slope = 1, col = "red", size = 2)+
    geom_abline(intercept = mod$coefficients[1], slope = mod$coefficients[2], size = 2, color = "yellow")+
    geom_smooth(aes(x = xaxis$g.N2O.N.ha.day, y = yaxis$g.N2O.N.ha.day), method=lm,
                level = 0.99, color='yellow')+
    annotate("text", x = 200, y = 175, col = "red", size = 8, label = "1:1")+
    {if(round(mod$coefficients[1], 4) > 0)annotate(
        "text", x = 140, y = -10, col = "black", size = 8,
        label = paste0("y  = ", round(mod$coefficients[2], 4), "x + ", round(mod$coefficients[1], 4)))}+
     {if(round(mod$coefficients[1], 4) < 0)annotate(
        "text", x = 140, y = -10, col = "black", size = 8,
        label = paste0("y  = ", round(mod$coefficients[2], 4), "x - ", abs(round(mod$coefficients[1], 4))))}+
    annotate("text", x = 90, y = -24, col = "black", size = 8, label = expression(paste(R^{2}, " = ")))+
    annotate("text", x = 125, y = -25, col = "black", size = 8, label = round(sum.mod$r.squared, 4) )+
    coord_cartesian(xlim = c(-25, 200), ylim = c(-25, 200))+
    ## xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})))+
    ## ylab(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})))+
    ggtitle(index.list.2[[i]])


 )

    graphics.off()

    }


#Merge plots

png.list <- list.files("./Figures/Flux_calculation/Fluxes_different_chamber_samplings", patt = "Flux_version*")



#Shorter chamber closure

png.list.1 <-
    png.list[png.list %in% c(                       
                               "Flux_version_1,2,3,4,5,6,7,8_hexbin.png",
                               "Flux_version_1,2,3,4,5,6,7_hexbin.png", "Flux_version_1,2,3,4,5,6_hexbin.png",
                               "Flux_version_1,2,3,4,5_hexbin.png", "Flux_version_1,2,3,4_hexbin.png",
                               "Flux_version_1,2,3_hexbin.png")]

png.list.1.a <- png.list.1[1:3]
png.list.1.b <- png.list.1[4:6]

png.list.1 <- c(rbind(png.list.1.a, png.list.1.b))

png.list.1 <- as.list(paste0("./Figures/Flux_calculation/Fluxes_different_chamber_samplings/", png.list.1))
png.list.1 <- png.list.1[6:1]



library("png")
library("grid")
library("gridExtra")


plots <- lapply(png.list.1, function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}
)


ggsave(
    "./Figures/Flux_calculation/Fluxes_different_chamber_samplings/Plots_Combined_short_closure_hexbin.png",
    width=24, height=16, marrangeGrob(grobs = plots, nrow=2, ncol=3, top=NULL,
                 left = textGrob(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})),
                                 gp = gpar(fontsize = 25, font = 8), rot = 90),
                  bottom = textGrob(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})),
                 gp = gpar(fontsize = 25, font = 8)))
)


#Same chamber closure, evenly spaced sampling and sampling concentrated after closure and before
#opening, one measured concentration at 20 minutes and backgound concentration as first point.


png.list.2 <-  c(
"Flux_version_1,2,3,7,8,9_hexbin.png", "Flux_version_1,2,8,9_hexbin.png", "Flux_version_1,9_hexbin.png",
"Flux_version_1,3,5,7,9_hexbin.png", "Flux_version_1,5,9_hexbin.png", "Flux_version_9_hexbin.png")

png.list.2.a <- png.list.2[1:3]
png.list.2.b <- png.list.2[4:6]

png.list.2 <- c(rbind(png.list.2.a, png.list.2.b))

png.list.2 <- as.list(paste0("./Figures/Flux_calculation/Fluxes_different_chamber_samplings/", png.list.2[1:6]))



library("png")
library("grid")
library("gridExtra")


plots <- lapply(png.list.2[1:5], function(x){
    
    img <- as.raster(readPNG(x))
    rasterGrob(img, interpolate = FALSE)
}
)


ggsave(
    "./Figures/Flux_calculation/Fluxes_different_chamber_samplings/Plots_Combined_same_closure_hexbin.png",
        width=24, height=16, marrangeGrob(grobs = plots, nrow=2, ncol=3, top=NULL,
                 left = textGrob(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})),
                                 gp = gpar(fontsize = 25, font = 8), rot = 90),
                  bottom = textGrob(expression(paste("g ", N[2], "O-N ", ha^{-1}, d^{-1})),
                 gp = gpar(fontsize = 25, font = 8)))
)


#=====================================================================================================
#=====================================================================================================
# END Plots and linear models hexbins
#=====================================================================================================
#=====================================================================================================



#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
# END FLUX CALCULATION VARIABLE N2O CONCENTRATIONS
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================
#=====================================================================================================

