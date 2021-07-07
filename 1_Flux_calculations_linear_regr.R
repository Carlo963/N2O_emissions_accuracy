#=====================================================================================================
#=====================================================================================================
# FLUX CALCULATION
#=====================================================================================================
#=====================================================================================================


## This script contains the following:
##
##  1) contingency table N2O concentrations per flux
##  2) removes concentration sets with less than 8 concentrations
##  3) flux calculations with linear regressions, R2 vs flux, NRMSE vs flux, detection limit
##  4) generate plots for manuscript
##  5) calculate flux detection limits




setwd("/home/pro/projects/Lanna_papers/Methodological_paper_manuscript/Submission_Agr_For_Met/Submission_major_revision_2/Data_analysis/")

n2o.closed <- read.csv( "./Data/Cleaned_data/N2O_ppm_air_T_p/Cham_closed_N2O_icos.csv")

n2o.closed$dateTime <- as.POSIXct(n2o.closed$dateTime)


sum(is.na(n2o.closed$g.N2O.N))
sum(is.na(n2o.closed$hPa))
sum(is.na(n2o.closed$degC.1))

which(is.na(n2o.closed$g.N2O.N)) == which(is.na(n2o.closed$hPa))
which(is.na(n2o.closed$hPa)) == which(is.na(n2o.closed$degC.1))


#split by chamber, date, full hour


n2o.closed.spli <- split(n2o.closed, with(n2o.closed, {list(chambN, substr(dateTime, 1 ,10), substr(dateTime, 12, 13))}), drop = TRUE)




#count rownumbers for each df in list (i.e. number of concentrations per flux)

nrow.vector <- unname(unlist(lapply(n2o.closed.spli, nrow)))

table(nrow.vector)



#insert min after closure time and number of concentrations per flux


min.closed.n.conc <- function(x) {
                                    
                                    x$n.conc.flux <- as.integer(nrow(x))
                                    x$min.closure <- as.numeric(difftime(x$dateTime, x$dateTime[1], units = "mins"))
                                    
                                    return(x)

                                                                        
                                    }





library("pbapply")


n2o.closed <- do.call(rbind,pblapply(n2o.closed.spli, min.closed.n.conc))

n2o.closed <- n2o.closed[with(n2o.closed, order(chambN, dateTime)), ]


#remove all instances in wich conc. n per fulx is not 8 or 9

unique(n2o.closed$n.conc.flux)


n2o.9.8 <- n2o.closed[n2o.closed$n.conc.flux %in% c(8,9), ]
n2o.9.8 <- n2o.9.8[with(n2o.9.8, order(chambN, dateTime)), ]

unique(n2o.9.8$n.conc.flux)

n2o.9  <-  n2o.closed[n2o.closed$n.conc.flux == 9, ]
unique(n2o.9$n.conc.flux)
unique(n2o.9$min.closure)
table(n2o.9$min.closure)
table(round(n2o.9$min.closure, digits=0))
table(n2o.9$chambN)


n2o.8  <-  n2o.closed[n2o.closed$n.conc.flux == 8, ]
unique(n2o.8$n.conc.flux)
unique(n2o.8$min.closure)
table(n2o.8$min.closure)
table(round(n2o.8$min.closure, digits=0))
table(n2o.8$chambN)# 8 N2O conc. instead of 9 mainly for chamber 4


cham.4 <- n2o.9.8[n2o.9.8$chambN == 4, ]
table(substr(cham.4$dateTime, 15, 19))

#=====================================================================================================
# Flux calculation (linear regression, 9 and 8 N2O masses per flux)
#=====================================================================================================


n2o.9.8.spli <- split(n2o.9.8,  with(n2o.9.8, {list(chambN, substr(dateTime, 1 ,10), substr(dateTime, 12, 13))}), drop = TRUE)


flux.calc.linear <- function (x) {
    data.frame(
        Assigned.DateTime = x$dateTime[1] + difftime(x$dateTime[nrow(x)],x$dateTime[1])/2,
        DateTime.start  = x$dateTime[1],
        DateTime.stop   = x$dateTime[nrow(x)],
        ChambN          = x$chambN[1],
        N.conc          = x$n.conc.flux[1], 
        g.N2O.N.min     = lm(g.N2O.N ~ min.closure, x)$coef[2],
        slope.p.1.2     = if(is.na(x$g.N2O.N[1]) | is.na(x$g.N2O.N[2]))  {NA} else {
                                 lm(g.N2O.N ~ min.closure, x[1:2,])$coef[2]}, slope.p.nminus1.n
                                 =if(is.na(x$g.N2O.N[nrow(x)-1]) | is.na(x$g.N2O.N[nrow(x)])) {NA}
                                  else{ lm(g.N2O.N ~ min.closure, x[((nrow(x)-1):nrow(x)),
                                                                       ])$coef[2]},
        R2              = as.numeric(summary(lm(x$g.N2O.N ~ x$min.closure, x))[8]),

        measured.range  = (max(x$g.N2O.N, na.rm = TRUE)-min(x$g.N2O.N, na.rm = TRUE)),
        
        predicted.range = {predicted <- predict.lm(lm(x$g.N2O.N ~ x$min.closure,x))
                           max(predicted)-min(predicted)
                           },

        RMSE            =  {resid <- as.numeric(resid(lm(x$g.N2O.N ~ x$min.closure,x)))
                           sqrt(sum(resid^2)/length(resid))
                           },
        
        
        NRMSE.old       =  {resid <- as.numeric(resid(lm(x$g.N2O.N ~ x$min.closure,x)))
                           sqrt(sum(resid^2)/length(resid)) / (max(x$g.N2O.N, na.rm =
                           TRUE)-min(x$g.N2O.N, na.rm = TRUE))
                           },
        NRMSE           = {resid <- as.numeric(resid(lm(x$g.N2O.N ~ x$min.closure,x)))
                           predicted <- predict.lm(lm(x$g.N2O.N ~ x$min.closure,x))
                           sqrt(sum(resid^2)/length(resid)) / (max(predicted)-min(predicted))
                           },
        P.coeff1.N2O   = summary(lm(x$g.N2O.N ~ x$min.closure, x, na.action = na.omit))$coef[8],
        Lower.95.limit.slope = (confint(lm(x$g.N2O.N ~ x$min.closure, x, na.action = na.omit), level
                                        =0.95)[2]),
        Upper.95.limit.slope = (confint(lm(x$g.N2O.N ~ x$min.closure, x, na.action = na.omit), level
                                        =0.95)[4])
    )
}


fluxes.9.8.tmp <- do.call(rbind,lapply(n2o.9.8.spli, flux.calc.linear))

fluxes.9.8 <- fluxes.9.8.tmp

#From slope (g N2O-N/min) to flux [g N2O-N/(ha*day)]

fluxes.9.8$g.N2O.N.ha.day <- fluxes.9.8$g.N2O.N.min / 0.44^2 * 1e4 * 60 *24
fluxes.9.8$Lower.95.confint.flux <- fluxes.9.8$Lower.95.limit.slope / 0.44^2 * 1e4 * 60 *24
fluxes.9.8$Upper.95.confint.flux <- fluxes.9.8$Upper.95.limit.slope / 0.44^2 * 1e4 * 60 *24
fluxes.9.8$Confint95.amplitude <- fluxes.9.8$Upper.95.confint.flux - fluxes.9.8$Lower.95.confint.flux
fluxes.9.8$Abs.rel.confint.amplitude  <- fluxes.9.8$Confint95.amplitude /
    abs(fluxes.9.8$g.N2O.N.ha.day)

fluxes.9.8 <- fluxes.9.8[with(fluxes.9.8, order(ChambN, Assigned.DateTime)), ]


#=====================================================================================================
# END Flux calculation (linear regression, 9 and 8 N2O masses per flux)
#=====================================================================================================


#=====================================================================================================
# Eliminate outlying fluxes (based on max min + R2 cutoff and known issues with measuring system)
#=====================================================================================================


# Max min cutoff with R2 as additional criterion


sum(fluxes.9.8$g.N2O.N.ha.day > 400)
sum(fluxes.9.8$g.N2O.N.ha.day < -70)

plus.outliers <- fluxes.9.8[fluxes.9.8$g.N2O.N.ha.day > 400, ]
minus.outliers <- fluxes.9.8[fluxes.9.8$g.N2O.N.ha.day <  -70, ]
outliers <- rbind(plus.outliers, minus.outliers)


plus.outliers.R2 <- fluxes.9.8[fluxes.9.8$g.N2O.N.ha.day > 400 & fluxes.9.8$R2 < 0.9, ]
minus.outliers.R2 <- fluxes.9.8[fluxes.9.8$g.N2O.N.ha.day < -70 & fluxes.9.8$R2 < 0.9, ]
outliers.R2 <- rbind(plus.outliers.R2, minus.outliers.R2)

fluxes.9.8 <- fluxes.9.8[!(fluxes.9.8$g.N2O.N.ha.day > 400 & fluxes.9.8$R2 < 0.9), ]

fluxes.9.8 <- fluxes.9.8[!(fluxes.9.8$g.N2O.N.ha.day < -70 & fluxes.9.8$R2 < 0.9), ]


# END Max min cutoff with R2 as additional criterion





### Fluxes eliminated based on known issues with measuring system

#2015-06-09 15:xx (suspicius techincally because immediately after data elimination and all considerably and very similarly negative)


ggplot(fluxes.9.8, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, col = ChambN))+
       facet_wrap(~ChambN, nrow = 10)+
       theme_bw()+
       coord_cartesian(ylim = c(-100,50), xlim = as.POSIXct(c("2015-06-08 00:00:00","2015-06-10 00:00:00")))+
       geom_point(size = 1.0)




suspicious.1 <- fluxes.9.8[substr(fluxes.9.8$Assigned.DateTime, 1, 14) == "2015-06-09 15:", ]

fluxes.9.8 <- fluxes.9.8[substr(fluxes.9.8$Assigned.DateTime, 1, 14) != "2015-06-09 15:", ]


ggplot(fluxes.9.8, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, col = ChambN))+
       facet_wrap(~ChambN, nrow = 10)+
       theme_bw()+
       coord_cartesian(ylim = c(-100,50), xlim = as.POSIXct(c("2015-06-08 00:00:00","2015-06-10 00:00:00")))+
       geom_point(size = 1.0)



#chamber 4 2015-07-20 09:09:45(see no anomaly on that day!?)


ggplot(fluxes.9.8, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, col = ChambN))+
       facet_wrap(~ChambN, nrow = 10)+
       theme_bw()+
       coord_cartesian(ylim = c(-100,50), xlim = as.POSIXct(c("2015-07-19 00:00:00","2015-07-21 00:00:00")))+
       geom_point(size = 1.0)


#chamber 2 2015-06-18 13:10:35(see no anomaly on that day!?)


ggplot(fluxes.9.8, aes(x = Assigned.DateTime, y = g.N2O.N.ha.day, col = ChambN))+
       facet_wrap(~ChambN, nrow = 10)+
       theme_bw()+
       coord_cartesian(ylim = c(-100,50), xlim = as.POSIXct(c("2015-06-17 00:00:00","2015-06-20 00:00:00")))+
       geom_point(size = 1.0)

suspicious.3 <- fluxes.9.8[fluxes.9.8$Assigned.DateTime > as.POSIXct("2015-06-18 00:00:00") & fluxes.9.8$Assigned.DateTime < as.POSIXct("2015-06-19 00:00:00") &  fluxes.9.8$ChambN == 2,  ]



### END Fluxes eliminated based on known issues with measuring system



#=====================================================================================================
# END Eliminate outlying fluxes (based on max min + R2 cutoff and known issues with measuring system)
#=====================================================================================================


#write clean flux dataset to disk

write.csv(fluxes.9.8[ ,!(colnames(fluxes.9.8)%in%c("slope.p.1.2","slope.p.nminus1.n"))], "./Data/Processed_data/Fluxes_9_8_p_linear_regr.csv", row.names = FALSE)





#=====================================================================================================
# Plots for manuscript
#=====================================================================================================

fluxes.9.8 <- read.csv("./Data/Processed_data/Fluxes_9_8_p_linear_regr.csv")


fluxes.9.8 <- 
within(fluxes.9.8, {

     Assigned.DateTime   <- as.POSIXct(Assigned.DateTime)     
     DateTime.start      <- as.POSIXct(DateTime.start)     
     DateTime.stop       <- as.POSIXct(DateTime.stop) 

    }
    )

    
#extract fluxes for which 9 points are available

fluxes.9 <- fluxes.9.8[fluxes.9.8$N.conc == 9, ]

library(ggplot2)

ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = R2))+
    coord_cartesian(xlim = c(-0,25))+
    geom_bin2d(bins = 2000)+
    scale_fill_gradient(breaks = seq(0, 1250, by = 100), low = "#CCCCCC", high = "#000000")
    #geom_hex()


#plot R2 vs flux

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/R2_vs_flux_manuscr.png", width = 24, height = 16, units = "cm", res = 600)



ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = R2))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    coord_cartesian(xlim = c(-10,35), ylim = c(0,1))+
    scale_x_continuous(breaks = seq(-75, 80, 2.5))+
    geom_point(shape = ".", alpha = 0.25)+
    xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, day^{-1})))+
    ylab(expression( R^{2}))

graphics.off()



#plot NRMSE vs flux

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/NRMSE_vs_flux_manuscr.png", width = 24, height = 16, units = "cm", res = 600)


ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = NRMSE))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())+
    coord_cartesian(xlim = c(-10,35), ylim = c(0, 2.5))+
    scale_x_continuous(breaks = seq(-75, 80, 2.5))+
    geom_point(shape = ".", alpha = 0.25)+
    xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, day^{-1})))+
    ylab("NRMSE")

graphics.off()


#combined R2 & NRMSE plots



p1 <-
    ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = R2))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          plot.margin= margin(t = 4, r = 4, b = 0, l = 4, unit = "pt"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(size = 7.8))+
    coord_cartesian(xlim = c(-10,100), ylim = c(0,1))+
    #scale_x_continuous(breaks = seq(-75, 80, 2.5))+
    geom_point(shape = ".", alpha = 0.1)+
    #xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, day^{-1})))+
    ylab(expression( R^{2}))

p2 <-
    ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = NRMSE))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          plot.margin= margin(t = 0, r = 4, b = 0, l = 4, unit = "pt"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(size = 7.8))+
    coord_cartesian(xlim = c(-10,100), ylim = c(0, 2.5))+
    scale_y_continuous(breaks = seq(0, 3, 0.5))+
    geom_point(shape = ".", alpha = 0.1)+
    xlab(expression(paste("Flux [g ", N[2], "O-N ", ha^{-1}, day^{-1}, "]")))+
    ylab("NRMSE")




library(cowplot)

R2.NRMSE <- plot_grid(p1, p2, nrow = 2, align="v")

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/R2NRMSE_vs_flux_manuscr.png", width = 24, height = 16, units = "cm", res = 600)

R2.NRMSE

graphics.off()

#plot confint95s vs fluxes

p3 <- 
ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, ymin = Lower.95.confint.flux, ymax =
                                                                           Upper.95.confint.flux))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          plot.margin= margin(t = 4, r = 4, b = 0, l = 4, unit = "pt"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    coord_cartesian(xlim = c(-10, 35), ylim = c(-50, 100))+
    ylab(expression(paste("Confid. intervals [g ", N[2], "O-N ", ha^{-1}, day^{-1}, "]")))+
    geom_linerange(size = 0.05)


#plot confint95 amplitude/flux ~ flux

p4 <- 
ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = Abs.rel.confint.amplitude))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
           plot.margin= margin(t = 0, r = 4, b = 0, l = 4, unit = "pt"),
           axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 8))+
    coord_cartesian(xlim = c(-10, 100),ylim = c(0, 10))+
    scale_x_continuous(breaks = seq(-75, 100, 5))+
    xlab(expression(paste("Flux [g ", N[2], "O-N ", ha^{-1}, day^{-1}, "]")))+
    ylab("RCIW")+
    geom_point(shape = ".", alpha = 0.10)


Confint <- plot_grid(p3, p4, nrow = 2, align="v")

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/Confint_flux_manuscr.png", width = 24, height = 16, units = "cm", res = 600)

Confint

graphics.off()


#Panel with R2, NRMSE and plot confint95 amplitude/flux ~ flux


R2.NRMSE.Confint <- plot_grid(p1, p2, p4, rel_heights = c(0.8, 0.8, 1), nrow = 3, align="v")

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/R2_NRMSE_Confint_manuscr.png", width = 24, height = 16, units = "cm", res = 600)

R2.NRMSE.Confint

graphics.off()





#plot histogram of amplitudes of confints

ggplot(fluxes.9, aes(x = Confint95.amplitude))+
    theme_bw()+
    geom_histogram(binwidth = 2)

breaks=seq(20, 50, by = 2)
                 



#plot RMSE amplitude vs flux

p5 <-
    ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = RMSE))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          plot.margin= margin(t = 0, r = 4, b = 0, l = 4, unit = "pt"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(size = 7.8))+
    coord_cartesian(xlim = c(-10,100), ylim = c(0, 2e-6))+
    #scale_x_continuous(breaks = seq(-75, 80, 2.5))+
    geom_point(shape = ".", alpha = 0.1)+
    #xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, day^{-1})))+
    ylab((expression(paste("RMSE [g ", N[2], "O-N]"))))


p6 <-
    ggplot(fluxes.9, aes(x = g.N2O.N.ha.day, y = Confint95.amplitude))+
    theme_bw()+
    theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
          plot.margin= margin(t = 0, r = 4, b = 0, l = 4, unit = "pt"),
          axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y = element_text(size = 7.8))+
    coord_cartesian(xlim = c(-10,100), ylim = c(0, 25))+
    #scale_y_continuous(breaks = seq(0, 150, 50))+
    geom_point(shape = ".", alpha = 0.1)+
    #xlab(expression(paste("g ", N[2], "O-N ", ha^{-1}, day^{-1})))+
    ylab((expression(paste("CIW [g ", N[2], "O-N ", ha^{-1}, day^{-1}, "]"))))







R2.RMSE.NRMSE.CIW.RCIW <-
    plot_grid(p1, p5, p2, p6, p4, rel_heights = c(0.8, 0.8, 0.8, 0.8, 1.1), nrow = 5, align="v")

X11(24/2.54, 16/2.54)

png("./Figures/Flux_calculation/R2.RMSE.NRMSE.CIW.RCIW_manuscr.png",
    width = 24, height = 16, units = "cm", res = 600)

R2.RMSE.NRMSE.CIW.RCIW

graphics.off()





#=====================================================================================================
# END Plots for manuscript
#=====================================================================================================


#=====================================================================================================
# Calculate detection limits METHOD 2 (simulated N2O concentrations for fluxes = 0)
#=====================================================================================================

## 1) Residuals will be extracted from linear regressions
## 2) Dependence of residuals on flux magnitude will be tested. If dependence analysis limited to very
##    small fluxes (-2, 2?)
## 3) Normality of residuals will be tested, then meand and SD will be calculated
## 4) "Simulated measured N2O concentrations" will be determined for fluxes = 0 (background conc.) by
##     adding random variabeĺe (normally distributed, mean and SD as determined above)
## 5) Fluxes will be calculated by fitting linear regressions to these "simulated measured N2O
##    concentrations"
## 6) Resulting fluxes will be tested for normality and 0.025-0.975 quantiles will be calucalted (+/- 2 sd).
##    These will be considered estimates of the positive/
##    positive/negative detection limits

# Extract all residuals from fluxes calculated with 9 points


resid.extr <- function(x){

            data.frame(
        Assigned.DateTime = x$dateTime[1] + difftime(x$dateTime[nrow(x)],x$dateTime[1])/2,
        ChambN          = x$chambN[1],
        N.conc          = x$n.conc.flux[1], 
        Slope           = lm(g.N2O.N ~ min.closure, x)$coef[2],
        g.N2O.N.ha.day  = (lm(g.N2O.N ~ min.closure, x)$coef[2]) / 0.44^2 * 1e4 * 60 *24,
        Residuals       = resid(lm(x$g.N2O.N ~ x$min.closure,x))


            )

    }

resid.tab <- do.call(rbind, lapply(n2o.9.8.spli, resid.extr))

#limit to fluxes calculated with 9 concnetrations

resid.tab <- resid.tab[resid.tab$N.conc == 9, ]


# There are crazy values. Remove.
resid.tab <- resid.tab[resid.tab$g.N2O.N.ha.day < 5000 &  resid.tab$g.N2O.N.ha.day > -5000, ]

plot(Residuals ~ g.N2O.N.ha.day, resid.tab)
abline(lm(Residuals ~ g.N2O.N.ha.day, resid.tab))

# Residuals do not seem to depend on flux magnitude. Let's test it

summary(lm(Residuals ~ g.N2O.N.ha.day, resid.tab))
#TOTALLY no dependence on flux magnitude


# Visually test normality of residuals
hist(resid.tab$Residuals, breaks = 2000, xlim = c(2e-06 , -2e-06))

plot(density(resid.tab$Residuals), xlim = c(2e-06 , -2e-06))

qqnorm(resid.tab$Residuals)
qqline(resid.tab$Residuals, col = 2)
#Distribution symmetrical, but heavy tails! Let'test it with Shapiro-Wilk test.

# Shapiro-Wilk test (requires sample no larger than 5000. So will be done on random sample)

resid.5000  <-
    resid.tab$Residuals[as.integer(round(runif(5000, min = 1, max = length(resid.tab$Residuals)), 0))]

shapiro.test(resid.5000)
# So, p-value extremely low, not normally distributed. Will assume normality in simulating fluxes

# Calculate Mean and SD+

mean.resid <- mean(resid.tab$Residuals)
sd.resid <- sd(resid.tab$Residuals)

#=====================================================================================================
# Simulate measured concentrations for fluxes = 0


# Calculate N2O mass in chamber for conc = 329 ppm, temperature = 7.96°C (average T during trial) and
# air pressure = 1004.10 hPa (average air pressure during trial)


N2O.ppmv <- 0.329
air.T.C <- 7.96
air.p.hPa <- 1004.10

cham.vol.m3 <- 0.268
R <- 8.314
N2O.mol.weight.g <- 14.007*2 + 15.999
N.ratio <- 14.007*2/N2O.mol.weight.g



N2O.vol.m3 <- cham.vol.m3 / 1e6 * N2O.ppmv

moles.N2O <- (N2O.vol.m3 * air.p.hPa * 100)/(R*(273.15 + air.T.C))

g.N2O <- moles.N2O * N2O.mol.weight.g

g.N2O.N <- g.N2O*N.ratio


# Generate simulated measurements for fluxes = 0

flux.list <-  vector(mode = "list", length = 1e6)

for(i in 1:length(flux.list)){


flux.list[[i]] <- data.frame(

    Flux.n = i,
    Closure.min  = seq(0, 20, 2.5),
    Backg.N2O  = as.numeric(g.N2O.N),
    Simul.meas.N2O =  g.N2O.N + rnorm(n = 9, mean = mean.resid, sd = sd.resid)

)
}


# Calculate fluxes from simulated measurements


calc.sim.fluxes <- function(x){

    data.frame(
        
        Flux.n = unique(x$Flux.n),
        g.N2O.N.ha.day  = (lm(Simul.meas.N2O ~ Closure.min, x)$coef[2]) / 0.44^2 * 1e4 * 60 *24
        
   )

}

sim.0.fluxes <- do.call(rbind, lapply(flux.list, calc.sim.fluxes))


# END Simulate measured concentrations for fluxes = 0
#====================================================================================================


# Simulated fluxes normally distributed? If yes 95% of fluxes within +- 2 sd. These are detection limits

hist(sim.0.fluxes$g.N2O.N.ha.day, breaks = 100)

plot(density(sim.0.fluxes$g.N2O.N.ha.day))

qqnorm(sim.0.fluxes$g.N2O.N.ha.day)
qqline(sim.0.fluxes$g.N2O.N.ha.day, col = 2)

sim.0.fluxes.5000  <-
    sim.0.fluxes$g.N2O.N.ha.day[
                     as.integer(round(runif(5000, min = 1, max = length(sim.0.fluxes$g.N2O.N.ha.day)), 0))]

shapiro.test(sim.0.fluxes.5000)
#p-value high, so normality assumption reasonably accurate



# Calculate mean, sd and detection limits

mean(sim.0.fluxes$g.N2O.N.ha.day)
sd(sim.0.fluxes$g.N2O.N.ha.day)

upper.limit <- mean(sim.0.fluxes$g.N2O.N.ha.day) + 2 * sd(sim.0.fluxes$g.N2O.N.ha.day)
lower.limit <- mean(sim.0.fluxes$g.N2O.N.ha.day) - 2 * sd(sim.0.fluxes$g.N2O.N.ha.day)

upper.limit
lower.limit

#=====================================================================================================
# END Calculate detection limits METHOD 2 (simulated N2O concentrations for fluxes = 0)
#=====================================================================================================

#=====================================================================================================
#=====================================================================================================
# END FLUX CALCULATION
#=====================================================================================================
#=====================================================================================================
