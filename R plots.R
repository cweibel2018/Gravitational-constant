library(ape)
library(MASS)
library(nlme)
library(lme4)
require(lmerTest)
library(optimx)
library(ggplot2)
library(MuMIn)
library(plotly)
library(plyr)
library(phytools)
library(scatterplot3d)
library(rgl)
library(gridExtra)

df<- read.csv(file = "~/PHYS382/PHYS Labs/The Big G/+-/dataforRplot.txt", header = T)
str(df)

fitfunction<- function(x) 70*exp(-0.001537*x)*cos(0.03182*x + 3.842) + -0.001098*x -4.582

fitdata<- data.frame(df$Time,df$Value,fitfunction(df$Time), df$Value- fitfunction(df$Time)) 
  
plot <-ggplot(data= df, aes(Time,Value, color = "data")) + geom_point(size=2) + labs( x = "Time [sec]", y = "Angle [mrad]")
plot <- plot+theme(axis.title=element_text(size=24, face ="bold"),axis.text =element_text(size=24, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
plot <- plot + geom_point(data=df, aes(Time, fitfunction(Time), color = "curve fit"), size = 1) +theme(legend.title=element_blank(),legend.text=element_text(size = 18),legend.position = c(0.89,0.875))
plot

plot1 <-ggplot(data= fitdata, aes(df.Time,df.Value...fitfunction.df.Time.)) + geom_point(size=1) + labs( x = "Time [sec]", y = "Residuals [mrad]")
plot1 <- plot1+theme(axis.title=element_text(size=24, face ="bold"),axis.text =element_text(size=24, face ="bold"),panel.background = element_rect(fill = "white", colour = "grey50"),panel.grid.major = element_line(colour = "grey90"),panel.grid.minor = element_line(colour = "grey90"))
plot1 <- plot1 + geom_hline(yintercept=0, linetype="dashed", color="red", size = 1)
plot1

grid.arrange(plot,plot1, nrow=2)

