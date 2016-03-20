
# load dataset, retrieving if necessary.
# For more: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html
if(!file.exists("airquality.csv") ){
  
  # R has this dataset built in usually but for completeness' sake:
  download.file("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/airquality.csv"
                  , destfile = "airquality.csv")
}
aq <- read.table("airquality.csv", header=TRUE, 
                   sep=",")

# IMPORT LIBRARIES -------------------------------------------
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot) # package may need installing.
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

# CLEAN DATA -------------------------------------------
# remove observations with NA values
df.aq <- aq[complete.cases(aq), ]

df.aq$Full <- factor(df.aq$Month, levels = 1:12, labels = month.name)

names(df.aq)[1]<-"id"

# BASIC STATS -------------------------------------------

print('Summary Statistics')
print('Mean of Ozone')
mean(df.aq$Ozone)
print('Median of Ozone')
median(df.aq$Ozone)

print('Standard Deviation of Wind')
sd(df.aq$Wind)
sd.wind <- sqrt(var(df.aq$Wind))

maxtemp <- max(df.aq$Temp)
selection <- df.aq[df.aq$Temp == maxtemp, ]
print('Day when temperature was highest')
print(paste('was on the ',selection$Day, ' of ', selection$Month.name, sep = ""))

# VISUALISE SOME PLOTS -------------------------------------------

# plot a correlation matrix for the main variables
Cmatrix <-  cor(df.aq[,2:5]) 
# corrplot(Cmatrix)


# draw boxplot of the 4 measurements, scaling all values to between 0 and 1.
normalise <- function(x, na.rm = TRUE) {
  ranx <- range(x, na.rm = na.rm)
  (x - ranx[1]) / diff(ranx)
}
scaled <- as.data.frame(apply(df.aq[, c(2,3,4,5)], 2, normalise))

par(mfrow=c(1,4))
par(oma = c(4, 4, 4, 4)) # make room (i.e. the 4's) for the overall x and y axis titles
solar.label = 'Solar Radiation (lang)' 
temp.label <- 'Max. Daily Temperature (°f)'
ozone.label <- 'Ozone Concentration (ppb)'
wind.label <- 'Avg. Wind Speed (mph)'

boxplot(df.aq$Ozone, main=ozone.label) 
boxplot(df.aq$Solar.R, main=solar.label ) 
boxplot(df.aq$Wind, main=wind.label) 
boxplot(df.aq$Temp, main=temp.label)

mtext('Air Quality Measurements', side = 2, outer = TRUE, line = 2)



## Show Some Relationships with ggplot2 
title <- 'New York, Summer 1973 Air Quality Data'

col.red = "#dd0000"

g <- ggplot(data = df.aq, aes(x = Temp, y = Ozone))
g <- g + geom_point(size=2, colour=col.red)

g <- g + ggtitle(title)
g <- g + xlab(temp.label) + ylab(ozone.label)
print(g)

g <- ggplot(data = df.aq, aes(x = Wind, y = Temp, col=Ozone)) + labs(col='Mean\nOzone\nConcentration\n(ppb)') 
g <- g + geom_point(size=5, alpha=0.9) + scale_colour_continuous(low = "#0077aa", high = "#ff0000")
g <- g + facet_wrap(~Full)

g <- g + theme(legend.background = element_rect(fill="lightblue",
                                       size=0.5, linetype="solid", 
                                       colour ="darkblue"))

g <- g + ggtitle(title)
g <- g + xlab(wind.label) + ylab(temp.label)
print(g)






# FIT REGRESSION LINE -------------------------------------------


fitline <- lm(Ozone ~ Temp, data = df.aq)
print(summary(fitline))

# confidence ribbon represents the standard error
g <- ggplot(df.aq, aes(x = Temp, y = Ozone)) + geom_point(size=1.25) + stat_smooth(method = "lm")
g <- g + ggtitle(paste("Linear Regression for Ozone and Temperature" )) + ylab(ozone.label) + xlab(temp.label)

regress.stats <- paste( "\nAdjusted R2 (coeff. of determination) = ", signif(summary(fitline)$adj.r.squared, 5),
                        "\nY-Intercept =", signif(fitline$coef[[1]],5 ),
                        "  Slope =", signif(fitline$coef[[2]], 5) )
g <- g + annotate(geom="text", x=65, y=140, label=regress.stats,
                  color="dark blue")
print(g)

print(anova(fitline))

fitline <- lm(Ozone ~ Wind, data = df.aq)
print(summary(fitline))
# confidence ribbon represents the standard error
g <- ggplot(df.aq, aes(x = Wind, y = Ozone)) + geom_point(size=1.25) + stat_smooth(method = "lm")
g <- g + ggtitle(paste("Linear Regression for Ozone and Wind" )) + ylab(ozone.label) + xlab(wind.label)

regress.stats <- paste( "\nAdjusted R2 (coeff. of determination) = ", signif(summary(fitline)$adj.r.squared, 5),
                        "\nY-Intercept =", signif(fitline$coef[[1]],5 ),
                        "  Slope =", signif(fitline$coef[[2]], 5) )
g <- g + annotate(geom="text", x=9, y=140, label=regress.stats,
                  color="dark blue")
print(g)
print(anova(fitline))

# multiple regression
fitline <- lm(Ozone ~ Temp + Wind, data = df.aq)
print(anova(fitline))


# CHECK FOR A NORMAL DISTRIBUTION -------------------------------------------


g <- ggplot(df.aq, aes(sample=df.aq$Wind)) + stat_qq(color="#22cc22", size=4, alpha=0.5) + 
  geom_abline(intercept = mean(df.aq$Wind), slope = sd(df.aq$Wind), linetype="dashed")
g <- g + labs(title="Quantile quantile plot \n of Wind vs Normal distribution", x = "Theoretical Quantiles (Normal)",
     y = "Sample (Wind)")
print(g)

wind = df.aq$Wind
png('histfdnormal.png')
wind.histo = hist(wind, 50, freq=F)

wind.ylim.normal = range(0, wind.histo$density, dnorm(wind, mean = mean(wind), sd = sd(wind)))
nbins <- nclass.FD(wind) # use Freedman-Diaconis method
hist(wind, nbins, freq=F,  xlim=c(-1, 25), ylim = wind.ylim.normal, xlab = 'Wind (mph)', ylab = 'Probability',
     main = 'Histogram of Wind\nCompared to Normal PDF', border="dark blue", 
     col="light yellow", las=1)

# construct a synthetic normal distribution.
# supply the mean and sd from Wind data to align.
curve(dnorm(x, mean = mean(wind), sd = sd(wind)), 0, 20, add=T, col='blue') 
dev.off()
z=mean(wind)

