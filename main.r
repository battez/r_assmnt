# Luke Barker
# load dataset, retrieving if necessary.
# For more: https://stat.ethz.ch/R-manual/R-devel/library/datasets/html/airquality.html
if(!file.exists("airquality.csv") ){
  
  # R has this dataset built in usually but for completeness' sake:
  download.file("https://vincentarelbundock.github.io/Rdatasets/csv/datasets/airquality.csv"
                  , destfile = "airquality.csv")
}
aq <- read.table("airquality.csv", header=TRUE, 
                   sep=",")

# import libs used:
if("corrplot" %in% rownames(installed.packages()) == FALSE) {install.packages("corrplot")}
library(corrplot) # package may need installing.
if("ggplot2" %in% rownames(installed.packages()) == FALSE) {install.packages("ggplot2")}
library(ggplot2)

# CLEAN DATA -------------------------------------------
# remove observations with NA values
df.aq <- aq[complete.cases(aq), ]

df.aq$Full <- factor(df.aq$Month, levels = 1:12, labels = month.name)

names(df.aq)[1]<-"id"

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

boxplot(scaled$Ozone, main='Ozone Concentration' ) 
boxplot(scaled$Solar.R, main='Solar Radiation' ) 
boxplot(scaled$Wind, main='Average Wind Speed') 
boxplot(scaled$Temp, main='Temperature')
mtext('Air Quality Measurements (normalised)', side = 2, outer = TRUE, line = 2)

title <- 'New York, Summer 1973 Air Quality Data'
temp.label <- 'Max. Daily Temperature (°f)'
col.red = "#dd0000"
## Some Relationships
g <- ggplot(data = df.aq, aes(x = Temp, y = Ozone))
g <- g + geom_point(size=2, colour=col.red)

g <- g + ggtitle(title)
g <- g + xlab(temp.label) + ylab('Mean Ozone Concentration (ppb)')
print(g)

g <- ggplot(data = df.aq, aes(x = Wind, y = Temp, col=Ozone)) + labs(col='Mean\nOzone\nConcentration\n(ppb)') 
g <- g + geom_point(size=5, alpha=0.9) + scale_colour_continuous(low = "#0077aa", high = "#ff0000")
g <- g + facet_wrap(~Full)

g <- g + theme(legend.background = element_rect(fill="lightblue",
                                       size=0.5, linetype="solid", 
                                       colour ="darkblue"))

g <- g + ggtitle(title)
g <- g + xlab('Avg. Wind Speed (mph)') + ylab(temp.label)
print(g)
















