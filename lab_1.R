# creating dataframes
days <- c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday')
temp <- c(24,23.5,18,22,20,23,19)
rained <- c(F, F, T, F, T, F, T)

help('data.frame')

RPI_weeklyweather <- data.frame(days,temp,rained)

RPI_weeklyweather
head(RPI_weeklyweather)

help(str)
help(summary)
str(RPI_weeklyweather)
summary(RPI_weeklyweather)

RPI_weeklyweather[1:7,]
RPI_weeklyweather[,2:3]
RPI_weeklyweather[3,'rained']
RPI_weeklyweather$temp
help(subset)
subset(RPI_weeklyweather, rained==TRUE)

help(order)
sorted.rain <- order(RPI_weeklyweather[,'temp'], decreasing = TRUE)
sorted.rain
sorted.rain <- order(RPI_weeklyweather[,'temp'], decreasing = FALSE)
sorted.rain

dec.rained <- order(-RPI_weeklyweather$temp) # descendind alone temp
dec.rained
remove(RPI_weeklyweather)

#empty data frame
empty_dataframe <- data.frame()
v1 <- c(2:19)
v2 <- seq(1:18)
v3 <- 1:7
empty_dataframe <- data.frame(v1)
empty_dataframe
cbind(empty_dataframe,v2)
# cbind(empty_dataframe,v3)  x

empty_dataframe <- data.frame()
ept_rnames <- letters[1:7]
empty_dataframe <- data.frame(LETTERS[1:7],row.names = ept_rnames)
empty_dataframe
names(empty_dataframe) = "cap"
remove(empty_dataframe)
rm(list = ls())

# exercise
library(MASS)

EPI_data <- read.csv('C:/Users/栗寻/Desktop/RPI/6600 Data Analytics/dataset/EPI/2010EPI_data.csv')
EPI_data
str(EPI_data)
View(EPI_data)

head(EPI_data)
names(EPI_data) <- c(EPI_data[1,])  # as.matrix() works as well
EPI_data <- EPI_data[-1,]
rownames(EPI_data) <- 1:nrow(EPI_data)
fix(EPI_data)
attach(EPI_data)

colnames(EPI_data)  

#-----
tf <- is.na(EPI)
EPI_filtered <-EPI[!tf]

EPI_data <- lapply(EPI_data, function(x) type.convert(as.character(x)))
#-----

summary(EPI_data$EPI)
fivenum(EPI_data$EPI)  # fivenum(EPI_data$EPI,na.rm=TRUE)
stem(EPI_data$EPI)
hist(EPI_data$EPI)
hist(EPI_data$EPI,seq(30,95,1),prob=TRUE, main = 'Histogram of EPI')  # breaks = seq()

?density
lines(density(EPI_data$EPI,na.rm = TRUE,bw=1))
?rug
rug(EPI_data$EPI)   # one-dim density plot



# exercise 1

#ECOSYSTEM
summary(EPI_data$ECOSYSTEM)
fivenum(EPI_data$ECOSYSTEM)
tf <- is.na(EPI_data$ECOSYSTEM)
tf
  # plot1
hist(EPI_data$ECOSYSTEM, seq(0,100,1),prob=TRUE,main ='Histogram of ecosystem index')
lines(density(EPI_data$EPI,na.rm=TRUE, bw=1))
rug(EPI_data$ECOSYSTEM)
  # plot 2
plot(ecdf(EPI_data$ECOSYSTEM), do.points=FALSE, verticals=TRUE, main = 'cdf-ECOSYSTEM INDEX')
  # q-q normal
par(pty='s')
qqnorm(EPI_data$ECOSYSTEM)
qqline(EPI_data$ECOSYSTEM)
  # try to fit other distributions
  #student?
qqplot(qt(ppoints(170),df = 8), EPI_data$ECOSYSTEM,
       xlab = 't-df=5',ylab = 'ECOSYSTEM',main = 'q-q student ECOSYSTEM')
qqline(EPI_data$ECOSYSTEM, distribution = function(p) qt(p,df=5),
       prob = c(.3,.7))


#AIR_H
summary(EPI_data$AIR_H)
fivenum(EPI_data$AIR_H)
tf <- is.na(EPI_data$AIR_H)
tf
# plot1
hist(EPI_data$AIR_H, seq(0,100,2),prob=TRUE,main ='Histogram of AIR_H index')
lines(density(EPI_data$AIR_H,na.rm=TRUE, bw=1))
rug(EPI_data$AIR_H)
# plot 2
plot(ecdf(EPI_data$AIR_H), do.points=FALSE, verticals=TRUE, main = 'cdf-AIR_H INDEX')
# q-q normal
par(pty='s')
qqnorm(EPI_data$AIR_H)
qqline(EPI_data$AIR_H, prob =c(.25,.75))


# comparing
boxplot(EPI_data$AIR_E,EPI_data$WATER_E)
boxplot(EPI_data$WATER_E,EPI_data$WATER_H)
qqplot(EPI_data$AIR_E,EPI_data$AIR_H)
qqplot(EPI_data$ECOSYSTEM,EPI_data$BIODIVERSITY)


# exercise 2

EPI_NR <- EPI_regions[!is.na(EPI_regions)]
head(EPI_regions)
EPI_r_noteu <- EPI_regions[EPI_regions!='Europe']

head(GEO_subregion)
GEO_subCaribbean <- GEO_subregion[GEO_subregion == 'Caribbean']






# GPw3

GPW3 <- read.csv('C:/Users/栗寻/Desktop/RPI/6600 Data Analytics/dataset/GPW3_GRUMP_SummaryInformation_2010.csv')
GPW3
str(GPW3)
View(GPW3)
head(GPW3)
fix(GPW3)
attach(GPW3)

colnames(GPW3)  

#-----
GPW3 <- lapply(GPW3, function(x) type.convert(as.character(x)))
#-----

summary(GPW3$PopulationPerUnit)
fivenum(GPW3$PopulationPerUnit)
tf <- is.na(GPW3$PopulationPerUnit)

hist(GPW3$PopulationPerUnit, seq(0,2700,50),prob=TRUE,main ='Histogram of Population Per Unit')
lines(density(GPW3$PopulationPerUnit,na.rm=TRUE, bw=1))
rug(GPW3$PopulationPerUnit)
plot(ecdf(GPW3$PopulationPerUnit), do.points=FALSE, verticals=TRUE, main = 'cdf-Population Per Unit')

summary(GPW3$Largest.Extent..sq.km.)
Largest.Extent_sq.km. <- GPW3$Largest.Extent..sq.km.[!is.na(GPW3$Largest.Extent..sq.km.)]
boxplot(GPW3$PopulationPerUnit, as.numeric(Largest.Extent_sq.km.))




#water
wat_t <- read.csv('C:/Users/栗寻/Desktop/RPI/6600 Data Analytics/dataset/water-treatment.csv')
wat_t
str(wat_t)
View(wat_t)
head(wat_t)
fix(wat_t)
attach(wat_t)

colnames(wat_t)  

#-----
GPW3 <- lapply(wat_t, function(x) type.convert(as.character(x)))
#-----

summary(wat_t$PH.E)
fivenum(wat_t$PH.E)
tf <- is.na(wat_t$PH.E)

hist(wat_t$PH.E, seq(6.5,9,.1),prob=TRUE,main ='Histogram of PH.E')
lines(density(wat_t$PH.E,na.rm=TRUE, bw=1))
rug(wat_t$PH.E)
plot(ecdf(wat_t$PH.E), do.points=FALSE, verticals=TRUE, main = 'cdf-PH.E')

par(pty='s')
qqnorm(wat_t$PH.E)
qqline(wat_t$PH.E, prob =c(.25,.75))


DBO.E_rmna <- wat_t$DBO.E[wat_t$DBO.E!='?']
DBO.E_rmna
boxplot(wat_t$PH.P, wat_t$PH.E)
