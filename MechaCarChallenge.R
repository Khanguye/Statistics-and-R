library(tidyverse)
#######MPG Regression########

#Open and read data in the dataframe table
MechaCar_mpg_table <- read.csv(file='MechaCar_mpg.csv',check.names = F,stringsAsFactors = F)

#view columns
colnames(MechaCar_mpg_table)

#Change columns without space
MechaCar_mpg_table <- MechaCar_mpg_table %>% rename(
   veh_length = "vehicle length" ,
   veh_weight = "vehicle weight" ,
   spoiler_angle = "spoiler angle" ,
   grd_clearance = "ground clearance" 
)

#peak data 6 rows
head(MechaCar_mpg_table)

#generate multiple linear regression model
#mile per gallon (mpg) is dependent variable
#vehicle length (veh_length), vehicle weight (veh_weight), spoiler angle (spoiler_angle) and ground clearance (grd_clearance) are independent variables

lm(mpg ~ veh_length + veh_weight + spoiler_angle + grd_clearance, data = MechaCar_mpg_table) 
#Coefficients:
#(Intercept)     veh_length     veh_weight  spoiler_angle  grd_clearance  
#-1.076e+02      6.240e+00      1.276e-03      8.031e-02      3.659e+00  
#mpg = 6.240(veh_length) + 0.001(veh_weight) + 0.08(spoiler_angle) + 3.66(grd_clearance) - 107.6

#generate summary statistics
summary(lm(mpg ~ veh_length + veh_weight + spoiler_angle + grd_clearance, data = MechaCar_mpg_table)) 
#Coefficients:
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   -1.076e+02  1.576e+01  -6.823 1.87e-08 ***
#veh_length     6.240e+00  6.609e-01   9.441 3.05e-12 ***
#veh_weight     1.277e-03  6.948e-04   1.837   0.0728 .  
#spoiler_angle  8.031e-02  6.656e-02   1.207   0.2339    
#grd_clearance  3.659e+00  5.394e-01   6.784 2.13e-08 ***
#vehicle length (veh_length) and ground clearance (grd_clearance) as well as intercept are statistically unlikely to provide random amounts of variance to the linear model.
#The vehicle weight and the spoiler angle factor won't contribute in the linear model.
#Recommend: countinous colleting the vehicle weight and the spoiler angle data becuase this data set is small.

######Suspension Coil Summary######

#Open and read data in the dataframe table
Suspension_Coil_table <- read.csv(file='Suspension_Coil.csv',check.names = F,stringsAsFactors = F)

#Calculate the mean,median,variance, and standard deviation
metrics <- Suspension_Coil_table %>% summarize(mean = mean(PSI), median= median(PSI), variance = var(PSI), std = sd(PSI) )
metrics
#Out put
#mean     median  variance   std
#1498.78  1500    62.29356   7.892627

ggplot(aes(x=PSI),data=Suspension_Coil_table)+geom_boxplot()
tbl <- subset(Suspension_Coil_table,PSI<1510 & PSI>1490)

######Suspension Coil T-Test######

Suspension_Coil_sample_table <- Suspension_Coil_table %>% sample_n(50) #randomly sample 50 data points

t.test((Suspension_Coil_sample_table$PSI),mu=mean(Suspension_Coil_table$PSI)) #compare sample versus population means

######MPG vs AWD#######
# Is there any statistical different in the Mile per Gallon of Mechacar base on its All Wheel Drive?
##AoV test
MechaCar_mpg_filter <- MechaCar_mpg_table[,c("mpg","AWD")] #filter columns from mtcars dataset
MechaCar_mpg_filter$AWD <- factor(MechaCar_mpg_filter$AWD) #convert numeric column to factor
aov(mpg ~ AWD,data=MechaCar_mpg_filter) #compare means across multiple levels
summary(aov(mpg ~ AWD,data=MechaCar_mpg_filter))
##AoV test
mpg_data <- read.csv('mpg_modified.csv') #import dataset
mpg_data_filter <- mpg_data[,c("hwy","drv")]
mpg_data_filter$drv <- factor(mpg_data$drv) 
aov(hwy ~ drv,data=mpg_data_filter) #compare means across multiple levels
summary(aov(hwy ~ drv,data=mpg_data_filter))

