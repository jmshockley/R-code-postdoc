setwd("C:\\users\\jmshockley\\Work Folders\\Documents\\AFM\\2017-03-09 2507 I13\\2017-03-09 2507_gridded\\10V 1-7\\10V 1-7 Height Movie Gwyddion")

library(tidyverse)
library(caret)
library(glmnet)

filename="3_Current_movie_10V, first 7 frames cropped for differential.txt"
TD<-read.table(filename, header=FALSE, blank.lines.skip=FALSE, skip = 3)
colnames(TD)<-c("X", "Y","Current1")

TDtemp<-read.table("5_Current_movie_10V, first 7 frames cropped for differential.txt", header=FALSE, blank.lines.skip=FALSE, skip = 3)
TD$Current2=TDtemp$V3

TDtemp<-read.table("7_Current_movie_10V, first 7 frames cropped for differential.txt", header=FALSE, blank.lines.skip=FALSE, skip = 3)
TD$Current3=TDtemp$V3

TDtemp<-read.table("3-1_Height_movie_10V_corrected, first 7 frames differential.txt", header=FALSE, blank.lines.skip=FALSE, skip = 3)
TD$Height1=TDtemp$V3

TDtemp<-read.table("5-3_Height_movie_10V_corrected, first 7 frames differential.txt", header=FALSE, blank.lines.skip=FALSE, skip = 3)
TD$Height2=TDtemp$V3

TDtemp<-read.table("7-5_Height_movie_10V_corrected, first 7 frames differential.txt", header=FALSE, blank.lines.skip=FALSE, skip = 3)
TD$Height3=TDtemp$V3
rm(TDtemp)

TestDatasig<-read.table("5_Current_movie_10V, first 7 frames sigma mask.txt", header=FALSE, blank.lines.skip=FALSE)
TD$sigma=TestDatasig$V3

TestDatasig<-read.table("5_Current_movie_10V, first 7 frames sec aus mask.txt", header=FALSE, blank.lines.skip=FALSE)
TD$secaus=TestDatasig$V3

TestDatasig<-read.table("5_Current_movie_10V, first 7 frames cr2n mask.txt", header=FALSE, blank.lines.skip=FALSE)
#TD$cr2n=TestDatasig$V3
TD$cr2n=0

TestDatasig<-read.table("5_Current_movie_10V, first 7 frames chi mask.txt", header=FALSE, blank.lines.skip=FALSE)
TD$chi=TestDatasig$V3
rm(TestDatasig, filename)
TD$Xpos_um=TD$X*10^6
TD$Ypos_um=TD$Y*10^6

TD$Current1=TD$Current1*10^9
TD$Current2=TD$Current2*10^9
TD$Current3=TD$Current3*10^9
TD$Height1_nm=TD$Height1*10^9
TD$Height2_nm=TD$Height2*10^9
TD$Height3_nm=TD$Height3*10^9
TD$sum=TD$sigma+TD$secaus+TD$cr2n+TD$chi
TD=subset(TD, TD$sum==1)
TD= subset(TD, select = -sum)

TD$Fe_wtpct=65.3*TD$secaus+56.5*TD$sigma+54.2*TD$chi+0*TD$cr2n
TD$Cr_wtpct=22.2*TD$secaus+30.56*TD$sigma+26.6*TD$chi+88*TD$cr2n
TD$Mo_wtpct=3.6*TD$secaus+7.8*TD$sigma+13.3*TD$chi+0*TD$cr2n
TD$Ni_wtpct=8.23*TD$secaus+4.22*TD$sigma+4.43*TD$chi+0*TD$cr2n
TD$Si_wtpct=0.63*TD$secaus+0.52*TD$sigma+0.6*TD$chi+0*TD$cr2n
TD$Mn_wtpct=0.26*TD$secaus+0.38*TD$sigma+0.43*TD$chi+0*TD$cr2n


Current1mean=aggregate(TD$Current1, by=list(TD$Y), FUN=sum)
Current1sd=aggregate(TD$Current1, by=list(TD$Y), FUN=sd)
Fe_wtpct_mean=aggregate(TD$Fe_wtpct, by=list(TD$Y), FUN=mean)
Cr_wtpct_mean=aggregate(TD$Cr_wtpct, by=list(TD$Y), FUN=mean)
Mo_wtpct_mean=aggregate(TD$Mo_wtpct, by=list(TD$Y), FUN=mean)
Ni_wtpct_mean=aggregate(TD$Ni_wtpct, by=list(TD$Y), FUN=mean)
Si_wtpct_mean=aggregate(TD$Si_wtpct, by=list(TD$Y), FUN=mean)
Mn_wtpct_mean=aggregate(TD$Mn_wtpct, by=list(TD$Y), FUN=mean)

Height1mean=aggregate(TD$Height1_nm, by=list(TD$Y), FUN=mean)
Height1sd=aggregate(TD$Height1_nm, by=list(TD$Y), FUN=sd)

Current1mean$Height=-Height1mean$x
Current1mean$Current=Current1mean$x
Current1mean$Fe_wtpct_mean=Fe_wtpct_mean$x
Current1mean$Cr_wtpct_mean=Cr_wtpct_mean$x
Current1mean$Mo_wtpct_mean=Mo_wtpct_mean$x
Current1mean$Ni_wtpct_mean=Ni_wtpct_mean$x
Current1mean$Si_wtpct_mean=Si_wtpct_mean$x
Current1mean$Mn_wtpct_mean=Mn_wtpct_mean$x


#ggplot(Current1mean, aes(`Height`, `Current`))+geom_point(size=1)
  
myvars <- names(Current1mean) %in% c("Height", "Current", "Fe_wtpct_mean", "Cr_wtpct_mean", "Mo_wtpct_mean", "Ni_wtpct_mean", "Si_wtpct_mean", "Mn_wtpct_mean") 
TDformodel <- Current1mean[myvars]
TDformodel=subset(TDformodel, TDformodel$Height>0.25)

set.seed(123)
training.samples <- createDataPartition(TDformodel$Current,times=1, p = 0.8, list = FALSE)
train.data  <- TDformodel[training.samples, ]
test.data <- TDformodel[-training.samples, ]

alpha=1 #0 for ridge, 1 for lasso
x <- model.matrix(Current~., train.data)[,-1]
y <- train.data$Current
glmnet(x, y, alpha=alpha, lambda = NULL)

# Find the best lambda using cross-validation
set.seed(123) 
cv <- cv.glmnet(x, y, alpha = alpha)
# Display the best lambda value
cv$lambda.min

# Fit the final model on the training data
model <- glmnet(x, y, alpha = alpha, lambda = cv$lambda.min)
# Display regression coefficients
print(coef(model))

# Make predictions on the test data
x.test <- model.matrix(Current~., test.data)[,-1]
predictions <- model %>% predict(x.test) %>% as.vector()
# Model performance metrics
performance=data.frame(
  RMSE = RMSE(predictions, test.data$Current),
  Rsquare = R2(predictions, test.data$Current)
)


