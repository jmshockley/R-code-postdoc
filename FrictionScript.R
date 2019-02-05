library(lubridate)
library(ggplot2)
filename=readline(prompt="Enter filename with file extension: ")

TestDatajunk<-read.csv2(filename, header=FALSE, blank.lines.skip=FALSE)
x<-which(TestDatajunk$V1 == 'EOF')
if(length(x)==0) {x<-which(TestDatajunk$V1 == 'EOF,,,,')}
if(length(x)==0) {x<-which(TestDatajunk$V1 == 'EOF,,,,,')}
rm(TestDatajunk)

if (length(x)>1) testnumber<-as.numeric(readline(prompt=paste0("There are currently ", length(x), " wear tests in this file. Enter which test number to use: ")))




if(length(x)==1 || testnumber==1) TestData<-read.csv(filename, colClasses=c("numeric",
                                       "numeric",
                                       "numeric",
                                       "character"), 
                          skip=9, nrows=x[1]-11, header =FALSE) else TestData<-read.csv(filename, colClasses=c("numeric",
                                                                                                                 "numeric",
                                                                                                                 "numeric",
                                                                                                                 "character"), 
                                                                                          skip=(x[testnumber-1]+8), nrows=x[testnumber]-x[testnumber-1]-9, header =FALSE)


if(length(x)==1 || testnumber==1) TestParameters=read.csv(filename, 
                        colClasses=c("numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric",
                                     "numeric"), 
                        skip=6, nrows=1) else TestParameters=read.csv(filename, 
                                                                      colClasses=c("numeric",
                                                                                   "numeric",
                                                                                   "numeric",
                                                                                   "numeric",
                                                                                   "numeric"), 
                                                                      skip=(x[testnumber-1]+4), nrows=1)

Sweep_mm=as.numeric(TestParameters[1])
Speed_mm_s<-as.numeric(TestParameters[2])
NumberofCycles=as.numeric(TestParameters[4])
rm(TestParameters)

colnames(TestData) <- c("Position(mm)",
                 "COF",
                 "Cycle#",
                 "F_or_R")
TestData$`Position(mm)`=TestData$`Position(mm)`/1000


TestData$PrevForR <- c("NA", TestData$F_or_R[-length(TestData$F_or_R)])
CycleChanges<-which(TestData$F_or_R != TestData$PrevForR)
TestData$PrevForR=NULL



CycleChangesdf=data.frame(CycleChanges)
CycleChangesdf$Cycle=seq(0.5, as.numeric(NumberofCycles), by =0.5)
rm(CycleChanges)

v=which.min(TestData$`Position(mm)`)
u=TestData$`Position(mm)`[v]
rm(v)
TestData$`Position(mm)`=TestData$`Position(mm)`-u
rm(u)

#TestData$POS=0.030
#for (i in 2:nrow(TestData)) {TestData$'POS'[i]=TestData$'Position(mm)'[i]-TestData$'Position(mm)'[i-1]}
x=which(TestData$'F_or_R'==' F')
TestData$`Cycle#`[x]=TestData$`Cycle#`[x]-0.5
y=which(TestData$'F_or_R'==' R')

#for (i in 1:nrow(TestData)) {if(TestData$POS[i]<0){TestData$'POS'[i]=-TestData$'POS'[i]}}


TestData$NumberofCycles=NumberofCycles
TestData$'Speed(mm/s)'=Speed_mm_s
TestData$'Sweep(mm)'=Sweep_mm

TestData$'SEQPOS'=NA
TestData$'SEQPOS'[x]=TestData$`Position(mm)`[x]+2*Sweep_mm*(TestData$`Cycle#`[x]-0.5)
TestData$'SEQPOS'[y]=2*Sweep_mm*(TestData$`Cycle#`[y]-1)+2*Sweep_mm-TestData$`Position(mm)`[y]

TestData$POSjunk=c(0, TestData$'SEQPOS'[-length(TestData$'SEQPOS')])
TestData$POS=TestData$SEQPOS-TestData$POSjunk
TestData$POSjunk=NULL

makeActiveBinding("data1", function() TestData, .GlobalEnv)
makeActiveBinding("data2", function() CycleChangesdf, .GlobalEnv)

#--------------------------------------
#   _____       ______  ______ _       _   
#  / __  \      |  _  \ | ___ \ |     | |  
#  `' / /'______| | | | | |_/ / | ___ | |_ 
#    / / |______| | | | |  __/| |/ _ \| __|
#  ./ /___      | |/ /  | |   | | (_) | |_ 
#  \_____/      |___/   \_|   |_|\___/ \__|
#                                          
#                                          

v={ggplot(TestData, aes(x=`Cycle#`, y=`POS`, fill=`COF`)) +
  geom_bar(stat="identity")  +
  scale_fill_continuous(low="blue", high="pink", limits=c(0.4,0.6))  +
  ggtitle(paste0("test ", testnumber, " of ", filename))  +
  theme(plot.title = element_text(lineheight=.8, face="bold"))}
v


t=paste0("TestData_test", testnumber, "_", strtrim(filename, nchar(filename)-4))
assign(t, TestData)
write.csv(TestData,paste0(t, ".csv"))
t=paste0("CycleChanges_test", testnumber, "_", strtrim(filename, nchar(filename)-4))
assign(t, CycleChangesdf)
write.csv(CycleChangesdf,paste0(t, ".csv"))

#+scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint=0.4, limits=c(0.2, 0.5))
#+ scale_x_continuous(breaks=seq(0, 100, 1))
#+coord_cartesian(xlim=c(0, 25))

#----------------------------------------------
#   __       ______  ______ _       _   
#  /  |      |  _  \ | ___ \ |     | |  
#  `| |______| | | | | |_/ / | ___ | |_ 
#   | |______| | | | |  __/| |/ _ \| __|
#  _| |_     | |/ /  | |   | | (_) | |_ 
#  \___/     |___/   \_|   |_|\___/ \__|
#                                       
#                                       
CycleChangesdf=CycleChanges_test1_2507_003_run4
FirstCycle=1
LastCycle=5
a=FirstCycle
b=LastCycle+0.5
x=data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]:CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]
y=data1$'COF'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]:CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]

plot(x,y,type="n", ylab= "Friction Coefficient", xlab="Cumulative Position (mm)", xlim=c(data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]],data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]), ylim=c(0, 0.65))
lines(x,y)

ablines=data1$SEQPOS[CycleChangesdf$CycleChanges[1:NumberofCycles]]
abline(v=ablines)

for (i in a:(b+0.5)) {text((data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-1]]+data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-2]])/2, 0.05, paste0(i-0.5,"F"))}
for (i in a:(b+0.5)) {text((data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-1]]+data1$SEQPOS[CycleChangesdf$CycleChanges[2*i]])/2, 0.025, paste0(i-0.5,"R"))}
text(0.75*data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]], 0.65, paste0("test ", testnumber, " of ", deparse(substitute(TestData_test1_2507_003_run4))), col = "black")

# save(`CycleChanges_test1_2507_003 run4`, "CycleChanges_test1_2507_003 run4.Rda")
