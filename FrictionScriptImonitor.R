#Read Filename
filename=readline(prompt="Enter filename with file extension: ")

#Open the file and quickly find how many tests are on it
TestDatajunk<-read.csv2(filename, header=FALSE, blank.lines.skip=FALSE)
x<-which(TestDatajunk$V1 == 'EOF')
if(length(x)==0) {x<-which(TestDatajunk$V1 == 'EOF,,,,')}
if(length(x)==0) {x<-which(TestDatajunk$V1 == 'EOF,,,,,')}
rm(TestDatajunk)

#TestData dataframe is created, reading only the desired tests according to user input.
if (length(x)>1) testnumber<-as.numeric(readline(prompt=paste0("There are currently ", length(x), " wear tests in this file. Enter which test number to use: "))) else testnumber=1

if(length(x)==1 || testnumber==1) TestData<-read.csv(filename, colClasses=c("numeric",
                                       "numeric",
                                       "numeric",
                                       "character",
                                       "numeric",
                                       "character"), 
                          skip=9, nrows=x[1]-11, header =FALSE) else TestData<-read.csv(filename, colClasses=c("numeric",
                                                                                                                 "numeric",
                                                                                                                 "numeric",
                                                                                                                 "character",
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


#Name the columns in #TestData
colnames(TestData) <- c("Position(mm)",
                 "COF",
                 "Cycle#",
                 "F_or_R",
                 "Current",
                 "Timestamp")


#Convert m to mm in position column. Normalize all position data to the lowest position value.
TestData$`Position(mm)`=TestData$`Position(mm)`/1000
v=which.min(TestData$`Position(mm)`)
u=TestData$`Position(mm)`[v]
rm(v)
TestData$`Position(mm)`=TestData$`Position(mm)`-u
rm(u)


#Find the cycle changes and record them in CycleChanges dataframe.
TestData$PrevForR <- c("NA", TestData$F_or_R[-length(TestData$F_or_R)])
CycleChanges<-which(TestData$F_or_R != TestData$PrevForR)
TestData$PrevForR=NULL
CycleChangesdf=data.frame(CycleChanges)
CycleChangesdf$Cycle=seq(0.5, as.numeric(NumberofCycles), by =0.5)
rm(CycleChanges)


#On forward halfcycles, subtract cycle# by 0.5
x=which(TestData$'F_or_R'==' F')
TestData$`Cycle#`[x]=TestData$`Cycle#`[x]-0.5
y=which(TestData$'F_or_R'==' R')


#Record test parameters into dataframe so it lives forever
TestData$NumberofCycles=NumberofCycles
TestData$'Speed(mm/s)'=Speed_mm_s
TestData$'Sweep(mm)'=Sweep_mm


#Define sequential positions ("SEQPOS") column which is the total sliding distance throughout the test
TestData$'SEQPOS'=NA
TestData$'SEQPOS'[x]=TestData$`Position(mm)`[x]+2*Sweep_mm*(TestData$`Cycle#`[x]-0.5)
TestData$'SEQPOS'[y]=2*Sweep_mm*(TestData$`Cycle#`[y]-1)+2*Sweep_mm-TestData$`Position(mm)`[y]


#Define incremental positions ("POS") column. Used for 2D spatial friction/current plots.
TestData$POSjunk=c(0, TestData$'SEQPOS'[-length(TestData$'SEQPOS')])
TestData$POS=TestData$SEQPOS-TestData$POSjunk
TestData$POSjunk=NULL


#Create "data1" and "data2" as shortcuts to TestData. Probably not needed.
makeActiveBinding("data1", function() TestData, .GlobalEnv)
makeActiveBinding("data2", function() CycleChangesdf, .GlobalEnv)


#Create "TestDataReordered" which is only necessary for 2D plotting.
TestDataReordered=TestData
for (i in 1:(nrow(CycleChangesdf)/2-1)) {
  TestDataReordered[CycleChangesdf$CycleChanges[2*i]:CycleChangesdf$CycleChanges[2*i+1],]=TestData[CycleChangesdf$CycleChanges[2*i+1]:CycleChangesdf$CycleChanges[2*i],]}
i=nrow(CycleChangesdf)/2
TestDataReordered[CycleChangesdf$CycleChanges[2*i]:nrow(TestData),]=TestData[nrow(TestData):CycleChangesdf$CycleChanges[2*i],]
TestDataReordered$Current=-TestDataReordered$Current
#--------------------------------------
#   _____       ______  ______ _       _   
#  / __  \      |  _  \ | ___ \ |     | |  
#  `' / /'______| | | | | |_/ / | ___ | |_ 
#    / / |______| | | | |  __/| |/ _ \| __|
#  ./ /___      | |/ /  | |   | | (_) | |_ 
#  \_____/      |___/   \_|   |_|\___/ \__|
#                                          
#                                          

v={ggplot(TestDataReordered, aes(x=`Cycle#`, y=`POS`, fill=`COF`)) +
    geom_bar(stat="identity", width=0.5)  +
    scale_fill_continuous(low="black", high="red", limits=c(0,1))  +
    ggtitle(paste0("test ", testnumber, " of ", filename))  +
    theme(plot.title = element_text(lineheight=.8, face="bold"))}

w={ggplot(TestDataReordered, aes(x=`Cycle#`, y=`POS`, fill=`Current`)) +
    geom_bar(stat="identity", width=0.5)  +
    scale_fill_continuous(low="black", high="green", limits=c(0,0.5))  +
    ggtitle(paste0("test ", testnumber, " of ", filename))  +
    theme(plot.title = element_text(lineheight=.8, face="bold"))}
w

TestDataReorderedF<-TestDataReordered[!(floor(TestDataReordered$`Cycle#`) == TestDataReordered$`Cycle#`), ]
wF={ggplot(TestDataReorderedF, aes(x=`Cycle#`, y=`POS`, fill=`Current`)) +
           geom_bar(stat="identity", width=1)  +
            scale_fill_continuous(low="black", high="green", limits=c(-0.3,0.3))  +
           ggtitle(paste0("test ", testnumber, " (forward direction) of ", filename))  +
            theme(plot.title = element_text(lineheight=.8, face="bold"))}

TestDataReorderedR<-TestDataReordered[floor(TestDataReordered$`Cycle#`) == TestDataReordered$`Cycle#`, ]
wR={ggplot(TestDataReorderedR, aes(x=`Cycle#`, y=`POS`, fill=`Current`)) +
    geom_bar(stat="identity", width=1)  +
    scale_fill_continuous(low="black", high="green", limits=c(-0.3,0.3))  +
    ggtitle(paste0("test ", testnumber, " (reverse direction) of ", filename))  +
    theme(plot.title = element_text(lineheight=.8, face="bold"))}



t=paste0("TestData_test", testnumber, "_", strtrim(filename, nchar(filename)-4))
assign(t, TestData)
write.csv(TestData,paste0(t, ".csv"))
t=paste0("TestDataReordered_test", testnumber, "_", strtrim(filename, nchar(filename)-4))
assign(t, TestDataReordered)
write.csv(TestDataReordered,paste0(t, ".csv"))
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
#CycleChangesdf=CycleChanges_test1_2507_003_run4
FirstCycle=0
LastCycle=100
a=FirstCycle
b=LastCycle+0.5
x=data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]:CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]
y=data1$'COF'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]:CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]
z=-data1$Current[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]:CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]


plot(x,y,type="n", ylab= "Friction Coefficient", xlab="Cumulative Position (mm)", xlim=c(data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==a]],data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]]), ylim=c(0, 0.65))
lines(x,y, col="red")
lines(x,z, col="green")
abline(v=ablines)



#ablines=data1$SEQPOS[CycleChangesdf$CycleChanges[1:NumberofCycles]]
#ablines=data1$SEQPOS[CycleChangesdf$CycleChanges[1:100]]
abline(v=ablines)

for (i in a:(b+0.5)) {text((data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-1]]+data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-2]])/2, 0.05, paste0(i-0.5,"F"))}
for (i in a:(b+0.5)) {text((data1$SEQPOS[CycleChangesdf$CycleChanges[2*i-1]]+data1$SEQPOS[CycleChangesdf$CycleChanges[2*i]])/2, 0.025, paste0(i-0.5,"R"))}
#text(0.75*data1$'SEQPOS'[CycleChangesdf$CycleChanges[CycleChangesdf$Cycle==b]], 0.65, paste0("test ", testnumber, " of ", deparse(substitute(TestData_test1_2507_003_run4))), col = "black")

# save(`CycleChanges_test1_2507_003 run4`, "CycleChanges_test1_2507_003 run4.Rda")
