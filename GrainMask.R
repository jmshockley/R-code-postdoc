filenameZeroCurrent="zeromask_CURRENT_MOVIE_60.txt" #readline(prompt="Enter zero CURRENT mask filename with file extension: ")
ZeroCurrentMask<-read.table(filenameZeroCurrent)
filenameZeroHeight="zeromask_HEIGHT_MOVIE_59.txt" #readline(prompt="Enter zero HEIGHT mask filename with file extension: ")
ZeroHeightMask<-read.table(filenameZeroHeight)

AllFrameHeights=data.frame(ZeroHeightMask$V1)
colnames(AllFrameHeights)[1]="X"
AllFrameHeights$Y=ZeroHeightMask$V2

AllFrameCurrent=data.frame(ZeroHeightMask$V1)
colnames(AllFrameCurrent)[1]="X"
AllFrameCurrent$Y=ZeroHeightMask$V2

filenameGrain="GrainlargeMask_MOVIE_59.txt" #readline(prompt="Enter grain mask filename with file extension: ")
GrainMask<-read.table(filenameGrain)

framenumber=6


for (i in 0:framenumber) {
  t=paste0(i, "_CURRENT_MOVIE_60.txt")
  Grain<-read.table(t, skip = 4)
  dx=Grain$V1[2]-Grain$V1[1]
  
  LevelZeroCurrent=as.numeric(sum(Grain$V3[which(ZeroCurrentMask$V3==1)]))/as.numeric(sum(ZeroCurrentMask$V3))
  Grain$NormCurrent=Grain$V3-LevelZeroCurrent
  
  AllFrameCurrent$NormCurrent=Grain$NormCurrent
  colnames(AllFrameCurrent)[i+3]=paste0("C", i)
  
  Integral<-as.numeric(sum(Grain$NormCurrent[which(GrainMask$V3==1)]))*(dx*dx)
  
  write.table(Integral, file=paste0("IntegralCurrent_", filenameGrain), append=TRUE, row.names=FALSE, col.names=FALSE)
  
  
  
  t=paste0(i, "_HEIGHT_MOVIE_59.txt")
  Grain<-read.table(t, skip = 4)
  dx=Grain$V1[2]-Grain$V1[1]
  
  LevelZeroHeight=as.numeric(sum(Grain$V3[which(ZeroHeightMask$V3==1)]))/as.numeric(sum(ZeroHeightMask$V3))
  Grain$NormHeight=Grain$V3-LevelZeroHeight
  
  AllFrameHeights$NormHeight=Grain$NormHeight
  colnames(AllFrameHeights)[i+3]=paste0("H", i)
  
  Integral<-as.numeric(sum(Grain$NormHeight[which(GrainMask$V3==1)]))*(dx*dx)
  
  write.table(Integral, file=paste0("IntegralHeight_", filenameGrain), append=TRUE, row.names=FALSE, col.names=FALSE)
  
}

AllFrameCurrent$Csum <- rowSums(AllFrameCurrent[, c(3:length(AllFrameCurrent))])
AllFrameHeights[GrainMask$V3 ==0 , 3:length(AllFrameHeights)] <- 0
AllFrameCurrent[GrainMask$V3 ==0 , 3:length(AllFrameCurrent)] <- 0


write.table(AllFrameCurrent, file=paste0("AllFrameCurrent", filenameGrain), append=FALSE, row.names=FALSE, col.names=TRUE)
write.table(AllFrameHeights, file=paste0("AllFrameHeights", filenameGrain), append=FALSE, row.names=FALSE, col.names=TRUE)