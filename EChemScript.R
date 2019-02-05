filename=readline(prompt="Enter filename with file extension: ")

linestoskip=readline(prompt="Enter number of lines to skip (e.g. 57 for pstat, 62 for chronoamp): ")

A=read.table(filename, 
             colClasses=c("numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "numeric",
                          "character"), 
             skip=linestoskip, nrows=213128)
colnames(A) <- c("Pt(#)",
                 "T(s)",
                 "Vf(VvsRef)",
                 "Im(A)",
                 "Vu(V)",
                 "Sig(V)",
                 "Ach(V)",
                 "IERange(#)",
                 "Overbits")
rm(linestoskip)

TrackLength<- as.numeric(readline(prompt="Enter wear track length ('sweep', mm): "))
SlidingVelocity<- as.numeric(readline(prompt="Enter sliding velocity (mm/s): "))
NumberofCycles<- as.numeric(readline(prompt="Number of sliding cycles: "))
SlidingStart<- as.numeric(readline(prompt="Enter time in electrochem data that sliding began (s): "))
AveragingInterval<- as.numeric(readline(prompt="Enter number of datapoints for moving average: "))

freq<-rep(1/AveragingInterval, AveragingInterval)
ImAvg<- filter(A[,4], freq,sides=2)

A$ImAvg<- ImAvg

HalfCycles=1:2*NumberofCycles
HalfCycles=HalfCycles/2

BeginningofSliding=1

while (A[BeginningofSliding,2]<SlidingStart) {BeginningofSliding=BeginningofSliding+1}
#repeat {i=i+1; if (A[2,i]>SlidingStart) {break } else i=i+1} 
#for(i in 1:dim(A)[1]) #{ }

TotalSlidingTime=TrackLength/SlidingVelocity*NumberofCycles*2
EndofSliding=BeginningofSliding
while (A[EndofSliding,2]<(TotalSlidingTime+A[BeginningofSliding,2])) {EndofSliding=EndofSliding+1}