TestData$COFfilter=as.numeric(filter(TestData$COF,rep(1/10,10), sides=2))
TestData$Currentfilter=as.numeric(filter(TestData$Current,rep(1/10,10), sides=2))

TestData$COFjunkminus=c(NA, TestData$COFfilter[-length(TestData$COFfilter)])
TestData$COFjunkplus=c(TestData$COFfilter[-1], NA)
TestData$Currentjunkminus=c(NA, TestData$Currentfilter[-length(TestData$Currentfilter)])
TestData$Currentjunkplus=c(TestData$Currentfilter[-1], NA)

TestData$POSjunkminus=c(NA, TestData$POS[-length(TestData$POS)])
TestData$POSjunkplus=c(TestData$POS[-1], NA)

TestData$COFdiff=0.5*((TestData$COFjunkplus-TestData$COFfilter)/(TestData$POSjunkplus)+(TestData$COFfilter-TestData$COFjunkminus)/(TestData$POS))
TestData$COFjunkminus=NULL
TestData$COFjunkplus=NULL

TestData$Currentdiff=0.5*((TestData$Currentjunkplus-TestData$Currentfilter)/(TestData$POSjunkplus)+(TestData$Currentfilter-TestData$Currentjunkminus)/(TestData$POS))
TestData$Currentjunkminus=NULL
TestData$Currentjunkplus=NULL

TestData$COFdiffjunkminus=c(NA, TestData$COFdiff[-length(TestData$COFdiff)])
TestData$COFdiffjunkplus=c(TestData$COFdiff[-1], NA)
TestData$Currentdiffjunkminus=c(NA, TestData$Currentdiff[-length(TestData$Currentdiff)])
TestData$Currentdiffjunkplus=c(TestData$Currentdiff[-1], NA)


TestData$COFdiff2=0.5*((TestData$COFdiffjunkplus-TestData$COFdiff)/(TestData$POSjunkplus)+(TestData$COFdiff-TestData$COFdiffjunkminus)/(TestData$POS))
TestData$Currentdiff2=0.5*((TestData$Currentdiffjunkplus-TestData$Currentdiff)/(TestData$POSjunkplus)+(TestData$Currentdiff-TestData$Currentdiffjunkminus)/(TestData$POS))

x={ggplot(TestData, aes(x=`Cycle#`, y=`POS`, fill=`COFdiff`)) +geom_bar(stat="identity", width=0.5)  +
    scale_fill_gradient2(low="red", mid="black", high="green", limits=c(-0.5,0.5))  +
    ggtitle(paste0("test ", testnumber, " of ", filename))  +
  theme(plot.title = element_text(lineheight=.8, face="bold"))}

y={ggplot(TestData, aes(x=`Cycle#`, y=`POS`, fill=`Currentdiff`)) +geom_bar(stat="identity", width=0.5)  +
    scale_fill_gradient2(low="red", mid="black", high="green", limits=c(-0.35,0.35))  +
    ggtitle(paste0("test ", testnumber, " of ", filename))  +
    theme(plot.title = element_text(lineheight=.8, face="bold"))}

i=49.5
a=ccf(TestData$COFdiff[which(TestData$`Cycle#`==i)], TestData$Currentdiff[which(TestData$`Cycle#`==i)], type = c("correlation"),  plot = TRUE, na.action = na.pass)


