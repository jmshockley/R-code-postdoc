FnSeq <- c(0.25,0.5,1,2,5,10)
muSeq<-0#c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
for (mu in muSeq) {
  for (Fn in FnSeq) {
    source('Hamilton_x.R')
  }
}
