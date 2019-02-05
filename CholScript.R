#Load the data
chol <- read.table(url("http://assets.datacamp.com/blog_assets/chol.txt"), header = TRUE)
#Calculate some statistics for the chol dataset
#library(Rmisc)
#cholc <- summarySE(chol,
#                   measurevar="CHOL",
#                  groupvars=c("MORT","SMOKE"))
#Plot the data
plot(cholc$N,
     cholc$CHOL,
     ylim=range(c(cholc$CHOL-cholc$sd, cholc$CHOL+cholc$sd)),
     pch=19,
     xlab="Cholesterol Measurements",
     ylab="Cholesterol Mean +/- SD",
     main="Scatterplot With sd Error Bars"
)

#Draw arrows of a "special" type
arrows(cholc$N,
       cholc$CHOL-cholc$sd,
       cholc$N,
       cholc$CHOL+cholc$sd,
       length=0.05,
       angle=90,
       code=3)