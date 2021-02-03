##
## Pour l'exo 1
##
yi <- c(42, 41, 36, 32, 29, 27, 23, 19, 16, 15, 14, 11, 10, 9, 7, 6, 5, 4, 3)
ei <- c(23, 4, 1, 1, 1, 2, 1, 1, 2, 2, 1, 1, 1, 1, 1, 3, 2, 1, 1)
ech <- rep(yi,ei)
##
## Pour l'exo 2
##
prof.ech <- read.table("Donnees/tabA02.txt",header=TRUE,encoding="utf8")
N <- 200
n <- 50
#
# Pour le premier domaine
#
ybar <- mean(prof.ech$Sal01)
y.20 <- prof.ech$Sal01*(prof.ech$Exp<=20)
iD.20 <- (prof.ech$Exp<=20)
ND.20 <- 24
ybar1 <- mean(y.20[iD.20])
sd1 <- sd(y.20[iD.20])
sd1.prime <- sd(y.20)
ybar1.prime <- mean(y.20)
nd1 <- length(y.20[iD.20])
#
# Pour le second domaine
y.30 <- prof.ech$Sal01*(prof.ech$Exp<=30)
iD.30 <- (prof.ech$Exp<=30)
ND.30 <- 114
ybar2 <- mean(y.30[iD.30])
sd2 <- sd(y.30[iD.30])
sd2.prime <- sd(y.30)
ybar2.prime <- mean(y.30)
nd2 <- length(y.30[iD.30])
#
# A vous de le faire en vous inspirant du code pour le premier domaine
#
##
## Pour l'exo 3
##
N <- 250
y <- 0:8
# effectif
e <- c(14, 3, 2, 4, 0, 0, 0, 1, 1)
y <- rep(y,e)
##
## Pour l'exo 4
##
#
# Script 2
#
pop.y <- c(8, 10, 67, 44, 66, 56, 89, 99)
pop.x <- c(3, 6, 24, 27, 30, 36, 51,  57)
N <- length(pop.x)
n <- 3
(n.ech <- choose(N,n))
(ybar.pop <- mean(pop.y))
(xbar.pop <- mean(pop.x))
(R <- ybar.pop / xbar.pop )
ech.all <- cbind(choix <- t(matrix(combn(1:N,n),n,n.ech)),
                 (matrix(pop.y[choix],n.ech,n)),(matrix(pop.x[choix],n.ech,n)))	
rownames(ech.all) <- seq(1:nrow(ech.all))
colnames(ech.all) <- c(paste("i",1:n,sep=""), paste("y[i",1:n,"]",sep=""),
                       paste("x[i",1:n,"]",sep=""))
head(ech.all)
stats <- data.frame(
  ybar=apply(ech.all[,(n+1):(2*n)],MARGIN=1,FUN=mean),
  xbar=apply(ech.all[,(2*n+1):(3*n)], MARGIN=1,FUN=mean))
R.chap <- stats[,"ybar"]/stats[,"xbar"]
stats <- data.frame(stats,	R.chap)
head(stats)