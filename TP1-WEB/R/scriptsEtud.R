# Pour les questions délciates

options(OutDe=",")

questDelicateMeth1 <- function(n, P, theta, n.sim, p.neg=0)
{P.chap <- rep(NA,n.sim)
P <- (1-p.neg)*P
for(i in 1:n.sim){
  # Choix de la question
  question <- sample(c(1,2),n,prob=c(theta,1-theta),replace=TRUE)
  # Choix du répondant
  repondant <- sample(c(1,2),n,prob=c(P,1-P),replace=TRUE)
  #
  nb.oui <- sum(question-repondant==0)
  P.chap[i] <- (nb.oui/n+theta-1)/(2*theta-1)
}
return(P.chap=P.chap)
}
questDelicateMeth2 <- function(n, P, theta, alpha, n.sim, p.neg=0)
{P.chap <- rep(NA,n.sim)
P <- (1-p.neg)*P
for(i in 1:n.sim){
  # Choix de la question
  question <- sort(sample(c(1,2),n,prob=c(theta,1-theta),replace=TRUE))
  # La question étant choisie, à qui s'adresse-t-elle ?
  nb.1 <- sum(question==1)
  nb.oui <- sum(sample(c(1,2),nb.1,prob=c(P,1-P),replace=TRUE)==1)+
    sum(sample(c(1,2),n-nb.1,prob=c(alpha,1-alpha),replace=TRUE)==1)
  P.chap[i] <- (nb.oui/n-(1-theta)*alpha)/(theta)
}
return(P.chap=P.chap)
}
#
# Étude exhaustive d'un estimateur dans une population finie
#
options(OutDec = ",")
pop <- c(3, 6, 24, 27, 30, 36, 51, 57)
N <- length(pop)
n <- 3
(n.ech <- choose(N,n))
ech.all <- cbind(choix <- t(matrix(combn(1:N,n),n,n.ech)),(matrix(pop[choix],n.ech,n)))	
rownames(ech.all) <- seq(1:nrow(ech.all))
colnames(ech.all) <- c(paste("i",1:n,sep=""),paste("pop[i",1:n,"]",sep=""))
head(ech.all)
#
# Pour les pointures de chaussure
#
# Descriptif de population
# Valeurs
y <- c(36,  36.5 , 37,  37.5 , 38,  38.5 , 39,  39.5 , 40,  40.5 , 41,  41.5 , 42,  42.5 , 43,  43.5 , 44,  44.5 , 45,  45.5 , 46,  46.5)
# Probabilités
e <- c(0.02, 0.03, 0.04, 0.06, 0.10, 0.12, 0.13, 0.10, 0.07, 0.05, 0.05, 0.04, 0.04, 0.03, 0.03, 0.02, 0.02, 0.01, 0.01, 0.01, 0.01, 0.01)
# Un échantillon
echt <- sample(y,n,replace=TRUE,prob=e)