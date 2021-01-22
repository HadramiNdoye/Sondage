# Realisation d'un script creant un echantillon de taille 5 1000 fois
y <- c (36 , 36.5 , 37 , 37.5 , 38 , 38.5 , 39 , 39.5 , 40 , 40.5,41 ,41.5 , 42 , 42.5 , 43 , 
        43.5 , 44 , 44.5 , 45 , 45.5 , 46 ,46.5)
e <- c (0.02 , 0.03 , 0.04 , 0.06 , 0.10 , 0.12 , 0.13 , 0.10 , 0.07 , 0.05 , 0.05 , 0.04 , 0.04, 
        0.03 , 0.03 , 0.02 , 0.02 , 0.01 , 0.01 , 0.01 , 0.01 ,  0.01)
N <- 1000
n <- 5
echt <- matrix(rep(NA,N),nrow = n,ncol = N)
for (i in 1:N){
  echt[,i] <- sample (y ,n , replace = TRUE , prob = e )
}

