options(OutDe=",")
pop <- c (3 , 6 , 24 , 27 , 30 , 36 , 51 , 57)
N <- length ( pop )
n <- 3
(n.ech <- choose (N , n))
ech.all <- cbind ( choix <- t ( matrix ( combn (1: N , n ) ,n , n.ech ) ) ,( matrix (pop [ choix ] , n.ech , n ) ) )
rownames (ech.all ) <- seq (1: nrow ( ech.all ) )
colnames (ech.all ) <- c ( paste ( " i " ,1:n , sep = " " ) , paste ( " pop [ i " ,1:n , " ] ", sep = " " ) )
head (ech.all)

