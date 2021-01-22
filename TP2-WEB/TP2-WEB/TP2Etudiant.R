options(OutDec = ",")
require(xtable)
##
## Fonctions utiles
##
un_dedans <- function(x, no) {
  sum(x == no) >= 1
}
deux_dedans <- function(x, no1, no2) {
  as.logical((sum(x == no1) >= 1) * (sum(x == no2) >= 1))
}
##
## Exercice 1
##
##
## Une population de 6 unités
##
Pop <- c(98, 102, 154, 133, 190, 175)
#
# Plan 1
#
i1 <- rep(c(1, 2), each = 4)
i2 <- rep((rep(c(3, 4), each = 2)), 2)
i3 <- rep(c(5, 6), 4)
plan1 <- cbind(i1, i2, i3, p.ech = rep(1 / 8, 8))
rownames(plan1) <- 1:8
plan1
#
# Plan 2
#
plan2 <-
  cbind(
    i1 = c(1, 2, 1),
    i2 = c(4, 3, 3),
    i3 = c(6, 6, 5),
    p.ech = c(0.25, 0.50, 0.25)
  )
rownames(plan2) <- 1:3
plan2
#
# Exercice 2
#
##
## Une population de 8 unités
##
Pop <- c(1, 2, 4, 4, 7, 7, 7, 8)
#
# Plan
#
i1 <- c(1, 2, 1, 2, 4)
i2 <- c(3, 3, 4, 4, 5)
i3 <- c(5, 7, 6, 6, 7)
i4 <- c(6, 8, 8, 8, 8)
p.ech <- c(1 / 8, 1 / 4, 1 / 8, 3 / 8, 1 / 8)
plan <- cbind(i1, i2, i3, i4, p.ech)
rownames(plan) <- 1:5
plan
#
# Exercice 3
#
#
# Plan 4
#
Pop <- c(1, 2, 4, 4, 7, 7, 7, 8)
N <- length(Pop)
n <- 4
n.ech <- choose(N, n)
ech.all <-
  cbind(choix <-
          t(matrix(combn(1:N, n), n, n.ech)),
        (matrix(Pop[choix], n.ech, n)))
rownames(ech.all) <- seq(1:nrow(ech.all))
colnames(ech.all) <-
  c(paste("i", 1:n, sep = ""), paste("Pop[", 1:n, "]", sep = ""))
set.seed(101)
plan4 <- cbind(ech.all[sample(1:nrow(ech.all),20),][,1:4],pr_ech=1/20)
rownames(plan4) <- 1:20
plan4