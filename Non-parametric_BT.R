
# Libraries ---------------------------------------------------------------

library(tidyverse)
library(igraph)
library(softImpute)

# Re-scale odds so that probabilities add to one --------------------------

df <- read_csv("Group stage odds 10Jun.csv")%>%
  rename("odds_1" = "1", "odds_2" = "2")%>%
  mutate(p_ij = 1 / odds_1,
         p_draw = 1 / X,
         p_ji = 1/ odds_2,
         total_prob = p_ij + p_draw + p_ji)%>%
  mutate(p_ij = p_ij / total_prob,
         p_draw = p_draw / total_prob,
         p_ji = p_ji / total_prob)%>%
  select(-total_prob)%>%
  mutate(graph_p_ij = p_ij + p_draw / 2,
         graph_p_ji = p_ji + p_draw / 2)


# Produce weighted adjacency matrix ---------------------------------------


g <- df%>%select(TeamName1, TeamName2, graph_p_ij)%>%setNames(c("From", "To", "weight"))%>%
  rbind(
    df%>%select(TeamName2, TeamName1, graph_p_ji)%>%setNames(c("From", "To", "weight"))
  )%>%
  graph.data.frame()
X <- as_adjacency_matrix(g, attr = "weight")%>%
  as.matrix()


# Just do regular matrix completion ---------------------------------------

X <- ifelse(X == 0, NA, X)
# USVT suggested taking six singular values so let's try that as rank
set.seed(123)
sv <- softImpute(X, rank.max = 6, lambda = 1, maxit = 1000)
lambda0(X)
M_hat <- complete(X, sv)
# symmetrize so that p_ij = 1 - p_ji
R <- M_hat + t(M_hat)
M_final <- ifelse(M_hat == 0, M_hat, M_hat / R)

ranks <- M_final%>%rowSums()%>%as.data.frame()%>%
  rownames_to_column()%>%
  setNames(c("country", "strength"))%>%
  arrange(desc(strength))


########### THIS PART FAILED

# Fit BT ------------------------------------------------------------------

# number nodes
n <- nrow(X)

# force values of X to lie in [-1,1]
X <- (X - 0.5) * 2
# diag(X) <- 0

# SVD
sv <- svd(X)
sv$d
# fraction of observed matches, that is, fraction of non-zero entries above the diagonal
# this is 24 * 3 / 2 = 24 teams, three entries per team, div by 2, since we're only looking
# at upper triangular part
p_hat <- n*3/2 / (choose(n,2) + n)
# q_hat
q_hat <- p_hat*0.25 + p_hat * (1 - p_hat) * 0.75
threshold <- 2 * sqrt(n*q_hat)
threshold
S <-  sv$d[sv$d >= threshold]
S
# calculate estimator
t <- length(S) # number of singular values we take
W <- 1/p_hat * sv$u[,1:t] %*% diag(S) %*% t( sv$v[, 1:t])


# truncate anything above 1 or below -1
M_tilde <- ifelse(W > 1, 1,
                  ifelse(W < 0, 0, W))
# transform back
M_hat <- M_tilde * 0.5 + 0.5
# symmetrize
R <- M_hat + t(M_hat)
M_final <- ifelse(M_hat == 0, M_hat, M_hat/R)



max(abs(M_tilde + t(M_tilde) - 1))
R <- M_tilde + t(M_tilde)
M_hat <- ifelse(M_tilde == 0, M_tilde, M_tilde/R)
M_hat_plus <- (M_hat*0.5) + 0.5

a_test <- sv$u[,1:10] %*% diag(sv$d[1:10]) %*% t(sv$v[,1:10])
a_test <- ifelse(a_test > 1e-15, a_test, 0)
