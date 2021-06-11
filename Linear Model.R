# Libraries ---------------------------------------------------------------
library(tidyverse)
library(igraph)


# Re-scale odds so that probabilities add to one --------------------------
df <- read_csv("Group odds 10Jun (inc Group).csv")%>%
  rename("odds_1" = "1", "odds_draw" = "X", "odds_2" = "2")%>%
  mutate(p_ij = 1 / odds_1,
         p_draw = 1 / odds_draw,
         p_ji = 1/ odds_2,
         total_prob = p_ij + p_draw + p_ji)%>%
  mutate(p_ij = p_ij / total_prob,
         p_draw = p_draw / total_prob,
         p_ji = p_ji / total_prob)%>%
  select(-total_prob)%>%
  mutate(logit_p_ij = log(p_ij / p_ji))

df

# Create adjacency matrix ------------------------------------

makeX_lm <- function(df){
  X_lm <- matrix(0,
                 nrow(df),
                 length(levels(factor(as.character(df$TeamName1)))))
  colnames(X_lm) <- levels(factor(as.character(df$TeamName1)))
  for (team in colnames(X_lm)) {
    X_lm[df$TeamName1 == team, team] <- 1
    X_lm[df$TeamName2 == team, team] <- -1
  }
  return(X_lm)
}

df$X <- makeX_lm(df)




# Fit linear model ---------------------------------------

lmfi_groupo <- lm(logit_p_ij ~ -1 + X, data = df)

r <- coef(lmfit_group)
r[is.na(r)] <- 0
r
s <- exp(r)


# Calc probs ------ leaving code here for now but to be moved -----

r <- setNames(r, colnames(df$X))
probs <- plogis(outer(r, r, '-'))
probs

## But these are not the right strengths to be using to calculate these probabilities 
## because the group probabilities have not been scaled


# scale the Group probabilities ----------------------------

# Under Bradley-Terry the probability of i winning the final given i has made it to the final is 
# q_io = \pi_i / (\pi_i + \pi_o) where \pi_i is the strength of team i and \pi_o is the strength of the final opponent.
# \pi_o is assumed to be the same for all teams (since there are so many permutations of reaching final)
# so logit(q_io) = lambda_i - lambda_o (1) where lambda_i = log(\pi_i).
# We have the intra-group strengths s_i, but we do not know the relative strengths of the groups.
# Suppose group G has strength \gamma_G such that s_i = \gamma_G(i) \pi_i where G(i) is i's group.
# Then lambda_i = log(\pi_i) = log(s_i) - log(\gamma_G(i)) = r_i - lambda_G(i) (2) where lambda_G(i) = log(\gamma_G(i))
# So combining (1) and (2) we have r_i - logit(q_io) = lambda_G(i) + \lambda_o.
# So we estimate \lambda_o and the \lambda_G by regressing against r_i - logit(q_io) 


# take tournament win odds and translate to tournament win odds

winner <- read_csv("Winner.csv")%>%
  mutate(p_i = 1 / Odds)
total_prob <- sum(winner$p_i)
winner$p_i <- winner$p_i/total_prob

# take tournament 'reaching final' odds and translate to 'tournament 'reaching final' odds 

final <- read_csv("making_final.csv")%>%
  mutate(p_i = 1 / Odds)
total_prob <- sum(final$p_i)
final$p_i <- 2*final$p_i/total_prob

# calculate the conditional probability of winning given you make the final

winner$q_io <- winner$p_i/final$p_i

y <- r - qlogis(winner$q_io)

X <- matrix(0,
               nrow(winner),
               length(levels(factor(as.character(winner$Group)))))
colnames(X) <- levels(factor(as.character(winner$Group)))
rownames(X) <- winner$TeamName
for (group in colnames(X)) {
  X[winner$Group == group, group] <- 1
}
X

lmfit_win <- lm(y ~ X)

lambda <- coef(lmfit_win)
lambda[is.na(lambda)] <- 0
lambda


