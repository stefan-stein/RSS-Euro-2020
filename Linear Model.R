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

lmfit <- lm(logit_p_ij ~ -1 + X, data = df)

r <- coef(lm_fit)
r[is.na(r)] <- 0
r


# Check that setting to zero is correct thing to do --------------

#Looking at just Group A and going back to basics on linear regression

df_A <- subset(df, Group == "A",)

df_A$X <- makeX_lm(df_A)

X <- df_A$X

XTX <- t(X)%*%X

y <- df_A$logit_p_ij

XTy <- t(X)%*%y

## XTX is not of full rank so replace first line with identifiability constraint

XTX[1,] <- c(1,1,1,1)

XTy[1] <- 0

s_A <- solve(XTX, XTy)

s_A <- s_A - s_A[4]
s_A # these match what we got from lmfit

## Just to check if truly colinear then shouldn't matter which row the identifiability constraint is imposed on

XTX <- t(X)%*%X

XTy <- t(X)%*%y

XTX[4,] <- c(1,1,1,1)

XTy[4] <- 0

s_A <- solve(XTX, XTy)

s_A <- s_A - s_A[4]
s_A # these match what we got from lmfit with NAs subbed to 0 and from previous calc - we are all good!

# ----------------------------------------------------------------

probs <- outer(r, r, function(x,y){plogis(x - y)})
dimnames(probs) <- list(i = colnames(df$X), j = colnames(df$X))
probs

## But these are not the right strengths to be using to calculate these probabilities 
## because the group probabilities have not been scaled


# scale the Group probabilities ----------------------------



