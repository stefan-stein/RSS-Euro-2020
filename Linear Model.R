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

df

# Fit linear model ---------------------------------------

lm_fit <- lm(logit_p_ij ~ X, data = df)

summary(lm_fit)

r <- coef(lm_fit)
r[is.na(r)] <- 0



