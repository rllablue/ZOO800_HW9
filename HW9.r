####################
### ZOO800: HW 9 ###
####################

# Author: Rebekkkah LaBlue
# Topic: Probability & Distributions
# Due: November 3, 2025

### --- SET UP --- ###

library(tidyverse)
library(dplyr)
library(magrittr)
library(stats)
library(ggplot2)
library(viridis)


###################
### OBJECTIVE 1 ###
###################

### --- A --- ###

# FUNCTION #
Simulate_lreg <- function(sims_n = 100, alpha = 13, beta = 7, sigma_vals = c(1, 10, 25)) {
  
  lr_results <- data.frame( # empty long-form df to store linear regression results
    x = numeric(0),
    y = numeric(0),
    sigma = factor()
  )
  
  for (s in sigma_vals) {
    x <- runif(n = sims_n, min = 0, max = 10) # sample from uniform distribution (for each sigma P(outcome) is the constant across sims)
    epsilon <- rnorm(n = sims_n, mean = 0, sd = s) # sample from normal distribution
    y <- alpha + beta * x + epsilon # structure regression equation
  
  
    lr_results <- rbind( # assign actual values from sims to empty df
      lr_results, 
      data.frame(
        x = x,
        y = y,
        sigma = factor(s)
      )
    )
  }
  
  return(lr_results)
}

# APPLY #
set.seed(333)
lr_data <- Simulate_lreg()
View(lr_data)



### --- B --- ###

facet_labels <- c(
  "1" = "Sigma = 1",
  "10" = "Sigma = 10",
  "25" = "Sigma = 25"
)

lr_plot <- ggplot(lr_data, aes(x = x, y = y)) +
  geom_point(color = "black", alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "orange") + 
  facet_wrap(~ sigma, nrow = 1, labeller = as_labeller(facet_labels)) +
  theme_bw(base_size = 14) +
  labs(
    x = "Predictor (x)",
    y = "Response (y)",
    caption = "Figure 1. Change in the discernability of the relationship between the independent and dependent variables in linear regression when varying sigma across simulations (n = 100)."
  ) + 
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 15))
  )
  
lr_plot



### --- C --- ###

### An observer loses the ability to discern the relationship between x and y as 
# error increases due to the large spread of points that fail to coalese around the 
# regression line; x's parameter estimate will be uninformative and the relationship 
# between the dependent and independent variables appears more random than it really is.



###################
### OBJECTIVE 2 ###
###################

### --- A & B --- ###

# FUNCTION #

### Approach: Three different levels of 'randomness' to be modeled, ie. nested for-loops, 
# where 'deeper' loops define automated processes that need to run/varied more frequently: 
# Outer: major experimental conditions of interest; Middle: sub-conditions; 
# Inner: individual repetitions, often involve (random) responses to avg across.

# How often can I detect a coin is unfair (proportion out of # of sims_n) [Inner] for
# different numbers of flips (flips_n) [Middle] and different coins with different loaded 
# probabilities of heads v tails (prob_vals) [Outer].

Simulate_binom <- function(sims_n = 100, flips_n = 1:20, prob_vals = c(0.55, 0.6, 0.65), alpha = 0.05){
  
  binom_results <- data.frame(prob = numeric(0), # empty long-form df to store function results
                              n_flips = numeric(0),
                              bias_det = numeric(0))
  
  for (p in prob_vals) { # Outer loop: perform all below for each coin w/ set P(outcomes)
    
    for (f in flips_n) { # Middle loop: sub-samples, ie. numbers of flips 1:20 for each coin
      outcomes_results <- numeric(sims_n) # vector to store outcomes for each sim (length = 100) for each sub-sample size
      
      for (s in 1:sims_n) { # Inner loop: simulate/flip coin n_sims times for each sub-sample flip size, record T/F
        flips <- rbinom(n = f, size = 1, prob = p) # flip one coin (size = 1) to total of sub-sample size f
        p_val <- binom.test(sum(flips), n = f, p = 0.5, alternative = "greater")$p.value # specifications for one-sided H0 v HA test (want to know if p > 0.5 / outcome biased)
                                                                                     # sum heads outcomes from flips; extract p-value from trial
        outcomes_results[s] <- (p_val < alpha) # assign outcomes from each sim to numeric vector from above, assigning T/F where T/1 = p > 0.5 (unfair)
                                               # ie. inside: 100 values, either 0 or 1, for one combination of prob_vals and sub-sample size f
      }
      
      binom_results <- rbind(
        binom_results, # assign acutal values to empty df
        data.frame(
         prob = p,
         n_flips = f,
        bias_det = mean(outcomes_results) # in binom context mean = proportion
        )
      )
    }
  }

  return(binom_results)
}
  

# APPLY #

set.seed(333)
binom_data <- Simulate_binom()
View(binom_data)


# PLOT # 

binom_plot <- ggplot(binom_data, aes(x = n_flips, y = bias_det, color = as.factor(prob))) +
  geom_line(linewidth = 1, alpha = 0.7) +
  geom_point(color = "black", alpha = 0.5) +
  theme_bw(base_size = 14) +
  labs(
    x = "Coin Flips",
    y = "Probability of Detecting Bias (p < 0.05)",
    color = "Coin Bias (p)",
    caption = "Figure 1. Probability of detecting the bias of a loaded coin across 20 flips (simulations = 100); each line denotes a set outcome of heads (p) on three different coins. "
  ) +
  scale_color_viridis_d(option = "plasma") +
  theme(
    axis.title.x = element_text(margin = margin(t = 15)),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.position = "right",
    legend.text = element_text(size = 11),
    legend.key.size = unit(1, "cm"),
    plot.caption = element_text(hjust = 0.5, size = 9, margin = margin(t = 15))
  )

binom_plot