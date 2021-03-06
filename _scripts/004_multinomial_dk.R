# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 4: National Economic Perceptions (Multinomial, Don't Knows)

# Jack Bailey
# University of Manchester
# jack.bailey@manchester.ac.uk



# 1. Housekeeping ---------------------------------------------------------

# The following code requires that you have first opened the associated
# project file. If not, click the button on the top right and open it
# ("Open Project...").

# Note also that the Bayesian methods I use below rely on having installed
# the probabilistic programming language Stan. If you have yet to install
# Stan, please see https://mc-stan.org/users/interfaces/


# Set random seed

set.seed(666)


# Load packages

library(tidyverse)
library(haven)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(rstan)
library(brms)
library(here)


# Load data

dta <- read_sav(here("_data", "surveys_bias_econ_percs.sav"))



# 2. Transform data -------------------------------------------------------

# Before we get started, let's select only the variables in the data that
# we need to perform this analysis: the treatments by past voting behaviour
# and the subjects' responses to the retrospective economic item.

dta <- 
  dta %>% 
  select(
    torysplit,
    othersplit,
    dnvsplit,
    economy
  )


# First, we need to create a variable that records what party each subject
# supported at the last election. As this was a part of the design, we can
# use the design-related variables to create this while also creating the
# treatment indicator. In line with the terminology in the paper, we'll
# call the party support/vote variable "p".

dta <- 
  dta %>% 
  mutate(
    p = 
      case_when(
        torysplit %in% 1:2 ~ "Inc",
        othersplit %in% 1:2 ~ "Opp",
        dnvsplit %in% 1:2 ~ "Non"
      ) %>% 
      factor(levels = c("Non", "Inc", "Opp"))
  )


# Likewise, we'll call the treatment variable "t".

dta <- 
  dta %>% 
  mutate(
    t =
      case_when(
        torysplit == 1 | othersplit == 1 | dnvsplit == 1 ~ 1,
        torysplit == 2 | othersplit == 2 | dnvsplit == 2 ~ 0
      ) %>% 
      factor(labels = c("Control", "Treatment"))
  )


# We also need to convert respondents' self-reported economic perceptions
# from a labelled numeric vector to an ordered factor and mark any subjects
# who responded "Don't know" as NA. Again, as in the paper, we'll rename
# this variable from "economy" to "e".

dta <- 
  dta %>% 
  mutate(
    e = 
      economy %>% 
      as_factor(ordered = T)
  )


# Finally, we'll clean up a little by selecting only those variables that
# we need to run our analysis ('p', 't', and 'e') and omit any missing
# cases list-wise.

dta <- 
  dta %>% 
  select(p, t, e) %>% 
  na.omit()



# 3. Fit model ------------------------------------------------------------

# We'll fit a multinomial logit model to check that the main findings are
# robust to model specification.

m4 <- 
  brm(
    formula = 
      bf(e ~ t*p),
    family =
      categorical(link = "logit",
                  refcat = "Stayed the same"),
    prior = 
      prior(normal(0, 1.5), class = "Intercept", dpar = "muGotalotbetter") +
      prior(normal(0, .5), class = "b", dpar = "muGotalotbetter") +
      prior(normal(0, 1.5), class = "Intercept", dpar = "muGotalittlebetter") +
      prior(normal(0, .5), class = "b", dpar = "muGotalittlebetter") +
      prior(normal(0, 1.5), class = "Intercept", dpar = "muGotalittleworse") +
      prior(normal(0, .5), class = "b", dpar = "muGotalittleworse") +
      prior(normal(0, 1.5), class = "Intercept", dpar = "muGotalotworse") +
      prior(normal(0, .5), class = "b", dpar = "muGotalotworse"),
    data = dta,
    iter = 2e3,
    chains = 4,
    cores = 4,
    file = here("_output", "m4")
  )


# Now, we'll check that the chains have converged

plot(m4, ask = F)


# Then conduct a posterior predictive check

pp_check(m4, type = "bars")


# And then check the model for any pathological behaviour

check_hmc_diagnostics(m4$fit)



# 4. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "005_multinomial_dk.txt"))


# One last thing...

thanks()

