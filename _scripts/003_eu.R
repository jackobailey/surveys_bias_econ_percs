# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 3: National Economic Perceptions (EU Ref)

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
# we need to perform this analysis: the treatments by past voting behaviour,
# the subjects' responses to the retrospective economic item, and also how
# they voted at the 2016 referendum on EU membership.

dta <- 
  dta %>% 
  select(
    torysplit,
    othersplit,
    dnvsplit,
    eu = pastvote_EURef,
    economy
  )


# First, we need to create a variable that records what side each subject
# supported at the Brexit referendum. We'll call this variable "r".

dta <- 
  dta %>% 
  mutate(
    r =
      eu %>% 
      as_factor() %>% 
      mark_na(c("Can’t remember")) %>% 
      fct_recode(
        "Remain" = "I voted to Remain",
        "Leave" = "I voted to Leave",
        "Nonvoter" = "I did not vote",
      ) %>% 
      factor(levels = c("Nonvoter", "Leave", "Remain"))
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
      as_factor(ordered = T) %>% 
      mark_na("Don't know")
  )


# Finally, we'll clean up a little by selecting only those variables that
# we need to run our analysis ('p', 't', and 'f') and omit any missing
# cases list-wise.

dta <- 
  dta %>% 
  select(r, t, e) %>% 
  na.omit()



# 3. Fit model ------------------------------------------------------------

# We'll fit an ordered probit model that can tell if or how the treatment
# affects the national economic perceptions that our subjects report after
# whatever type of survey they received.

m3 <- 
  brm(
    formula =
      bf(e ~ t*r) +
      lf(disc ~ 0 + t*r, cmc = FALSE),
    family =
      cumulative(link = "probit",
                 link_disc = "log"),
    prior = 
      prior(normal(0, 1), class = "Intercept") +
      prior(normal(0, 0.25), class = "b") +
      prior(normal(0, 0.25), class = "b", dpar = "disc"),
    data = dta,
    inits = 0,
    iter = 2e3,
    chains = 4,
    cores = 4,
    file = here("_output", "m3")
  )


# Now, we'll check that the chains have converged

plot(m3, ask = F)


# Then conduct a posterior predictive check

pp_check(m3, type = "bars")


# And then check the model for any pathological behaviour

check_hmc_diagnostics(m3$fit)



# 4. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "003_eu.txt"))


# One last thing...

thanks()

