# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 13: BES Bias Plot

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
library(magrittr)
library(haven)
library(lubridate)
library(jbmisc)
library(brms)
library(tidybayes)
library(here)


# Load BESIP data and select variables

dta <- 
  read_dta(here::here("_data", "BES2019_W19_Panel_v0.3.dta.zip")) %>% 
  select(
    id,
    num_range("starttimeW", c(1:4, 6:8, 10:17)),
    num_range("econGenRetroW", c(1:4, 6:8, 10:17)),
    num_range("partyIdW", c(1:4, 6:8, 10:17))
  )



# 2. Transform BESIP data -------------------------------------------------

# The BESIP data take a little more work because they're panel data and
# initially in a wide-format. We need to convert them instead to long-format
# to get them to work.

# First, we'll rename the economic perception items so that they are the
# same as the cms version. We'll do the same with party id too.

names(dta)[2:16] <- paste0("date_", c(1:4, 6:8, 10:17))

names(dta)[17:31] <- paste0("econ_", c(1:4, 6:8, 10:17))

names(dta)[32:46] <- paste0("pid_", c(1:4, 6:8, 10:17))


# Next, we'll convert the data from wide- to long-format so that it reflects
# the structure of the cms data. This will also make it easier to transform
# the variables.

dta <- 
  dta %>% 
  pivot_longer(cols = -id, 
               names_to = c(".value", "group"),
               names_sep = "_")


# Next, we'll convert the econ variable to an ordered factor

dta <- 
  dta %>% 
  mutate(
    econ =
      econ %>% 
      as_factor(ordered = T) %>% 
      mark_na("Don't know")
  )


# Next, we'll do the same for party id

dta <- 
  dta %>% 
  mutate(
    pid =
      pid %>% 
      as_factor() %>% 
      mark_na("Don't know") %>% 
      fct_recode(
        "Con" = "Conservative",
        "Lab" = "Labour",
        "Lib" = "Liberal Democrat",
        "Oth" = "Scottish National Party (SNP)",
        "Oth" = "Plaid Cymru",
        "Oth" = "United Kingdom Independence Party (UKIP)",
        "Oth" = "Green Party",
        "Oth" = "British National Party (BNP)",
        "Oth" = "Other",
        "Oth" = "Change UK- The Independent Group",
        "Oth" = "Brexit Party",
        "Non" = "No - none"
      )
  )

dta <- 
  dta %>% 
  mutate(
    ion =
      case_when(
        # Code NAs as NA
        is.na(pid) == T ~ "NA",
        
        # Maintain codes for non-identifiers
        pid == "Non" ~ "Non",
        
        # Recode those who support an incumbent party as "Inc"
        date < "2015-05-07" & pid %in% c("Con", "Lib") ~ "Inc",
        date >= "2015-05-07" & pid == "Con" ~ "Inc",
        
        # Recode those who don't support an incumbent party as "Opp"
        date < "2015-05-07" & !(pid %in% c("Con", "Lib")) ~ "Opp",
        date >= "2015-05-07" & !(pid == "Con") ~ "Opp",
      ) %>% 
      factor(levels = c("Non", "Inc", "Opp", "NA")) %>% 
      mark_na("NA")
  )


# Now we need to rename the time variable to survey so that it matches the
# cms data. We'll also add "wave" in front of the wave number

dta <- 
  dta %>% 
  rename(survey = group) %>% 
  mutate(survey = paste("wave", survey))


# Finally, we'll remove any missing data list-wise

dta <- 
  dta %>% 
  na.omit()



# 4. Fit model ------------------------------------------------------------

# Now we'll fit a similar model to the experimental model to each wave of
# data. The only difference is that there is no treatment variable. 

for(i in c(1:4, 6:8, 10:17)){
  
  brm(
    formula = 
      bf(econ ~ ion) +
      lf(disc ~ 0 + ion, cmc = FALSE),
    family =
      cumulative(link = "probit",
                 link_disc = "log"),
    prior = 
      prior(normal(0, 1), class = "Intercept") +
      prior(normal(0, 0.25), class = "b") +
      prior(normal(0, 0.25), class = "b", dpar = "disc"),
    data =
      dta %>%
      filter(survey == paste("wave", i)),
    inits = 0,
    iter = 2e3,
    chains = 4,
    cores = 4,
    file = here("_output", "_bias", paste0("bias_w", i))
  )
  
}



# 5. Plot partisan bias ---------------------------------------------------

# Before we create the plot, we need to load all of the different model fit
# objects. This includes the experimental model ("m1") and each of the
# wave-specific models we fit above ("bias_wx"). To make these easy to handle,
# we'll save them all to a list were each element is given the name of the
# model in question.

m1 <- readRDS(here("_output", "m1.rds"))

models <- list()

for(i in c(1:4, 6:8, 10:17)){
  
  models[[paste0("bias_w", i)]] <- 
    readRDS(
      here(
        "_output",
        "_bias",
        paste0("bias_w", i, ".rds")
      )
    )
  
}


# Now we'll replace the model fits for each wave with posterior draws for
# the "ionInc" and "ionOpp" effects.

m1 <-
  m1 %>% 
  posterior_samples(pars = "b_tTreatment") %>% 
  mutate(
    b_Inc = b_tTreatment + `b_tTreatment:pInc`,
    b_Opp = b_tTreatment + `b_tTreatment:pOpp`
  ) %>% 
  select(b_Inc, b_Opp) %>% 
  mutate(wave = "m1") %>%
  pivot_longer(cols = -wave,
               names_to = "var",
               values_to = "est")


for(i in c(1:4, 6:8, 10:17)){
  
  models[[paste0("bias_w", i)]] <- 
    models[[paste0("bias_w", i)]] %>% 
    posterior_samples(pars = "b_ion") %>% 
    mutate(wave = paste("wave", i)) %>%
    pivot_longer(cols = -wave,
                 names_to = "var",
                 values_to = "est")
  
}


# Now we'll combine the list into a single data frame so that we can plot it.

models <- do.call(rbind.data.frame, models)


# Next, we'll create a new variable that tells us what proportion of the
# incumbent and opposition effects we see for each wave we might attribute
# to the political survey context.

models <- models %>% mutate(bias = NA)

for(i in c(1:4, 6:8, 10:17)){
  
  models$bias[models$wave == paste("wave", i) &
                models$var == "b_ionInc"] <-
    m1$est[m1$var == "b_Inc"]/
    models$est[models$wave == paste("wave", i) &
                 models$var == "b_ionInc"]
  
  models$bias[models$wave == paste("wave", i) &
                models$var == "b_ionOpp"] <-
    m1$est[m1$var == "b_Opp"]/
    models$est[models$wave == paste("wave", i) &
                 models$var == "b_ionOpp"]
  
}


# Finally, we'll average these estimates and row bind them to the data.

mean_bias <- 
  models %>% 
  mutate(chain = rep(1:4000, each = 2, length.out = 120000)) %>%
  group_by(var, chain) %>% 
  summarise(bias = mean(bias)) %>% 
  mutate(wave = "mean") %>% 
  rename(est = chain) %>% 
  ungroup()

models <- rbind(models, mean_bias[, c(4, 1:3)])


# Then we'll summarise the data so that we can plot them as data labels.

bias_labs <- 
  models %>% 
  group_by(wave, var) %>% 
  summarise(median = (median(bias)),
            lower = (quantile(bias, probs = .025)),
            upper = (quantile(bias, probs = .975))) %>% 
  ungroup() %>% 
  mutate(
    var =
      var %>% 
      factor(
        levels = c("b_ionInc", "b_ionOpp"),
        labels = c("Incumbent", "Opposition")
      ),
    wave = 
      wave %>% 
      factor(
        levels = rev(c("mean", paste("wave", c(1:4, 6:8, 10:17)))),
        labels = rev(c("Mean", paste("Wave", c(1:4, 6:8, 10:17))))
      )
  ) 


# Now we can get started plotting. First, we'll create a rudimentary
# plot using ggplot.

bias_fig <- 
  models %>% 
  mutate(
    var =
      var %>% 
      factor(
        levels = c("b_ionInc", "b_ionOpp"),
        labels = c("Incumbent", "Opposition")
      ),
    wave = 
      wave %>% 
      factor(
        levels = rev(c("mean", paste("wave", c(1:4, 6:8, 10:17)))),
        labels = rev(c("Mean", paste("Wave", c(1:4, 6:8, 10:17))))
      )
  ) %>% 
  ggplot(
    aes(
      y = wave,
      x = bias
    )
  ) +
  facet_wrap(~ var) +
  geom_vline(xintercept = 0,
             linetype = "12",
             colour = bailey_colours("grey6"),
             size = .5)


# Now we'll add the density plots for the posterior proportion of bias
# estimates that we just computed.

bias_fig <- 
  bias_fig +
  stat_halfeyeh(aes(slab_fill = var,
                    slab_colour = var),
                .width = .95,
                point_size = 1,
                interval_size = 0.5,
                slab_alpha = 0.7,
                slab_size = 0.5,
                height = .7,
                normalize = "xy",
                show.legend = F) +
  geom_point(data = bias_labs,
             aes(x = median,
                 y = wave), colour = "white",
             size = 0.3, show.legend = F) +
  geom_text(data = bias_labs,
            aes(label = scales::percent(median,
                                        accuracy = 1),
                x = median + .04,
                y = wave),
            family = "Cabin",
            size = 2,
            vjust = 2,
            hjust = 0.7,
            inherit.aes = FALSE)


# Next we'll make it look nicer

bias_fig <- 
  bias_fig +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 8)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1),
                     breaks = seq(-1, 1, by = .5)) +
  scale_colour_manual(values = bailey_colours("blue", "red", "grey"),
                      aesthetics = "slab_colour") +
  scale_fill_manual(values = bailey_colours("blue8", "red8", "grey8"),
                    aesthetics = "slab_fill") +
  coord_cartesian(xlim = c(-1, 1)) +
  theme_bailey() +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_line(lineend = "round"),
        axis.text.y = element_text(hjust = 0.5)) +
  labs(x = "Proportion of Partisan Bias Attributable to the Political Survey Context")



# 6. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "013_bias_plot.txt"))


# One last thing...

thanks()


