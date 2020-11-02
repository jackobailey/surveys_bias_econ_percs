# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 10: Power Analysis

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
library(jbmisc)
library(haven)
library(brms)
library(broom)
library(here)



# 2. Create functions and objects for simulation --------------------------

# First, we'll create a function to simulate 5-point Likert-type data

sim_likert <- 
  function(
    seed = NULL,
    n = 1500,
    t = c(-.5, 0, .5, 1),
    b = rep(1, 5),
    d = rep(0, 5)
  ){
    
    # Set random seed
    set.seed(seed)
    
    # Simulate dataset
    dta <- 
      tibble(
        treat = rep(0:1, each = n/2),
        vote = 
          sample(
            x = c("Non", "Inc", "Opp"),
            replace = T,
            size = n,
            prob = rep(1/3, 3)
          )
      ) %>% 
      mutate(
        inc = vote %>% as_dummy("Inc"),
        opp = vote %>% as_dummy("Opp"),
      )
    
    # Compute latent-y location and scale parameters for each individual
    dta <- 
      dta %>% 
      mutate(
        mean =
          b[1]*dta$treat +
          b[2]*dta$inc +
          b[3]*dta$opp +
          b[4]*(dta$treat*dta$inc) +
          b[5]*(dta$treat*dta$opp),
        sd = 
          1/exp(
            d[1]*dta$treat +
              d[2]*dta$inc +
              d[3]*dta$opp +
              d[4]*(dta$treat*dta$inc) +
              d[5]*(dta$treat*dta$opp)
          )
      )
    
    # Calculate probabilities of each response for each individual
    dta <- 
      dta %>% 
      mutate(
        p1 = pnorm(t[1], mean, sd),
        p2 = pnorm(t[2], mean, sd) - pnorm(t[1], mean, sd),
        p3 = pnorm(t[3], mean, sd) - pnorm(t[2], mean, sd),
        p4 = pnorm(t[4], mean, sd) - pnorm(t[3], mean, sd),
        p5 = 1 - pnorm(t[4], mean, sd)
      )
    
    # For each individual, sample a single response category
    dta <- dta %>% mutate(econ = NA)
    
    for (i in 1:nrow(dta)) {
      dta$econ[i] <- 
        sample(
          x = c(1:5), 
          size = 1, 
          prob = c(dta$p1[i], dta$p2[i], dta$p3[i], dta$p4[i], dta$p5[i])
        )
    }
    
    # Convert econ to ordered
    dta <-
      dta %>% 
      mutate(
        econ = econ %>% ordered(labels = c("Got a lot worse",
                                           "Got a little worse",
                                           "Stayed the same",
                                           "Got a little better",
                                           "Got a lot better" ))
      )
    
    # Return data frame
    dta %>% select(econ, treat, inc, opp)
    
  }


# Second, we'll use the function to simulate some data

test <- 
  sim_likert(
    seed = 666,
    n = 1500,
    t = c(-0.95, 0.32, 1.43, 2.58),
    b = c(0, 0, 0, 0.58, -.21),
    d = c(0, 0, 0, 0, -.06)
  )


# We'll then fit a model to these data to avoid recompiling later

fit <- 
  brm(
    formula = 
      bf(econ ~ treat + inc + opp + treat:inc + treat:opp) +
      lf(disc ~ 0 + treat + inc + opp + treat:inc + treat:opp, cmc = F),
    family = cumulative(link = "probit",
                        link_disc = "log"),
    prior = 
      prior(normal(0, 1), class = "Intercept") + 
      prior(normal(0, 0.25), class = "b") +
      prior(normal(0, 0.25), class = "b", dpar = "disc"),
    data = test,
    inits = 0,
    warmup = 250,
    iter = 750,
    chains = 2,
    cores = 2,
    seed = 666,
    file = here("_output", "init")
  )


# Now, we'll create a function to simulates similar Likert-type data and then
# fits a brms model to it

sim_and_fit <- 
  function(
    seed, 
    n,
    t = c(-.5, 0, .5, 1),
    b = rep(1, 5),
    d = rep(0, 5)
  ){
    
    # Simulate data
    dta <- sim_likert(seed = seed, n = n, t = t, b = b, d = d)
    
    # Print simulation number
    print(paste0("Simulation ", seed, "/1000"))
    
    # Fit model
    update(fit,
           newdata = dta, 
           seed = seed,
           inits = 0,
           warmup = 250,
           iter = 750,
           chains = 2,
           cores = 2,
           silent = T,
           refresh = 0) %>% 
      fixef() %>% 
      as.data.frame() %>% 
      rownames_to_column(var = "term") %>% 
      rename(
        est = Estimate,
        error = Est.Error,
        lower = `Q2.5`,
        upper = `Q97.5`
      ) %>% 
      filter(term %in% c("treat:inc"))
  }


# 3. Simulate and analyse distributions -----------------------------------

# 3.1. Scenario 1: Treatment causes total BES effects

# Specify effects

t <- c(-0.95, 0.32, 1.43, 2.58)
b <- c(0, 0, 0, 0.58, -.21)


# Run simulations

total_1500 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 1500,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "1500",
         effect = "Total")
saveRDS(total_1500, here("_output", "total_1500.rds"))

total_2000 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 2000,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "2000",
         effect = "Total")
saveRDS(total_2000, here("_output", "total_2000.rds"))

total_2500 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 2500,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "2500",
         effect = "Total")
saveRDS(total_2500, here("_output", "total_2500.rds"))


# 4.2. Scenario 2: Treatment causes 1/2 of BES effects

# Specify effects

t <- c(-0.95, 0.32, 1.43, 2.58)
b <- c(0, 0, 0, 0.58*.5, -.21*.5)


# Run simulations

half_1500 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 1500,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "1500",
         effect = "Half")
saveRDS(half_1500, here("_output", "half_1500.rds"))

half_2000 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 2000,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "2000",
         effect = "Half")
saveRDS(half_2000, here("_output", "half_2000.rds"))

half_2500 <-
  tibble(seed = 1:1000) %>% 
  mutate(tidy = map(seed,
                    sim_and_fit,
                    n = 2500,
                    t = t,
                    b = b)) %>% 
  unnest(tidy) %>% 
  mutate(n = "2500",
         effect = "Half")
saveRDS(half_2500, here("_output", "half_2500.rds"))


# 4.3. Create single simulation data set for plots

# Read data

dta <- paste0(here("_output"), "/",
              c("total_1500.rds",
                "total_2000.rds",
                "total_2500.rds",
                "half_1500.rds",
                "half_2000.rds",
                "half_2500.rds"))


# Load data (ignore warnings)

dta <- lapply(dta, readRDS)


# Combine data sets

sim <- do.call("rbind", dta)


# Order "n" variable

sim$n <- sim$n %>% factor(levels = c("1500", "2000", "2500"))


# Order "effect" variable

sim$effect <- sim$effect %>% factor(levels = c("Total", "Half"))


# Save simulation to disk

saveRDS(sim, here("_output", "all_sims.rds"))



# 4. Create power analysis plots/tables -----------------------------------

# 5.1. Plot simulations: Treatment x Incumbent

# Plot

power_fig <- 
  sim %>%
  mutate(effect = 
           effect %>% 
           factor(labels = 
                    c("Scenario 1: Total Effect",
                      "Scenario 2: Half Effect")
           )
  ) %>% 
  group_by(n, effect) %>% 
  arrange(lower) %>% 
  mutate(index = row_number()) %>% 
  ungroup() %>% 
  ggplot(
    aes(
      x = index,
      y = est,
      ymin = lower,
      ymax = upper,
      colour = n,
      fill = n
    )
  ) +
  facet_wrap(~ effect, nrow = 1) +
  geom_ribbon(colour = NA) +
  geom_hline(yintercept = 0, color = "black") +
  geom_hline(data = 
               tibble(
                 effect = factor(c("Scenario 1: Total Effect",
                                   "Scenario 2: Half Effect")),
                 ef_size = c(.58, .58*.5)),
             aes(yintercept = ef_size),
             color = "black",
             linetype = "dashed") +
  scale_x_discrete("Random Seed Ordered by Lower 95% Credible Interval", breaks = NULL) +
  scale_fill_grey(start = .9, end = .6) +
  labs(
    title = "Power Analysis Suggests a Sample of 2,500 to Reach 80% Power",
    y = "95% Credible Interval of Estimated Effect") +
  coord_cartesian(ylim = c(-0.5, 1.5)) +
  theme_bailey()


# Calculate proportion of simulations > 0

power <- 
  sim %>% 
  group_by(effect, n) %>% 
  mutate(check = ifelse(lower > 0, 1, 0)) %>% 
  summarise(power = mean(check)) %>% 
  knitr::kable(digits = 2)



# 5. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "010_power_analysis.txt"))


# One last thing...

thanks()

