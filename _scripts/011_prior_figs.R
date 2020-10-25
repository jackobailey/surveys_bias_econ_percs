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
library(haven)
library(jbmisc)
library(tidybayes)
library(brms)
library(here)


# Create function to simulate the outcome scale for arbitrary priors

rlikert <- 
  function(
    n = 1e3,
                    t1 = -0.84,
                    t2 = -0.25,
                    t3 = 0.25,
                    t4 = 0.84,
                    mean = 0,
                    disc = 0
    ){
  
  # Calculate standard deviation from discrimination
  sd <- 1/exp(disc)
  
  # Create empty dataframe
  dta <- tibble(n = 1:n)
  
  # Calculate probabilities of each response for each individual
  dta <- 
    dta %>% 
    mutate(
      p1 = pnorm(t1, mean, sd),
      p2 = pnorm(t2, mean, sd) - pnorm(t1, mean, sd),
      p3 = pnorm(t3, mean, sd) - pnorm(t2, mean, sd),
      p4 = pnorm(t4, mean, sd) - pnorm(t3, mean, sd),
      p5 = 1 - pnorm(t4, mean, sd)
    )
  
  # For each individual, sample a single response category
  dta <- dta %>% mutate(resp = NA)
  for (i in 1:nrow(dta)) {
    dta$resp[i] <- 
      sample(
        x = c(1:5), 
        size = 1, 
        prob = c(dta$p1[i], dta$p2[i], dta$p3[i], dta$p4[i], dta$p5[i])
      )
  }
  
  # Return
  dta$resp
  
}



# 2. Simulate prior threshold distributions -------------------------------

# In this script, we're going to run some prior predictive simulations so
# that we can determine what priors to use for our models. We'll start by
# simulating the distributions for each of the threshold parameters. Before
# we get started, we need some uninformative simulated data that we can fit
# a prior predictive model to. The data doesn't contribute anything, other
# than to let brms know about our data structure. We'll simulate it as though
# everything is random and unrelated.

dta <- 
  tibble(
    t =
      sample(x = c("Control", "Treatment"),
             size = 2e3,
             replace = T) %>% 
      factor(levels = c("Control", "Treatment")),
    p =
      sample(x = c("Non", "Inc", "Opp"),
             size = 2e3,
             replace = T) %>% 
      factor(levels = c("Non", "Inc", "Opp")),
    e =
      rlikert(n = 2e3) %>% 
      ordered(
        labels = 
          c("Lot worse",
            "Little worse",
            "Neither",
            "Little better",
            "Lot better")
      )
  )


# Now that we have simulated the data, we can fit our prior predictive model
# to check the prior threshold distributions. We can ignore the beta and delta
# priors for now. Note: the 'sample_prior = "only"' means that we're not updating
# the model and instead just sampling from the prior. I.e. the data won't
# contribute any information.

m5 <- 
  brm(
    formula = 
      bf(e ~ t*p) +
      lf(disc ~ 0 + t*p, cmc = FALSE),
    family =
      cumulative(
        link = "probit",
        link_disc = "log"
      ),
    prior =
      prior(normal(0, 1), class = "Intercept") +
      prior(normal(0, 0.25), class = "b") +
      prior(normal(0, 0.25), class = "b", dpar = "disc"),
    sample_prior = "only",
    data = dta,
    seed = 666,
    inits = 0,
    iter = 5e3,
    chains = 4,
    cores = 4,
    control = list(adapt_delta = 0.95),
    file = here("_output", "m5")
  )


# We can now use the fitted model to obtain the threshold parameters that our
# prior implies so that we can plot them. We'll save this in a data frame called
# 'thresh' for plotting. We'll also use the pnorm() function to convert them to
# the probability scale.

thresh <- 
  fixef(
    m5,
    pars = paste0("Intercept[", 1:4, "]"),
    summary = F
  ) %>% 
  data.frame() %>% 
  `names<-`(paste("Threshold", 1:4)) %>% 
  pivot_longer(
    cols = paste("Threshold", 1:4)
  ) %>% 
  mutate(
    value =
      value %>%
      pnorm()
  )


# We also need to be able to plot the overall probability distribution too. This
# is the same as imagining that rather than samples belonging to a particular to
# threshold, they all belong to one parameter that we'll call "Overall". We'll
# then append this to the thresh data then tell R the order that we want the
# distributions to appear.

thresh <- 
  rbind(
    thresh,
    thresh %>% 
      mutate(
        name = "Overall (Flat)"
      )
  ) %>% 
  mutate(
    name =
      name %>% 
      factor(levels = c(paste("Threshold", 1:4), "Overall (Flat)"))
  )


# Now, we can plot each to show the resulting distributions. We could do this as
# either density plots or as histograms. While the former perhaps look nicer, the
# effects we want to show are harder to discern as smoothing function can sometimes
# obscure them. Thus, we'll opt for histograms instead. I'll also normalize all the
# distributions so that they're the same height.

thresh_fig <- 
  thresh %>% 
  ggplot(aes(x = value,
             y = NA)) +
  facet_wrap(~ name,
             ncol = 5) +
  stat_histinterval(
    point_colour = NA,
    interval_colour = NA,
    slab_fill = bailey_colours("grey8"),
    slab_colour = "black",
    slab_size = 0.5,
    normalize = "xy"
  )


# Now that we've done the essentials, we can make it look nicer. We'll make sure
# that the x-axis is shown as percentages. We'll also provide new labels for the
# axes and make use of theme_bailey() to get rid of unnecessary clutter.

thresh_fig <- 
  thresh_fig +
  scale_x_continuous(labels = scales::percent_format(),
                     breaks = seq(0, 1, by = .5)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(title = "Normal(0, 1) Prior Over Threshold Distributions",
       x = "Prior Predicted Probability") +
  theme_bailey() +
  theme(axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        panel.spacing = unit(.5, "cm"))



# 3. Simulate prior beta distributions ------------------------------------

# Next, we'll do the same for the beta parameter, though unlike last time
# we'll simulate different priors so that we can see their respective
# differences. Again, I sample each 4,000 times, as in the final model. Then,
# I rename the parameters to show their respective distributions.

beta <- 
  tibble(
    beta_1_0 = rlikert(n = 4e3, mean = 0*rnorm(4e3, 0, 5)),
    beta_1_1 = rlikert(n = 4e3, mean = 1*rnorm(4e3, 0, 5)),
    beta_1_2 = rlikert(n = 4e3, mean = 2*rnorm(4e3, 0, 5)),
    beta_1_3 = rlikert(n = 4e3, mean = 3*rnorm(4e3, 0, 5)),
    beta_2_0 = rlikert(n = 4e3, mean = 0*rnorm(4e3, 0, 2)),
    beta_2_1 = rlikert(n = 4e3, mean = 1*rnorm(4e3, 0, 2)),
    beta_2_2 = rlikert(n = 4e3, mean = 2*rnorm(4e3, 0, 2)),
    beta_2_3 = rlikert(n = 4e3, mean = 3*rnorm(4e3, 0, 2)),
    beta_3_0 = rlikert(n = 4e3, mean = 0*rnorm(4e3, 0, 1)),
    beta_3_1 = rlikert(n = 4e3, mean = 1*rnorm(4e3, 0, 1)),
    beta_3_2 = rlikert(n = 4e3, mean = 2*rnorm(4e3, 0, 1)),
    beta_3_3 = rlikert(n = 4e3, mean = 3*rnorm(4e3, 0, 1)),
    beta_4_0 = rlikert(n = 4e3, mean = 0*rnorm(4e3, 0, .5)),
    beta_4_1 = rlikert(n = 4e3, mean = 1*rnorm(4e3, 0, .5)),
    beta_4_2 = rlikert(n = 4e3, mean = 2*rnorm(4e3, 0, .5)),
    beta_4_3 = rlikert(n = 4e3, mean = 3*rnorm(4e3, 0, .5)),
    beta_5_0 = rlikert(n = 4e3, mean = 0*rnorm(4e3, 0, .25)),
    beta_5_1 = rlikert(n = 4e3, mean = 1*rnorm(4e3, 0, .25)),
    beta_5_2 = rlikert(n = 4e3, mean = 2*rnorm(4e3, 0, .25)),
    beta_5_3 = rlikert(n = 4e3, mean = 3*rnorm(4e3, 0, .25))
  ) %>% 
  pivot_longer(cols = 1:20) %>% 
  mutate(
    name = 
      name %>% 
      factor(
        labels = c(
          rep("Normal(0, 5)", 4),
          rep("Normal(0, 2)", 4),
          rep("Normal(0, 1)", 4),
          rep("Normal(0, 0.5)", 4),
          rep("Normal(0, 0.25)", 4)
        )
      ),
    multiplier = 
      rep(0:3,
          times = 20e3) %>% 
      factor(levels = 3:0),
    value =
      value %>%
      factor()
  )


# Next, we'll plot the resulting outcome distributions as bars

beta_fig <- 
  beta %>% 
  ggplot(
    aes(
      x = value
    )
  ) +
  facet_grid(multiplier ~ name,
             switch = "y",
             scales = "free_x") +
  geom_bar(colour = "black",
           fill = bailey_colours("grey8"),
           width = 0.85,
           size = 0.5)


# Now we can make it look nice. We'll add some labels and a title to the
# plot, use theme_bailey(), and make some manual adjustments to the theme
# to make it consistent with the other plots we use.

beta_fig <- 
  beta_fig +
  theme_bailey() +
  theme(axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        axis.line.y = element_blank()) +
  labs(title = "Prior Predictive Outcome Distributions Across Different Priors on Beta",
       x = "Response Option",
       y = "Sum of Parameter Values")



# 4. Simulate prior delta distributions -----------------------------------

# Next, we'll do the same for the delta parameter. This is the same as for
# the beta parameters, so I'll run through this with minimal commentary.

delta <- 
  tibble(
    delta_1_0 = rlikert(n = 4e3, disc = 0*rnorm(4e3, 0, 5)),
    delta_1_1 = rlikert(n = 4e3, disc = 1*rnorm(4e3, 0, 5)),
    delta_1_2 = rlikert(n = 4e3, disc = 2*rnorm(4e3, 0, 5)),
    delta_1_3 = rlikert(n = 4e3, disc = 3*rnorm(4e3, 0, 5)),
    delta_2_0 = rlikert(n = 4e3, disc = 0*rnorm(4e3, 0, 2)),
    delta_2_1 = rlikert(n = 4e3, disc = 1*rnorm(4e3, 0, 2)),
    delta_2_2 = rlikert(n = 4e3, disc = 2*rnorm(4e3, 0, 2)),
    delta_2_3 = rlikert(n = 4e3, disc = 3*rnorm(4e3, 0, 2)),
    delta_3_0 = rlikert(n = 4e3, disc = 0*rnorm(4e3, 0, 1)),
    delta_3_1 = rlikert(n = 4e3, disc = 1*rnorm(4e3, 0, 1)),
    delta_3_2 = rlikert(n = 4e3, disc = 2*rnorm(4e3, 0, 1)),
    delta_3_3 = rlikert(n = 4e3, disc = 3*rnorm(4e3, 0, 1)),
    delta_4_0 = rlikert(n = 4e3, disc = 0*rnorm(4e3, 0, .5)),
    delta_4_1 = rlikert(n = 4e3, disc = 1*rnorm(4e3, 0, .5)),
    delta_4_2 = rlikert(n = 4e3, disc = 2*rnorm(4e3, 0, .5)),
    delta_4_3 = rlikert(n = 4e3, disc = 3*rnorm(4e3, 0, .5)),
    delta_5_0 = rlikert(n = 4e3, disc = 0*rnorm(4e3, 0, .25)),
    delta_5_1 = rlikert(n = 4e3, disc = 1*rnorm(4e3, 0, .25)),
    delta_5_2 = rlikert(n = 4e3, disc = 2*rnorm(4e3, 0, .25)),
    delta_5_3 = rlikert(n = 4e3, disc = 3*rnorm(4e3, 0, .25))
  ) %>%
  pivot_longer(cols = 1:20) %>% 
  mutate(
    name = 
      name %>% 
      factor(
        labels = c(
          rep("Normal(0, 5)", 4),
          rep("Normal(0, 2)", 4),
          rep("Normal(0, 1)", 4),
          rep("Normal(0, 0.5)", 4),
          rep("Normal(0, 0.25)", 4)
        )
      ),
    multiplier = 
      rep(0:3,
          times = 20e3) %>% 
      factor(levels = 3:0),
    value =
      value %>%
      factor()
  )


# Next, we'll plot the resulting outcome distributions as bars

delta_fig <- 
  delta %>% 
  ggplot(
    aes(
      x = value
    )
  ) +
  facet_grid(multiplier ~ name,
             switch = "y",
             scales = "free_x") +
  geom_bar(colour = "black",
           fill = bailey_colours("grey8"),
           width = 0.85,
           size = 0.5)


# Now we can make it look nice. We'll add some labels and a title to the
# plot, use theme_bailey(), and make some manual adjustments to the theme
# to make it consistent with the other plots we use.

delta_fig <- 
  delta_fig +
  theme_bailey() +
  theme(axis.text.y = element_blank(),
        strip.text.y = element_text(angle = 180),
        axis.line.y = element_blank()) +
  labs(title = "Prior Predictive Outcome Distributions Across Different Priors on Beta",
       x = "Response Option",
       y = "Sum of Parameter Values")



# 5. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "011_prior_figs.txt"))


# One last thing...

thanks()

