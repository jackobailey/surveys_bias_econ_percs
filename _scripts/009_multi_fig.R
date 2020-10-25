# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 9: Multinomial National Economic Perceptions Results Figure

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
library(tidybayes)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load model

m4 <- readRDS(here("_output", "m4.rds"))



# 2. Triptych plot --------------------------------------------------------

# We're going to create three plots that describe how the treatment affected
# the national retrospective economic perceptions that our subjects reported
# based on who they voted for at the 2017 election. We're also going to respect
# the ordinal nature of the data too. First, we'll create a plot that shows
# how the responses of incumbents in the treatment group compared to those in
# the control group.

# Use m4 to predict 'e' where 'p' = 'Inc' and 't' = 'Treatment'

inc_t <- 
  add_fitted_draws(m4,
                   newdata = tibble(p = "Inc", t = "Treatment")) %>% 
  ungroup()


# Use m4 to predict 'e' where 'p' = 'Inc' and 't' = 'Control'

inc_c <-
  add_fitted_draws(m4,
                   newdata = tibble(p = "Inc", t = "Control")) %>% 
  ungroup()


# Compute inc_t - inc_c to determine the average treatment effect for each
# response option conditional on having voted for the incumbent. Then we'll
# pick and rename the variables we'll need to plot it.

inc_cate <-
  inc_t %>%
  mutate(cate = .value - inc_c$.value) %>% 
  select(p, t, resp = .category, cate)


# Next, we do the same as above, but for those who voted for an opposition
# party at the 2017 General Election. These instructions are the same as
# above, but substitute "Inc" for "Opp".

# Use m4 to predict 'e' where 'p' = 'Opp' and 't' = 'Treatment'

opp_t <- 
  add_fitted_draws(m4,
                   newdata = tibble(p = "Opp", t = "Treatment")) %>% 
  ungroup()


# Use m4 to predict 'e' where 'p' = 'Opp' and 't' = 'Control'

opp_c <-
  add_fitted_draws(m4,
                   newdata = tibble(p = "Opp", t = "Control")) %>% 
  ungroup()


# Compute opp_t - opp_c to determine the average treatment effect for each
# response option conditional on having voted for the incumbent. Then we'll
# pick and rename the variables we'll need to plot it.

opp_cate <-
  opp_t %>%
  mutate(cate = .value - opp_c$.value) %>% 
  select(p, t, resp = .category, cate)


# Again, we do the same as above, but for those who did not vote at the 2017
# General Election. These instructions are the same as above, but substitute
# "Non" for "Opp".

# Use m4 to predict 'e' where 'p' = 'Non' and 't' = 'Treatment'

non_t <- 
  add_fitted_draws(m4,
                   newdata = tibble(p = "Non", t = "Treatment")) %>% 
  ungroup()


# Use m4 to predict 'e' where 'p' = 'Non' and 't' = 'Control'

non_c <-
  add_fitted_draws(m4,
                   newdata = tibble(p = "Non", t = "Control")) %>% 
  ungroup()


# Compute non_t - non_c to determine the average treatment effect for each
# response option conditional on having voted for the incumbent. Then we'll
# pick and rename the variables we'll need to plot it.

non_cate <-
  non_t %>%
  mutate(cate = .value - non_c$.value) %>% 
  select(p, t, resp = .category, cate)


# Now that we've computed the estimated CATEs for those respondents who voted
# for the incumbent, an opposition party, or did not vote at the last election,
# we need to combine the estimates into a single dataset. We'll also convert
# the 'p' column to a factor at the same time so that we can control the order
# in which its options appear in our plots.

cates <- 
  rbind(inc_cate, opp_cate, non_cate) %>% 
  mutate(
    p =
      p %>% 
      factor(levels = c("Inc", "Opp", "Non"),
             labels = 
               c("Incumbent",
                 "Opposition",
                 "Nonvoter")),
    resp =
      resp %>% 
      factor(labels =
               c("Lot Worse",
                 "Little Worse",
                 "Stayed Same",
                 "Little Better",
                 "Lot Better",
                 "Don't know")
      ) %>% 
      fct_relevel("Don't know", after = 0)
  )


# We'll also use these data to create summaries that we can use as labels

cate_labs <-
  cates %>% 
  group_by(p, resp) %>% 
  summarise(
    median = median(cate),
    .groups = "drop"
  )


# Now, we have the data for the plot and the labels too, so can begin to plot
# the data. First, we tell ggplot2 that we want to use the 'cates' dataset we
# just created, that we want it to plot each option in the 'p' variable in a
# different facet, and that we want to add a dotted line at the zero mark.

multi_fig <- 
  cates %>% 
  ggplot(aes(x = cate, y = resp)) +
  facet_wrap(~ p) +
  geom_vline(xintercept = 0,
             linetype = "12",
             colour = bailey_colours("grey6"),
             size = .5)


# Next, we'll add our data to the plots and also the labels that we just created
# too. At the same time, we'll also add in points that show the median value of
# each distribution.

multi_fig <- 
  multi_fig +
  stat_halfeye(aes(slab_fill = p,
                    slab_colour = p),
                .width = .95,
                point_size = 1,
                interval_size = 0.5,
                slab_alpha = 0.7,
                slab_size = 0.5,
                height = .7,
                normalize = "xy",
                show.legend = F) +
  geom_point(data = cate_labs,
             aes(x = median,
                 y = resp), colour = "white",
             size = 0.3, show.legend = F) +
  geom_text(data = cate_labs,
            aes(label = scales::percent(median,
                                        accuracy = 0.1,
                                        suffix = ""),
                x = median + .002,
                y = resp),
            family = "Cabin",
            size = 2,
            vjust = 2,
            hjust = 0.7,
            inherit.aes = FALSE)


# Finally, we need to make the plot look nicer.

multi_fig <- 
  multi_fig +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 8)) +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1, suffix = ""),
                     breaks = seq(-.15, .15, by = .05)) +
  scale_colour_manual(values = bailey_colours("blue", "red", "grey"),
                      aesthetics = "slab_colour") +
  scale_fill_manual(values = bailey_colours("blue8", "red8", "grey8"),
                    aesthetics = "slab_fill") +
  coord_cartesian(xlim = c(-.15, .15)) +
  theme_bailey() +
  theme(axis.title.y = element_blank(),
        axis.ticks.x = element_line(lineend = "round"),
        axis.text.y = element_text(hjust = 0.5)) +
  labs(title = "Political Survey Bias is Robust to Model Specification and Missingness",
       x = "Conditional Average Treatment Effect (Percentage Points)")



# 3. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "007_national_fig.txt"))


# One last thing...

thanks()

