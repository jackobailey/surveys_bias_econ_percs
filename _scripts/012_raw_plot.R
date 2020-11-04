# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 12: Raw Percentages Plot

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
library(janitor)
library(haven)
library(jbmisc)     # https://github.com/jackobailey/jbmisc
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
      as_factor(ordered = T) %>% 
      mark_na("Don't know")
  )


# Finally, we'll clean up a little by selecting only those variables that
# we need to run our analysis ('p', 't', and 'e') and omit any missing
# cases list-wise.

dta <- 
  dta %>% 
  select(p, t, e) %>% 
  na.omit()



# 3. Create plot ----------------------------------------------------------

# We're going to plot the raw vote shares by partisanship. First, we must
# work out what these figures are.

raw_dta <- 
  dta %>% 
  group_by(p, t) %>% 
  summarise(
    lot_worse = length(e[e == "Got a lot worse"])/length(e),
    little_worse = length(e[e == "Got a little worse"])/length(e),
    stayed_same = length(e[e == "Stayed the same"])/length(e),
    little_better = length(e[e == "Got a little better"])/length(e),
    lot_better = length(e[e == "Got a lot better"])/length(e),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  pivot_longer(
    cols = c(lot_worse, little_worse, stayed_same, little_better, lot_better),
    names_to = "resp",
    values_to = "pct"
  ) %>% 
  mutate(
    p =
      case_when(
        p == "Inc" & t == "Treatment" ~ "Incumbent (Treatment)",
        p == "Opp" & t == "Treatment" ~ "Opposition (Treatment)",
        p == "Non" & t == "Treatment" ~ "Nonvoter (Treatment)",
        p == "Inc" & t == "Control" ~ "Incumbent (Control)",
        p == "Opp" & t == "Control" ~ "Opposition (Control)",
        p == "Non" & t == "Control" ~ "Nonvoter (Control)"
      ) %>% 
      factor(
        levels =
          c(
            "Incumbent (Treatment)",
            "Opposition (Treatment)",
            "Nonvoter (Treatment)",
            "Incumbent (Control)",
            "Opposition (Control)",
            "Nonvoter (Control)"
          )
      ),
    resp = 
      resp %>% 
      factor(
        levels = 
          c(
            "lot_worse",
            "little_worse",
            "stayed_same",
            "little_better",
            "lot_better"
          ),
        labels =
          c(
            "Lot worse",
            "Little worse",
            "Stayed same",
            "Little better",
            "Lot better"
          )
      )
  )


# Next, we'll plot them.

raw_fig <- 
  raw_dta %>% 
  ggplot(
    aes(
      x = resp,
      y = pct,
      colour = p,
      fill = p
    )
  ) +
  facet_wrap(~ p) +
  geom_col(
    position = position_dodge(width = 1)
  ) +
  geom_text(data = raw_dta,
            aes(label = 
                  scales::percent(
                    pct,
                    accuracy = 0.1
                  ),
                y = pct + 0.09,
                x = resp),
            family = "Cabin",
            size = 2,
            vjust = 2,
            inherit.aes = FALSE) +
  scale_colour_manual(values = bailey_colours("blue", "red", "grey", "blue", "red", "grey")) +
  scale_fill_manual(values = bailey_colours("blue8", "red8", "grey8", "blue8", "red8", "grey8")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 8)) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1),
    breaks = seq(0, .6, by = .2)
  ) +
  coord_cartesian(ylim = c(0, .6)) +
  labs(y = "Percentage Selecting Response") +
  theme_bailey() +
  theme(
    legend.position = "none",
    axis.title.x = element_blank()
  )




# 4. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "012_raw_plot.txt"))


# One last thing...

thanks()

