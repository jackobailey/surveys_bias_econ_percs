# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 14: Regression Tables

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

library(kableExtra)
library(janitor)
library(tidyverse)
library(jbmisc) # https://github.com/jackobailey/jbmisc
library(haven)
library(here)


# Load data

dta <- read_sav(here("_data", "surveys_bias_econ_percs.sav"))



# 2. Create crosstab ------------------------------------------------------

# We're going to create a crosstab where the DV is vote at the 2016 Brexit
# referendum and the IV is vote at the 2017 GE. First, we need to load the
# data and derive the necessary variables. We'll do this the same way as
# in the national and eu model scripts.

dta <- 
  dta %>% 
  select(
    torysplit,
    othersplit,
    dnvsplit,
    eu = pastvote_EURef
  ) %>% 
  mutate(
    p =
      case_when(
        torysplit %in% 1:2 ~ "Incumbent",
        othersplit %in% 1:2 ~ "Opposition",
        dnvsplit %in% 1:2 ~ "Nonvoter"
      ) %>% 
      factor(levels = c("Nonvoter", "Incumbent", "Opposition")),
    r = 
      eu %>% 
      as_factor() %>% 
      mark_na(c("Can’t remember")) %>% 
      fct_recode(
        "\\textsf{Remain}" = "I voted to Remain",
        "\\textsf{Leave}" = "I voted to Leave",
        "\\textsf{Nonvoter}" = "I did not vote",
      ) %>% 
      factor(levels = c("\\textsf{Nonvoter}", "\\textsf{Leave}", "\\textsf{Remain}"))
  )


# Next, we'll feed these two variables into janitor's tabyl function to
# create our crosstab. We'll also use the various adorn functions to make
# it look nice and presentable.

table <- 
  dta %>% 
  tabyl(r, p, show_na = F) %>%
  adorn_totals(where = c("row", "col")) %>% 
  adorn_percentages(denominator = "col") %>% 
  adorn_pct_formatting() %>% 
  adorn_ns() %>% 
  mutate(
    r = ifelse(r == "Total", "\\textsf{Total}", r),
    Nonvoter = gsub("%", "\\%", Nonvoter, fixed = T),
    Incumbent = gsub("%", "\\%", Incumbent, fixed = T),
    Opposition = gsub("%", "\\%", Opposition, fixed = T),
    Total = gsub("%", "\\%", Total, fixed = T)
    ) %>% 
  rename(
    " " = r,
    "\\textsf{Nonvoter}" = Nonvoter,
    "\\textsf{Incumbent}" = Incumbent,
    "\\textsf{Opposition}" = Opposition,
    "\\textsf{Total}" = Total
  )


# Now, we'll convert the whole thing into a latex table that we can use in
# our paper.

table <- 
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l", rep("r", 4)),
    linesep = "",
    caption = "There is a strong relationship between voting behaviour at the 2016 referendum on European Union membership and the 2017 general election. Even so, this relationship is not absolute. For example, some who voted for the incumbent Conservative Party in 2017 voted to remain in the EU in 2016. Likewise, some who voted for an opposition party voted to leave the EU.",
    label = "tab1"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Finally, we'll save the table to disk

sink(file = here("_paper", "_assets", "tab1.tex"))
cat(table)
sink()



# 6. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "014_eu_crosstab.txt"))


# One last thing...

thanks()


