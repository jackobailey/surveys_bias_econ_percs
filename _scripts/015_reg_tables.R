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
library(tidyverse)
library(jbmisc) # https://github.com/jackobailey/jbmisc
library(brms)
library(here)


# Load models

m1 <- readRDS(here("_output", "m1.rds"))
m2 <- readRDS(here("_output", "m2.rds"))
m3 <- readRDS(here("_output", "m3.rds"))
m4 <- readRDS(here("_output", "m4.rds"))



# 2. Create national table ------------------------------------------------


# Get each draw from the posterior distribution

draws <- posterior_samples(m1)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Treatment}` = b_tTreatment,
    `\\textsf{Incumbent}` = b_pInc,
    `\\textsf{Opposition}` = b_pOpp,
    `\\textsf{Treatment $\\times$ Inc.}` = `b_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp.}` = `b_tTreatment:pOpp`,
    `\\textsf{Treatment disc}` = b_disc_tTreatment,
    `\\textsf{Incumbent disc}` = b_disc_pInc,
    `\\textsf{Opposition disc}` = b_disc_pOpp,
    `\\textsf{Treatment $\\times$ Inc. disc}` = `b_disc_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. disc}` = `b_disc_tTreatment:pOpp`,
    `\\textsf{Threshold 1}` = `b_Intercept[1]`,
    `\\textsf{Threshold 2}` = `b_Intercept[2]`,
    `\\textsf{Threshold 3}` = `b_Intercept[3]`,
    `\\textsf{Threshold 4}` = `b_Intercept[4]`
  )


# Pivot table to long-format then compute summary statistics

table <- 
  draws %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    type =
      case_when(
        str_detect(name, "disc") ~ "Discrimination",
        str_detect(name, "Threshold") ~ "Threshold",
        TRUE ~ "Mean"
      ) %>% 
      factor(levels = c("Mean", "Discrimination", "Threshold")),
    name =
      name %>% 
      str_remove_all(" disc") %>% 
      factor(
        levels =
          c(
            "\\textsf{Treatment}",
            "\\textsf{Incumbent}",
            "\\textsf{Opposition}",
            "\\textsf{Treatment $\\times$ Inc.}",
            "\\textsf{Treatment $\\times$ Opp.}",
            "\\textsf{Threshold 1}",
            "\\textsf{Threshold 2}",
            "\\textsf{Threshold 3}",
            "\\textsf{Threshold 4}"
          )
      )
  ) %>% 
  group_by(name, type) %>% 
  summarise(
    `\\multicolumn{1}{r}{\\textsf{Median}}` = format(round(median(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{Error}}` = format(round(sd(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = format(round(quantile(value, probs = 0.025), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = format(round(quantile(value, probs = 0.975), 2), nsmall = 2),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(" ", type) %>% 
  select(-type) %>% 
  add_row(
    .before = 1,
    name = "\\textsf{\\textbf{Mean}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 7,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 8,
    name = "\\textsf{\\textbf{Discrimination}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 14,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 15,
    name = "\\textsf{\\textbf{Threshold}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  rename(" " = name)


# Create latex table

table <-
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l ", rep("D{.}{.}{-1} ", 4)),
    linesep = "",
    caption = "Incumbent supporters report more positive economic perceptions when they answer in a political versus a non-political survey. Data come from a survey experiment conducted by YouGov between 6th and 8th November 2019, the start of the campaign for the 2019 UK General Election.",
    label = "tabA1"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Add number of individuals and groups

table <- 
  table %>% 
  str_replace(
    "\\\\bottomrule\\\n",
    paste0(
      "\\\\midrule\\\n",
      "\\\\textsf{N (Individuals)} & ",
      "\\\\multicolumn{4}{r}{$",
      format(nrow(m1$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Save table to disk

sink(file = here("_paper", "_assets", "tabA1.tex"))
cat(table)
sink()



# 3. Create personal table ------------------------------------------------


# Get each draw from the posterior distribution

draws <- posterior_samples(m2)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Treatment}` = b_tTreatment,
    `\\textsf{Incumbent}` = b_pInc,
    `\\textsf{Opposition}` = b_pOpp,
    `\\textsf{Treatment $\\times$ Inc.}` = `b_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp.}` = `b_tTreatment:pOpp`,
    `\\textsf{Treatment disc}` = b_disc_tTreatment,
    `\\textsf{Incumbent disc}` = b_disc_pInc,
    `\\textsf{Opposition disc}` = b_disc_pOpp,
    `\\textsf{Treatment $\\times$ Inc. disc}` = `b_disc_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. disc}` = `b_disc_tTreatment:pOpp`,
    `\\textsf{Threshold 1}` = `b_Intercept[1]`,
    `\\textsf{Threshold 2}` = `b_Intercept[2]`,
    `\\textsf{Threshold 3}` = `b_Intercept[3]`,
    `\\textsf{Threshold 4}` = `b_Intercept[4]`
  )


# Pivot table to long-format then compute summary statistics

table <- 
  draws %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    type =
      case_when(
        str_detect(name, "disc") ~ "Discrimination",
        str_detect(name, "Threshold") ~ "Threshold",
        TRUE ~ "Mean"
      ) %>% 
      factor(levels = c("Mean", "Discrimination", "Threshold")),
    name =
      name %>% 
      str_remove_all(" disc") %>% 
      factor(
        levels =
          c(
            "\\textsf{Treatment}",
            "\\textsf{Incumbent}",
            "\\textsf{Opposition}",
            "\\textsf{Treatment $\\times$ Inc.}",
            "\\textsf{Treatment $\\times$ Opp.}",
            "\\textsf{Threshold 1}",
            "\\textsf{Threshold 2}",
            "\\textsf{Threshold 3}",
            "\\textsf{Threshold 4}"
          )
      )
  ) %>% 
  group_by(name, type) %>% 
  summarise(
    `\\multicolumn{1}{r}{\\textsf{Median}}` = format(round(median(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{Error}}` = format(round(sd(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = format(round(quantile(value, probs = 0.025), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = format(round(quantile(value, probs = 0.975), 2), nsmall = 2),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(" ", type) %>% 
  select(-type) %>% 
  add_row(
    .before = 1,
    name = "\\textsf{\\textbf{Mean}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 7,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 8,
    name = "\\textsf{\\textbf{Discrimination}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 14,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 15,
    name = "\\textsf{\\textbf{Threshold}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  rename(" " = name)


# Create latex table

table <-
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l ", rep("D{.}{.}{-1} ", 4)),
    linesep = "",
    caption = "Respondents do not differ in how they report their personal economic perceptions when they answer in a political versus a non-political survey. Data come from a survey experiment conducted by YouGov between 6th and 8th November 2019, the start of the campaign for the 2019 UK General Election.",
    label = "tabA2"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Add number of individuals and groups

table <- 
  table %>% 
  str_replace(
    "\\\\bottomrule\\\n",
    paste0(
      "\\\\midrule\\\n",
      "\\\\textsf{N (Individuals)} & ",
      "\\\\multicolumn{4}{r}{$",
      format(nrow(m2$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Save table to disk

sink(file = here("_paper", "_assets", "tabA2.tex"))
cat(table)
sink()



# 4. Create EU table ------------------------------------------------------


# Get each draw from the posterior distribution

draws <- posterior_samples(m3)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Treatment}` = b_tTreatment,
    `\\textsf{Leave}` = b_rLeave,
    `\\textsf{Remain}` = b_rRemain,
    `\\textsf{Treatment $\\times$ Leave}` = `b_tTreatment:rLeave`,
    `\\textsf{Treatment $\\times$ Remain}` = `b_tTreatment:rRemain`,
    `\\textsf{Treatment disc}` = b_disc_tTreatment,
    `\\textsf{Leave disc}` = b_disc_rLeave,
    `\\textsf{Remain disc}` = b_disc_rRemain,
    `\\textsf{Treatment $\\times$ Leave disc}` = `b_disc_tTreatment:rLeave`,
    `\\textsf{Treatment $\\times$ Remain disc}` = `b_disc_tTreatment:rRemain`,
    `\\textsf{Threshold 1}` = `b_Intercept[1]`,
    `\\textsf{Threshold 2}` = `b_Intercept[2]`,
    `\\textsf{Threshold 3}` = `b_Intercept[3]`,
    `\\textsf{Threshold 4}` = `b_Intercept[4]`
  )


# Pivot table to long-format then compute summary statistics

table <- 
  draws %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    type =
      case_when(
        str_detect(name, "disc") ~ "Discrimination",
        str_detect(name, "Threshold") ~ "Threshold",
        TRUE ~ "Mean"
      ) %>% 
      factor(levels = c("Mean", "Discrimination", "Threshold")),
    name =
      name %>% 
      str_remove_all(" disc") %>% 
      factor(
        levels =
          c(
            "\\textsf{Treatment}",
            "\\textsf{Leave}",
            "\\textsf{Remain}",
            "\\textsf{Treatment $\\times$ Leave}",
            "\\textsf{Treatment $\\times$ Remain}",
            "\\textsf{Threshold 1}",
            "\\textsf{Threshold 2}",
            "\\textsf{Threshold 3}",
            "\\textsf{Threshold 4}"
          )
      )
  ) %>% 
  group_by(name, type) %>% 
  summarise(
    `\\multicolumn{1}{r}{\\textsf{Median}}` = format(round(median(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{Error}}` = format(round(sd(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = format(round(quantile(value, probs = 0.025), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = format(round(quantile(value, probs = 0.975), 2), nsmall = 2),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(" ", type) %>% 
  select(-type) %>% 
  add_row(
    .before = 1,
    name = "\\textsf{\\textbf{Mean}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 7,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 8,
    name = "\\textsf{\\textbf{Discrimination}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 14,
    name = "",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 15,
    name = "\\textsf{\\textbf{Threshold}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  rename(" " = name)


# Create latex table

table <-
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l ", rep("D{.}{.}{-1} ", 4)),
    linesep = "",
    caption = "Like party identification, those who voted for the winning side at the 2016 referendum on European Union membership report more positive economic perceptions when they answer in a political versus a non-political survey. Data come from a survey experiment conducted by YouGov between 6th and 8th November 2019, the start of the campaign for the 2019 UK General Election.",
    label = "tabA3"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Add number of individuals and groups

table <- 
  table %>% 
  str_replace(
    "\\\\bottomrule\\\n",
    paste0(
      "\\\\midrule\\\n",
      "\\\\textsf{N (Individuals)} & ",
      "\\\\multicolumn{4}{r}{$",
      format(nrow(m3$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Save table to disk

sink(file = here("_paper", "_assets", "tabA3.tex"))
cat(table)
sink()



# 5. Create multinomial table ---------------------------------------------

# Get each draw from the posterior distribution

draws <- posterior_samples(m4)


# Get regression coefficients

draws <-
  draws %>% 
  select(
    `\\textsf{Intercept (Got a lot worse)}` = b_muGotalotworse_Intercept,
    `\\textsf{Intercept (Got a little worse)}` = b_muGotalittleworse_Intercept,
    `\\textsf{Intercept (Got a little better)}` = b_muGotalittlebetter_Intercept,
    `\\textsf{Intercept (Got a lot better)}` = b_muGotalotbetter_Intercept,
    `\\textsf{Intercept (Don't know)}` = b_muDontknow_Intercept,
    `\\textsf{Treatment (Got a lot worse)}` = b_muGotalotworse_tTreatment,
    `\\textsf{Incumbent (Got a lot worse)}` = b_muGotalotworse_pInc,
    `\\textsf{Opposition (Got a lot worse)}` = b_muGotalotworse_pOpp,
    `\\textsf{Treatment $\\times$ Inc. (Got a lot worse)}` = `b_muGotalotworse_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. (Got a lot worse)}` = `b_muGotalotworse_tTreatment:pOpp`,
    `\\textsf{Treatment (Got a little worse)}` = b_muGotalittleworse_tTreatment,
    `\\textsf{Incumbent (Got a little worse)}` = b_muGotalittleworse_pInc,
    `\\textsf{Opposition (Got a little worse)}` = b_muGotalittleworse_pOpp,
    `\\textsf{Treatment $\\times$ Inc. (Got a little worse)}` = `b_muGotalittleworse_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. (Got a little worse)}` = `b_muGotalittleworse_tTreatment:pOpp`,
    `\\textsf{Treatment (Got a little better)}` = b_muGotalittlebetter_tTreatment,
    `\\textsf{Incumbent (Got a little better)}` = b_muGotalittlebetter_pInc,
    `\\textsf{Opposition (Got a little better)}` = b_muGotalittlebetter_pOpp,
    `\\textsf{Treatment $\\times$ Inc. (Got a little better)}` = `b_muGotalittlebetter_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. (Got a little better)}` = `b_muGotalittlebetter_tTreatment:pOpp`,
    `\\textsf{Treatment (Got a lot better)}` = b_muGotalotbetter_tTreatment,
    `\\textsf{Incumbent (Got a lot better)}` = b_muGotalotbetter_pInc,
    `\\textsf{Opposition (Got a lot better)}` = b_muGotalotbetter_pOpp,
    `\\textsf{Treatment $\\times$ Inc. (Got a lot better)}` = `b_muGotalotbetter_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. (Got a lot better)}` = `b_muGotalotbetter_tTreatment:pOpp`,
    `\\textsf{Treatment (Don't know)}` = b_muDontknow_tTreatment,
    `\\textsf{Incumbent (Don't know)}` = b_muDontknow_pInc,
    `\\textsf{Opposition (Don't know)}` = b_muDontknow_pOpp,
    `\\textsf{Treatment $\\times$ Inc. (Don't know)}` = `b_muDontknow_tTreatment:pInc`,
    `\\textsf{Treatment $\\times$ Opp. (Don't know)}` = `b_muDontknow_tTreatment:pOpp`
  )


# Pivot table to long-format then compute summary statistics

table <- 
  draws %>% 
  pivot_longer(cols = everything()) %>% 
  mutate(
    type =
      case_when(
        str_detect(name, "Got a lot worse") ~ "Got a lot worse",
        str_detect(name, "Got a little worse") ~ "Got a little worse",
        str_detect(name, "Got a little better") ~ "Got a little better",
        str_detect(name, "Got a lot better") ~ "Got a lot better",
        str_detect(name, "Don't know") ~ "Don't know",
        TRUE ~ "Stayed the same"
      ) %>% 
      factor(
        levels = 
          c(
            "Got a lot worse",
            "Got a little worse",
            "Got a little better",
            "Got a lot better",
            "Don't know")),
    name =
      name %>% 
      str_remove_all(" \\([^)]*\\)") %>% 
      factor(
        levels =
          c(
            "\\textsf{Intercept}",
            "\\textsf{Treatment}",
            "\\textsf{Incumbent}",
            "\\textsf{Opposition}",
            "\\textsf{Treatment $\\times$ Inc.}",
            "\\textsf{Treatment $\\times$ Opp.}"
          )
      )
  ) %>% 
  group_by(name, type) %>% 
  summarise(
    `\\multicolumn{1}{r}{\\textsf{Median}}` = format(round(median(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{Error}}` = format(round(sd(value), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = format(round(quantile(value, probs = 0.025), 2), nsmall = 2),
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = format(round(quantile(value, probs = 0.975), 2), nsmall = 2),
    .groups = "drop"
  ) %>% 
  ungroup() %>% 
  arrange(" ", type) %>% 
  select(-type) %>% 
  add_row(
    .before = 1,
    name = "\\textsf{\\textbf{Got a lot worse}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 8,
    name = "\\textsf{\\textbf{Got a little worse}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 15,
    name = "\\textsf{\\textbf{Got a little better}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 22,
    name = "\\textsf{\\textbf{Got a lot better}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  add_row(
    .before = 29,
    name = "\\textsf{\\textbf{Don't know}}",
    `\\multicolumn{1}{r}{\\textsf{Median}}` = "",
    `\\multicolumn{1}{r}{\\textsf{Error}}` = "",
    `\\multicolumn{1}{r}{\\textsf{2.5\\%}}` = "",
    `\\multicolumn{1}{r}{\\textsf{97.5\\%}}` = ""
  ) %>% 
  rename(" " = name)


# Create latex table

table <-
  table %>% 
  kable(
    format = "latex",
    booktabs = TRUE,
    escape = FALSE,
    align = c("l ", rep("D{.}{.}{-1} ", 4)),
    linesep = "",
    caption = "That incumbent supporters report more positive economic perceptions when they answer in a political versus a non-political survey is robust to model specification. Data come from a survey experiment conducted by YouGov between 6th and 8th November 2019, the start of the campaign for the 2019 UK General Election.",
    label = "tabA4"
  ) %>% 
  kable_styling(
    position = "center"
  )


# Add number of individuals and groups

table <- 
  table %>% 
  str_replace(
    "\\\\bottomrule\\\n",
    paste0(
      "\\\\midrule\\\n",
      "\\\\textsf{N (Individuals)} & ",
      "\\\\multicolumn{4}{r}{$",
      format(nrow(m4$data), big.mark = ","),
      "$}\\\\\\\\\n",
      "\\\\bottomrule\\\n"
    )
  )


# Save table to disk

sink(file = here("_paper", "_assets", "tabA4.tex"))
cat(table)
sink()



# 6. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "015_reg_tables.txt"))


# One last thing...

thanks()


