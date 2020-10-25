# Political Surveys Bias Voters' Self-Reported Economic Perceptions
# Script 5: Ordinal Explainer Figure

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
library(ggpubr)
library(here)




# 2. Create density plots -------------------------------------------------

# We'll create three plots that explain to the reader how ordered regression
# models work. First, we'll specify the distributions we need.

dists <- 
  tribble(
    ~group, ~line,  ~colour, ~mean, ~sd,
    1,   "a",    "red",     0,    1,
    2,   "a",   "grey",     0,    1,
    2,   "a",    "red",     2,    1,
    3,   "a",   "grey",     0,    1,
    3,   "a",    "red",     0,  0.5,
    4,   "a",   "grey",     0,    1,
    4,   "a",    "red",     0,    2
  ) %>% 
  mutate(
    colour =
      colour %>% 
      factor(
        levels = c("grey", "red")
      )
  )


# Next, we'll use these distributions to create a rudimentary plot. We'll
# also organise things a little and make use of three separate facets. We
# will also add vertical lines that represent the threshold parameters too.

prt1 <- 
  dists %>% 
  ggplot(
    aes(
      y = line,
      dist = "norm",
      arg1 = mean,
      arg2 = sd,
      colour = colour,
      linetype = colour
    )
  ) +
  facet_wrap(~ group,
             nrow = 1,
             labeller =
               as_labeller(
                 c(`1` = "Baseline",
                   `2` = "Shift",
                   `3` = "Compression",
                   `4` = "Dispersion")
               )
  ) +
  geom_vline(xintercept = -1.68,
             linetype = "dotted",
             colour = bailey_colours("grey8"),
             size = 0.5) +
  geom_vline(xintercept = -0.5,
             linetype = "dotted",
             colour = bailey_colours("grey8"),
             size = 0.5) +
  geom_vline(xintercept = 0.5,
             linetype = "dotted",
             colour = bailey_colours("grey8"),
             size = 0.5) +
  geom_vline(xintercept = 1.68,
             linetype = "dotted",
             colour = bailey_colours("grey8"),
             size = 0.5) +
  stat_dist_slab(fill = NA,
                 slab_size = 0.8)


# Now, we'll change the line types so that the baseline takes a dashed line
# when it features on other plots and all others take a solid line instead.
# We'll also reduce some of the visual clutter too.

prt1 <- 
  prt1 +
  scale_linetype_manual(values = c("dashed", "solid")) +
  scale_colour_manual(values = bailey_colours("grey2", "red")) +
  theme_bailey() +
  theme(axis.text.y = element_blank(),
        axis.title.x = element_blank())


# The next step is to add some arrows to the plot that show the reader what
# is happening to each distribution at each point in the process. This is a
# little tricky, so we need to create some data first. There are going to be
# five data frames, one for the shift facet and two each for dispersion and
# compression.

sgmt1 <- 
  tibble(
    x = 0, xend = 2,
    y = 1.55, yend = 1.55,
    line = "a",
    group = 2
  )

sgmt2 <- 
  tibble(
    x = -3, xend = -1,
    y = 1.55, yend = 1.55,
    line = "a",
    group = 3
  )

sgmt3 <- 
  tibble(
    x = 3, xend = 1,
    y = 1.55, yend = 1.55,
    line = "a",
    group = 3
  )

sgmt4 <- 
  tibble(
    x = -1, xend = -3,
    y = 1.55, yend = 1.55,
    line = "a",
    group = 4
  )

sgmt5 <- 
  tibble(
    x = 1, xend = 3,
    y = 1.55, yend = 1.55,
    line = "a",
    group = 4
  )


# Having created these data, we can now incorporate them into our plot.

prt1 <- 
  prt1 +
  geom_segment(data = sgmt1,
               aes(
                 x = x, xend = xend,
                 y = y, yend = yend
               ),
               size = 0.5,
               colour = "black",
               arrow = arrow(ends = "last",
                             length = unit(.03, "in"),
                             type = "closed"
               ),
               inherit.aes = FALSE) +
  geom_segment(data = sgmt2,
               aes(
                 x = x, xend = xend,
                 y = y, yend = yend
               ),
               size = 0.5,
               colour = "black",
               arrow = arrow(ends = "last",
                             length = unit(.03, "in"),
                             type = "closed"
               ),
               inherit.aes = FALSE) +
  geom_segment(data = sgmt3,
               aes(
                 x = x, xend = xend,
                 y = y, yend = yend
               ),
               size = 0.5,
               colour = "black",
               arrow = arrow(ends = "last",
                             length = unit(.03, "in"),
                             type = "closed"
               ),
               inherit.aes = FALSE) +
  geom_segment(data = sgmt4,
               aes(
                 x = x, xend = xend,
                 y = y, yend = yend
               ),
               size = 0.5,
               colour = "black",
               arrow = arrow(ends = "last",
                             length = unit(.03, "in"),
                             type = "closed"
               ),
               inherit.aes = FALSE) +
  geom_segment(data = sgmt5,
               aes(
                 x = x, xend = xend,
                 y = y, yend = yend
               ),
               size = 0.5,
               colour = "black",
               arrow = arrow(ends = "last",
                             length = unit(.03, "in"),
                             type = "closed"
               ),
               inherit.aes = FALSE)


# This needs a little more visual tweaking before we can move onto the bottom
# half of the plot. This will involve going into some specific detail using
# the theme() function. We'll also add a y-axis title to tell the reader that
# they're looking at a probability density function.

prt1 <- 
  prt1 +
  scale_x_continuous(breaks = c(-4, 0, 4),
                     labels = c("-", "0", "+")) +
  scale_y_discrete(expand = c(0, 0)) +
  coord_cartesian(xlim = c(-4, 4)) +
  labs(y = "Latent Prob. Density") +
  theme(legend.position = "none")



# 3. Create mass plots ----------------------------------------------------

# Again, we need to make up some data to reflect the changes that we imply
# in our density plots. This time, we'll use a tibble so that we can repeat
# things in a structured manner.

dists <- 
  tibble(
    group = rep(c("Baseline", "Shift", "Compression", "Dispersion"), each = 5),
    resp = rep(1:5, 4),
    prob = c(
      rep(0.2, 5),
      c(0.01, 0.05, 0.12, 0.26, 0.56),
      c(0.02, 0.22, 0.52, 0.22, 0.02),
      c(0.35, 0.11, 0.08, 0.11, 0.35)
    )
  ) %>% 
  mutate(
    group =
      group %>% 
      factor(
        levels = c("Baseline", "Shift", "Compression", "Dispersion")
      )
  )


# Now we'll plot the resulting data, again using facets to make everything
# look nice. First we'll make a set of new data that lets use plot different
# horizontal lines on each facet.

hline <- 
  tibble(
    int = c(0, .2, .2, .2),
    group = c("Baseline", "Shift", "Compression", "Dispersion")
  ) %>% 
  mutate(
    group =
      group %>% 
      factor(levels = c("Baseline", "Shift", "Compression", "Dispersion"))
  )

prt2 <- 
  dists %>% 
  ggplot(
    aes(
      x = resp,
      y = prob
    )
  ) +
  facet_wrap(~ group,
             nrow = 1) +
  geom_hline(data = hline,
             aes(yintercept = int),
             linetype = "dashed",
             colour = bailey_colours("grey2"),
             size = 0.5) +
  geom_col(colour = bailey_colours("red"),
           fill = bailey_colours("red9"),
           size = 0.5,
           width = 0.85) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(ylim = c(0, .6))


# These are almost done. All that remains is to sort out the visual elements
# again and add some more informative labels.

prt2 <- 
  prt2 +
  labs(y = "Observed Prob. Mass") +
  theme_bailey() +
  theme(strip.text = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y = element_blank())



# 4. Complete plots -------------------------------------------------------

# First, we'll combine the plots into a single plot

ord_fig <- ggarrange(prt1, prt2,
                     nrow = 2,
                     heights = c(1, .875))


# And finally we'll give the figure a title

ord_fig <- 
  annotate_figure(ord_fig,
                  top = text_grob("Extending the Ordered Regression Model",
                                  family = "Cabin",
                                  face = "bold",
                                  size = 12,
                                  just = "left",
                                  hjust = 0.93))



# 4. Replication details --------------------------------------------------

# Save session information

save_info(here("_output", "_session_info", "006_ordinal_fig.txt"))


# One last thing...

thanks()

