---
title: |
  | Supplementary Materials: Political Surveys
  | Bias Self-Reported Economic Perceptions
author: |
  | Jack Bailey\thanks{Research Associate, Department of Politics, The University of Manchester, UK. If you have any comments or questions, feel free to contact me either by email (\href{mailto:jack.bailey@manchester.ac.uk}{jack.bailey@manchester.ac.uk}) or on Twitter (\href{https://www.twitter.com/PoliSciJack}{@PoliSciJack}).}
  |
  |
  | 1. Supplementary Material A: Power Analysis
  | 2. Supplementary Material B: Prior Distributions for Ordered Regression Models
  | 3. Supplementary Material C: Robustness Checks
indent: yes
fontsize: 12pt
geometry: margin = 1.15in
subparagraph: yes
compact-title: false
linkcolor: black
urlcolor: violet
citecolor: black
bibliography: _assets/master.bib
biblio-style: _assets/apsr.bst
classoption: a4paper
output: 
  bookdown::pdf_document2: 
    latex_engine: xelatex
    toc: false
    keep_tex: false
    keep_md: true
    includes:
      in_header:
        - _assets/rmd-preamble.tex
    number_sections: false
    highlight: kate
    fig_caption: true
---
<!-- Latex setup -->

\doublespacing

\thispagestyle{empty}
\clearpage

\pagebreak

\setcounter{page}{1}



\setcounter{table}{0}

\renewcommand{\thetable}{SM\arabic{table}}

\setcounter{figure}{0}

\renewcommand{\thefigure}{SM\arabic{figure}}

\setcounter{page}{1}


# Supplementary Material A: Power Analysis

![(\#fig:power-plot)Outcomes from 6,000 simulation-based power analyses, ordered by lower 95% credible interval. In scenario 1, I assume that political survey effects account for the total effect I find in the observational data. In scenario 2, I assume instead that political survey effects account for half of this effect. All samples achieve 80% in scenario 1. Only a sample of 2,500 achieved 80% power in scenario 2.](supplementary_materials_files/figure-latex/power-plot-1.pdf) 

Before fielding my experiment, it was essential that I determine an appropriate sample size. To do so, I conducted a simulation-based power analysis. This approach was necessary as my dependent variable was ordinal. Unlike continuous data, it is not possible to conduct power analyses for ordinal data by hand. It was important that the effect sizes I used in my power analysis be of a realistic and reasonable size. I fit a similar ordered-probit model to the one I discussed above to data from wave 16 of the British Election Study Internet Panel [@fieldhouse2020a]. I then used the regression parameters to establish informative prior expectations for what effect sizes I might expect.

The BESIP data are observational. As such, the resulting estimates tell us only the net effect that respondents' past voting behavior and the political survey context have on the economic perceptions that they report. We do not, however, know what proportion of these effects each accounts for. I proposed two hypothetical scenarios as the basis of my power analysis. In the first, I assumed that political survey effects accounted for the total effect I observed in the observational data. In the second scenario, I instead assumed that they accounted for only half of it. I then simulated 1,000 experiments for each across three sample sizes ($n =$ 1,500, $n =$ 2,000, and $n =$ 2,500). These matched the blocked structure of my experiment. To make sure that my results did not depend on the random seed I used to simulate my data, I incremented it by one for each simulation. This gave 6,000 simulated experiments in total. In simulating my data, I focused on the treatment's effect on incumbent partisans. Further, I set my desired level of power at 80%. As I expect this effect to be positive, this implies that 80% of the effects from my simulation should have a lower 95% credible interval exceeds zero.

Figure \@ref(fig:power-plot) shows the outcomes of all 6,000 simulated experiments, ordered by their lower 95% credible interval. For scenario 1, all sample sizes achieved the desired level of power. Indeed, every simulated experiment yielded estimates that were greater than zero matter the sample size. This was not the case for scenario 2. Instead, every sample size included at least some simulations with lower 95% credible intervals that did not exceed zero. Here, a sample size of 1,500 corresponded with a power level of 60%; 2,000 with a power level of 74%; and 2,500 with a power level of 84%. Thus, I opted for the latter to exceed 80% power.

\pagebreak


# Supplementary Material B: Prior Distributions for Ordered Regression Models

As I discuss in my methods section above, my experiment includes an outcome variables that is ordinal rather than continuous or binary. Though others often treat these data as though they are continuous for the sake of convenience, this practice is prone to a whole host of serious inferential pitfalls. Further, though more robust, almost all conventional ordered regression models face similar issues. As such, I use Bayesian methods to implement an extended ordered regression model that overcomes these problems, thereby allowing me to estimate any treatment effects in a principled manner that respects the nature of the data.

Though similar, the Bayesian approach to statistical analysis does introduce some points of difference compared to the classical statistics that dominates much political science research. Most notably, it requires that one specify a prior distribution over each parameter in one's model before fitting it to the data^[For an introduction to Bayesian statistics and Bayesian methods, see @mcelreath2020; @lambert2018; @kruschke2017; @kruschke2015]. As well as allowing us to shift focus from the likelihood (*"what is the probability of the data given the hypothesis?"*) to the posterior distribution (*"what is the probability of the hypothesis given the data?"*), these "priors" also serve two useful purposes. First, they allow us to incorporate any pre-existing knowledge that we might be privy to into our models. This might include the results of a previous analysis (thereby having our model expect results similar to the previous case before it sees the data) or simply our understanding of the nature of the model and what values it is reasonable for certain parameters to take (for example, we know that it is not possible for probabilities to be negative). Second, they make our models skeptical by nature and shrink any parameter estimates towards the prior. This "regularization" protects against over-fitting common to maximum likelihood-based approaches, especially where sample sizes are small [@mcelreath2020; @lambert2018; @gelman2014a].

In this case, there is little existing evidence on which to draw. Though others have fielded the same economic perception items in the past, they have tended to do so in an observational, rather than an experimental, context. As a result, I do not have a good idea about what values my parameters might take before I fit the model to the data. To complicate matters further, ordered regression models have many moving parts, which interact with one another to produce the implied ordinal outcome. To overcome this complication, I take a principled approach below and decide my priors based on a series of prior predictive simulations. In doing so, I seek to ensure that all my decisions be conservative so that any treatment effects I estimate will be robust. This has two implications. First, that I should assume all effects to be zero before fitting my model to the data. Second, that I should also assume all possible combination of response probabilities to be equally likely. The various ordered regression models that I present in this paper rely on the same three sets of parameters, each of which serves a distinct function. I discuss each specific parameter in turn, below.


## Threshold Parameters

Ordered regression models work by translating between an ordinal variable that we observe and a continuous variable that we do not. To perform this feat, they rely on a series of threshold parameters that split the latent continuous distribution into as many segments as their are response options. The area of each segment then corresponds to the probability that the response option that it represents will occur. Absent any knowledge about the nature of the data, the most conservative assumption that we can make is that any possible combination of responses is as likely as any other.

As we measure each response option in terms of its probability of occurring, it is worth also thinking on the probability scale when setting our priors. Thus, given this assumption, we should expect the probability of a threshold parameter landing on any point on the probability scale to be constant. The issue then is to find a prior on the latent probit scale on which the model operates, which ranges from $-\infty$ to $+\infty$, that gives a flat prior on the probability scale that we really care about, which is bounded by 0 and 1. Fortunately, the answer is relatively well-known: a Normal(0, 1) prior on the former gives a uniform prior on the latter.

![(\#fig:thresh-plot)Before seeing the data, the most conservative assumption that we can make about the distribution of response options is that each combination is as likely as any other. This implies that prior thresholds must be able to take any possible value on the probability scale, conditional on the constraints of the model itself. A Normal(0, 1) prior on the latent probit scale yields fulfills this requirement and implies a flat prior on the probability scale. The figure above shows the resulting prior threshold parameters. Note also that the probability of a threshold occurring at any point on the scale is constant over the entire range of the scale (with any deviations arising only due to random noise in the simulation process).](supplementary_materials_files/figure-latex/thresh-plot-1.pdf) 

The four left-most panels of figure \@ref(fig:thresh-plot) show the resulting prior distributions that this collective prior over all thresholds implies for each specific threshold. The right-most panel instead shows the implied prior over the whole probability scale (i.e. the distribution that we would find were we to stack each threshold distribution on top of each other). As the histograms in the figure make clear, the implied priors for each threshold are non-informative and, in all cases, take a wide range of possible values. For example, the priors shown here allow for a non-zero probability that the first threshold occurs as high as the $80\%$ mark and the fourth threshold as low as $20\%$. That the first response option corresponds to respondents reporting that the economy "got a lot worse" and the last that it has "got a lot better" reaffirms just how non-informative these priors really are.

Though we do not specify priors for each specific threshold, these prior predictive simulations suggest that they take their own distinctive shapes nonetheless. This phenomenon arises due to the constraints that both the prior and the model impose on the values that these parameters can take. For example, each threshold is constrained to take only values smaller than those of the threshold that follow it. As a result, it is not possible for any threshold to cover the entire space as this would leave the others with nowhere to go. Likewise, the collective nature of the prior means that the priors for each threshold *must* result in a flat prior overall. The result is the set of symmetrical distributions that we see above.


## Beta Parameters

Threshold parameters segment the latent outcome distribution, though do not move it. Beta parameters, instead, shift the latent distribution up and down its scale. This movement then serves to shift the probability mass of each observed response option in turn. As such, we can interpret beta parameters much like regression coefficients in linear and logistic regression models, which perform a similar role.

Before seeing the data, the most conservative assumption that one can make about these effects is that they are equal to zero (i.e. that they are null). Doing so is simple and, in the absence of any better information, uncontroversial. More difficult however is determining how uncertain these priors should be. One the one hand, a tight prior around zero will be very conservative, but perhaps to the extent that it ignores perfectly informative data. On the other, a loose prior will pay closer attention to the data, but perhaps to the extent that it will result in over-fitting.

For models with continuous outcomes, things are straightforward. If the prior is very wide, then it is also likely to cover the full range of plausible values that its respective parameter might take. But ordinal variables are not continuous and, as a result, this common practice can lead to perverse implications. Unlike models with continuous outcomes, wide priors on the latent probit scale *do not* give wide priors on the outcome scale. This is because the outcome scale takes only a finite set of discrete values. As a result, wide priors on the latent probit scale instead imply U-shaped priors due to probability mass piling up at the extremes. This problem then multiplies --- quite literally --- where the data include variables that exhibit a high degree of variation (for example age, which in the study of voting behavior might take any value between 18 and 100) or where the sum of all variables is large (such as when a model contains many parameters).

![(\#fig:beta-plot)While it is common to set wide priors on beta values where the outcome is continuous, such "non-informative" priors have perverse consequences when the outcome is ordered. This is because they make the latent scale too diffuse, thereby concentrating almost all of the prior probability mass at the two extremes of the observed ordinal outcome scale. Further, models that include many independent variables or independent variables that take extreme values worsen this problem further.](supplementary_materials_files/figure-latex/beta-plot-1.pdf) 

Figure \@ref(fig:beta-plot) displays this phenomenon across different priors and different values of beta. Where these sum to zero, only the thresholds determine the response distribution, which I fix to ensure that each response has a prior probability of $20\%$ where betas sum to zero. As the figure shows, when this sum exceeds zero the prior probability of responding with either a 1 or a 5 increases. This is true for all priors, though the effect is most pronounced where the prior standard deviations are large. In each model in this paper, the sum of parameters increases where respondents voted at the last election or are in the treatment group. In light of this, using a prior on beta with a large standard deviation is akin to assuming that these participants are more likely to say either that the economy has "got a lot worse" or "got a lot better". Perhaps counter-intuitively, smaller standard deviations are, thus, less informative. Thus, I use the least informative prior --- Normal(0, 0.25) --- for all beta values in my models.


## Delta Parameters

![(\#fig:delta-plot)Setting diffuse priors on delta parameters can also have perverse consequences. In this case, they instead concentrate the prior probability mass in the middle and at the two extremes of the observed ordinal variable. Again, this is likely to be worse where models also include many independent variables or independent variables that take extreme values.](supplementary_materials_files/figure-latex/delta-plot-1.pdf) 

Whereas beta parameters shift the latent outcome distribution, delta parameters compress or disperse it at a given point. This redistributes the observed outcome's probability mass towards central or extreme responses, conditional on its place on the scale. As in the previous case, the most conservative assumption that we can make before seeing the data is to expect these parameters to be equal to zero. Where this is true, the standard deviation of the latent outcome distribution does not vary across participants. Again, this is simple to achieve and, again, things become more complicated when it comes to setting the standard deviation. The problem is the same as before: large standard deviations imply more, not less, informative outcomes. 

Figure \@ref(fig:delta-plot) shows the implication that different priors and different values of delta have on the implied prior outcome distribution. As before, I fix all thresholds to imply an equal chance of any response option being selected and fix all beta parameters to 0. While wide priors on the beta parameters produced U-shaped distributions, wide priors on the delta parameters produce crown-like distributions. Note, however, that this pattern is conditional on the choice of thresholds and that U-shaped distributions may arise here too under different circumstances. As before, each response has an equal probability where the sum of delta parameters is zero. As this sum increases, the central response option becomes much more likely and extreme responses somewhat more likely. This implies that tighter standard deviations are also less informative in this case too. Given this, I opt to use a Normal(0, 0.25) prior on my delta parameters.

\pagebreak


# Supplementary Material C: Robustness Checks

There are three plausible objections to the results I report above. First, that the treatment effects occur due to some mechanism other than partisan bias. Second, that the theory does not generalize to other types of electoral identification. And, third, that the results are sensitive to my model specification. I test each below. The first tests if the treatment mechanism relies on partisan bias. To do so, I apply the same test to participants' reported *personal* economic perceptions. Past research finds that these show little sensitivity to party identification. The second tests if the theory generalizes to other types of identification. In particular, voting behavior at the 2016 referendum on European Union membership. The third tests if the findings are robust to different methods. In this case, by substituting ordered regression for multinomial regression instead.


## Personal Economic Perceptions and Partisan Bias as a Potential Mechanism

![(\#fig:pers-plot)Political surveys do not cause voters to report different perceptions of their own personal finances (left panel). This is unsurprising, since prior research shows that they are much less sensitive to party identification. Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. In general, treatment effects showed the expected signs. Incumbent voters were more positive. Likewise, opposition voters were more negative. Even so, in all cases, the distribution of treatment estimates were centered on small values and had a plausible chance of being practically-equivalent to zero. Here, density plots show the posterior distribution of conditional average treatment effects. Further, black bars show their 95% credible intervals and point estimates their medians.](supplementary_materials_files/figure-latex/pers-plot-1.pdf) 

Above, I assume that my findings result from partisan bias. This seems reasonable given existing research [@devries2018; @bartels2002; @conover1987]. Even so, a skeptic might argue that I have not yet provided good evidence that this is indeed the case. Instead, they might argue that some other mechanism is reasonable for my findings. As a result, the pattern that I observe might also apply to any other dependent variable. This is a reasonable objection, as my design does not allow me to tease apart any intermediary steps in the causal chain between survey context and reported economic perceptions. Fortunately, there are ways to reduce this uncertainty. One is to test how the treatment affects a similar item that we know suffers from little partisan bias. Voters' perceptions of their own personal finances are on such possibility. Like national-level items, these too have their origin in consumer confidence surveys [@katona1951]. But, unlike national-level items, they are much less sensitive to partisan bias. This makes sense. After all, many would argue that the government is less accountable for any one person's well-being than it is for the well-being of the nation as a whole [@lewis-beck2017; @lewis-beck2000; @paldam1981; @kinder1981; @kinder1979; though see @tilley2018].

Figure \@ref(fig:pers-plot) shows how the treatment affected the personal economic perceptions that my participants reported. As before, I condition these estimates on prior voting behavior for the same reasons as above. In this case, all treatment effects have the expected signs. That is, incumbent supporters are more positive and opposition supporters more negative under the treatment. This might, then, suggest the presence of at least some partisan bias. Yet, in all cases, point estimates are small. These range in size from only -0.4 (95% CI: -1.9 to 1.1) to 1.9 percentage points (95% CI: -1.4 to 5.2). Further, these effects have 95% credible intervals that, in all cases, are very uncertain.

Taken together, these results suggest little evidence that political surveys affect the personal economic perceptions that respondents report. Were some other mechanism responsible for the treatment effects I find, this might not be the case. Instead, I find that the treatment might have a similar effect for both items. Instead, both sets of results are consistent with existing theory and the argument that I present above. That is, respondents must have a reason to assign responsibility to the government if political surveys are to prime respondents to respond in a different way. This does not seem to be the case for perceptions of one's personal finances. Instead, they appear to exhibit little partisan bias, leaving the treatment with nothing to manipulate. Of course, it is never possible to rule out any other mechanism with absolute certainty. Still, these results do at least make such a possibility seem much less likely.


## Generalization of Treatment Effects Across Different Types of Electoral Identification

If the theory that underpins my analysis is robust, it should generalize to other types of political identification. The British case is useful here. Due to the 2016 referendum on EU membership, the country now has *two* forms of electoral identification^[And yet more still in Scotland, where unionist versus nationalist identities rose to prominence after the 2014 referendum on Scottish independence.]. The first is conventional party identification. The second is identification with either the Leave or Remain side at the EU referendum. Further, recent evidence shows that the latter also affect self-reported economic perceptions [@fieldhouse2020; @sorace2018]. As the Leave side won, supporting it is, for all intents and purposes, akin to supporting the incumbent party. By the same logic, supporting Remain is now akin to supporting an opposition party. Accordingly, we should expect any treatment effects to generalize to EU referendum identification in the same way that they do to party identification.

\input{_assets/tab1.tex}

![(\#fig:eu-plot)Political surveys cause participants to report different perceptions of the national economy, conditional on their voting behavior at the 2016 referendum on European Union membership. Like with party identification, these effects are most pronounced where they voted for the winning side (Leave). But, in this case, there is also good evidence of a treatment effect on Remain voters too. Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. Density plots show the posterior distribution of conditional average treatment effects. Black bars show their 95% credible intervals and point estimates their medians.](supplementary_materials_files/figure-latex/eu-plot-1.pdf) 

EU and party identification are not unrelated. But, the former does still cut across the latter to a meaningful extent. Table \@ref(tab:tab1) makes this clear. It shows the proportion of participants who voted for each combination of options at the 2016 referendum on EU membership and the 2017 general election. As we can see, participants who voted for the incumbent Conservative Party in 2017 most often voted to leave in 2016. Likewise, those who voted for an opposition party most often voted to remain. But this is not true in all cases. For example, 27.3% of participants who voted for the incumbent Conservative Party also voted to remain in the EU. Similarly, 26.4% of participants who voted for an opposition party also voted to leave. Further, 12.1% of participants voted only in 2016. Thus, we should not expect treatment effects for EU identification to be mere reflections of those across party identification.

Fortunately, the fourth question on the political survey primed voters to consider how they voted at the 2016 referendum (see appendix). Figure \@ref(fig:eu-plot) shows the corresponding treatment effects. In this case, Leave supporters in the treatment group were -4.7 percentage points (95% CI: -8.3 to -0.9) less likely to report either that the economy "got a little worse" and -4.6 percentage points (95% CI: -7.3 to -1.9) less likely to say that it "got a lot worse". They were also 6.3 percentage points (95% CI: 2.5 to 10.0) more likely to report that it had "stayed the same" and 2.9 percentage points (95% CI: -0.1 to 5.9) more likely to report that it "got a little better". Again, almost no one said that the economy "got a lot better" and there was no meaningful treatment effect (0.1, 95% CI: -0.9 to 1.0).

Those who voted Remain also showed similar effects to opposition voters. Yet they were much more likely to say that the economy "got a lot worse" in the last twelve months. This effect was large (4.4, 95% CI: -0.5 to 9.7). Further, though its 95% credible interval crossed zero, 96% of the posterior distribution was greater than zero. Thus, we can be reasonably confident that the true effect is, in fact, greater than zero. Likewise, given these results, we can also be confident that the treatment generalizes to other types of electoral identification too.


## Sensitivity of Treatment Effects to Modeling Assumptions

Ordered regression models estimate effects that are consistent across threshold parameters and, thus, across responses. This is known as the proportional odds assumption [@agresti2010; @mccullagh1980]. Consider the present case. The treatment has a positive effect on the national economic perceptions that incumbents report when measured on the probit scale (see table 1). This is why they are more likely to say that the economy "got a little better" or "stayed the same" and less likely to say that the economy "got a little worse" or "got a lot worse". But, of course, this assumption may not hold. Instead, the treatment might have a unique effect on each response option.

![(\#fig:multi-plot)Using a multinomial rather than an ordinal model does little to change the results. We still find that political surveys cause incumbent voters to report more positive economic perceptions (left panel). Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. Density plots show the posterior distribution of conditional average treatment effects. Further, black bars show their 95% credible intervals and point estimates their medians.](supplementary_materials_files/figure-latex/multi-plot-1.pdf) 

To relax this assumption, we can use multinomial regression instead. Figure \@ref(fig:multi-plot) shows the resulting estimates from such a model. Note that the multinomial model is less efficient and, thus, estimates tend to be less precise. Even so, they still lead to the same conclusion: that political survey effects are most clear where participants voted for the incumbent at the last election. Here, incumbent voters were -7.3 percentage points (95% CI: -13.4 to -1.6) less likely to say that the economy "got a little worse" and -2.5 percentage points (95% CI: -5.5 to 0.7) that it "got a lot worse". They were also 5.8 percentage points (95% CI: 0.0 to 11.6) more likely to say that the economy had "stayed the same" and 3.8 percentage points (95% CI: -0.3 to 8.3) more likely to say that it "got a little better". Results for opposition supporters and non-voters differ little to the results in figure 3. Further, there appear to be no difference in the propensity of respondents to answer "Don't know" under the political survey treatment compared to the non-political survey control. As such, my conclusions appear robust to both model specification and missing data.

\pagebreak

\input{_assets/tabA2.tex}

\pagebreak

\input{_assets/tabA3.tex}

\pagebreak

\input{_assets/tabA4.tex}

\pagebreak

# References

<div id="refs"></div>
