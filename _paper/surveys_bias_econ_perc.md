---
title: |
  | Political Surveys Bias Self-Reported
  | Economic Perceptions\thanks{Thank you to Ed Fieldhouse, Jon Mellon, Marta Cantijoch, and Rosie Shorrocks for reading and commenting on early versions of this paper. Thank you also to the three anonymous reviewers and Eric Plutzer for their helpful suggestions, Chris Wlezien for hosting me at UT Austin where I wrote the majority of this paper, and Adam McDonnell and Jemma Connor at YouGov for helping to field my experiment.}
author: |
  | Jack Bailey\thanks{Research Associate, Department of Politics, The University of Manchester, UK. If you have any comments or questions, feel free to contact me either by email (\href{mailto:jack.bailey@manchester.ac.uk}{jack.bailey@manchester.ac.uk}) or on Twitter (\href{https://www.twitter.com/PoliSciJack}{@PoliSciJack}). For replication files, see https://doi.org/10.7910/DVN/GSEXCC}
abstract: |
  | If voters are to hold governments to account for the state of the economy, they must know how it has changed. Indeed, this is a prerequisite for democratic accountability. Yet the perceptions that voters report often show signs of clear partisan bias. At present, we do not know if this bias is real or instead due to priming in political surveys. To test this, I assign subjects at random to either a political or non-political survey. I then record their economic perceptions and compare the results for each group. I show that political surveys do worsen partisan bias, though only among supporters of the incumbent party. Still, much partisan bias remains unexplained, even in the non-political condition. So, while economic perception items remain biased, we can at least be sure that most people respond to them in a similar way no matter the survey context.
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




# Introduction

To hold governments to account for the state of the economy, voters must first know how it has changed. Indeed, this is an essential prerequisite for democratic accountability [@healy2013; @ashworth2012]. Thus, voters should notice that conditions improve when the economy grows and worsen when it shrinks. Just as this variation in perceptions is important, so too are its consequences. If voters are to reward and punish appropriately, then they should be more likely to support the incumbent where they also think that the economy has improved. This two-step process --- first, of economic updating; second, of electoral sanctioning --- is crucial for good governance. Rather than force voters to suffer fools, it lets them "kick the rascals out" when they fail to live up to expectations [@stegmaier2019].

Though this idea has great normative appeal, reality often falls short. Voters are not so dispassionate when it comes to judging economic management. Instead, all manner of considerations influence the decisions that they make. For instance, voters use their pre-existing beliefs to process new information. Evidence of this behavior is rife in political surveys, where respondents often report economic perceptions that show clear signs of partisan bias: those who support the incumbent tend to be more positive, and those who support the opposition more negative, than those who support no party at all [for recent evidence of this phenomenon, see @bailey2019; @bisgaard2019; @devries2018].

Given the potential ramifications, much work now focuses on mitigating this bias. Yet we still do not know if it is meaningful or, instead, the result of partisan priming in political surveys. I test this possibility in this paper using new survey experimental data collected during the 2019 UK General Election campaign.

I find that political surveys worsen partisan bias in voters' self-reported economic perceptions. But this is true only for those who voted for the incumbent at the last election. What's more, much partisan bias remains unexplained. Thus, while economic perception items are far from perfect, survey researchers and economic voting scholars can at least be sure that most partisan bias remains no matter the survey context.


# Economic Perceptions, Partisan Bias, and Political Surveys

Party identification biases the economic perceptions that voters report [see, for example, @devries2018; @bartels2002; @conover1987]. What's more, this bias serves to undermine accountability mechanisms that make democracy possible [@healy2013; @anderson2007]. The reason for this is simple. Partisan voters report economic perceptions that paint their party in a positive light. For example, incumbent supporters tend to report more positive economic perceptions. Opposition supporters, instead, tend to report more negative ones. Thus, we cannot be sure that voters will hold their party to account on the basis of economic management when in power.

While we know that party identification affects reported economic perceptions, we are less certain *why* this is the case. Clearly, this is an important gap in our understanding. If we do not know what causes partisan bias, we cannot hope to mitigate its worst effects. To this end, the literature on economic perceptions offers four competing hypotheses. Though they are competing, they are not exclusive. Rather, all four likely influence the economic perceptions that voters report to some extent or another. I discuss each below in turn.

The first potential cause of partisan bias is consistency-motivated reasoning [@kunda1990]. This theory holds that voters weight new information based on its congruence with their existing beliefs [@hill2017]. This behavior weakens voters' ability to hold parties to account. Still, it is easy to see why it might play a role in their economic calculus. Existing research shows that the economy shapes wider perceptions of party competence [@green2017]. And this can lead to much psychological discomfort for partisan voters [@groenendyk2013]. Consider an incumbent supporter during the middle of an economic downturn. Such a voter must contend with two conflicting beliefs: that the economy is doing badly and that their own party is managing the economy well. Consistency-motivated reasoning offers them a way out of this conundrum. By down-weighting incongruent information, they can either ignore evidence that the economy is doing badly or, instead, update their perceptions based only on information that pins the blame on someone else [@bisgaard2019; @bisgaard2015; @tilley2011].

The second potential cause is partisan cuing [@brady1985]. Like consistency-motivated reasoning, this too is psychological in nature. It suggests that voters make use of cognitive shortcuts. These shortcuts are necessary because many voters pay little attention to politics [@campbell1960]. As such, they have a hard time when it comes to making political decisions. The partisan cuing literature argues that they resolve this problem by making a simple substitution. Rather than derive their own belief, they rely on their favorite party's position on the issue instead. They may do this either out of party loyalty or the belief that they would have come to the same conclusion were they fully-informed [@ramirez2014; @brader2013a]. Evidence in favor of partisan cuing is most striking where it concerns party elites. @bisgaard2018, for example, show that when the Danish government began to consider the budget deficit in a negative light, its supporters came to do so too, despite not having done so a short time before.

The third potential cause is expressive responding [@schaffner2018; @bullock2015]. Unlike the two previous explanations, it does not rely on voter psychology to explain partisan bias. Instead, it contends that survey respondents use survey items to signal their support for a particular party. For example, a respondent might report that the economy has gotten better not because they really believe it, but because they support the incumbent party. Recent survey experimental evidence shows that expressive responding almost certainly occurs. Though concerned with factual questions, @bullock2015 and @prior2015 run similar experiments where they manipulate the incentive to engage in expressive responding. Respondents in the treatment groups received a small cash reward where they admitted either to not knowing the answer or happened to give the correct answer to a series of factual questions about the economy and other policy-related topics. Respondents in the control groups received no such reward. In both cases, the authors find that partisan disagreement was lower under the treatment than under the control, implying that some responses serve only to signal respondents' party preferences.

The fourth and final potential cause is item-order effects. These occur where the order in which survey questions are asked affects the answers that respondents give. If non-political items precede political ones, they may *personalize* respondents' answers. Likewise, where political items precede non-political ones, they may *politicize* them instead [@sears1983]. Item-order effects are both large and long-lasting. Indeed, even where several buffer items separate them, political questions still come to bias the economic perceptions that respondents report [@wilcox1993]. Further, as many electoral surveys begin by asking their respondents how they voted or how they intend to vote, this politicization of economic perception items is probably common.

Though distinct, all four causes share a common catalyst: the political survey context. That is to say, partisan priming in political surveys might worsen their effects. For the sake of illustration, consider expressive responding. If the survey context implies that the survey administrator does not care about politics, then respondents face fewer incentives to engage in partisan cheer-leading. Likewise, consider motivated reasoning and partisan cuing. If the survey context does not encourage respondents to consider the economy through a partisan lens, then it seems reasonable to expect them to be less likely to rely on partisanship to determine what they think about the state of the economy. It is for this reason that most consumer confidence surveys rarely ask for respondents' party affiliations [@curtin2019].

As a result, the political survey context *itself* might moderate how party identification affects the economic perceptions that voters report. And, given that partisan bias varies direction based on party identification, so too should political survey effects. Thus, we should expect incumbent supporters to be *more* likely to report positive and *less* likely to report negative economic perceptions in political compared to non-political surveys. We should expect opposition supporters, instead, to do the opposite. This implies the two following hypotheses:

\vspace{.5cm}
\setlength\parindent{0em}

\textsf{\textbf{Hypothesis 1:}} Incumbent partisans report *more positive* economic perceptions in political surveys than do similar incumbent partisans in non-political surveys.

\vspace{.5cm}

\textsf{\textbf{Hypothesis 2:}} Opposition partisans report *more negative* economic perceptions in political surveys than do similar opposition partisans in non-political surveys.

\vspace{.5cm}
\setlength\parindent{2em}


# Experimental Design

I use a simple survey experiment to test my hypotheses. The market research and polling company YouGov collected the corresponding data from its panel of eligible British voters^[YouGov uses non-probability samples, *not* convenience samples. It ensures that its data are nationally-representative using "active sampling" [@twyman2008]. This approach has proven robust and the company's surveys often yield results substantively similar to those collected using random probability sampling [@sanders2007].]. Data collection occurred between the 6\textsuperscript{th} and the 8\textsuperscript{th} November 2019.

The British case is especially useful and provides a strong test of my argument for two reasons. First, data collection coincided with the start of the 2019 UK General Election campaign. Thus, my subjects were exposed to a general politicization of the information environment that we might expect to bias their responses in *non-political* surveys too. Any differences between my treatment and control groups are, therefore, likely conservative. Second, data collection also occurred at a time of economic uncertainty. Though the economy was not in recession, it was not growing much either. At the time, GDP data showed that the UK economy had contracted by 0.2% in the previous quarter. This is important as new evidence shows that even strong partisans "get it" when the going gets tough [@bisgaard2019; @devries2018] and that this leads partisan bias to diminish [@bailey2019; @stanig2013]. As such, it seems reasonable to expect the economic circumstances at the time to provide less overall partisan bias for my treatment to manipulate.

In the first stage of the experiment, I drew a blocked sample from YouGov's online panel^[The design is deliberately non-representative to maximize power. As such, I do not weight my data. Regardless, this likely makes little difference. As @miratrix2018 show, also using YouGov data, "sample quantities, which do not rely on weights, are often sufficient" (p. 275).]. The first block contained only those panelists who had voted for the incumbent Conservative Party at the last election in 2017, the second only those who had voted for an opposition party, and the third only those who had not voted at all^[Retention was high. Just 3.5% (91) of respondents failed to finish the survey. Of these, 48 left before being assigned to a condition, 18 left after being assigned to the treatment, and 25 left after being assigned to the control.]. To determine my sample size, I conducted a simulation-based power analysis. The results from 6,000 simulated experiments showed that I would need a sample of around 2,500 respondents to reach a power level of 80%^[For more information, see Supplementary Material, section A.].

I did not have my participants report their voting behavior during the experiment, but instead relied on contemporaneous data that YouGov collected after the 2017 election. As such, misreporting bias or other related issues should be low. Some might argue that it would be better to use participants' *current* party identification and not how they voted in the past. After all, attitudes and choices change over time. While this is a reasonable objection, it is not possible to include such an item without undermining the non-political survey context. Further, using past voting behavior has one particular advantage: voters cannot undo it. This may explain why it appears to exert such a considerable effect on the economic perceptions that voters report in political surveys [@anderson2004].

In the second stage of the experiment, I exploited YouGov's day-to-day operations to administer my treatment. As a large commercial polling company, YouGov runs many simultaneous political and non-political surveys. It also runs them in tandem. As a result, panelists are used to surveys that concern one topic then switch to another. My treatment group first completed a version of YouGov's standard voting intention poll. This includes five questions that concern voting behavior and the perception of party leaders. The control group, instead, completed a survey on dental hygiene. This had an almost identical structure to the political survey. For example, it asked the same number of questions, the same type of questions, and included the same number of response options in all cases. Further, it also used only questions that YouGov had fielded in the past to ensure that it was believable^[The full questionnaire is available in the appendix.]. In all cases, participants had an equal chance of being assigned to the treatment or to the control.

In the third and final stage of the experiment, I again exploited YouGov's day-to-day operations, this time to measure my participants' economic perceptions. After receiving their treatment, both groups saw the topic of the survey switch from politics or dental hygiene to the economy. I then asked them to report their own retrospective economic and financial perceptions. As I used a sample of eligible British voters, I followed the lead of the British Election Study Internet Panel [@fieldhouse2020a] and had my participants answer the two following questions:

- Now, a few questions about economic conditions. How does the *financial situation of your household* now compare with what it was 12 months ago?

- How do you think the *general economic situation in this country* has changed over the *last 12 months*?

These items have their origins in consumer confidence surveys [@katona1951], entered political science via *The American Voter* [@campbell1960], and are now ubiquitous in economic voting research [@lewis-beck2007]^[Note that the consumer confidence surveys from which these items originate rarely field questions of partisanship as they are known to engender emotional states that bias how survey respondents answer economic perception questions [@curtin2019]]. By and large, the literature on economic perceptions and partisan bias focuses only on national-level items [for recent examples, see @dassonneville2019; @anson2017; @hansford2015]. While I do include this item, I also asked my subjects to report their personal financial perceptions too. Doing so serves two useful purposes: it provides a benchmark for any national-level effects and helps to prevent an unusual one-question-long topic.

In both cases, my subjects faced exactly the same response options. They could answer each question on a five-point ordinal scale that ranged from *"1 -- Got a lot worse"* to *"5 -- Got a lot better"*. They could also report that they did not know how either the national or their own personal economic situation compared to what it was 12 months ago. Where this was the case, I removed these participants using list-wise deletion^[List-wise deletion can produce biased estimates if data are not missing completely at random. Still, simulation studies show that list-wise deletion yields less biased estimates than multiple imputation where data are missing not at random [@pepinsky2018]. Even so, I include these data as a robustness check (see Supplementary Material, section C). This does not change my results. Further, participants were no more likely to answer "Don't know" under the treatment than under the control condition.].

![(\#fig:raw-plot)Distribution of responses under the treatment and control. Incumbent partisans (left column) tend to be more positive than nonvoters (right column). Likewise, opposition partisans (middle column) tend to be more negative than nonvoters. Further, these figures also suggest evidence in favor of my first hypothesis that incumbent partisans in the treatment would be more positive than incumbent partisans in the control, though not my second hypothesis that opposition partisans in the treatment would be more negative than opposition partisans in the control.](surveys_bias_econ_perc_files/figure-latex/raw-plot-1.pdf) 

Figure \@ref(fig:raw-plot) shows the raw percentages for each response option stratified by party and treatment status. As we would expect, these show that incumbent partisans are more positive than do nonvoters. Further, this is true under both the treatment and the control. For example, 17.6% of incumbent partisans in the treatment condition (a political survey) said that the economy had gotten a lot or a little better while only 8.9% of nonvoters in the treatment condition said the same. Likewise, opposition partisans were more negative than nonvoters. Among opposition partisans in the treatment condition, 77.2% said that the economy had got a lot or a little worse whereas only 61.6% of similar nonvoters made the same judgment.

Though still descriptive, figure \@ref(fig:raw-plot) also suggests evidence in favor of my first hypothesis. Incumbent partisans in the political survey treatment condition were 3.3% more likely to say that things had gotten better than similar incumbent partisans in the control condition. They were also 10.0% less likely to say that the economy had gotten worse. The data suggest little evidence in favor of my second hypothesis. Opposition partisans in the political survey treatment condition were 1.4% more likely to say that things had gotten better and 2.6% less likely to say that things had gotten worse than opposition partisans in the non-political control condition. While informative, any inferences that we make from these descriptive statistics do not account for the uncertainty inherent in the sample. To do so requires a more rigorous approach, which I describe in greater detail below.


# Modeling Ordinal Outcome Variables

Economic perception items yield ordinal data. Yet many researchers treat them as continuous. This is convenient, as it allows them to estimate treatment effects using only a simple comparison of means. But this simplicity belies drawbacks that include false positives, false negatives, and even estimates with incorrect signs [@liddell2018].

One argument for treating these items as continuous is that while the outcome is ordinal, subgroup means and their differences are continuous. This is true. But it is not clear what such treatment effects even imply. Indeed, when ordinal variables have three or more response options, there are an *infinite* combination of response distributions that could produce any given difference in means.

Better then to model the choices that survey respondents really face: the ordinal variable's various response options. To do so, one might expect to use ordered regression. But these models face similar problems. Figure \@ref(fig:ord-plot) shows why. Ordered regression treats the ordinal distribution that we observe (bottom row) as a function of a continuous one that we do not (top row). It then uses a set of threshold parameters (gray dotted lines) to convert between the two. These divide the latent continuous distribution into as many segments as there are response options. The area between two thresholds then gives the probability of each response occurring.

The first column shows the baseline case. Here, each response has an equal probability. To change this, we can adjust either the latent distribution's mean or its variance. This has three consequences. When we adjust the mean, the latent distribution *shifts* up or down the scale (second column). This alters the area between the thresholds and moves the ordinal distribution in the same direction. When we instead adjust the variance, the latent distribution either *compresses*, squeezing the ordinal distribution's probability mass (third column), or *disperses*, piling up probability mass at the extremes (fourth panel).

![(\#fig:ord-plot)A graphical description of the assumptions of ordered regression. Ordered regression assumes that the observed ordinal scale is a function of a latent continuous one. When the latent scale *shifts*, so too does the probability of selecting a higher value on the observed scale. Likewise, when it *compresses* or *disperses*, the observed scale follows suit. As we can see, each may have a large effect. Yet most ordered regression models account only for shift.](surveys_bias_econ_perc_files/figure-latex/ord-plot-1.pdf) 

As figure \@ref(fig:ord-plot) shows, compression and dispersion can have large effects on the ordinal distribution. Yet conventional ordered regression accounts only for shift. This is a problem, as treatments may affect the outcome without shifting the probability mass to one end or the other. Dealing with this is difficult using Frequentist methods. Thus, in line with recent recommendations [@liddell2018], I use Bayesian methods instead^[Note that Bayesian models require prior distributions. In this case, I specify a set of conservative and weakly informative priors for each parameter. I discuss my choices in greater detail in Supplementary Material, section B.].

My model is as follows. Let $E_{i}$ be person $i$'s reported retrospective economic perceptions. In line with existing economic voting research, this item is measured on a five-point ordinal scale as described above and which takes a value that varies between 1 $=$ "Got a lot worse" and 5 $=$ "Got a lot better". In order to model the data as ordinal, I assume that the observed ordered variable, $E_{i}$, is a function of some latent continuous variable, $E_{i}^{*}$. I then assume that this latent continuous variable follows a normal distribution with mean, $\mu_{i}$, and standard deviation, $\sigma_{i}$:

\begin{equation*}
E_{i}^{*} \sim \text{Normal}(\mu_{i}, \sigma_{i})
\end{equation*}

Likewise, the observed ordinal outcome variable, $E_{i}$, takes a particular value as follows:

\begin{equation*}
E_{i} = k \text{ if } \tau_{k-1} \leq E_{i}^{*} \leq \tau_{k} \text{ for } k = 1, ..., K
\end{equation*}

Here, $\tau_{k}$ for $k \in \{0, ..., K\}$ represent threshold parameters which segment the latent continuous distribution. We fix the $0^{th}$ and $k^{th}$ thresholds equal to $- \infty$ and $+ \infty$, such that $- \infty = \tau_{0} < \tau_{1} < ... < \tau_{k-1} < \tau_{k} = \infty$. As such, the probability that $E_{i} = k$ is:

\begin{equation*}
\text{Pr}(E_{i} = k) = \Phi(\frac{\tau_{k} - \mu_{i}}{\sigma_{i}}) - \Phi(\frac{\tau_{k-1} - \mu_{i}}{\sigma_{i}})
\end{equation*}

Where $\Phi$ is the cumulative distribution function of the normal distribution with mean $\mu_{i}$ and standard deviation $\sigma_{i}$. As I discuss above, both influence the ordinal distribution that we observe. Likewise, both may also vary according either to party preference or treatment status:

\begin{align*}
\mu_{i} &= \beta_{1} T_{i} + \beta_{2} I_{i} + \beta_{3} O_{i} + \beta_{4} (T_{i} \times I_{i}) + \beta_{5} (T_{i} \times O_{i}) \\
log(\sfrac{1}{\sigma_{i}}) &= \delta_{1} T_{i} + \delta_{2} I_{i} + \delta_{3} O_{i} + \delta_{4} (T_{i} \times I_{i}) + \delta_{5} (T_{i} \times O_{i})
\end{align*}

Here, $T_{i}$ takes the value $1$ where person $i$ is in the treatment group. Likewise, $I_{i}$ and $O_{i}$ take the value $1$ where person $i$ voted for the incumbent or an opposition party at the last election, respectively. Rather than model $\sigma_{i}$, I instead model $log(\sfrac{1}{\sigma_{i}})$, thereby fixing $\sigma_{i}$ to 1 for the baseline category (non-voters) for the sake of identification.

Both my first and second hypotheses assume heterogeneous treatment effects. This is why the linear models I fit on $\mu_{i}$ and $\sigma_{i}$ above include interactions between treatment status, $T_{i}$, and incumbent and opposition voting, $I_{i}$ and $O_{i}$. Thus, I test my hypotheses based on the value of $\beta_{4}$ (the shift in latent mean for incumbent supporters under the treatment) and test my second hypothesis based on the value of $\beta_{5}$ (the shift in latent mean for opposition supporters under the treatment). As I use Bayesian methods, the standard decision criterion---a p-value less than 0.05---makes little sense in this case as Bayesian statistics have no equivalent to statistical significance. Instead, I base my decision criterion on each parameter's posterior distribution. In particular, whether or not the parameter's 95% credible interval includes zero.

Though complex, the method that I use is robust to the various problems I discuss above. Still, like any ordered regression model, the parameters that it produces are hard to interpret. Fortunately, as Bayesian models are generative [@lambert2018] we can have them estimate the treatment's effect on the more intuitive probability scale while also incorporating any inherent uncertainty. I do this below, and compute treatment effects for each response category as follows:

\begin{equation*}
\text{ATE}_{k} = \text{Pr}(E_{i} = k | T_{i} = 1) - \text{Pr}(E_{i} = k \mid T_{i} = 0)
\end{equation*}


# Results

Table \@ref(tab:tabA1) shows the resulting parameter estimates from my model. Here, the various mean parameters shift the latent continuous distribution. As we can see, and as we would expect, the political survey treatment appears to have had no effect on the economic perceptions that non-voters reported ($-0.02$, 95% CI = $-0.16$ to 0.12). Likewise, and again as we would expect, incumbent partisans tended to report more positive (0.41, 95% CI: 0.29 to 0.54) and opposition partisans tended to report more negative ($-0.35$, 95% CI: $-0.49$ to $-0.22$) economic perceptions no matter their treatment status.

\input{_assets/tabA1.tex}

In line with my expectations, it appears that political surveys *do* affect the economic perceptions that respondents report in political surveys. As my first hypothesis suggests, incumbent partisans who first completed a political survey tended also to report more positive economic perceptions (0.20, 95% CI = 0.02 to 0.38). There was, however, little support for my second hypothesis. Unlike incumbent partisans, opposition partisans showed little to no difference in the economic perceptions that they reported under the treatment and the control (0.05, 95% CI = $0.14$ to 0.22). Political survey treatment effects may, thus, be limited only to those respondents who voted for the incumbent Conservatives in 2017^[This finding is robust to a range of tests. See Supplementary Material, section C.].

It is interesting to note that the treatment also caused differences in compression and dispersion too. For example, the treatment caused the range of responses that incumbent supporters reported to compress (0.18, 95% CI: 0.03 to 0.33), giving their latent economic perceptions less variance. As a consequence, incumbent partisans were not only more positive under the treatment, they showed a greater consensus too.

![(\#fig:nat-plot)Conditional average treatment effect of the political survey condition. Political surveys cause incumbent voters to report different economic perceptions (left panel). They were less likely to say that the economy had gotten worse and more likely to say that it had"stayed the same" or "got a little better". Density plots show the posterior distribution of conditional average treatment effects. Further, black bars show their 95% credible intervals and point estimates their medians.](surveys_bias_econ_perc_files/figure-latex/nat-plot-1.pdf) 

Figure \@ref(fig:nat-plot) shows how the treatment's consequences on the latent scale affect the choices that my respondents actually made. Here, density plots show the full posterior distribution of each treatment effect, black bars their 95% credible intervals, and point estimates their median. Each density curve reflects the difference in the probability of reporting a given response under the treatment versus the control. Thus, a positive value implies that the political survey treatment increased the probability of a respondent picking a given response by a given number of percentage points compared to similar respondents in the control group.

The left-most panel shows how the treatment affected those who voted for the incumbent party in 2017. As discussed above, incumbent supporters tended to be more positive on the latent response scale. This would suggest that they should also be more positive on the observed one too. This is exactly what we see. Under the treatment, incumbent voters were 3.7 percentage points (95% CI: 1.3 to 6.2) less likely to say that the economy "got a lot worse" and 4.6 percentage points (95% CI: 0.8 to 8.4) less likely to say that it "got a little worse". In comparison, they were 2.8 percentage points (95% CI: -0.3 to 6.2) *more* likely to say that the economy "got a little better".

Interestingly, incumbent partisans appeared no more likely to say that the economy "got a lot better" (0.1, 95% CI: -0.9 to 1.2). This effect was also much more precise than for other responses. Though this may seem unusual, it arises only because almost no one reported that the economy "got a lot better". This is not uncommon, at least in the British case, even when the economy is booming [see @bailey2019]. Finally, those reporting that the economy "stayed the same" made up the difference. These participants were 5.3 percentage points (95% CI: 1.5 to 9.2) more likely to pick this option under the treatment compared to the control.

As the parameter estimates in table \@ref(tab:tabA1) suggest, the effect of taking a political survey was less clear where participants voted for an opposition party at the last election. These subjects were not much more likely to say that the economy "got a lot better" (0.1, 95% CI: 0.0 to 0.3), "got a little better" (1.2, 95% CI: -0.3 to 2.8), or "stayed the same" (1.6, 95% CI: -2.0 to 5.0) where they took the political survey treatment. And, while they were 1.1 percentage points (95% CI: -4.2 to 6.5) more likely to say that the economy "got a lot worse", they were in fact 4.0 percentage points (95% CI: 0.3 to 9.1) *less* likely to say that it "got a little worse". Interestingly, non-voters showed a similar pattern of treatment effects to opposition voters, though were even more muted. This is perhaps unsurprising, given that the participants who comprised this group presumably had little sense of party identification.


# Political Surveys and Partisan Bias

One question remains unanswered: how much partisan bias do political surveys account for? With only a single experiment to draw upon, this is difficult to know. Yet we can approximate this proportion by assuming that my treatment effects represent upper-bounds on the true effect. As I discuss above, my estimates are likely conservative. As such, treating them as an *upper-* and not *lower-*bounds is also conservative as the true value may be larger.

![(\#fig:bias-plot)Estimated partisan bias due to the political survey condition in each wave of the BES Internet Panel. Assuming that my estimates represent upper-bounds, the political survey context accounts for around one-quarter of all partisan bias in the economic perceptions that voters report in fifteen recent waves of the British Election Study Internet Panel (BESIP). Density plots here show the posterior distribution of the proportion of partisan bias attributable to the political survey context. Further, black bars show their 95% credible interval and point estimates their median.](surveys_bias_econ_perc_files/figure-latex/bias-plot-1.pdf) 

Computing the bias *within* the experiment is simple if we use the parameters in table \@ref(tab:tabA1). One need only divide the treatment's main effect and its interaction with partisanship by its main effect, its interaction, and the main effect of partisanship. In the present case, this suggests that around 30.3 percent (95% CI: 11.4 to 47.3) of the partisan bias present in incumbent supporters self-reported economic perceptions is due to the political survey context itself.

While informative, this estimate is limited only to a single case. It would be better to compute a *distribution* of proportions using data from many points in time. The British Election Study Internet Panel, 2014--2023 [@fieldhouse2020a], provides one such source of data. The BESIP includes the national economic perceptions item in fifteen separate waves. These cover the period between April 2014 and November 2019. I fit a similar ordered regression model to each wave of the data then, as the data do not vary the survey context, use the treatment effect from my survey experiment to approximate the proportion of partisan bias due to the political survey context under the assumption that it remains constant.

Figure \@ref(fig:bias-plot) shows the resulting estimates. For incumbent supporters, these range from a low of 20.1 percent (95% CI: 6.7 to 34.0) in wave 6 to a high of 31.7 percent (95% CI: 10.8 to 53.5) in wave 13. The average across all waves is 27.4 percent (95% CI: 9.4 to 45.8), suggesting that around one-quarter of all partisan bias in the economic perceptions that incumbent partisans report is due to political survey effects. The equivalent effects for opposition supporters are much weaker and much more uncertain.


# Discussion and Conclusion

Survey research often proceeds as though survey respondents say what they mean. This is especially true when it comes to studying both the economic vote and voters' economic perceptions. Most often, this research assumes that differences between groups that exist *within the survey* reflect real differences that exist *outside of the survey* [@bullock2019]. My results show that this is not always the case. Some partisan bias arises simply due to the political survey context itself. In particular, I show that incumbent partisans report more positive economic perceptions in political compared to non-political surveys.

Why might this affect incumbent partisans but not opposition partisans? One explanation is that different partisans face different pressures when the economy is middling or poor. First, let us consider opposition partisans. When things are bad, these voters' primed and unprimed responses should coincide. Thus, they should show little difference in partisan bias. Now consider incumbent partisans, who face the opposite pressure. For this group, political priming leads them to report that things have gotten better. As a result, the economic climate causes a gap to open between the perceptions that they report in political and non-political survey contexts. If this is correct, and the state of the economy moderates political survey treatment effects, then future research might find that its effect reverses when the economy is doing well.

As the political survey context worsens partisan bias in self-reported economic perceptions, the most pressing issue is to work out how this affects applied research. For the economic vote, the outcome is mixed. Measuring economic perceptions in political surveys almost certainly worsens economic voting's endogeneity problem [@visconti2017; @evans2010; @evans2006]. Yet any effects appear limited only to incumbent partisans and much partisan bias remains constant. For political science in general, my results raise questions of measurement and validity. After all, these are likely not the only items sensitive to changes in survey context. Take items that measure attitudes towards immigration. It seems reasonable to expect voters to report different attitudes when primed to consider party politics or, say, changes in the labor market or even unrelated topics like dental hygiene.

The consequences are most serious for macro research. This is because small differences at the individual-level can yield large differences at the aggregate-level. Most often, this research aggregates these data in one of two ways. First, they compute the proportion of respondents who say that the economy has gotten better. However, my results suggest that doing so would over-estimate how rosy incumbent supporters think things really are where these figures are split by partisanship [see, for example, @enns2012a]. Second, they compute net economic perceptions. That is, the proportion of respondents who report that the economy has gotten better minus those who report that it has gotten worse. Yet using net figures both over-estimates incumbent positivity and under-estimates incumbent negativity. For example, the results in figure 2 suggest a difference in net economic perceptions of almost 12 percentage points depending on the survey context. Some might argue that this is no issue for economic voting research that most often relies on real economic statistics [though see @lewis-beck2013b]. This might be true. Even so, it could still be a problem for the broader analysis of attitudes and opinions in mass publics.

Future research should consider if political survey effects are constant or, instead, if they vary by external context. For example, I have alluded to the possibility that my results might change in good economic times. Ultimately, this remains to be tested. Nevertheless, my results suggest a fruitful avenue for future research: we might opt not to adjust our *models*, but rather to adjust our *designs*. Research in this vein has already begun [@visconti2017]. One obvious suggestion would be to field separate surveys to measure respondents' party political and non-party political attitudes and beliefs. Though this might be more costly, the gains could be considerable if it reduced nuisance variation that muddies our inferences. As a result, students of the economic vote might gain both a better understanding of how the economy affects voters' behavior and how voters come to update their economic perceptions.

\pagebreak


# References

<div id="refs"></div>


\pagebreak


# Appendix: Questionnaire

\setlength\parindent{0em}

<!-- Column headers -->
\begin{minipage}[t]{0.4\linewidth}
\center{\sffamily{\textbf{Treatment}}}\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\center{\sffamily{\textbf{Control}}}\\
\end{minipage}

<!-- Question 1 -->
\begin{minipage}[t]{0.4\linewidth}
\sffamily \textbf{Q1.} \rmfamily If there were a general election held tomorrow, which party would you vote for?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Conservative}\\
\sffamily \textbf{2.} \rmfamily \emph{Labour}\\
\sffamily \textbf{3.} \rmfamily \emph{Liberal Democrat}\\
\sffamily \textbf{4.} \rmfamily \emph{Scottish National Party (SNP)}\\
\sffamily \textbf{5.} \rmfamily \emph{Plaid Cymru}\\
\sffamily \textbf{6.} \rmfamily \emph{Brexit Party}\\
\sffamily \textbf{7.} \rmfamily \emph{Green}\\
\sffamily \textbf{8.} \rmfamily \emph{Some other party}\\
\sffamily \textbf{9.} \rmfamily \emph{Would not vote}\\
\sffamily \textbf{10.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\sffamily \textbf{Q1.} \rmfamily Imagine that you need to buy toothpaste in the near future, which brand would you choose?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Colgate}\\
\sffamily \textbf{2.} \rmfamily \emph{Sensodyne}\\
\sffamily \textbf{3.} \rmfamily \emph{Aquafresh}\\
\sffamily \textbf{4.} \rmfamily \emph{Oral-B}\\
\sffamily \textbf{5.} \rmfamily \emph{Macleans}\\
\sffamily \textbf{6.} \rmfamily \emph{Arm \& Hammer}\\
\sffamily \textbf{7.} \rmfamily \emph{Crest}\\
\sffamily \textbf{8.} \rmfamily \emph{Some other brand}\\
\sffamily \textbf{9.} \rmfamily \emph{I would not buy toothpaste}\\
\sffamily \textbf{10.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}


<!-- Question 2 -->
\begin{minipage}[t]{0.4\linewidth}
\sffamily \textbf{Q2.} \rmfamily On a scale of 0 (certain NOT to vote) to 10 (absolutely certain to vote), how likely would you be to vote in a general election tomorrow?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{0 -- Certain NOT to vote}\\
\sffamily \textbf{2.} \rmfamily \emph{1}\\
\sffamily \textbf{3.} \rmfamily \emph{2}\\
\sffamily \textbf{4.} \rmfamily \emph{3}\\
\sffamily \textbf{5.} \rmfamily \emph{4}\\
\sffamily \textbf{6.} \rmfamily \emph{5}\\
\sffamily \textbf{7.} \rmfamily \emph{6}\\
\sffamily \textbf{8.} \rmfamily \emph{7}\\
\sffamily \textbf{9.} \rmfamily \emph{8}\\
\sffamily \textbf{10.} \rmfamily \emph{9}\\
\sffamily \textbf{11.} \rmfamily \emph{10 -- Absolutely certain to vote}\\
\sffamily \textbf{12.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\sffamily \textbf{Q2.} \rmfamily On a scale of 0 (not at all important) to 10 (very important), how important do you think dental hygiene is in everyday life?\\
\\
\\
\sffamily \textbf{1.} \rmfamily \emph{0 -- Not at all important}\\
\sffamily \textbf{2.} \rmfamily \emph{1}\\
\sffamily \textbf{3.} \rmfamily \emph{2}\\
\sffamily \textbf{4.} \rmfamily \emph{3}\\
\sffamily \textbf{5.} \rmfamily \emph{4}\\
\sffamily \textbf{6.} \rmfamily \emph{5}\\
\sffamily \textbf{7.} \rmfamily \emph{6}\\
\sffamily \textbf{8.} \rmfamily \emph{7}\\
\sffamily \textbf{9.} \rmfamily \emph{8}\\
\sffamily \textbf{10.} \rmfamily \emph{9}\\
\sffamily \textbf{11.} \rmfamily \emph{10 -- Very important}\\
\sffamily \textbf{12.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}


<!-- Question 3 -->
\begin{minipage}[t]{0.4\linewidth}
\sffamily \textbf{Q3.} \rmfamily Who do you think would make the best Prime Minister?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Boris Johnson}\\
\sffamily \textbf{2.} \rmfamily \emph{Jeremy Corbyn}\\
\sffamily \textbf{3.} \rmfamily \emph{Jo Swinson}\\
\sffamily \textbf{4.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\sffamily \textbf{Q3.} \rmfamily Generally speaking, what type of toothbrush do you use?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Manual}\\
\sffamily \textbf{2.} \rmfamily \emph{Electric}\\
\sffamily \textbf{3.} \rmfamily \emph{I do not have a toothbrush}\\
\sffamily \textbf{4.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}


<!-- Question 4 -->
\begin{minipage}[t]{0.4\linewidth}
\sffamily \textbf{Q4.} \rmfamily In hindsight, do you think Britain was right or wrong to vote to leave the European Union?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Right to leave}\\
\sffamily \textbf{2.} \rmfamily \emph{Wrong to leave}\\
\sffamily \textbf{3.} \rmfamily \emph{Neither right nor wrong}\\
\sffamily \textbf{4.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\sffamily \textbf{Q4.} \rmfamily When brushing your teeth, do you...\\
\\
\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Wet your toothbrush, then apply toothpaste?}\\
\sffamily \textbf{2.} \rmfamily \emph{Apply toothpaste, then wet your toothbrush?}\\
\sffamily \textbf{3.} \rmfamily \emph{Not wet your toothbrush at all}\\
\sffamily \textbf{4.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}


<!-- Question 5 -->
\begin{minipage}[t]{0.4\linewidth}
\sffamily \textbf{Q5.} \rmfamily How well or badly do you think the government are doing at handling Britain's exit from the European Union?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Very well}\\
\sffamily \textbf{2.} \rmfamily \emph{Fairly well}\\
\sffamily \textbf{3.} \rmfamily \emph{Neither well nor badly}\\
\sffamily \textbf{4.} \rmfamily \emph{Fairly badly}\\
\sffamily \textbf{5.} \rmfamily \emph{Very badly}\\
\sffamily \textbf{6.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}
\hfill
\begin{minipage}[t]{0.5\linewidth}
\sffamily \textbf{Q5.} \rmfamily Generally speaking, on average how many times do you brush your teeth every day?\\
\\
\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Never}\\
\sffamily \textbf{2.} \rmfamily \emph{Once}\\
\sffamily \textbf{3.} \rmfamily \emph{Twice}\\
\sffamily \textbf{4.} \rmfamily \emph{Three times}\\
\sffamily \textbf{5.} \rmfamily \emph{More than three times}\\
\sffamily \textbf{6.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}

\pagebreak

<!-- Treatment ends-->
\begin{minipage}[t]{\linewidth}
\sffamily{\textbf{Treatment ends. Subsequent questions are identical for each group.}}\\
\end{minipage}


<!-- Question 6 -- All -->
\begin{minipage}[t]{\linewidth}
\sffamily \textbf{Q6.} \rmfamily Now, a few questions about economic conditions. How does the \emph{financial situation of your household} now compare with what it was 12 months ago?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Got a lot worse}\\
\sffamily \textbf{2.} \rmfamily \emph{Got a little worse}\\
\sffamily \textbf{3.} \rmfamily \emph{Stayed the same}\\
\sffamily \textbf{4.} \rmfamily \emph{Got a little better}\\
\sffamily \textbf{5.} \rmfamily \emph{Got a lot better}\\
\sffamily \textbf{6.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}


<!-- Question 7 -- All -->
\begin{minipage}[t]{\linewidth}
\sffamily \textbf{Q7.} \rmfamily How do you think the \emph{general economic situation} in this country has changed over the last 12 months?\\
\\
\sffamily \textbf{1.} \rmfamily \emph{Got a lot worse}\\
\sffamily \textbf{2.} \rmfamily \emph{Got a little worse}\\
\sffamily \textbf{3.} \rmfamily \emph{Stayed the same}\\
\sffamily \textbf{4.} \rmfamily \emph{Got a little better}\\
\sffamily \textbf{5.} \rmfamily \emph{Got a lot better}\\
\sffamily \textbf{6.} \rmfamily \emph{Don't know}\\
\\
\end{minipage}

\setlength\parindent{2em}

\pagebreak

\setcounter{table}{0}

\renewcommand{\thetable}{SM\arabic{table}}

\setcounter{figure}{0}

\renewcommand{\thefigure}{SM\arabic{figure}}

\setcounter{page}{1}


#  Political Surveys Bias Self-Reported Economic Perceptions, Supplementary Material

Jack Bailey, Research Associate at The University of Manchester
Humanities Bridgeford Street, Manchester, M13 9PL, UK
jack.bailey@manchester.ac.uk

- Supplementary Material A: Power Analysis
- Supplementary Material B: Prior Distributions for Ordered Regression Models
- Supplementary Material C: Robustness Checks


\pagebreak


# Supplementary Material A: Power Analysis

![(\#fig:power-plot)Outcomes from 6,000 simulation-based power analyses, ordered by lower 95% credible interval. In scenario 1, I assume that political survey effects account for the total effect I find in the observational data. In scenario 2, I assume instead that political survey effects account for half of this effect. All samples achieve 80% in scenario 1. Only a sample of 2,500 achieved 80% power in scenario 2.](surveys_bias_econ_perc_files/figure-latex/power-plot-1.pdf) 

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

![(\#fig:thresh-plot)Before seeing the data, the most conservative assumption that we can make about the distribution of response options is that each combination is as likely as any other. This implies that prior thresholds must be able to take any possible value on the probability scale, conditional on the constraints of the model itself. A Normal(0, 1) prior on the latent probit scale yields fulfills this requirement and implies a flat prior on the probability scale. The figure above shows the resulting prior threshold parameters. Note also that the probability of a threshold occurring at any point on the scale is constant over the entire range of the scale (with any deviations arising only due to random noise in the simulation process).](surveys_bias_econ_perc_files/figure-latex/thresh-plot-1.pdf) 

The four left-most panels of figure \@ref(fig:thresh-plot) show the resulting prior distributions that this collective prior over all thresholds implies for each specific threshold. The right-most panel instead shows the implied prior over the whole probability scale (i.e. the distribution that we would find were we to stack each threshold distribution on top of each other). As the histograms in the figure make clear, the implied priors for each threshold are non-informative and, in all cases, take a wide range of possible values. For example, the priors shown here allow for a non-zero probability that the first threshold occurs as high as the $80\%$ mark and the fourth threshold as low as $20\%$. That the first response option corresponds to respondents reporting that the economy "got a lot worse" and the last that it has "got a lot better" reaffirms just how non-informative these priors really are.

Though we do not specify priors for each specific threshold, these prior predictive simulations suggest that they take their own distinctive shapes nonetheless. This phenomenon arises due to the constraints that both the prior and the model impose on the values that these parameters can take. For example, each threshold is constrained to take only values smaller than those of the threshold that follow it. As a result, it is not possible for any threshold to cover the entire space as this would leave the others with nowhere to go. Likewise, the collective nature of the prior means that the priors for each threshold *must* result in a flat prior overall. The result is the set of symmetrical distributions that we see above.


## Beta Parameters

Threshold parameters segment the latent outcome distribution, though do not move it. Beta parameters, instead, shift the latent distribution up and down its scale. This movement then serves to shift the probability mass of each observed response option in turn. As such, we can interpret beta parameters much like regression coefficients in linear and logistic regression models, which perform a similar role.

Before seeing the data, the most conservative assumption that one can make about these effects is that they are equal to zero (i.e. that they are null). Doing so is simple and, in the absence of any better information, uncontroversial. More difficult however is determining how uncertain these priors should be. One the one hand, a tight prior around zero will be very conservative, but perhaps to the extent that it ignores perfectly informative data. On the other, a loose prior will pay closer attention to the data, but perhaps to the extent that it will result in over-fitting.

For models with continuous outcomes, things are straightforward. If the prior is very wide, then it is also likely to cover the full range of plausible values that its respective parameter might take. But ordinal variables are not continuous and, as a result, this common practice can lead to perverse implications. Unlike models with continuous outcomes, wide priors on the latent probit scale *do not* give wide priors on the outcome scale. This is because the outcome scale takes only a finite set of discrete values. As a result, wide priors on the latent probit scale instead imply U-shaped priors due to probability mass piling up at the extremes. This problem then multiplies --- quite literally --- where the data include variables that exhibit a high degree of variation (for example age, which in the study of voting behavior might take any value between 18 and 100) or where the sum of all variables is large (such as when a model contains many parameters).

![(\#fig:beta-plot)While it is common to set wide priors on beta values where the outcome is continuous, such "non-informative" priors have perverse consequences when the outcome is ordered. This is because they make the latent scale too diffuse, thereby concentrating almost all of the prior probability mass at the two extremes of the observed ordinal outcome scale. Further, models that include many independent variables or independent variables that take extreme values worsen this problem further.](surveys_bias_econ_perc_files/figure-latex/beta-plot-1.pdf) 

Figure \@ref(fig:beta-plot) displays this phenomenon across different priors and different values of beta. Where these sum to zero, only the thresholds determine the response distribution, which I fix to ensure that each response has a prior probability of $20\%$ where betas sum to zero. As the figure shows, when this sum exceeds zero the prior probability of responding with either a 1 or a 5 increases. This is true for all priors, though the effect is most pronounced where the prior standard deviations are large. In each model in this paper, the sum of parameters increases where respondents voted at the last election or are in the treatment group. In light of this, using a prior on beta with a large standard deviation is akin to assuming that these participants are more likely to say either that the economy has "got a lot worse" or "got a lot better". Perhaps counter-intuitively, smaller standard deviations are, thus, less informative. Thus, I use the least informative prior --- Normal(0, 0.25) --- for all beta values in my models.


## Delta Parameters

![(\#fig:delta-plot)Setting diffuse priors on delta parameters can also have perverse consequences. In this case, they instead concentrate the prior probability mass in the middle and at the two extremes of the observed ordinal variable. Again, this is likely to be worse where models also include many independent variables or independent variables that take extreme values.](surveys_bias_econ_perc_files/figure-latex/delta-plot-1.pdf) 

Whereas beta parameters shift the latent outcome distribution, delta parameters compress or disperse it at a given point. This redistributes the observed outcome's probability mass towards central or extreme responses, conditional on its place on the scale. As in the previous case, the most conservative assumption that we can make before seeing the data is to expect these parameters to be equal to zero. Where this is true, the standard deviation of the latent outcome distribution does not vary across participants. Again, this is simple to achieve and, again, things become more complicated when it comes to setting the standard deviation. The problem is the same as before: large standard deviations imply more, not less, informative outcomes. 

Figure \@ref(fig:delta-plot) shows the implication that different priors and different values of delta have on the implied prior outcome distribution. As before, I fix all thresholds to imply an equal chance of any response option being selected and fix all beta parameters to 0. While wide priors on the beta parameters produced U-shaped distributions, wide priors on the delta parameters produce crown-like distributions. Note, however, that this pattern is conditional on the choice of thresholds and that U-shaped distributions may arise here too under different circumstances. As before, each response has an equal probability where the sum of delta parameters is zero. As this sum increases, the central response option becomes much more likely and extreme responses somewhat more likely. This implies that tighter standard deviations are also less informative in this case too. Given this, I opt to use a Normal(0, 0.25) prior on my delta parameters.

\pagebreak


# Supplementary Material C: Robustness Checks

There are three plausible objections to the results I report above. First, that the treatment effects occur due to some mechanism other than partisan bias. Second, that the theory does not generalize to other types of electoral identification. And, third, that the results are sensitive to my model specification. I test each below. The first tests if the treatment mechanism relies on partisan bias. To do so, I apply the same test to participants' reported *personal* economic perceptions. Past research finds that these show little sensitivity to party identification. The second tests if the theory generalizes to other types of identification. In particular, voting behavior at the 2016 referendum on European Union membership. The third tests if the findings are robust to different methods. In this case, by substituting ordered regression for multinomial regression instead.


## Personal Economic Perceptions and Partisan Bias as a Potential Mechanism

![(\#fig:pers-plot)Political surveys do not cause voters to report different perceptions of their own personal finances (left panel). This is unsurprising, since prior research shows that they are much less sensitive to party identification. Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. In general, treatment effects showed the expected signs. Incumbent voters were more positive. Likewise, opposition voters were more negative. Even so, in all cases, the distribution of treatment estimates were centered on small values and had a plausible chance of being practically-equivalent to zero. Here, density plots show the posterior distribution of conditional average treatment effects. Further, black bars show their 95% credible intervals and point estimates their medians.](surveys_bias_econ_perc_files/figure-latex/pers-plot-1.pdf) 

Above, I assume that my findings result from partisan bias. This seems reasonable given existing research [@devries2018; @bartels2002; @conover1987]. Even so, a skeptic might argue that I have not yet provided good evidence that this is indeed the case. Instead, they might argue that some other mechanism is reasonable for my findings. As a result, the pattern that I observe might also apply to any other dependent variable. This is a reasonable objection, as my design does not allow me to tease apart any intermediary steps in the causal chain between survey context and reported economic perceptions. Fortunately, there are ways to reduce this uncertainty. One is to test how the treatment affects a similar item that we know suffers from little partisan bias. Voters' perceptions of their own personal finances are on such possibility. Like national-level items, these too have their origin in consumer confidence surveys [@katona1951]. But, unlike national-level items, they are much less sensitive to partisan bias. This makes sense. After all, many would argue that the government is less accountable for any one person's well-being than it is for the well-being of the nation as a whole [@lewis-beck2017; @lewis-beck2000; @paldam1981; @kinder1981; @kinder1979; though see @tilley2018].

Figure \@ref(fig:pers-plot) shows how the treatment affected the personal economic perceptions that my participants reported. As before, I condition these estimates on prior voting behavior for the same reasons as above. In this case, all treatment effects have the expected signs. That is, incumbent supporters are more positive and opposition supporters more negative under the treatment. This might, then, suggest the presence of at least some partisan bias. Yet, in all cases, point estimates are small. These range in size from only -0.4 (95% CI: -1.9 to 1.1) to 1.9 percentage points (95% CI: -1.4 to 5.2). Further, these effects have 95% credible intervals that, in all cases, are very uncertain.

Taken together, these results suggest little evidence that political surveys affect the personal economic perceptions that respondents report. Were some other mechanism responsible for the treatment effects I find, this might not be the case. Instead, I find that the treatment might have a similar effect for both items. Instead, both sets of results are consistent with existing theory and the argument that I present above. That is, respondents must have a reason to assign responsibility to the government if political surveys are to prime respondents to respond in a different way. This does not seem to be the case for perceptions of one's personal finances. Instead, they appear to exhibit little partisan bias, leaving the treatment with nothing to manipulate. Of course, it is never possible to rule out any other mechanism with absolute certainty. Still, these results do at least make such a possibility seem much less likely.


## Generalization of Treatment Effects Across Different Types of Electoral Identification

If the theory that underpins my analysis is robust, it should generalize to other types of political identification. The British case is useful here. Due to the 2016 referendum on EU membership, the country now has *two* forms of electoral identification^[And yet more still in Scotland, where unionist versus nationalist identities rose to prominence after the 2014 referendum on Scottish independence.]. The first is conventional party identification. The second is identification with either the Leave or Remain side at the EU referendum. Further, recent evidence shows that the latter also affect self-reported economic perceptions [@fieldhouse2020; @sorace2018]. As the Leave side won, supporting it is, for all intents and purposes, akin to supporting the incumbent party. By the same logic, supporting Remain is now akin to supporting an opposition party. Accordingly, we should expect any treatment effects to generalize to EU referendum identification in the same way that they do to party identification.

\input{_assets/tab1.tex}

![(\#fig:eu-plot)Political surveys cause participants to report different perceptions of the national economy, conditional on their voting behavior at the 2016 referendum on European Union membership. Like with party identification, these effects are most pronounced where they voted for the winning side (Leave). But, in this case, there is also good evidence of a treatment effect on Remain voters too. Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. Density plots show the posterior distribution of conditional average treatment effects. Black bars show their 95% credible intervals and point estimates their medians.](surveys_bias_econ_perc_files/figure-latex/eu-plot-1.pdf) 

EU and party identification are not unrelated. But, the former does still cut across the latter to a meaningful extent. Table \@ref(tab:tab1) makes this clear. It shows the proportion of participants who voted for each combination of options at the 2016 referendum on EU membership and the 2017 general election. As we can see, participants who voted for the incumbent Conservative Party in 2017 most often voted to leave in 2016. Likewise, those who voted for an opposition party most often voted to remain. But this is not true in all cases. For example, 27.3% of participants who voted for the incumbent Conservative Party also voted to remain in the EU. Similarly, 26.4% of participants who voted for an opposition party also voted to leave. Further, 12.1% of participants voted only in 2016. Thus, we should not expect treatment effects for EU identification to be mere reflections of those across party identification.

Fortunately, the fourth question on the political survey primed voters to consider how they voted at the 2016 referendum (see appendix). Figure \@ref(fig:eu-plot) shows the corresponding treatment effects. In this case, Leave supporters in the treatment group were -4.7 percentage points (95% CI: -8.3 to -0.9) less likely to report either that the economy "got a little worse" and -4.6 percentage points (95% CI: -7.3 to -1.9) less likely to say that it "got a lot worse". They were also 6.3 percentage points (95% CI: 2.5 to 10.0) more likely to report that it had "stayed the same" and 2.9 percentage points (95% CI: -0.1 to 5.9) more likely to report that it "got a little better". Again, almost no one said that the economy "got a lot better" and there was no meaningful treatment effect (0.1, 95% CI: -0.9 to 1.0).

Those who voted Remain also showed similar effects to opposition voters. Yet they were much more likely to say that the economy "got a lot worse" in the last twelve months. This effect was large (4.4, 95% CI: -0.5 to 9.7). Further, though its 95% credible interval crossed zero, 96% of the posterior distribution was greater than zero. Thus, we can be reasonably confident that the true effect is, in fact, greater than zero. Likewise, given these results, we can also be confident that the treatment generalizes to other types of electoral identification too.


## Sensitivity of Treatment Effects to Modeling Assumptions

Ordered regression models estimate effects that are consistent across threshold parameters and, thus, across responses. This is known as the proportional odds assumption [@agresti2010; @mccullagh1980]. Consider the present case. The treatment has a positive effect on the national economic perceptions that incumbents report when measured on the probit scale (see table \@ref(tab:tabA1)). This is why they are more likely to say that the economy "got a little better" or "stayed the same" and less likely to say that the economy "got a little worse" or "got a lot worse". But, of course, this assumption may not hold. Instead, the treatment might have a unique effect on each response option.

![(\#fig:multi-plot)Using a multinomial rather than an ordinal model does little to change the results. We still find that political surveys cause incumbent voters to report more positive economic perceptions (left panel). Positive values imply that those in the treatment group were more likely to report a given response. Negative values imply the opposite. Density plots show the posterior distribution of conditional average treatment effects. Further, black bars show their 95% credible intervals and point estimates their medians.](surveys_bias_econ_perc_files/figure-latex/multi-plot-1.pdf) 

To relax this assumption, we can use multinomial regression instead. Figure \@ref(fig:multi-plot) shows the resulting estimates from such a model. Note that the multinomial model is less efficient and, thus, estimates tend to be less precise. Even so, they still lead to the same conclusion: that political survey effects are most clear where participants voted for the incumbent at the last election. Here, incumbent voters were -7.3 percentage points (95% CI: -13.4 to -1.6) less likely to say that the economy "got a little worse" and -2.5 percentage points (95% CI: -5.5 to 0.7) that it "got a lot worse". They were also 5.8 percentage points (95% CI: 0.0 to 11.6) more likely to say that the economy had "stayed the same" and 3.8 percentage points (95% CI: -0.3 to 8.3) more likely to say that it "got a little better". Results for opposition supporters and non-voters differ little to the results in figure \@ref(fig:nat-plot). Further, there appear to be no difference in the propensity of respondents to answer "Don't know" under the political survey treatment compared to the non-political survey control. As such, my conclusions appear robust to both model specification and missing data.

\pagebreak

\input{_assets/tabA2.tex}

\pagebreak

\input{_assets/tabA3.tex}

\pagebreak

\input{_assets/tabA4.tex}
