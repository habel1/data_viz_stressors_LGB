Stressors in the LGB Population
================
Zoe Habel

People who identify as sexual gender minorities such as lesbian, gay, or
bisexual are at unique risks for several health outcomes. In order to
further explore some of the potential vulnerabilities for certain health
outcomes, I analyzed a national cohort study of LGB people[^1]. The
survey was given in 3 waves once a year for 3 years, so there is
longitudinal data on participants as well.

To look at some of the unique circumstances, I decided to look at the
measures of Everyday Discrimination, Internalized Homophobia,
Psychological Distress and Social Wellbeing as potential contributing
factors to negative health outcomes. I was interested in seeing if
certain identities are more likely to experience certain healthcare
outcomes. Additionally, I also wanted to see how well represented
different groups were across the study as a whole and at each wave.

``` r
sexuality_barplotW1 <- ggplot(
  generations_combined,
  aes(x = W1SEXMINID, fill =W1SEXMINID)
                             ) +
  geom_bar() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(
    x = NULL,
    y = "# of Participants",
    fill = "Sexual Minority\nIdentity",
    title = "Wave 1"
      ) +
  theme_classic()


sexuality_barplotW2 <- ggplot(
  W2generations_combined,
  aes(x = W2SEXMINID, fill =W2SEXMINID)
                             ) +
  geom_bar() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(
    x = NULL,
    y = "# of Participants",
    fill = "Sexual Minority\nIdentity",
    title = "Wave 2"
      ) +
  theme_classic()
  
sexuality_barplotW3 <- ggplot(
  W3generations_combined,
  aes(x = W3SEXMINID, fill =W3SEXMINID)
                             ) +
  geom_bar() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(
    x = NULL,
    y = "# of Participants",
    fill = "Sexual Minority\nIdentity",
    title = "Wave 3"
      ) +
  theme_classic()
```

``` r
allwaves_sexuality_barplot <- ggplot(
  sexualities,
  aes(x = SEXMINID, fill =ALLSEXMINID)
                                    ) +
  geom_bar(position="fill") +
  scale_y_continuous(
    name = "% of Participants", 
    labels = scales::label_percent())+
  scale_fill_grey(
    labels = c("Wave 1", "Wave 2","Wave 3")
                       ) +
  labs(
    x = "Sexual Minority Identity",
    fill = "Survey Wave",
    title = "All Waves"
      ) +
  theme_classic()
```

``` r
sexuality_barplotW1 + sexuality_barplotW2 + sexuality_barplotW3 + allwaves_sexuality_barplot +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Sexual Minority Identity Distribution Per Wave of Study")
```

![](miniproj_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

I chose stacked bar plots to display the distribution of different
identities accross all 3 waves. As seen in the bar plots, **Lesbian and
Gay adults are overrepresented accross all 3 waves.** Depending on how
much outcomes vary by identity, this could greatly impact the
signifcance of the findings and is a methodological weakness.

``` r
internalized_plotW1 <- ggplot(generations_combined,
                              aes(x = W1SEXMINID, y = W1INTERNALIZED_I, fill = W1SEXMINID)) +
  geom_boxplot() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(y = "Internalized\nHomophobia", x = NULL, fill = "Sexual Minority\nIdentity") +
  theme_classic()
everyday_plotW1 <- ggplot(generations_combined,
                          aes(x = W1SEXMINID, y = W1EVERYDAY_I, fill = W1SEXMINID)) +
  geom_boxplot() +
  scale_fill_canva(     palette = "Pop art",     labels = c("Lesbian/Gay", "Bisexual","Other")                   ) +
  labs(y = "Everyday\nDiscrimination", x = NULL, fill = "Sexual Minority\nIdentity") +
  theme_classic()

kessler6_plotW1 <- 
  ggplot(
  generations_combined,
  aes(x = W1SEXMINID, y = W1KESSLER6_I, fill = W1SEXMINID)
        ) +
  geom_boxplot() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(
    y = "Psychological\nDistress", 
    x = NULL, fill = "Sexual Minority\nIdentity"
      ) +
  theme_classic()

social_wb_plotW1 <- 
  ggplot(
  generations_combined,
  aes(x = W1SEXMINID, y = W1SOCIALWB_I, fill = W1SEXMINID)
        ) +
  geom_boxplot() +
  scale_fill_canva(
    palette = "Pop art",
    labels = c("Lesbian/Gay", "Bisexual","Other")
                  ) +
  labs(
    y =  "Social\nWellbeing", 
    x = NULL, 
    fill = "Sexual Minority\nIdentity"
      )+
  theme_classic()
```

``` r
internalized_plotW1 + everyday_plotW1 + kessler6_plotW1 + social_wb_plotW1 +
  plot_layout(guides = 'collect') +
  plot_annotation(title = "Stressors and Vulnerabilites experienced by Lesbian, Gay,\nBisexual, and other Sexual Minority Identifying Adults", caption = "*Missing data replaced with imputation")
```

![](miniproj_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

To in look at the distribution of different sexual minority identities
for certain risk factors, I chose to use side-by-side box plots.

These boxplots all use data from Wave 1, because it has the most
participants. **For each risk factor, the boxplots overlap, suggesting
that the difference between groups is minimal**. People identifying as
**bisexual** or an other sexual minority identity showed **higher median
scores for psychological distress and everyday discrimination**, and
those identifying as **lesbian, gay or bisexual** showed slightly
**larger median internalized homophobia compared scores** to other
identities. For **social wellbeing**, people identifying as **bisexual**
had the **lowest median scores**.

A limitation of this report is that the colors are not color-blind
friendly. If these results were to be published, I would utilize
color-blind friendly colors or shapes to more clearly differentiate the
data. Additionally, without more rigorous statistical analysis, it is
not possible to conclude whether or not these results are statistically
significant, but these plots do bring to light the differences between
the experiences of people with different sexual minority identities and
the need to develop targeted studies and interventions with these
differences in mind.

Though LGB people share similar struggles, their unique identities can
make them more susceptible to certain circumstances.

Previous research has shown bisexual to face greater issues with
community belonging, because they are isolated and separated from both
homosexual and heterosexual groups, as well as being more susceptible to
invalidation of their identity. These figures support these previous
findings, suggesting that bisexual people and other sexual minorities
may be **more susceptible** to certain risk factors.

[^1]: Meyer, I. H. (2023). Generations: A Study of the Life and Health
    of LGB People in a Changing Society, United States, 2016-2019
    \[Dataset\]. Inter-university Consortium for Political and Social
    Research \[distributor\]. <https://doi.org/10.3886/ICPSR37166.v2>
