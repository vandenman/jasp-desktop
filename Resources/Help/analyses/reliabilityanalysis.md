Reliability Analysis
==========================

The Reliability Analysis allows you to examine whether your variables load on a common scale. It also helps for selecting variables that improve the scale if they are left out.  

Assumptions
-----------
The variables load on one common scale. This assumption might appear somewhat contradictory, as one uses reliability statistics to find out if variables load on a common scale. However,  many scale statistics are not a proper measurement of whether the variables load on one common scale, but rather the extent to which they load on a common scale.


Basic output
-----------

Scale statistics
-----------
To assess whether items true load on one scale, the reliability of the scale can be assessed using multiple measures. The exact equations for each of the scale statistics can be found in their references. For Cronbach's &alpha;, unstandardized values are calculated.

- McDonald's &omega;: directly estimates the population reliability.
- Cronbach's 	&alpha;: a lower bound of the population reliability.
- Gutmann's &lambda;6: a lower bound of the population reliability.
- Greatest lower bound: The largest of multiple lower bounds on the population reliability.
- Average interitem correlation: The average correlation among variables in the scale. 
- ICC: see below.
- Mean: The mean of the scale.
- Standard deviation: the standard deviation of the scale.


Item statistics
-----------
A table containing the scale statistics for all variables *when the variables are omitted from the scale*. This does not check what happens whenever two or more variables are dropped. Criteria are:

- McDonald's &omega; (if item dropped)
- Cronbach's 	&alpha; (if item dropped)
- Gutmann's &lambda;6 (if item dropped)
- Mean: The mean of the scale.
- Standard deviation: the standard deviation of the scale.
- Item-rest correlation

Reverse scaled items
-------
Sometimes, items load negatively on a scale. For example, imagine that participants answer questions about extraversion on a likert scale where a 1 is fully disagree and a 5 is fully agree. One question might be "I like to go to parties" whereas another question could be "I like to stay home in the evenings". Both questions might measure the same construct (extraversion), but a 5 on the first question would indicate high extraversion while a 5 on the second question would indicate low extraversion. Either the first or the second item would then need to be reversed. This can be done by dragging the item from the Normal-Scaled Items box to the Reverse-Scaled Items box.

Intraclass Correlations
-------

By checking ICC under scale statistics, a 6x9 table with intraclass correlations appears. There are 6 different methods for calculatingt the intraclass correlation, which are detailed below.

- ICC1: Each observation is a rating from a different rater. Raters were selected at random. This is equivalent to a one-way fixed effects ANOVA; the ICC is calculated using: (MSB - MSW)/(MSB + (nr - 1) * MSW). 
- ICC2: A random sample from k raters rated each observation.  This ICC is measure of absolute agreement across ratings. The ICC is calculated using: (MSB - MSE)/(MSB + (nr - 1) \* MSE + nj \* (MSJ - MSE)/n.obs).
- ICC3: A fixed sample of k raters rated each observation. There is no generalization to a larger population of raters. (MSB - MSE)/(MSB + (nr - 1) * MSE). 

ICC1 is influenced by mean differences in ratings across raters. ICC2 and ICC3 remove mean differences between raters, but both measures are influenced by interactions between raters. The final three rows reflect the means of k raters. Their calculations are detailed below.

- ICC1k: (MSB - MSW) / MSB.
- ICC2k: (MSB - MSE) / (MSB + (MSJ - MSE) / n.obs).
- ICC3k: (MSB - MSE) / MSB.

Abbreviations:

- MSB: Mean squares between raters.
- MSW: Mean squares within raters.
- MSE: Mean squares error.
- nObs: number of observations.
- nr: number of raters.

Advanced Options
-------

- Missing Values (delete listwise or pairwise)
- Confidence interval: calculate a confidence interval for Cronbach's 	&alpha;. The confidence interval is calculated using an analytical method, subject to distributional assumptions. These assumptions are not violated if the items satisfy the assumptions of a two-way random effects model: items are normally distributed and have homogeneous error variances (Feldt, Woodruff & Salih, 1987).


References
-------

Feldt, L. S., Woodruff, D. J., & Salih, F. A. (1987). Statistical inference for coefficient alpha. *Applied psychological measurement, 11*(1), 93-103.
