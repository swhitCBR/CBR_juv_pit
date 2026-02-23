
- Quotes on what constitutes non-sparse sample sizes

	- "Usually $\lambda_{v,i+1}$ and $R_{v,i+1}$ are jointly large enough to ensure that the bias in $\tilde\phi_{vi}$ is virtually zero (e.g., $\lambda_{v,i+1} \geq 0.05$ and $R_{v,i+1} \geq 99$  suffices." (pg 208, bottom of page)
	- "If capture probabilities (i.e., effort) are unavoidably low at any site, then increase the number of sampling sites" (pg 315 paragraph 1)
	- In reference to a simulation with high S and $\phi$ and R=200: "the bias remails small (about 1%) if the assumptions of the model are realized."
	- In reference model misspecification bias: "(analytic-numeric method) is suitable for determining if the bias associated with using the alternative model, rather than the true model, is small, medium, or large (e.g., $\leq$ 2.5%, about 10%, or $\geq$"20%)" (pg 215 paragraph 3)
	- 

Recommended transformations are the log-transform for $\hat\phi$ and $\hat{S}$ and the logistic (log-odds) transform for $\hat{p}$. These transformations are not routinely necssary; they make little difference if the cv of the parameter estimator in question is sufficiently small, say cv $\leq$ 0.1 (pg 211 2nd paragraph)

It is important to consider stratifying the fish by any recognizable variable that might affect the parameters $\phi$, p, and S. The most important variable is probably fish size, as measured, for example by length; others are age, sex, strain, and so forth. The purpose of stratification is twofold. First, fish in a lot should be homogenous in the response parameters. Homogeneity reduces extraneous sources of variation. Second, we want to identify factors that might influence treatment effects. If treatment effect varies by size (over the range of size of interest), we want to know this. We make size a design factor by releasing two or more size classes. (pg. 327 paragraph 3)

Monte Carlo simulation might be used to assess a proposed design by providing an estimate of the expected confidence interval widths and achieved confidence interval coverage. ... THe sampling distribution of $\phi_{vi}$ and S estimators are only normal in large samples; RELEASE providsmeans to study these distributions for particular sample sizes and parameter values. Results of Monte Carlo simulations also provide a basis for the parametric bootstrap method of establishing confidence intervals (Buckland 1984) (pg.330; paragraph3)



Some block quotes

> [!NOTE] Description of cases where a Monte Carlo approach is needed to evaluate bias
> paragraph 2 on pg.216
> 
> *The validity of this theoretical evaluation of model bias, precision, and power depends on sample size being large. THis procedure can give poor results if the $R_{vi}$ are small. Mote Carlo methods are necessary to investigate small-sample properties of statistical procedures. Also, this numerical procedure does not aid in determining the properties of complex procedures such as model selection, which involves a sequence of steps. Finally, one cannot learn anything about the sampling distribution of estimators or statistics from this analytical procedure. Hence, there is still a need for Monte Carlo procedures (e.g., see Buckland 1984); however, simulation is not needed to determine asymptotic model bias, precision, or power
> 


**Statement that model bias trumps statistical bias** (pg 207 bottom of page)

Parameter estimators have some statistical bias even when the model used as a basis for analysis is true (Gilbert 1973)... We do not dwell on statistical bias or its adjustment, howeer, because statistical bias is a trivial source of bias. The serious source of bias is "model" bias. By model biases, we mean biases which occur because the incorrect model is used. Statistical biases are smaller than one standard error of the parameter estimator; however, model biases can be large and thus serious if the wrong model is used. 