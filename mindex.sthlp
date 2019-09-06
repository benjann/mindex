{smcl}
{* *! version 1.0.0  06sep2019  Ben Jann & Simon Seiler}{...}
{vieweralsosee "[R] mlogit" "help mlogit"}{...}
{viewerjumpto "Basic syntax" "mindex##syntax"}{...}
{viewerjumpto "Advanced syntax" "mindex##asyntax"}{...}
{viewerjumpto "Description" "mindex##description"}{...}
{viewerjumpto "Options (basic syntax)" "mindex##basicoptions"}{...}
{viewerjumpto "Options (advanced syntax)" "mindex##advoptions"}{...}
{viewerjumpto "Examples" "mindex##examples"}{...}
{viewerjumpto "Methods and formulas" "mindex##methods"}{...}
{viewerjumpto "Saved results" "mindex##saved_results"}{...}
{viewerjumpto "References" "mindex##references"}{...}
{viewerjumpto "Authors" "mindex##authors"}{...}
{hi:help mindex}
{hline}

{title:Title}

{pstd}{hi:mindex} {hline 2}  M-Index analysis


{marker syntax}{...}
{title:Basic syntax}

{p 8 15 2}
    {cmd:mindex} {depvar} [{indepvars}] {ifin} {weight} [{cmd:,} {help mindex##basicopts:{it:options}} ]

{marker basicopts}{synoptset 22 tabbed}{...}
{col 5}{help mindex##basicoptions:{it:options}}{col 29}Description
{synoptline}
{syntab :Main}
{synopt :{opth over(varname)}}compute results by subpopulations
    {p_end}
{synopt :{opt t:otal}}include total across subpopulations
    {p_end}
{synopt :{opt contrast}}report differences between subpopulations
    {p_end}
{synopt :{opt dec:ompose}}decompose differences between subpopulations
    {p_end}
{synopt :{opt split}}subdivide margins decomposition component
    {p_end}
{synopt :{opt ref:erence(refgroup)}}select reference group for contrasts and decompositions
    {p_end}
{synopt :{opth cont:rols(varlist)}}control variables to be included outcome models
    {p_end}

{syntab :SE/Robust}
{synopt :{opt nose}}do not compute standard errors
    {p_end}
{synopt :{opth vce(vcetype)}}{it:vcetype} may be
   {opt analytic}, {opt cl:uster} {it:clustvar}, {opt boot:strap}, or
   {opt jack:knife}
   {p_end}
{synopt :{opt cl:uster(clustvar)}}synonym for {cmd:vce(cluster} {it:clustvar}{cmd:)}
    {p_end}

{syntab :Reporting}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt :{opt noh:eader}}suppress header display above coefficient table
    {p_end}
{synopt :{it:{help estimation_options##display_options:display_options}}}standard display options
    {p_end}
{synopt :{opt coefl:egend}}display legend instead of statistics
    {p_end}

{syntab :Generate}
{synopt :{opt g:enerate}[{cmd:(}{newvar}{cmd:)}]}store local M-index in variable {newvar}; default is {cmd:_M}
    {p_end}
{synopt :{opt r:eplace}}allow overwriting existing variables
    {p_end}

{syntab :Technical}
{synopt :{opt cmd(command)}}command to be used for the outcome models; default is {helpb mlogit}
    {p_end}
{synopt :[{cmd:no}]{opt coll:apse}}enforce/prevent internal data collapsing
    {p_end}
{synopt :{opt force}}enforce outcome model estimation even if tabulation is feasible
    {p_end}
{synopt :{opt noi:sily}}display output from outcome models
    {p_end}
{synoptline}
{p 4 6 2}{it:indepvars} and {cmd:controls()} may contain factor variables; see {help fvvarlist}.{p_end}
{p 4 6 2}{cmd:fweight}s, {cmd:aweight}s, {cmd:iweight}s, and {cmd:pweight}s are allowed; see help {help weight}.{p_end}


{marker asyntax}{...}
{title:Advanced syntax}

{p 8 15 2}
    {cmd:mindex} {depvar}
    {cmd:(}{it:eq1}{cmd:)}
    {cmd:(}{it:eq2}{cmd:)}
    {cmd:(}{it:eq3}{cmd:)}
    {ifin} {weight}
    [{cmd:,} {help mindex##advopts:{it:options}} ]

{pstd}
    where the syntax for {it:eq1}, {it:eq2}, and {it:eq3} is

{p 8 15 2}
    [{it:model}{cmd::}] [{varlist}] [{cmd:,} {opt cmd(command)} {it:opts} ]

{pstd}
    and {it:model} is

        {cmdab:f:ull}     full-information outcome model
        {cmdab:r:educed}  reduced-information outcome model
        {cmdab:m:index}   model analyzing the M-index

{pstd}
    If {it:model} is omitted, the models are assumed to be specified in the
    order a listed above. Each model can only be specified once. {cmd:cmd()}
    selects the command to be used for estimation; the default is
    {helpb mlogit} for the outcome models and {helpb regress} for the M-index
    model. {it:opts} are any options allowed by the estimation
    command. {it:varlist} may contain factor variables; see
    {help fvvarlist}.

{marker advopts}{synoptset 22 tabbed}{...}
{col 5}{help mindex##advoptions:{it:options}}{col 29}Description
{synoptline}
{syntab :SE/Robust}
{synopt :{opth vce(vcetype)}}{it:vcetype} may be {opt boot:strap} or
   {opt jack:knife}, or any {it:vcetype} allowed by the
   command used to analyze the M-index, e.g. {opt r:obust} or
   {opt cl:uster} {it:clustvar}
   {p_end}
{synopt :{opt robust}}synonym for {cmd:vce(robust)}
    {p_end}
{synopt :{opt cl:uster(clustvar)}}synonym for {cmd:vce(cluster} {it:clustvar}{cmd:)}
    {p_end}

{syntab :Reporting}
{synopt :{opt l:evel(#)}}set confidence level; default is {cmd:level(95)}
    {p_end}
{synopt :{opt noh:eader}}suppress header display above coefficient table
    {p_end}
{synopt :{it:{help estimation_options##display_options:display_options}}}standard display options
    {p_end}
{synopt :{opt coefl:egend}}display legend instead of statistics
    {p_end}

{syntab :Generate}
{synopt :{opt g:enerate}[{cmd:(}{newvar}{cmd:)}]}store local M-index in variable
    {newvar}; default is {cmd:_M}
    {p_end}
{synopt :{opt r:eplace}}allow overwriting existing variables
    {p_end}

{syntab :Technical}
{synopt :{opt coll:apse}}internally collapse data to improve speed
    {p_end}
{synopt :{opt force}}enforce outcome model estimation even if tabulation is feasible
    {p_end}
{synopt :{opt noi:sily}}display output from outcome models
    {p_end}
{synopt :{it:mopts}}options to be passed through to the model analyzing the M-index
    {p_end}
{synoptline}
{p 4 6 2}{cmd:fweight}s, {cmd:aweight}s, {cmd:iweight}s, and {cmd:pweight}s are allowed; see help {help weight}.{p_end}


{marker description}{...}
{title:Description}

{pstd}
    {cmd:mindex} is a command to compute and analyze the mutual information
    index (M-index), an entropy-based measure of association between
    categorical variables (see Section III in Theil 1970). More generally,
    {cmd:mindex} computes the M-index between
    a categorical dependent variable and one or several categorical or
    continuous covariates. The M-index is used, for example, to study
    occupational segregation (Theil/Finizza 1971, Mora/Ruiz-Castillo 2009,
    2011, DiPrete et al. 2017) or intergenerational class mobility
    (Silber/Spadaro 2011, Seiler 2018, Seiler/Jann 2019a,b). Intuitively, the
    M-index quantifies how much we can "learn" about the dependent variable by
    knowing the covariates. The larger the value of the M-index, the more
    information do the covariates carry about the dependent variable and,
    hence, the "stronger" the relation between the variables.

{pstd}
    {cmd:mindex} estimates the M-index based on individual-level data. In case
    of aggregate data (e.g. sizes of ethnic groups by districts), use
    {cmd:mindex} with {cmd:fweight}s.


{marker options}{...}
{marker basicoptions}{...}
{title:Basic syntax options}

{phang}
    {opth over(varname)} estimates the M-index for the subpopulations defined by
    the values of {it:varname}.

{phang}
    {opt total} reports the M-index of the total population in addition to the
    the individual subpopulations. {cmd:total} requires {cmd:over()}.

{phang}
    {opt contrast} computes differences in the M-index between subpopulations.
    One of the subpopulations (or the total population if {cmd:total} is
    specified) serves as the reference group for the contrasts. {cmd:contrast}
    requires {cmd:over()}.

{phang}
    {opt decompose} decomposes the M-index contrasts into a component due to
    differences in the association pattern (internal structure) and a
    component due to differences in the marginal structure. {cmd:decompose}
    requires {cmd:over()} and is only supported if {cmd:cmd()} is
    {helpb mlogit} and if {cmd:controls()} is empty. {cmd:decompose} 
    implies {cmd:contrast}.

{phang}
    {opt split} subdivides the marginal structure component of the decomposition
    into a part due to differences in the distribution of the dependent variable and
    a part due to differences in the distribution of the predictors. {cmd:split}
    implies {cmd:decompose}.

{phang}
    {opt refgroup(refgroup)} selects the reference group for contrasts and
    decompositions. {it:refgroup} can be {cmd:#}{it:#} to select the {it:#}th
    subpopulation, or {it:#} to select the subpopulation whose value is equal
    to {it:#}. The default is to use the first subpopulation as reference group
    or, if {cmd:total} is specified, to use the total population.

{phang}
    {opth controls(varlist)} are control variables to be included in both
    the reduced model and the full model. {it:varlist} may contain factor
    variables; see {help fvvarlist}.

{phang}
    {opt nose} skips the computation if standard errors. Use this option to save
    computer time. {cmd:vce(bootstrap)} and {cmd:vce(jackknife)} imply {cmd:nose}.

{phang}
    {opt vce(vcetype)} specifies the type of variance estimation to be used
    to determine the standard errors. {it:vcetype} may be
    {opt analytic}, {opt cl:uster} {it:clustvar}, {opt boot:strap}, or
    {opt jack:knife}; see {help vce_option:[R] {it:vce_option}}.

{phang}
    {opt cluster(clustvar)} is a synonym for {cmd:vce(cluster} {it:clustvar}{cmd:)}.

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)}
    or as set by {helpb set level}.

{phang}
    {opt noheader} suppresses the header above the coefficient table.

{phang}
    {it:display_options} are standard display options; see
    {helpb estimation_options##display_options:[R] estimation options}.

{phang}
    {opt coeflegend} specifies that the legend of the coefficients and how
    to specify them in an expression be displayed rather than displaying the
    statistics for the coefficients.

{phang}
    {opt generate}[{cmd:(}{newvar}{cmd:)}] stores the observation-level
    components of the M-index in variable {newvar}. The default variable name
    is {cmd:_M}.

{phang}
    {opt replace} allows overwriting existing variables.

{phang}
    {opt cmd(command)} selects the command to be used to estimate the reduced model
    and the full model. The default command is {helpb mlogit}.

{phang}
    {opt collapse} internally collapses the data to a dataset of frequencies and
    runs all computations on such a compressed dataset. {opt collapse}
    has no effect on the results. Use this option to speedup computations
    if the data structure is simple (e.g. if all variables are
    categorical with only few levels). {opt collapse} is not allowed if
    weights are specified.

{pmore}
    If {it:indepvars} contains a single categorical variable and
    {cmd:controls()} is empty, the data will be collapsed automatically (unless
    {cmd:force} or weights have been specified). Type {cmd:nocollapse} to
    prevent the automatic collapsing.

{phang}
    {opt force} enforces model estimation even if tabulation is feasible. If
    {cmd:controls()} is empty, base probabilities can be obtained from a
    one-way table of {it:depvar} without estimating the reduced model.
    Likewise, if {cmd:controls()} is empty and {it:indepvars} contains a single
    categorical variable, conditional probabilities can be obtained from a
    two-way table. To save computer time, {cmd:mindex} automatically detects
    these situations and switches to tabulation whenever feasible. Apply option
    {cmd:force} to deactivate this behavior. In any case, tabulation will only
    be considered as an alternative to model estimation if {cmd:cmd()} is
    {helpb mlogit}.

{phang}
    {opt noisily} displays the output of the reduced model and the full model. By
    default, these results are not displayed.


{marker advoptions}{...}
{title:Advanced syntax options}

{phang}
    {opt vce(vcetype)} specifies the type of variance estimation to be used
    to determine the standard errors. {it:vcetype} may be {opt boot:strap} or
    {opt jack:knife} or any {it:vcetype} allowed by the
    command used to analyze the M-index. Examples are {opt r:obust} or
    {opt cl:uster} {it:clustvar}. Also see {help vce_option:[R] {it:vce_option}}.

{phang}
    {opt robust} is a synonym for {cmd:vce(robust)}.

{phang}
    {opt cluster(clustvar)} is a synonym for {cmd:vce(cluster} {it:clustvar}{cmd:)}.

{phang}
    {opt level(#)} specifies the confidence level, as a percentage, for
    confidence intervals. The default is {cmd:level(95)}
    or as set by {helpb set level}.

{phang}
    {opt noheader} suppresses the header above the coefficient table.

{phang}
    {it:display_options} are standard display options; see
    {helpb estimation_options##display_options:[R] estimation options}.

{phang}
    {opt coeflegend} specifies that the legend of the coefficients and how
    to specify them in an expression be displayed rather than displaying the
    statistics for the coefficients.

{phang}
    {opt generate}[{cmd:(}{newvar}{cmd:)}] stores the observation-level components of the
    M-index in variable {newvar}. The default variable name is {cmd:_M}.

{phang}
    {opt replace} allows overwriting existing variables.

{phang}
    {opt collapse} internally collapses the data to a dataset of frequencies and
    runs all computations on such a compressed dataset. {opt collapse}
    has no effect on the results. Use this option to speedup computations
    if the data structure is simple (e.g. if all variables are
    categorical with only few levels). {opt collapse} is not allowed if
    weights are specified.

{phang}
    {opt force} enforces model estimation even if tabulation is feasible. If
    the reduced model is empty, base probabilities can be obtained from a
    one-way table of {it:depvar} without estimating the reduced model.
    Likewise, if the full model only contains a single categorical variable,
    conditional probabilities can be obtained from a two-way table. To save
    computer time, {cmd:mindex} automatically detects these situations and
    switches to tabulation whenever feasible. Apply option {cmd:force} to
    deactivate this behavior. In any case, tabulation will only be considered
    as an alternative to model estimation if the command for the outcome models
    is {helpb mlogit}.

{phang}
    {opt noisily} displays the output of the reduced model and the full
    model. By default, these models are not displayed.

{phang}
    {it:mopts} are options to be passed through to the command estimating the
    model analyzing the M-index. This is an alternative to specifying the options
    directly within the model equation.


{marker examples}{...}
{title:Examples}

{pstd}
    M-index by country using the {cmd:example2.dta} from Pisati (2000; the original source is
    Xie 1992):

        . {stata "use http://www.stata.com/stb/stb55/sg142/example2.dta, clear"}
        . {stata mindex son i.father [fweight=obs], over(country)}

{pstd}
    {cmd:example2.dta} contains aggregate-level data (frequencies of
    combinations of son's and father's class by country); this is why we apply
    frequency weights. {cmd:mindex} stores its results in {cmd:e()} so that
    they can easily be processed into tables or graphs. Here is an example
    using {helpb coefplot} (Jann 2014):

        . {stata coefplot, sort xtitle(M-index)}

{pstd}
    Same analysis using advanced syntax:

{p 8 12 2}. {stata mindex son (i.father i.country i.father#i.country) (i.country) (ibn.country, robust noconstant) [fweight=obs]}

{pstd}
    Use explicit declarations if you want specify the equations in a different order:

{p 8 12 2}. {stata "mindex son (m:ibn.country, robust noconstant) (full:i.father i.country i.father#i.country) (reduced:i.country) [fweight=obs]"}

{pstd}
    Decomposition of country differences using {cmd:example1.dta} from Pisati (2000):

        . {stata "use http://www.stata.com/stb/stb55/sg142/example1.dta, clear"}
        . {stata mindex son i.father [fweight=obs], over(country) decompose}

{pstd}
    No standard errors are reported for the decomposition components. One approach
    to obtain the standard errors is to use the bootstrap (since Stata's
    {helpb bootstrap} command does not support {cmd:fweight}s, we
    first have to expand the example data to observation-level data):

        . {stata expand obs}
{p 8 12 2}. {stata mindex son i.father, over(country) decompose vce(boot, reps(100))}{p_end}


{marker methods}{...}
{title:Methods and formulas}

{dlgtab:Basic definition}

{pstd}
    For the basic ideas behind the M-index see Section III in Theil (1970;
    although the term "M-index" does not appear). For a formal discussion in
    the context of segregation research see Theil/Finizza (1971) and
    Mora/Ruiz-Castillo (2009, 2011). For a formal discussion in the context of
    social mobility research see Silber/Spadaro (2011).

{pstd}
    The above sources describe the M-index in terms of aggregate data (i.e. cell
    probabilities in contingency tables). Here we give a more general
    account in terms of individual-level observations. The M-index is computed
    as

        {it:M} = 1/N sum({it:m}_i)

{pstd}
    where the sum is across all observations (i = 1,...,N). The observation-level
    components {it:m}_i are given as

        {it:m}_i = ln(Pr1_i / Pr0_i)

{pstd}
    where Pr1_i is an estimate under full-information (i.e. including the
    predictors we are interested in, say parents' social class in case of
    mobility research) of the probability that the dependent variable takes on
    its observed value for observation i, and Pr0_i is the estimate under
    restricted information (e.g. excluding parents' social class). By default,
    {cmd:mindex} uses multinomial logistic regression to estimate Pr1 and
    Pr0, but other models, such as ordered logistic regression, may make sense
    depending on context.

{pstd}
    As is evident from this definition, the M-index can be decomposed by
    subgroups. This allows modeling the M-index using regression techniques, a
    property that is exploited by the advanced syntax of {cmd:mindex}. The model
    used to analyze the M-index should usually only include
    predictors that were taken into account when computing the M-index.

{dlgtab:Counterfactual decomposition}

{pstd}
    Silber/Spadaro (2011) propose a Shapley decomposition to separate the
    difference in the M-index between two subpopulations into a component due
    to differences in the internal structure (the association pattern
    between the dependent variable and the predictors) and a component due to
    differences in the marginal distributions of the variables. Silber/Spadaro
    formalize the decomposition at the aggregate level of tabular data and use
    a "raking" procedure that allows manipulating the internal structure of a
    table while maintaining given marginal distributions. {cmd:mindex}
    generalizes the procedure to individual-level data. The "raking" is
    implemented by manipulating the constants of a multinomial logistic
    regression. That is, for example, the model estimated in the reference group
    is used to generate predictions in a comparison group. This corresponds to a
    situation where the marginal distribution of the predictors is as in the
    comparison group, but the association pattern (the "internal structure") is
    as in the reference group. These out-of-sample predictions yield an outcome distribution
    that neither matches the observed distribution in the
    reference group, nor the one in the comparison group. However, by adjusting the
    outcome-specific intercepts of the model it is possible to reproduce any
    desired outcome distribution without changing the internal structure or the
    distribution of the predictors ({cmd:mindex} uses a numerical algorithm to
    derive the necessary adjustments; a closed-form solution seems difficult
    to obtain). Based on such adjusted predictions it is then possible to
    compute the various counterfactual M-indices required for the decomposition
    (for example, the M-index for the comparison group keeping all margins fixed but
    switching the association pattern to the pattern observed in the reference
    group; or keeping the distribution of covariates and the association pattern fixed,
    but switching the distribution of the dependent variable to the distribution
    observed in the reference group; etc.)

{dlgtab:Standard errors}

{pstd}
    By default, {cmd:mindex} computes standard errors that assume Pr1 and Pr0
    to be fixed, not estimated. In our experience, these standard errors are
    often more or less consistent, despite the simplifying assumption. This
    finding, however, only applies to levels and contrasts and does not
    translate to the components of the counterfactual decomposition; this is
    why we do not report standard errors for the decomposition components. In any
    case, we suggest using the bootstrap to obtain reliable standard errors.


{marker saved_results}{...}
{title:Saved results}

{pstd}
    Under basic syntax {cmd:mindex} stores the following in {cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(N_over)}}number of subpopulations (or undefined){p_end}
{synopt:{cmd:e(N_clust)}}number of clusters (or undefined){p_end}
{synopt:{cmd:e(irefgroup)}}index of reference subpopulation (or undefined){p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:mindex}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of outcome variable{p_end}
{synopt:{cmd:e(over)}}name of {cmd:over()} variable{p_end}
{synopt:{cmd:e(over_levels)}}values from {cmd:over()} variable{p_end}
{synopt:{cmd:e(total)}}{cmd:total} or empty{p_end}
{synopt:{cmd:e(contrast)}}{cmd:contrast} or empty{p_end}
{synopt:{cmd:e(decompose)}}{cmd:decompose} or empty{p_end}
{synopt:{cmd:e(split)}}{cmd:split} or empty{p_end}
{synopt:{cmd:e(refgroup)}}value of reference subpopulation{p_end}
{synopt:{cmd:e(collapse)}}{cmd:collapse} if data has been collapsed, else empty{p_end}
{synopt:{cmd:e(force)}}{cmd:force} or empty{p_end}
{synopt:{cmd:e(fcmd)}}estimation command used for full model{p_end}
{synopt:{cmd:e(fvars)}}covariates in full model{p_end}
{synopt:{cmd:e(fopts)}}options applied to full model{p_end}
{synopt:{cmd:e(ftable)}}{cmd:ftable} if full model table-based, else empty{p_end}
{synopt:{cmd:e(rcmd)}}estimation command used for reduced model{p_end}
{synopt:{cmd:e(rvars)}}covariates in reduced model{p_end}
{synopt:{cmd:e(ropts)}}options applied to reduced model{p_end}
{synopt:{cmd:e(rtable)}}{cmd:rtable} if reduced model table-based, else empty{p_end}
{synopt:{cmd:e(generate)}}name of variable generated by {cmd:generate()}{p_end}
{synopt:{cmd:e(wtype)}}weight type{p_end}
{synopt:{cmd:e(wexp)}}weight expression{p_end}
{synopt:{cmd:e(clustvar)}}name of cluster variable{p_end}
{synopt:{cmd:e(vce)}}{it:vcetype} specified in {cmd:vce()}{p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err.{p_end}
{synopt:{cmd:e(title)}}{cmd:M-index analysis}{p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}

{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}estimates{p_end}
{synopt:{cmd:e(V)}}variance-covariance matrix of estimates{p_end}
{synopt:{cmd:e(_N)}}numbers of observations in subpopulations{p_end}

{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}

{pstd}
    Under advanced syntax {cmd:mindex} passes through all returns from the
    estimation command used for the M-index model and adds the following
    macros:

{synopt:{cmd:e(cmd)}}{cmd:mindex}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(depvar)}}name of outcome variable{p_end}
{synopt:{cmd:e(collapse)}}{cmd:collapse} if data has been collapsed, else empty{p_end}
{synopt:{cmd:e(force)}}{cmd:force} or empty{p_end}
{synopt:{cmd:e(fcmd)}}estimation command used for full model{p_end}
{synopt:{cmd:e(fvars)}}covariates in full model{p_end}
{synopt:{cmd:e(fopts)}}options applied to full model{p_end}
{synopt:{cmd:e(ftable)}}{cmd:ftable} if full model table-based, else empty{p_end}
{synopt:{cmd:e(rcmd)}}estimation command used for reduced model{p_end}
{synopt:{cmd:e(rvars)}}covariates in reduced model{p_end}
{synopt:{cmd:e(ropts)}}options applied to reduced model{p_end}
{synopt:{cmd:e(rtable)}}{cmd:rtable} if reduced model table-based, else empty{p_end}
{synopt:{cmd:e(mcmd)}}estimation command used for the M-index model{p_end}
{synopt:{cmd:e(mvars)}}covariates in the M-index model{p_end}
{synopt:{cmd:e(mopts)}}options applied to M-index model{p_end}
{synopt:{cmd:e(generate)}}name of variable generated by {cmd:generate()}{p_end}
{synopt:{cmd:e(title)}}{cmd:M-index analysis}{p_end}


{marker references}{...}
{title:References}

{phang}
    DiPrete, T.A., T. Bol, C.C. Eller, H.G. van de Werfhorst. 2017. School-to-Work
    Linkages in the United States, Germany, and France. American Journal of
    Sociology 122(6): 1869–1938.
    {p_end}
{phang}
    Jann, B. 2014. {browse "http://www.stata-journal.com/article.html?article=gr0059":Plotting regression coefficients and other estimates.} The
    Stata Journal 14(4): 708-737.
    {p_end}
{phang}
    Mora, R., J. Ruiz-Castillo. 2009. The Invariance Properties of the Mutual
    Information Index of Multigroup Segregation. P. 33–53 in: Y. Flückiger,
    S.F. Reardon, J. Silber (eds.). Occupational and Residential Segregation.
    Bingley: Emerald.
    {p_end}
{phang}
    Mora, R., J. Ruiz-Castillo. 2011. Entropy-Based Segregation Indices.
    Sociological Methodology 41: 159–194.
    {p_end}
{phang}
    Pisati, M. 2000. {stata "net describe sg142, from(http://www.stata.com/stb/stb55)":sg142}: Uniform
    layer effect models for the analysis of differences in two-way associations. Stata
    Technical Bulletin 55: 33-47.
    {p_end}
{phang}
    Seiler, S. 2018. Social Mobility in Modernizing Switzerland. Dissertation. University
    of Bern.
    {p_end}
{phang}
    Seiler, S., B. Jann. 2019a. Bringing the Margins Back In. Using the M-Index for
    the Analysis of Social Mobility. Paper presented at the 114th Annual Meeting
    of the American Sociological Association, New York, August 10-13,
    2019. DOI: {browse "http://dx.doi.org/10.7892/boris.132702":10.7892/boris.132702}.
    {p_end}
{phang}
    Seiler, S., B. Jann. 2019b International Comparison of Social Mobility
    using the M-Index. Paper presented at the ISA RC28 Summer Meeting,
    Princeton University, August 15-17,
    2019. DOI: {browse "http://dx.doi.org/10.7892/boris.132703":10.7892/boris.132703}.
    {p_end}
{phang}
    Silber, J., A. Spadaro. 2011. Inequality of Life Chances and the
    Measurement of Social Immobility. P. 129–154 in: M. Fleurbaey, M. Salles,
    J.A. Weymark (eds.). Social Ethics and Normative Economics. Berlin:
    Springer.
    {p_end}
{phang}
    Theil, H., A.J. Finizza. 1971. A note on the measurement of racial
    integration of schools by means of informational concepts. Journal of
    Mathematical Sociology 1(2): 187–193.
    {p_end}
{phang}
    Theil, H. 1970. On the estimation of relationships involving qualitative
    variables. American Journal of Sociology 76(1): 103–154.
    {p_end}
{phang}
    Xie, Y. 1992. The Log-Multiplicative Layer Effect Model for Comparing Mobility
    Tables. American Sociological Review 57(3): 380–395.
    {p_end}


{marker authors}{...}
{title:Authors}

{pstd}
    Ben Jann, University of Bern, ben.jann@soz.unibe.ch
    {p_end}
{pstd}
    Simon Seiler, University of Bern, simon.seiler@icer.unibe.ch

{pstd}
    Thanks for citing this software as follows:

{pmore}
    Jann, B., S. Seiler. 2019. mindex: Stata module to compute and analyze the 
        mutual information index (M-index). Available from
    {browse "http://github.com/benjann/mindex"}.

