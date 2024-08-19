{smcl}
{cmd:help xtspj}{right: ({browse "https://doi.org/10.1177/1536867X19854016":SJ19-2: st0557})}
{hline}

{title:Title}

{p2colset 5 14 15 2}{...}
{p2col :{cmd:xtspj} {hline 2}}Split-panel jackknife estimation for
fixed-effects panel-data models{p_end}
{p2colreset}{...}


{title:Syntax}

{pstd}
Split-panel jackknife estimation of probit, logit, linear, negative binomial,
Poisson, exponential, gamma, and Weibull regression models with fixed effects

{p 8 13 2}
{opt xtspj} {depvar} [{indepvars}] {ifin}{cmd:,} 
{opt mo:del(string)} {opt me:thod(string)}
[{opt l:evel(#)}
{opt ltol(#)}
{opt ptol(#)}
{opt maxi:ter(#)}
{opt diag:nosis}
{opt verb:ose}
{opt a:lpha(newvar)}]


{pstd}
Split-panel jackknife estimation of linear models (using closed-form
expressions)

{p 8 13 2}
{opt xtspj} {depvar} [{indepvars}] {ifin}{cmd:,} {opt mo:del(regress)} 
{opt me:thod(string)}
[{opt l:evel(#)}
{opt ltol(#)}
{opt ptol(#)}
{opt maxi:ter(#)}
{opt diag:nosis}
{opt verb:ose}
{opt a:lpha(newvar)} {help regress:{it:regress_options}}]


{pstd}
Split-panel jackknife estimation of user-written fixed-effects models

{p 8 13 2}
{opt xtspj} {it:eq} [{it:eq} ...] {ifin}{cmd:,} {opt mo:del(string)} 
{opt me:thod(string)}
[{opt l:evel(#)}
{opt ltol(#)}
{opt ptol(#)}
{opt maxi:ter(#)}
{opt diag:nosis}
{opt verb:ose}
{opt a:lpha(newvar)}]


{phang}
where {it:eq} is similar to {help ml##eq:{it:eq}} of {helpb ml:ml}, that is,

{p 8 13 2}
{cmd:(} [{it:eqname}{cmd::}] [{it:{help varlist:varlist_y}}] {cmd:=}
[{indepvars}] [{cmd:,} {cmdab:nocons:tant}] {cmd:)}

{phang}
Note that {cmd:=} is required even if an equation has an empty right-hand
side, that is, if there is only a constant term.  In the first {it:eq}, which
is always without a constant term because of the presence of fixed effects,
{cmd:noconstant} is implicitly assumed, without specifying it.  When a factor
variable appears as {it:indepvars} in more than one {it:eq}, it must appear
with a different name in each {it:eq}.  Hence, one has to generate a
duplicate of the underlying variable for each additional {it:eq} where the
factor variable appears.

{phang}
{indepvars}, but not {depvar} or {it:{help varlist:varlist_y}}, may contain
time-series operators and factor variables; see {help tsvarlist:tsvarlist} and
{help fvvarlist:fvvarlist}.

{phang}
{help regress:{it:regress_options}} are options of the {helpb regress:regress}
command.  They do not affect the estimation of the model parameters but do
affect the estimated covariance matrix of the estimator.  See
{help xtspj##Remarks:{it:Remarks}} below.


{title:Description}

{pstd}
{cmd:xtspj} implements the first-order split-panel jackknife (also termed the
half-panel jackknife) of Dhaene and Jochmans (2015) for possibly nonlinear
panel-data models with fixed effects.  These models produce a potentially
inconsistent maximum-likelihood (ML) estimate of the common parameters theta
(the parameters common to all cross-sectional units and time periods).  The
data, (Y_it,X_it), are indexed with i referring to the cross-sectional unit
(or individual) and t to the time period.  For each individual, there is an
individual-specific but time-invariant parameter a_i.

{pstd}
{cmd:xtspj} accepts both balanced and unbalanced datasets, but there must be
no gaps in the time series.  Missing data are excluded automatically.  The
data must be {helpb xtset:xtset} with both {it:panelvar} and {it:timevar}.  If
the regressand is a lagged variable, it has to be generated manually prior to
estimation.  When the regressors contain lagged values of the regressand up to
p lags, the model becomes autoregressive of order p and is estimated
conditional on the first p observations.  The maximization of the likelihood
function is carried out by the Newton-Raphson algorithm.

{pstd}
{cmd:xtspj} may also be used as a fast (but less flexible) alternative to
{helpb ml:ml} for ML estimation of panel-data models with fixed effects.


{title:Options}

{phang}
{opt model(string)} specifies the type of regression model to be fit: probit
({cmd:probit}), logit ({cmd:logit}), linear ({cmd:linear}), negative binomial
({cmd:negbin}), Poisson ({cmd:poisson}), exponential ({cmd:exponential}),
gamma ({cmd:gamma}), Weibull ({cmd:weibull}), or some other, user-written
model.  For user-written models, see 
{help xtspj##UserWrittenModels:{it:User-written models}}.  The linear model
can also be fit by specifying {cmd:model(regress)}; see 
{help xtspj##Remarks:{it:Remarks}} below.  {cmd:model()} is required.

{phang}
{opt method(string)} takes {cmd:none}, {cmd:like}, or {cmd:parm} for,
respectively, no correction (the usual ML estimate theta_hat), the split-panel
jackknife estimate based on the jackknifed log likelihood (theta_dot), and the
split-panel jackknife ML estimate (theta_tilde).  {cmd:method()} is required.

{phang}
{opt level(#)} sets the confidence level.  The default is {cmd:level(95)}.

{phang}
{opt ltol(#)} sets the tolerance level for changes in the objective function
value.  When the difference between the objective function values of the
current and the previous iteration, divided by the absolute value of the
current objective function value, is nonnegative and less than {opt ltol(#)},
the algorithm stops and reports that convergence has been achieved.  The
default is {cmd:ltol(1e-4)}.

{phang}
{opt ptol(#)} sets the tolerance level for changes in the parameter values.
The algorithm stops and reports convergence when the change in the parameter
is smaller than {opt ptol(#)}.  The change is computed as the absolute
difference between the parameter values in the current and the previous
iteration.  When the parameter is a vector, the maximum element of the vector
of absolute differences is taken.  The default is {cmd:ptol(1e-4)}.

{phang}
{opt maxiter(#)} sets the maximum number of iterations the algorithm is
allowed to use.  The default is {cmd:maxiter(100)}.

{phang}
{opt diagnosis} specifies that a simple diagnostic algorithm be invoked when
the Newton-Raphson algorithm gives an updated parameter vector that does not
improve the objective function value (see {help xtspj##Remarks:{it:Remarks}}
below).  The diagnostic algorithm is slow and disabled by default.

{phang}
{opt verbose} specifies whether the iteration log of the maximization (that
is, the objective function values) and extra notes (for example, about omitted
individuals) should be displayed on the screen.  When {cmd:method(parm)} is
requested, the iteration log can be lengthy.  It is disabled by default.

{phang}
{opt alpha(newvar)} specifies the name of the variable to be created to store
the estimates of the fixed effects (the same value for all observations
corresponding to i).  When {cmd:method(none)} is requested, {it:newvar}
contains the ML estimate of the fixed effects.  When {cmd:method(like)} or
{cmd:method(parm)} is requested, {it:newvar} is the ML estimates of the fixed
effects with theta held fixed in the likelihood function at theta_dot and
theta_tilde, respectively.


{marker Remarks}{...}
{title:Remarks}

{pstd}
Covariance matrix

{pin}
The covariance matrix of the estimated theta is computed as follows.  When no
correction is requested, the usual {cmd:oim} covariance matrix based on the
Hessian of the concentrated log likelihood is computed (see 
{help vce_option:{it:vce_option}}).  When {cmd:method(like)} or
{cmd:method(parm)} is requested, the covariance matrix is obtained from the
same Hessian but now evaluated at the corresponding estimate, theta_dot or
theta_tilde.  This requires maximizing the uncorrected log likelihood with
respect to the fixed-effects parameters while theta is kept fixed at theta_dot
or theta_tilde.  For this maximization, the options {opt ltol()}, 
{opt ptol()}, and {opt maxiter()} are also effective.

{pstd}
Option {opt diagnosis}

{pin}
When {opt diagnosis} is turned on, a diagnostic algorithm is invoked every
time the current objective function value in the Newton-Raphson algorithm is
less than that in the previous iteration.  Given the gradient vector and
Hessian matrix obtained from the previous iteration, the algorithm
successively reduces the step size, used to update the parameter vector, from
1 to 0.5, 0.5^2, ..., possibly down to a minimum value of 0.5^10.  The
objective function is evaluated at the parameter vector updated with the
reduced step size.  When the objective function value improves on that from
the previous iteration, the diagnostic algorithm stops, and maximization
continues from the currently updated parameter vector.  Otherwise, it keeps
reducing the step size unless the minimum step size is already reached, in
which case the diagnostic algorithm reports that no improvement could be
achieved by reducing the step size, it stops, and maximization continues from
the parameter vector obtained before the diagnostic algorithm was called.

{pstd}
Check of multicollinearity

{pin}
By default, the program runs a multicollinearity check {it:eq}-wise for each
half-panel, that is, on the set of regressors of each equation, with
observations corresponding to each half-panel.  The check is performed using
the command {helpb _rmcoll:_rmcoll}, and the coefficients of the omitted
variables are kept in the regression but fixed at 0 and assigned a variance of
0.

{pin}
When {help fvvarlist:fvvarlist} is present, the indicators corresponding to
the base level (marked with {cmd:b.}) and those marked with {cmd:o.} by
{helpb fvexpand:fvexpand} are omitted from the estimation in the same way as
above.

{pstd}
Noninformative individuals

{pin}
For the probit and logit regression models, a check is performed for each
individual i in each of the subpanels to ensure that the regressand varies
across t.  When the regressand does not vary across t, the individual is
omitted from the estimation altogether because it is not informative.  A
similar check is also performed in, for example, {helpb probit:probit}.
Similarly, for the Poisson and negative binomial regression models,
individuals for which the regressand is zero for all t are uninformative and
are therefore dropped.  For the exponential, gamma, and Weibull regression
models, individuals for which the regressand is nonpositive are dropped; this
is similar to {helpb streg:streg}.

{pstd}
{cmd:model(linear)} versus {cmd:model(regress)}

{pin}
When {cmd:model(linear)} is specified, the linear model estimates are computed
by maximization of a Gaussian log likelihood using the Newton-Raphson
algorithm.  When {cmd:model(regress)} is specified, the linear model estimates
are computed using least squares-based closed-form expressions.  Apart from
small numerical differences, the two ways of computing the estimates give
identical results.

{pin}
When {cmd:model(regress)} is specified, no estimate of the error variance is
reported, as is often the case for least squares-based procedures.  When
{cmd:model(linear)} is specified, the estimated error variance is reported, as
is standard for ML-based procedures.  For the implementation,
{cmd:model(regress)} computes within-group estimates.  If {cmd:method(none)}
is specified, the resulting coefficient estimates are identical to those
obtained from {helpb xtreg:xtreg, fe}.

{pin}
Users are advised to use {cmd:model(regress)} when the error variance is not
of interest.  Because {cmd:model(regress)} uses closed-form expressions to
compute the estimates, it is faster and more accurate than
{cmd:model(linear)}.

{pin}
When {cmd:model(regress)} is specified, the options {opt ltol(#)}, 
{opt ptol(#)}, {opt maxiter(#)}, {opt diagonsis}, and {opt verbose} are
ineffective (that is, they are ignored).  The option {opt alpha(newvar)}
remains effective.

{pin}
When {cmd:model(regress)} is specified, the estimated covariance matrix is
obtained from the {helpb regress:regress} command by fitting the model using
the same dataset but after the within transformation.  Here, when
{cmd:regress} is called, the {opt noconstant} option is activated and cannot
be overridden.  However, other options for the {cmd:regress} command can be
specified via {help regress:{it:regress_options}} to control the estimation of
the covariance matrix.  In particular, {cmd:robust} can be specified to obtain
robust covariance matrix estimates.

{pstd}
Model parameterizations

{pin}
The parameterizations for the probit, logit, and Poisson regression models are
standard.  For the negative binomial model, the parameterization is the same
as for {helpb nbreg:nbreg}.

{pin}
For the gamma regression model, the log likelihood is specified as

{pin2}
log[f(Y_it|X_it; theta, a_i, kappa)] = - log[gamma(kappa)] - kappa * log(u_it) - (kappa - 1) * log(Y_it) - Y_it / u_it

{pin}
with

{pin2}
u_it = exp(X'_it * theta + a_i)

{pin}
where a_i is the ith fixed effect and kappa is a scalar shape parameter.  This
parameterization is the same as for {helpb glm:glm, family(gamma) link(log)}
for the regression slope coefficients; we also report the shape parameter
estimate.

{pin}
For the exponential regression model, the log likelihood is specified as

{pin2}
log[f(Y_it|X_it; theta, a_i)] = log(1/u_it) - Y_it / u_it

{pin}
which is the same as for {cmd:glm, family(gamma) link(log)}.

{pin}
For the Weibull regression model, the log likelihood is specified as

{pin2}
log[f(Y_it|X_it; theta, a_i, k)] = log(kappa) + (kappa-1) * log(Y_it) + log(u_it) - u_it * Y_it ^ kappa

{pin}
where kappa=exp(k).


{title:Examples}

{pstd}Data setup{p_end}
{phang2}{cmd:. webuse chicken}{p_end}
{phang2}{cmd:. generate worker=.}{p_end}
{phang2}{cmd:. by restaurant: replace worker=_n}{p_end}
{phang2}{cmd:. xtset restaurant worker}{p_end}
{phang2}{cmd:. label define sex 0 "female" 1 "male"}{p_end}
{phang2}{cmd:. label values gender sex}{p_end}

{pstd}Uncorrected fixed-effects probit ML{p_end}
{phang2}{cmd:. xtspj complain age i.gender, model(probit) method(none)}{p_end}

{pstd}Jackknifed log likelihood{p_end}
{phang2}{cmd:. xtspj complain age i.gender, model(probit) method(like)}{p_end}

{pstd}Jackknifed ML{p_end}
{phang2}{cmd:. xtspj complain age i.gender, model(probit) method(parm)}{p_end}


{title:Stored results}

{pstd}
{cmd:xtspj} stores the following in {cmd:e()}:

{synoptset 20 tabbed}{...}
{p2col 5 20 24 2: Scalars}{p_end}
{synopt:{cmd:e(N)}}number of observations{p_end}
{synopt:{cmd:e(empty)}}contains {cmd:1} if there are no covariates in the model{p_end}
{synopt:{cmd:e(converged)}}contains {cmd:1} if the maximization converged{p_end}
{synopt:{cmd:e(ll)}} log-likelihood value (only for {cmd:method(none)} or
{cmd:method(like)} when {cmd:model(regress)} is not specified){p_end}

{p2col 5 20 24 2: Macros}{p_end}
{synopt:{cmd:e(cmd)}}{cmd:xtspj}{p_end}
{synopt:{cmd:e(cmdline)}}command as typed{p_end}
{synopt:{cmd:e(title)}}title of table of results{p_end}
{synopt:{cmd:e(vce)}}{cmd:oim} or {it:vcetype} specified in {cmd:vce()} (for {cmd:model(regress)} only){p_end}
{synopt:{cmd:e(vcetype)}}title used to label Std. Err. (for {cmd:model(regress)} only){p_end}
{synopt:{cmd:e(properties)}}{cmd:b V}{p_end}
{synopt:{cmd:e(model)}}value of {opt model()}{p_end}
{synopt:{cmd:e(method)}}value of {opt method()}{p_end}
{synopt:{cmd:e(depvar)}}the regressands or the list of regressands (when a
user-written model with several {it:{help ml##eq:eq}} was specified){p_end}

{p2col 5 20 24 2: Matrices}{p_end}
{synopt:{cmd:e(b)}}coefficient vector{p_end}
{synopt:{cmd:e(V)}}covariance matrix{p_end}

{p2col 5 20 24 2: Functions}{p_end}
{synopt:{cmd:e(sample)}}marks estimation sample{p_end}


{marker UserWrittenModels}{...}
{title:User-written models}

{pstd}
Parameterization of the density

{pin}
To use {cmd:xtspj} with a user-written model, the density has to be of the form

{pin}
f(Y_it|X_it; u_1_it, ..., u_M_it)

{pin}
with

{pin}
u_1_it = X'_1_it * theta_1 + a_i{p_end}
{pin}
u_m_it = X'_m_it * theta_m

{pin}
where a_i is the fixed effect of the ith individual and m=2,...,M for an
arbitrary number of M>=1 equations depending on covariates X_m_it.  The
regressands Y_it can be a vector, at most of dimension M.  In the first
equation, u_1_it contains an additive fixed effect.  Nevertheless, this design
is general in that when the fixed effects enter nonadditively, one simply
needs to keep the first equation free of covariates.  It is also possible to
set X_m_it=1 for one or several m>1; then the corresponding u_m_it=theta_m are
simply parameters entering the model.

{pstd}
User-written evaluator

{pin}
The likelihood function has to be specified as a {cmd:mata} 
{helpb m2_class:class} containing two member functions {cmd:::Evaluate()} and
{cmd:::Check()}.  The template for the class is

{marker ClassTemplate}{...}
{cmd:	1 | mata}
{cmd:	2 | class xtspj{it:YourModel} extends xtspjModel {c -(}}
{cmd:	3 | 	public:}
{cmd:	4 | 		void Evaluate(), Check()}
{cmd:	5 | {c )-}}
{cmd:	6 | void function xtspj{it:YourModel}::Evaluate(}
{cmd:	7 | 		real matrix Y,}
{cmd:	8 | 		real matrix XB,}
{cmd:	9 | 		real colvector LogLikelihood,}
{cmd:	10|  		real matrix Gradient,}
{cmd:	11|  		pointer(real colvector) matrix Hessian) {c -(}}
{cmd:	12|  			// compute the log likelihood, gradient, and Hessian}
{cmd:	13|  {c )-}}
{cmd:	14|  void function xtspj{it:YourModel}::Check(}
{cmd:	15|  		real matrix Data,}
{cmd:	16|  		real scalar Keep) {c -(}}
{cmd:	17| 			// check data}
{cmd:	18| {c )-}}
{cmd:	19| end}

{pin}
The user needs to supply the relevant Mata code in lines 12 and 17 as
discussed below and change {cmd:YourModel} (lines 2, 6, and 14) to a desired
name, for example, {cmd:MyModel}.  When {cmd:xtspj} is executed, this name
must be specified in the option {opt model(string)}, for example,
{cmd:model(MyModel)}, so that {cmd: xtspj} calls the user-written
specification of the log-likelihood function.

{pin}
{ul on}{cmd:void function ::Evaluate()}{ul off}

{pin}
This function computes the log likelihood, gradient, and Hessian for a given
set of observations (i,t).  The function is called repeatedly and separately
for each i and various sets {t}.  The arguments of {cmd:::Evaluate()} are the
following:

{phang2}
- {cmd:real matrix Y}, the matrix of regressands.  The kth column of Y
  corresponds to the kth regressand defined by an {it:{help ml##eq:eq}}, and
  each row corresponds to an observation (i,t) with t belonging to a specific
  set {t}.

{phang2}
- {cmd:real matrix XB}, the matrix of linear indices (u_m_it for 1<=m<=M).
  The mth column corresponds to the mth equation, and each row corresponds to
  an observation (i,t).

{phang2}
- {cmd:real colvector LogLikelihood}, the column vector of log-likelihood
  values.  Each row contains the log-likelihood value corresponding to an
  observation (i,t) evaluated at the corresponding row of {cmd:XB} and
  {cmd:Y}; that is,

    			log[f(Y_it|X_it; u_1_it, ..., u_M_it)] 

{phang3}
where t belongs to a given {t}.

{phang2}
- {cmd:real matrix Gradient}, the matrix of scores.  The mth column of
  {cmd:Gradient} corresponds to the log-likelihood derivative with respect to
  the mth equation, evaluated at {cmd:XB} and {cmd:Y}; that is,

    			d log[f(Y_it|X_it; u_1_it, ..., u_M_it)] 
    			---------------------------------------- 
    			            d u_m_it                

{phang3}
where t belongs to a given {t}.

{phang2}
- {cmd:pointer(real colvector) matrix Hessian}, the 
  {helpb m2_pointers:pointer} matrix pointing to the Hessian.  The matrix must
  be {help mf_makesymmetric:made symmetric}.  The (m,n)th element of Hessian
  is a pointer to the column vector of second-order log-likelihood derivatives
  with respect to the mth and nth equations, evaluated at {cmd:XB} and
  {cmd:Y}; that is,

    			d^2 log[f(Y_it|X_it; u_1_it, ..., u_M_it)] 
    			------------------------------------------ 
    			          d u_m_it  d u_n_it          

{phang3}
where t belongs to a given {t}.

{pin}
Here the arguments {cmd:real matrix gradient} and 
{cmd:pointer(real colvector) matrix Hessian} are optional.  If they are not
changed inside {cmd:::Evaluate()}, {cmd:xtspj} uses a numerical
differentiation algorithm to compute the derivatives.

{pin}
Below is an example of {cmd:::Evaluate()} for the probit model:

{cmd:	1 | void function xtspjprobit::Evaluate(}
{cmd:	2 | 		real matrix Y,}
{cmd:	3 | 		real matrix XB,}
{cmd:	4 | 		real colvector LogLikelihood,}
{cmd:	5 |  		real matrix Gradient,}
{cmd:	6 |  		pointer(real colvector) matrix Hessian) {c -(}}
{cmd:	7 |  			q=(Y:*2:-1)}
{cmd:	8 |  			LogLikelihood=lnnormal(q:*XB)}
{cmd:	9 |  			Gradient=q:*(normalden(q:*XB):/normal(q:*XB))}
{cmd:	10|  			Hessian=&(-Gradient:*(XB+Gradient))}
{cmd:	11|  {c )-}}

{pin}
Here the first point to be noted is that {it:YourModel} is replaced with
{cmd:probit}.  The {cmd:LogLikelihood}, {cmd:Gradient}, and {cmd:Hessian} are
computed in lines 8 to 10.  Lines 9 and 10 can be omitted for a numerical
differentiation algorithm to take over.

{pin}
When a model contains more than one equation (M>1), the variable
{cmd:Gradient} and {cmd:Hessian} may need to be preallocated to expedite the
computation.  To do so, simply insert

{cmd:	1 | Gradient=J(rows(Data),cols(Data),.)}
{cmd:	2 | Hessian=J(rows(Data),cols(Data),NULL)}

{pin}
between lines 6 and 7.  In addition, one should avoid reusing variables in the
calculation of the Hessian; that is, code such as

{cmd:	1 | H=...}
{cmd:	2 | Hessian[1,1]=&H}
{cmd:	3 | H=...}
{cmd:	4 | Hessian[1,2]=&H}

{pin}
should be replaced by

{cmd:	1 | H11=...}
{cmd:	2 | Hessian[1,1]=&H11}
{cmd:	3 | H12=...}
{cmd:	4 | Hessian[1,2]=&H12}

{pin}
{ul on}{cmd:void function ::Check()}{ul off}

{pin}
If a check is not desired, one can simply replace line 17 in 
{help xtspj##ClassTemplate:this code} with {cmd:Keep=1}.  To perform a
model-specific check of the data, one has to supply suitable code to
{cmd:::Check()}.  Similarly to {cmd:::Evaluate()}, the function is called
repeatedly and separately for each i and various sets {t}.  The arguments are
the following:

{phang2}
- {cmd:real matrix Data}, the data to be checked.  The columns of {cmd:Data}
  correspond to the regressands of the model, followed by the regressors, in
  the same order as specified by the equations.  The rows correspond to the
  observations (i,t), with t belonging to a given set {t}.

{phang2}
- {cmd:real scalar Keep}, the main output.  {cmd:Keep} is set to {cmd:1} if
  {cmd:Data} passes the check and to {cmd:0} otherwise.

{pin}
Below is an example of {cmd:::Check()} for the probit model:

{cmd:	1 |  void function xtspjprobit::Check(}
{cmd:	2 |  		real matrix Data,}
{cmd:	3 |  		real scalar Keep) {c -(}}
{cmd:	4 | 			if (sum(Data[.,1])==0 || sum(Data[.,1])==rows(Data)) {c -(}}
{cmd:	5 | 				Keep=0}
{cmd:	6 | 			{c )-} else {c -(}}
{cmd:	7 | 				Keep=1}
{cmd:	8 | 			{c )-}}
{cmd:	9 | {c )-}}


{title:Reference}

{phang}
Dhaene, G., and K. Jochmans. 2015. Split-panel jackknife estimation of
fixed-effect models. {it:Review of Economic Studies} 82: 991-1030.


{title:Authors}

{pstd}
Yutao Sun{break}
Northeast Normal University{break}
School of Economics{break}
Changchun, China{break}
and Erasmus University Rotterdam{break}
Rotterdam, The Netherlands{break}
sunyt100@nenu.edu.cn

{pstd}
Geert Dhaene{break}
KU Leuven{break}
Department of Economics{break}
Leuven, Belgium{break}
geert.dhaene@kuleuven.be


{title:Also see}

{p 4 14 2}
Article:  {it:Stata Journal}, volume 19, number 2: {browse "https://doi.org/10.1177/1536867X19854016":st0557}{p_end}
