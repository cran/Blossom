\name{lad}
\alias{lad}

\title{
Least absolute deviation
}
\description{
Least absolute deviation (LAD) regression is an alternative to ordinary least
squares (OLS) regression that has greater power for thick-tailed symmetric and
asymmetric error distributions (Cade and Richards 1996). LAD regression estimates
the conditional median (a conditional 0.50 quantile) of a dependent variable given
the independent variable(s) by minimizing sums of absolute deviations between
observed and predicted values.  LAD regression can be used anywhere OLS regression would be used but is often
more desirable because it is less sensitive to outlying data points and is more
efficient for skewed error distributions as well as some symmetric error distributions.
}
\usage{
lad(formula, data, contrasts = NULL, number.perms = 5000,
   quant, test = FALSE, all.quants = FALSE, OLS,weights)
}

\arguments{
  \item{formula}{an object of class \code{\link{formula}} or a statement that can be coerced to this class.}
  \item{data}{a data frame or object that can be coerced to one using \code{\link{as.data.frame}} containing the variables and response in the model.
    If missing the formula is evaluated in the environment from which the original function call was made.}
  \item{contrasts}{an optional list see \code{\link{contrasts}} for further details.}
  \item{number.perms}{the number of permutations used if a Monte Carlo resampling procedure is specified.}
  \item{quant}{a numeric value which specifies a quantile for which all subsequent testing is done.}
  \item{test}{a logical indicating whether to test if all slope parameters are equal to zero.}                
  \item{all.quants}{a logical indicating whether all possible quantile regression estimates should be returned.}
  \item{weights}{a vector of weights to be applied to the response and model frame matrix.}
  \item{OLS}{a logical indicating whether ordinary least squares regression should be performed.}
}
\details{
The \code{lad} command can be used to fit a variety of least absolute deviation regressions.
The \code{hypothesis.test} command allows the specification of reduced parameter LAD regression
model to compare with the full parameter regression model. The regressions are run using the \code{lad}
command and the tests performed with the \code{hypothesis.test} command. If the \code{quant = num} option
is specified, all subsequent testing is done on the specified conditional quantile.  By default the model
will include a constant term.  To specify the exclusion of the constant term use \code{-1} in the formula specification.

The \code{number.perms = num} option allows the user to specify more or fewer permutations than the default of 5,000 used in approximating probabilities.
The \code{save.test = TRUE} option specifies that predicted values, residuals, and model variables are to be stored as part
of the LAD object. The fitted values and residuals can be accessed using the commands \code{predict(LadObj)} and \code{residuals(LadObj)}.

The \code{quant = num} option specifies a regression quantile, where the number specified must be greater than 0.0 and less than 1.0.
Specifying \code{all.quants = TRUE} yields all quantile regression estimates and when combined with a \code{save.test=TRUE},
the parameter estimates by quantile are saved as part of the LAD object and can be retrieved using the command \code{\link{QuantValues}}.

The \code{test=TRUE} option in a \code{lad} command is used to test an intercept only null model against the specified model and
the \code{hypothesis.test} command can be used to test a reduced parameter null model.  The \code{hypothesis.test} command requires
 two LAD objects similar to the \code{anova} command commonly used for \code{lm} or \code{glm} objects (Note that it is not possible to
 test a hypothesis when all quantiles were selected with the \code{all.quants = TRUE} option. The dependent variable should be the same
 in both LAD objects and a reduced number of the same independent variables used in the first LAD object should be present in the second
 LAD object.  The \code{rank.score} option bases hypothesis tests on a scoring function of the sign of the residuals for the reduced
 parameter model specified by \code{hypothesis.test}. Asymptotic Chi-square distributional and permutation approximations of P-values
 are both provided. The \code{double.permutation} option provides double permutation for null models that are constrained through the origins,
 for either the drop in dispersion permutation test or the \code{rank.score} test option. The \code{save.test = TRUE} option allows the Monte
 Carlo resampled test statistics to be saved into a single column variable as part of the LAD object, where the first value is always the
 observed test statistic value.  These can be retrieved using the command \code{\link{ResampVals}}.
}
\value{
\code{lad} returns an object of class \code{\link{LADObj}}.

}
\references{
Cade, B.S., and J.D. Richards. 1996. Permutation tests for least absolute deviation regression. \emph{Biometrics} \bold{52}, 886--902.

Cade, B.S. 2005. \emph{Linear models: Permutation methods}. Pages 1049--1054 in B. Everitt and D. Howell, eds. Encyclopedia of Statistics in the Behavioral Science. Vol. 2. John Wiley and Sons.

Cade, B.S., J.D. Richards, and P.W. Mielke, Jr.  2006.  Rank score and permutation testing alternatives for regression quantile estimates.  \emph{Journal of Statistical Computation and Simulation} \bold{76}, 331--355. 

Cade, B.S., and J.D. Richards.  2006.  A permutation test for quantile regression.  \emph{Journal of Agricultural, Biological, and Environmental Statistics} \bold{11}, 106--126. 
}
\author{
Marian Talbert
}


\seealso{
\code{\link{hypothesis.test}}, \code{\link{LADObj}} and \code{\link{summary}}.
}
\examples{

Out <- lad(lcc~-1+apico+picopha,data = fraserf,number.perms = 10000,
 test = TRUE)
 
summary(Out)
residuals(Out)
predict(Out)
coefficients(Out)

Out<-lad(lnlctm~widrat,data = lahontan,all.quants = TRUE)
summary(Out)
AllQuants<-QuantValues(Out)
}

\keyword{nonparametric}
\keyword{htest}
