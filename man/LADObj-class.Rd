\name{LADObj-class}
\Rdversion{1.1}
\docType{class}
\alias{LADObj-class}
\alias{LADObj}
\alias{coefficients,LADObj-method}
\alias{predict,LADObj-method}
\alias{print,LADObj-method}
\alias{residuals,LADObj-method}
\alias{show,LADObj-method}
\alias{summary,LADObj-method}

\title{Class \code{"LADObj"}}
\description{The \code{LADObj} class holds model specification and output from
 the Least Absolute Deviation \code{lad} and Hypothesis Test \code{hypothesis.test} commands.  The \code{summary.LADObj} class
represents the summary of these objects.
}

\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("LADObj", ...)} or, more commonly, via the
  \code{\link{lad}} or \code{\link{hypothesis.test}} function call.}


\section{Slots}{
  \describe{
  
    \item{\code{inputData}:}{A \code{data.frame} used for the analysis.  The first column is the group (if supplied) and the second is the variable for which the analysis was performed.}
    \item{\code{NumObs}:}{The total number of observations in the original data set.}
    \item{\code{NumVars}:}{The total number of variables in the original data set.}
    \item{\code{theta}:}{The quantile used if quantile regression is desired.}
    \item{\code{NumPerm}:}{The number of permutations used.}
    \item{\code{Test}:}{A logical indicator of whether a hypothesis test was performed.}
    \item{\code{DoublePermutation}:}{A logical indicator of whether a double permutation was used this should only be \code{TRUE} for \code{hypothesis.test}.}
    \item{\code{T_o}:}{The dispersion test statistic equal to (sum of absolute deviations for reduced model -
                           sum of absolute deviations for full model) / sum of absolute deviations for full model.}
    \item{\code{AsyRankScore}:}{If the option \code{rank.score} is set equal to true in the hypothesis test then
          this value contains the asymptotic rank score statistic.}
    \item{\code{P_value}:}{The probability of a smaller or equal delta under the null hypothesis.}
    \item{\code{P_valueTN}:}{The P-Value of Asymptotic rank score statistic.}
    \item{\code{Betas}:}{A vector of regression coefficients in the full model fit.}
    \item{\code{RedBetas}:}{A vector of regression coefficients in the reduced model fit (if applicable).}
    \item{\code{SumAbsValRes}:}{The sum of absolute values of the residuals in the full model.}
    \item{\code{SumAbsValResRed}:}{The sum of absolute values of the residuals in the reduced model (if applicable).}
    \item{\code{WtSumAbsDevsFulMod}:}{The weighted sum of the absolute deviations in the full model.}
    \item{\code{WtSumAbsDevsRedMod}:}{The weighted sum of the absolute deviations in the full model (if applicable).}
    \item{\code{NumIter}:}{The number of iterations for model convergence.}
    \item{\code{ExitCode}:}{A numeric code indicating whether a solution was successfully found:
                                \itemize{
        \item{ ExitCode = 1:  }{Successful}
        \item{ ExitCode = 2:  }{Rounding Error}
        \item{ ExitCode = 0:  }{Non-Unique}
        \item{ ExitCode = 7:  }{Multiple Solutions}
      }}
    \item{\code{PermVals}:}{if \code{save.test} was set equal to \code{TRUE} and a
         hypothesis test was performed then this is a vector of the permutation values of the test statistic.}
    \item{\code{HasIntercept}:}{A logical vector indicating whether the full and reduced models include intercept terms.}
    \item{\code{DoAllQuants}:}{A logical indicator of whether all quantile regression estimates were calculated.}
    \item{\code{DoRankScore}:}{A logical indicator of whether a rank score test was performed.}
    \item{\code{IsOLS}:}{A logical indicator of whether ordinary least squares regression was performed.}
    \item{\code{NumLaVars}:}{A vector with the number of variables in the full and reduced models.}
    \item{\code{ResRed}:}{Residuals from the reduced model (if applicable).}
    \item{\code{Resids}:}{Residuals from the full model.}
    \item{\code{Call}:}{The original function call.}
    \item{\code{response}:}{The response vector used in the model fit.}
    \item{\code{full.mod.names}:}{The names of predictors used in the full model for hypothesis testing.}
    \item{\code{QuantOut}:}{If \code{all.quants} was true in the function code this is a matrix containing the quantile values.
         Using the function \code{QuantValues()} will display these in a useful manner.}
  }
}
\section{Methods}{
  \describe{
    \item{coefficients}{\code{signature(x = "LADObj")}: Returns the coefficients from the model fit.}
    \item{predict}{\code{signature(x = "LADObj")}: Reports the predicted values from the model fit.}
    \item{print}{\code{signature(x = "LADObj")}: Same as \code{summary}.}
    \item{pvalue}{\code{signature(x = "LADObj")}: Returns the p-value from the \code{LadObj} assuming a hypothesis test was specified.}
    \item{QuantValues}{\code{signature(x = "LadObj")}: Reports all quantile regression estimates if \code{all.quants} was set to \code{TRUE}.}
    \item{ResampVals}{\code{signature(x = "LADObj")}: Returns the resample values if applicable.}
    \item{residuals}{\code{signature(x = "LADObj")}: Returns the residuals from the full model fit.}
    \item{show}{\code{signature(object = "LADObj")}: Same as \code{summary}.}
    \item{summary}{\code{signature(object = "LADObj")}:  Prints a detailed summary of the LAD fit.}
	 }
}
\references{
Cade, B.S. and J.D. Richards. 1996. Permutation tests for least absolute deviation regression. \emph{Biometrics} \bold{52}, 886--902.
}
\author{
Marian Talbert
}

\seealso{
\code{\link{lad}}, \code{\link{hypothesis.test}}

}
\examples{
Out <- lad(lnlctm~widrat,data = lahontan,all.quants = TRUE)
summary(Out)
AllQuants <- QuantValues(Out)

}
\keyword{classes}
