#' @title \eqn{{R}^2}{R^2} for Linear Mixed Effects Models
#'
#' @description
#' Returns the \eqn{{R}^2}{R^2} value for a linear mixed effect model.
#'
#' @details
#' In a mixed effects model, much of the variance will be "explained" in a
#' theoretically uninformative way simply by the random effects (e.g., "some
#' people are different from others").  To the extent that an \eqn{{R}^2}{R^2}
#' value is useful for these models, it will be useful when comparing the
#' \eqn{{R}^2}{R^2} of a model that incorporates some fixed effect(s) of
#' interest to that of a model that does not.
#'
#' @param model a model of class \code{merMod} for which the \eqn{{R}^2}{R^2}
#' value should be obtained.
#' @return \eqn{{R}^2}{R^2} value between 0 and 1.
#' @references Baayen, R.H. (2008). \emph{Analyzing linguistic data: A practical
#' introduction to statistics}. Cambridge, UK: Cambridge University Press.
#' @references Baayen, R.H., & Milin, P. (2010). \emph{Analyzing reacion times},
#' \emph{3}, 12-28.
#' @export
#' @importFrom stats cor fitted

lmer.rsquared <- function(model) {

	# Check that this was a GAUSSIAN model
	if (model@call[4] != 'gaussian()' && model@call[4] != 'NULL()') {
		# NULL is accepted because the default IS Gaussian
		stop('Gaussian / normal models only')
	}

	# Return squared correlation between fitted & actual Y values
	cor(fitted(model), model@y)^2
}
