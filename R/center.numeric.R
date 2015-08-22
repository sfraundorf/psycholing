#' @title Center Numeric Variable About Its Mean
#'
#' @description
#' Centers a numeric variable about its mean.  That is, the mean of the variable
#' becomes the new 0, values greater than the mean are positive, and values
#' smaller than the mean are negative.
#'
#' @details
#' In many cases, mean-centering predictor variables in a regression model
#' often results in a more interpretable intercept.  The intercept captures the
#' predicted \eqn{y} value when all other variables are 0; however, predicting
#' (for instance) reading time when word length is 0 characters long is not very
#' informative because no word can be zero characters long.  Mean-centering the
#' word length predictor would result in the intercept corresponding to
#' predicted reading time at an \emph{average} word length, which is more
#' meaningful.
#'
#' In addition, mean-centering can often facilitate convergence of mixed-effect
#' regression models.
#'
#' However, the choice of whether to center a variable should ultimately be
#' guided by the particular substantive question at hand.
#' @param x numeric vector that should be centered about its mean.
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds.
#' @return numeric vector centered about its mean.
#' @seealso \code{\link{contr.helmert.weighted}} for centering factor
#' (categorical) variables.
#' @references Barr, D.J., Levy, R., Scheepers, C., & Tily, H.J. (2013). Random
#' effects structure for confirmatory hypothesis testing: Keep it maximal.
#' \emph{Journal of Memory and Language}, \emph{68}, 255-278.
#' @examples
#' wordLengths <- c(4, 7, 10)
#' wordLengths.c <- center.numeric(wordLengths)
#' @export

center.numeric <- function(x, na.rm=FALSE) {

	if (!is.numeric(x)) {
		stop('not a numeric vector')
	}

	x - mean(x, na.rm=na.rm)
}
	
	