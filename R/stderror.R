#' @title Standard error
#'
#' @description
#' Calculates the standard error of the mean of x with or without applying the
#' Bessel correction (dividing by \emph{N}-1 rather than \emph{N}).  Default
#' behavior is to apply the correction.	
#'
#' @details
#' Note that Bessel's correction provides an unbiased estimate of the
#' \emph{variance} of the sampling distribution, but the estimate of the
#' \emph{standard deviation} is still biased.
#' @param x value(s) for which the standard error of the mean should be computed
#' @param bessel a logical value indicating whether Bessel's correction should
#' be applied to obtain an unbiased estimate of the variance of the sampling
#' distribution.
#' @param na.rm a logical value indicating whether NA values should be stripped
#' before the computation proceeds.
#' @return standard error of the mean of x
#' @references Bessel's correction. (2015, Aug 15). Retrieved from
#' http://en.wikipedia.org/wiki/Bessel's_correction
#' @references Hays, W.L. (1994). \emph{Statistics} (5th ed.). Belmont, CA:
#' Wadsworth.
#' @examples
#' stderror(c(150, 115, 130, 170, 190))
#' @export
	
stderror <- function(x,bessel=TRUE,na.rm=FALSE) {

	if (na.rm) {
		n <- length(na.omit(x))
	} else {
		n <- length(x)
	}
	sd <- sd(x, na.rm=na.rm)	
	
	if (bessel) {
		sd/sqrt(n-1)
	} else {
		sd/sqrt(n)
	}
}