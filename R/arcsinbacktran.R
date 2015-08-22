#' @title Arcsine back-transformation
#'
#' @description
#' Given a set of arcsine-transformated proportions X, back-transforms them back to "raw" proportions; that is, undoes the arcsine transformation.
#' @param x Arcsine-transformed proportion(s) to be back-transformed back to proportions.
#' @return numeric vector with the original proportions
#' @seealso \code{\link{arcsintran}} for the arcsine transformation
#' @examples
#' mydata <- data.frame(Accuracy.Proportion = c(0.5,0.75,0.99,0, 0.2))
#' mydata$Accuracy.Arcsin <- arcsintran(mydata$Accuracy.Proportion)
#' mydata$Accuracy.Backtran <- arcsintran(mydata$Accuracy.Arcsin)
#' mydata # same as original proportions
#' @export

arcsinbacktran <- function(x) {
		
	(sin(x))^2
	}