#' @title Logit-to-Proportion Transformation
#'
#' @description
#' Back-transforms logits to proportions.  The logit
#' represents the log odds of a "hit" or a 1--that is, log(p(hit)/(1-p(hit))--
#' and is a better transformation to proportions for approximating the normal
#' distribution than is the arcsine transformation.
#'
#' @param x logit value to transformed into a proportion
#' @seealso \code{\link{logit}} to transform proportions into logits.
#' @examples
#' logittoprob(1.37)
#' logittoprob(logit(0.25))
#'
#' @export

logittoprob <- function(x) {
	exp(x)/(1+exp(x))
}

