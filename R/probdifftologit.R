#' @title Proportion Difference to Logit
#'
#' @description
#' Converts a difference in proportions to a difference of log odds (logits).
#' Note that the relationship of a difference in proportion to a difference in
#' logits depends on the positioning on the overall scale of propotions; thus,
#' this function also requires specifying an intercept term from the proportions.
#'
#' @param x A difference of proportions.
#' @param intercept The intercept for the model.
#' @seealso \code{\link{logit}} to transform individual probabilities into logits.
#' @examples
#' probdifftologit(x=.20, intercept=.50)
#' logit(.60) - logit (.40)
#' probdifftologit(x=.20, intercept=.75)
#' logit(.85) - logit (.65)
#'
#' @export

probdifftologit <- function(x, intercept=.50) {
	odds1 <- (intercept + (x/2)) / (1-(intercept+(x/2)))
	odds2 <- (intercept - (x/2)) / (1-(intercept-(x/2)))
	ratio <- odds1/odds2
	logiteffect <- log(ratio)
	return(logiteffect)
}

