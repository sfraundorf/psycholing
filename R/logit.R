#' @title logit transformation
#'
#' @description
#' Applies the logit transformation to one or more proportions.  The logit
#' represents the log odds of a "hit" or a 1--that is, log(p(hit)/(1-p(hit))--
#' and is a better transformation to proportions for approximating the normal
#' distribution than is the arcsine transformation.
#'
#' @details
#' Note that the logit is also a link function used in logistic regression to
#' model binomial outcomes.  If the binomial outcomes are available--i.e.,
#' observations at the level of individual "hits" or "misses" (0 or 1s), rather
#' than a proportion aggregated over multiple observations--then logistic
#' regression may be directly applied to those data, and applying any sort of
#' transformation to the proportions is unnecessary.
#'
#' The logit is undefined when p(hit) = 1 or p(hit) = 0; in addition, when
#' p(hit) is close to 0 or 1, the logit is defined but not particularly stable.
#' In these cases, the empirical logit may be useful instead.
#' @param x proportion(s) to which the logit transformation should be applied.
#' @return numeric vector with the logit-transformed proportions
#' @references Jaeger, T. F. (2008). Categorical data analysis: Away from ANOVAs
#' (transformation or not) and towards logit mixed models. \emph{Journal of
#' Memory and Language}, \emph{59}, 434-446.
#' @seealso \code{\link{logittoprob}} to transform logits back into probabilities.
#' @examples
#' mydata = data.frame(Accuracy.Proportion = c(0.5,0.75,0.90,0.2))
#' mydata$Accuracy.Logit <- logit(mydata$Accuracy.Proportion)
#' @export

logit <- function(x) {
	log(x/(1-x))
}

