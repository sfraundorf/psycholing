#' @title Generalized Linear Mixed Effect Model Summary with Exponeniated
#' Coefficients
#'
#' @description
#' For a \code{glm} or \code{glmer} model fit using the logit or log link function, displays
#' a model summary with the exponentiation function applied to the fixed effect
#' point estimates. Thus, estimates of effects on the \emph{log odds} in a
#' logistic model become estimates of effects on the \emph{odds}, which are
#' usually more interpretable.  Similarly, estimates of effects on the \emph{log
#' counts} in a Poisson regression become estimate of effcts on the
#' \emph{counts} to facilitate interpretation.
#'
#' @details
#' Note that once the coefficients have been exponeniated, the estimates now
#' represent \emph{multiplicative} effects.  For instance, a variable associated
#' with an odds ratio of 1.25 in a logistic regression means the variable is
#' associated with 1.25 TIMES the odds ofsuccess.  (This is different from
#' regular linear regression, where the effects are additive.)
#'
#' Consequently, a factor that has no effect on the response variable would have
#' an estimate of 1 rather than 0.  (Multiplying something by 1 leaves it
#' unchanged, of course.)
#'
#' 95% confidence intervals for the estimates are also displayed.  Because the
#' effects are multiplicative, the CIs will NOT be symmetric about the point
#' estimate on the original scale.  Thus, it would be preferable to report the
#' lower- and upper-bound of the CI (which is the current APA style for
#' confidence intervals anyway) rather than its width.
#' @param model \code{glmer} model to be summarized.
#' @param confidence desired level of confidence for the confidence interval.
#' @return model summary including exponentiated fixed effect estimates and
#' confidence intervals.
#' @examples
#' data(VerbAgg, package='lme4')
#' model1 <- glmer(r2 ~ Gender + btype+ (1|id) + (1|item),
#'   family=binomial, data=VerbAgg)
#' summaryExp(model1)
#' @export
#' @importFrom stats family qnorm

summaryExp <- function(model, confidence=.95) {
	# Check the model family:
	if (family(model)$family == 'binomial') {
		coef.column.name <- 'Odds Ratio'
	} else if (family(model)$family == 'poisson') {
		coef.column.name <- 'Incident Rate Ratio'
	} else {
		stop('Not a supported model family.')
	}

	# Check the link function:
	if (family(model)$link != 'logit') {
		stop('Not a supported link function.')
	}

	# Change percents into proportions if needed:
	if (confidence > 1) {
		confidence = confidence/100
	}

	# Get the existing model summary:
	model.summary <- summary(model)
	coefs <- model.summary$coefficients

	# Exponeniate the coefficients:
	exp.estimate <- exp(coefs[,1])

	# Calculate confidence interval:
	z <- qnorm((1-confidence)/2, lower.tail=FALSE)
	ci.lower <- exp(coefs[,1] - (z * coefs[,2]))
	ci.upper <- exp(coefs[,1] + (z * coefs[,2]))

	# Add these to the coefficients table:
	model.summary$coefficients <- cbind('Estimate'=coefs[,'Estimate'],
	                                    'Std. Error'=coefs[,'Std. Error'],
	                                    'z value'=coefs[,'z value'],
	                                    'Exp Coeff'=exp.estimate,
	                                    'Lower CI'=ci.lower,
	                                    'Upper CI'=ci.upper,
	                                    'Pr(>|z|)'=coefs[,'Pr(>|z|)'])
	colnames(model.summary$coefficients)[colnames(model.summary$coefficients)
	    == 'Exp Coeff'] <- coef.column.name

	# Display:
	model.summary
}
