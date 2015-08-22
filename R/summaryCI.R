#' @title Linear Mixed Effect Model Summary with Confidence Intervals
#'
#' @description
#' For a \code{lmer} model fit, displays a model summary with confidence
#' intervals. Confidence intervals are obtained by comparing the
#' \emph{t}-statistic to the standard normal distribution, which is appropriate
#' for models with a large number of degrees of freedom.
#'
#' @param model \code{lmer} model to be summarized.
#' @param confidence desired level of confidence for the confidence interval.
#' @return model summary including confidence intervals.
#' @references Baayen, R.H. (2008). \emph{Analyzing linguistic data: A practical
#' introduction to statistics}. Cambridge, UK: Cambridge University Press.
#' @examples
#" data(InstEval, package='lme4')
#' model1 <- lmer(y ~ service + (1|s) + (1|d), data=InstEval)
#' summaryCI(model1)
#' @export

summaryCI <- function(model, confidence=.95) {

	# First, check that this was a GAUSSIAN model:
	if (family(model)$family != "gaussian") {
		stop('Model was not fit using a Gaussian / normal distribution.')
	}
	
	# Change percents into proportions if needed:
	if (confidence > 1) {
		confidence = confidence/100
	}
	
	# Get the existing model summary:
	model.summary <- summary(model)
	coefs <- model.summary$coefficients
	
	# Make it two-tailed:
	p <- 1-((1-confidence)/2)
	# Get the quantile
	quantile <- qnorm(p)
		
	# Calculate confidence interval:
	ci.lower <- coefs[,1] - (quantile * coefs[,2])
	ci.upper <- coefs[,1] + (quantile * coefs[,2])
	                     
	# Add these to the coefficients table:
	model.summary$coefficients <- cbind('Estimate'=coefs[,'Estimate'],
	                                    'Std. Error'=coefs[,'Std. Error'],
	                                    't value'=coefs[,'t value'],
	                                    'Lower CI'=ci.lower,
	                                    'Upper CI'=ci.upper)
		
	# Display:
	model.summary
}