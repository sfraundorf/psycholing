#' @title Compare Multiple Mixed-Effects Models on AIC and BIC
#'
#' @description
#' Compares a set of mixed-effects models on their AIC and BIC values,
#' highlighting the best model according to each of these criteria.
#'
#' @details
#' Log likelihood and deviance are \emph{not} reported because likelihood-ratio
#' tests are only applicable for NESTED models, and the models may or may not
#' be nested.
#'
#' @param listofmodels a list of \code{merMod} objects.
#' @param show.formulae a logical value whether the model formula should be
#' displayed for each model.
#' @param summarize.aic a logical value indicating whether the model summary
#' should be displayed for the model that is the best-fitting according to
#' AIC.
#' @param summarize.bic a logical value indicating whether the model summary
#' should be displayed for the model that is the best-fitting according to
#' BIC.  Note that if \code{summarize.aic} and \code{summarize.bic} are both
#' \code{TRUE} but the same model wins according to both criteria, only one
#' summary will be displayed.
#' @export

lmer.compare <- function(listofmodels,show.formulae=TRUE,
                         summarize.aic=FALSE, summarize.bic=FALSE) {
	
	# get the list of model names
	modelnames <- substitute(listofmodels)
	modellabels <- as.character(modelnames)[2:length(modelnames)]

	# get # of models	
	nummodels <- length(listofmodels)
	
	# check to make sure all models are fit to the same dataset..
	datasets = sapply(listofmodels, function(x) x@call['data'])
	if (length(unique(datasets)) > 1) {
		stop('Models not all fit to the same data!')
	}
	
	# ...and to the same DV
	DVs = sapply(listofmodels,
	  function(x) strsplit(as.character(x@call['formula']), '~')[[1]][1])
	if (length(unique(DVs)) > 1) {
		stop('Models do not all have the same dependent variable!')
	}
	
	# get the AIC & BIC of each model
	AICs <- sapply(listofmodels, function(x) summary(x)$AICtab['AIC'])
	BICs <- sapply(listofmodels, function(x) summary(x)$AICtab['BIC'])
	
	# find the BEST (smallest) AIC & BIC
	bestAIC <- min(AICs)
	bestBIC <- min(BICs)
	
	# find the LONGEST model name, counting the 'MODEL' header as a label
	longestlabel <- max(c(nchar(modellabels),5))
	
	# get the formulae of each model, if needed
	formulae <- sapply(listofmodels, function(x) as.character(x@call['formula']))
	
	# print the header
	extraspaces <- max(5,longestlabel) - nchar('MODEL')
	modellabel <- paste('MODEL', paste(rep(' ',extraspaces),collapse=''), sep = '')
	if (show.formulae) {
		cat(sprintf('%s  AIC         BIC         FORMULA\n',modellabel))
	} else {
		cat(sprintf('%s  AIC         BIC\n',modellabel))
	}
	
	# print individual rows
	for (i in 1:nummodels) {
		
		# extra spaces needed
		extraspaces = longestlabel - nchar(modellabels[i])
		modellabel = paste(modellabels[i], paste(rep(' ',extraspaces),collapse=''), sep = '')
				
		# is this best AIC?
		AICcode = ifelse(AICs[i]==bestAIC, '*', ' ')
		# is this best BIC?
		BICcode = ifelse(BICs[i]==bestBIC, '*', ' ')
		
		if (show.formulae==TRUE) {
			cat(sprintf('%s  %4.3f %s  %4.3f %s  %s\n',
			   modellabel, AICs[i], AICcode, BICs[i], BICcode,formulae[i]))
		} else {
			cat(sprintf('%s  %4.3f %s  %4.3f %s\n',
			   modellabel, AICs[i], AICcode, BICs[i], BICcode))
		}
	}
	
	# display summary(s) if requested	
	aic.winners <- which(AICs==bestAIC)  # could be a tie
	bic.winners <- which(BICs==bestBIC)  # could be a tie
	if (summarize.aic) {
		for (model in aic.winners) {
			cat(sprintf('\nModel %s:\n', modellabels[model]))
			print(summary(listofmodels[model][[1]]))
		}
	}
	if (summarize.bic) {		
		if (!summarize.aic | !identical(aic.winners, bic.winners)) {
			# only do BIC summary if not redundant with an AIC summary
			for (model in bic.winners) {
				cat(sprintf('\nModel %s:\n', modellabels[model]))
				print(summary(listofmodels[model][[1]]))
			}
		}
	}		
	
}