#' @title Linear Mixed Effect Model with Maximal Random Effects Structure
#'
#' @description
#' Determine the call for an \code{lmer} model with the maximal random effects
#' structure for a prototypical psycholinguistic design with a factorial
#' experimental design and with subjects and items as (crossed or partially
#' crossed) random effects.
#'
#' @details
#' This function is only applicable to the set of designs in which there are
#' exactly two sampling units (subjects and items) and all of the variables are
#' factorially crossed. Although such designs are common in psycholinguistics,
#' many other designs are certainly possible and are also valid applications of
#' linear mixed effects models (merely outside the purview of this function).
#'
#' In addition, the maximal random effects structure may or may not be
#' appropriate for the particular dataset or analytic question (e.g., the model
#' may be overparameterized). This function is provided simply to help beginning
#' users understand how the model call relates to the experimental design and as
#' a shortcut for when a maximal random effects structure is known to be
#' desired.
#' @param data a data frame containing the data to be fit with the model.
#' @param outcome the name of the column containing the outcome variable (i.e.,
#' the dependent variable in an experimental study).
#' @param subjects the name of the column containing the subject names or
#' subject numbers.
#' @param items the name of the column containing the item names or item numbers 
#' @param ivs the name(s) of the columns containing the independent or predictor
#' variables (excluding the subjects and items).
#' @param within.subjects the names of the IV columns, if any, that are
#' within-subjects variables (i.e., each subject sees more than one level of
#' this variable).
#' @param within.items the names of the IV columns, if any, that are within-item
#' variables (i.e., each subject sees more than one level of this variable).
#' @param fit.model logical - should the model actually be fit, or should the
#' function merely return a string describing the model call?
#' @param ... additional arguments to \code{\link{lmer}}.
#' @return either a model of class \code{\link{merMod}} or a string containing
#' an \code{lmer} function call that could be used to fit that model.
#' @seealso \code{\link{maximal.glmer}} for \emph{generalized} linear mixed
#' effects models.
#' @keywords models
#' @references Barr, D.J., Levy, R., Scheepers, C., & Tily, H.J. (2013). Random
#' effects structure for confirmatory hypothesis testing: Keep it maximal.
#' \emph{Journal of Memory and Language}, \emph{68}, 255-278.
#' @examples
#' maximal.lmer(data=my.dataframe, outcome='RT',
#'              subjects='Subject', items='Items',
#'              ivs=c('SentenceType', 'PrimeType'),
#'              within.subjects=c('SentenceType', 'PrimeType'),
#'              within.items='PrimeType', fit.model=FALSE)
#' @export
#' @importFrom lme4 lmer

maximal.lmer <- function(data, outcome, subjects='Subject', items='Item',
                         ivs=NULL, within.subjects=NULL, within.items=NULL,
                         fit.model=FALSE, ...) {
                         
    # Is there at least one random effect?
    if (is.null(subjects) & is.null(items)) {
    	stop('Must have at least one random effect term!')
    }
    # Are there random slopes but no corresponding ID variable?
    if (is.null(subjects) & length(within.subjects) > 0) {
    	stop(paste('Within-subjects variables specified,',
       	  'but subject ID variable is not!'))
    }
    if (is.null(items) & length(within.items) > 0) {
    	stop(paste('Within-items variables specified,',
       	  'but item ID variable is not!'))
    }    
    # Are there within-subjects orwithin-item variables that aren't in the
    # main list of IVs?
    if (all(within.subjects %in% ivs) == FALSE) {
    	bad.variables <- within.subjects[within.subjects %in% ivs == FALSE]
    	stop(paste0('Variable ', bad.variables[1], ' is specified as a',
    	   ' within-subjects variable but is not included in ivs'))
    }
    if (all(within.items %in% ivs) == FALSE) {
    	bad.variables <- within.items[within.items %in% ivs == FALSE]
    	stop(paste0('Variable ', bad.variables[1], ' is specified as a',
    	   ' within-items variable but is not included in ivs'))
    }    
    	
	# Subject random effects:
	subject.string <- ifelse(is.null(subjects), '',
	   paste('(1', ifelse(length(within.subjects) > 0, ' + ', ''),
	   paste(within.subjects, collapse= ' * '), '|', subjects, ')', sep=''))

    # Item random effects:
	item.string <- ifelse(is.null(items), '',
	   paste('(1', ifelse(length(within.items) > 0, ' + ', ''),
	   paste(within.items, collapse= ' * '), '|', items, ')', sep=''))
	
	# Make into a formula:
	formula.string <- paste0(outcome, ' ~ 1 +', paste(ivs, collapse=' * '),
	   ifelse(length(ivs)>0, ' + ', ''), subject.string,
	   ifelse(is.null(subjects)==FALSE & is.null(items)==FALSE,' + ', ''),
	   item.string)
	   
	# Optional arguments:
	optional.arg.names <- names(list(...))
	optional.args <- as.list(substitute(list(...)))[-1L]
    optional.arg.string <- paste0(
       optional.arg.names, '=', optional.args, collapse=', ')
	
	# Get the full string of the model call:
	full.string <- paste0('lmer(', formula.string,
	   ', data=', substitute(data), 
	   ifelse(length(optional.args) > 0,
	      paste0(', ', optional.arg.string), ''),
	   ')')
	
	if (fit.model) {
		# Actually fit the model:
		eval(parse(text=full.string))
	} else {
		# Just display a string:
		print(full.string, quote=FALSE)
	}
	
}