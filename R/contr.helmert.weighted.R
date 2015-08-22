#' @title Weighted Helmert Contrasts for Factor Variables
#'
#' @description
#' Returns a matrix of Helmert contrasts, scaled so that the resulting contrast
#' estimates (in an ANOVA or regression model) correspond to the difference
#' between the levels (categories) being compared, and weighted so that the mean
#' of each contrast is zero.
#'
#' @details
#' Helmert contrasts compare the second level with the first, the third with the
#' average of the first two, and so on. As with other contrasts, they are
#' orthogonal to each other and to the intercept.
#'
#' When the levels differ in frequency, \emph{weighted} coding is appropriate
#' if the differences in frequency in the sample correspond to \emph{actual}
#' differences in the population (e.g., some college majors are more common than
#' others).  (If the differences in frequency instead represent incidental
#' differences in the sample, unweighted coding may be more appropriate.)
#'
#' If all of the factor levels are equally common, there is no difference
#' between weighted and unweighted coding.
#' @param x a factor variable (or variable that can be coerced to a factor) for
#' which mean-centered Helmert contrasts should be calculated.
#' @param reference.levels vector specifying, in order, the category treated as
#' the reference level (i.e., assigned the next negative value) in each
#' successive contrast.
#' @param na.rm logical indicating whether \code{NA} values should be ignored
#' when computing the contrasts.
#' @return A matrix with \code{n} rows and \code{k} columns, where \code{n} is
#' the number of levels and \code{k=n-1}.
#' @references
#' Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2002). Categorical or
#' nominal independent variables. In \emph{Applied multiple regression/
#' correlation analysis for the behavioral sciences} (3rd ed., pp. 302-353).
#' Mahwah, NJ: Lawrence Erlbaum Associates.
#' @seealso \code{\link{center.numeric}} for centering numeric variables and
#' \code{\link{contr.helmert.unweighted}} for unweighted Helmert contrasts.
#' @examples
#' cuedata <- as.factor(c('ValidCue', 'ValidCue',
#'    'InvalidCue', 'NoCue', 'InvalidCue', 'NoCue', 'InvalidCue'))
#' contr.helmert.weighted(cuedata, reference.levels=c('ValidCue','InvalidCue'))
#' @export

contr.helmert.weighted <- function(x,
           reference.levels=levels(as.factor(x))[-length(levels(as.factor(x)))],
           na.rm=FALSE) {
           
    # Coerce to a factor if needed:
	if (is.factor(x) == FALSE) {
		warning(paste0('Coerced to a factor from ',class(x)))
		x <- as.factor(x)
	}
        
	# First, code the factor w/o centering:
	helmert.matrix <- contr.helmert.unweighted(x,
	   reference.levels=reference.levels)    
    
    # Handle NAs:
    if (any(is.na(x)) & !na.rm) {
    	helmert.matrix[,] <- NA
    	return(helmert.matrix)
    } else if (na.rm) {
    	x <- na.omit(x)
    }
    
	# Frequency count of the levels:
	level.freq <- summary(x)
	# Total number of observations:
	total.obs <- sum(summary(x))    
	
	# Center each contrast around its mean:
	apply(helmert.matrix, 2,
	   function(contrast) contrast - (sum(level.freq * contrast/total.obs)))	       
}