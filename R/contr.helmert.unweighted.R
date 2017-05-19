#' @title Unweighted Helmert Contrast Matrices
#'
#' @description
#' Returns a matrix of Helmert contrasts, scaled so that the resulting contrast
#' estimates (in an ANOVA or regression model) correspond to the difference
#' between the levels (categories) being compared. The contrasts may be computed
#' either based on a numerical number of levels or a vector of data.
#'
#' @details
#' Helmert contrasts compare the second level with the first, the third with the
#' average of the first two, and so on. As with other contrasts, they are
#' orthogonal to each other and to the intercept.
#'
#' When the levels differ in frequency, \emph{unweighted} coding is appropriate
#' if the differences in frequency in the sample are merely \emph{incidental}
#' (e.g., both conditions were intended to be presented equally frequently, but
#' by chance there are more observations from one condition than another).  (If
#' the differences in frequency instead represent genuine differences in the
#' population, weighted coding may be more appropriate.)
#'
#' If all of the factor levels are equally common, there is no difference
#' between unweighted and weighted coding.
#' @param x a factor variable (or variable that can be coerced to a factor) for
#' which contrasts should be calculated.
#' @param reference.levels vector specifying, in order, the category treated as
#' the reference level (i.e., assigned the next negative value) in each
#' successive contrast.
#' @param n a vector of levels for a factor, or the number of levels, which can
#' be provided instead of \code{x}.
#' @return A matrix with \code{n} rows and \code{k} columns, where \code{n} is
#' the number of levels and \code{k=n-1}.
#' @references
#' Cohen, J., Cohen, P., West, S.G., & Aiken, L.S. (2002). Categorical or
#' nominal independent variables. In \emph{Applied multiple regression/
#' correlation analysis for the behavioral sciences} (3rd ed., pp. 302-353).
#' Mahwah, NJ: Lawrence Erlbaum Associates.
#' @seealso \code{\link{contr.helmert.weighted}} for \emph{weighted} Helmert
#' contrasts, and \code{\link{contrasts}} and \code{\link{contr.helmert}}.
#' @examples
#' contr.helmert.unweighted(n=4)
#'
#' contr.helmert.unweighted(n=c('Active','Passive'), reference.levels=1)
#'
#' cuedata <- as.factor(c('ValidCue', 'ValidCue',
#'    'InvalidCue', 'NoCue', 'InvalidCue', 'NoCue'))
#' contr.helmert.unweighted(x=cuedata)
#' contr.helmert.unweighted(x=cuedata,
#'                          reference.levels=c('ValidCue','InvalidCue'))
#' @export
#' @importFrom stats contr.helmert

contr.helmert.unweighted <- function(x,reference.levels=levels[-length(levels)],
                                 n=NULL) {

	# Check to make sure that both a factor and a number of levels have not
	# BOTH been specified:
	if (!missing(x) & !missing(n)) {
		stop('Provide vector of data OR number of levels, not both')
	}

	# Work with a vector of data:
	if (missing(n)) {

		# Coerce to a factor if needed:
		if (is.factor(x) == FALSE) {
			warning(paste0('Coerced to a factor from ',class(x)))
			x <- as.factor(x)
		}

		# Get levels:
		n <- levels(x)
	}

	# Get levels:
	if (is.numeric(n)) {
		levels <- 1:n
	} else {
		levels <- levels(factor(n))
	}
	# Get number of levels:
	numlevels <- length(levels)
	# Get number of contrasts:
	k <- numlevels-1

	# Check to make sure the right number of reference levels were specified
	if (length(reference.levels) != k) {
		stop(paste('Wrong number of reference levels!',
		  k, 'contrast(s) needed, but', length(reference.levels),
		  'reference level(s) specified', sep= ' '))
	}

	# Make sure none of the reference levels is out of bounds
	if (is.numeric(reference.levels)) {
		if (any(reference.levels < 0) | any(reference.levels > numlevels) |
		   any((reference.levels - round(reference.levels)) > .0001)) {
			stop(paste0('Level indices must be integers 1 <= x <= ', numlevels))
		}
	}

	# Convert the reference levels from characters to numbers, if needed:
	if (is.character(reference.levels)) {
		newreflevels <- sapply(reference.levels,
		   function(y) which(levels==y)[1])
		if (any(is.na(newreflevels))) {
			# User named a level that's not actually a level of the factor
			badlevels <- newreflevels[is.na(newreflevels)]
			stop(paste0('Factor does not have a level named "',
				     names(badlevels[1]), '"'))
		}
		reference.levels <- newreflevels
	}

	# Get default helmert contrasts:
	orig.matrix <- contr.helmert(numlevels)

	# Reorder the matrix according to the desired contrasts:
	# Step 1 - get the correct ordering of contrasts
	contrast.order <- c(reference.levels,
	   which(c(1:numlevels) %in% reference.levels==FALSE)[1])
	# Step 2 - name the contrast matrix accordingly
	rownames(orig.matrix) <- levels[contrast.order]
	# Step 3 - reorder matrix to match the original ordering of factor levels
	sorted.matrix <- orig.matrix[levels,,drop=FALSE]

	# Rescale and return:
	apply(sorted.matrix, 2, function(x) x/length(x[x != 0]) )

}
