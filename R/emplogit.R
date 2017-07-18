#' @title Empirical logit
#'
#' @description
#' Calculates the empirical logit for binomial data; i.e., data that consists of
#' "hits" (1s) versus "misses" (0s).  The formula for the empirical logit
#' requires specification of the sample size; sample size may be provided to
#' this function in any one of three ways:
#' \itemize{
#' \item the raw data (which implicitly indicate sample size)
#' \item the number of hits and total number of observations
#' \item the proportion of hits and total number of observations
#' }
#' Simply providing a proportion of hits or count of hits is insufficient to
#' determine the empirical logit.
#'
#' @details
#' The empirical logit is an approximation of the logit that is useful when the
#' proportion of hits is 0 or 1 (for which the logit is undefined) or when the
#' proportion of hits is very close to 0 or 1 (for which the logit is unstable),
#' as is sometimes the case with \emph{empirical} data.
#'
#' As the sample size increases, the empirical logit converges to the logit.
#'
#' The empirical logit requires aggregating over multiple observations.  Thus,
#' in general, it may be less advantageous than logistic regression, which
#' allows the modeling of individual observations using a logit link function.
#' However, using the empirical logit is necessary or advisable when the
#' proportion of hits closely approaches (or is) 0 or 1.
#'
#' Because the empirical logit aggregates over multiple observations, once the
#' empirical logit has been calculated, the original number of observations is
#' not reflected in the empirical logit itself and is "lost."  Thus, when using
#' the empirical logit in an analysis where the number of observations differs
#' across cells, it is suggested to incorporate the number of observations back
#' into the model in another way--namely, by performing a weighted regression.
#' For more information, see the provided references (esp., Barr, 2008, p. 470).
#'
#' @param hits frequency count of observed "hits" or 1s.
#' @param n total number of observations.
#' @param proportion proportion of observed "hits" or 1s.
#' @param rawdata a vector of binomial data (hits and misses) for which the
#' empirical logit should be calculated.  Raw data may be provided in any of
#' three forms:
#' \itemize{
#' \item a numeric vector (0 for miss, 1 for hit)
#' \item a logical vector (FALSE for miss, TRUE for hit)
#' \item a factor vector (first level of the factor is a "miss" and all other
#' levels a "hit", following the behavior of \code{\link{family}})
#' }
#' @param na.rm a logical value indicating whether NA values should be stripped
#' form the raw data before the computation proceeds.  Only relevant when
#' providing the raw data.
#' @return value of the empirical logit.
#' @seealso \code{\link{logit}} for the unadjusted logit.
#' @references Agresti, A. (2002). \emph{Categorical data analysis} (2nd ed.).
#' Hoboken, NJ: Wiley.
#' @references Barr, D.J. (2008). Analyzing 'visual world' eyetracking data
#' using multilevel logistic regression. \emph{Journal of Memory and Language},
#' \emph{59}, 457-474.
#' @references McCullagh, P., & Nelder, J. (1989). \emph{Generalized linear
#' models}. London: Chapman and Hall.
#' @examples
#' emplogit(proportion=.71, n=100)
#'
#' my.hits = 50
#' emplogit(hits=my.hits, n=50)
#'
#' my.emplogit <- emplogit(rawdata=as.factor(c('no','no','yes','no')))
#' my.emplogit
#' @export
#' @importFrom stats na.omit

emplogit <- function(hits,n,proportion,rawdata, na.rm=FALSE) {

	# Check to make sure one and only one method of calculating the empirical
	# logit is being used
	providedarguments <- c(!missing('hits'),!missing('proportion'),
	  !missing('rawdata'))
	if (length(which(providedarguments == TRUE)) > 1) {
		stop(
		'Provide only ONE of raw data, number of hits, or proportion of hits')
	}

	if (!missing('hits')) {
		if (any(hits > n)) {
			warning('Number of hits greater than total number of observations!')
		} else if (any(hits < 0)) {
			warning('Number of hits must be >= 0')
		}
	}

	if (!missing('rawdata')) {
		# Calculate empirical logit from the raw data
		# Get the sample size from the raw data
		if (na.rm) {
			n <- length(na.omit(rawdata))
		} else {
			n <- length(rawdata)
		}
		# Get the number of hits from the raw data
		if (is.numeric(rawdata)) {
			# NUMERIC raw data
			# make sure all values are valid
			if (na.rm) {
				rawdata <- na.omit(rawdata)
			}
			if (any(na.omit(rawdata) != 1 & na.omit(rawdata) != 0)) {
				stop('Values must be 0 or 1')
			}
			# compute number of hits
			hits = sum(rawdata, na.rm=na.rm)
		} else if (is.factor(rawdata)) {
			# FACTOR raw data
			if (na.rm) {
				hits = sum(na.omit(rawdata) != levels(rawdata)[1])
			} else {
				hits = sum(rawdata != levels(rawdata)[1])
			}
		} else if (is.logical(rawdata)) {
			# LOGICAL raw data
			if (na.rm) {
				hits = sum(na.omit(rawdata))
			} else {
				hits = sum(rawdata)
			}
		} else {
			stop('Raw data must be numeric, factor, or logical')
		}
	}

	if (!missing('proportion')) {
		if (any(proportion > 1) | any(proportion < 0)) {
			warning('Proportions must be 0 <= y <= 1')
		}
		hits = n * proportion
	}

	log((hits+.5)/(n-hits+.5))
}

