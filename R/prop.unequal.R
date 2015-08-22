#' @title Proportion of unequal elements
#'
#' @description
#' Compares the first element in vector \code{x} to the first element in vector
#' \code{y}, the second element in \code{x} to the second element in \code{y},
#' and so forth, and returns the proportion of elements which are not equal.
#'
#' @param x First vector to be compared
#' @param y Second vector to be compared
#' @param na.rm a logical value indicating positions should be excluded if they
#' are \code{NA} in at least one of the vectors.
#' @return numeric value that is proportion of corresponding elements that are
#' not equal. 
#' @examples
#' prop.unequal(c(1,2,3), c(1,3,3))
#' prop.unequal(c(1,2,3), c(1,3,NA))
#' prop.unequal(c(1,2,3), c(1,3,NA), na.rm=TRUE)
#' @export

prop.unequal <- function(x,y, na.rm=FALSE) {
	
	if (length(x) != length(y)) {
		stop('Vectors must be the same length')
	}
	
	if (na.rm) {
		comparable <- is.na(x) == FALSE & is.na(y) == FALSE
		sum(x[comparable] != y[comparable]) / length(x[comparable])
	} else {
		sum(x != y) / length(x)
	}
	
}