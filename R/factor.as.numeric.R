#' @title Coerce Factor Labels to Numeric
#'
#' @description
#' Coerces a factor to a numeric vector based on the labels for the factor 
#' levels (rather than the integer codes assigned to them); e.g., factor level
#' "3.5" becomes a value of 3.5.  Labels that cannot be coerced to numeric are
#' treated as NA.
#' @param x factor variable whose labels should be coerced to numeric.
#' @return numeric vector.
#' @examples
#' my.factor <- as.factor(c(1,2.5,10))
#' factor.as.numeric(my.factor)
#' # compare to:
#' as.numeric(my.factor)
#' @export

factor.as.numeric <- function(x) {
		
	#return(as.numeric(as.character(x)))
	as.numeric(levels(x))[x]
	
}