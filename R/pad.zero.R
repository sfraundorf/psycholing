#' @title Pad String With Leading Zeros
#'
#' @description
#' Pads each element of a numeric vector, or a vector that can be coerced to
#' numeric, so that it is a string of a minimum \code{width} by adding leading
#' 0s.
#'
#' @details
#' Non-integers are truncated towards 0.
#'
#' For negative integers, the negative sign is counted towards the string
#' length.
#'
#' Integers that are already as long or longer than \code{width} are unaffected. 
#'
#' @param x a numeric vector or vector that can be coerced to numeric.
#' @param width minimum width desired for each string.
#' @return character vector of digit strings in which 0s have been prepended to
#' each string to make it at least \code{width} characters long.
#' @examples	
#' pad.zero(c('3','80','155'), width=2)
#' pad.zero(20, width=3)
#' @export

pad.zero <- function(x, width) {

	# bad with floating points
	tol <- .Machine$double.eps^0.5
	if(any(sapply(as.numeric(x), function(y) abs(y - round(y)) > tol))) {
		warning('non-integer values truncated towards 0')
	}

	formatC(as.numeric(x), width=width, format='d', flag='0')
}