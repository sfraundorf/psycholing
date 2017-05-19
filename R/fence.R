#' @title Fence Values Based on Mean and Standard Deviation
#'
#' @description
#' Identifies values of x that are a certain number of standard deviations from
#' the mean and replaces them with that boundary value (e.g., values more than
#' 3 standard deviations from the mean are replaced with the mean +/= 3 standard
#' deviations).
#'
#' @details
#' This function is provided because this is a standard data processing
#' procedure in the psycholinguistic literature and not as an endorsement or
#' rejection of this or any other procedure.
#'
#' @param x vector of values to which the fencing procedure should be applied.
#' @param sds number of standard deviations away from the mean at which values
#' should be replaced.
#' @param print a logical value indicating whether to display the number and
#' percentage of replaced values.
#' @param na.rm a logical value indicating whether NA values should be ignored
#' when computing the mean and standard deviation.
#' @return vector in which values more than the specified number of standard
#' deviations from the mean have been replaced with the boundary value.
#' @seealso \code{\link{flag.by.sds}} to identify such observations without
#' replacing them.
#' @seealso \code{\link{trim}} to treat such observations as missing data rather
#' than replacing them with a new value.
#' @seealso \code{\link{fence.by.group}} to fence values a certain number of
#' standard deviations away from a group mean (e.g., the mean within a condition
#' or the mean for a research participant) rather than the overall mean.
#'
#' @examples
#' my.data <- data.frame(RT=rnorm(300, mean=789, sd=150))
#' my.data$FencedRT <- fence(my.data$RT, sds=2.5)
#' @export
#' @importFrom stats sd na.omit

fence <- function(x,sds=3,print=TRUE,na.rm=FALSE) {

	upperlimit <- mean(x, na.rm=na.rm) + (sd(x, na.rm=na.rm) * sds)
	lowerlimit <- mean(x, na.rm=na.rm) - (sd(x, na.rm=na.rm) * sds)

	y <- ifelse(x>upperlimit, upperlimit, ifelse(x<lowerlimit, lowerlimit, x))

	if (print) {
		if (na.rm & any(is.na(y))) {
			# Display percentages w/ and w/o NAs
			number.fenced <- sum(na.omit(x) != na.omit(y))
			print(paste('# fenced: ', number.fenced,
			  ' (', round(100*(number.fenced/length(na.omit(y))),2),
			  '% non-NA observations)',
			  ' (', round(100*(number.fenced/length(y)),2),
			  '% all observations)',
			  sep=''))
		} else {
			print(paste('# fenced: ', sum(x != y),
			  ' (', round(100*prop.unequal(x,y),2), '%)', sep=''))
		}
	}

	y

}
