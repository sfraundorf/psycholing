#' @title Identify Extreme Values Based on Mean and Standard Deviation
#'
#' @description
#' Identifies which elements of x are a certain number of standard deviations
#' from the mean (e.g., values more than 3 standard deviations from the mean).
#'
#' @param x vector of values.
#' @param sds number of standard deviations away from the mean at which
#' "extreme" values should be identified.
#' @param print a logical value indicating whether to display the number and
#' percentage of identified elements.
#' @param na.rm a logical value indicating whether NA values should be ignored
#' when computing the mean and standard deviation.
#' @return logical vector indicating whether each element of x is more than
#' the specificed number of deviations away from the mean.
#' @seealso \code{\link{trim}} to treat such elements as missing data.
#' @seealso \code{\link{fence}} to \emph{replace} such elements with the
#' threshold value.
#' @seealso \code{\link{flag.by.group}} to identify values a certain number of
#' standard deviations away from a group mean (e.g., the mean within a condition
#' or the mean for a research participant) rather than the overall mean.
#'
#' @examples
#' my.data <- data.frame(RT=rnorm(300, mean=789, sd=150))
#' my.data$OutlyingRT <- flag.by.sds(my.data$RT, sds=2)
#' @export
#' @importFrom stats sd na.omit

flag.by.sds <- function(x, sds=3, print=TRUE, na.rm=FALSE) {

	upperlimit <- mean(x, na.rm=na.rm) + (sd(x, na.rm=na.rm) * sds)
	lowerlimit <- mean(x, na.rm=na.rm) - (sd(x, na.rm=na.rm) * sds)

	y <- ifelse(x>upperlimit, TRUE, ifelse(x<lowerlimit, TRUE, FALSE))

	if (print) {
		if (na.rm & any(is.na(y))) {
			# Display percentages w/ and w/o NAs
			print(paste('# flagged: ', sum(y, na.rm=na.rm),
			  ' (', round(100*(sum(y, na.rm=na.rm)/length(na.omit(y))),2),
			  '% non-NA observations)',
			  ' (', round(100*(sum(y, na.rm=na.rm)/length(y)),2),
			  '% all observations)',
			  sep=''))
		} else {
			print(paste('# flagged: ', sum(y),
			  ' (', round(100*(sum(y)/length(y)),2), '%)', sep=''))
		}
	}

	y

}
