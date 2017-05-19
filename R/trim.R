#' @title Trim Values Based on Mean and Standard Deviation
#'
#' @description
#' Identifies values of x that are a certain number of standard deviations from
#' the mean and treats them as missing (\code{NA}) values (e.g., values more
#' than 3 standard deviations from the mean are considered missing) so that they
#' can be easily deleted.
#'
#' @details
#' This function is provided because this is a standard data processing
#' procedure in the psycholinguistic literature and not as an endorsement or
#' rejection of this or any other procedure.
#'
#' @param x vector of values to which the trimming procedure should be applied.
#' @param sds number of standard deviations away from the mean at which values
#' should be considered missing.
#' @param print a logical value indicating whether to display the number and
#' percentage of extreme values.
#' @param na.rm a logical value indicating whether \emph{existing} NA values
#' should be ignored when computing the mean and standard deviation.
#' @return vector in which values more than the specified number of standard
#' deviations from the mean have been replaced with \code{NA}.
#' @seealso \code{\link{flag.by.sds}} to identify such values without deleting
#' them.
#' @seealso \code{\link{fence}} to replace such observations with the boundary
#' value.
#' @seealso \code{\link{trim.by.group}} to trim values a certain number of
#' standard deviations away from a group mean (e.g., the mean within a condition
#' or the mean for a research participant) rather than the overall mean.
#'
#' @examples
#' my.data <- data.frame(RT=rnorm(300, mean=789, sd=150))
#' my.data$TrimmedRT <- trim(my.data$RT, sds=2.5)
#' @export
#' @importFrom stats sd na.omit

trim <- function(x,sds=3,print=TRUE,na.rm=FALSE) {

	upperlimit <- mean(x, na.rm=na.rm) + (sd(x, na.rm=na.rm) * sds)
	lowerlimit <- mean(x, na.rm=na.rm) - (sd(x, na.rm=na.rm) * sds)

	y <- ifelse(x>upperlimit, NA, ifelse(x<lowerlimit, NA, x))

	if (print) {
		if (na.rm & any(is.na(x))) {
			# Display percentages w/ and w/o NAs
			number.trimmed <- sum(is.na(y)) - sum(is.na(x))
			print(paste('# trimmed: ', number.trimmed,
			  ' (', round(100*(number.trimmed/length(na.omit(x))),2),
			  '% non-NA observations)',
			  ' (', round(100*(number.trimmed/length(y)),2),
			  '% all observations)',
			  sep=''))
		} else if (!na.rm & any(is.na(x))) {
			print('# trimmed: NA (NA%)')
		} else {
			number.trimmed <- sum(is.na(y))
			print(paste('# trimmed: ', number.trimmed,
			  ' (', round(100*number.trimmed/length(y),2), '%)', sep=''))
		}
	}

	y

}
