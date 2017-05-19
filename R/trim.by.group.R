#' @title Trim Values Based on Group Means and Standard Deviations
#'
#' @description
#' Identifies values of x that are a certain number of standard deviations from
#' the mean within a particular set of grouping variables and treats them as
#' missing (\code{NA}) values so that they can be easily deleted (e.g., delete
#' values more than 3 standard deviations from the).  For example, trim response
#' times that are more than 2.5 standard deviations from the mean within each
#' cell of a PrimeType x WordFrequency factorial experimental design.
#'
#' @param x a numeric vector.
#' @param INDEX list of one or more grouping variables, typically factors, each
#' of the same length as \code{x}.
#' @param sds number of standard deviations away from the mean at which
#' values should be trimmed.
#' @param print a logical value indicating whether to display the number and
#' percentage of trimmed elements.
#' @param na.rm a logical value indicating whether \emph{existing} NA values
#' should be ignored when computing the mean and standard deviation.
#' @return vector in which values more than the specified number of standard
#' deviations from each cell mean have been replaced with \code{NA}.
#' @seealso \code{\link{flag.by.group}} to identify such observations without
#' changing them.
#' @seealso \code{\link{fence.by.group}} to replace such observations with the
#' boundary value.
#' @seealso \code{\link{trim}} to trim values a certain number of standard
#' deviations away from the overall mean rather than a group mean.
#' @examples
#' data(sleepstudy, package='lme4')
#' sleepstudy$Reaction.Trimmed <- trim.by.group(sleepstudy$Reaction,
#'   sleepstudy$Subject, sds=3) # trim RTs 3 std devs from subject mean
#'
#' data(VerbAgg, package='lme4')
#' VerbAgg$Anger.Trimmed <- trim.by.group(VerbAgg$Anger,
#'   list(VerbAgg$btype, VerbAgg$situ), sds=3)
#' # trim 3 std devs from behavior type x situation type cell mean
#' @export
#' @importFrom stats sd na.omit

trim.by.group <- function(x, INDEX, sds=3, print=TRUE, na.rm=FALSE) {

	y <- ifelse(x > (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) +
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            NA,
         ifelse(x < (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) -
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            NA,
	     x))

	if (print) {
		if (na.rm & any (is.na(x))) {
			# Display percentages w/ and w/o NAs
			number.trimmed <- sum(is.na(y)) - sum(is.na(x))
			print(paste('# trimmed: ', number.trimmed,
			  ' (', round(100*(number.trimmed/length(na.omit(x))),2),
			  '% non-NA observations)',
			  ' (', round(100*(number.trimmed/length(y)),2),
			  '% all observations)',
			  sep=''))
		} else if (any(is.na(x))) {
			print('# trimmed: NA (NA%)')
		} else {
			number.trimmed <- sum(is.na(y))
			print(paste('# trimmed: ', number.trimmed,
			  ' (', round(100 *(number.trimmed / length(y)), 2), '%)', sep=''))
		}
	}

	y

}
