#' @title Fence Values Based on Group Means and Standard Deviations
#'
#' @description
#' Identifies values of x that are a certain number of standard deviations from
#' the mean within a particular set of grouping variables and replaces them with
#' that boundary value (e.g., values more than 3 standard deviations from the
#' mean are replaced with the mean +/= 3 standard deviations).  For example,
#' fence response times that are more than 2.5 standard deviations from the mean
#' within each cell of a PrimeType x WordFrequency factorial experimental
#' design.
#'
#' @param x a numeric vector.
#' @param INDEX list of one or more grouping variables, typically factors, each
#' of the same length as \code{x}.
#' @param sds number of standard deviations away from the mean at which
#' values should be replaced.
#' @param print a logical value indicating whether to display the number and
#' percentage of replaced elements.
#' @param na.rm a logical value indicating whether NA values should be ignored
#' when computing the mean and standard deviation.
#' @return vector in which values more than the specified number of standard
#' deviations from each cell mean have been replaced with the boundary value.
#' @seealso \code{\link{flag.by.group}} to identify such observations without
#' replacing them.
#' @seealso \code{\link{trim.by.group}} to treat such observations as missing
#' data rather than replacing them with a new value.
#' @seealso \code{\link{fence}} to fence values a certain number of standard
#' deviations away from the overall mean rather than a group mean.
#' @examples
#' data(sleepstudy, package='lme4')
#' sleepstudy$Reaction.Fenced <- fence.by.group(sleepstudy$Reaction,
#'   sleepstudy$Subject, sds=3) # fence RTs 3 std devs from subject mean
#'
#' data(VerbAgg, package='lme4')
#' VerbAgg$Anger.Fenced <- fence.by.group(VerbAgg$Anger,
#'   list(VerbAgg$btype, VerbAgg$situ), sds=3)
#' # fence 3 std devs from behavior type x situation type cell mean
#' @export

fence.by.group <- function(x,INDEX,sds=3, print=TRUE, na.rm=FALSE) {

	y <- ifelse(x > (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) +
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) +
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX)),
         ifelse(x < (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) -
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) -
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX)),
	     x))

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
