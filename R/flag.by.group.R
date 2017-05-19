#' @title Identify Extreme Values Based on Group Means and Standard Deviations
#'
#' @description
#' Identifies, within subsets defined by the combination one or more grouping
#' variables, which elements of x are a certain number of standard deviations
#' from the mean of that group (e.g., values more than 3 standard deviations
#' from the mean). For example, identify GRE scores more than 3 standard
#' deviations from the mean within each university (rather than the overall
#' mean), or identify response times more than 2.5 standard deviations from the
#' mean within a particular subject x condition cell in an experimental design.
#'
#' @param x a numeric vector.
#' @param INDEX list of one or more grouping variables, typically factors, each
#' of the same length as \code{x}.
#' @param sds number of standard deviations away from the mean at which
#' "extreme" values should be identified.
#' @param print a logical value indicating whether to display the number and
#' percentage of identified elements.
#' @param na.rm a logical value indicating whether NA values should be ignored
#' when computing the mean and standard deviation.
#' @return logical vector indicating whether each element of x is more than
#' the specificed number of deviations away from the corresponding group mean.
#' @seealso \code{\link{fence.by.group}} to \emph{replace} such elements with
#' the boundary value.
#' @seealso \code{\link{trim.by.group}} to treat such observations as missing
#' data.
#' @seealso \code{\link{flag.by.sds}} to identify values a certain number of
#' standard deviations away from the overall mean.
#' @examples
#' data(sleepstudy, package='lme4')
#' sleepstudy$Possible.Outlier <- flag.by.group(sleepstudy$Reaction,
#'   sleepstudy$Subject, sds=3) # RTs 3 std devs from subject mean
#'
#' data(VerbAgg, package='lme4')
#' VerbAgg$Possible.Outlier <- flag.by.group(VerbAgg$Anger,
#'   list(VerbAgg$btype, VerbAgg$situ), sds=3)
#' # 3 std devs from behavior type x situation type cell mean
#' @export
#' @importFrom stats sd na.omit

flag.by.group <- function(x, INDEX, sds=3, print=TRUE, na.rm=FALSE) {

	y <- ifelse(x > (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) +
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            TRUE,
         ifelse(x < (unsplit(lapply(split(x,INDEX), mean, na.rm=na.rm), INDEX) -
	                sds * (unsplit(lapply(split(x,INDEX), sd, na.rm=na.rm), INDEX))),
	            TRUE,
	     FALSE))

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
