#' @title z-Score Within Each of Several Cells
#'
#' @description
#' z-scores \code{x} within each of one or more cells.  For instance, z-score
#' Likert scale ratings within each participant, or z-score response times
#' within each participant x condition pairing.
#' @param x a numeric vector
#' @param INDEX list of one or more grouping variables, typically factors, each
#' of the same length as x
#' @return numeric vector with \code{x} z-scored separately within each level
#' of \code{INDEX}
#' @seealso \code{\link{center.by.group}} to center within groups without
#' scaling by the standard deviation.
#' @examples
#' my.data <- data.frame(Subject=c(rep('S1',6), rep('S2',6)),
#'                       Session=c(rep('Day1',3), rep('Day2',3),
#'                                 rep('Day1',3), rep('Day2',3)),
#'                       Rating=c(10,30,20,20,40,30,15,35,25,25,45,35))
#' # z-score within each subject:
#' my.data$Rating.Subject.z <- zscore.by.group(my.data$Rating, my.data$Subject)
#' # z-score within each subject x session pairing:
#' my.data$Rating.SubjectSession.z <- zscore.by.group(my.data$Rating,
#' list(my.data$Subject, my.data$Session))
#' @export
#' @importFrom stats ave sd

zscore.by.group <- function(x,INDEX) {

	(x - ave(x, INDEX, FUN=mean))/ave(x, INDEX, FUN=sd)

}
