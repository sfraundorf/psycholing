#' @title Center Numeric Variable Within Each of Several Cells
#'
#' @description
#' Centers the elements of a numeric vector around the means of subsets defined
#' by one or more grouping variables.  For instance, center Likert scale ratings
#' within each participant, or center student achievement scores around the
#' mean within a particular school rather than the overall mean.
#' @param x a numeric vector
#' @param INDEX list of one or more grouping variables, typically factors, each 
#' of the same length as x
#' @return numeric vector with \code{x} centered separately within each level
#' of \code{INDEX}
#' @seealso \code{\link{zscore.by.group}} to center within groups \emph{and}
#' scale by the standard deviation of each group.
#' @examples
#' my.data <- data.frame(Subject=c(rep('S1',6), rep('S2',6)),
#'                       Session=c(rep('Day1',3), rep('Day2',3),
#'                                 rep('Day1',3), rep('Day2',3)),
#'                       Rating=c(10,30,20,20,40,30,15,35,25,25,45,35))
#' # center within each subject:
#' my.data$Rating.c <- center.by.group(my.data$Rating, my.data$Subject)
#' # center within each subject x session pairing:
#' my.data$Rating.c <- center.by.group(my.data$Rating, list(my.data$Subject,
#' my.data$Session))
#' @export

center.by.group <- function(x,INDEX) {
	
	(x - ave(x, INDEX, FUN=mean))
	
}