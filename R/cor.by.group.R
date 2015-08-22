#' @title Correlation Within Groups
#'
#' @description
#' Computes the correlation between \code{x} and \code{y} separately within each
#' of several groups defined by all of the combinations of the factor variables
#' contained within \code{by}. For example, correlate high school GPA with
#' college GPA separately within each of several universities, or correlate
#' working memory score with response time separately in each cell of a 2 x 2
#' factorial experiment.
#' @param x one of the numeric vectors to be correlated.
#' @param y the other numeric vector to be correlated.
#' @param INDEX list of one or more grouping variables, typically factors, each 
#' of the same length as \code{x}.
#' @param ... arguments to \code{\link{cor}}.
#' @seealso \code{\link{cor}} for correlations without division into groups.
#' @examples
#' data(grouseticks, package='lme4')
#' cor.by.group(grouseticks$HEIGHT, grouseticks$TICKS, INDEX=grouseticks$YEAR)
#' # Correlation of HEIGHTS and TICKS separately for each YEAR
#'
#' data(Arabidopsis, package='lme4')
#' cor.by.group(Arabidopsis$nutrient, Arabidopsis$total.fruits,
#'    INDEX=list(Arabidopsis$reg, Arabidopsis$status))
#' # Correlation of nutrient and total.fruits for each combination of
#' # reg x status
#' @export

cor.by.group <- function(x, y, INDEX, ...) {
	
	mapply(function(x,y) cor(x,y, ...), split(x,INDEX), split(y,INDEX))
	
}
