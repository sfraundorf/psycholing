#' @title Data Input from Multiple Files
#' @aliases read.multiple.table
#' @aliases read.multiple.csv
#'
#' @description
#' Open each of a set of files assumed to represent additional cases /
#' observations on the same set of variables, then combine them into a single
#' data frame containing all of the cases.  For example, if data from 10
#' research participants are in 10 separate files, they can be read and
#' combined into a single data frame.
#' @param uniquenames A vector of string filenames, or if \code{prefix} and/or
#' \code{suffix} are specified, a vector of the portion of the filename that
#' uniquely identifies each filename.
#' @param path Optional string indicating a path ("directory" or "folder") where
#' to be appended to the start of every filename.
#' @param prefix Optional string indicating a string that should be prepended to
#' the start of each filename.
#' @param suffix Optional string indicating a string that should be appended to
#' the end of every filename.
#' @param addfilename a logical value indicating whether a 'Filename' column
#' should be added to the resulting dataframe indicating the filename of each
#' original file.
#' @param paduniquenames a logical value indicating whether each of the elements
#' of \code{uniquenames} should be padded up to the length of the longest
#' element by prepending zeros (e.g., '3' becomes '03').
#' @param ... arguments to \code{\link{read.table}} or \code{\link{read.csv}}.
#' @return Data frame with the content of all of the files combined by rows.
#' @seealso \code{\link{read.table}} to read from a single file.
#' @examples
#' write.csv(data.frame(StudentName=c('Duane','Scott'), GPA=c(3.90, 2.3)),
#'           file='school01.csv', row.names=FALSE)
#' write.csv(data.frame(StudentName=c('Alison','Laurel'), GPA=c(2.90, 4.0)),
#'           file='school22.csv', row.names=FALSE)
#' allschools <- read.multiple.csv(uniquenames=c(1,22), prefix='school',
#'                                 suffix='.csv',
#'                                 paduniquenames=TRUE) # for 01 rather than 1
#' @rdname read.multiple
#' @export
#' @importFrom utils read.csv read.table

read.multiple.table <- function(uniquenames, path=getwd(), prefix='',
                                suffix='.txt', addfilename=FALSE,
                                paduniquenames=FALSE, ...) {

	# check to make sure the path has a / at the end
	if (substr(path,nchar(path),nchar(path)) != '/') {
		path <- paste(path, '/', sep='');
	}

	# pad with leading zeros if requested
	if (paduniquenames) {
		maxlength <- max(nchar(as.character(uniquenames)))
		uniquenames <- pad.zero(uniquenames, maxlength)
	}

	# assemble the file names
	filenames <- paste(path,prefix,uniquenames,suffix, sep=''); # get file names

	# read the files as a list
	if (addfilename) {
		datalist <- lapply(filenames, function(x) data.frame(cbind(
		  read.table(file=x, ...), Filename=x)))
	} else {
		datalist <- lapply(filenames, function(x) read.table(file=x, ...))
	}

	# convert the list to a single dataframe
	data <- do.call(rbind, datalist)

	data

}

#' @rdname read.multiple
#' @export
read.multiple.csv <- function(uniquenames, path=getwd(), prefix='',
                              suffix='.csv', addfilename=FALSE,
                              paduniquenames=FALSE, ...) {

	# check to make sure the path has a / at the end
	if (substr(path,nchar(path),nchar(path)) != '/') {
		path <- paste(path, '/', sep='');
	}

	# pad with leading zeros if requested
	if (paduniquenames) {
		maxlength <- max(nchar(as.character(uniquenames)))
		uniquenames <- pad.zero(uniquenames, maxlength)
	}

	# assemble the file names
	filenames <- paste(path,prefix,uniquenames,suffix, sep=''); # get file names

	# read the files as a list
	if (addfilename) {
		datalist <- lapply(filenames, function(x) data.frame(cbind(
		  read.csv(file=x, ...), Filename=x)))
	} else {
		datalist <- lapply(filenames, function(x) read.csv(file=x, ...))
	}

	# convert the list to a single dataframe
	data <- do.call(rbind, datalist)

	data

}
