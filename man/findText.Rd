% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findText.R
\name{findText}
\alias{findText}
\title{findText}
\usage{
findText(files, pattern = "\\\\w+{2,}")
}
\arguments{
\item{files}{character: names of R files to analyse}

\item{pattern}{character: pattern to match a string (default: \code{"\\\\w+{2,}"})}
}
\value{
a data frame with the columns
\itemize{
\item \code{file} with file name
\item \code{line} the line number
\item \code{type} if a string constant (\code{STR_CONST}), a \code{getMsg} (\code{GETMSG}), or a \code{setMsg} (\code{SETMSG}) was found
\item \code{text} the text of the string constant or the key used in \code{getMsg} or \code{setMsg}
}
}
\description{
Extract all texts, \code{getMsg}, or \code{setMsg} calls in the R files given.
Note that the \verb{'} and \verb{"} are part of the string, thus \code{'ROUND'} has a length of 7!
}
\examples{
findText(system.file("messages", "messages.R", package="stranslate"))
}
