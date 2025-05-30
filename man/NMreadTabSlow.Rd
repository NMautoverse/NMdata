% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/NMreadTabSlow.R
\name{NMreadTabSlow}
\alias{NMreadTabSlow}
\title{Read Nonmem table files without assumptions about what tables they
contain}
\usage{
NMreadTabSlow(file, col.table.name = TRUE)
}
\arguments{
\item{file}{A Nonmem table file. Can be output tables, or one of
the several different results files from Nonmem.}

\item{col.table.name}{Name of the column (to be created)
containing the "table name" which is derived from the Nonmem
description of the table sometimes pasted above the table
data.}
}
\description{
Read Nonmem table files without assumptions about what tables they
contain
}
\details{
`NMreadTabSlow` reads parameter tables from Nonmem very
    slowly, and most often `NMreadTab` is a better function to
    use. However, `NMreadTabslow` also works for table files that
    contain incompatible tables.
}
\keyword{internal}
