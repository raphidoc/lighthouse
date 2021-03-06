% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/qc_spm.R
\name{qc_spm}
\alias{qc_spm}
\title{qc_spm}
\usage{
qc_spm(project, mission, LabLog, SPM_tbl)
}
\arguments{
\item{project}{The top level folder of the project}

\item{mission}{Optional, name for data base file.
If none is providen, it is taken from the project name.}
}
\description{
Quality Check SPM based on SPM, Bbp relationship (minimum requirement).
Optional: CTD, Anap will be add to output QC plots if present, Other variable could be added as needed.
Data are serached for in L3.
}
\author{
Raphael Mabit
}
