% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/l3_iop_gen.R
\name{generate_iop_db}
\alias{generate_iop_db}
\alias{l3_iop_gen}
\title{generate instrument databases in L3}
\usage{
l3_iop_gen(project, mission = "")
}
\description{
Loop over all L2 Station QC above 0 (in iop_processing_log) to load and extract
all non-empty list in IOP.fitted.down.RData file.
Create in L3 one csv file by instrument and a global html report for all processed variables.

TO DO: The method used here eval(parse(...)) should be changed also use check_wl_consistency to do what it says
Check ID consistency between L2 and processing_log
}
\author{
Raphael Mabit
}
