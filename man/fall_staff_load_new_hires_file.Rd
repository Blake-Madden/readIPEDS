% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FallStaff.R
\name{fall_staff_load_new_hires_file}
\alias{fall_staff_load_new_hires_file}
\title{Loads and pivots a Fall Staff New Hires file, creating "Race", "Gender", "Employee Type", and "Occupation" columns and a single count column.}
\usage{
fall_staff_load_new_hires_file(year, ..., institutions = NULL)
}
\arguments{
\item{year}{The year in the file name to load.}

\item{...}{Institutions to collapse. This is an argument to `fct_collapse`, which is an
institution and its character vector of satellite institutions to collapse into it.
An example could be `"206604" = c("206613")`, and you can use a variadic list of assignments such as these.
Refer to the documentation for `fct_collapse` for further details.}

\item{institutions}{A vector of college IDs (i.e., the UNITID) to filter on.}
}
\value{
The transformed new hires file.
}
\description{
Loads a Fall Staff New Hires file (i.e., the 'nh' file, such as s2019_nh_rv.csv)
and its respective data dictionary and collapses the counts columns into one
This will create additional "Race", "Gender", "Employee Type", and "Occupation" categorical columns in the dataset.

Also, employee types, occupation descriptions, and race descriptions are also joined with the returned dataset.

Finally, an "Academic Year" column will also be added so that the output can be combined with other
years' data.
}
\note{
The subtotal rows be removed from the output. Also, if the final
version of the fall staff file is not found, then the provisional version will be used.
}
\author{
Blake Madden
}
