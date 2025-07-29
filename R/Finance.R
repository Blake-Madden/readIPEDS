#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
finance_file_internal <- function(finFile, dictionaryFile,
                                  ..., institutions=NULL,
                                  year)
    {
    # The year on the file is financial year, so step back a year to convert to academic year.
    # Also, don't read this from the IPEDS files, because they show it as a range (e.g., 22-23),
    # unlike other files.
    fileYear <- year - 1

    financeData <- readr::read_csv(finFile) %>%
        # for forcats
        dplyr::mutate("UNITID" = forcats::as_factor(UNITID)) %>%
        # remove imputation fields
        dplyr::select(-c(dplyr::matches("^XF")))

    if (!is.null(institutions))
        {
        financeData %<>% dplyr::filter(UNITID %in% institutions)
        }

    financeData %<>%
        # if requested, collapse satellite campuses into main campuses
        dplyr::mutate("UNITID" = forcats::fct_collapse(UNITID, ...)) %>%
        # add year column
        dplyr::mutate("Academic Year" = stringr::str_glue("AY {fileYear}-", stringr::str_sub(as.character(fileYear + 1), 3)))
    
    return(financeData)
    }

#' Converts column short names in a finance dataset into long names.
#' @description Changes the variable names in a finance file to the more descriptive
#'              versions listed in the latest finance dictionary file.
#' @returns The finance file with expanded variable names.
#' @param data The finance file loaded from `finance_file_internal`.
#' @author Blake Madden
#' @note This function should be called after joining multiple finance files.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
finance_expand_colnames <- function(data)
    {
    if (is.null(data))
        {
        warning(stringr::str_glue("Invalid dataset in finance_libraries_expand_colnames()."))
        return(NULL)
        }
    if (is.na(pkg.env$readIPEDS_data_folder))
        {
        warning("IPEDS folder not defined. Please call set_ipeds_data_folder() first.")
        return(NULL)
        }
    if (!dir.exists(pkg.env$readIPEDS_data_folder))
        {
        warning(stringr::str_glue("{pkg.env$readIPEDS_data_folder} does not exist."))
        return(NULL)
        }
    
    dictionaryFile <- sort(list.files(path=pkg.env$readIPEDS_data_folder, pattern = "f[0-9]{4}_f1a.xlsx"), decreasing=T)[1]
    if (is.na(dictionaryFile))
        {
        warning(stringr::str_glue("Finance dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }
    
    varDescritptions <- readxl::read_excel(stringr::str_glue("{pkg.env$readIPEDS_data_folder}/{dictionaryFile}"),
                                           sheet = "varlist") %>%
        dplyr::select("varname", "varTitle") %>%
        # don't change the UNITID column, just the data columns
        dplyr::slice(-1)

    # use the more descriptive variable names from the dictionary file
    colNamesDF <- data.frame(varname = names(data)) %>%
        dplyr::left_join(varDescritptions, by = c("varname" = "varname")) %>%
        dplyr::mutate("varname" = dplyr::coalesce(varTitle, varname)) %>%
        dplyr::select(varname)
    names(data) <- colNamesDF$varname
    return(data)
    }

#' Loads and pivots a finance 'a' file, pivoting it into an endowment dataset.
#' @description Loads a finance file (i.e., the 'a' finance file, such as f2019_a_rv.csv)
#' and converts it into an endowments dataset.
#' 
#' Finally, an "Academic Year" column will also be added so that the output can be combined with other
#' years' data.
#' @returns The transformed endowments file.
#' @param year The year in the file name to load.
#' @param ... Institutions to collapse. This is an argument to `fct_collapse`, which is an
#'            institution and its character vector of satellite institutions to collapse into it.
#'            An example could be `"206604" = c("206613")`, and you can use a variadic list of assignments such as these.
#'            Refer to the documentation for `fct_collapse` for further details.
#' @param institutions A vector of college IDs (i.e., the UNITID) to filter on.
#' @author Blake Madden
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' \dontrun{
#' library(tidyverse)
#' 
#' # Assuming you have downloaded the finance and dictionary files
#' # (e.g., "f202223_a_rv.csv", "f202122_a.xlsx") from 2014-2019 into
#' #  your documents folder).
#' set_ipeds_data_folder(path.expand('~'))
#'   
#' financeData <-
#'   finance_load_endowments_file(year=2014) %>%
#'     bind_rows(finance_load_endowments_file(year=2015)) %>%
#'     bind_rows(finance_load_endowments_file(year=2016)) %>%
#'     bind_rows(finance_load_endowments_file(year=2017)) %>%
#'     bind_rows(finance_load_endowments_file(year=2018)) %>%
#'     bind_rows(finance_load_endowments_file(year=2019))
#' }
#' @export
finance_load_endowments_file <- function(year, ..., institutions = NULL)
    {
    if (is.na(pkg.env$readIPEDS_data_folder))
        {
        warning("IPEDS folder not defined. Please call set_ipeds_data_folder() first.")
        return(NULL)
        }
    if (!dir.exists(pkg.env$readIPEDS_data_folder))
        {
        warning(stringr::str_glue("{pkg.env$readIPEDS_data_folder} does not exist."))
        return(NULL)
        }

    # full.names=T yields relative paths, just glue the root folder to the filename later instead
    yearRange = stringr::str_glue("<str_extract(as.character(year-1), '[0-9]{2}([0-9]{2})', 1)><str_extract(as.character(year),
                         '[0-9]{2}([0-9]{2})', 1)>", .open="<", .close=">", ignore.case=T)
    fileProvisional <- list.files(pkg.env$readIPEDS_data_folder,
                                  pattern=stringr::str_glue("f{yearRange}_f1a[.]csv", ignore.case=T))
    fileFinal <- list.files(pkg.env$readIPEDS_data_folder,
                            pattern=stringr::str_glue("f{yearRange}_f1a_rv[.]csv", ignore.case=T))
    dataDictionary <- list.files(pkg.env$readIPEDS_data_folder,
                                 pattern=stringr::str_glue("f{yearRange}_f1a[.]xlsx", ignore.case=T))

    if (length(dataDictionary) == 0)
        {
        warning(stringr::str_glue("{year} finance data dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    if (length(fileProvisional) == 0 && length(fileFinal) == 0)
        {
        warning(stringr::str_glue("{year} finance file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    finance_file_internal(
        finFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/",
                                            ifelse(purrr::is_empty(fileFinal), fileProvisional, fileFinal)),
        dictionaryFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", dataDictionary),
        ...=...,
        institutions = institutions,
        year=year) %>%
        dplyr::select("Academic Year", "UNITID", dplyr::starts_with("F1H0"))
    }
