#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @import tidyverse
#' @import broom
academic_libraries_load_file_internal <- function(ALFile, ..., institutions = NULL)
    {
    fileYear <- as.numeric(stringr::str_extract(ALFile, "([0-9]{4})"))

    message("Importing academic libraries file...")
    ALData <- readr::read_csv(ALFile) %>%
        # remove empty columns at end of file
        janitor::remove_empty(which = "cols") %>%
        # remove imputation fields
        dplyr::select(-c(dplyr::matches("^X[LS]"))) %>%
        # for forcats
        dplyr::mutate("UNITID" = forcats::as_factor(UNITID))

    message("Filtering institutions...")
    if (!is.null(institutions))
        {
        ALData %<>% dplyr::filter(UNITID %in% institutions)
        }

    message("Transforming academic libraries file...")
    ALData %<>%
        # if requested, collapse satellite campuses into main campuses
        dplyr::mutate("UNITID" = forcats::fct_collapse(UNITID, ...)) %>%
        # make sure numeric columns are imported as such
        # (sometimes they are read as characters for some odd reason)
        dplyr::mutate_at(dplyr::vars(!dplyr::starts_with("UNITID")), list(~ as.numeric(.))) %>%
        # add year column
        dplyr::mutate("Academic Year" = stringr::str_glue("AY {fileYear}-", stringr::str_sub(as.character(fileYear+1), 3)))

    message("Returning academic libraries file...")
    return(ALData)
    }

#' Converts column short names in an AL dataset into long names.
#' @description Changes the variable names in an Academic Libraries file to the more descriptive
#'              versions listed in the latest AL dictionary file.
#' @returns The Academic Libraries file with expanded variable names.
#' @param data The academic libraries file loaded from `academic_libraries_load_file`.
#' @author Blake Madden
#' @note This function should be called after joining multiple AL files.
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' \dontrun{
#' library(tidyverse)
#' 
#' # Assuming you have downloaded the AL files
#' # (e.g., "al2018_rv.csv", "al2018.xlsx") from
#' #  2014-2019 into your documents folder.
#' set_ipeds_data_folder(path.expand('~'))
#'
#' ALData <-
#'   academic_libraries_load_file(year=2014,
#'                                institutions=oh_community_colleges) %>%
#'     bind_rows(academic_libraries_load_file(year=2015)) %>%
#'     bind_rows(academic_libraries_load_file(year=2016)) %>%
#'     bind_rows(academic_libraries_load_file(year=2017)) %>%
#'     bind_rows(academic_libraries_load_file(year=2018)) %>%
#'     bind_rows(academic_libraries_load_file(year=2019)) %>%
#'     academic_libraries_expand_colnames()
#' }
#' @export
academic_libraries_expand_colnames <- function(data)
    {
    if (is.null(data))
        {
        warning(stringr::str_glue("Invalid dataset in academic_libraries_expand_colnames()."))
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

    dictionaryFile <- sort(list.files(path=pkg.env$readIPEDS_data_folder, pattern = "al[0-9]{4}.xlsx"), decreasing=T)[1]
    if (is.na(dictionaryFile))
        {
        warning(stringr::str_glue("Academic libraries dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    varDescritptions <- readxl::read_excel(stringr::str_glue("{pkg.env$readIPEDS_data_folder}/{dictionaryFile}"),
                                          sheet = "varlist") %>%
        dplyr::select("varname", "varTitle") %>%
        # don't change the UNITID column, just the data columns
        dplyr::slice(-1) %>%
        # simplify the column names
        dplyr::mutate("varTitle" =
                        stringr::str_to_sentence(
                          stringr::str_remove(varTitle, "^(Number of )(digital/|physical )?"))) %>%
        # improve the names a little
        dplyr::mutate("varTitle"= dplyr::case_when(
          # is there a such thing as a paper database?
          varTitle == 'Electronic databases' ~ 'Databases',
          varTitle == 'Electronic books' ~ 'E-books',
          varTitle == 'Electronic serials' ~ 'Digital serials',
          varTitle == 'Electronic media' ~ 'Digital media',
          TRUE ~ varTitle
          ))

    # use the more descriptive variable names from the dictionary file
    colNamesDF <- data.frame(varname = names(data)) %>%
        dplyr::left_join(varDescritptions, by = c("varname" = "varname")) %>%
        dplyr::mutate("varname" = dplyr::coalesce(varTitle, varname)) %>%
        dplyr::select(varname)
    names(data) <- colNamesDF$varname
    return(data)
    }

#' Loads and filters an Academic Libraries file.
#' @description Loads an Academic Libraries file, removes the imputation columns,
#'              and optionally filters and combines institutions.
#' @returns The transformed Academic Libraries file.
#' @param year The year in the file name to load.
#' @param ... Institutions to collapse. This is an argument to `fct_collapse`, which is an
#'            institution and its character vector of satellite institutions to collapse into it.
#'            An example could be `"206604" = c("206613")`, and you can use a variadic list of assignments such as these.
#'            Refer to the documentation for `fct_collapse` for further details.
#' @param institutions A vector of college IDs (i.e., the UNITID) to filter on.
#' @author Blake Madden
#' @note Additional functions to pivot this data are available.
#' @seealso academic_libraries_pivot_by_collection, academic_libraries_pivot_by_circ, academic_libraries_pivot_by_expenditure
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' \dontrun{
#' library(tidyverse)
#' 
#' # Assuming you have downloaded the AL files
#' # (e.g., "al2018_rv.csv", "al2018.xlsx") from
#' #  2014-2019 into your documents folder.
#' set_ipeds_data_folder(path.expand('~'))
#' 
#' ALData <-
#'   academic_libraries_load_file(year=2014) %>%
#'     bind_rows(academic_libraries_load_file(year=2015)) %>%
#'     bind_rows(academic_libraries_load_file(year=2016)) %>%
#'     bind_rows(academic_libraries_load_file(year=2017)) %>%
#'     bind_rows(academic_libraries_load_file(year=2018)) %>%
#'     bind_rows(academic_libraries_load_file(year=2019))
#' }
#' @export
academic_libraries_load_file <- function(year, ..., institutions = NULL)
    {
    if (is.na(pkg.env$readIPEDS_data_folder))
        {
        warning("IPEDS folder not defined. Please call set_ipeds_data_folder() first.")
        return(NULL)
        }
    if (!dir.exists(pkg.env$readIPEDS_data_folder))
        {
        warning(stringr::str_glue("Folder '{pkg.env$readIPEDS_data_folder}' does not exist."))
        return(NULL)
        }

    # full.names=T yields relative paths, just glue the root folder to the filename later instead
    fileProvisional <- list.files(path=pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("al{year}[.]csv", ignore.case=T))
    fileFinal <- list.files(path=pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("al{year}_rv[.]csv", ignore.case=T))

    if (length(fileProvisional) == 0 && length(fileFinal) == 0)
        {
        warning(stringr::str_glue("{year} academic libraries file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    academic_libraries_load_file_internal(
        ALFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/",
                                   ifelse(purrr::is_empty(fileFinal), fileProvisional, fileFinal)),
        institutions = institutions,  ... = ...,)
    }

# Internal function
academic_libraries_pivot <- function(data, pivotColumnsPattern, namesTo, totalName)
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
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot().")
        return(NULL)
        }

    data %<>% dplyr::select("UNITID", dplyr::matches(pivotColumnsPattern), "Academic Year") %>%
      academic_libraries_expand_colnames() %>%
      tidyr::pivot_longer(cols = c(dplyr::everything(), -c("UNITID", "Academic Year")),
                          names_to = namesTo, values_to = totalName)
    return(data)
    }

#' Pivots an Academic Libraries (wide) dataset into a collections dataset.
#' @description Creates a count column for all media (by school and year),
#'              with a categorical column specifying the media type.
#' @param data An academic libraries dataset loaded from `academic_libraries_load_file`.
#' @note It is required that the column names in the input are in short form.
#'       In other words, do not call `academic_libraries_expand_colnames` on the dataset
#'       before calling this.
#' @examples
#' \dontrun{
#' library(tidyverse)
#' 
#' # Assuming you have downloaded the AL files
#' # (e.g., "al2018_rv.csv", "al2018.xlsx") from
#' #  2014-2019 into your documents folder.
#' set_ipeds_data_folder(path.expand('~'))
#'   
#' ALMediaData <-
#'   academic_libraries_load_file(year=2014,
#'                                institutions=oh_community_colleges) %>%
#'     bind_rows(academic_libraries_load_file(year=2015)) %>%
#'     bind_rows(academic_libraries_load_file(year=2016)) %>%
#'     bind_rows(academic_libraries_load_file(year=2017)) %>%
#'     bind_rows(academic_libraries_load_file(year=2018)) %>%
#'     bind_rows(academic_libraries_load_file(year=2019)) %>%
#'     academic_libraries_pivot_by_collection()
#' }
#' @export
academic_libraries_pivot_by_collection <- function(data)
    {
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot_by_collection().")
        return(NULL)
        }

    academic_libraries_pivot(data, "^L[PE][DBMS]", "Type", "Collection") %>%
      dplyr::mutate("Type (Simple)" =
                      dplyr::case_when(str_detect(`Type`, "^(Electronic|Digital|E[-]|Database)") ~ "Digital",
                                       TRUE ~ "Physical"))
    }

#' Pivots an Academic Libraries dataset into a circulation dataset.
#' @description Creates a circulation column (by school and year),
#'              with a categorical column specifying the circulation type.
#' @param data An academic libraries file loaded from `academic_libraries_load_file`.
#' @note It is required that the column names in the input are in short form.
#'       In other words, do not call `academic_libraries_expand_colnames` on the dataset
#'       before calling this.
#' @export
academic_libraries_pivot_by_circ <- function(data)
    {
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot_by_circ().")
        return(NULL)
        }

    academic_libraries_pivot(data, "(LPCRCLT|LECRCLT)", "Type", "Circulation") %>%
      dplyr::mutate("Type (Simple)" =
                      dplyr::case_when(str_detect(`Type`, "([Ee]lectronic|[Dd]igital)") ~ "Digital",
                                       TRUE ~ "Physical"))
    }

#' Pivots an Academic Libraries dataset into an expenditures dataset.
#' @description Creates an expenditures column (by school and year),
#'              with a categorical column specifying the expenditure type.
#' @param data An academic libraries dataset loaded from `academic_libraries_load_file`.
#' @note It is required that the column names in the input are in short form.
#'       In other words, do not call `academic_libraries_expand_colnames` on the dataset
#'       before calling this.
#' @export
academic_libraries_pivot_by_expenditure <- function(data)
    {
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot_by_expenditure().")
        return(NULL)
        }

    academic_libraries_pivot(data,
        "(LSALWAG|LFRNGBN|LEXMSBB|LEXMSCS|LEXMSOT|LEXOMPS|LEXOMOT)",
        "Expenditure Type", "Expenditures")
    }

#' Pivots an Academic Libraries dataset into a circulation and collection dataset.
#' @description Creates circulation and collection columns (by school and year),
#'              with a categorical column specifying the media.
#' @param data An academic libraries dataset loaded from `academic_libraries_load_file`.
#' @param includeGroupedZScores TRUE to include collection and circulation z-scores, grouped by
#'                              academic year. This is useful for comparing collection vs. circulation
#'                              in a more standardized way.
#' @note It is required that the column names in the input are in short form.
#'       In other words, do not call `academic_libraries_expand_colnames` on the dataset
#'       before calling this.
#'
#'       Also, the media and circulation types are simplified down to "Physical" and "Electronic."
#' @importFrom stats ave
#' @export
academic_libraries_pivot_by_circ_and_collection <- function(data, includeGroupedZScores=F)
    {
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot_by_circ_and_collection().")
        return(NULL)
        }

    academicLibrariesMediaData <- academic_libraries_pivot_by_collection(data) %>%
    dplyr::group_by(UNITID, `Academic Year`, `Type (Simple)`) %>%
    dplyr::summarize("Collection" = sum(`Collection`, na.rm=T))

    academicLibrariesCircData <- academic_libraries_pivot_by_circ(data) %>%
    dplyr::group_by(UNITID, `Academic Year`, `Type (Simple)`) %>%
    dplyr::summarize("Circulation" = sum(`Circulation`, na.rm=T))
    
    combinedData <- academicLibrariesMediaData %>%
    dplyr::left_join(academicLibrariesCircData,
              c("UNITID"="UNITID", "Academic Year"="Academic Year", "Type (Simple)" = "Type (Simple)"))

    if (includeGroupedZScores)
        {
        combinedData$`Collection (Std. x Year)` <- stats::ave(combinedData$Collection, combinedData$`Academic Year`, FUN=scale)
        combinedData$`Circulation (Std. x Year)` <- stats::ave(combinedData$Circulation, combinedData$`Academic Year`, FUN=scale)
        }

    return(combinedData)
    }

#' Pivots an Academic Libraries dataset into an inter-library loan dataset.
#' @description Creates an inter-library column (by school and year),
#'              with a categorical column specifying the loan type (i.e., outgoing or received).
#' @param data An academic libraries dataset loaded from `academic_libraries_load_file`.
#' @note It is required that the column names in the input are in short form.
#'       In other words, do not call `academic_libraries_expand_colnames` on the dataset
#'       before calling this.
#' @export
academic_libraries_pivot_by_interlibrary_loan <- function(data)
    {
    if (is.null(data))
        {
        warning("Invalid dataset in academic_libraries_pivot_by_interlibrary_loan().")
        return(NULL)
        }

    academic_libraries_pivot(data,
                             "(LILLDPR|LILLDRC)",
                             "Loan Type", "Loans")
    }
