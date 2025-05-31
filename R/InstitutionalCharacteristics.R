#' Searches for and loads the most recent institutional characteristics directory information file.
#' @returns The latest IC dataset (if found).
#' @note This can be useful for joining with other files (by "UNITID") to add the full
#' institutions' names, geo-locations, web addresses, etc.
#' @author Blake Madden
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' library(tidyverse)
#' library(magrittr)
#' 
#' # Assuming you have downloaded the completion, dictionary, and IC files
#' # (e.g., "c2014_a_rv.csv", "c2014_a.xlsx", & hd2019.csv) from
#' #  2014-2019 into into your documents folder.
#' set_ipeds_data_folder(path.expand('~'))
#' institutionFile <- institution_characteristics_load_directory_info_file()
#'   
#' if (!is.null(institutionFile))
#'     {
#'     institutionFile %<>%
#'         dplyr::select(UNITID, INSTNM, CITY, STABBR,
#'                   WEBADDR, LONGITUD, LATITUDE)
#'     completionsData <-
#'       completions_load_cip_code_file(year=2014) %>%
#'       union_all(completions_load_cip_code_file(year=2015)) %>%
#'       union_all(completions_load_cip_code_file(year=2016)) %>%
#'       union_all(completions_load_cip_code_file(year=2017)) %>%
#'       union_all(completions_load_cip_code_file(year=2018)) %>%
#'       union_all(completions_load_cip_code_file(year=2019))
#'       
#'     if (!is.null(completionsData))
#'         {
#'         completionsData %<>%
#'             left_join(institutionFile, by = c("UNITID" = "UNITID"))
#'         }
#'     }
#' @export
institution_characteristics_load_directory_info_file <- function()
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

    institutionFilePath <- sort(list.files(path=pkg.env$readIPEDS_data_folder, pattern = "^hd[0-9]{4}[.]csv"), decreasing=T)[1]
    if (is.na(institutionFilePath))
        {
        warning(stringr::str_glue("IC directory information file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    ICData <- readr::read_csv(stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", institutionFilePath)) %>%
    # useful for forcats if you need to collapse these
    dplyr::mutate("UNITID" = forcats::as_factor(UNITID))

    dictionaryFile <- sort(list.files(path=pkg.env$readIPEDS_data_folder, pattern = "^hd[0-9]{4}[.]xlsx"), decreasing=T)[1]
    if (is.na(dictionaryFile))
        {
        warning(stringr::str_glue("IC directory data dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    ControlDescriptions <-
        readxl::read_excel(stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", dictionaryFile), sheet = "Frequencies") %>%
    dplyr::filter(stringr::str_detect(varname, "^CONTROL$")) %>%
    dplyr::mutate("CONTROL" = as.numeric(codevalue)) %>%
    dplyr::mutate("PublicOrPrivate" = valuelabel) %>%
    dplyr::select(c("CONTROL", "PublicOrPrivate")) %>%
    dplyr::distinct_all()

    ICData %<>% dplyr::left_join(ControlDescriptions, by=c("CONTROL"="CONTROL"))
    }

#' Splits institution column into institution and campus columns.
#' @description Splits the `INSTNM` column in an institutional characteristics directory information dataset
#'              into `Institution` and `Campus` columns.
#' @param data An institution directory dataset loaded from `load_institution_directory_info_file`.
#' @returns The updated dataset with the new columns.
#' @note The `INSTNM` will be removed after the split.
#' @author Blake Madden
#' @importFrom magrittr %>%
#' @seealso load_institution_directory_info_file
#' @export
institution_characteristics_split_institution_and_campus <- function(data)
    {
    if (is.null(data))
        {
        warning(stringr::str_glue("Invalid dataset in institution_characteristics_split_institution_and_campus()."))
        return(NULL)
        }

    data %>%
        # add missing hyphens
        dplyr::mutate("INSTNM" = stringr::str_replace(INSTNM, '(Purdue University) (Fort Wayne|Northwest)', '\\1 - \\2')) %>%
        # "The University of Alabama" -> "University of Alabama"
        dplyr::mutate("INSTNM" = stringr::str_replace(INSTNM, '(^The )(.*)', '\\2')) %>%
        # almost all schools use a hyphen to separate the campus name,
        # but a handful use "at" and "in", so adjust those first
        dplyr::mutate("INSTNM" = stringr::str_replace(INSTNM, ' at ', ' - ')) %>%
        # just need to fix this one school
        dplyr::mutate("INSTNM" = stringr::str_replace(INSTNM, 'University - Buffalo', 'University at Buffalo')) %>%
        dplyr::mutate("INSTNM" = stringr::str_replace(INSTNM, ' in (Huntsville|St Louis)', ' - \\1')) %>%
        # split the college and campus names based an embedded hyphen
        # (note that there are a number of exceptions handled in the regex here)
        tidyr::extract(INSTNM, remove=T,
                       regex="^(All-[A-Za-z&' ]+|Mid-[A-Za-z&' ]+|Asian-[A-Za-z&' ]+|[A-Za-z]+-[A-Za-z]+ [A-Za-z&' ]+|Lex La-Ray [A-Za-z&' ]+|Northwest-Shoals [A-Za-z&' ]+|Tri-[A-Za-z&' ]+|Board of Trustees-[A-Za-z&' ]+|Hussian College-Daymar [A-Za-z&' ]+|Stevens-[A-Za-z&' ]+|West Virginia Junior College-United [A-Za-z&' ]+|University at Buffalo|Saint Mary-of-the-Woods [A-Za-z&' ]+|[A-Za-z&' ]+)(-| at )?(.*)?",
                       into=c("Institution", "sep", "Campus")) %>%
        dplyr::mutate("Institution" = stringr::str_trim(Institution)) %>%
        dplyr::mutate("Campus" = stringr::str_trim(Campus)) %>%
        dplyr::select(-sep)
    }

pkg.env <- new.env()
pkg.env$readIPEDS_data_folder <- new.env()

#' Sets the folder to read IPEDS data files from.
#' @description This should be called before any other function.
#' @param dataPath The folder to read files from.
#' @export
set_ipeds_data_folder <- function(dataPath)
    {
    pkg.env$readIPEDS_data_folder <- dataPath
    }

#' A vector containing all Ohio Community College IDs.
#' @description The UNITIDs of Ohio Community Colleges.
#' @export
oh_community_colleges <-
  c('202648', # Edison State Community College
    '202356', # Tri-C
    '202222', # Columbus State
    '201973', # Clark State
    '201928', # Cincinnati State
    '203331', # Eastern Gateway (defunct)
    '203599', # Lakeland
    '203748', # Lorain
    '204255', # Zane State
    '204422', # North Central State College
    '204440', # Northwest State Community College
    '204945', # Owens
    '205470', # Sinclair Community College
    '205841', # Stark State
    '205966', # Southern State Community College
    '206011', # Terra State Community College
    '206446', # Washington State Community College
    '201283', # Belmont
    '201672', # Central Ohio Technical College
    '203155', # Hocking
    '203678', # Rhodes
    '203881', # Marion Technical College
    '205203'  # U. of Rio Grande
    )

utils::globalVariables(c('varTitle', 'varname', 'UNITID', 'Academic Year', 'Type (Simple)', 'Collection',
                         'Circulation', 'Fact', '.', 'Demographic', 'codevalue', 'valuelabel', 'INSTNM',
                         'Institution', 'Campus', 'sep', 'CIP Description', 'MAJORNUM', 'AWLEVEL', 'CIP',
                         'Description', 'CIPCode', 'CIPTitle', 'Count', 'Gender', 'longDescription',
                         'CIPCODE', 'Race', 'Race Description', 'Employee Type', 'GenderTotal', 'GrandTotal', 'RaceTotal',
                         'SNHCAT'))
