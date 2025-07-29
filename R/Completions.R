#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
completions_load_cip_code_file_internal <- function(completionsFile, dictionaryFile,
                                           ...,
                                           institutions=NULL, CipLevel=1)
  {
  # the year on the file is financial year, so step back a year to convert to academic year
  fileYear <- as.numeric(stringr::str_extract(completionsFile, "([0-9]{4})")) - 1

  data('cip_codes_2020', package='readIPEDS')

  CipCodeDescriptions <- cip_codes_2020 %>%
    dplyr::select(CIPCode, CIPTitle) %>%
    dplyr::rename("CIPCODE" = CIPCode) %>%
    dplyr::rename("CIP Description" = CIPTitle)

  AwardLevelDescriptions <-readxl::read_excel(dictionaryFile, sheet = "Frequencies") %>%
    dplyr::filter(`varname` == 'AWLEVEL') %>%
    dplyr::mutate("AWLEVEL" = as.numeric(`codevalue`)) %>%
    dplyr::rename("Award Description" = `valuelabel`) %>%
    dplyr::select(c("AWLEVEL", "Award Description"))

  RaceDescriptions <- readxl::read_excel(dictionaryFile, sheet = "Description") %>%
    dplyr::filter(stringr::str_detect(varname, "C[A-Z0-9]{4}(M|W)$")) %>%
    dplyr::mutate("Race Description" = stringr::str_split(stringr::str_split(longDescription, "\r\n") %>%
                                                              purrr::map_chr(., 3), " - ") %>% purrr::map_chr(., 1)) %>%
    dplyr::mutate("Race" = stringr::str_sub(varname, 1, 5)) %>%
    dplyr::select("Race", "Race Description") %>%
    dplyr::distinct_all()

  completionsData <- readr::read_csv(completionsFile) %>%
    # remove empty columns at end of file
    janitor::remove_empty(which = "cols") %>%
    # remove CIP grand totals
    dplyr::filter(`CIPCODE` != '99') %>%
    # remove imputation and total fields (counts that we want are just the ones for race and gender)
    dplyr::select(-c(dplyr::matches("^XC"), dplyr::matches("T$"), "CTOTALM", "CTOTALW")) %>%
    # for forcats
    dplyr::mutate("UNITID" = forcats::as_factor(UNITID))

  if (!is.null(institutions))
    {
    completionsData %<>% dplyr::filter(UNITID %in% institutions)
    }

  completionsData %<>%
    # if requested, collapse satellite campuses into main campuses
    dplyr::mutate("UNITID" = forcats::fct_collapse(UNITID, ...)) %>%
    # definition file has these number formatted like '5' instead of '05',
    # so convert to numeric so that we can join them correctly
    dplyr::mutate("AWLEVEL" = as.numeric(`AWLEVEL`)) %>%
    # stack the race and gender columns
    tidyr::pivot_longer(
      cols = 5:ncol(.),
      names_to = c("Race", "Gender"),
      names_pattern = "([A-Z0-9]{5})(M|W)",
      values_to = "Count") %>%
    # collapse the CIP code to the first or second level (if requested), or keep at third
    dplyr::mutate("CIPCODE" = dplyr::case_when(
                        CipLevel == 1 ~ stringr::str_match(`CIPCODE`, "^([0-9]{2})[.][0-9]{4}")[,2],
                        CipLevel == 2 ~ stringr::str_match(`CIPCODE`, "^([0-9]{2}[.][0-9]{2})[0-9]{2}")[,2],
                        TRUE ~ `CIPCODE`)) %>%
    dplyr::group_by(`UNITID`, `CIPCODE`, `Race`, `Gender`, `AWLEVEL`) %>%
    dplyr::summarize("Count" = sum(`Count`)) %>%
    dplyr::ungroup() %>%
    # add useful description for the CIP codes, race, and award levels
    dplyr::left_join(CipCodeDescriptions, by = c("CIPCODE" = "CIPCODE")) %>%
    dplyr::left_join(RaceDescriptions, by = c("Race" = "Race")) %>%
    dplyr::left_join(AwardLevelDescriptions, by = c("AWLEVEL" = "AWLEVEL")) %>%
    # expand gender
    dplyr::mutate("Gender" = dplyr::recode(Gender, M = "Men", W = "Women")) %>%
    # add year column
    dplyr::mutate("Academic Year" = stringr::str_glue("AY {fileYear}-", stringr::str_sub(as.character(fileYear+1), 3))) %>%
    # reorder columns
    dplyr::select(c("UNITID", "Academic Year", "CIPCODE", "CIP Description",
                    "AWLEVEL", "Award Description", "Race", "Race Description",
                    "Gender", "Count"))

  return(completionsData)
  }

#' Loads and pivots a Completions 'a' file, creating "Race" and "Gender" columns and a single award-count column.
#' @description Loads a CIP Code Completions file (i.e., the 'a' Completions file, such as c2019_a_rv.csv)
#' and its respective data dictionary and collapses the award count columns into one column.
#' This will create additional "Race" and "Gender" categorical columns in the dataset.
#' 
#' Also, CIP, award level, and race descriptions are also joined with the returned dataset.
#' 
#' Finally, an "Academic Year" column will also be added so that the output can be combined with other
#' years' data.
#' @returns The transformed completions file.
#' @note The '99' CIP Code subtotals will be removed from the output. Also, if the final
#' version of the completions file is not found, then the provisional version will be used.
#' @param year The year in the file name to load.
#' @param ... Institutions to collapse. This is an argument to `fct_collapse`, which is an
#'            institution and its character vector of satellite institutions to collapse into it.
#'            An example could be `"206604" = c("206613")`, and you can use a variadic list of assignments such as these.
#'            Refer to the documentation for `fct_collapse` for further details.
#' @param institutions A vector of college IDs (i.e., the UNITID) to filter on.
#' @param CipLevel 1 to only show the CIP codes and descriptions based on the first 2 digits of the CIP codes; 2 to get the second level CIPs, 3 to use the full descriptions for the awards.
#' @author Blake Madden
#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
#' @examples
#' \dontrun{
#' library(tidyverse)
#' 
#' # Assuming you have downloaded the completion and dictionary files
#' # (e.g., "c2014_a_rv.csv", "c2014_a.xlsx") from 2014-2019 into
#' #  your documents folder).
#' set_ipeds_data_folder(path.expand('~'))
#'   
#' completionsData <-
#'   completions_load_cip_code_file(year=2014) %>%
#'     bind_rows(completions_load_cip_code_file(year=2015)) %>%
#'     bind_rows(completions_load_cip_code_file(year=2016)) %>%
#'     bind_rows(completions_load_cip_code_file(year=2017)) %>%
#'     bind_rows(completions_load_cip_code_file(year=2018)) %>%
#'     bind_rows(completions_load_cip_code_file(year=2019))
#' }
#' @export
completions_load_cip_code_file <- function(year, ..., institutions = NULL, CipLevel = 1)
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
    fileProvisional <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("c{year}_a[.]csv", ignore.case=T))
    fileFinal <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("c{year}_a_rv[.]csv", ignore.case=T))
    dataDictionary <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("c{year}_a[.]xlsx", ignore.case=T))

    if (length(dataDictionary) == 0)
        {
        warning(stringr::str_glue("{year} completions data dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    if (length(fileProvisional) == 0 && length(fileFinal) == 0)
        {
        warning(stringr::str_glue("{year} completions file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    completions_load_cip_code_file_internal(
        completionsFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/",
                                            ifelse(purrr::is_empty(fileFinal), fileProvisional, fileFinal)),
        dictionaryFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", dataDictionary),
        ...=...,
        institutions = institutions,
        CipLevel = CipLevel)
    }

#' Summarizes the n number of colleges that awarded the most awards for each CIP code.
#' @param data Data loaded from `completions_load_cip_code_file`.
#' @param n The number of top colleges to include for each CIP code.
#' @note In the case of ties within a CIP code, multiple colleges will be included.
#' @author Blake Madden
#' @export
completions_top_institutions_by_cip_code <- function(data, n = 1)
    {
    data %>% dplyr::group_by(UNITID, `Academic Year`, `CIP Description`) %>%
      dplyr::summarize(Count=sum(Count)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(`Academic Year`, `CIP Description`) %>%
      dplyr::slice_max(order_by=Count,n=n) %>%
      # don't count a college as being the top one for a degree
      # if they just reported zero and nobody else reported anything
      # for a given CIP Code
      dplyr::filter(Count > 0) %>%
      dplyr::select(UNITID, `Academic Year`, `CIP Description`, Count)
    }

#' Summarizes the n number of colleges that awarded the least awards for each CIP code.
#' @param data Data loaded from `completions_load_cip_code_file`.
#' @param n The number of top colleges to include for each CIP code.
#' @note In the case of ties within a CIP code, multiple colleges will be included.
#'       Also, if a college had reported zero awards for a CIP, then they are not counted
#'       because they clearly should have deleted the CIP instead of entering zero.
#'       Instead, we will count the colleges that actually reported a real number of awards.
#' @author Blake Madden
#' @export
completions_bottom_institutions_by_cip_code <- function(data, n=1)
    {
    data %>% dplyr::group_by(UNITID, `Academic Year`, `CIP Description`) %>%
    dplyr::summarize(Count=sum(Count)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(`Academic Year`, `CIP Description`) %>%
    dplyr::slice_min(order_by=Count,n=n) %>%
    # don't count a college as being the bottom one for a degree
    # if they just reported zero. IMO, they should have deleted this
    # CIP when they reported it and note have entered zero, so we will
    # remove these and find the college(s) with the lowest award counts
    # that actually reported something.
    dplyr::filter(Count > 0) %>%
    dplyr::select(UNITID, `Academic Year`, `CIP Description`, Count)
    }
