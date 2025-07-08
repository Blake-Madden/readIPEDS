#' @importFrom magrittr %>%
#' @importFrom magrittr %<>%
fall_staff_load_new_hires_file_internal <- function(fallStaffNewHiresFile, dictionaryFile,
                                           ..., institutions=NULL)
    {
    # Fall staff represents the staff at the start of this academic year
    fileYear <- as.numeric(stringr::str_extract(fallStaffNewHiresFile, "([0-9]{4})"))
    AYYear <- stringr::str_glue("AY {fileYear}-", stringr::str_sub(as.character(fileYear+1), 3))

    RaceDescriptions <- readxl::read_excel(dictionaryFile, sheet = "Description") %>%
        dplyr::filter(stringr::str_detect(varname, "^HR[A-Z0-9]{4}(M|W)$")) %>%
        dplyr::filter(!stringr::str_detect(varname, "TOTL")) %>%
        dplyr::mutate("Race Description" =
                    stringr::str_remove(stringr::str_split(longDescription, "\r\n") %>% purrr::map_chr(., 1), " (men|women)[.]*[ ]*$")) %>%
        dplyr::mutate("Race" = stringr::str_sub(varname, 1, 6)) %>%
        dplyr::select("Race", "Race Description") %>%
        dplyr::distinct_all()

    Occupations <-readxl::read_excel(dictionaryFile, sheet = "Frequencies") %>%
        dplyr::filter(`varname` == 'SNHCAT') %>%
        dplyr::mutate("SNHCAT" = as.numeric(`codevalue`)) %>%
        dplyr::rename("Occupations" = `valuelabel`) %>%
        dplyr::select(c("SNHCAT", "Occupations"))

    newHiresData <- readr::read_csv(fallStaffNewHiresFile, na = c("", "NA", ".")) %>%
        # remove empty columns at end of file
        janitor::remove_empty(which = "cols") %>%
        # remove imputation and total fields (counts that we want are just the ones for race and gender)
        dplyr::select(-c(dplyr::matches("^XHR"), dplyr::matches("^HR[A-Z0-9]{4}T$"), "HRTOTLM", "HRTOTLW")) %>%
        # for forcats
        dplyr::mutate("UNITID" = forcats::as_factor(UNITID))

    if (!is.null(institutions))
        {
        newHiresData %<>% dplyr::filter(UNITID %in% institutions)
        }

    newHiresData %<>%
        # remove subtotal rows
        dplyr::filter(!(SNHCAT %in% c(10000, 20000, 21000, 21010, 21041, 21040))) %>%
        # if requested, collapse satellite campuses into main campuses
        dplyr::mutate("UNITID" = forcats::fct_collapse(UNITID, ...)) %>%
        # stack the race and gender columns
        tidyr::pivot_longer(
            cols = 6:ncol(.),
            names_to = c("Race", "Gender"),
            names_pattern = "([A-Z0-9]{6})(M|W)",
            values_to = "Count") %>%
        # add a faculty/staff column
        dplyr::mutate("Employee Type" =
                          dplyr::case_when(
                              # 0 is used for total new hire rows,
                              # as well as non-Faculty
                              FACSTAT == 0 | is.na(FACSTAT) ~ "Staff",
                              TRUE ~ "Faculty"
                          )) %>%
        # FACSTAT and OCCUPCAT columns are just shorthands for the SNHCAT column
        # and aren't really needed
        dplyr::group_by(`UNITID`, `SNHCAT`, `Employee Type`, `Race`, `Gender`) %>%
        dplyr::summarize("Count" = sum(`Count`)) %>%
        dplyr::ungroup() %>%
        dplyr::left_join(RaceDescriptions, by = c("Race" = "Race")) %>%
        dplyr::left_join(Occupations, by = c("SNHCAT" = "SNHCAT")) %>%
        # expand gender
        dplyr::mutate("Gender" = dplyr::recode(Gender, M = "Men", W = "Women")) %>%
        # add year column
        dplyr::mutate("Academic Year" = AYYear)

    return(newHiresData)
    }

#' Loads and pivots a Fall Staff New Hires file, creating "Race", "Gender", "Employee Type", and "Occupation" columns and a single count column.
#' @description Loads a Fall Staff New Hires file (i.e., the 'nh' file, such as s2019_nh_rv.csv)
#' and its respective data dictionary and collapses the counts columns into one
#' This will create additional "Race", "Gender", "Employee Type", and "Occupation" categorical columns in the dataset.
#' 
#' Also, employee types, occupation descriptions, and race descriptions are also joined with the returned dataset.
#' 
#' Finally, an "Academic Year" column will also be added so that the output can be combined with other
#' years' data.
#' @returns The transformed new hires file.
#' @note The subtotal rows be removed from the output. Also, if the final
#' version of the fall staff file is not found, then the provisional version will be used.
#' @param year The year in the file name to load.
#' @param ... Institutions to collapse. This is an argument to `fct_collapse`, which is an
#'            institution and its character vector of satellite institutions to collapse into it.
#'            An example could be `"206604" = c("206613")`, and you can use a variadic list of assignments such as these.
#'            Refer to the documentation for `fct_collapse` for further details.
#' @param institutions A vector of college IDs (i.e., the UNITID) to filter on.
#' @author Blake Madden
#' @export
fall_staff_load_new_hires_file <- function(year, ..., institutions = NULL)
    {
    if (!dir.exists(pkg.env$readIPEDS_data_folder))
        {
        warning(stringr::str_glue("{pkg.env$readIPEDS_data_folder} does not exist."))
        return(NULL)
        }

    # full.names=T yields relative paths, just glue the root folder to the filename later instead
    fileProvisional <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("s{year}_nh[.]csv", ignore.case=T))
    fileFinal <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("s{year}_nh_rv[.]csv", ignore.case=T))
    dataDictionary <- list.files(pkg.env$readIPEDS_data_folder, pattern=stringr::str_glue("s{year}_nh[.]xlsx", ignore.case=T))

    if (length(dataDictionary) == 0)
        {
        warning(stringr::str_glue("{year} fall staff data dictionary file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    if (length(fileProvisional) == 0 && length(fileFinal) == 0)
        {
        warning(stringr::str_glue("{year} fall staff file not found in {pkg.env$readIPEDS_data_folder}."))
        return(NULL)
        }

    fall_staff_load_new_hires_file_internal(
        fallStaffNewHiresFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", ifelse(purrr::is_empty(fileFinal), fileProvisional, fileFinal)),
        dictionaryFile = stringr::str_glue("{pkg.env$readIPEDS_data_folder}/", dataDictionary),
        institutions = institutions,
        ... = ...)
    }

#' Summarizes new hires data into percentage breakdowns by gender and institution.
#' @returns The percentages summary.
#' @param data The new hires data loaded from `fall_staff_load_new_hires_file`.
#' @author Blake Madden
#' @export
fall_staff_summarize_new_hires_percentages_by_gender <- function(data)
    {
    if (is.null(data))
        {
        warning(stringr::str_glue("Invalid dataset in fall_staff_summarize_new_hires_percentages_by_gender()."))
        return(NULL)
        }

    newHiresTotals <- data %>% dplyr::group_by(UNITID) %>% dplyr::summarize("GrandTotal"=sum(Count))

    newHiresGenderTotals <- data %>% 
        dplyr::group_by(UNITID, Gender) %>%
        dplyr::summarize("GenderTotal"=sum(Count))

    newHiresTotals %>%
        dplyr::inner_join(newHiresGenderTotals,by=c("UNITID" = "UNITID")) %>%
        dplyr::mutate("GenderPercent"=GenderTotal/GrandTotal) %>%
        dplyr::select(-c(GrandTotal, GenderTotal))
    }

#' Summarizes new hires data into percentage breakdowns by race and institution.
#' @returns The percentages summary.
#' @param data The new hires data loaded from `fall_staff_load_new_hires_file`.
#' @author Blake Madden
#' @export
fall_staff_summarize_new_hires_percentages_by_race <- function(data)
    {
    if (is.null(data))
        {
        warning(stringr::str_glue("Invalid dataset in fall_staff_summarize_new_hires_percentages_by_gender()."))
        return(NULL)
        }

    newHiresTotals <- data %>% dplyr::group_by(UNITID) %>% dplyr::summarize("GrandTotal"=sum(Count))

    newHiresRaceTotals <- data %>% 
        dplyr::group_by(UNITID, `Race Description`) %>%
        dplyr::summarize("RaceTotal"=sum(Count))

    newHiresTotals %>%
        dplyr::inner_join(newHiresRaceTotals,by=c("UNITID" = "UNITID")) %>%
        dplyr::mutate("RacePercent"=RaceTotal/GrandTotal) %>%
        dplyr::select(-c(GrandTotal, RaceTotal))
    }
