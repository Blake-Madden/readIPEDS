#' Summarizes a demographic CSV file from "https://www.census.gov/quickfacts/fact/table".
#' @description This summary will include the race and gender descriptions (which match those from IPEDS),
#'              a percentage column (in decimal format), and a location column showing the area that
#'              was included in the CSV file.
#'
#'              This dataset can be useful for joining with IPEDS data, such as new hire data.
#' @seealso fall_staff_summarize_new_hires_percentages_by_race, fall_staff_summarize_new_hires_percentages_by_gender
#' @note The race labels "unknown" and "nonresident" are not included in census data and will
#'       not be in the results. (Nonresident technically is included, but overlaps the other
#'       values, rather than being a distinct classification.)
#' @returns A summary of the race's and gender's percentages within the area.
#' @param filePath The path to the census demographic CSV file.
#' @author Blake Madden
#' @export
census_load_demographic_percentages <- function(filePath)
    {
    demoData <- readr::read_csv(filePath, na = c("", "NA", ".")) %>%
        dplyr::mutate(Fact =
            dplyr::recode(Fact,
                          "Female persons, percent" = "Women",
                          "Two or More Races, percent" = "Two or more races",
                          # "White alone, percent" includes Latinx, so don't include
                          # that to prevent overlap
                          "White alone, not Hispanic or Latino, percent" = "White",
                          "Hispanic or Latino, percent" = "Hispanic or Latino",
                          "Asian alone, percent" = "Asian",
                          "Native Hawaiian and Other Pacific Islander alone, percent" = "Native Hawaiian or Other Pacific Islanders",
                          "American Indian and Alaska Native alone, percent" = "American Indian or Alaska Native",
                          "Black or African American alone, percent" = "Black or African American")) %>%
        dplyr::filter(Fact %in% c("Women", "Two or more races", "White", "Hispanic or Latino", "Asian",
                           "Native Hawaiian or Other Pacific Islanders",
                           "American Indian or Alaska Native",
                           "Black or African American")) %>%
        # convert percent into decimal
        dplyr::mutate_at(dplyr::vars(3), function(x){ as.numeric(sub("%", "",x,fixed=TRUE))/100 }) %>%
        # rename columns and add a categorical displaying the county
        dplyr::select(-c(2,4)) %>%
        dplyr::mutate("Location"=colnames(.[2])) %>%
        dplyr::rename("Demographic"=Fact) %>%
        dplyr::rename("Percent"=colnames(.[2]))

    # Add a Men row
    demoData %>%
        tibble::add_row(Demographic="Men",
                        Percent=1-(demoData %>% dplyr::filter(Demographic=="Women"))$Percent[1],
                        Location=demoData$Location[1])
    }