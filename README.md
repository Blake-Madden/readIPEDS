# readIPEDS

Data importing and transformation functions for IPEDS data.

Specifically, complete IPEDS data files (*.csv) are loaded and transformed in the following ways:

- MD Imputation and subtotal columns are removed
- Subtotal rows are removed
- Datasets are pivoted longer. For example, race and gender
  values are collapsed into a single `Count` column with
  respective `Race` and `Gender` categorical columns
- Descriptive labels (e.g., occupation and race descriptions)
  are joined with the data (using the data dictionary files)
- Data can be filtered by Institution IDs
- Institutions can be collapsed (i.e., satellite campuses
  can be combined into their main campus's ID)
  
 Currently, the following IPEDS components are supported:
 
 - Academic Libraries
 - Completions
 - Institutional Characteristics
 
 ## Usage
 
 For each component, download all CSV datasets and XLSX dictionary files into a folder.
 (The Institutional Characteristics files should also be downloaded to include supplemental information to your datasets.)
 
 Next, call `set_ipeds_data_folder()` to specify this folder so that *readIPEDS* knows where to load the data from.
 
 Then call functions such as `academic_libraries_load_file()` with the file year to import the data.
 (Note that loading functions support filtering on specific sets of institutions.)
 
 Finally, this dataset can be pivoted and joined with additional information.
 
 The following example will:
 
 - Load the Academic Library for all Ohio community colleges (for 2023)
 - Pivot it by collection
 - And finally, add the institutional information to the dataset
 
 ```r
library(tidyverse)
library(readIPEDS)

# All IPEDS files should be in your documents folder:
# al2023.xlsx (the AL dictionary file)
# al2023.csv (the AL data)
# hd2023.xlsx (the Institutional Characteristics dictionary file)
# hd2023.csv (the Institutional Characteristics file)

set_ipeds_data_folder(path.expand('~'))

institutionFile <- institution_characteristics_load_directory_info_file() %>%
        dplyr::select(UNITID, INSTNM, CITY)
 
ALMediaData <-
   academic_libraries_load_file(year=2023,
                                institutions=oh_community_colleges) %>%
     academic_libraries_pivot_by_collection() %>%
     # combine the institution names and cities with the AL data
     left_join(institutionFile, by = c("UNITID" = "UNITID"))
 ```
 
