#### D2: Setup spatial ####
## Function: Clean and plot postcodes
## Author: Dr Peter King (p.king1@leeds.ac.uk)
## Last change: 30/10/2024
## Notes: 
# - Translating from prior code so maybe scrappy
# - fixed issue with Data_Covariates_Merged wrong column names



## ****************************************************************************
#### Stage Zero:  Replication and libraries ####
## ****************************************************************************



# R version 4.1.3 (2022-03-10)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 10 x64 (build 19043)
# [1] LC_COLLATE=English_United Kingdom.1252 
#   [1] magrittr_2.0.3     lubridate_1.8.0    tidygeocoder_1.0.5 PostcodesioR_0.3.1
# [5] DCchoice_0.1.0     here_1.0.1         forcats_0.5.1      stringr_1.4.0     
# [9] dplyr_1.0.8        purrr_0.3.4        readr_2.1.2        tidyr_1.2.0       
# [13] tibble_3.1.6       ggplot2_3.3.6      tidyverse_1.3.1   
#   [1] tidyselect_1.1.2 splines_4.1.3    haven_2.5.0      lattice_0.20-45 
# [5] colorspace_2.0-3 vctrs_0.4.1      generics_0.1.2   utf8_1.2.2      
# [9] survival_3.3-1   rlang_1.0.2      pillar_1.7.0     glue_1.6.2      
# [13] withr_2.5.0      DBI_1.1.2        MLEcens_0.1-5    dbplyr_2.1.1    
# [17] modelr_0.1.8     readxl_1.4.0     lifecycle_1.0.1  munsell_0.5.0   
# [21] gtable_0.3.0     cellranger_1.1.0 rvest_1.0.2      tzdb_0.3.0      
# [25] fansi_1.0.3      broom_0.8.0      scales_1.2.0     backports_1.4.1 
# [29] jsonlite_1.8.0   fs_1.5.2         Icens_1.66.0     interval_1.1-0.8
# [33] hms_1.1.1        stringi_1.7.6    grid_4.1.3       rprojroot_2.0.3 
# [37] cli_3.3.0        tools_4.1.3      perm_1.0-0.2     Formula_1.2-4   
# [41] crayon_1.5.1     pkgconfig_2.0.3  ellipsis_0.3.2   MASS_7.3-56     
# [45] Matrix_1.4-1     xml2_1.3.3       reprex_2.0.1     assertthat_0.2.1
# [49] httr_1.4.2       rstudioapi_0.13  R6_2.5.1         compiler_4.1.3 

## Any issues installing packages try:
# Sys.setenv(RENV_DOWNLOAD_METHOD="libcurl")
# Sys.setenv(RENV_DOWNLOAD_FILE_METHOD=getOption("download.file.method"))

# renv::snapshot()
rm(list=ls())
library(tidyverse)
library(here)
library(PostcodesioR)
library(data.table)
library(ggplot2)
library(sf)

# 
# library(tidygeocoder)
# library(lubridate)
# library(tidyr)
# library(magrittr)
# library(readxl)
# library(dplyr)
# 
# library(stringi)
# library(stringr)
# library(sp)
# library(raster)
# library(RColorBrewer)
# library(tmap)
# library(gstat)


# *******************************************************
# Section 1: Import Data ####
# The long data and the shapefile 
# *******************************************************


Data_Covariates <- here("Data/Main", 
                        "Data_Covariates_Step3.csv") %>% fread() %>% data.frame()


# ## If you CBA for the above, just import this and skip it:
# Data_Covariates <-  data.frame(read.csv("Microplastics_AllData_Long_PlusSpatial_2022_07_11.csv", encoding = "latin1")) ## Otherwise just import these two



## Start by reading in the new shapefile
### Available here: https://geoportal.statistics.gov.uk/datasets/ons::counties-and-unitary-authorities-december-2019-boundaries-uk-buc/about
GB <-
  here(
    "Data",
    "Counties_and_Unitary_Authorities_(December_2019)_Boundaries_UK_BUC.shp"
  ) %>% st_read() %>% st_transform(crs = 4326)



# *******************************************************---------------
# Section 2: Add county data for matching #### 
# *******************************************************---------------




# Test_1 <- Data_Covariates$Q8Postcode[1]
# 
# 
# PostcodesioR::nearest_outcode(outcode = Test_1)[[1]]["latitude"] %>% as.numeric()
# PostcodesioR::nearest_outcode(outcode = Test_1)[[1]]["longitude"] %>% as.numeric()


# Find unique postcodes
unique_postcodes <- unique(Data_Covariates$Q8Postcode)


# Create a lat/lon lookup with admin_district and admin_county
County_lookup <- data.frame(
  post_area = unique_postcodes,
  do.call(bind_rows, lapply(unique_postcodes, function(postcode) {
    result <- PostcodesioR::nearest_outcode(outcode = postcode)
    
    # Check if result is valid; if not, return NA for all columns
    if (!is.null(result) && length(result) > 0) {
      # Extract latitude and longitude
      lat <- result[[1]]$latitude
      lon <- result[[1]]$longitude
      
      # Extract the second admin_district if available; otherwise, NA
      admin_districts <- result[[1]]$admin_district
      admin_district <- if (length(admin_districts) >= 2) {
        admin_districts[[2]]
      } else if (length(admin_districts) == 1) {
        admin_districts[[1]]
      } else {
        NA
      }
      
      # Extract admin_county; if empty, return NA
      admin_county <- if (length(result[[1]]$admin_county) > 0) {
        result[[1]]$admin_county[[1]]
      } else {
        NA
      }
      
      # Return as a data frame row
      return(data.frame(
        latitude = lat,
        longitude = lon,
        admin_district = admin_district,
        admin_county = admin_county
      ))
      
    } else {
      # If result is NULL or empty, return NA for all fields
      return(data.frame(
        latitude = NA,
        longitude = NA,
        admin_district = NA,
        admin_county = NA
      ))
    }
  }))
)


Postcode_lookup <- County_lookup %>%
  dplyr::mutate(
    ctyua19nm = coalesce(admin_county, admin_district)
  )


Data_Covariates$Postcode <- Data_Covariates$Q8Postcode
Postcode_lookup$Postcode <- County_lookup$post_area
Data_Covariates_Merged <- left_join(Data_Covariates, Postcode_lookup, by = "Postcode")




## Make sf merged object here
GB_Merged <- left_join(GB, Data_Covariates_Merged, by = "ctyua19nm")





# *******************************************************
# Section 4: Plot Data ####
# *******************************************************



## Plotting Woodlands and Respondents Together:
FigureX_Spatial <-
  ggplot(data = GB_Merged) +
  geom_sf() +
  geom_point(aes(
    y = latitude,
    x = longitude,
    colour = factor(Q3Country)
  ),
  size = 1,
  shape = 4) +
  scale_color_manual(
    name = "Country",
    values = c("red", "green", "blue"),
    labels = c("England\n(N: 1451)",
               "Scotland\n(N: 134)",
               "Wales\n(N: 66)")
  ) +
  coord_sf(
    xlim = c(st_bbox(GB_Merged)["xmin"],
             st_bbox(GB_Merged)["xmax"]),
    ylim = c(st_bbox(GB_Merged)["ymin"],
             st_bbox(GB_Merged)["ymax"]),
    expand = FALSE
  ) + theme(aspect.ratio = 5 / 5) +
  xlab(label = 'Longitude') +
  ylab(label = 'Latitude') +
  ggtitle("Location of Respondents") +
  labs(color = "Country:") +
  theme_bw()



ggsave(
  FigureX_Spatial,
  device = "jpeg",
  filename = "D2_Responses_2024_10_29.jpeg",
  width = 20,
  height = 15,
  units = "cm",
  dpi = 500
)


# **********************************************************************************
#### Section X: Export ####
# **********************************************************************************


## Exporting as CSV for ease
## and using fwrite() is much faster than write.csv()
Data_Covariates_Merged %>% 
  fwrite(sep = ",", 
         here("Data/Main", 
              "Data_Covariates_Spatial_Step4.csv") )

GB_Merged %>% st_write(here("Data/Main", 
                            "GB_D2_V1.gpkg"), append = FALSE)
 



# End Of Script ----------------------------------------------------------------------------------------------------------