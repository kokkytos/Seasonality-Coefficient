library(here)
library(raster)
library(purrr)
library(furrr)
library(tibble)
library(lubridate)
library(tidyr)
library(dplyr)
library(sf)
library(parallel)


#### Read settings ####
cfg <- config::get(file = here::here("R", "config.yml"))
options(pillar.sigfig = 7)


# functions ---------------------------------------------------------------

prepare_dates <- function(dnb_files) {
  as.Date(sub(".*_(\\d{8}).*-.*", "\\1", dnb_files), format = "%Y%m%d")
}

tiffs_to_stack <- function(dnb_files, mydates) {
  dnb_stack <- raster::stack(dnb_files)
  dnb_stack <- raster::setZ(dnb_stack, mydates)
  return(dnb_stack)
}



#### Data ####

# vector filenames
vector_files <- list.files(
  here::here(cfg$DATA_DIR, cfg$ADMIN_UNITS_DIR),
  pattern = cfg$LIST_VECTOR_PATTERN,
  recursive = F,
  full.names = F
)



#### Build Raster Stack ####
dnb_files <- list.files(
  here::here(cfg$DATA_DIR, cfg$TIFFS_DIR),
  pattern = cfg$LIST_RASTER_PATTERN,
  recursive = T,
  full.names = T
)
mydates <- prepare_dates(dnb_files)
dnb_stack <- tiffs_to_stack(dnb_files, mydates)




Scoef <- function(infile, dnb_stack) {
      
  OUTFILE <- sprintf("%s_%s%s", tools::file_path_sans_ext(infile[1]), "NTL", ".gpkg")
  LAYER <- tools::file_path_sans_ext(OUTFILE)

   if(file.exists(here::here(cfg$OUTPUT_DIR, OUTFILE))){
     message(sprintf("%s already exists. Will be deleted.",here::here(cfg$OUTPUT_DIR, OUTFILE)))
     file.remove(here::here(cfg$OUTPUT_DIR, OUTFILE))
   }
  
 
  #### Load vector dataset ####
  admin_units <- sf::st_read(here::here(cfg$DATA_DIR, cfg$ADMIN_UNITS_DIR, infile), stringsAsFactors = F) %>%
    mutate(MY_CODE = row_number())


  # Vector and raster stack should have identical crs
  if (!compareCRS(st_crs(admin_units)$proj4string, crs(dnb_stack))) {
    stop("CRS mismatch between vector and raster data")
  }


  ### Calculate monthly SoL for each year
  future::plan(multisession, workers = detectCores() - 1)
  df <-
    furrr:::future_map_dfr(seq_along(1:raster::nlayers(dnb_stack)), ~ {
      return(
        data.frame(
          SoL = exactextractr::exact_extract(dnb_stack[[.x]], admin_units, "sum"),
          layer = names(dnb_stack[[.x]]),
          dates = mydates[.x],
          year = lubridate::year(mydates[.x]),
          month = lubridate::month(mydates[.x]),
          MY_CODE = admin_units$MY_CODE,
          stringsAsFactors = F
        )
      )
    }, .progress = TRUE) %>% as_tibble()

  # Stop clusters
  future:::ClusterRegistry("stop")

  ####  Seasonality coefficient ####
  ####  Seasonal population estimates based on night-time lights, https://www.sciencedirect.com/science/article/pii/S0198971517303113 ####
  nyears <- length(dplyr::distinct(df, year)$year)
  base_month <- 3 # March
  seasonality_coefficient <- df %>%
    dplyr::group_by(MY_CODE, month) %>%
    dplyr::summarise(mean_SoL = mean(SoL, na.rm = T)) %>% # calculate average SoL per month
    dplyr::mutate(median_SoL = median(mean_SoL, na.rm = T)) %>%
    dplyr::left_join(admin_units, by = c("MY_CODE" = "MY_CODE")) %>%
    dplyr::mutate(mean_SoL = replace(mean_SoL, mean_SoL < median_SoL / nyears, NA)) %>% # 1 rule of filtering
    dplyr::mutate(mean_SoL = replace(mean_SoL, mean_SoL > (2 * median_SoL), NA)) %>% # 2 rule of filtering
    dplyr::group_by(MY_CODE) %>% # 3 rule of filtering
    dplyr::mutate(mean_SoL = replace(mean_SoL, month == base_month & is.na(mean_SoL), mean_SoL[month == base_month - 1])) %>% # 3 rule of filtering
    dplyr::mutate(mean_SoL = replace(mean_SoL, month == base_month & is.na(mean_SoL), mean_SoL[month == base_month + 1])) %>% # 3 rule of filtering
    dplyr::group_by(MY_CODE) %>% # calculate seasonality_coefficient
    dplyr::mutate(seasonality_coefficient = mean_SoL / mean_SoL[month == base_month]) %>% # calculate seasonality_coefficient
    dplyr::arrange(MY_CODE, month) # arrange data


  # Calculate peak month,peak value
  peak <- seasonality_coefficient %>%
    dplyr::group_by(MY_CODE) %>% # calculate peak value & month
    dplyr::summarise(
      peak_value_S = max(seasonality_coefficient, na.rm = T),
      peak_month_S = month[which(seasonality_coefficient == max(seasonality_coefficient, na.rm = T))][1]
    ) # calculate peak value & month. Σε περίπτωση που αντιστοιχούν δύο μήνες στην max value κράτα μονο τον πρώτο

  # Calculate season length
  season_length <- seasonality_coefficient %>%
    dplyr::group_by(MY_CODE) %>% # calculate season length
    dplyr::summarise(season_length = sum(seasonality_coefficient > (seasonality_coefficient[month == 3] * 1.5))) # calculate season length. set na.rm=T στο sum() αν δεν θελω να έχω NA στα season length


  mean_SoL_wide <- df %>%
    dplyr::group_by(MY_CODE, month) %>%
    dplyr::summarise(mean_SoL = mean(SoL, na.rm = T)) %>%
    pivot_wider(
      id_cols = c(MY_CODE),
      names_from = c(month),
      values_from = mean_SoL
    )

  colnames(mean_SoL_wide) <- c(names(mean_SoL_wide[1]), paste("meanSoL", tail(names(mean_SoL_wide), -1), sep = "_"))


  SoL_wide <- df %>% pivot_wider(
    id_cols = c(MY_CODE),
    names_from = c(year, month),
    values_from = SoL
  )
  colnames(SoL_wide) <- c(names(SoL_wide[1]), paste("SoL", tail(names(SoL_wide), -1), sep = "_"))


  seasonality_coefficient_wide <- seasonality_coefficient %>% pivot_wider(
    id_cols = c(MY_CODE),
    names_from = c(month),
    values_from = seasonality_coefficient
  )
  colnames(seasonality_coefficient_wide) <- c(names(seasonality_coefficient_wide[1]), paste("Scoef", tail(names(seasonality_coefficient_wide), -1), sep = "_"))

  final <- admin_units %>%
    dplyr::left_join(season_length, by = c("MY_CODE" = "MY_CODE")) %>%
    dplyr::left_join(peak, by = c("MY_CODE" = "MY_CODE")) %>%
    dplyr::left_join(mean_SoL_wide, by = c("MY_CODE" = "MY_CODE")) %>%
    dplyr::left_join(SoL_wide, by = c("MY_CODE" = "MY_CODE")) %>%
    dplyr::left_join(seasonality_coefficient_wide, by = c("MY_CODE" = "MY_CODE")) %>%
    sf::st_as_sf()

  # if FID exists, rename it... (FID is special column)
  if ("FID" %in% colnames(final)) {
    final <- final %>% dplyr::rename(FID_CODE = FID)
  }

  # Delete MY_CODE
  final <- select(final, -c(MY_CODE))

  # export as geopackage
  st_write(final, dsn = here::here(cfg$OUTPUT_DIR, OUTFILE), layer = LAYER)
}



# for each vector apply Scoef function
sapply(vector_files,Scoef, dnb_stack=dnb_stack)
