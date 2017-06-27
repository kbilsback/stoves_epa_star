#________________________________________________________
# load relevant libraries
  library(tidyverse)
  library(lubridate)

#________________________________________________________

#________________________________________________________
# load temp data and convert each column to appropriate R class
load_field_temp <- function(){

  # files are read in files twice to extract data and logger id
  lapply(list.files("../data/field/temp",
                    pattern = ".csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 7,
                           col_names = c("date", "time", "temp"),
                           col_types =
                             cols(
                               date = col_character(),
                               time = col_character(),
                               temp = col_double()),
                           na = c("", "NA")) %>%
           dplyr::mutate(logger_id = 
                           gsub("Serial Number:,,|,","",
                                readr::read_delim(x,
                                                  "\n",
                                                  n_max = 1,
                                                  skip = 1, 
                                                  col_types = 
                                                    cols(.default = col_character())
                                                  )
                                )
                         )
         ) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(date = as.Date(ifelse(grepl("0[0-9]/", date),
                                      as.Date(date, format = "%d/%m/%Y"),
                                      as.Date(date, format = "%m/%d/%y")
                                      ),
                               origin = "1970-01-01"),
                datetime = as.POSIXct(ifelse(grepl("PM|AM", time),
                                             as.POSIXct(paste(date, time),
                                                        format = "%Y-%m-%d %I:%M:%S %p"),
                                             as.POSIXct(paste(date, time),
                                                        format = "%Y-%m-%d %H:%M:%S")),
                                      origin = "1970-01-01"),
                time = as.numeric(hms(format(datetime, "%H:%M:%S")))
                )
}
#________________________________________________________

#________________________________________________________
# load sums data and convert each column to appropriate R class
load_field_sums <- function(){

  # files are read in files twice to extract data and logger id
  lapply(list.files("../data/field/sums",
                    pattern = ".csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x, 
                           skip = 20,
                           col_names = c("datetime", "units", "stove_temp"),
                           col_types = 
                             cols(
                               datetime = col_character(),
                               units = col_character(),
                               stove_temp = col_double()
                               ),
                           na = c("", "NA")
                           ) %>%
           dplyr::mutate(logger_id = 
                           gsub(".*: ", "", readr::read_csv(x, 
                                                            skip = 1,
                                                            n_max = 1,
                                                            col_names = "id",
                                                            col_types =
                                                              cols(id = col_character())
                                                            )
                                )
                         )
         ) %>%
  dplyr::bind_rows() %>%  # bind data from all files
  # convert time to secs in day and fix file problems
  dplyr::mutate(datetime = as.POSIXct(gsub("00", "16", datetime), 
                                      format = "%d/%m/%y %I:%M:%S %p"),
                date = as.Date(datetime),
                time = as.numeric(hms(format(datetime, "%H:%M:%S")))
                )
}
#________________________________________________________

#________________________________________________________ 
# load grav data and convert each column to appropriate R class
load_field_grav <- function(){

  lapply(list.files("../data/field/grav",
                    pattern = "grav.csv",
                    full.names = TRUE),
         function(x) readr::read_csv(x, col_names = TRUE,
                                     col_types = 
                                       cols(
                                         .default = col_double(),
                                         id = col_character(),
                                         pre_date = col_character(),
                                         blank_id = col_character(),
                                         post_date = col_date(format = "%d/%m/%Y"),
                                         post_pressure = col_character(),
                                         post_blank_id = col_character(),
                                         notes = col_character()
                                         ),
                                     na = c("", "NA")
                                     )
         ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(pre_date = as.Date(ifelse(grepl("^IN[A-Z]", pre_date),
                                            as.Date(pre_date, format = "%d/%m/%y"),
                                            as.Date(pre_date, format = "%m/%d/%y")),
                                     origin = "1970-01-01")
                  )

}
#________________________________________________________

#________________________________________________________ 
# load aqe data and convert each column to appropriate R class
load_field_aqe <- function(){

  lapply(list.files("../data/field/aqe",
                    pattern = "AQE.csv",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 1,
                           col_names = c("tag", "date", "time", "temp_units",
                                         "pol_units", "flow_units", "t_amb",
                                         "t_stack", "t_preheat", "o2", "co",
                                         "co2", "stack_draft", "so2", "velocity",
                                         "pressure", "rh", "dew_point",
                                         "wet_bulb_temp", "vocs"),
                           col_types = 
                             cols(
                               .default = col_double(),
                               tag = col_character(),
                               date = col_date(format = "%m/%d/%y"),
                               time = col_time(format = "%H:%M:%S"),
                               temp_units = col_character(),
                               pol_units = col_character(),
                               flow_units = col_character()),
                           na = c("", "NA", "   NA")
                           )
         ) %>% 
  dplyr::bind_rows() %>%
  dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                      format = "%Y-%m-%d %H:%M:%S"),
                time = as.numeric(hms(time)) # convert time to secs in day
                )
  }
#________________________________________________________

#________________________________________________________ 
# load aqe data and convert each column to appropriate R class
load_field_ma <- function(){

  lapply(list.files("../data/field/microaeth",
                    pattern = "MA",
                    full.names = TRUE),
         function(x)
           readr::read_csv(x,
                           skip = 17,
                           col_names = c("date", "time", "ref", "sen",
                                         "atn", "flow", "pcb_temp",
                                         "status", "battery", "bc"),
                           col_types = 
                             cols(
                               .default = col_double(),
                               date = col_date(format = ""),
                               time = col_time(format = "")),
                           na = c("", "NA")
           )
  ) %>% 
    dplyr::bind_rows() %>%
    dplyr::mutate(datetime = as.POSIXct(paste(date, time), 
                                        format = "%Y-%m-%d %H:%M:%S"),
                  time = as.numeric(hms(time)) # convert time to secs in day
    )
}
#________________________________________________________

#________________________________________________________
# load grav data and convert each column to appropriate R class
load_lab_grav <- function(){

  readr::read_csv("../data/lab/grav/grav.csv",
                  skip = 1,
                  col_names = c("id", "date", "sample_id", "start_time",
                                "end_time", "pm_mass", "pm_ef", "ir_atn",
                                "uv_atn", "mce", "fp", "bc_mass", "bc_ef",
                                "pm_flag", "bc_flag", "pm_rate", "bc_rate"),
                  col_types = 
                    cols(
                      id = col_character(),
                      date = col_date(format = "%m/%d/%y"),
                      sample_id = col_character(),
                      start_time = col_double(),
                      end_time = col_double(),
                      pm_mass = col_double(),
                      pm_ef = col_double(),
                      ir_atn = col_double(),
                      uv_atn = col_double(),
                      mce = col_double(),
                      fp = col_double(),
                      bc_mass = col_double(),
                      bc_ef = col_double(),
                      pm_flag = col_integer(),
                      bc_flag = col_integer(),
                      pm_rate = col_double(),
                      bc_rate = col_double()
                      ),
                       na = c("", "NaN")
                  )

}
#________________________________________________________