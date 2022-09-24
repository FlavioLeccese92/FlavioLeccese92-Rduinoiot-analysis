### load libraries
library(Rduinoiot)
library(tibble)
library(dplyr)
library(lubridate)

### define function to loop over batches of 1000 rows of 1 minute interval
### (due to api limit)
things_properties_timeseries_full = function(thing_id = thing_id, property_id = property_id,
                                             from = from , to = to,...){

  # from = as.POSIXct(from, tz = "Europe/Berlin")
  # to = as.POSIXct(to, tz = "Europe/Berlin")
  all_time_points = seq.POSIXt(from, to, by = "1 min")
  time_points = all_time_points[seq(from = 1, to = length(all_time_points), by = 1000)]

  out = NULL
  for(i in seq_len(length(time_points))){

    out = rbind(out,
                things_properties_timeseries(
                  thing_id = thing_id, property_id = property_id,
                  from = time_points[i], to = time_points[i] + 1000*60) # %>%
                  # {if (nrow(.) > 0) mutate(., time = with_tz(time, tzone = "Europe/Berlin")) else .}
    )
  }
  out = out %>% filter(time < to)
  return(out)
}

### authenticate
create_auth_token()

### define thing id
thing_id = "b6822400-2f35-4d93-b3e7-be919bdc5eba"

### get property ids
properties_list = things_properties_list(thing_id) %>%
  filter(name %in% c("temperature", "humidity", "light", "pressure"))

### read old data
pre_properties_data = readRDS("data/properties_data.rds")

# from = as.POSIXct("2022-08-12")
# to = Sys.time()

### initialize from and to
from = max(pre_properties_data$time) + 60
to = Sys.time()

### initialize empty tibble with time column
out = tibble(time = seq.POSIXt(from, to, by = "1 min"))

### loop over parameters to retrieve fresh data
for(i_name in properties_list$name){

  property_id = properties_list %>% filter(name == i_name) %>% pull(id)
  inner_out = things_properties_timeseries_full(
    thing_id = thing_id, property_id = property_id,
    from = from , to = to, desc = TRUE)
  names(inner_out) = c("time", i_name)
  out = out %>% left_join(inner_out, by = "time")
}

### remove potential heading or trailing NAs
out = out %>%
  mutate(aux = complete.cases(cur_data())) %>%
  filter(cumany(aux) & rev(cumany(rev(aux)))) %>%
  select(-aux)

### bind old data with new one
out = bind_rows(pre_properties_data, out)

### make very sure you don't have duplicates
out = distinct(out)

### overwrite old data with new one
saveRDS(out, "data/properties_data.rds")

