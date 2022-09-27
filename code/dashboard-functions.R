.build_data2 = function(data, ...) {
  require(dplyr)
  row.names(data) = NULL
  data = data %>% select(...)
  data = unname(data)
  out = as.list(as.data.frame(t(data))) %>% unname()
  return(out)
}

.plot_line_simple = function(properties_data, y, plot_group = NULL, last_n = 60,
                             width = "100%", height = "100%"){

  require(dplyr)
  require(echarts4r)

  ts_data = properties_data %>% select(time, value = {{y}}) %>% slice_tail(n = last_n) %>%
    mutate(value = round(value, 2)) %>% .build_data2(., time, value)

  series = list()
  series[[1]] = list(type = 'line', name = "Value", color = "#007BFF", showSymbol = FALSE, smooth = TRUE,
                     connectNulls = TRUE, animation = TRUE, data = ts_data,
                     emphasis = NULL)
  series[[2]] = list(type = 'scatter', name = "Start", color = "#007BFF", animation = TRUE,
                     data = list(ts_data[[1]]), tooltip = list(show = FALSE),
                     label = list(show = TRUE, position = 'left', fontWeight = 'lighter',
                                  fontSize = 16, color = "#007BFF", formatter = '{@[1]}'),
                     emphasis = NULL)
  series[[3]] = list(type = 'scatter', name = "End", color = "#007BFF", animation = TRUE,
                     data = list(ts_data[[last_n]]), tooltip = list(show = FALSE),
                     label = list(show = TRUE, position = 'right', fontWeight = 'lighter',
                                  fontSize = 16, color = "#007BFF", formatter = '{@[1]}'),
                     emphasis = NULL)

  opts = list(
    grid = list(top = 15, right = 75, left = 75, bottom = 15),
    xAxis = list(show = FALSE, type = "time"),
    yAxis = list(show = FALSE, scale = TRUE),
    series = series,
    backgroundColor = "transparent",
    tooltip = list(trigger = 'axis',
                   axisPointer = list(type = 'none'),
                   textStyle = list(fontWeight = 'lighter'))
  )

  plot = e_charts(width = width, height = height) %>% e_list(opts)
  if(!is.null(plot_group)){plot = plot %>% e_group(plot_group) %>% e_connect_group(plot_group)}

  return(plot)
}

.plot_line_multiple = function(properties_data, y, plot_group = NULL,
                               width = "100%", height = "100%"){

  require(dplyr)
  require(echarts4r)

  temp =
    properties_data %>% select(time, y = {{y}}) %>% mutate(y = round(y, 2)) %>%
    mutate(time = with_tz(time, tzone = "Europe/Berlin")) %>%
    group_by(hour = floor_date(time, 'hour')) %>%
    summarise(mean_y = mean(y, na.rm = T), sd_y = sd(y, na.rm = T), .groups = "drop") %>%
    mutate(week = floor_date(hour, "weeks", week_start = 1)) %>%
    group_by(week) %>% arrange(hour) %>%
    mutate(h_num = paste0(wday(hour, label = TRUE, abbr = TRUE), " h ",
                          format(hour, "%H"))) %>%
    ungroup()

  temp = inner_join(temp %>% distinct(week) %>% arrange(week) %>% slice_tail(n = 4),
                    temp, by = "week")
  series = list()

  temp_color = colorRampPalette(c("#A8AABC", "#007BFF"))(4)
  temp_opacity = c(0.4, 0.4, 0.4, 1)
  temp_width = c(2, 2, 2, 4)
  temp_type = c("dashed", "dashed", "dashed", "solid")

  for(w in 1:4){
    temp_w = temp %>% filter(week == unique(temp$week)[w])
    temp_w_list = temp_w %>% .build_data2(h_num, mean_y)
    series[[w]] = list(type = 'line', name = paste0("Week ", format(temp_w$week[1], "%d %B")),
                       color = temp_color[w],
                       lineStyle = list(color = temp_color[w], opacity = temp_opacity[w],
                                        width = temp_width[w], type = temp_type[w]),
                       showSymbol = FALSE, smooth = TRUE,
                       connectNulls = TRUE, animation = TRUE, data = temp_w_list,
                       emphasis = NULL)
  }
  opts = list(
    title = list(text = "Trend over last 4 weeks by hour", top = 0, left = 15,
                 textStyle = list(fontWeight = 'lighter', fontSize = 20)),
    grid = list(top = 45, right = 15, left = 50, bottom = 25),
    xAxis = list(show = TRUE, type = "category", axisLabel = list(interval = 23)),
    yAxis = list(show = TRUE, scale = TRUE),
    series = series, backgroundColor = "transparent",
    tooltip = list(trigger = 'axis', axisPointer = list(type = 'none'),
                   textStyle = list(fontWeight = 'lighter')),
    legend = list(show = TRUE, textStyle = list(fontWeight = 'lighter'), top = 0, right = 0)
  )

  plot = e_charts(width = width, height = height) %>% e_list(opts)

  if(!is.null(plot_group)){plot = plot %>% e_group(plot_group) %>% e_connect_group(plot_group)}

  return(plot)
}
.plot_boxplot = function(properties_data, y, plot_group = NULL,
                         width = "100%", height = "100%"){

  require(dplyr)
  require(echarts4r)

  temp = properties_data %>% select(time, y = {{y}}) %>%
    mutate(time = with_tz(time, tzone = "Europe/Berlin"),
           week = floor_date(time, "weeks", week_start = 1),
           tday = paste0("h ", format(time, "%H"))) %>%
    filter(!is.na(y))

  temp = inner_join(temp %>% distinct(week) %>% arrange(week) %>% slice_tail(n = 4),
                    temp, by = "week")

  temp_stat = temp %>% group_by(tday) %>%
    summarise(min = min(y), Q1 = quantile(y, 0.25), median = median(y),
              Q3 = quantile(y, 0.75), max = max(y),
              outlier_lower = quantile(y, 0.05),
              outlier_upper = quantile(y, 0.95), .groups = "drop")

  series = list()
  temp_h_list = list()
  for(h in 1: length(unique(temp_stat$tday))){
    temp_h_list[[h]] = temp_stat %>% filter(tday == unique(temp_stat$tday)[h]) %>%
      select(min, Q1, median, Q3, max) %>% as.numeric() %>% round(., 2)
  }
  series[[1]] = list(type = 'boxplot', color = "#007BFF",
                     itemStyle = list(color = "#A8AABC", borderColor = "#007BFF"),
                     animation = TRUE, data = temp_h_list,
                     emphasis = NULL)

  temp_outlier_list = temp %>% left_join(temp_stat, by = "tday") %>%
    filter(y <= outlier_lower | y >= outlier_upper) %>% mutate(y = round(y, 2)) %>% .build_data2(tday, y)

  series[[2]] = list(type = 'scatter', color = "#A8AABC",
                     itemStyle = list(color = "#A8AABC", borderColor = "#A8AABC"),
                     animation = TRUE, data = temp_outlier_list,
                     emphasis = NULL, silent = TRUE, z = 0,
                     tooltip = list(show = FALSE))

  temp_current_list = temp %>% filter(time >= max(as.Date(floor_date(temp$time)))) %>%
    group_by(tday) %>% summarise(y = median(y)) %>% mutate(y = round(y, 2)) %>% .build_data2(tday, y)

  series[[3]] = list(type = 'scatter', color = "#007BFF", name = "Current value",
                     itemStyle = list(color = "#007BFF", borderColor = "#007BFF"),
                     animation = TRUE, data = temp_current_list,
                     emphasis = NULL, z = 10,
                     tooltip = list(show = FALSE))

  opts = list(
    title = list(text = "Boxplot of last 4 weeks values by hour of day", top = 0, left = 15,
                 textStyle = list(fontWeight = 'lighter', fontSize = 20)),
    grid = list(top = 45, right = 15, left = 50, bottom = 25),
    xAxis = list(show = TRUE, type = "category",
                 data = as.list(unique(temp$tday))),
    yAxis = list(show = TRUE, scale = TRUE),
    series = series, backgroundColor = "transparent",
    tooltip = list(trigger = 'axis', axisPointer = list(type = 'none'),
                   textStyle = list(fontWeight = 'lighter')),
    legend = list(show = TRUE, top = 0, right = 0)
  )

  plot = e_charts(width = width, height = height) %>% e_list(opts)
  if(!is.null(plot_group)){plot = plot %>% e_group(plot_group) %>% e_connect_group(plot_group)}

  return(plot)
}
