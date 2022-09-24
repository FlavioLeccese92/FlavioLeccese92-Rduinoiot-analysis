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
    .build_data2(., time, value)

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
    grid = list(top = 15, right = 100, left = 100, bottom = 15),
    xAxis = list(show = FALSE, type = "time"),
    yAxis = list(show = FALSE, scale = TRUE),
    series = series,
    backgroundColor = "transparent",
    tooltip = list(trigger = 'axis',
                   axisPointer = list(type = 'none'),
                   textStyle = list(fontWeight = 'ligher'))
  )

  plot = e_charts(width = width, height = height) %>% e_list(opts)
  if(!is.null(plot_group)){plot = plot %>% e_group(plot_group) %>% e_connect_group(plot_group)}

  return(plot)
}
