library(rmarkdown)
render("dashboard/weather-report.Rmd")
file.rename("dashboard/weather-report.html", "docs/weather-report.html")
