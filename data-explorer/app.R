#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(showtext)
library(dplyr)
library(ggplot2)
library(ggiraph)
library(labelled)

latest_release = gh::gh("GET /repos/ucsf-bhhi/coc-data/releases/latest", .token = config::get()$gh_token)
asset_position = purrr::detect_index(
  latest_release$assets, 
  ~ purrr::pluck(.x, "name") == "coc_data.rds"
)
asset_url = purrr::pluck(latest_release$assets, asset_position, "url")

# setup a tempfile
tf = tempfile()

# download the release
response = httr::GET(
  asset_url, 
  httr::add_headers(
    c(
      Authorization = paste("token", config::get()$gh_token),
      Accept = "application/octet-stream"
    )
  ),
  httr::write_disk(tf, overwrite = TRUE)
)

# load the data
coc_data = readr::read_rds(tf)

excluded_variables = c("coc_name", "coc_number")
y_excluded_variables = c("year", "coc_category")

make_choices = function(data, excluded) {
  exclusion_mask = !(names(data) %in% excluded)
  values = names(data)[exclusion_mask]
  labels = var_label(data, unlist = TRUE)[exclusion_mask]
  setNames(values, labels)
}

x_choices = make_choices(coc_data, excluded_variables)
y_choices = make_choices(coc_data, c(y_excluded_variables, excluded_variables))

font_add_google("Libre Franklin", family = "libre franklin")
showtext_auto()

base_plot = ggplot(coc_data) +
  scale_y_continuous(labels = scales::label_comma()) +
  scale_x_continuous(labels = scales::label_comma()) +
  theme_minimal(base_family = "libre franklin") +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey93")
    ,
    axis.text = element_text(size = 7, color = "grey50"),
    axis.title = element_text(size = 8, color = "grey25")
  )

format_values = function(x) {
    case_when(
        abs(x) <= 1 ~ scales::comma(x, accuracy = 0.01, trim = FALSE),
        abs(x) > 1  ~ scales::comma(x, accuracy = 1, trim = FALSE)
    )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("@import url('https://fonts.googleapis.com/css2?family=Libre+Franklin&display=swap');"))
  ),
  sidebarLayout(
    sidebarPanel(
      selectInput("y",
                  "Y-axis variable",
                  choices = y_choices,
                  selected = "homeless_per_1000_total_pop"),
      selectInput("x",
                  "X-axis variable",
                  choices = x_choices,
                  selected = "share_rent_over_50_pct_inc"),
      width = 3
    ),
    mainPanel(
      girafeOutput("scatterPlot", height = "600px"),
      width = 9
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderGirafe({
    plot = base_plot +
    geom_point_interactive(
      aes(x = .data[[input$x]],
          y = .data[[input$y]],
          tooltip = paste0("<b>", coc_number, ": ", coc_name, ", ", year, "</b><br><hr style='margin-top:1px; margin-bottom:4px'>", look_for(coc_data, input$x)$label, ": ", format_values(.data[[input$x]]), "<br>", look_for(coc_data, input$y)$label, ": ", format_values(.data[[input$y]]))
      ),
      alpha = 0.7) +
      labs(
        x = look_for(coc_data, input$x)$label,
        y = look_for(coc_data, input$y)$label
      )
    
    girafe(ggobj = plot, width_svg = 8, options = list(opts_tooltip(css = "background-color: white; rx: 15; padding: 3px 5px 3px 5px; border-color: white; border-radius: 3px; box-shadow: 3px 3px 7px grey; font-family: Libre Franklin,Helvetica,Arial,sans-serif;", opacity = 1)))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
