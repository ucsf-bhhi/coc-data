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
library(ggplot2)

latest_release = gh::gh("GET /repos/ucsf-bhhi/coc-data/releases/latest")
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
  httr::add_headers(c(Accept = "application/octet-stream")),
  httr::write_disk(tf, overwrite = TRUE)
)

# load the data
coc_data = readr::read_rds(tf)

excluded_variables = c("coc_name", "coc_number")
y_excluded_variables = c("year", "coc_category")

make_choices = function(data, excluded) {
  names(data)[!(names(data) %in% excluded)]
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
    panel.grid.major = element_line(color = "grey93"),
    axis.text = element_text(size = 16),
    axis.title = element_text(size = 20)
  )

ui <- fluidPage(
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
    ),
    mainPanel(
      plotOutput("scatterPlot")
    )
  )
)

server <- function(input, output) {
  output$scatterPlot <- renderPlot({
    base_plot +
    geom_point(aes(x = .data[[input$x]], y = .data[[input$y]]))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
