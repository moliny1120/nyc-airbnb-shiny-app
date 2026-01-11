library(shiny)
library(dplyr)
library(ggplot2)
library(rmarkdown)
library(htmltools)
library(hexbin)


source("R/helpers.R")

airbnb_raw <- read_airbnb("data/AB_NYC_2019.csv")

ui <- navbarPage(
  title = "NYC Airbnb Pricing Information",

  tabPanel(
    "Explore",
    sidebarLayout(
      sidebarPanel(
        selectInput(
          "borough",
          "Borough",
          choices = c("All", sort(unique(airbnb_raw$neighbourhood_group))),
          selected = "All"
        ),

        selectInput(
          "room_type",
          "Room Type",
          choices = c("All", sort(unique(airbnb_raw$room_type))),
          selected = "All"
        ),

        sliderInput(
          "price",
          "Price Range",
          min = floor(min(airbnb_raw$price, na.rm = TRUE)),
          max = ceiling(quantile(airbnb_raw$price, 0.99, na.rm = TRUE)),
          value = c(
            floor(quantile(airbnb_raw$price, 0.10, na.rm = TRUE)),
            ceiling(quantile(airbnb_raw$price, 0.90, na.rm = TRUE))
          ),
          step = 1
        ),

        selectInput(
          "heat_style",
          "Heat style (ggplot2)",
          choices = c("Hexbin (count)" = "hex",
                      "Level (density)" = "density"),
          selected = "hex"
        ),

        sliderInput(
          "bins",
          "Hex bins (only for Hexbin)",
          min = 15, max = 80, value = 40, step = 1
        )
      ),

      mainPanel(
        fluidRow(
          column(8, plotOutput("heat_plot", height = "520px")),
          column(4,
                 h4("Summary (filtered)"),
                 tableOutput("summary_tbl"),
                 br(),
                 h4("Top neighborhoods (filtered)"),
                 tableOutput("top_neigh_tbl")
          )
        )
      )
    )
  ),
  tabPanel(
    "About",
    uiOutput("about_ui")
  )
)



server <- function(input, output, session) {

  observeEvent(input$borough, {
    df_b <- airbnb_raw
    if (input$borough != "All") {
      df_b <- df_b |> filter(neighbourhood_group == input$borough)
    }
    rt <- sort(unique(df_b$room_type))
    updateSelectInput(session, "room_type", choices = c("All", rt), selected = "All")
  })

  filtered <- reactive({
    df <- airbnb_raw |>
      filter(price >= input$price[1], price <= input$price[2])

    if (input$borough != "All") {
      df <- df |> filter(neighbourhood_group == input$borough)
    }
    if (input$room_type != "All") {
      df <- df |> filter(room_type == input$room_type)
    }
    df
  })

  output$heat_plot <- renderPlot({
    df <- filtered()

    base <- ggplot(df, aes(x = longitude, y = latitude)) +
      coord_fixed() +
      labs(
        x = "Longitude", y = "Latitude",
        title = "NYC Airbnb Heat (no basemap)",
        subtitle = paste(
          "Borough:", input$borough,
          "| Room type:", input$room_type,
          "| Price:", paste0("$", input$price[1], " - $", input$price[2])
        )
      )

    if (input$heat_style == "hex") {
      base +
        geom_hex(bins = input$bins) +
        labs(fill = "Listings") +
        scale_fill_gradientn(
          colors = c("navy", "skyblue", "white")
        ) +
        theme_minimal()
    } else {
      base +
        stat_density_2d(
          aes(fill = after_stat(level)),
          geom = "polygon",
          alpha = 0.7
        ) +
        labs(fill = "Density") +
        scale_fill_gradientn(
          colors = c("navy", "skyblue", "white")
        ) +
        theme_minimal()
    }
  })

  output$summary_tbl <- renderTable({
    summarize_listings(filtered())
  })

  output$top_neigh_tbl <- renderTable({
    filtered() |>
      group_by(neighbourhood) |>
      summarise(
        n_listings = n(),
        avg_price = round(mean(price, na.rm = TRUE), 2),
        .groups = "drop"
      ) |>
      arrange(desc(n_listings)) |>
      head(8)
  })

  output$about_ui <- renderUI({
    out_html <- "about.html"
    if (!file.exists(out_html)) {
      rmarkdown::render("about.Rmd", output_file = out_html, quiet = TRUE)
    }
    htmltools::includeHTML(out_html)
  })

}

shinyApp(ui, server)

