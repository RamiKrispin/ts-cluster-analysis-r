library(shiny)
library(bslib)

ts <- read.csv(file = "https://raw.githubusercontent.com/RamiKrispin/ts-cluster-analysis-r/refs/heads/main/data/us_gas.csv") |>
    dplyr::mutate(date = as.Date(date)) |>
    dplyr::mutate(
        index = tsibble::yearmonth(date),
        label = paste(area_name, process_name, sep = " - ")
    ) |>
    tsibble::as_tsibble(index = index, key = c(area_name, process_name))
features <- read.csv(file = "https://raw.githubusercontent.com/RamiKrispin/ts-cluster-analysis-r/refs/heads/main/data/features.csv")


features_list <- names(features)

features_list <- features_list[-which(features_list %in% c("process", "PC1", "PC2", "PC3", "process", "area_name"))]

keys <- attributes(ts)$key

area_list <- unique(keys$area_name)
area <- c("USA", area_list[-which(area_list == "USA")])

us_process <- unique(keys$process_name[which(keys$area_name == "USA")])

process <- unique(keys$process_name)

ui <- page_navbar(
    title = "US Demand for Natural Gas Analysis",
    nav_panel(
        title = "Time Series View",
        fluidRow(
            selectInput(
                inputId = "select_area",
                label = "Select Area",
                selected = "USA",
                choices = area,
                multiple = TRUE
            ),
            selectInput(
                inputId = "select_process",
                label = "Select Process",
                selected = "Delivered to Consumers",
                choices = process,
                multiple = TRUE
            )
        ),
        fluidRow(
            plotly::plotlyOutput("ts_plot")
        )
    ),
    nav_panel(
        title = "Cluster Analysis",
        fluidRow(
            column(
                selectInput(
                    inputId = "view",
                    label = "View By:",
                    choices = c("Variable", "Cluster"),
                    selected = "Variable",
                    multiple = FALSE
                ),
                width = 3
            ),
            column(
                conditionalPanel(
                    condition = "input.view == 'Variable'",
                    selectInput(
                        inputId = "color",
                        label = "Select Variable",
                        choices = c(features_list, "area_name", "process_name"),
                        selected = "area_name",
                        multiple = FALSE
                    )
                ),
                conditionalPanel(
                    condition = "input.view == 'Cluster'",
                    selectInput(
                        inputId = "c_num",
                        label = "Number of Clusters:",
                        choices = c(2:5),
                        selected = 2,
                        multiple = FALSE
                    )
                ),
                width = 3
            ),
            column(
                selectInput(
                    inputId = "pc",
                    label = "Number of PCs",
                    choices = c("Two", "Three"),
                    selected = "Two",
                    multiple = FALSE
                ),
                width = 3
            )
        ),
        fluidRow(
            plotly::plotlyOutput("pca_plot", height = "500px", width = "100%"),
            conditionalPanel(
                condition = "input.view == 'Cluster'",
                plotly::plotlyOutput("cluster_plot", height = "400px")
            ),
            plotly::plotlyOutput("series", height = "400px")
        )
    )
)





# Define server logic required to draw a histogram ----
server <- function(input, output) {
    data <- reactiveValues(
        ts = ts,
        features = features
    )

    observeEvent(
        {
            input$select_area
            input$select_process
        },
        {
            data$ts <- ts |>
                dplyr::filter(area_name %in% input$select_area &
                    process_name %in% input$select_process)
        }
    )

    output$ts_plot <- plotly::renderPlotly({
        d <- data$ts
        p <- plotly::plot_ly(
            data = d,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "line",
            color = ~label
        )
    })


    output$pca_plot <- plotly::renderPlotly({
        if (!is.null(input$view) && input$view == "Variable") {
            if (input$pc == "Two") {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    color = ~ get(input$color),
                    type = "scatter",
                    mode = "markers"
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            } else {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    z = ~PC3,
                    color = ~ get(input$color)
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            }
        } else if (!is.null(input$view) && input$view == "Cluster") {
            if (input$pc == "Two") {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                    type = "scatter",
                    mode = "markers"
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            } else {
                p <- plotly::plot_ly(features,
                    x = ~PC1,
                    y = ~PC2,
                    z = ~PC3,
                    color = ~ as.factor(get(paste("cluster", input$c_num, sep = "")))
                ) #|>
                # plotly::layout(dragmode = "select") |>
                # plotly::event_register("m_s")
            }
        }

        return(p)
    })


    output$series <- plotly::renderPlotly({
        d <- plotly::event_data("plotly_click")
        if (!is.null(d)) {
            print(d)
            print(d[1, "x"])
            x <- which(features$PC1 == d[1, "x"])
            y <- which(features$PC2 == d[1, "y"])

            print(x)
            print(y)



            if (length(x) == 1 && x == y) {
                t <- ts |>
                    dplyr::filter(
                        area_name == features$area_name[x],
                        process_name == features$process[x]
                    ) |>
                    dplyr::arrange(date)
            }
            print(head(t))
            p <- plotly::plot_ly(t,
                x = ~date,
                y = ~value,
                type = "scatter",
                mode = "line",
                name = features$area_name[x]
            ) |>
                plotly::layout(
                    title = paste("Natural Gas - ", features$process_name[x], ", ", features$area_name[x]),
                    yaxis = list(title = "MMcf", xaxis = list(title = "Source: EIA API"))
                )
        } else {
            p <- NULL
        }

        return(p)
    })

    output$cluster_plot <- plotly::renderPlotly({
        if (input$view == "Cluster") {
            p1 <- plotly::plot_ly(features,
                y = ~trend,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = TRUE
            ) |>
                plotly::layout(
                    legend = list(title = list(text = "<b> Cluster </b>")),
                    yaxis = list(title = "Trend")
                )


            p2 <- plotly::plot_ly(features,
                y = ~linearity,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "Linearity"))


            p3 <- plotly::plot_ly(features,
                y = ~entropy,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "Entropy"))


            p4 <- plotly::plot_ly(features,
                y = ~x_acf1,
                color = ~ as.factor(get(paste("cluster", input$c_num, sep = ""))),
                type = "box",
                legendgroup = "c",
                showlegend = FALSE
            ) |>
                plotly::layout(yaxis = list(title = "ACF 1"))

            p <- plotly::subplot(p1, p2, p3, p4, nrows = 2, titleY = TRUE)
        } else {
            p <- NULL
        }

        return(p)
    })
}

shinyApp(ui = ui, server = server)


# shinylive::export(appdir = ".", destdir = "docs", package_cache = FALSE, wasm_packages =  FALSE)
