#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import bslib
#' @import shinyWidgets
#' @noRd

dashboard_theme <- bslib::bs_theme(version = 5,
                                   bg = "white",
                                   fg = "black",
                                   primary = "darkblue",
                                   secondary = "goldenrod")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    fluidPage(
      theme = dashboard_theme,
      shinyWidgets::setBackgroundColor(c("ghostwhite","lightblue", "ghostwhite"),
                                       gradient = "radial"),

          fluidRow(
            column(3,
                   align = 'center',
                   br(),
                   fileInput(
                     inputId = "sales_raw",
                             label = NULL,
                             accept = c(".xlsx", ".csv"),
                             buttonLabel = "Browse",
                     placeholder = "Upload data")),
            column(3,
                   align = 'center',
                   shiny::sliderInput(inputId = "global_day",
                                      label = h4(style = "color:goldenrod;",
                                                 strong("Day")),
                                      min = 1,
                                      ticks = F,
                                      max = 31,
                                      value = c(1,31))),
            column(3,
                   align = 'center',
                   shinyWidgets::virtualSelectInput(inputId = "global_month",
                                                      label = h4(style = "color:goldenrod;",
                                                                 strong("Month")),
                                                    multiple = T,
                                                      choices = NULL)),
            column(3,
                   align = 'center',
                   shinyWidgets::virtualSelectInput(inputId = "global_year",
                                                    label = h4(style = "color:goldenrod;",
                                                               strong("Year")),
                                                    multiple = T,
                                                    choices = NULL))
          )
        ,
      div(class =  "shadow p-3 mb-5 bg-body rounded",
        navset_pill(
          nav_panel(
            title = h4("Customer Type"),
                fluidRow(
                  column(12,

                         conditionalPanel("output.custtype_table",

                                          fluidRow(
                                            column(3, align='left',
                                                   shinyWidgets::numericInputIcon(inputId = "custtype_show",
                                                                                  icon = icon("magnifying-glass"),
                                                                                  label = "Number of results to show",
                                                                                  min = 0,
                                                                                  step = 8,
                                                                                  value = 24)),
                                            column(3, align='left',
                                                   shinyWidgets::virtualSelectInput(inputId = "custtype_focus",
                                                                                    label = "Customer type to show",
                                                                                    choices = NULL,
                                                                                    multiple = T,
                                                                                    optionsCount = 6)),
                                            column(3,align='left',
                                                   shinyWidgets::virtualSelectInput(inputId = "custtype_arrange",
                                                                                    label = "Arrange by",
                                                                                    choices = c("Qty sold", "Revenue generated", "Revenue per unit (avg)", "Profit", "Profit per unit (avg)"),
                                                                                    autoSelectFirstOption = T)
                                            ),
                                            column(3,align='left',
                                                   br(),
                                                   shinyWidgets::switchInput(inputId = "asc_desc_custtype",
                                                                             label = "Sort",
                                                                             onLabel = "Low to High",
                                                                             offLabel = "High to Low"))
                                          )))),
                fluidRow(
                  column(12,
                         conditionalPanel("output.custtype_table",
                                          fluidRow(column(12,align='center',
                                                          plotOutput("custtype_graph")))
                         ))),
                fluidRow(column(12,align='center',
                                DT::DTOutput("custtype_table"))
            )),
          nav_panel(
            title = h4("Products"),
            fluidRow(
              column(12,

                                         conditionalPanel("output.prod_table",

                    fluidRow(
                             column(3, align='left',
                                    shinyWidgets::numericInputIcon(inputId = "prod_show",
                                                                   icon = icon("magnifying-glass"),
                                                                   label = "Number of results to show",
                                                                   min = 0,
                                                                   step = 8,
                                                                   value = 24)),
                             column(3, align='left',
                                    shinyWidgets::virtualSelectInput(inputId = "prod_focus",
                                                                     label = "Products",
                                                                     choices = NULL,
                                                                     multiple = T,
                                                                     optionsCount = 6)),
                      column(3,align='left',
                      shinyWidgets::virtualSelectInput(inputId = "prod_arrange",
                                                       label = "Arrange by",
                                                       choices = c("Qty sold", "Revenue generated", "Revenue per unit (avg)", "Profit", "Profit per unit (avg)"),
                                                       autoSelectFirstOption = T)
                    ),
                      column(3,align='left',
                             br(),
                           shinyWidgets::switchInput(inputId = "asc_desc",
                                                     label = "Sort",
                                                     onLabel = "Low to High",
                                                     offLabel = "High to Low"))
                    )))),
            fluidRow(
                    column(12,
                        conditionalPanel("output.prod_table",
        fluidRow(column(12,align='center',
                        plotOutput("prod_graph")))
        ))),
        fluidRow(column(12,align='center',
                        DT::DTOutput("prod_table"))

        )),
        nav_panel(
          title = h4("Channels"),
          fluidRow(
            column(12,

                   conditionalPanel("output.channel_table",

                                    fluidRow(
                                      column(3, align='left',
                                             shinyWidgets::numericInputIcon(inputId = "channel_show",
                                                                            icon = icon("magnifying-glass"),
                                                                            label = "Number of results to show",
                                                                            min = 0,
                                                                            step = 8,
                                                                            value = 24)),
                                      column(3, align='left',
                                             shinyWidgets::virtualSelectInput(inputId = "channel_focus",
                                                                              label = "Channel to show",
                                                                              choices = NULL,
                                                                              multiple = T,
                                                                              optionsCount = 6)),
                                      column(3,align='left',
                                             shinyWidgets::virtualSelectInput(inputId = "channel_arrange",
                                                                              label = "Arrange by",
                                                                              choices = c("Qty sold", "Revenue generated", "Revenue per unit (avg)", "Profit", "Profit per unit (avg)"),
                                                                              autoSelectFirstOption = T)
                                      ),
                                      column(3,align='left',
                                             br(),
                                             shinyWidgets::switchInput(inputId = "asc_desc_chan",
                                                                       label = "Sort",
                                                                       onLabel = "Low to High",
                                                                       offLabel = "High to Low"))
                                    )))),
          fluidRow(
            column(12,
                   conditionalPanel("output.channel_table",
                                    fluidRow(column(12,align='center',
                                                    plotOutput("channel_graph")))
                   ))),
          fluidRow(column(12,align='center',
                          DT::DTOutput("channel_table")))
        ),
        nav_panel(
          title = h4("Customers & Products"),
              fluidRow(
                column(12,

                       conditionalPanel("output.customer_table",

                                        fluidRow(
                                          column(3, align='left',
                                                 shinyWidgets::numericInputIcon(inputId = "customer_show",
                                                                                icon = icon("magnifying-glass"),
                                                                                label = "Number of results to show",
                                                                                min = 0,
                                                                                step = 8,
                                                                                value = 24)),
                                          column(2, align='left',
                                                 shinyWidgets::virtualSelectInput(inputId = "customer_focus",
                                                                                  label = "Customer",
                                                                                  choices = NULL,
                                                                                  multiple = T,
                                                                                  optionsCount = 6)),
                                          column(2, align='left',
                                                 shinyWidgets::virtualSelectInput(inputId = "customer_item_focus",
                                                                                  label = "Item",
                                                                                  choices = NULL,
                                                                                  multiple = T,
                                                                                  optionsCount = 6)),
                                          column(3,align='left',
                                                 shinyWidgets::virtualSelectInput(inputId = "customer_arrange",
                                                                                  label = "Arrange by",
                                                                                  choices = c("Qty sold", "Revenue generated", "Revenue per unit (avg)", "Profit", "Profit per unit (avg)"),
                                                                                  autoSelectFirstOption = T)
                                          ),
                                          column(2,align='left',
                                                 br(),
                                                 shinyWidgets::switchInput(inputId = "asc_desc_cust",
                                                                           label = "Sort",
                                                                           onLabel = "Low to High",
                                                                           offLabel = "High to Low"))
                                        )))),
              fluidRow(
                column(12,
                       conditionalPanel("output.customer_table",
                                        fluidRow(column(12,align='center',
                                                        plotOutput("customer_graph")))
                       ))),
              fluidRow(column(12,align='center',
                              DT::DTOutput("customer_table"))
          ))
        ))
        ,
      fluidRow(column(12,
                      align = "right",
                      img(width = "200px",
                        src="www/logo_transparent.png")
                      )
               )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(tags$style(type = "text/css", ".vscomp-dropbox {position: absolute !important;
                       bottom: 100$ !important;
                       top: auto !important}"),
    favicon(ext= "png"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SalesDashboard"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
