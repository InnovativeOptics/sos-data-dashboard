#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @import tools
#' @import dplyr
#' @import lubridate
#' @import tidyr
#' @import ggplot2
#' @import forcats
#' @import scales
#' @import purrr
#' @import stringr
#' @import RColorBrewer
#' @noRd

options(scipen = 999)

app_server <- function(input, output, session) {
  # FILE INPUT
  input_data_raw <- eventReactive(input$sales_raw,{
    ext <- tools::file_ext(input$sales_raw$name)
    switch(ext,
           xlsx = readxl::read_excel(input$sales_raw$datapath,
                                     guess_max = 21474836) %>%
             mutate("Day" = lubridate::day(`Order Date`),
               "Month" = lubridate::month(`Order Date`, label = T),
                    "Year" = lubridate::year(`Order Date`)) %>%
             head(-1)%>%
             mutate("Channel" = if_else(str_detect(pattern = coll("Traci"), Channel), "Traci Fonder",
                                        if_else(str_detect(pattern = coll("Andy"), Channel), "Andy Barrows",
                                                if_else(str_detect(pattern = coll("Ryan"), Channel), "Ryan Wilson",
                                                        if_else(str_detect(pattern = coll("Barriers/Industrial"), Channel), "Jeremy Ericson",
                                                                if_else(str_detect(pattern = coll("Matt"), Channel), "Matt Cevasco",
                                                                        if_else(str_detect(pattern = coll("Connie"), Channel), "Connie Zayas",
                                                                                if_else(str_detect(pattern = coll("Amazon"), Channel), "Amazon",
                                                                                        if_else(str_detect(pattern = coll("Online"), Channel), "Online",
                                                                                                if_else(str_detect(pattern = coll("Show"), Channel), "Show", "Other")
                                                                                        ))
                                                                        )
                                                                )
                                                        )
                                                )
                                        )

             )
             ) %>%
             mutate("Customer" = if_else(str_detect(Customer, pattern = coll("Ideal Image")), "Ideal Image",
                                         if_else(str_detect(Customer, pattern = coll("Skin Laundry")), "Skin Laundry",
                                                 if_else(str_detect(Channel, pattern = coll("Amazon")), "Amazon",
                                                         if_else(str_detect(Customer, pattern = coll("Andau")), "Andau",
                                                                 if_else(`Total Amt` < 5000, "Small orders (<$5000)",Customer)))))
             ) %>%
             mutate("Item" = if_else(str_detect(pattern = coll("MXS"), Item), "MXS",
                                     if_else(str_detect(pattern = coll("OLS"), Item), "Ocular Laser Shields",
                                             if_else(str_detect(pattern = coll("Blocky"), Item), "Blockyz",
                                                     if_else(str_detect(pattern = coll("MXM"), Item), "MXM",
                                                             if_else(str_detect(pattern = coll("MXL"), Item), "MXL",
                                                                     if_else(str_detect(pattern = coll("Gi1"), Item), "Gi1",
                                                                             if_else(str_detect(pattern = coll("Gi2"), Item), "Gi2",
                                                                                     if_else(str_detect(pattern = coll("GP30"), Item), "GP30",
                                                                                             if_else(str_detect(pattern = coll("PT"), Item), "PTCE-III",
                                                                                                     if_else(str_detect(pattern = coll("Pi3"), Item), "Pi3",
                                                                                                             if_else(str_detect(pattern = coll("IPL3"), Item), "IPL3",
                                                                                                                     if_else(str_detect(pattern = coll("IPL5"), Item), "IPL5",
                                                                                                                             if_else(str_detect(pattern = coll("GiT1"), Item), "GiT1",
                                                                                                                                     if_else(str_detect(pattern = coll("GiT5"), Item), "GiT5",
                                                                                                                                             if_else(str_detect(pattern = coll("Pi17"), Item), "Pi17",
                                                                                                                                                     if_else(str_detect(pattern = coll("Pi4"), Item), "Pi4",
                                                                                                                                                             if_else(str_detect(pattern = coll("GiT7"), Item), "GiT7",
                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi5"), Item), "Pi5",
                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi15"), Item), "Pi15",
                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi14"), Item), "Pi14",
                                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi7"), Item), "Pi7",
                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi8"), Item), "Pi8",
                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi10"), Item), "Pi10",
                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi11"), Item), "Pi11",
                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi16"), Item), "Pi16",
                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("IPL V"), Item), "IPL Vee Shields",
                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Laser V"), Item), "Laser Vee Shields",
                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi18"), Item), "Pi18",
                                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi19"), Item), "Pi19",
                                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("PL7"), Item), "PL7",
                                                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("BBCE"), Item), "BBCE",
                                                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi22"), Item), "Pi22",
                                                                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("PP16"), Item), "PP16",
                                                                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Silicone"), Item), "Goggle Accessories",
                                                                                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Ship"), Item), "Shipping",
                                                                                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("IPL"), Item), "IPL",
                                                                                                                                                                                                                                                                                                                             if_else(str_detect(pattern = coll("Pi23"), Item), "Pi23",
                                                                                                                                                                                                                                                                                                                                     if_else(str_detect(pattern = coll("Pi1"), Item), "Pi1",

                                                                                                                                                                                                                                                                                                                                             "Other"))))))))))))))))))))
                                                                                                                                                                     ))))))))))))
                                                                     )
                                                             )
                                                     )
                                             )
                                     ))
             ),
           csv = vroom::vroom(input$sales_raw$datapath, delim = ",") ,
           validate("Please upload an xlsx or csv file..."))
  })
  observeEvent(input_data_raw(),
               {
                 updateVirtualSelect(inputId = "global_month",
                                     choices = sort(unique(input_data_raw()$Month)),
                                     selected = unique(input_data_raw()$Month))
                 updateVirtualSelect(inputId = "global_year",
                                     choices = unique(input_data_raw()$Year),
                                     selected = unique(input_data_raw()$Year))
               })
  input_data <- reactive({
    req(input$global_month)
    req(input$global_year)
    req(input$global_day)
    year_data <- purrr::map_df(unique(input$global_year),
                               ~input_data_raw() %>%
                                 filter(Year == .x))
    month_data <- purrr::map_df(unique(input$global_month),
    ~year_data %>%
      filter(Month == .x))
    day_data <- purrr::map_df(seq(input$global_day[[1]], input$global_day[[2]]),
                               ~month_data %>%
                                 filter(Day == .x))
    day_data
  })
  # PRODUCT SECTION
  items_hold <- reactive({
    req(input_data())
      items_hold <- input_data()  %>%
                                  group_by(Item, Month, Year) %>%
                                  summarise(
                                    "Qty sold" = sum(Ordered, na.rm = T),
                                            "Revenue generated" = sum(`Total Amt`),
                                            "Revenue per unit (avg)" = `Revenue generated`/`Qty sold`,
                                    "Profit" = sum(Profit),
                                    "Profit per unit (avg)" = Profit/`Qty sold`)
      updateVirtualSelect(inputId = "prod_focus",
                          choices = unique(items_hold$Item),
                          selected = unique(items_hold$Item))
      items_hold

                              })
  items_view <- reactive({
    req(items_hold())
    if(is.character(input$prod_focus) && is.numeric(input$prod_show)){
      if(input$asc_desc == F){
        purrr::map_df(unique(input$prod_focus),~
                        items_hold() %>%
                        filter(Item == .x)) %>%
          arrange(desc(!!sym(input$prod_arrange))) %>%
          head(input$prod_show)
      }
      else {
        purrr::map_df(unique(input$prod_focus),~
                        items_hold() %>%
                        filter(Item == .x)) %>%
          arrange(!!sym(input$prod_arrange)) %>%
          head(input$prod_show)}
    }
    else{
      tibble("Item" = "nothing to see here",
             "Year" = 2000,
             "Qty sold" = 0,
             "Revenue generated" = 0,
             "Revenue per unit (avg)" = 0,
             "Profit" = 0,
             "Profit per unit (avg)" = 0)
    }
  })
  output$prod_graph <- renderPlot(
                                    { req(input$prod_focus)
                                      req(input$prod_show>0)
                                      req(items_view())
                                      #blues <- RColorBrewer::brewer.pal(11, "RdYlBu")
                                      #blue_range <- colorRampPalette(blues)
                                      ggplot2::ggplot(data = items_view(),
                                                      aes(y = !!sym(input$prod_arrange),
                                                          x = Month,
                                                          fill = forcats::fct_reorder(Item, !!sym(input$prod_arrange))))+
                                        ggplot2::geom_col(color = "black")+
                                        facet_wrap(vars(Year))+
                                        labs(y = input$prod_arrange)+
                                        theme(axis.text = element_text(color = "black"),
                                          axis.title = element_text(size = 24),
                                              text = element_text(family = "Arial", face = "bold", size = 14, color="black"),
                                              axis.text.x = element_text(angle = 30, hjust = 1),
                                              legend.text = element_text(size=14),
                                          legend.title = element_blank(),
                                              legend.position = "right",
                                              strip.text = element_text(size=22),
                                              legend.background = element_rect(fill = "white"),
                                              panel.background = element_rect(fill = "darkgrey"),
                                              plot.background = element_rect(fill = "white",
                                                                             color = "white"))
  })
  output$prod_table <- DT::renderDT({
                                     req(items_view())
                                     req(input$prod_arrange)
                                     DT::datatable(rownames = F,
                                                   filter = list(plain = F),
    items_view() %>%
      bind_rows(items_view() %>%
                  ungroup() %>%
                  group_by(Year) %>%
                  summarise("Item" = "Totals",
                            "Month" = "All",
                            "Qty sold" = sum(`Qty sold`, na.rm=T),
                            "Revenue generated" = sum(`Revenue generated`, na.rm=T),
                            "Revenue per unit (avg)" = mean(`Revenue per unit (avg)`, na.rm=T),
                            "Profit" = sum(Profit, na.rm=T),
                            "Profit per unit (avg)" = mean(`Profit per unit (avg)`, na.rm=T)))%>%
      arrange(desc(!!sym(input$prod_arrange)))) %>%
                                       DT::formatRound(columns = c(4,5,6,7,8),
                                                       digits = 0)
  })
  # CHANNEL SECTION
  channel_hold <- eventReactive(input_data(),{
    channel_hold <-input_data() %>%
        group_by(`Channel`, Month, Year) %>%
        summarise("Qty sold" = sum(Ordered, na.rm = T),
                  "Revenue generated" = sum(`Total Amt`),
                  "Revenue per unit (avg)" = `Revenue generated`/`Qty sold`,
                  "Profit" = sum(Profit),
                  "Profit per unit (avg)" = Profit/`Qty sold`)
    updateVirtualSelect(inputId = "channel_focus",
                        choices = unique(channel_hold$`Channel`),
                        selected = unique(channel_hold$`Channel`))
    channel_hold
  })
  channel_view <- reactive({
    req(channel_hold())
    if(is.character(input$channel_focus) && is.numeric(input$channel_show)){
      if(input$asc_desc_chan == F){
        purrr::map_df(unique(input$channel_focus),~
                        channel_hold() %>%
                        filter(`Channel` == .x)) %>%
          arrange(desc(!!sym(input$channel_arrange))) %>%
          head(input$channel_show)
      }
      else {
      purrr::map_df(unique(input$channel_focus),~
      channel_hold() %>%
        filter(`Channel` == .x)) %>%
        arrange(!!sym(input$channel_arrange)) %>%
        head(input$channel_show)}
    }
    else{
      tibble("Channel" = "nothing to see here",
             "Year" = 2000,
             "Qty sold" = 0,
             "Revenue generated" = 0,
             "Revenue per unit (avg)" = 0,
             "Profit" = 0,
             "Profit per unit (avg)" = 0)
    }
  })
  output$channel_graph <- renderPlot(
    { req(channel_view())
      req(input$channel_focus)
      req(input$channel_show>0)
      #blues <- RColorBrewer::brewer.pal(11, "RdYlBu")
      #blue_range <- colorRampPalette(blues)

      ggplot2::ggplot(data = channel_view(),
                      aes(y = !!sym(input$channel_arrange),
                          x = Month,
                          fill = forcats::fct_reorder(`Channel`, !!sym(input$channel_arrange))))+
        ggplot2::geom_col(color = "black")+
        facet_wrap(vars(Year))+
        labs(y = input$channel_arrange)+
        theme(axis.text = element_text(color = "black"),
              axis.title = element_text(size = 24),
              text = element_text(family = "Arial", face = "bold", size = 14, color="black"),
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.text = element_text(size=14),
              legend.title = element_blank(),
              legend.position = "right",
              strip.text = element_text(size=22),
              legend.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "darkgrey"),
              plot.background = element_rect(fill = "white",
                                             color = "white"))
    })
  output$channel_table <- DT::renderDT({
                                        req(channel_view())
                                        req(input$channel_arrange)
                                        DT::datatable(rownames = F,
                                                      filter = list(plain = F),
    channel_view() %>%
      bind_rows(channel_view() %>%
                  ungroup() %>%
                  group_by(Year) %>%
                  summarise("Channel" = "Total",
                            "Month" = "All",
                            "Qty sold" = sum(`Qty sold`, na.rm=T),
                            "Revenue generated" = sum(`Revenue generated`, na.rm=T),
                            "Revenue per unit (avg)" = mean(`Revenue per unit (avg)`, na.rm=T),
                            "Profit" = sum(Profit, na.rm=T),
                            "Profit per unit (avg)" = mean(`Profit per unit (avg)`, na.rm=T)))%>%
      arrange(desc(!!sym(input$channel_arrange)))) %>%
                                          DT::formatRound(columns = c(4,5,6,7,8),
                                                          digits = 0)
  })
  # CUSTOMER TYPE SECTION
  custtype_hold <- eventReactive(input_data(),{
    custtype_hold <-input_data()  %>%
      group_by(`Customer Type`, Month, Year) %>%
      summarise("Qty sold" = sum(Ordered, na.rm = T),
                "Revenue generated" = sum(`Total Amt`),
                "Revenue per unit (avg)" = `Revenue generated`/`Qty sold`,
                "Profit" = sum(Profit),
                "Profit per unit (avg)" = Profit/`Qty sold`)
    updateVirtualSelect(inputId = "custtype_focus",
                        choices = unique(custtype_hold$`Customer Type`),
                        selected = unique(custtype_hold$`Customer Type`))
    custtype_hold
  })
  custtype_view <- reactive({
    req(custtype_hold())

    if(is.character(input$custtype_focus) && is.numeric(input$custtype_show)){
      if(input$asc_desc_custtype == F){
        purrr::map_df(unique(input$custtype_focus),~
                        custtype_hold() %>%
                        filter(`Customer Type` == .x)) %>%
          arrange(desc(!!sym(input$custtype_arrange))) %>%
          head(input$custtype_show)
      }
      else {
        purrr::map_df(unique(input$custtype_focus),~
                        custtype_hold() %>%
                        filter(`Customer Type` == .x)) %>%
          arrange(!!sym(input$custtype_arrange)) %>%
          head(input$custtype_show)}
    }
    else{
      tibble("Customer Type" = "nothing to see here",
             "Year" = 2000,
             "Qty sold" = 0,
             "Revenue generated" = 0,
             "Revenue per unit (avg)" = 0,
             "Profit" = 0,
             "Profit per unit (avg)" = 0)
    }
  })
  output$custtype_graph <- renderPlot(
    { req(custtype_view())
      req(input$custtype_focus)
      req(input$custtype_show>0)
      #blues <- RColorBrewer::brewer.pal(11, "RdYlBu")
      #blue_range <- colorRampPalette(blues)

      ggplot2::ggplot(data = custtype_view(),
                      aes(y = !!sym(input$custtype_arrange),
                          x = Month,
                          fill = forcats::fct_reorder(`Customer Type`, !!sym(input$custtype_arrange))))+
        ggplot2::geom_col(color = "black")+
        facet_wrap(vars(Year))+
        labs(y = input$custtype_arrange)+
        theme(axis.text = element_text(color = "black"),
              axis.title = element_text(size = 24),
              text = element_text(family = "Arial", face = "bold", size = 14, color="black"),
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.text = element_text(size=14),
              legend.title = element_blank(),
              legend.position = "right",
              strip.text = element_text(size=22),
              legend.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "darkgrey"),
              plot.background = element_rect(fill = "white",
                                             color = "white"))
    })
  output$custtype_table <- DT::renderDT({req(input$global_month)
                                        req(custtype_view())
                                        req(input$custtype_arrange)
                                        DT::datatable(rownames = F,
                                                      filter = list(plain = F),
                                        custtype_view() %>%
                                          bind_rows(custtype_view() %>%
                                                      ungroup() %>%
                                                      summarise("Customer Type" = "Totals",
                                                                "Qty sold" = sum(`Qty sold`, na.rm=T),
                                                                "Revenue generated" = sum(`Revenue generated`, na.rm=T),
                                                                "Revenue per unit (avg)" = mean(`Revenue per unit (avg)`, na.rm=T),
                                                                "Profit" = sum(Profit, na.rm=T),
                                                                "Profit per unit (avg)" = mean(`Profit per unit (avg)`, na.rm=T)))%>%
                                          arrange(desc(!!sym(input$custtype_arrange)))) %>%
    DT::formatRound(columns = c(4,5,6,7,8),
                    digits = 0)
                                      })
  # CUSTOMER SECTION
  customer_hold <- eventReactive(input_data(),{
    customer_hold <- input_data() %>%
      group_by(Customer, Item, Month, Year) %>%
      summarise("Qty sold" = sum(Ordered, na.rm = T),
                "Revenue generated" = sum(`Total Amt`),
                "Revenue per unit (avg)" = `Revenue generated`/`Qty sold`,
                "Profit" = sum(Profit),
                "Profit per unit (avg)" = Profit/`Qty sold`)
    updateVirtualSelect(inputId = "customer_focus",
                        choices = unique(customer_hold$Customer),
                        selected = unique(customer_hold$Customer))
    updateVirtualSelect(inputId = "customer_item_focus",
                        choices = unique(customer_hold$Item),
                        selected = unique(customer_hold$Item))
    customer_hold

  })
  customer_view <- reactive({
    req(customer_hold())
    if(is.character(input$customer_focus) && is.numeric(input$customer_show) && is.character(input$customer_item_focus)){
      if(input$asc_desc_cust == F){
       cust_item <-  purrr::map_df(unique(input$customer_focus),~
                        customer_hold() %>%
                        filter(Customer == .x))
       inner_join(cust_item,
                  purrr::map_df(unique(input$customer_item_focus),~
                                  customer_hold() %>%
                                  filter(Item == .x))) %>%
          arrange(desc(!!sym(input$customer_arrange))) %>%
          head(input$customer_show)
      }
      else {
        purrr::map_df(unique(input$customer_focus),~
                        customer_hold() %>%
                        filter(Customer == .x)) %>%
          arrange(!!sym(input$customer_arrange)) %>%
          head(input$customer_show)}
    }
    else{
      tibble("Customer" = "nothing to see here",
             "Item" = "or here",
             "Year" = 2000,
             "Qty sold" = 0,
             "Revenue generated" = 0,
             "Revenue per unit (avg)" = 0,
             "Profit" = 0,
             "Profit per unit (avg)" = 0)
    }
  })
  output$customer_graph <- renderPlot(
    { req(length(customer_view()>0))
      req(input$customer_focus)
      req(input$customer_item_focus)
      req(input$customer_show>0)
      #blues <- RColorBrewer::brewer.pal(11, "RdYlBu")
      #blue_range <- colorRampPalette(blues)
      if(length(input$customer_focus) == 1){
        ggplot2::ggplot(data = customer_view(),
                        aes(y = !!sym(input$customer_arrange),
                            x = Month,
                            fill = forcats::fct_reorder(Item, !!sym(input$customer_arrange))))+
          ggplot2::geom_col(color="black")+
          facet_wrap(vars(Year))+
          labs(y = input$customer_arrange)+
          theme(axis.text = element_text(color = "black"),
                axis.title = element_text(size = 24),
                text = element_text(family = "Arial", face = "bold", size = 14, color="black"),
                axis.text.x = element_text(angle = 30, hjust = 1),
                legend.text = element_text(size=14),
                legend.title = element_blank(),
                legend.position = "right",
                strip.text = element_text(size=22),
                legend.background = element_rect(fill = "white"),
                panel.background = element_rect(fill = "darkgrey"),
                plot.background = element_rect(fill = "white",
                                               color = "white"))
      }
      else{
      ggplot2::ggplot(data = customer_view(),
                      aes(y = !!sym(input$customer_arrange),
                          x = Month,
                          fill = forcats::fct_reorder(Customer, !!sym(input$customer_arrange))))+
        ggplot2::geom_col(color="black")+
        facet_wrap(vars(Year))+
        labs(y = input$customer_arrange)+
        theme(axis.text = element_text(color = "black"),
              axis.title = element_text(size = 24),
              text = element_text(family = "Arial", face = "bold", size = 14, color="black"),
              axis.text.x = element_text(angle = 30, hjust = 1),
              legend.text = element_text(size=14),
              legend.title = element_blank(),
              legend.position = "right",
              strip.text = element_text(size=22),
              legend.background = element_rect(fill = "white"),
              panel.background = element_rect(fill = "darkgrey"),
              plot.background = element_rect(fill = "white",
                                             color = "white"))
      }
    })
  output$customer_table <- DT::renderDT({
                                         req(customer_view())
                                         req(input$customer_arrange)
                                         DT::datatable(rownames = F,
                                                       filter = list(plain = F),
    customer_view() %>%
      bind_rows(customer_view() %>%
                  ungroup() %>%
                  group_by(Year) %>%
                  summarise("Customer" = "Totals",
                            "Item" = "All",
                    "Qty sold" = sum(`Qty sold`, na.rm=T),
                            "Revenue generated" = sum(`Revenue generated`, na.rm=T),
                            "Revenue per unit (avg)" = mean(`Revenue per unit (avg)`, na.rm=T),
                            "Profit" = sum(Profit, na.rm=T),
                            "Profit per unit (avg)" = mean(`Profit per unit (avg)`, na.rm=T))) %>%
      relocate(Year, .after = Item) %>%
      arrange(desc(!!sym(input$customer_arrange)))) %>%
    DT::formatRound(columns = c(5,6,7,8,9),
                    digits = 0)

  })
}
