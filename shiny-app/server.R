library(shiny)
source("../helper/helper.R", local = TRUE)

###functions from helper.R
include = get_include()
opp_atlas = readRDS("../clean-data/opp.Rda")
climate = readRDS("../clean-data/climate.Rda")
zillow = readRDS("../clean-data/zillow.Rda")
counties_sf = read_counties()
climate_sf = join_climate(climate, counties_sf)
climate_top = get_top()
climate_top_m = merge_climate()


shinyServer(function(input, output, session) {

  
  domain <- reactive({
    domain = climate_sf %>% 
      filter(state_name == input$variable & !is.na(net)) %>%
      filter(net == max(net) | net == min(net)) %>% 
      arrange(desc(net)) %>% 
      pull(net)
  })
  
  stateInput <- reactive({
      if(input$variable == "All"){
        climate_sf
      }
    else{
     climate_sf %>% 
      filter(state_name == input$variable)
    }
  })
  
  #This is so cool but too complicated
  # observe({
  #   x <- input$select
  # 
  #   # Can use character(0) to remove all choices
  #   if (is.null(x)){
  #     x <- character(0)
  #   }
  #   
  #   c = climate_sf %>% 
  #     filter(state_name == x & net < 0) %>% 
  #     pull(county_name) 
  #   
  #   # Can also set the label and select items
  #   updateSelectInput(session, "inSelect",
  #                     choices = c,
  #                     selected = "California"
  #   )
  # })
  # 
  # coun <- reactive({
  #   distance %>% 
  #     filter(countyname1 == input$inSelect)
  # })
  # 
  # 
  # output$dis = renderTable({
  #   coun()
  # })
  
  

    output$state <- renderPlot({
        stateInput() %>% 
            ggplot(aes()) +
            geom_sf(aes(fill = cb_net)) +
            labs(
                title = "Predicted Net Migration by County", 
                subtitle = "Global mean sea level rise resulting from global warming", 
                fill = "Net Migration"
            ) +
            scale_fill_gradient2(low = "#fdbf11", high = "#0a4c6a") +
          theme(
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(), 
          text = element_text(family = "Lato"), 
          legend.position = "none"
        )  
    })
    
    output$top_pos <- render_gt({
      
      stateInput()%>% 
        filter(!is.na(net)) %>% 
        arrange(desc(net)) %>% 
        head(n = 5) %>% 
        select(-county_fips, -cb_net) %>%
        gt() %>% 
        tab_header(
          title = "Top 5 Counties for Predicted In-Migration",
          subtitle = "Counties with Positive Net Migration"
        ) %>% 
        cols_hide(
          columns = vars(
            geometry)
        ) %>% 
        data_color(
          columns = vars(net),
          colors = scales::col_numeric(
            palette = c(
              "#cfe8f3", "#73bfe2", "#1696d2", "#0a4c6a"), 
            domain = c(domain()[1], 0))
        ) %>% 
        cols_label(
          county_name = "County",
          state_name = "State",
          net = "Net Migration")
      
    })
    
    output$top_neg <- render_gt({
      
        stateInput() %>%
        filter(!is.na(net)) %>% 
        arrange(desc(net)) %>% 
        tail(n = 5) %>% 
        select(-county_fips, -cb_net) %>%
        gt() %>% 
        tab_header(
          title = "Top 5 Counties for Predicted Out-Migration",
          subtitle = "Counties with Negative Net Migration"
        ) %>% 
        cols_hide(
          columns = vars(
            geometry)
        ) %>% 
        data_color(
          columns = vars(net),
          colors = scales::col_numeric(
            palette = c(
              "#ca5800","#fdbf11", "#fdd870", "#fff2cf"), 
            domain = c(0, domain()[2]))
        ) %>% 
        cols_label(
          county_name = "County",
          state_name = "State",
          net = "Net Migration")
    })
    
    
  output$bar <- renderPlot({
    
    climate_top %>% 
      ggplot(aes(reorder(name, net),net, fill = (net > 0)))+
      geom_bar(stat="identity") +
      coord_flip() +
      scale_fill_manual(labels = c("Negative", "Positve"),values = c("#fdbf11", "#0a4c6a")) +
      labs(
        title = "Top 15 + / -  Migration by County", 
        subtitle = "", 
        fill = "Net Migration"
      ) + 
      theme(
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Lato"),
        legend.text = element_text(size = 10),
        panel.border = element_blank(),
        legend.position = c(0.3, 0.7),
        legend.direction = "horizontal", 
        legend.key.size = unit(1, "cm")
      ) 
  })  
    
    
  output$Agg <- renderPlot({
    
    climate_top_m %>% 
      ggplot(aes(log10(zillow), h_income))+
      geom_point(color = "#73bfe2", size = .5) +
      theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Lato")
      ) +
      scale_x_reverse() +
      geom_smooth(method='lm', se = FALSE) +
      labs(
        title = "ZHVI vs Upward Mobility", 
        subtitle = "Aggregate", 
        x = "ZHVI", 
        y = "Household Income for Children Parents in 25th Percentile"
      )
  })
  
  output$Quart <- renderPlot({
    climate_top_m %>% 
      ggplot(aes(log10(zillow), h_income, color = as.factor(PCT)))+
      geom_point(size = .5) +
      scale_color_manual(values = c("#fdbf11", "#ec008b", "#000000", "#55b748")) +
      theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Lato")
      ) +
      scale_x_reverse() +
      geom_smooth(method='lm', se = FALSE) +
      labs(
        title = "ZHVI vs Household Income 25 percentile", 
        subtitle = "Quartiles", 
        color = "Quartiles", 
        x = "ZHVI", 
        y = "Household Income for Children Parents in 25th Percentile"
      )
  })
  
  
  output$Best <- render_gt({
    climate_top_m %>% 
      arrange(ratio) %>% 
      head(n = 10) %>%
      select(Name, h_income, zillow, ratio) %>% 
      gt() %>% 
      cols_label(
        Name = "County",
        h_income = "Household Income (Upward Mobility)",
        zillow = "Housing Price (ZHVI)",
        ratio = "Ratio (ZHVI / Income)")
  })
    
  

})
