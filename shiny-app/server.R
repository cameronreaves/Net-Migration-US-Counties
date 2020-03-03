library(shiny)
source("helper/helper.R", local = TRUE)

###functions from helper.R
include = get_include()
climate = readRDS("clean-data/climate.Rda")
stats = readRDS("clean-data/stats.Rds")
counties_sf = read_counties()
climate_sf = join_climate(climate, counties_sf)

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
  
  button <- reactive({
    dist <- switch(input$adjust,
                   non = FALSE,
                   adj = TRUE)
    dist
  })

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
    
    stats %>%
      head(5) %>% 
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
    
  output$pop <- renderPlot({
    
    if(button()){
      stats %>%
        mutate(normal = net / population * 10000) %>% 
        arrange(normal) %>% 
        FSA::headtail(10) %>% 
        ggplot(aes(reorder(name, normal),normal, fill = (net > 0)))+
        geom_bar(stat="identity") +
        coord_flip() +
        scale_fill_manual(labels = c("Negative", "Positve"),values = c("#fdbf11", "#0a4c6a")) +
        labs(
          title = "Top + / -  Migration by County", 
          subtitle = "", 
          fill = "Net Migration"
        ) + 
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(family = "Lato"),
          legend.text = element_text(size = 10),
          panel.border = element_blank())
    }
    else{
      stats %>%
        arrange(net) %>% 
        FSA::headtail(10) %>% 
        ggplot(aes(reorder(name, net),net, fill = (net > 0)))+
        geom_bar(stat="identity") +
        coord_flip() +
        scale_fill_manual(labels = c("Negative", "Positve"),values = c("#fdbf11", "#0a4c6a")) +
        labs(
          title = "Top + / -  Migration by County", 
          subtitle = "", 
          fill = "Net Migration"
        ) + 
        theme(
          panel.background = element_blank(),
          panel.grid = element_blank(),
          axis.ticks = element_blank(),
          text = element_text(family = "Lato"),
          legend.text = element_text(size = 10),
          panel.border = element_blank())
    }
  })  
  output$Agg <- renderPlot({
    
    stats %>% 
      ggplot(aes(log10(population), cb_net))+
      geom_jitter(color = "#73bfe2", size = .5, alpha = .5) +
      theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Lato")
      ) +
      geom_smooth(method='lm', se = FALSE) +
      labs(
        title = "Net Migration vs Population", 
        subtitle = "", 
        x = "Population log base 10", 
        y = "Cubed Root Net Migration"
      )
  })
  
  output$Quart <- renderPlot({
    stats %>% 
      ggplot(aes(income, cb_net))+
      geom_point(color = "#73bfe2", size = .5) +
      theme(
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        text = element_text(family = "Lato")
      ) +
      geom_smooth(method='lm', se = FALSE) +
      labs(
        title = "Net Migration vs Income", 
        subtitle = "", 
        x = "Median Income", 
        y = "Cubed Root Net Migration"
      )
  })
  
  
  output$pop_table <- render_gt({
    if(button()){
      stats %>% 
        mutate(normal = net / population * 10000) %>% 
        arrange(normal) %>% 
        FSA::headtail(n = 10) %>%
        gt() %>% 
        cols_label(
          name = "County",
          income = "Median Income",
          net = "Net Migration",
          cb_net = "Cubed",
          normal = "Net / Population ",
          population = "Population")
    }else{
      stats %>% 
        mutate(normal = net / population * 10000) %>% 
        arrange(net) %>% 
        FSA::headtail(n = 10) %>%
        gt() %>% 
        cols_label(
          name = "County",
          income = "Median Income",
          net = "Net Migration",
          cb_net = "Cubed",
          normal = "Net / Population ",
          population = "Population")
    }
  })
    
  

})
