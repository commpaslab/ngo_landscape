# Charlottesville Region Nonprofit Landscape
# CommPAS Lab
# November 2019

# Load libraries
library(shiny)
library(shinycssloaders)
library(tidyverse)
library(sf)
library(leaflet)
library(RColorBrewer)
library(plotly)
library(scales)
library(DT)
library(packcircles)
library(GGally)
library(treemap)
library(highcharter)


# Load data
load("www/nonprofit_data.Rdata")


# Define UI ----
ui <- fluidPage(
  
  tags$head(HTML("<title>CommPAS Lab</title>")),
  titlePanel("Charlottesville Region's Charitable NGO Sector"),
  
  # sidebar ----
  fluidRow(
    column(3,
           wellPanel(h3("Selections"),
                     br(),
                     radioButtons(inputId = "df",
                                  label = "Select a Nonprofit Type:",
                                  choices = c("Charitable Nonprofits", "Charitable Foundations"),
                                  selected = "Charitable Nonprofits",
                                  inline = TRUE),
                     br(),
                     checkboxGroupInput(inputId = "geo",
                                        label = "Select Localities:",
                                        choices = levels(local_ngo$locality), 
                                        selected = levels(local_ngo$locality),
                                        inline = TRUE),
                     br(),
                     checkboxGroupInput(inputId = "group",
                                        label = "Select Major Area of Activity (Groups):",
                                        choices = levels(local_ngo$major_group2), 
                                        selected = levels(local_ngo$major_group2),
                                        inline = TRUE), 
                     br(),
                     # add logo and description
                     tags$div(img(src = "three-line-bw.png", height = 90), style="text-align: center;",
                              tags$p("Dashboard Contributors:", tags$a(href = "https://data.library.virginia.edu/michele-claibourn/", "Michele Claibourn,"), "Gus Truslow,", tags$a(href = "https://batten.virginia.edu/people/paul-martin/", "Paul Martin"))
                              )
                     )
           ),
    
    # main panel ----
    column(9,
           tabsetPanel(type = "tabs",
                       tabPanel("About",
                                includeMarkdown("about1.Rmd")
                       ),
                       
                       tabPanel("Map", 
                                leafletOutput("ngomap"),
                                tags$a(href="https://nccs-data.urban.org/index.php", "Data are from Urban Institute, National Center for Charitable Statistics, IRS Business Master Files, 2019."),
                                br(),
                                br(),
                                uiOutput("summs"),
                                tags$head(tags$style("#summs{color: black;
                                                        font-size: 16px;
                                                        text-align:center;}
                                                        #summs b{color: orange;}")),
                       ),
                       
                       tabPanel("Table", 
                                tags$div(downloadButton("downloadData1", "Download Data"), style="float: right;"),
                                DTOutput("tbl")
                       ),
                       
                       tabPanel("Organizations", 
                                tags$h4("Number of Organizations by Category"),
                                withSpinner(plotlyOutput("org1"), type = 7),
                                tags$h4("Number of Organizations by Group and Locality"),
                                plotlyOutput("org2")
                       ),
                       
                       tabPanel("Income", 
                                tags$h4("Income by Category"),
                                withSpinner(highchartOutput("inc1"), type = 1),
                                tags$h4("Income by Group and Locality"),
                                plotlyOutput("inc2")
                       ),
                       
                       tabPanel("Assets", 
                                tags$h4("Assets by Category"),
                                withSpinner(highchartOutput("asset1"), type = 1),
                                tags$h4("Assets by Group and Locality"),
                                plotlyOutput("asset2")
                       ),
                       
                       tabPanel("Initial Data", 
                                tags$div(downloadButton("downloadData2", "Download Data"), style="float: right;"),
                                DTOutput("tbl_all")
                       ),
                       
                       tabPanel("About the Data",
                                includeMarkdown("about2.Rmd")
                       )
           ) # tabsetPanel close
    ) # mainPanel/column close
  ) # sidebarLayout/fluidRow close
) # fluidPage close


# Define server logic ----
server <- function(input, output) {
  
  # select data sets ----
  # selected geo dataset
  dfInput <- reactive({
    d1 <- switch(input$df,
                 "Charitable Nonprofits" = local_ngo_geo,
                 "Charitable Foundations" = local_fnd_geo)
    d1 <- filter(d1, locality %in% input$geo & major_group2 %in% input$group)
  })
  
  # select non-geo dataset
  df2 <- reactive({
    d2 <- if (input$df == "Charitable Nonprofits") {
      local_ngo
    }
    else {local_fnd}
    d2 <- filter(d2, locality %in% input$geo & major_group2 %in% input$group)
  })
  
  
  # leaflet map ----
  # generate leaflet map, use geo data
  pal <- reactive({
    colorFactor("Paired", domain = dfInput()$locality)
  })
  
  output$ngomap <- renderLeaflet({
    leaflet(dfInput()) %>%
      addProviderTiles("CartoDB.Positron") %>% # 
#      addProviderTiles("OpenStreetMap.Mapnik") %>% # 
      addCircleMarkers(lng = ~lon, lat = ~lat,
                       radius = 2, fillOpacity = 0.5,
                       color = ~pal()(dfInput()$locality),
                       popup = paste(dfInput()$name,  "<br>", 
                                     "Major Group:", dfInput()$major_group2)) %>% 
      addPolygons(data = subset(counties_sf, NAME %in% input$geo),
                  color = "grey",
                  fill = FALSE,
                  weight = 2)
  })
  
  
  # add text summary of data below map
  output$summs <- renderUI({
    list(
      
      HTML(paste("<div id='summs'><b>", as.character(nrow(df2())), input$df, "</b>", "in the selected region and groups", "</div>")),
      HTML(paste("<div id='summs'><b>", as.character(dollar(sum(df2()$income, na.rm=T))), "</b>", "in collective income", "</div>")),
      HTML(paste("<div id='summs'>", "(", filter(df2(), income == 0) %>% count(), "organizations reported 0 income)", "</div>")),
      HTML(paste("<div id='summs'><b>", as.character(dollar(sum(df2()$assets, na.rm=T))), "</b>", "in collective assets", "</div>")),
      HTML(paste("<div id='summs'>", "(", filter(df2(), assets == 0) %>% count(), "organizations reported 0 assets)", "</div>"))
      
    )
    
  })
  
  
  # data tables ----
  # generate data table for selected ngo type
  output$tbl <-  renderDT({
    subtbl <- select(df2(), name, sec_name, assets, income, ctotrev,
                     major_sector, major_group2, major_category2, 
                     address, city, state,
                     zip5, locality, fips, ein)
    datatable(subtbl) %>% 
      DT::formatStyle(columns = colnames(subtbl), fontSize = '75%') %>% 
      formatCurrency(c('income', 'assets', "ctotrev"), digits = 0)
  })
  
  # Downloadable csv of selected dataset
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste(input$df, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(dfInput(), file, row.names = FALSE)
    }
  )

  
  # org figures ----
  # generate figures for org tab: circle
  output$org1 <- renderPlotly({
    
    # Make the dataframe
    local_count <- df2() %>% 
      group_by(major_category2, major_group) %>% 
      summarize(num_org = n()) %>% ungroup()    
    
    # Generate a dataframe with one line per bubble, with center (x and y) and radius (proportional to num_org)
    packing <- circleProgressiveLayout(local_count$num_org, sizetype='area')
    local_count <- cbind(local_count, packing) # add to local_count
    # Generate coordinates of circle drawn by many straight lines (based on center/radius)
    dat_circ <- circleLayoutVertices(packing, npoints=50)
    dat_circ$num_org <- rep(local_count$num_org, each=51) # add attribute (num_org) for color aes
    dat_circ$major_group <- rep(local_count$major_group, each=51) # add attribute (num_org) for color aes
    
    # Make the circle packing plot
    p <- ggplot() + # make bubbles with geom_polygon
      geom_polygon(data = dat_circ, aes(x, y, group = id, fill=major_group), colour = "black", alpha = 0.9) +
      scale_color_brewer(palette = "Paired") + 
      geom_text(data = local_count, aes(x, y, size=num_org, label = major_category2)) + # add text in center of bubble
      scale_size_continuous(range = c(1,4)) +
      theme_void() + 
      theme(legend.position="none") +
      coord_equal()
    ggplotly(p, tooltip = c("label", "size"))
    
  })
  
  
  # generate figures for org tab: parallel coord
  output$org2 <- renderPlotly({
    
    # Make the dataframe
    local_count <- df2() %>% 
      group_by(locality, major_group) %>% 
      summarize(num_org = n()) %>% 
      pivot_wider(names_from = major_group, values_from = num_org) %>% 
      mutate_at(vars(-group_cols()), ~ifelse(is.na(.), 0, .))
    
    # Make the parellel coordinate plot
    p <-  ggparcoord(local_count,
                     columns = 2:length(local_count), groupColumn = 1,
                     showPoints = TRUE, 
                     alphaLines = 0.3,
                     scale = "globalminmax") + 
      scale_color_brewer(palette = "Paired") +
      labs(x = "", y = "Number of Organizations", color = "Locality") + 
      theme(axis.text.x = element_text(angle = 45))
    ggplotly(p, height = 500) 
    
  })
  
  
  # income figures ----
  # generate figures for income tab: tree
  
  output$inc1 <- renderHighchart({
    
    # Make the dataframe
    local_income <- df2() %>% 
      group_by(major_sector, major_category2) %>% 
      summarize(income = sum(income, na.rm=T)) %>% ungroup() %>% 
      mutate(major_sector = as.character(major_sector),
             major_category2 = as.character(major_category2),
             major_category2 = ifelse(major_category2 == "Education", "Education-Total",
                                      major_category2),
             major_category2 = ifelse(major_category2 == "Health", "Health-General", 
                                      major_category2),
             major_category2 = ifelse(major_category2 == "Arts, Culture", "Arts, Culture-Total",
                                      major_category2))
    
    # Make the tree map
    p <- treemap(local_income,
                 index=c("major_sector", "major_category2"),
                 vSize="income",
                 type="index",
                 vColor = "major_sector",
                 title = "",
                 palette = "Paired",
                 pdf(file = NULL)
    )
    
    hctreemap(p, allowDrillToNode = TRUE) %>% 
      hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Income: {point.value:,.0f}") 
    
  })
  
  
  # generate figures for income tab: lollipop
  output$inc2 <- renderPlotly({
    
    # Make the dataframe
    local_income <- df2() %>% 
      group_by(locality, major_group) %>% 
      summarize(income = sum(income, na.rm=T)) %>% ungroup() %>% 
      mutate(major_group = fct_rev(major_group),
             income = income/1e6)
    
    # Make the lollipop chart
    p <- ggplot(local_income, aes(y=income, x=major_group, color=major_group, 
                                  label = paste0("$", round(income, 1), "M"))) +
      geom_segment(aes(y = 0, x = major_group, yend = income, xend = major_group), color = "grey50") +
      geom_point(size = 5) +
      geom_text(color = "white", size = 2) +
      scale_color_brewer(palette = "Paired") +
      labs(color = "Major Group", y = "", x = "") +
      scale_y_continuous(labels = unit_format(unit = "M")) +
      coord_flip() +
      facet_wrap(~ locality) + 
      guides(color = guide_legend(reverse=F)) +
      theme(axis.text.x = element_text(angle = 45), axis.text.y = element_blank())
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
  
  
  # assets figures ----
  # generate figures for asset tab: tree
  
  output$asset1 <- renderHighchart({
    
    # Make the dataframe
    local_assets <- df2() %>% 
      group_by(major_sector, major_category2) %>% 
      summarize(assets = sum(assets, na.rm=T)) %>% ungroup() %>% 
      mutate(major_sector = as.character(major_sector),
             major_category2 = as.character(major_category2),
             major_category2 = ifelse(major_category2 == "Education", "Education-Total",
                                      major_category2),
             major_category2 = ifelse(major_category2 == "Health", "Health-General", 
                                      major_category2),
             major_category2 = ifelse(major_category2 == "Arts, Culture", "Arts, Culture-Total",
                                      major_category2))
    
    # Make the tree map
    p <- treemap(local_assets,
                 index=c("major_sector", "major_category2"),
                 vSize="assets",
                 type="index",
                 vColor = "major_sector",
                 title = "",
                 palette = "Paired",
                 pdf(file = NULL)
    )
    
    hctreemap(p, allowDrillToNode = TRUE) %>% 
      hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Income: {point.value:,.0f}") 
    
  })
  
  
  # generate figures for assets tab: lollipop
  output$asset2 <- renderPlotly({
    
    # Make the dataframe
    local_assets <- df2() %>% 
      group_by(locality, major_group) %>% 
      summarize(assets = sum(assets, na.rm=T)) %>% ungroup() %>% 
      mutate(major_group = fct_rev(major_group),
             assets = assets/1e6)
    
    # Make the lollipop chart
    p <- ggplot(local_assets, aes(y=assets, x=major_group, color=major_group, 
                                  label = paste0("$", round(assets, 1), "M"))) +
      geom_segment(aes(y = 0, x = major_group, yend = assets, xend = major_group), color = "grey50") +
      geom_point(size = 5) +
      geom_text(color = "white", size = 2) +
      scale_color_brewer(palette = "Paired") +
      labs(color = "Major Group", y = "", x = "") +
      scale_y_continuous(labels = unit_format(unit = "M")) +
      coord_flip() +
      facet_wrap(~ locality) + 
      guides(color = guide_legend(reverse=F)) +
      theme(axis.text.x = element_text(angle = 45), axis.text.y = element_blank())
    
    ggplotly(p, tooltip = c("x", "y"))
    
  })
  
  
  # full data ----
  # generate data table for full data for review
  
  output$tbl_all <-  renderDT({
    datatable(local) %>% 
      DT::formatStyle(columns = colnames(local), fontSize = '90%') %>% 
      formatCurrency(c('income', 'assets', "ctotrev"), digits = 0)
  })
  
  # Downloadable csv of dataset
  output$downloadData2 <- downloadHandler(
    filename = "ngo.csv",
    content = function(file2) {
      write.csv(local, file2, row.names = FALSE)
    }
    
  )
  
}


# Run the app ----
shinyApp(ui = ui, server = server)


