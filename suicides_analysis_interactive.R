#Load libraries
#install.packages("rstudioapi", dependencies = TRUE)
#install.packages("readxl", dependencies = TRUE)
#install.packages("ggplot2", dependencies = TRUE)
#install.packages("dplyr", dependencies = TRUE)
#install.packages("tidyr", dependencies = TRUE)
#install.packages("directlabels", dependencies = TRUE)
#install.packages("ggimage", dependencies = TRUE)

library(shiny)
library(rstudioapi)
library(readxl)
library(dplyr)
library(tidyr)
library(plyr)
library(ggplot2)
library(directlabels)
library(data.table)
library(reshape2)
library(ggimage)
library('grid')
library(RColorBrewer)
library(maptools)
library(ggplot2)

theme_var =   theme(plot.title = element_text(hjust = 0.5), 
                    panel.background = element_rect(fill = '#f1f1f1', colour = 'white'),
                    legend.position = c(.90,.85),
                    panel.grid.minor = element_blank(),
                    axis.text.y=element_blank(),
                    axis.ticks.y=element_blank(),
                    axis.text=element_text(size=14))


# Set current directory
currentDir <- dirname(getActiveDocumentContext()$path)
setwd(currentDir)

# Get europe boarders for map charts
# http://thematicmapping.org/downloads/TM_WORLD_BORDERS_SIMPL-0.3.zip


get_eurostat_table <- function(path,sheet,columns_numeric,columns,skip=0) {
  df <- read_excel(path,sheet=sheet, skip = skip,col_types = "text")
  df <- pivot_longer(df, cols = starts_with("2"))
  colnames(df) <- columns
  df[columns_numeric] <- sapply(df[columns_numeric],function(x) as.numeric(as.character(x)))
  return(df)
}


# Country main table
df_country_metadata <- read.csv("data/country_metadata.csv", header =TRUE)
colnames(df_country_metadata) <- c("country","country_name","iso_name","lat_center","long_center")



# sucide_rate_total
df_suicide_rate_total = get_eurostat_table(path="data/eurostat_suicide_rate_by_sex.xlsx",
                                           sheet='Sheet 1',
                                           columns_numeric = c("year", "sucide_rate_total"),
                                           columns=c("country", "year", "sucide_rate_total"),
                                           skip=10)


df_suicide_rate_male = get_eurostat_table(path="data/eurostat_suicide_rate_by_sex.xlsx",
                                           sheet='Sheet 2',
                                           columns_numeric = c("year", "male"),
                                           columns=c("country", "year", "male"),
                                           skip=10)


df_suicide_rate_female = get_eurostat_table(path="data/eurostat_suicide_rate_by_sex.xlsx",
                                          sheet='Sheet 3',
                                          columns_numeric = c("year", "female"),
                                          columns=c("country", "year", "female"),
                                          skip=10)



# Combine datasets - select only specific counties in EU in years 2011 - 2015
dfs = list(df_suicide_rate_total,df_suicide_rate_female,df_suicide_rate_male)
dfs_new = join_all(dfs, by=c("country","year"), type='left')
df_final = join_all(list(df_country_metadata,dfs_new), by="country", type='left')
df_final = df_final[df_final$year %in% c("2011","2012","2013","2014","2015"),]


countries_labels = unique(df_final$country_name)

df_filal_sex <- melt(df_final[c("male","female","year","country_name")], id=c("year","country_name"))
colnames(df_filal_sex) <- c("year","country_name","sex","value")


expandy = function(vec) {
  max.val = max(vec, na.rm=TRUE)
  min.val = min(vec, na.rm=TRUE)
  expand_limits(y=c(min.val, max.val+(max.val-min.val)/2))
}


ui <- fluidPage(
  
  # App title ----
  titlePanel("Suicide Rates per country"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId="country_name",label="Choose Country",choices = countries_labels,
                  selected = "Blue",multiple = F)
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "barPlot"),
      splitLayout(cellWidths = c("50%", "50%"), plotOutput("barPlotM"), plotOutput("barPlotF")),
      plotOutput(outputId = "barPlotMF")
      
    )
  )
)


server <- function(input, output){
  dataset <- reactive({
    df_final[df_final$country_name==input$country_name,]
  })

  dataset2 <- reactive({
    df_filal_sex[df_filal_sex$country_name==input$country_name,]
  })
  
    output$barPlot <- renderPlot({
    
    d<- dataset()
    p<-ggplot(d,aes(x = year, y = sucide_rate_total)) + 
      geom_line(stat = "identity") +
      geom_text(aes(label = sucide_rate_total),vjust = -2, hjust = 0.5,color="#515151",size=5) +
      geom_line(color = "#828282", size = 2) +
      geom_point(color = "#828282", size = 7)+
      theme_var+
      labs( title = paste0("Suicide Rate,",input$country_name ," 2011-2015")) +
      ylab("sucide rate (per 100k population)") +
      xlab("year")  +
      expandy(d$sucide_rate_total)
    p
  })
    
    
    output$barPlotM <- renderPlot({
      
      d<- dataset()
      p<-ggplot(d,aes(x = year, y = male)) + 
        geom_line(stat = "identity") +
        geom_text(aes(label = male),vjust = -2, hjust = 0.5,color="#515151",size=5) +
        geom_line(color = "#79B4B7", size = 2) +
        geom_point(color = "#79B4B7", size = 7)+
        theme_var+
        labs( title = "Suicide Rate Male") +
        ylab("sucide rate (per 100k population)") +
        xlab("year") +
        expandy(d$male)
      
      p
    })
    
    output$barPlotF <- renderPlot({
      
      d<- dataset()
      p<-ggplot(d,aes(x = year, y = female)) + 
        geom_line(stat = "identity") +
        geom_text(aes(label = female),vjust = -2, hjust = 0.5,color="#515151",size=5) +
        geom_line(color = "#BEAEE2", size = 2) +
        geom_point(color = "#BEAEE2", size = 7)+
        theme_var+
        labs( title = "Suicide Rate Female") +
        ylab("sucide rate (per 100k population)") +
        xlab("year") +
        expandy(d$female)
      
      p
    })
    
    
    
    output$barPlotMF <- renderPlot({
      
      d<- dataset2()
      p<- ggplot(d, aes( x=year, y=value, group=sex, fill=sex )) + 
        geom_bar(stat="identity", width=.5, position = "dodge")   +
        scale_fill_manual(values=c("female"="#BEAEE2","male"="#79B4B7")) +
        geom_text(aes(label = value),vjust = -1, hjust = 0.5 ,size=5, position = position_dodge(width = .5)) +
        theme(plot.title = element_text(hjust = 0.5), 
              panel.background = element_rect(fill = '#f1f1f1', colour = 'white'),
              legend.position = c(.95,.85),
              panel.grid.minor = element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              axis.text=element_text(size=14)) +
        ylab("sucide rate (per 100k population)") +
        xlab("year") +
       labs( title = "Suicide Rate Male/ Female") + 
       expandy(d$value)
      
      p
    })
    
}

shinyApp(ui = ui, server = server)