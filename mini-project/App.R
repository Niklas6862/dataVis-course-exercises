library(shiny)
library(tidyverse)
library(xfun)
library(rstatix)
library(shinyWidgets)
library(bslib)
library(shinythemes)
library(googlesheets4)
library(gsheet)
library(plotly)
library(styler)
library(lintr)
library(shinylogs)
library(DT)
library(shinydashboard)
library(shinyscreenshot)
library(ggforce)
library(readr)
library(ggtext)
library(stringr)

hospital_name ="Samaritan"

#source("C:/Users/nikl2/Documents/GitHub/Datavis/mini-project/utils/DataAggregation.R")

agg_data = agg_dataNum

ui = fluidPage(theme = bs_theme(version = 4),
  
  titlePanel(h1("Dashboard for Samaritan Hospital")),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar1",
      width = "200px",
      height = "800px",
      pickerInput(
        "gender_picker",
        h3("Gender"),
        choices = c("female", "male"),
        selected = "Female",
        multiple = FALSE
      ),
      pickerInput(
        "qi_picker",
        h3("Choose kpi"),
        choices = c("dnt_leq_60", "dnt_leq_45"),
        selected = "dnt_leq_60",
        multiple = FALSE
      )
    ),
    
    mainPanel(plotOutput("plot1"))
  ),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      id = "sidebar_2",
      width = "200px",
      height = "800px",
      pickerInput(
        "variable_picker",
        h3("Category"),
        choices = c("prenotification", "imaging_done", "stroke_type"),
        selected = "stroke_type"
      ),
      pickerInput(
        "qi_2_picker",
        h3("Subcategory"),
        choices = c(
          "intracerebral hemorrhage",
          "ischemic",
          "subarachnoid hemorrhage",
          "cerebral venous thrombosis",
          "transient ischemic",
          "stroke mimics"
        ),
        selected = "ischemic"
      )
    ),
    mainPanel(plotlyOutput("plot2"))
  ),
  hr(),
)  


server = function(input, output, session) {
  
  
  observeEvent(input$variable_picker, {
    if(input$variable_picker == "stroke_type"){
      updatePickerInput(session, "qi_2_picker", choices = c("intracerebral hemorrhage", "ischemic", "subarachnoid hemorrhage", "cerebral venous thrombosis", "transient ischemic", "stroke mimics"),
                        selected = "ischemic")
    }
    else if(input$variable_picker == "imaging_done"){
      updatePickerInput(session, "qi_2_picker", choices = c("TRUE", "FALSE"),
                        selected = "TRUE")
    }
    else if(input$variable_picker == "prenotification"){
      updatePickerInput(session, "qi_2_picker", choices = c("TRUE", "FALSE"),
                        selected = "TRUE")
    }  
    
  })
  
  
  
  output$plot1 = renderPlot({
    color_function = function(row){
      dia = "#00FFFF"
      gold="#D8C745"
      plat = "#D6DADA"
      if(input$qi_picker=="dnt_leq_60"){
        if(row["qual4Diamond"]=="1"){
          return(dia)
        }
        else if(row["qual4Gold"]=="1"){
          return(gold)
        }
        else{
          return("#FF0000")
          
        }
        
      }
      else{
        if(row["qual4Diamond"]=="1"){
          return(dia)
        }
        else if(row["qual4Platinum"]=="1"){
          return(plat)
        }
        else{
          return("#FF0000")
        }
      }
    }
    
    if(input$qi_picker=="dnt_leq_60"){
      y_label = "DNT ≤ 60 min"
      subtitle = HTML("The given angel award <span style='color:#00FFFF'>diamond</span> and
                                 <span style='color:#D8C745'>gold</span>, as well as
                                 <span style='color:#FF0000'>no awards</span>
                      ")
    }
    else{
      y_label = "DNT ≤ 45 min"
      subtitle = HTML("The given angel award <span style='color:#00FFFF'>diamond</span> and
                                 <span style='color:#D6DADA'>platinum</span>, as well as
                                 <span style='color:#FF0000'>no awards</span>
                      ")
    }
    
    award = function(row){
      if(row["is1stDiam"] == 1){
        return("First gold")
      }
      if(row["is1stPlat"]==1){
        return("First Platinum")
      }
      if(row["is1stGold"]==1){
        return("First Gold")
      }
      return("")
    }
    
    
    
    
    hosp_plot1_data = agg_data %>%
      filter(h_name==hospital_name, quarter=="all", subGroupVal == input$gender_picker, nameOfAggr == input$qi_picker)
    
    hosp_plot1_data$color = apply(hosp_plot1_data, MARGIN = 1, FUN = color_function)
    year_scales = unique(hosp_plot1_data$year)
    
    
    ggplot(data = hosp_plot1_data, aes(x=year, y=Value)) +
      geom_point(fill=hosp_plot1_data$color, size=10, color="black", pch=21) +
      scale_x_continuous(breaks=c(2016:2022), limits=c(2016,2022))+
      scale_y_continuous(limits = c(min(hosp_plot1_data$Value-5),max(hosp_plot1_data$Value+5)),
                         labels = scales::label_percent(scale = 1, accuracy = 1)) +
      theme_minimal() +
      theme(
        plot.title = element_markdown(color = "black", size = 30, face = "bold", hjust=0.5),
        plot.subtitle = element_markdown(color = "black", size = 15, face = "bold", hjust=0.5),
        axis.title.x = element_text(color = "black", size = 16, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, face = "plain"),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FFFAE9", color = "#FFFAE9"),
        plot.background = element_rect(fill = "#FFFAE9", color = "#FFFAE9"),
      )+
      xlab("Year") +
      ylab(sprintf("Percentage of %s", y_label)) +
      labs(
        title=sprintf("Yearly angel award of %s (%%) for %ss", y_label, input$gender_picker),
        subtitle = subtitle
        
      ) 
    
    
    
  }, height = 400, width = 1000)
  
  
  output$plot2 = renderPlotly({
    hosp_data_plot2 = agg_data %>%
      filter(h_name == hospital_name, quarter != "all", nameOfAggr == "door_to_needle", subGroup == input$variable_picker, subGroupVal == input$qi_2_picker)
    
    hosp_data_plot2 = hosp_data_plot2 %>% drop_na(Value)
    
    years = c("2017", "2018", "2019", "2020", "2021", "2022")
    quarters = c("Q1", "Q2", "Q3", "Q4")
    year_q = expand.grid(year = years, quarter = quarters)
    year_q$YQ = paste(year_q$year, year_q$quarter, sep = " ")
    
    hosp_data_plot2_1 = merge(year_q, hosp_data_plot2, by = "YQ", all = TRUE)
    
    hosp_data_plot2 = hosp_data_plot2_1 %>%
      pivot_longer(cols = c(C_Value, Value), names_to = "variable", values_to = "values")
    
    p = ggplot(hosp_data_plot2, aes(x = YQ, y = values, fill = variable)) +
      geom_col(position = "dodge", color = "black") +
      scale_fill_manual(values = c(C_Value = "#F7FF00", Value = "#0036FF")) +
      ylim(0, 50) +
      labs(
      title = ("Performance of Door to Needle Time for <span style='color:#0036FF'>Samaritan </span> <br> compared to <span style='color:#F7FF00'>Country average</span>"), 
      x = "Year-Quarter", 
      y = "Median time in minutes"
      
      ) +
      theme(
        plot.title = element_markdown(color = "black", size = 23, face = "bold", hjust = 0.5),
        axis.title.x = element_text(color = "black", size = 16, face = "plain"),
        axis.title.y = element_text(color = "black", size = 16, face = "plain"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "#FFFAE9", color = "#FFFAE9"),
        plot.background = element_rect(fill = "#FFFAE9", color = "#FFFAE9")
      )
  ggplotly(p, height=400, width=1000)
  })
  
}
shinyApp(ui = ui, server = server)


