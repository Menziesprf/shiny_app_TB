library(shiny)
library(shinyWidgets)
library(shinyBS)
library(shinythemes)

source("prep_script.R")

# Countries vector for selecter
countries_vect <- tb_top20 %>% 
  distinct(country) %>% 
  pull()

# Age brackets for selecter
age_brackets <- c("0-4", "5-14", "15-24", "25-34", "35-44", "45-54", "55-64", "65+" = "65plus")

# Info for popout button
data_info <- paste("The countries included are the 20 countries with the highest incidence of TB in 2019.",
                   "The WHO provide a &#39;low&#39;, &#39;high&#39; and &#39;best&#39; estimate. The &#39;best&#39; estimate has been used in this app.",
                   sep = "<br><br>")

ui <- fluidPage(
  theme = shinytheme("yeti"),
  titlePanel(h1("Estimated Incidence of TB in 2019", align = "center")),
  tabsetPanel(
    tabPanel("Home",
             br(),
      fluidRow(
          column(4,
                 pickerInput(inputId = "countries",
                             label = NULL,
                             choices = countries_vect,
                             selected = countries_vect,
                             multiple = T,
                             options = list(`actions-box` = TRUE,
                                            `none-selected-text` = "Countries",
                                            `selected-text-format` = "static",
                                            `size` = "auto")
                 ),
          ),
          column(3,
                 uiOutput("datainfo"),
                 offset = 1
          ),
          column(4,
                 tags$a("WHO Global TB Report and Data", 
                        href = "https://www.who.int/teams/global-tuberculosis-programme/data")
          )
        ),
        fluidRow(
          plotOutput(outputId = "tbplot")
      )
    ),
    tabPanel("Age and Sex",
             br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "sex",
                       label = "Sex:",
                       choices = c("Male" = "m",
                                   "Female" = "f"),
                       inline = T),
          checkboxGroupButtons(inputId = "age",
                               label = "Age Group(s):",
                               choices = age_brackets,
                               selected = age_brackets,
                               width = "70%")
        ),
        mainPanel(
          plotOutput(outputId = "tbplot_age_sex")
        )
      )
             
      
    ),
    tabPanel("Risk Factors",
             br(),
      sidebarLayout(
        sidebarPanel(
          radioButtons(inputId = "riskfactor",
                       label = "Risk Factor:",
                       choices = c("Total Incidence" = "all",
                                   "Alcohol abuse" = "alc",
                                   "Diabetes" = "dia",
                                   "HIV" = "hiv",
                                   "Smoking" = "smk",
                                   "Malnutrition" = "und"),
                       selected = "all"
          )
        ),
        mainPanel(
          plotOutput(outputId = "tbplot_riskfactors")
        )
             
      )
    )
  )
)


server <- function(input, output) {
  
  output$tbplot <- renderPlot({
    
    tb_top20 %>% 
      filter(age_group == "all",
             sex == "a",
             risk_factor == "all",
             country %in% input$countries) %>% 
      arrange(country) %>% 
      ggplot() +
      aes(x = country, y = best/1000) +
      geom_col(fill = "light blue") +
      labs(x = "Country", y = "Estimated Incidence (thousands)") +
      scale_y_continuous(n.breaks = 8) +
      theme(axis.text.x = element_text(angle = 90, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            panel.background = element_blank())
  })
  
  output$datainfo <- renderUI({
    popify(bsButton(inputId = "infobutton",
                    label = "Data Info"),
           title = "Data provided by WHO",
           content = data_info)
  })
  
  output$tbplot_age_sex <- renderPlot({
    
    tb_top20 %>% 
      filter(age_group  %in%  input$age,
             sex == input$sex,
             risk_factor == "all") %>% 
      ggplot() +
      aes(x = country, y = best/1000) +
      geom_col(fill = "light blue") +
      labs(x = "Country", y = "Estimated Incidence (thousands)") +
      scale_y_continuous(n.breaks = 8)  +
      theme(axis.text.x = element_text(angle = 90, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            panel.background = element_blank())
  })
  
  output$tbplot_riskfactors <- renderPlot({
    
    tb_top20 %>% 
      filter(sex == "a",
             risk_factor %in% input$riskfactor) %>% 
      ggplot() +
      aes(x = country, y = best/1000) +
      geom_col(fill = "light blue") +
      labs(x = "Country", y = "Estimated Incidence (thousands)") +
      scale_y_continuous(n.breaks = 8)  +
      theme(axis.text.x = element_text(angle = 90, size = 12),
            axis.text.y = element_text(size = 12),
            axis.title.x = element_text(size = 14, face = "bold"),
            axis.title.y = element_text(size = 14, face = "bold"),
            panel.background = element_blank(),
            ) 
  })
}

shinyApp(ui, server)