library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(ggrepel)
library(shinycssloaders)

Postal <- read.csv("Postal_All.csv")

BreakScale <- function(x) sprintf("%.0f", x)
BreakScaleTwo <- function(x) sprintf("%.2f", x)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
  # Application title
  #titlePanel("Postal Results"),
  navbarPage("Postal",
             tabPanel("Individual Results Comparison", fluid = TRUE,
                      titlePanel("Individual Results Comparison"),
                      fluidRow(column(5,
                                      # Select which Athletes(s) to plot
                                      selectizeInput(inputId = "Athlete",
                                                     label = "Select Athlete(s):",
                                                     choices = levels(Postal$Name),
                                                     multiple = TRUE,
                                                     options = list(maxItems = 8, placeholder = 'Enter Athlete Last Name',
                                                                    onInitialize = I('function() { this.setValue(""); }')))
                                      #helpText("Tip: Enter athlete last name")
                      ),
                      column(5,
                             # Select which Clubs(s) to plot
                             selectizeInput(inputId = "Club",
                                            label = "Select Club(s):",
                                            choices = levels(Postal$Club),
                                            multiple = TRUE,
                                            options = list(maxItems = 8, placeholder = 'Enter Club Short Name',
                                                           onInitialize = I('function() { this.setValue(""); }')))
                             
                      )
                      # column(2,
                      #        #Show names
                      #        checkboxGroupInput(inputId = "show_Names",
                      #                     label = "Display:",
                      #                     choices = "Names",
                      #                     selected = "Names"))
                      
                      ),
                      helpText("Tip: Entering athlete names will add those athletes' results to the plot and table.  Entering club names will add results from all athletes representing that club to the plot and table.  These two functions may be combined by entering information in both fields."),
                      #plotOutput("AthletePlot"),
                      hr(),
                      withSpinner(plotOutput("TimeSeries")),
                      withSpinner(dataTableOutput("AthleteTable"))
                      
             ),
             tabPanel("Results By Age Group", fluid = TRUE,
                                     titlePanel("Results By Age Group"),
                                     fluidRow(column(3,
                                                     
                                                     # Select which Gender(s) to plot
                                                     checkboxGroupInput(inputId = "Gender",
                                                                        label = "Select Gender(s):",
                                                                        choices = c("Female" = "W", "Male" = "M"),
                                                                        selected = "W")
                                                     # selectizeInput(inputId = "Club",
                                                     #                label = "Select Club",
                                                     #                choices = levels(Postal$Club),
                                                     #                multiple = TRUE,
                                                     #                options = list(maxItems = 4, placeholder = 'Enter Club Short Name',
                                                     #                               onInitialize = I('function() { this.setValue(""); }'))),
                                                     # helpText("Club short name examples: OREG, DAM, SFGG")
                                                     ),
                                              
                                     column(6, offset = 2,
                                            # Select which Age Group(s) to plot
                                            selectizeInput(inputId = "AgeGroup",
                                                           label = "Select Age Group(s):",
                                                           choices = levels(Postal$Age_Group),
                                                           multiple = TRUE,
                                                           options = list(maxItems = 16, placeholder = 'Enter Age Group',
                                                                          onInitialize = I('function() { this.setValue(""); }'))),
                                            helpText("Tip: Age groups are formatted as 18-24, 25-29, 30-34, etc.")),
                                            hr()
                                            ),
                      
                                            
                                            
                                            #Select which Year(s) to plot
                                            selectizeInput(inputId = "Year",
                                                           label = "Select Year:",
                                                           choices = levels(as.factor(Postal$Year)),
                                                           multiple = FALSE,    
                                                           selected = "2020"),
                      hr(),

                                    
                                           withSpinner(plotOutput(outputId = "ResultsPlot")),
                                           hr(),
                                           
                        # hr(),
                        withSpinner(dataTableOutput(outputId = "ResultsTable"))
                      ),
             tabPanel("Club Results", fluid = TRUE,
                      titlePanel("Club Results"),
                      fluidRow(column(3,
                                      
                                      # Select which Gender(s) to plot
                                      radioButtons(inputId = "GenderClub",
                                                         label = "Select Gender(s):",
                                                         choices = c("Female" = "W", "Male" = "M", "Combined"),
                                                         selected = "W"),
                                      selectizeInput(inputId = "YearClub",
                                                     label = "Select Year:",
                                                     choices = levels(as.factor(Postal$Year)),
                                                     multiple = FALSE,    
                                                     selected = "2020")
                      ),
                      
                      column(6, offset = 2,
                             # # Select which Club Size(s) to plot
                             # checkboxGroupInput(inputId = "ClubSize",
                             #                label = "Select Club Size(s):",
                             #                choices = c("Small (25 or fewer swimmers)" = "S", "Medium (26-50 swimmers)" = "M", "Large (51-100 swimmers)" = "L", "Extra Large (More than 101 swimmers)" = "XL"),
                             #               selected = "S"
                             #                ),
                             # Set No. Athlete Range
                             fluidRow(column(5,
                                             numericInput(inputId = "SizesMin",
                                                       label = "Minimum No. Athletes:",
                                                       value = 1)
                             ),
                             column(5, ofset = 3,
                                    numericInput(inputId = "SizesMax",
                                              label = "Max No. Athletes:",
                                              value = 25
                                              ))
                             ),
                             h5(p("USMS catagorizes club size for the purposes of the e-Postal as:")),
                                           h6(p("Small (25 or fewer swimmers)"),
                                           p("Medium (26-50 swimmers)"),
                                           p("Large (51-100 swimmers"),
                                           p("Extra Large (More than 101 swimmers)"))
                             )),
                      hr(),
                      dataTableOutput("ClubTable")
                        
                      
                      ),
             # tabPanel("National Records", fluid = TRUE,
             #          selectizeInput(inputId = "YearRecord",
             #                         label = "Select Year:",
             #                         choices = levels(as.factor(Postal$Year)),
             #                         multiple = FALSE,    
             #                         selected = "2019"),
             #          h4("The following athletes broke National Records for their age group in the USMS 1 Hour ePostal.  Congratualtions to them all!"),
             #          dataTableOutput("RecordTable")
             #          ),
             tabPanel("About", fluid = TRUE,
                      fluidRow(
                        column(6,
                               br(),
                               h4(p("About the Project")),
                               h5(p("The ePostal is an annual event in US Masters Swimming where participants all over the country swim as many yards as possible in one hour and then email in their results. This project is intended to be an interesting and informative look at those ePostal results.  I hope you find them encouraging, motivating and inspiring.")),
                               br(),
                               h5(p("I began this project as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, the seed of which I collected from USMS as sourced below.")),
                               br(),
                               h5(p("Any comments or questions are welcome at gpilgrim2607@gmail.com")),
                               hr(),
                               h5("Sources:"),
                               h6(      
                                 p("Swimming information from ",
                                   a("USMS", 
                                     href = "https://www.usms.org/events/national-championships/epostal-national-championships/2018-epostal-national-championships"))),
                               h5("Built with",
                                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png", height = "30px"),
                                  "by",
                                  img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png", height = "30px"),
                                  ".")
                        )
                        ,
                        column(6,
                               br(),
                               HTML('<img src="GregPicCrop.png", height="110px"    
          style="float:right"/>','<p style="color:black"></p>'),
                               h4(p("About the Author")),
                               h5(p("Greg is a former collegiate swimmer.  After completing his undergrad degree he joined USMS, earned a PhD in chemistry, and began officiating swimming at the high school and NCAA level.  He now swims with his local USMS team while also working as a scientist.")
                               ))
                        
                        
                      )
             )
            
             )
)



# Define server logic
server <- function(input, output) {
  
  Postal_subset <- reactive({
    req(input$AgeGroup)
    req(input$Gender)
    filter(Postal, Gender %in% input$Gender) %>%
      filter(Age_Group %in% input$AgeGroup) %>% 
      filter(Year %in% input$Year)
      
  }) 
  
  
  Postal_Club <- reactive({
    req(input$GenderClub)
    if(input$GenderClub == "W"){
    filter(Postal, Gender == "W",
           Club_Count_Female >= input$SizesMin,
           Club_Count_Female <= input$SizesMax, 
           Year %in% input$YearClub)
    } else if (input$GenderClub == "M"){
      filter(Postal, Gender == "M",
             Club_Count_Male >= input$SizesMin,
             Club_Count_Male <= input$SizesMax, 
             Year %in% input$YearClub)
    } else {
      filter(Postal, Club_Count >= input$SizesMin,
             Club_Count <= input$SizesMax,
             Year %in% input$YearClub)
    }
  })
  
  Postal_Athlete <- reactive({
    req(sum(length(input$Athlete), length(input$Club)) >= 1)
    filter(Postal, Name %in% input$Athlete | Club %in% input$Club)
  }) 
  
  Postal_Athlete_First <- reactive({
    #req(input$Athlete | input$Club)
    filter(Postal, Name %in% input$Athlete | Club %in% input$Club) %>% 
    group_by(Name) %>% 
    filter(Year == min(Year))
  }) 
  
  Postal_Record <- reactive({
    filter(Postal,
           National_Record == "Y",
           Year == input$YearRecord)
  }) 
  
  output$TimeSeries <- renderPlot({
    ggplot(data = Postal_Athlete(), aes(y = Distance, x = Year, color = Perm_ID)) +
      geom_line(alpha = 0.5) +
      #Can add if statement linking to show_Names input above for toggling
      geom_text_repel(data = Postal_Athlete_First(), aes(x = Year, y = Distance, label = as.character(Name))) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      guides(color = FALSE) +
      scale_x_continuous(breaks = c(2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020)) +
      if (length(Postal_Athlete()) == 1) {
        scale_y_continuous(labels = BreakScaleTwo)
      } else {
        scale_y_continuous(labels = BreakScale) 
        }
      
  })
  
   output$ResultsPlot <- renderPlot({
     ggplot(data = Postal_subset(), aes(y = Distance, x = Age_Group, fill = Gender)) +
       geom_boxplot(aes(alpha = 0.6), position = position_dodge(1)) +
       geom_dotplot(aes(color =  as.factor(Gender), fill = as.factor(Gender)), binaxis = "y", stackdir='center',
                    position=position_dodge(1), binwidth = 25, dotsize = 2) +
       #geom_jitter(position = position_jitter(width = 0.1), alpha = 0.5) +
       theme_minimal() +
       labs(x = NULL, y = NULL) +
       theme(legend.title=element_blank(), panel.grid.major = element_line(color = "white"), panel.grid.minor = element_line(color = "white")) +
       theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
       theme(legend.position="top", legend.key.size = unit(3, "line"),
             axis.text.x  = element_text(angle = 45, size = 14),
             axis.text.y  = element_text(size = 14)) +
       scale_fill_manual(values = c("W" = "maroon", "M" = "seagreen"), labels = c("W" = "Female", "M" = "Male")) +
       scale_color_manual(values = c("W" = "maroon", "M" = "seagreen")) +
       guides(color = FALSE, alpha = FALSE)
       #labs(x = "Age Group", y = "Distace (y)")
   })
   
   output$ResultsTable<-DT::renderDataTable({
     DT::datatable(Postal_subset()[,c("Relative_Place", "Age_Group", "Name", "Club", "Distance", "Avg_Speed_50", "Age")], 
                   colnames = c("Place" = "Relative_Place", "Age Group" = "Age_Group", "Average 50 Time" = "Avg_Speed_50"),
                   rownames = FALSE,
                   options = list(order = list(4, 'desc'),
                                  columnDefs = list(list(className = 'dt-center', targets = 0:6))
                                  )
     )
   })
   
   output$ClubTable<-DT::renderDataTable({
     if(input$GenderClub == "W"){
     DT::datatable(unique(Postal_Club()[,c("Female_Rank", "Club", "Total_Distance_Female", "Club_Count_Female", "Avg_Distance_Female", "Avg_Speed_50_Club_Female", "Avg_Age_Club_Female", "Club_Size_Female")]), 
                   colnames = c("Ranking"= "Female_Rank", "Total Distance" = "Total_Distance_Female", "No. of Athletes" = "Club_Count_Female", "Average Distance Per Athlete" = "Avg_Distance_Female", "Club Average 50 Pace" = "Avg_Speed_50_Club_Female", "Club Average Age" = "Avg_Age_Club_Female", "Club Size" = "Club_Size_Female"),
                   rownames = FALSE,
                   options = list(order = list(0, 'asc'),
                                  columnDefs = list(list(className = 'dt-center', targets = 0:7)))
     )
       }
     else if(input$GenderClub == "M"){
       DT::datatable(unique(Postal_Club()[,c("Male_Rank", "Club", "Total_Distance_Male", "Club_Count_Male", "Avg_Distance_Male", "Avg_Speed_50_Club_Male", "Avg_Age_Club_Male", "Club_Size_Male")]),
                     colnames = c("Ranking"= "Male_Rank", "Total Distance" = "Total_Distance_Male", "No. of Athletes" = "Club_Count_Male", "Average Distance Per Athlete" = "Avg_Distance_Male", "Club Average 50 Pace" = "Avg_Speed_50_Club_Male", "Club Average Age" = "Avg_Age_Club_Male", "Club Size" = "Club_Size_Male"),
                     rownames = FALSE,
                     options = list(order = list(0, 'asc'),
                                    columnDefs = list(list(className = 'dt-center', targets = 0:7)))
       )}
     else{
       DT::datatable(unique(Postal_Club()[,c("Combined_Rank", "Club", "Total_Distance_Combined", "Club_Count", "Avg_Distance_Combined", "Avg_Speed_50_Club_Combined", "Avg_Age_Club", "Club_Size_Combined")]),
                     colnames = c("Ranking"= "Combined_Rank", "Total Distance" = "Total_Distance_Combined", "No. of Athletes" = "Club_Count", "Average Distance Per Athlete" = "Avg_Distance_Combined", "Club Average 50 Pace" = "Avg_Speed_50_Club_Combined", "Club Average Age" = "Avg_Age_Club", "Club Size" = "Club_Size_Combined"),
                     rownames = FALSE,
                     options = list(order = list(0, 'asc'),
                                    columnDefs = list(list(className = 'dt-center', targets = 0:7)))
     )}
   })
   
   output$AthleteTable<-DT::renderDataTable({
     DT::datatable(Postal_Athlete()[,c("Name", "Club", "Year", "Distance", "Avg_Speed_50", "Relative_Place", "Gender", "Age", "Age_Group")], 
                   colnames = c("Age Group"= "Age_Group", "Relative Place" = "Relative_Place",  "Average 50 Time" = "Avg_Speed_50"),
                   rownames = FALSE,
                   options = list(order = list(3, 'desc'),
                                  columnDefs = list(list(className = 'dt-center', targets = 1:7))
                   )
     )
   })
   
   output$RecordTable<-DT::renderDataTable({
     DT::datatable(Postal_Record()[,c("Name", "Club", "Distance", "Avg_Speed_50", "Relative_Place", "Gender", "Age", "Age_Group", "National_Record")], 
                   colnames = c("Age Group"= "Age_Group", "Relative Place" = "Relative_Place",  "Average 50 Time" = "Avg_Speed_50", "National Record" = "National_Record"),
                   rownames = FALSE,
                   options = list(order = list(2, 'desc'),
                                  columnDefs = list(list(className = 'dt-center', targets = 1:8))
                   )
     )
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

