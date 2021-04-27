library(shiny)
library(ggplot2)
library(dplyr)
library(DT)
library(ggrepel)
library(shinycssloaders)

# Import data
Postal <- read.csv("Postal_All.csv", stringsAsFactors = TRUE) %>% 
  select(-contains("X")) # remove row numbers column

# Define ranges to be used in inputs
Year_Min <- min(Postal$Year, na.rm = TRUE)
Year_Max <- max(Postal$Year, na.rm = TRUE)

Club_Size_Min <- min(Postal$Club_Count, na.rm = TRUE)
Club_Size_Max <- max(Postal$Club_Count, na.rm = TRUE)

# Define formatting functions used in plot labels
BreakScale <- function(x) sprintf("%.0f", x)
BreakScaleTwo <- function(x) sprintf("%.2f", x)

# Define UI
ui <- fluidPage(
   
  # Application title
  navbarPage("USMS ePostal",
             tabPanel("Individual Results Comparison", fluid = TRUE,
                      titlePanel("Individual Results Comparison"),
                      fluidRow(column(5,
                                      selectizeInput(
                                        # Select which athletes(s) to plot.
                                        # All athletes will be added to time series plot of postal distance (y) vs. year (x)
                                        # Can select both athlete name and club (all associated data will be added to plot)
                                        # Handled server side because the long list of athletes slows things down if done client side
                                        inputId = "Athlete",
                                        choices = NULL,
                                        label = "Select Athlete(s):",
                                        multiple = TRUE,
                                        options = list(
                                          maxItems = 8,
                                          placeholder = 'Enter Athlete Name',
                                          onInitialize = I('function() { this.setValue(""); }')
                                        )
                                      )
                      ),
                      column(5,
                             selectizeInput(
                               # Select which Clubs(s) to plot
                               # Select club name(s).  All athletes from those clubs will be
                               # added to time series plot of postal distance (y) vs. year (x)
                               # Can select both athlete name and club (all associated data will be added to plot)
                               inputId = "Club",
                               label = "Select Club(s):",
                               choices = levels(Postal$Club),
                               multiple = TRUE,
                               options = list(
                                 maxItems = 8,
                                 placeholder = 'Enter Club Short Name',
                                 onInitialize = I('function() { this.setValue(""); }')
                               ))
                             
                      )
                      ),
                      helpText("Tip: Entering athlete names will add those athletes' results to the plot and table.  Entering club names will add results from all athletes representing that club to the plot and table.  These two functions may be combined by entering information in both fields."),
                      hr(),
                      withSpinner(plotOutput("AthletePlot")),
                      withSpinner(dataTableOutput("AthleteTable")),
                      downloadButton(outputId = "DownloadAthleteData",
                                     label = "Download"),
                      helpText("Click to download the data from the above table as a .csv file."),
                      br()
                      
             ),
             tabPanel(
               # Contains results for age groups.
               # As box plots, by year
               "Results By Age Group", fluid = TRUE,
                      titlePanel("Results By Age Group"), 
                      fluidRow(
                        column(
                          3,
                          # Select which Gender(s) to plot
                          checkboxGroupInput(
                            inputId = "Gender",
                            label = "Select Gender(s):",
                            choices = c("Female" = "W", "Male" = "M"),
                            selected = "W"
                          )
                        ),
                        column(
                          6,
                          offset = 2,
                          # Select which Age Group(s) to plot
                          selectizeInput(
                            inputId = "AgeGroup",
                            label = "Select Age Group(s):",
                            choices = levels(Postal$Age_Group),
                            multiple = TRUE,
                            options = list(
                              maxItems = 16,
                              placeholder = 'Enter Age Group',
                              onInitialize = I('function() { this.setValue(""); }')
                            )
                          ),
                          helpText("Tip: Age groups are formatted as 18-24, 25-29, 30-34, etc.")
                        ),
                        hr()
                      ),
                      # Select which Year(s) to plot
                      selectizeInput(
                        inputId = "Year",
                        label = "Select Year:",
                        choices = levels(as.factor(Postal$Year)),
                        multiple = FALSE,
                        selected = "2021"
                      ),
               hr(),
               withSpinner(plotOutput(outputId = "AgeGroupPlot")),
               hr(),
               withSpinner(dataTableOutput(outputId = "AgeGroupTable")),
               downloadButton(outputId = "DownloadAgeGroupData",
                              label = "Download"),
               helpText("Click to download the data from the above table as a .csv file."),
               br()
             ), 
             tabPanel(
               # Panel contains results by club.
               # Can select club and view results by gender and year.
               # Will generate table that displays total distance, avg split etc. for selected clubs.
               "Club Results",
               fluid = TRUE,
               titlePanel("Club Results"),
               fluidRow(
                 column(
                   3,
                   radioButtons(
                     # Select which Gender(s) to plot
                     inputId = "GenderClub",
                     label = "Select Gender(s):",
                     choices = c("Female" = "W", "Male" = "M", "Combined"),
                     selected = "W"
                   ),
                   selectizeInput(
                     # Select which Years(s) to plot
                     inputId = "YearClub",
                     label = "Select Year:",
                     choices = levels(as.factor(Postal$Year)),
                     multiple = FALSE,
                     selected = "2021"
                   )
                 ),
                 
                 column(6, offset = 2,
                        fluidRow(
                          column(
                            5,
                            numericInput(
                              # minimum number of athletes in club to consider
                              inputId = "SizesMin",
                              label = "Minimum No. Athletes:",
                              value = Club_Size_Min
                            )
                          ),
                          column(
                            5,
                            ofset = 3,
                            numericInput(
                              # maximum number of athletes in club to consider
                              inputId = "SizesMax",
                              label = "Max No. Athletes:",
                              value = Club_Size_Max
                            )
                          )
                        ),
                        h5(
                          p("USMS catagorizes club size for the purposes of the e-Postal as:")
                        ),
                        h6(
                          p("Small (25 or fewer swimmers)"),
                          p("Medium (26-50 swimmers)"),
                          p("Large (51-100 swimmers"),
                          p("Extra Large (More than 101 swimmers)")
                        ))
               ),
               hr(),
               dataTableOutput("ClubTable"),
               downloadButton(outputId = "DownloadClubData",
                              label = "Download"),
               helpText("Click to download the data from the above table as a .csv file."),
               br()
             ), 
             tabPanel(
               # Brief discription of project, author
               "About", fluid = TRUE,
                      fluidRow(
                        column(6,
                               br(),
                               h4(p("About the Project")),
                               h5(p("The ePostal is an annual event in US Masters Swimming where participants all over the country swim as many yards as possible in one hour and then email in their results. This project is intended to be an interesting and informative look at those ePostal results.  I hope you find them encouraging, motivating and inspiring.")),
                               br(),
                               h5(p("I began this project as an attempt to combine my interest in swimming with a need to practice R, a programming language used primarily for analyzing and reporting data.  It has two components.  The first is this app, which queries a dataset to return information in the form of plots, data tables etc.  The second is the dataset itself, the seed of which I collected from USMS as sourced below.")),
                               br(),
                               h5(p("I write about R and Swimming reguarly ", a("here", href = "https://pilgrim.netlify.app/"))),
                               br(),
                               h5(p("Sourcecode for this project is available on ", a("Github", href = "https://github.com/gpilgrim2670/MastersPostal"))),
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
                               h4(p("About the Author")),
                               HTML('<img src="GregPicCrop.png", height="110px"    
          style="float:right"/>','<p style="color:black"></p>'),
                               h5(p("Greg is a former collegiate swimmer.  After completing his undergrad degree he joined USMS, earned a PhD in chemistry, and began officiating swimming at the high school and NCAA level.  He now swims with his local USMS team while also working as a scientist.")
                               ),
                               hr())
                        
                        
                      )
             ),
             tabPanel("Data",
                      # contains key for column values in Postal
                      # allows download of entire Postal dataset
                      br(),
                      downloadButton(outputId = "DownloadPostal",
                                     label = "Download"),
                      helpText("Click to download all the data from this app as a .csv file."),
                      hr(),
                      h3(p("Glossary")),
                      hr(),
                      h4(p("The following values refer to the athlete listed in 'Name'")),
                      br(),
                      h5(p(strong("Place:"), "Order of finish for an athlete in a given age group, gender and year")),
                      h5(p(strong("Name:"), "Athlete name as First Last")),
                      h5(p(strong("Age:"), "Athlete age in years")),
                      h5(p(strong("Distance:"), "Yards athlete swam in one hour")),
                      h5(p(strong("Club:"), "Name of organization/team athlete represents")),
                      h5(p(strong("Gender:"), "Athlete gender, 'M' for men, 'W' for women.  Competition is within age group and gender")),
                      h5(p(strong("Year:"), "When event took place")),
                      h5(p(strong("Age_Group:"), "USMS age categories.  Competition is within age group and gender")),
                      h5(p(strong("Avg_Split_50:"), "Average time to swim 50 yards, in seconds")),
                      h5(p(strong("Relative_Place:"), "Athlete's place within their gender and age group for a given year.  E.g. '4 of 23'.")),
                      h5(p(strong("USMS_ID:"), "An athlete's United States Masters Swimming identification number for a given year.  Changes year to year, not present prior to 2011.")),
                      h5(p(strong("Perm_ID:"), "A 5 character alphanumeric string that is the permanent portion of an athlete's USMS_ID.  Used to identify athletes across years in the event of name changes.  If no USMS_ID is present a randomly generated 6 character alphabetic string is used instead.")),
                      h5(p(strong("National_Record:"), "Did the athlete set a national record for their age group and gender with this particular swim?  Either (Y)es or (N)o.")),
                      hr(),
                      h4(p("The following values refer to the Club listed in 'Club'")),
                      br(),
                      h5(p(strong("Club_Count:"), "Number of athletes in Club")),
                      h5(p(strong("Club_Count_Male:"), "Number of male athletes in Club")),
                      h5(p(strong("Club_Count_Female:"), "Number of female athletes in Club")),
                      h5(p(strong("Club_Size_Combined:"), "USMS category for club size based on total number of athletes competing.  Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge")),
                      h5(p(strong("Club_Size_Male:"), "USMS category for club size based on total number of male athletes competing. Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge")),
                      h5(p(strong("Club_Size_Female:"), "USMS category for club size based on total number of female athletes competing. Values are (s)mall, (M)edium, (L)arge and e(x)tra (L)arge")),
                      h5(p(strong("Total_Distance_Combined:"), "Number of yards swam by all members of a club competing in the ePostal in a given year")),
                      h5(p(strong("Total_Distance_Male:"), "Number of yards swam by all male members of a club competing in the ePostal in a given year")),
                      h5(p(strong("Total_Distance_Female:"), "Number of yards swam by all female members of a club competing in the ePostal in a given year")),
                      h5(p(strong("Avg_Age_Club:"), "Average age in years of all club members competing in the ePostal in a given year")),
                      h5(p(strong("Avg_Age_Male:"), "Average age in years of all male club members competing in the ePostal in a given year")),
                      h5(p(strong("Avg_Age_Female:"), "Average age in years of all female club members competing in the ePostal in a given year")),
                      h5(p(strong("Combined_Rank:"), "Order of finish based on distance swam in a given year for a club within its appropriate club size")),
                      h5(p(strong("Male_Rank:"), "Order of finish based on distance swam by men in a given year for a club within its appropriate club size")),
                      h5(p(strong("Female_Rank:"), "Order of finish based on distance swam by women in a given year for a club within its appropriate club size")),
                      h5(p(strong("Avg_Distance_Combined:"), "Average number of yards swam by all athletes in a club in a given year")),
                      h5(p(strong("Avg_Distance_Male:"), "Average number of yards swam by all male athletes in a club in a given year")),
                      h5(p(strong("Avg_Distance_Female:"), "Average number of yards swam by all female athletes in a club in a given year")),
                      h5(p(strong("Avg_Split_50_Club_Combined:"), "Average number of seconds per 50 yards for all athletes in a club in a given year")),
                      h5(p(strong("Avg_Split_50_Club_Male:"), "Average number of seconds per 50 yards for all male athletes in a club in a given year")),
                      h5(p(strong("Avg_Split_50_Club_Female:"), "Average number of seconds per 50 yards for all female athletes in a club in a given year"))
                      )
            
             )
)



# Define server logic
server <- function(input, output) {
  
  ### "Individual Results Comparison" Tab Panel ###
  
  # Server side code for selecting athletes to add to time series plot
  # in first pane
  updateSelectizeInput(
    inputId = "Athlete",
    choices = levels(Postal$Name),
    server = TRUE,
  )
  
  # build data frame for individual athlete results based on selections
  Postal_Athlete <- reactive({
    req(sum(length(input$Athlete), length(input$Club)) >= 1)
    Postal %>%
      filter(Name %in% input$Athlete | Club %in% input$Club)
  }) 
  
  # used to add name to plot for first instance of an athlete
  Postal_Athlete_First <- reactive({
    #req(input$Athlete | input$Club)
    Postal %>%
      filter(Name %in% input$Athlete | Club %in% input$Club) %>%
      group_by(Name) %>%
      filter(Year == min(Year))
  }) 
  
  # all athletes or clubs selected in either input$Athlete or input$Club
  # in a timeseries plot
  output$AthletePlot <- renderPlot({
    ggplot(data = Postal_Athlete(), aes(y = Distance, x = Year, color = Perm_ID)) +
      geom_line(alpha = 0.5) +
      geom_text_repel(data = Postal_Athlete_First(), aes(
        x = Year,
        y = Distance,
        label = as.character(Name)
      )) +
      geom_point(size = 3) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      guides(color = FALSE) +
      if (length(Postal_Athlete()) == 1) {
        scale_y_continuous(labels = BreakScaleTwo)
      } else {
        scale_y_continuous(labels = BreakScale)
      }
    
  })
  
  # Results table for all athletes/clubs covered by either input$Athlete or input$Club
  output$AthleteTable <- DT::renderDataTable({
    DT::datatable(
      Postal_Athlete()[, c(
        "Name",
        "Club",
        "Year",
        "Distance",
        "Avg_Split_50",
        "Relative_Place",
        "Gender",
        "Age",
        "Age_Group"
      )],
      colnames = c(
        "Age Group" = "Age_Group",
        "Relative Place" = "Relative_Place",
        "Average 50 Time" = "Avg_Split_50"
      ),
      rownames = FALSE,
      options = list(order = list(3, 'desc'),
                     columnDefs = list(
                       list(className = 'dt-center', targets = 1:7)
                     ))
    )
  })
  
  # allow .csv file to be downloaded
  output$DownloadAthleteData <- downloadHandler(
    filename = "athlete_data.csv",
    content = function(file) {
      write.csv(Postal_Athlete(), file, row.names = FALSE)
    }
  )
  
  
  #### Age Group Results tab panel ###
  # subset by input$AgeGroup and $input$Gender and input$Year
  # data used to build boxplot and results table
  Postal_AgeGroup <- reactive({
    req(input$AgeGroup)
    req(input$Gender)
    Postal %>% 
      filter(Gender %in% input$Gender) %>%
      filter(Age_Group %in% input$AgeGroup) %>% 
      filter(Year %in% input$Year)
    
  }) 
  
  
  # boxplot for age group results
  output$AgeGroupPlot <- renderPlot({
    ggplot(data = Postal_AgeGroup(), aes(y = Distance, x = Age_Group, fill = Gender)) +
      geom_boxplot(aes(alpha = 0.6), position = position_dodge(1)) +
      geom_dotplot(
        aes(color =  as.factor(Gender), fill = as.factor(Gender)),
        binaxis = "y",
        stackdir = 'center',
        position = position_dodge(1),
        binwidth = 25,
        dotsize = 2
      ) +
      theme_minimal() +
      labs(x = NULL, y = NULL) +
      theme(
        legend.title = element_blank(),
        panel.grid.major = element_line(color = "white"),
        panel.grid.minor = element_line(color = "white")
      ) +
      theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
      theme(
        legend.position = "top",
        legend.key.size = unit(3, "line"),
        axis.text.x  = element_text(angle = 45, size = 14),
        axis.text.y  = element_text(size = 14)
      ) +
      scale_fill_manual(
        values = c("W" = "maroon", "M" = "seagreen"),
        labels = c("W" = "Female", "M" = "Male")
      ) +
      scale_color_manual(values = c("W" = "maroon", "M" = "seagreen")) +
      guides(color = FALSE, alpha = FALSE)
  })
  
  # age group results table
  output$AgeGroupTable <- DT::renderDataTable({
    DT::datatable(
      Postal_AgeGroup()[, c("Relative_Place",
                          "Age_Group",
                          "Name",
                          "Club",
                          "Distance",
                          "Avg_Split_50",
                          "Age")],
      colnames = c(
        "Place" = "Relative_Place",
        "Age Group" = "Age_Group",
        "Average 50 Time" = "Avg_Split_50"
      ),
      rownames = FALSE,
      options = list(order = list(4, 'desc'),
                     columnDefs = list(
                       list(className = 'dt-center', targets = 0:6)
                     ))
    )
  })
  
  # allow .csv file to be downloaded
  output$DownloadAgeGroupData <- downloadHandler(
    filename = "agegroup_data.csv",
    content = function(file) {
      write.csv(Postal_AgeGroup(), file, row.names = FALSE)
    }
  )
  
  ### "Club Results" Tab Panel ###
  # Builds results for club pane
  # Filters data based on gender and corresponding club size
  Postal_Club <- reactive({
    req(input$GenderClub)
    if (input$GenderClub == "W") {
      Postal %>%
        filter(
          Gender == "W",
          between(Club_Count_Female, input$SizesMin, input$SizesMax),
          Year %in% input$YearClub
        )
    } else if (input$GenderClub == "M") {
      Postal %>%
        filter(
          Gender == "M",
          between(Club_Count_Male, input$SizesMin, input$SizesMax),
          Year %in% input$YearClub
        )
    } else {
      Postal %>%
        filter(
          between(Club_Count, input$SizesMin, input$SizesMax),
          Year %in% input$YearClub
        )
    }
  })
  
  # club results table
  output$ClubTable <- DT::renderDataTable({
    if (input$GenderClub == "W") {
      DT::datatable(
        unique(Postal_Club()[, c(
          "Female_Rank",
          "Club",
          "Total_Distance_Female",
          "Club_Count_Female",
          "Avg_Distance_Female",
          "Avg_Split_50_Club_Female",
          "Avg_Age_Club_Female",
          "Club_Size_Female"
        )]),
        colnames = c(
          "Ranking" = "Female_Rank",
          "Total Distance" = "Total_Distance_Female",
          "No. of Athletes" = "Club_Count_Female",
          "Average Distance Per Athlete" = "Avg_Distance_Female",
          "Club Average 50 Pace" = "Avg_Split_50_Club_Female",
          "Club Average Age" = "Avg_Age_Club_Female",
          "Club Size" = "Club_Size_Female"
        ),
        rownames = FALSE,
        options = list(order = list(0, 'asc'),
                       columnDefs = list(
                         list(className = 'dt-center', targets = 0:7)
                       ))
      )
    }
    else if (input$GenderClub == "M") {
      DT::datatable(
        unique(Postal_Club()[, c(
          "Male_Rank",
          "Club",
          "Total_Distance_Male",
          "Club_Count_Male",
          "Avg_Distance_Male",
          "Avg_Split_50_Club_Male",
          "Avg_Age_Club_Male",
          "Club_Size_Male"
        )]),
        colnames = c(
          "Ranking" = "Male_Rank",
          "Total Distance" = "Total_Distance_Male",
          "No. of Athletes" = "Club_Count_Male",
          "Average Distance Per Athlete" = "Avg_Distance_Male",
          "Club Average 50 Pace" = "Avg_Split_50_Club_Male",
          "Club Average Age" = "Avg_Age_Club_Male",
          "Club Size" = "Club_Size_Male"
        ),
        rownames = FALSE,
        options = list(order = list(0, 'asc'),
                       columnDefs = list(
                         list(className = 'dt-center', targets = 0:7)
                       ))
      )
    }
    else{
      DT::datatable(
        unique(Postal_Club()[, c(
          "Combined_Rank",
          "Club",
          "Total_Distance_Combined",
          "Club_Count",
          "Avg_Distance_Combined",
          "Avg_Split_50_Club_Combined",
          "Avg_Age_Club",
          "Club_Size_Combined"
        )]),
        colnames = c(
          "Ranking" = "Combined_Rank",
          "Total Distance" = "Total_Distance_Combined",
          "No. of Athletes" = "Club_Count",
          "Average Distance Per Athlete" = "Avg_Distance_Combined",
          "Club Average 50 Pace" = "Avg_Split_50_Club_Combined",
          "Club Average Age" = "Avg_Age_Club",
          "Club Size" = "Club_Size_Combined"
        ),
        rownames = FALSE,
        options = list(order = list(0, 'asc'),
                       columnDefs = list(
                         list(className = 'dt-center', targets = 0:7)
                       ))
      )
    }
  })
  
  # allow .csv file to be downloaded
  output$DownloadClubData <- downloadHandler(
    filename = "club_data.csv",
    content = function(file) {
      write.csv(Postal_Club(), file, row.names = FALSE)
    }
  )
  
  ### Data Pane ###
  output$DownloadPostal <- downloadHandler(
    filename = "postal_data.csv",
    content = function(file) {
      write.csv(Postal, file, row.names = FALSE)
    }
  )
  
  ### National Records Pane ###
  # Currently doesn't have a UI component
  # Not currently in use
  # Postal_Record <- reactive({
  #   Postal %>%
  #     filter(National_Record == "Y",
  #            Year == input$YearRecord)
  # }) 
   
  # National Records table
  # output$RecordTable <- DT::renderDataTable({
  #   DT::datatable(
  #     Postal_Record()[, c(
  #       "Name",
  #       "Club",
  #       "Distance",
  #       "Avg_Split_50",
  #       "Relative_Place",
  #       "Gender",
  #       "Age",
  #       "Age_Group",
  #       "National_Record"
  #     )],
  #     colnames = c(
  #       "Age Group" = "Age_Group",
  #       "Relative Place" = "Relative_Place",
  #       "Average 50 Time" = "Avg_Split_50",
  #       "National Record" = "National_Record"
  #     ),
  #     rownames = FALSE,
  #     options = list(order = list(2, 'desc'),
  #                    columnDefs = list(
  #                      list(className = 'dt-center', targets = 1:8)
  #                    ))
  #   )
  # })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

