library(shiny)
library(readr)
library(tidyverse)
library(plotly)
library(ggplot2)
library(bslib)
library(thematic)
library(shinythemes)

## Data

df <- read_csv("Data/labor_practice.csv") %>%
  mutate(
    across(!group, as.numeric)
  )  %>%
  
  pivot_longer(
    -c(group, Year), 
    names_to = "country", 
    values_to = "variable"
  )


df2 <- read_csv("Data/df2.csv") %>% 
  mutate(
  across(!c(group, sex_variable), as.numeric)
) %>%
  select(-...1)%>%
  pivot_longer(
    -c(group, sex_variable,Year), 
    names_to = "country", 
    values_to = "variable"
  )


thematic_shiny(font = "auto")


# UI
ui <- navbarPage(
  
  # Title
  title = "International Labor Comparisons",
  
  # Theme
  theme = bs_theme(
    bg = "#0b3d91", fg = "white", primary = "#FCC780",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")),
  
  # Tab: About
  tabPanel("About",
           h1("Annual Labor Force Statistics (1970-2012)"),
           h2("U.S. Bureau of Labor Statistics"),
           br(),
           h4("The International Labor Comparisons (ILC) program adjusts data to a common conceptual framework because direct comparisons of national statistics across countries can be misleading due to differing concepts and methods. ILC data are used to assess U.S. economic and labor market performance relative to that of other countries and to evaluate the competitive position of the United States in increasingly global markets."),
           br(),
           h4("This report presents selected labor force statistics adjusted to U.S. concepts for 1970 onward for the United States and fifteen foreign countries: Australia, Canada, France, Germany, Italy, Japan, the Republic of Korea, Mexico, the Netherlands, New Zealand, South Africa, Spain, Sweden, Turkey, and the United Kingdom."), 
           br(), 
           h4("Even though the International Labor Comparisons program has been discontinued, anyone interested in learning more about the methodology and underlying data used for the program can contact the BLS."),
           br(), br(), br(),
           h5("Disclaimer: Not an official dashboard from U.S. Bureau of Labor Statistics; repurposed information from ", a("International Labor Comparisons report", 
                                                                                                         href = "https://www.bls.gov/fls/flscomparelf.htm"), "for practice and learning purposes."),
  ), 
  
  # Tab: Line Graph
  tabPanel("Change",
           verticalLayout(
             h1("Compare International Labor Statistics Changes Over Time"),
             br(),
               # Group
               selectInput(
                 inputId = "Group1",
                 label = "Select Group: ",
                 choices = unique(df$group)),
               br(),br(),
               plotlyOutput("allPlot")
           )
  ),   
  
  # Tab: Bar Graph
  tabPanel("Ranking",
           verticalLayout(
             h1("Compare International Labor Statistics Ranking Over Time"),
             br(),
             fluidRow(
             # Group
             column(width = 3, selectInput(
               inputId = "Group2",
               label = "Select Group: ",
               choices = unique(df$group))),
             
             # Year
             column(width = 3, selectInput(
               inputId = "Year1",
               label = "Select Year: ",
               choices = unique(df$Year)))
             ),
             br(), br(),
             plotlyOutput("barPlot")
           )
  ),
  
  # Tab: Gender
  tabPanel("Gender",
           
           verticalLayout(
             h1("Compare International Labor Statistics Based on Gender"),
             br(),
             fluidRow(
               # Sex
               column(width = 3,
                selectInput(
                 inputId = "Gender1",
                 label = "Select a variable by gender: ",
                 choices = c("Unemployment rates by sex", 
                             "Labor Force participation rates by sex", 
                             "Employment-polation ratios by sex", 
                             "Share of Labor Force by sex"))),
               
               # Year
               column(width = 3,
                      selectInput(
                        inputId = "Year2",
                        label = "Select Year: ",
                        choices = unique(df$Year)))),
             br(), br(),
             plotlyOutput("genderPlot")
           )
  ),
  
  
  # Tab: Age
  tabPanel("Age",
           verticalLayout(
             h1("Compare Employment Rages Based on Age group"),
             br(),
             # Year
             selectInput(
               inputId = "Year3",
               label = "Select Year: ",
               choices = unique(df$Year)),
             br(),br(),
             plotlyOutput("agePlot")
           )
          ),
  
  # Tab: About
  tabPanel("Technical Notes",
           h1("Technical Notes "),
           br(),
           h2("Unemployment"),
           h5("In the United States, unemployment includes all persons who, during the reference week: had no employment, were available for work, except for temporary illness, and had actively sought work during the 4-week period ending with the reference week.
           Active job search methods are those that have the potential to result in a job offer without further action on the part of the jobseeker. For example, sending a resume to an employer would be considered active, whereas simply reading newspaper advertisements would not.
           Persons who were waiting to start a new job must have fulfilled these criteria to be considered unemployed. However, persons who were waiting to be recalled to a job from which they had been laid off need not have been looking for work."),
          br(),
          h2("Employment"),
           h5("According to U.S. definitions, employment includes all persons who, during the reference week: worked at least 1 hour as paid employees, worked in their own business, profession, or on their own farm, or worked at least 15 hours as unpaid workers in a family-operated enterprise, and all those who did not work but had jobs or businesses from which they were temporarily absent due to vacation, illness, bad weather, childcare problems, maternity or paternity leave, labor-management dispute, job training, or other family or personal reasons, regardless of whether they were paid for the time off or were seeking other jobs.
           Each employed person is counted only once, even if he or she holds more than one job. For purposes of industry classification, multiple jobholders are counted in the job at which they worked the greatest number of hours during the reference week.
              Persons whose only activity consisted of work around their own house (painting, repairing, or own home housework) or volunteer work for religious, charitable, and other organizations are excluded."),
          br(),  
          h2("Employment by sector"),
           h5("Employment levels and distributions are shown for four broad economic sectors: agriculture, industry, and services. Sectoral employment data are based on the North American Industry Classification System (NAICS) for the United States for 2000 onward, Canada for 1976 onward, and Mexico for 2005 onward. Data for Japan are based on the Japanese Standard Industrial Classification System (JSIC). For all other countries covered, sectoral employment data are based on the International Standard Industrial Classification (ISIC). Effects of the change in classification system are discussed in the country notes."), 
          br(), 
          h2("Labor Force"), 
           h5("The labor force is comprised of persons who are in employment and unemployment. All members of the working-age population are eligible for inclusion in the labor force, and those 16 and over (in the United States; age limits vary by country) who have a job or are actively looking for one are so classified. All others—those who have no job and are not looking for one—are counted as 'not in the labor force.' The labor force participation rate represents the proportion of the working-age population that is in the labor force. Conversely, the inactivity rate represents the proportion of the working-age population that is not in the labor force. All persons in the civilian non-institutional working-age population who are neither employed nor unemployed are considered not in the labor force. Many who do not participate in the labor force are going to school or are retired. Family responsibilities keep others out of the labor force. Still others have a physical or mental disability which prevents them from participating in labor force activities."),
          br(), 
          h2("Working-Age Population"), 
           h5("The labor market statistics provided in this report describe the working-age population. In the United States, the working-age population is more specifically known as the civilian non- institutional working-age population: “Civilian” refers to persons who are not on active duty in the military; “Non-institutional” refers to persons who are not in institutions, such as prison inmates or those in a mental institution; and “Working-age” refers to persons 16 years of age and older.")
           
           

  ), 

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    # All Plot
    output$allPlot <- renderPlotly({
    
    df_graph <- df %>%
      filter(group == input$Group1) %>%
      drop_na()
    
    a <- ggplot(df_graph, aes(x = Year, y = variable, group = country, color = country)) +
      geom_line() +
      labs(x = "Year", y = "", title = input$Group)
    
    ggplotly(a)
  })  
  
    # Bar Plot  
    output$barPlot <- renderPlotly({
      
      df_graph <- df %>%
        filter(group == input$Group2 & Year == input$Year1) %>%
        arrange(desc(variable)) %>%
        drop_na()

      
      p <- ggplot(df_graph, aes(x = reorder(country,variable), y = variable)) +
        geom_bar(stat='identity') +
        labs(x = "", y = "", title = input$Group) +
        theme(
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.title.y = element_text(size = 14, face = "bold.italic")
          ) + 
        coord_flip() 
      
      ggplotly(p)
      
    })
    
    # Sector Plot
    output$sectorPlot <- renderPlotly({
      
      # if else for data
      df_graph <- df %>%
        filter(group %in% c("percent of employment in agriculture", "percent of employment in industry", "percent of employment in services") 
               & Year == input$Year) %>%
        arrange(desc(variable)) %>%
        drop_na()
      
      # Stacked
      c <- ggplot(df_graph, aes(fill=group, y=variable, x=country)) + 
        geom_bar(position="stack", stat="identity") + 
        labs(x = "", y = "", title = input$Year) +
        theme(legend.position = "bottom") + 
        coord_flip()
      
      ggplotly(c)
    })
    
    # Gender Plot
    output$genderPlot <- renderPlotly({
      
      df_graph <- df2 %>%
        filter(Year == input$Year2, 
               sex_variable == input$Gender1) %>%
        arrange(desc(variable)) %>%
        drop_na()
      
      df_segment <- df_graph %>%
        group_by(country) %>%
        summarize(min = min(variable, na.rm = TRUE),
                  max = max(variable, na.rm = TRUE)) 
      
      df_points <- df_graph
      
     t <-  ggplot(df_segment, aes(x = min, y = country)) +
        geom_segment(aes(xend = max, yend = country),
                     color = "#aeb6bf",
                     size = 3, 
                     alpha = .5) +
        geom_point(data = df_points, aes(x = variable, color = group), size = 4) + 
       labs(x = "", y = "", title = input$Gender)
      
      ggplotly(t)
      
    })
    
    # Age Plot
    output$agePlot <- renderPlotly({
      
      df_graph <- df %>%
        filter(group %in% c("unemployment rates for teenagers", "unemployment rates for young adult", "unemployment rates for adults"),
               Year == input$Year3) %>%
        arrange(desc(variable)) %>%
        drop_na()
      
      df_segment <- df_graph %>%
        group_by(country) %>%
        summarize(min = min(variable, na.rm = TRUE),
                  max = max(variable, na.rm = TRUE)) 
      
      df_points <- df_graph
      
      ggplot(df_segment, aes(x = min, y = country)) +
        geom_segment(aes(xend = max, yend = country),
                     color = "#aeb6bf",
                     size = 3, 
                     alpha = .5) +
        geom_point(data = df_points, aes(x = variable, color = group), size = 4) +
        labs(x = "", y = "")
      
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
