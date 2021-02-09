# This is an app for visualizing Mo'orea Coral Reef Long Term Ecological Reserach data
# Must be named "app.R"
# also can include a sub folder for a theme
# www MUST be the name of the subfolder for style

## Load packages and data----
library(shiny)
library(tidyverse) # for data wrangling
library(here) # for relative file paths
library(bslib) # for themes

# load invert data by habitat
invert_habitat <- read_csv(here("cleaned_data", 
                                "inverts_by_habitat.csv"))
# load invert data by site
invert_site <- read_csv(here("cleaned_data", 
                                "inverts_by_site.csv"))
# load benthos data by habitat
benth_habitat <- read_csv(here("cleaned_data", 
                               "benthos_by_habitat.csv"))
# load benthos data by site
benth_site <- read_csv(here("cleaned_data", 
                            "benthos_by_site.csv"))


# put text blocks here so they don't clog up the ui code
about.txt <- "The Mo'orea Long Term Ecological Research (LTER) site is a reserach site that was established by the U.S. National Science Foundation to gain understanding of long term dynamics in coral reef ecosystems. 
The Mo'orea LTER is part of a network of LTERs across the world that take repeated measurements each year to observe ecological changes over time. 
In Mo'orea, researchers have been observing a diverse suite of corals, non-coral invertebrates, fishes, and algae for nearly two decades, in addition to measuring changes in physical water properties such as temperature, salinity, and nutrient concentrations."

where.txt <- "Mo'orea is a tropical island in French Polynesia. Mo'orea is a part of the Society Islands, which is a volcanic island group in the Pacific that includes Tahiti, Bora Bora, and Tetiaroa.
Mo'orea is surrounded by coral reefs, which can be divided into different habitats based on their proximity to land (Figure 2, bottom). 
Coral reefs directly bordering the island are termed fringing reefs, which are usually separated from backreefs by deeper water. 
After the backreef (moving away from land) the coral forms a peak where waves break, termed the reef crest. 
Over the reef crest the water gets deeper. The corals on the ocean-side of the reef crest makeup the forereef (or 'outer' reef). 
The Mo'orea LTER collects data in all of these habitats, including at two different depths on the forereef (fringing, backreef, forereef/outer 10m, and forereef/outer 17m).
Researchers regularly survey all of four these habitats at six different sites around the island: LTER 1 and 2 (northern shore), LTER 3 and 4 (southeastern shore), and LTER 5 and 6 (southwestern shore)."

data.txt <- "We use data that are freely available on the Mo'orea LTER website (http://mcrlter.msi.ucsb.edu/data/topic/; downloaded Jan 2021). 
We focus on three core datasets: annual surveys of benthic organisms (i.e. species that live on the bottom, such as algae and sponges) [2]; 
annual surveys of non-coral invertebrates, such as sea urchins and snails [3]; and annual surveys of hard corals [4].
We create interactive visualizations of each of these time series, which we hope will inspire questions for further research and make the Mo'orea LTER data more accessible."

## Create theme----
mcr_theme <- bs_theme(
    bootswatch = "journal",
    base_font = font_google("Helvetica"),
    heading_font = font_google("Helvetica"),
    headings_color = "#fd5d5e"
)

## User interface----
ui <- fluidPage(theme = mcr_theme, # fluid page means it changes when you expand/contract it. theme is for the CSS in the www subfolder
                
                navbarPage("Mo'orea Coral Reef LTER",
                           tabPanel("Background information",
                                    mainPanel(
                                        h2("What is the Mo'orea LTER?"),
                                        p(about.txt),
                                        img(src = "Gump_station.jpg"),
                                        em("Figure 1: Photo of the UC Berkeley Gump Research Station, home of the Mo'orea LTER. Photo credit: MCR Website, http://mcr.lternet.edu/about/media"),
                                        h2("Where is Mo'orea?"),
                                        p(where.txt),
                                        img(src = "Moorea_map.png"),
                                        em("Figure 2: Map of Mo'orea LTER sites and habitats. Taken directly from Leichter et al. 2013"),
                                        h2("What does this app do with the LTER data?"),
                                        p(data.txt),
                                        h2("More questions?"),
                                        p("If you have more questions about the app, you can contact the app creator, Julianna Renzi, at jrenzi[at]ucsb.edu."),
                                        h2("Citations"),
                                        strong("[1]"),
                                               p("Leichter, J.J., A.L. Alldredge, G. Bernardi, A.J. Brooks, C.A. Carlson, R.C. Carpenter, P.J. Edmunds, M.R. Fewings, K.M. Hanson, J.L. Hench, and others. 2013. Biological and physical interactions on a tropical island coral reef: Transport and retention processes on Moorea, French Polynesia. Oceanography 26(3):52–63"),
                                        strong("[2]"),
                                            p("Carpenter, R of Moorea Coral Reef LTER. 2020. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Benthic Algae and Other Community Components, ongoing since 2005. knb-lter-mcr.8.32 doi:10.6073/pasta/0bf200e9e0f099de69826f57b18ff3da"),
                                        strong("[3]"), 
                                            p("Carpenter, R of Moorea Coral Reef LTER. 2020. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Other Benthic Invertebrates, ongoing since 2005. knb-lter-mcr.7.32 doi:10.6073/pasta/6b5bd9b8ef282fb9a23ba89572835f68"),
                                        strong("[4]"),
                                            p("Edmunds, P of Moorea Coral Reef LTER. 2020. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Corals, ongoing since 2005. knb-lter-mcr.4.38 doi:10.6073/pasta/10ee808a046cb63c0b8e3bc3c9799806")
    
                                        
                                    )
                                        
                                    ),
                           
                           
                           navbarMenu("Non-coral invertebrates",
                           tabPanel("By habitat",
                                    sidebarLayout(
                                        sidebarPanel(checkboxGroupInput(inputId = "pick_species_h", # ID for ui
                                                                        label = "Choose species: ", # what the user sees
                                                                        choices = unique(invert_habitat$Taxonomy),
                                                                        selected = "Acanthaster planci") # create widgets within panel
                                        ),
                                        mainPanel("Invertebrate abundance over time, by habitat (note the two outer sites are distinguished by their depth, in meters)",
                                                  plotOutput("inv_hab_plot")
                                        
                                        
                                    ))),
                           tabPanel("By site",
                                    sidebarLayout(
                                        sidebarPanel(checkboxGroupInput(inputId = "pick_species_s",
                                                                        label = "Choose species: ",
                                                                        choices = unique(invert_site$Taxonomy),
                                                                        selected = "Acanthaster planci")
                                            
                                        ),
                                        mainPanel("Invertebrate abundance over time, by LTER site. Recall LTER 1 and 2 are on the northern side, LTER 3 and 4 are on the southeastern side, and LTER 5 and 6 are on the southwestern side of the island",
                                                  plotOutput("inv_site_plot"))
                                        
                                        
                                    )
                                    )# want a widget and a graph (i.e. sidebar and a main panel)
                           ),
                           
                           
                           
                           tabPanel("Benthic species",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("select", label = h3("Select box"), 
                                                        choices = list("Choice 1" = 1, "Choice 2" = 2, "Choice 3" = 3), 
                                                        selected = 1),
                                            
                                            hr(),
                                            fluidRow(column(3, verbatimTextOutput("value")))
                                        ),
                                        mainPanel("Benthic (i.e. bottom-dwelling) species")
                                        
                                    )),
                           
                           
                           
                           tabPanel("Macroalgal diversity",
                                    sidebarLayout(
                                        sidebarPanel(
                                            # Copy the line below to make a date range selector
                                            dateRangeInput("dates", label = h3("Date range")),
                                            
                                            hr(),
                                            fluidRow(column(4, verbatimTextOutput("value")))
                                        ), 
                                        
                                        mainPanel("Macroalgal diversity through time")
                                    )
                                    ),
                           navbarMenu("Fishes",
<<<<<<< HEAD
                                
=======
                                      
>>>>>>> 0ddd0d816cb97bfb82af54c998930d20d7193b52
                                      tabPanel(
                                          "By habitat",
                                          sidebarLayout(
                                              checkboxGroupInput(
                                                  inputId = "pick_fish_h",
                                                  label = "Choose species: "),
                                              mainPanel(
                                                  "Fish abundance over time, by habitat (note the two outer sites are distinguished by their depth, in meters)"
                                              )
                                          )
                                      ),
                                      
                                      
<<<<<<< HEAD
                               tabPanel(
                                   "By site",
                                   sidebarLayout(
                                       checkboxGroupInput(
                                           inputId = "pick_fish_s",
                                           label = "Choose species: "),
                                   mainPanel(
                                       "Fish abundance over time, by LTER site. Recall LTER 1 and 2 are on the northern side, LTER 3 and 4 are on the southeastern side, and LTER 5 and 6 are on the southwestern side of the island"
                                   )
                                   )
                               )
=======
                                      tabPanel(
                                          "By site",
                                          sidebarLayout(
                                              checkboxGroupInput(
                                                  inputId = "pick_fish_s",
                                                  label = "Choose species: "),
                                              mainPanel(
                                                  "Fish abundance over time, by LTER site. Recall LTER 1 and 2 are on the northern side, LTER 3 and 4 are on the southeastern side, and LTER 5 and 6 are on the southwestern side of the island"
                                              )
                                          )
                                      )
>>>>>>> 0ddd0d816cb97bfb82af54c998930d20d7193b52
                           )
                           
                ) # create a navigation bar for tabs and names of tabs
                
)


## Reactive dataframes----

## Server for creating outputs from ui----
server <- function(input, output) {
    
    # first make a plot for invertebrate abundance divided by habitat (first drop down)
    inv_hab_reactive <- reactive({
        invert_habitat %>%
            filter(Taxonomy %in% input$pick_species_h)
    }) # reactive dataframe for the species the user selected (input$pick_species is now a vector of the user selections)
    
    output$inv_hab_plot <- renderPlot({
        ggplot(data = inv_hab_reactive(), aes(x = Year, y = Mean_abund_per_m2)) +
            geom_line(aes(color = Taxonomy)) + # don't forget parenthesis after calling reactive dataset!
            scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A",
                                          "#33A02C", "#FB9A99", "#E31A1C",
                                          "#FDBF6F", "#FF7F00", "#CAB2D6",
                                          "#6A3D9A", "#FFFF99", "#B15928",
                                          "#666666", "#E7298A")) +
            theme_classic() +
            ylab(expression(paste("Average abundance per ", m^2))) + 
            xlab("Survey year") +
            facet_wrap("Habitat") +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    # now create the widget for the same data, but divided by site (second drop down)
    inv_site_reactive <- reactive({
        invert_site %>%
            filter(Taxonomy %in% input$pick_species_s)
    }) # reactive dataframe for the species the user selected (input$pick_species is now a vector of the user selections)
    
    output$inv_site_plot <- renderPlot({
        ggplot(data = inv_site_reactive(), aes(x = Year, y = Mean_abund_per_m2)) +
            geom_line(aes(color = Taxonomy)) + # don't forget parenthesis after calling reactive dataset!
            scale_color_manual(values = c("#A6CEE3", "#1F78B4", "#B2DF8A",
                                          "#33A02C", "#FB9A99", "#E31A1C",
                                          "#FDBF6F", "#FF7F00", "#CAB2D6",
                                          "#6A3D9A", "#FFFF99", "#B15928",
                                          "#666666", "#E7298A")) +
            theme_classic() +
            ylab(expression(paste("Average abundance per ", m^2))) + 
            xlab("Survey year") +
            facet_wrap("Site") +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    
    # now start on work for drop down benthic tab
    output$value <- renderPrint({ input$select }) # this is from the gallery
    
    
    # now start work on the date range
    # You can access the values of the widget (as a vector of Dates) with input$dates, e.g.
    output$dates <- renderPrint({ input$dates })
    
    
}


## Run app----
shinyApp(ui = ui, server = server)





### NOTE: to start do this:
# make a user interface
#ui <- fluidPage()

# create a reactive dataset to use in the server

# make a server for creating outputs from ui
#server <- function(input, output) {}

#shinyApp(ui = ui, server = server)