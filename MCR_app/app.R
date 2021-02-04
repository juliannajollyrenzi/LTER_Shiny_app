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


## Create theme----
mcr_theme <- bs_theme(
    bootswatch = "journal",
    base_font = font_google("Helvetica"),
    heading_font = font_google("Helvetica")
)

## User interface----
ui <- fluidPage(theme = mcr_theme, # fluid page means it changes when you expand/contract it. theme is for the CSS in the www subfolder
                
                navbarPage("Mo'orea Coral Reef LTER",
                           navbarMenu("Non-coral invertebrates",
                           tabPanel("By habitat",
                                    sidebarLayout(
                                        sidebarPanel(checkboxGroupInput(inputId = "pick_species_h", # ID for ui
                                                                        label = "Choose species: ", # what the user sees
                                                                        choices = unique(invert_habitat$Taxonomy),
                                                                        selected = "Acanthaster planci") # create widgets within panel
                                        ),
                                        mainPanel("Invertebrate abundance over time, by habitat (note the two outer sites are distinguished by their depth, in meters)",
                                                  plotOutput("inv_hab_plot") # THIS IS THE LAST THING!! Put this in after you do output in server
                                        
                                        
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
                           tabPanel("Benthic species"),
                           tabPanel("Macroalgal diversity")
                           
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
    
    output$inv_hab_plot <- renderPlot(
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
    )
    
    # now create the widget for the same data, but divided by site (second drop down)
    inv_site_reactive <- reactive({
        invert_site %>%
            filter(Taxonomy %in% input$pick_species_s)
    }) # reactive dataframe for the species the user selected (input$pick_species is now a vector of the user selections)
    
    output$inv_site_plot <- renderPlot(
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
    )
    
    
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