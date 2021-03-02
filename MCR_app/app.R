# This is an app for visualizing Mo'orea Coral Reef Long Term Ecological Reserach data
# Must be named "app.R"
# also can include a sub folder for a theme
# www MUST be the name of the subfolder for style

## Load packages and data----
library(shiny)
library(tidyverse) # for data wrangling
library(here) # for relative file paths
library(bslib) # for themes
library(RColorBrewer) # for color

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
# load fish data by habitat
fish_habitat <- read_csv(here("cleaned_data",
                              "fish_by_habitat.csv")) %>% # then clean up one messy classification (planktivores)
    mutate(Fine_Trophic = replace(Fine_Trophic, Fine_Trophic == "Planktivore_exclusively", "Planktivore")) 
# load fish data by site
fish_site <- read_csv(here("cleaned_data",
                           "fish_by_site.csv")) %>% # then clean up one messy classification (planktivores)
    mutate(Fine_Trophic = replace(Fine_Trophic, Fine_Trophic == "Planktivore_exclusively", "Planktivore")) 
# load COTS data
cots_habitat <- read_csv(here("cleaned_data",
                              "cots_by_habitat.csv"))
cots_site <- read_csv(here("cleaned_data",
                           "cots_by_site.csv"))
# load temperature data
lter1.t <- read_csv(here("cleaned_data", "lter1_temp_summ.csv"))
lter2.t <- read_csv(here("cleaned_data", "lter2_temp_summ.csv"))
lter3.t <- read_csv(here("cleaned_data", "lter3_temp_summ.csv"))
lter4.t <- read_csv(here("cleaned_data", "lter4_temp_summ.csv"))
lter5.t <- read_csv(here("cleaned_data", "lter5_temp_summ.csv"))
lter6.t <- read_csv(here("cleaned_data", "lter6_temp_summ.csv"))

# algal diversity data
alg_div_h <- read_csv(here("cleaned_data", "alg_div_by_hab.csv"))
alg_div_s <- read_csv(here("cleaned_data", "alg_div_by_site.csv"))

# turbinaria data
t_ornata_habit <- read_csv(here("cleaned_data", "t_ornata_by_hab.csv"))
t_ornata_site <- read_csv(here("cleaned_data", "t_ornata_by_site.csv"))

# read in nutrient data
nutrients <- read_csv(here("cleaned_data", "lter1_nutrients.csv"))

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
We focus on these core datasets: annual surveys of benthic organisms (i.e. species that live on the bottom, such as algae and sponges) [2]; 
annual surveys of non-coral invertebrates, such as sea urchins and snails [3]; annual surveys of hard corals [4]; annual surveys of reef-associated fishes [5]; 
high-resolution temperature data taken every 20 minutes [6]; bi-monthly nutrient data [7]; and annual surveys of Crown-of-thorns-starfishes [8].
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
                                            p("Edmunds, P of Moorea Coral Reef LTER. 2020. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Corals, ongoing since 2005. knb-lter-mcr.4.38 doi:10.6073/pasta/10ee808a046cb63c0b8e3bc3c9799806"),
                                        strong("[5]"),
                                            p("Brooks, A of Moorea Coral Reef LTER. 2021. MCR LTER: Coral Reef: Long-term Population and Community Dynamics: Fishes, ongoing since 2005. knb-lter-mcr.6.58 doi:10.6073/pasta/a667eed481d9743c69c4209f6479acb4"),
                                        strong("[6]"),
                                            p("Leichter, J, K. Seydel and C. Gotschalk of Moorea Coral Reef LTER. 2019. MCR LTER: Coral Reef: Benthic Water Temperature, ongoing since 2005. knb-lter-mcr.1035.12 doi:10.6073/pasta/ea6a89415b1d9118d441235723c1a23f"),
                                        strong("[7]"),
                                            p("Alldredge, A of Moorea Coral Reef LTER. 2019. MCR LTER: Coral Reef: Water Column: Nutrients, ongoing since 2005. knb-lter-mcr.1034.9 doi:10.6073/pasta/9328a024f2bf16ecc66024f07dbcc574"),
                                        strong("[8]"),
                                            p("Brooks, A of Moorea Coral Reef LTER. 2018. MCR LTER: Coral Reef: Long-term Population Dynamics of Acanthaster planci, ongoing since 2005. knb-lter-mcr.1039.9 doi:10.6073/pasta/3fcbd829a459c879fc5e1d3b11518956"),
                                    
    
                                        
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
                           
                           
                           navbarMenu("Macroalgal diversity",
                           tabPanel("By habitat",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("div_met_h", label = h3("Select diversity metric"), 
                                                        choices = list("Shannon index" = "Shannon_index", "Species richness" = "Species_richness",
                                                                       "Simpson index" = "Simpson_index", "Pielou's evenness" = "Pielou_evenness"), 
                                                        selected = "Shannon_index")
                                        ),
                                        mainPanel("Macroalgal diversity through time",
                                                  plotOutput("alg_div_plot_h"))
                                        
                                    )),
                           tabPanel("By site",
                                    sidebarLayout(
                                        sidebarPanel(
                                            selectInput("div_met_s", label = h3("Select diversity metric"),
                                                        choices = list("Shannon index" = "Shannon_index", "Species richness" = "Species_richness",
                                                                       "Simpson index" = "Simpson_index", "Pielou's evenness" = "Pielou_evenness"),
                                                        selected = "Shannon_index")
                                                
                                            ),
                                        
                                    mainPanel("Macroalgal diversity through time",
                                              plotOutput("alg_div_plot_s"))
                                    )
                               
                           )),
                           
                           navbarMenu("Benthos over time",
                               tabPanel("By habitat",
                                    sidebarLayout(
                                        sidebarPanel(
                                            # Copy the line below to make a slider range 
                                            sliderInput("benthDates_h", label = h3("Year Range"), min = 2005, 
                                                        max = 2020, value = c(2005, 2010), sep = "", step = 1),
                                            
                                        ), 
                                        
                                        mainPanel("Benthic (i.e. bottom dwelling) organisms through time, divided by habitat type",
                                                  plotOutput("benth_time_plot_h"))
                                    )
                                    ),
                               tabPanel("By site",
                                        sidebarLayout(
                                            sidebarPanel(
                                                sliderInput("benthDates_s", label = h3("Year Range"), min = 2005,
                                                            max = 2020, value = c(2005, 2010), sep = "", step = 1),
                                            ),
                                            
                                            mainPanel("Benthic (i.e. bottom dwelling) organisms through time, divided by LTER Site",
                                                      plotOutput("benth_time_plot_s"))
                                        ))
                           ),
                           navbarMenu("Fishes",
                                      tabPanel(
                                          "By habitat",
                                          sidebarLayout(sidebarPanel(
                                              checkboxGroupInput(
                                                  inputId = "pick_fish_h",
                                                  label = "Choose species: ",
                                                  choices = unique(fish_habitat$Fine_Trophic),
                                                  selected = "Benthic Invertebrate Consumer")),
                                              mainPanel(
                                                  "Fish abundance over time, by habitat",
                                                  plotOutput("fish_hab_plot")
                                              )
                                          )
                                      ),
                                      
                                      
                               tabPanel(
                                   "By site",
                                   sidebarLayout(sidebarPanel(
                                       checkboxGroupInput(
                                           inputId = "pick_fish_s",
                                           label = "Choose species: ",
                                           choices = unique(fish_habitat$Fine_Trophic),
                                           selected = "Benthic Invertebrate Consumer")),
                                   mainPanel(
                                       "Fish abundance over time, by LTER site. Recall LTER 1 and 2 are on the northern side, LTER 3 and 4 are on the southeastern side, and LTER 5 and 6 are on the southwestern side of the island",
                                       plotOutput("fish_site_plot")
                                   )
                                   )
                               )
                           ),
                           tabPanel("Environmental factors",
                                    h3("Crown of thorns starfish outbreaks"),
                                    p("Crown of thorns starfish (COTS) are corallivorous (i.e. coral eating) starfish that can devastate reefs when their populations increase rapidly (an 'oubreak'). There was one massive COTS outbreak in Mo'orea's recent history (see plots below) that killed a lot of the corals on the forereef, leaving just skeletons behind. What time series appear to change in response to the COTS outbreak? How quickly do they recover?"),
                                    tabsetPanel(type = "tabs",
                                        tabPanel("By site", plotOutput("COTS_site_plot")),
                                        tabPanel("By habitat", plotOutput("COTS_hab_plot"))
                                    ),
                                    h3("Temparture over time"),
                                    p("Both extremely cold temperatures and extremely warm temperatures stress corals out and can lead to coral death. Below are time series plots from the LTER showing maximum daily temperatures at different sites, habitats, and depths through time. The most significant heat event during this time period was in 2019 (see dashed vertical line in plots below), when hot temperatures drove corals to bleach and some to die. Read more about the 2019 event here: https://www.abc.net.au/news/science/2019-05-21/coral-bleaching-french-polynesia/11129634"),
                                    tabsetPanel(type = "tabs",
                                                tabPanel("LTER 1", plotOutput("lter1_temp_plot")),
                                                tabPanel("LTER 2", plotOutput("lter2_temp_plot")),
                                                tabPanel("LTER 3", plotOutput("lter3_temp_plot")),
                                                tabPanel("LTER 4", plotOutput("lter4_temp_plot")),
                                                tabPanel("LTER 5", plotOutput("lter5_temp_plot")),
                                                tabPanel("LTER 6", plotOutput("lter6_temp_plot"))
                                                ),
                                    p("Below are plots of mean daily temperatures at different LTER sites, habitats, and depths."),
                                    tabsetPanel(type = "tabs",
                                                tabPanel("LTER 1", plotOutput("lter1_temp_plot_mean")),
                                                tabPanel("LTER 2", plotOutput("lter2_temp_plot_mean")),
                                                tabPanel("LTER 3", plotOutput("lter3_temp_plot_mean")),
                                                tabPanel("LTER 4", plotOutput("lter4_temp_plot_mean")),
                                                tabPanel("LTER 5", plotOutput("lter5_temp_plot_mean")),
                                                tabPanel("LTER 6", plotOutput("lter6_temp_plot_mean"))
                                    ),
                                    h3("Cyclone Oli"),
                                    p("Cyclones and hurricanes can damage coral reefs. Big storms can uproot corals entirely, the turbulence from the storm can re-suspend harmful sediments, and loose fragments in the water column can cut corals, which has been linked to increases in disease. In In February, 2010 a large tropical storm, Cyclone Oli, passed by French Polynesia, damaging coral reefs. Check out the infrared photograph below from captured by the National Oceanic and Atmospheric Administration's Geostationary Operational Environmental Satellite (GOES-11) showing Oli during its formation as a cyclone (~Feb 1, 2010)."),
                                    img(src = "Cyclone_Oli_FP.jpg", width="50%"),
                                    em("Photo credit: NOAA/JTWC"),
                                    p("The infrared photo below shows Oli passing French Polynesia. The image was taken by NASA's Aqua satellite on February 3. Purple indicates the most intense part of the storm, with hurricanes and cloud temperatures below -63° Farenheight, causing 20ft waves! Do you notice any time series in the LTER data that change after 2010? Which species do you think are most vulnerable to tropical storms? How does having long-term ecological monitoring sites like Mo'orea impact our ability to study ecosystem changes after natural disasters like Cyclone Oli?"),
                                    img(src = "Cyclone_oli_formation.jpg", width="50%"),
                                    em("Photo credit: NASA/JPL, Ed Olsen"),
                                    h3("A quickly spreading macroalga: Turbinaria ornata"),
                                    p("Turbinaria ornata is a physically-defended brown macroalgal species that has been spreading rapidly on Mo'orean reefs. T. ornata can compete with other species for space and light, which can be detrimental to hard corals. As it's increased in abundance over time, researchers have begun worrying that it may be bad news for some coral habitats. Do you notice any habitats or sites that look like they've seen large changes in T. ornata over time? Are there other species that seem to change as T. ornata increases?"),
                                    tabsetPanel(type = "tabs",
                                                tabPanel("By site", plotOutput("t_ornata_site_plot")),
                                                tabPanel("By habitat", plotOutput("t_ornata_hab_plot"))
                                    ),
                                    img(src = "T_ornata_moorea.jpg", width="50%"),
                                    em("A clump of T. ornata. Photo credit: Julianna Renzi"),
                                    h3("Nutrients"),
                                    p("Nutrients are vital to life, but some forms of nutrients can be harmful to some marine species. Nitrate, for instance, can facilitate the growth of macroalgae and harm corals. Phosphorous, on the other hand, has been shown to help corals in some instances and potentially prevent coral bleaching. Here are plots showing how nutrients have changed at LTER 1 on the north shore in the backreef, fringing reef, and forereef (only LTER site included in the nutrient time series). Do you notice any changes in other time series that might be related to nutrients?"),
                                    tabsetPanel(type = "tabs",
                                                tabPanel("Silicate", plotOutput("nutrients_silicate")),
                                                tabPanel("Phosphate", plotOutput("nutrients_phosphate")),
                                                tabPanel("Nitrite", plotOutput("nutrients_nitrite")),
                                                tabPanel("Nitrate", plotOutput("nutrients_nitrate"))
                                                )
                                    
                                    )
                           
                ) 
                
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
            scale_x_continuous(breaks = seq(min(inv_hab_reactive()$Year), 
                                            max(inv_hab_reactive()$Year), by = 1)) +
            facet_wrap("Habitat") +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom")
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
            scale_x_continuous(breaks = seq(min(inv_site_reactive()$Year), 
                                            max(inv_site_reactive()$Year), by = 1)) +
            facet_wrap("Site") +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom")
    })
    
    
    # now start on work for drop down benthic tab
    output$value <- renderPrint({ input$select }) # this is from the gallery
    
    
    
    # now start work on the slider range for the benthos
    # start with reactive df
    benth_hab_reactive <- reactive({
        benth_habitat %>% 
            filter(Spp_grouping == "Crustose_corallines" |
                       Spp_grouping == "Hard_coral" |
                       Spp_grouping == "Macroalgae" |
                       Spp_grouping == "Turf") %>% 
            filter(Year >= input$benthDates_h[1] & Year <= input$benthDates_h[2]) %>% 
            mutate(Year = as.integer(Year))
    })
    
    # plot benthos over time by habitat
    output$benth_time_plot_h <- renderPlot({
        ggplot(data = benth_hab_reactive(),
               aes(x = Year, y = Mean_perc_cov, group = Spp_grouping)) +
            geom_line(aes(colour = Spp_grouping)) +
            scale_color_manual(name = "Species grouping",
                               labels = c("Crustose coralline algae", "Hard coral", "Macroalgae", "Turf algae"),
                               values = brewer.pal(4, "Dark2")) +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_x_continuous(breaks = seq(min(benth_hab_reactive()$Year), 
                                            max(benth_hab_reactive()$Year), by = 1)) +
            ylab("Mean percent cover (%)") + 
            xlab("Survey year") +
            facet_wrap(~Habitat)
    })
    
    # now by site
    # start with reactive df
    benth_site_reactive <- reactive({
        benth_site %>% 
            filter(Spp_grouping == "Crustose_corallines" |
                       Spp_grouping == "Hard_coral" |
                       Spp_grouping == "Macroalgae" |
                       Spp_grouping == "Turf") %>% 
            filter(Year >= input$benthDates_s[1] & Year <= input$benthDates_s[2]) %>% 
            mutate(Year = as.integer(Year))
    })
    
    output$benth_time_plot_s <- renderPlot({
        ggplot(data = benth_site_reactive(),
               aes(x = Year, y = Mean_perc_cov, group = Spp_grouping)) +
            geom_line(aes(colour = Spp_grouping)) +
            scale_color_manual(name = "Species grouping",
                                 labels = c("Crustose coralline algae", "Hard coral", "Macroalgae", "Turf algae"),
                                 values = brewer.pal(4, "Dark2")) +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_x_continuous(breaks = seq(min(benth_site_reactive()$Year), 
                                            max(benth_site_reactive()$Year), by = 1)) +
            ylab("Mean percent cover (%)") + 
            xlab("Survey year") +
            facet_wrap(~Site) +
            theme(axis.text.x = element_text(angle = 90))
    })
    
    
    
    
    # plot macroalgal diversity over time: start with by habitat
    alg_div_react_h <- reactive({
        alg_div_h %>% 
            dplyr::select("Year", "Habitat", input$div_met_h)
    })
    
    output$alg_div_plot_h <- renderPlot({
        nms <- names(alg_div_react_h()) # this is to get column names in the right format for ggplot (need to undo quotes, etc.)
        yr <- nms[1]
        metric <- nms[3]
        y_lab <- c(str_split(nms[3], pattern = "_")[[1]]) # for y lab
        ggplot(data = alg_div_react_h(),
               aes(x = !!ensym(yr), y = !!ensym(metric), group = Habitat)) +
            geom_line(aes(colour = Habitat)) +
            scale_x_continuous(breaks = seq(min(alg_div_react_h()$Year), 
                                            max(alg_div_react_h()$Year), by = 1)) +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_color_brewer(palette = "Set2") + 
            ylab(paste(y_lab[1], y_lab[2]))
    })
    
    # now by site
    alg_div_react_s <- reactive({
        alg_div_s %>% 
            dplyr::select("Year", "Site", input$div_met_s)
    })
    
    output$alg_div_plot_s <- renderPlot({
        nms <- names(alg_div_react_s())
        yr <- nms[1]
        metric <- nms[3]
        y_lab <- c(str_split(nms[3], pattern = "_")[[1]]) # for y lab
        ggplot(data = alg_div_react_s(),
               aes(x = !!ensym(yr), y = !!ensym(metric), group = Site)) +
            geom_line(aes(colour = Site)) +
            scale_x_continuous(breaks = seq(min(alg_div_react_s()$Year), 
                                            max(alg_div_react_s()$Year), by = 1)) +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_color_brewer(palette = "Set2") + 
            ylab(paste(y_lab[1], y_lab[2]))
    })
    
    
    
    
    # work on fishes: start with by habitat
    fish_hab_reactive <- reactive({
        fish_habitat %>% 
            filter(Fine_Trophic %in% input$pick_fish_h)
    })
    
    output$fish_hab_plot <- renderPlot({
        ggplot(data = fish_hab_reactive(), 
               aes(x = Year, y = Total_biomass_kg, group = Fine_Trophic)) +
            geom_line(aes(colour = Fine_Trophic)) +
            scale_color_manual(name = "Trophic group",
                               values = brewer.pal(length(input$pick_fish_h), "Paired")) +
            theme_classic() +
            ylab("Total biomass (kg)") + 
            xlab("Survey year") +
            scale_x_continuous(breaks = seq(min(fish_hab_reactive()$Year), 
                                            max(fish_hab_reactive()$Year), by = 1)) +
            facet_wrap(~Habitat) +
            theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7)) +
            scale_fill_discrete(name = "Trophic position")
    })
    
    # then fishes by site
    fish_site_reactive <- reactive({
        fish_site %>% 
            filter(Fine_Trophic %in% input$pick_fish_s)
    })
    
    output$fish_site_plot <- renderPlot({
        ggplot(data = fish_site_reactive(),
               aes(x = Year, y = Total_biomass_kg, group = Fine_Trophic)) +
            geom_line(aes(colour = Fine_Trophic)) +
            scale_color_manual(name = "Trophic group",
                               values = brewer.pal(length(input$pick_fish_s), "Paired")) +
            theme_classic() +
            ylab("Total biomass (kg)") + 
            xlab("Survey year") +
            scale_x_continuous(breaks = seq(min(fish_site_reactive()$Year), 
                                            max(fish_site_reactive()$Year), by = 1)) +
            facet_wrap(~Site) +
            theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7)) +
            labs(title = "LTER Sites") +
            scale_fill_discrete(name = "Trophic position") 
    })
    
    # create some static graphs for the "other stressors" tab
    output$COTS_site_plot <- renderPlot({
        ggplot(data = cots_site,
               aes(x = Year, y = Count_COTS)) +
            geom_line(color = "#3895D3") +
            theme_light() +
            theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7)) +
            ylab("COTS counts") + 
            xlab("Survey year") +
            scale_x_continuous(breaks = seq(min(cots_site$Year), 
                                            max(cots_site$Year), by = 1)) +
            facet_wrap(~Site) +
            labs(title = "LTER Sites")  
    })
    output$COTS_hab_plot <- renderPlot({
        ggplot(data = cots_habitat,
               aes(x = Year, y = Count_COTS)) +
            geom_line(color = "#3895D3") +
            theme_light() +
            theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7)) +
            ylab("COTS counts") + 
            xlab("Survey year") +
            scale_x_continuous(breaks = seq(min(cots_site$Year), 
                                            max(cots_site$Year), by = 1)) +
            facet_wrap(~Habitat) +
            labs(title = "Habitats")  
    })
    
    
    # plot Maximum temperature data over time
    output$lter1_temp_plot <- renderPlot({
        lter1.t %>% 
               mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FRI"), labels = c("Backreef", "Fringing"))) %>% # for labeling
                   ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
                   geom_line(alpha = 0.6) +
                   facet_wrap(~reef_type_code, ncol= 1) +
                   scale_color_manual(name = "Depth", labels = c("1m", "6m"), values = brewer.pal(2, "Set1")) +
                   theme_light() +
                   ylab("Max benthic temperature (°C)") +
                   xlab("Date") +
                    scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                               to = max(x), 
                                                               by = "1 year")) +
                   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
                   geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter2_temp_plot <- renderPlot({
        lter2.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "4m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Max benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter3_temp_plot <- renderPlot({
        lter3.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "7m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Max benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter4_temp_plot <- renderPlot({
        lter4.t %>% 
        mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "6m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Max benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter5_temp_plot <- renderPlot({
        lter5.t %>% 
        mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "3m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Max benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter6_temp_plot <- renderPlot({
        lter6.t %>% 
        mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Max_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1 m", "2 m", "4m", "10 m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Max benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    
    
    # plot MEAN temperature data over time
    output$lter1_temp_plot_mean <- renderPlot({
        lter1.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FRI"), labels = c("Backreef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "6m"), values = brewer.pal(2, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter2_temp_plot_mean <- renderPlot({
        lter2.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "4m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter3_temp_plot_mean <- renderPlot({
        lter3.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "7m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter4_temp_plot_mean <- renderPlot({
        lter4.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "6m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter5_temp_plot_mean <- renderPlot({
        lter5.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1m", "2m", "3m", "10m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    output$lter6_temp_plot_mean <- renderPlot({
        lter6.t %>% 
            mutate(reef_type_code = factor(reef_type_code, levels = c("BAK", "FOR", "FRI"), labels = c("Backreef", "Forereef", "Fringing"))) %>% # for labeling
            ggplot(aes(x = Date, y = Mean_T, color = as.factor(sensor_depth_m))) +
            geom_line(alpha = 0.6) +
            facet_wrap(~reef_type_code, ncol= 1) +
            scale_color_manual(name = "Depth", labels = c("1 m", "2 m", "4m", "10 m", "20m", "30m", "40m"), values = brewer.pal(7, "Set1")) +
            theme_light() +
            ylab("Mean benthic temperature (°C)") +
            xlab("Date") +
            scale_x_date(breaks = function(x) seq.Date(from = as.Date("01-01-2005", format = "%m-%d-%Y"), 
                                                       to = max(x), 
                                                       by = "1 year")) +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
            geom_vline(xintercept = as.Date("03-01-2019", format = "%m-%d-%Y"), linetype = "dashed", alpha = 0.5)
    })
    
    
    output$t_ornata_site_plot <- renderPlot({
        ggplot(data = t_ornata_site,
               aes(x = Year, y = Mean_perc_cov, group = Site)) +
            geom_line(aes(colour = Site)) +
            scale_color_brewer(palette = "Dark2") +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_x_continuous(breaks = seq(min(t_ornata_site$Year), 
                                            max(t_ornata_site$Year), by = 1)) +
            ylab(expression(paste("Mean percent cover: ", italic("T. ornata")))) + 
            xlab("Survey year") 
    })
    output$t_ornata_hab_plot <- renderPlot({
        ggplot(data = t_ornata_habit,
               aes(x = Year, y = Mean_perc_cov, group = Habitat)) +
            geom_line(aes(colour = Habitat)) +
            scale_color_brewer(palette = "Dark2") +
            theme_classic() +
            theme(text = element_text(size=15),
                  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  axis.title.x = element_text(vjust = -0.7),
                  legend.position = "bottom") +
            scale_x_continuous(breaks = seq(min(t_ornata_habit$Year), 
                                            max(t_ornata_habit$Year), by = 1)) +
            ylab(expression(paste("Mean percent cover: ", italic("T. ornata")))) + 
            xlab("Survey year") 
    })
    
    
    
    # now create nutrient plots
    output$nutrients_silicate <- renderPlot({
        nutrients %>% 
            ggplot(aes(x = Date, y = Silicate, color = Habitat)) +
            geom_line() +
            scale_color_brewer(palette = "Set2") +
            theme_classic() +
            ylab(expression(paste("Silicate concentrations (", mu, "mol / L)"))) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                  legend.position = "bottom")
    })
    output$nutrients_phosphate <- renderPlot({
        nutrients %>% 
            ggplot(aes(x = Date, y = Phosphate, color = Habitat)) +
            geom_line() +
            scale_color_brewer(palette = "Set2") +
            theme_classic() +
            ylab(expression(paste("Phosphate concentrations (", mu, "mol / L)"))) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                  legend.position = "bottom")
    })
    output$nutrients_nitrite <- renderPlot({
        nutrients %>% 
            ggplot(aes(x = Date, y = Nitrite, color = Habitat)) +
            geom_line() +
            scale_color_brewer(palette = "Set2") +
            theme_classic() +
            ylab(expression(paste("Nitrite concentrations (", mu, "mol / L)"))) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                  legend.position = "bottom")
    })
    output$nutrients_nitrate <- renderPlot({
        nutrients %>% 
            ggplot(aes(x = Date, y = Nitrate, color = Habitat)) +
            geom_line() +
            scale_color_brewer(palette = "Set2") +
            theme_classic() +
            ylab(expression(paste("Nitrate concentrations (", mu, "mol / L)"))) +
            scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), 
                  legend.position = "bottom")
    })
    
}


## Run app----
shinyApp(ui = ui, server = server)