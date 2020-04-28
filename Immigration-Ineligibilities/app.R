# loading packages

library(shiny)
library(tidyverse)
library(readxl)
library(janitor)
library(skimr)
library(countrycode)
library(maps)
library(sf)
library(tidycensus)
library(tools)
library(gganimate)
library(ggthemes)
library(gapminder)
library(broom)
library(gt)
library(shinythemes)

# creating data

lpr_data <- read_excel("raw-data/lpr_data_recent.xlsx",
                       range = cell_rows(c(4:220)),
                       na = c("-", "D", "X")) %>% 
    clean_names() %>% 
    remove_empty()

adm_data <- read_excel("raw-data/aliens_inadmissable.xlsx",
                      range = cell_rows(c(4:201)),
                      na = c("-", "D", "X")) %>% 
    clean_names() %>% 
    remove_empty()

lpr_join <- lpr_data %>% 
    slice(12:216) %>% 
    select(region_and_country_of_birth, x2009, x2010, x2011, x2012, x2013, x2014, x2015, x2016, x2017, x2018) %>% 
    rename("country" = region_and_country_of_birth) %>% 
    pivot_longer(-country, 
                 names_to = "year",
                 names_prefix = "x",
                 values_to = "admissions") %>% 
    mutate(country = str_replace_all(country, "[0-9,]", ""))

adm_join  <- adm_data %>% 
    slice(12:197) %>% 
    select(region_and_country_of_nationality, x2009, x2010, x2011, x2012, x2013, x2014, x2015, x2016, x2017, x2018) %>% 
    rename("country" = region_and_country_of_nationality) %>% 
    pivot_longer(-country, 
                 names_to = "year",
                 names_prefix = "x",
                 values_to = "exclusions") %>% 
    mutate(country = str_replace_all(country, "[0-9,]", ""))


comp <- full_join(lpr_join, adm_join, by = c("country", "year")) %>% 
    filter(!is.na(admissions)) %>% 
    filter(!is.na(exclusions)) %>% 
    mutate(prop = exclusions / admissions)


total <- comp %>% 
    group_by(country) %>% 
    summarise(total_excluded = sum(exclusions), 
              total_admitted = sum(admissions)) %>% 
    filter(total_admitted > 1000) %>% 
    mutate(prop = total_excluded / total_admitted)  %>% 
    mutate(region = countrycode(country, origin = 'country.name', destination = 'region')) %>%
    filter(!is.na(region))



possible_regions <- c("East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean", "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa")

state <- read_excel("raw-data/admissable_by_state.xlsx",
                   range = cell_rows(c(4:58))) %>% 
    slice(-1) %>% 
    rename("state" = `State or territory of residence`) %>% 
    pivot_longer(-state, names_to = "year", values_to = "admissions")



states <- st_as_sf(map("state", plot = FALSE, fill = TRUE))


graph <- state %>% 
    filter(! state %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
    mutate(state = tolower(state)) %>% 
    full_join(states, by = c("state" = "ID"))


adm_state <- read_excel("raw-data/aliens_inadmissable_by_state.xlsx") %>% 
    pivot_longer(-state, names_to = "year", values_to = "exclusions")

graph_2 <- adm_state %>% 
    filter(! state %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
    mutate(state = tolower(state)) %>% 
    full_join(states, by = c("state" = "ID"))

states_combined <- state %>% 
    mutate(state = tolower(state)) %>% 
    inner_join(adm_state, by = c("state", "year"))

 

### UI


ui <- navbarPage(
    "Immigration Ineligibilities",
    
    tabPanel("Model",
             titlePanel("Modeling Immigration Exclusions"),
             p("Although the admission of immigrants is a well-studied subject, the exclusion of immigrants is far more difficult to tackle. Why do countries choose to deny the immigrants that they do? This page attempts to demonstrate some key relationships."),
             fluidPage(
                 theme = shinytheme("cerulean"),
                 titlePanel("Exclusions by Region"),
                 selectInput("region", "Select A Region", possible_regions),
                     plotOutput("distPlot"),
                     tableOutput('tbl'))
                 
             ),
    
    
    tabPanel("Discussion",
             titlePanel("Modeling Choices"),
             p("I decided to calculate a regression based on two factors: region and number of admitted immigrants. Although there are many other factors that I considered analyzing, these two provided strong and interesting correlations. 
               "),
             p("Still, there are a number of confounding variables that I have to address"),
             p("First, a major problem with this approach is that ineligibilities are largely bureacratic. The country with the highest ratio of exclusions per admissions is Canada. Most likely, this discrepency is due to an overlap in legal systems with cross-border violations rather than a special factor. Furthermore, people can reaaply and barred over and over, while they can only be admitted once."),
             p("Second, there is the issue of admission itself. Not all applications are equal, here is a graph of the acceptance by certain regions of the US."),
             fluidPage(
                 titlePanel("Internal Dilemnas"),
                 imageOutput("plot1")),
                 imageOutput("plot2")),
    
    
    tabPanel("About", 
             titlePanel("About"),
             h3("Purpose"),
             p("This project began because of my last high-school debate topic. Although it was a general immigration topic, there was a shocking amount of literature about the people who are denied access into the country. Although they are often not thought about, I thought it was important to shed a light onto their existence. "),
             h3("About Me"),
             p("My name is Julian Habermann, and I am a current Harvard first-year planning on studying History. 
             You can reach me at jhabermann@college.harvard.edu.")))

### server

server <- function(input, output, session) {
    output$distPlot <- renderPlot({
        region <- total %>% 
            filter(region == input$region) %>% 
            ggplot(aes(x = total_admitted, y = total_excluded)) +
            geom_point(aes(size = prop)) + 
            geom_smooth(method = "glm") +
            labs(title = "Exclusions vs Admissions by Region", subtitle = "Sum from 2009-2018", x = "Total Admitted", y = "Total Excluded", size = "Proportion of immigrants \n excluded compared to admitted") +
            theme_classic()
        region
    })
    output$tbl <- renderTable({
        table <- total %>% 
            filter(region == input$region) %>% 
            lm(total_excluded ~ total_admitted, data = .) %>% 
            tidy(conf.int = TRUE) %>% 
            slice(2) %>% 
            select(estimate, conf.low, conf.high)
        table
    })
    output$plot1 <- renderImage({
        outfile <- tempfile(fileext='.gif')
        p = graph %>% ggplot(aes(fill = admissions)) +
            geom_sf(data = graph, aes(geometry = geom)) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = "Little Change over Time in Admissions",
                 subtitle = "Year: {current_frame}",
                 fill = "Admissions") +
            theme_void() + transition_manual(year)
        anim_save("outfile.gif", animate(p))
        
        list(src = "outfile.gif",
             contentType = 'image/gif', 
             width = 400,
             height = 500,
             alt = "This is alternate text"
        )}, deleteFile = TRUE)
    output$plot2 <- renderImage({
        outfile <- tempfile(fileext='.gif')
        p <- graph_2 %>% ggplot(aes(fill = exclusions)) +
            geom_sf(data = graph_2, aes(geometry = geom)) +
            scale_fill_viridis_c(option = "plasma",
                                 direction = -1) +
            labs(title = "But larger swings in Exclusions",
                 subtitle = "Year: {current_frame}",
                 caption = "Source:",
                 fill = "Exclusions") +
            theme_void() + transition_manual(year)
        anim_save("outfile.gif", animate(p))
        list(src = "outfile.gif",
             contentType = 'image/gif', 
             width = 400,
             height = 500,
             alt = "This is alternate text"
        )}, deleteFile = TRUE)
    }


shinyApp(ui = ui, server = server)