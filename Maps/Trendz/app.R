library(shiny)
library(readr)
library(dplyr)
library(ggplot2)
library(sf)
library(rnaturalearth)

DD  <- readRDS("WeatherData.RDS")   # Data from WorldBank, pre-processed   
cnt <- DD %>%                       # Codes for all the countries we have
  select(ISO3) %>% 
  unique() %>% 
  mutate(ISO3 = str_replace(ISO3, ".*, ", ""))

ii    <- ceiling(length(cnt$ISO3)*runif(1))    # Start with a random country
world <- ne_countries(scale="medium", returnclass="sf") %>%
  filter(iso_a3 %in% cnt$ISO3)

chh   <- world %>% 
  select(iso_a3, sovereignt) %>% 
  st_drop_geometry() %>% 
  tidyr::unite(Select, iso_a3, sovereignt, sep = ": ")

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("Temperature trends"),
   sidebarLayout(
      sidebarPanel(
        selectInput("chosen", 
                    "Choose country:", 
                    choices  = chh$Select,
                    multiple = FALSE,
                    selected = chh$Select[ii]),
        radioButtons("smoother", 
                    "Which smoother?", 
                    choices  = c("loess", "lm"),
                    selected = "lm") ,
        br(),
        plotOutput("WorldPlot"), 
        width = 4),
      mainPanel(
        plotOutput("TemperaturePlot", height="600px"), 
        width = 8)
   )
)

# Define server logic 
server <- function(input, output) {

   output$TemperaturePlot <- renderPlot({
     
     DDs  <- filter(DD, ISO3 == substr(input$chosen, 1, 3))
     minx <- min(DDs$Year)
     maxx <- max(DDs$Year)
     ggplot(DDs, aes(x=Year,y=Temperature)) +
       geom_rect(data=filter(DDs, Month %in% month.abb[seq(1,11,2)]),
                 fill="lavenderblush2", alpha=.7, 
                 xmin=minx, xmax=maxx, ymin=-Inf, ymax=Inf) +
       geom_rect(data=filter(DDs, Month %in% month.abb[seq(2,12,2)]),
                 fill="paleturquoise1", alpha=.3, 
                 xmin=minx, xmax=maxx, ymin=-Inf, ymax=Inf) +
       geom_point(size=0.2, colour="seagreen") +
       geom_smooth(method=input$smoother, formula=y~x, size=.75, color="red", fill="royalblue1") +
       scale_x_discrete(expand = c(0,0)) + 
       facet_grid(. ~ Month) +
       theme_minimal() +
       labs(title=paste0("Av. temp. degrees C each month ", minx, "-", maxx,
                         " in ", substr(input$chosen, 6, nchar(input$chosen))), 
            caption="Source: https://climateknowledgeportal.worldbank.org", y="", x="") +
       theme(axis.text.x=element_blank(), panel.spacing=unit(0.0, "lines"))     
     })
   
   output$WorldPlot <- renderPlot({
 
     world %>% 
       mutate(ccnt = if_else(iso_a3 %in% cnt$ISO3, "seagreen2", "grey88"),  
              ccnt = if_else(iso_a3 == substr(input$chosen,1,3), "red", ccnt)) %>% 
       ggplot() + 
       geom_sf(aes(fill = ccnt), color = NA, alpha = .8) + 
       scale_fill_identity() +
       theme(panel.background = element_rect(fill="aliceblue")) +
       coord_sf(crs = "EPSG:3035") +    # 3035, 4326, coordinate reference system
       labs(title=substr(input$chosen, 6, nchar(input$chosen)))
     
     })

}

# Run the application 
shinyApp(ui = ui, server = server)
