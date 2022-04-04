#install.packages("shinythemes")
#install.packages("bslib")
#install.packages("viridis")
#install.packages("plotly")
#install.packages("plyr")
#install.packages("chromoMap")


library(shiny)
library(palmerpenguins)
library(shinythemes)
library(bslib)
library(ggplot2)
library(dplyr)
library(viridis)
library(plotly)
library(ggplot2)
library(dplyr)
library(viridis)
library(plotly)
library(plyr)
library(DT)

lupus_cat <- read.csv("/Users/gonzalac/OneDrive - KAUST/gwas_lupus_filtrado.csv")
lupus_cat ##1071
##Filtramos for p values
lupus_cat <- lupus_cat[lupus_cat$P.VALUE < 1e-6,]
lupus_cat  ##828
##Filtramos empty chromosomes

##Ordenamos X axis
lupus_cat$CHR_ID <- factor(lupus_cat$CHR_ID, levels = c("1","2","3","4","5","6","7","8","9",
                                                        "10","11","12","13","14","15","16","17","18","19","20","21","22","X","Y"))

lupus_cat[['DATE']] <- as.POSIXct(lupus_cat[['DATE']],
                                  format = "%m/%d/%Y")



ddf = read.table(text="
country value
Denmark	1
France	3.32
Finland	3.6
Greece	1.9
Iceland	3.3
Italy	2
Norway	2.9
Spain	2.2
Sweden	1
UK	4
Canada	3
USA	1.9
Argentina	6.3
Brazil	8.7
Zimbabwe	0.3
China	3.1
Kazakhastan	1.3
Russia	1.2
Ukraine	0.3
SouthKorea	2.8
Taiwan	8.1
Australia	11", header=T)



age <- c("<18" ,  "19-29" ,  "30-39" ,  "40-49" ,  ">50" )
white <- c(44 ,  52 ,  37 ,  19 ,  37)
african <- c(18 ,  35 ,  32 ,  24 ,  26)
asian <- c(49 , 76, 50, 38, 52)
hispanic <- c(17, 39, 18, 21, 14)

df_age <- data.frame(age, white, african, asian, hispanic)
df_age$age <- factor(df_age$age, levels = c("<18" ,  "19-29" ,  "30-39" ,  "40-49" ,  ">50"))


sex <- c("male" ,  "female" )
white <- c(17, 172)
african <- c(13, 122)
asian <- c(32, 233)
hispanic <- c(12, 97)

df_sex <- data.frame(sex, white, african, asian, hispanic)




############ UI ############
#theme = "bootstrap.min.css"

ui <- fluidPage(theme = bslib::bs_theme(bootswatch = "minty"),
                navbarPage(
                  "P2 Shiny App",id = "home_tab",
                  tabPanel(title = "Home",
                           img(src = "lupus.png", height = 270, width = "100%"),
                           
                           titlePanel(strong("Systemic 
                     Lupus Erythematosus (SLE)",style = "color:indigo")),
                           
                           br(),
                           strong("What is lupus?",style = "color:indigo"),
                           p("Lupus is a chronic (long-term) disease that can cause pain and inflammation in any part of the body."),
                           
                           strong("What is the role of genetics in lupus?",style = "color:indigo"),
                           p("Genes do play a role in the predisposition to the development of lupus. There are dozens of 
             known genetic variants linked to lupus. These genes impact both who gets lupus and how severe it is.
             20 percent of people with lupus will have a parent or sibling who already has lupus or may develop 
             lupus. About 5 percent of the children born to individuals with lupus will develop the illness.
             Although lupus can develop in people with no family history of lupus, there are likely to be other
             autoimmune diseases in some family members."),
                           actionButton('go_genomic_tab', 'More information about Lupus Genomics', 
                                        style="color: #fff; background-color: #cabdd6; border-color: white"),
                           
                           br(),
                           br(),
                           
                           strong("How common is lupus and who does it affect?",style = "color:indigo"),
                           p(" The Lupus Foundation of America estimates that at least five million people worldwide, have a form of lupus.
             Lupus strikes mostly women of childbearing age. However, men, children, and teenagers develop lupus, too. 
               Most people with lupus develop the disease between the ages of 15-44."),
                           actionButton('go_demographics_tab', 'More information about Demographics ',
                                        style="color: #fff; background-color: #cabdd6; border-color: white"),
                           actionButton('go_map_tab', 'More information about Global incidence ',
                                        style="color: #fff; background-color: #cabdd6; border-color: white"),
                           br(),
                           br(),
                           br(),
                           
                           
                  ),
                  navbarMenu("Analysis",
                             ############ GENOMIC
                             tabPanel(title ="Genomic", value = "genomic_tab",
                                      
                                      titlePanel(strong("Role of Genetics on SLE",style = "color:indigo")),
                                      sidebarLayout(
                                        sidebarPanel(
                                        
                                        selectInput("selectMC", label = h5("Select molecular consequence of mutation"), 
                                                    choices = list("synonymous_variant", "regulatory_region_variant", 
                                                      "non_coding_transcript_exon_variant","missense_variant", "intron_variant", "intergenic_variant"), 
                                                    selected = "synonymous_variant"),
                                        hr(),
                                        
                                        dateInput("date", label = h5("Select date of publication"), value = "2015-01-01")

                                        
                                        ),
                                        
                                        mainPanel(
                                          plotlyOutput("gen_map")
                                        )
                                      )
                             ),
                             
                             
                             ############ DEMOGRAPHIC
                             tabPanel("Demographic", value = "demographic_tab",
                                      titlePanel(strong("Who does it affect?",style = "color:indigo")),
                                      sidebarLayout(
                                        sidebarPanel(
                                          selectInput(
                                            inputId = "GroupRace",
                                            label = "Select ethnicity",
                                            choices = c("white",	"african",	"asian",	"hispanic"), selected = "hispanic"),
                                          p("Characteristics of prevalent cases of Sistematic Lupus Erythematosus in San Francisco County, 2007 – 2009."),
                                          
                                          p("Data obtained from:"),
                                          p("Maningding E, Dall'Era M, et al., Racial and Ethnic Differences in the Prevalence and
                   Time to Onset of Manifestations of Systemic Lupus Erythematosus: The California Lupus Surveillance Project. 
                   Arthritis Care Res). 2020 May;72(5):622-629.")
                                        ),
                                        mainPanel(
                                          fluidRow(
                                            splitLayout(cellWidths = c("50%", "50%"),plotOutput("barplot_age"),
                                                        plotOutput("barplot_sex"))
                                          )))),
                             
                             ############ MAP
                             tabPanel("Global Incidence", value = "global_tab",
                                      titlePanel(strong("Global Incidence",style = "color:indigo")),
                                      
                                      sidebarLayout(
                                        sidebarPanel(
                                          p("Incidence ranges for SLE per location 
                     (per 100,000 of the population), as denoted by the key. 
                     Not all data have been collected and reported uniformly across global regions."),
                                          p("Data obtained from:"),
                                          p("Frances Rees, Michael Doherty, Matthew J Grainge, et al., The worldwide incidence and prevalence of systemic 
                   lupus erythematosus: a systematic review of epidemiological studies, Rheumatology, 2017 Nov;56(11):1945-1961.", style = "font-family: 'times'; font-si16pt")),
                                        mainPanel(
                                          plotlyOutput("map_lupus"))
                                      )
                             )
                  )))


############ SERVER ############
server <- function(input, output, session){
  
  
  ###### MANHATTAN PLOT
  observeEvent(input$go_genomic_tab, {
    updateTabsetPanel(session, "home_tab",
                      selected = "genomic_tab")
  })
  
  observeEvent(input$go_demographics_tab, {
    updateTabsetPanel(session, "home_tab",
                      selected = "demographic_tab")
  })
  
  observeEvent(input$go_map_tab, {
    updateTabsetPanel(session, "home_tab",
                      selected = "global_tab")
  })


  output$gen_map <-renderPlotly({
    lupus_cat <- lupus_cat[lupus_cat$CONTEXT == input$selectMC,]
    #lupus_cat <- lupus_cat[lupus_cat$DATE > as.Date(input$date),]
    
    lupus_cat <- subset(lupus_cat, as.Date(DATE) >=input$date)
    
    p<-ggplot(data=lupus_cat, aes(x=CHR_ID, y=PVALUE_MLOG, color=CHR_ID, 
                                  SNPS=SNPS, REGION=REGION, P.VALUE = P.VALUE, DATE = DATE, PUBMEDID = PUBMEDID)) + 
      geom_point(size = 3, alpha = 0.55) + labs(x = "Genomic Position (number denotes chromosome)", 
                                                y = "-log10 p-value") + 
      theme_classic() + theme(legend.position="none")
    
    ggplotly(p, tooltip = c("SNPS", "REGION", "P.VALUE","DATE", "PUBMEDID")) 
    
  })

  
  
  ###### BARPLOT AGE
  output$barplot_age <-renderPlot({
    ggplot(df_age, aes_string(x="age",y=input$GroupRace, fill = "age"))  +
      geom_bar(stat="identity", width=0.5) +
      theme_classic()+
      labs(title=input$Diseases, y ="Number")+
      theme(text = element_text(size = 14),legend.position="none") +
      scale_fill_brewer(palette="Purples")
  })
  
  ###### BARPLOT SEX
  output$barplot_sex <-renderPlot({
    dat <- sym(req(input$GroupRace))
    
    ggplot(df_sex, aes(x="", y=!!dat, fill = sex))  +
      geom_bar(stat="identity", width = 1, color="white") +
      coord_polar("y", start=0)+ 
      labs(x = NULL, y = NULL)+
      theme_classic() +
      theme(text = element_text(size = 14), axis.line = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank()) +
      scale_fill_brewer(palette="Purples")
  })
  
  
  ###### MAP
  output$map_lupus <- renderPlotly({
    world <- map_data("world")
    world2 <- world %>%
      merge(ddf, by.x = "region", by.y = "country", all.x = T) %>%
      arrange(group, order)
    map <- ggplot(world2, aes(x = long, y = lat, group = group, fill = value, region =region)) +
      geom_polygon(color = "white", size = 0.2) +
      scale_fill_viridis("", na.value = "gray90") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            panel.grid = element_blank())
    ggplotly(map, tooltip = c("region", "value"))
    
  })
}


############ RUN APP ############

shinyApp(ui, server)

