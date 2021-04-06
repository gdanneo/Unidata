# Load packages ----
library(shiny)
library(maps)
library(mapproj)
library(ggplot2)
library(ggthemes)
library(plotly)
library(forcats)
library(kableExtra)

# Load data ----
load("fileforshiny.RDATA")
load("for_app2.RDATA")
load("quadroucq.RDATA")

# User interface ----
ui <- navbarPage("PalermoStat", 
                 tabPanel("Mappe, grafici e tabelle",
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style("#title_table{color: darkblue;
                              font-size: 20px;
                              }"
                              )
                              ),
                              
                              tags$head(tags$style("#metadati{color: gray;
                              font-size: 20px;
                              }"
                              )
                              ),
                              
                              helpText("Distribuzione per le diverse unita' di decentramento
                              di alcuni indicatori statistici,
                              basati sui dati dell'anagrafe al 31/12/2020
                              o del Censimento della popolazione del 2011, attraverso
                              mappe coropletiche, grafici e tabelle." , br(),br(), 
                              "Seleziona il tipo di unita' di decentramento 
                              (Circoscrizione, Quartiere o Unita' di primo livello) e l'indicatore", br(), br()),
                              
                              radioButtons("terr", 
                                           label = "Scegli il livello di decentramento",
                                           choices = c( "Circoscrizione",
                                                        "Quartiere",
                                                        "Unita' di primo livello"),
                                           selected = "Circoscrizione"),
                              
                              selectInput("var", 
                                          label = "Scegli l'indicatore",
                                          choices = c("Densita' (ab. per ettaro)",
                                                      "Perc. Stranieri",
                                                      "perc. 0-14 anni",
                                                      "perc. 65 anni e piu'",
                                                      "Indice di dipendenza strutturale dei giovani",
                                                      "Indice di dipendenza strutturale degli anziani",
                                                      "Indice di vecchiaia",
                                                      "Indice di ricambio generazionale",
                                                      "n. medio componenti",
                                                      "perc. 1 componente",
                                                      "perc. 5 componenti o piu'",
                                                      "Perc. analfabeti",
                                                      "Perc. laureati",
                                                      "Tasso di attivita'",
                                                      "Tasso di occupazione",
                                                      "Tasso di disoccupazione",
                                                      "Perc. edifici ottimo o buono stato conservazione",
                                                      "Perc. edifici pessimo o mediocre stato conservazione"),
                                          selected = "Densita' (ab. per ettaro)")),
                            
                            mainPanel(h1("L'identita' statistica della Citta' di Palermo", 
                                         align = "left", style = "color:darkblue"),
                                      tabsetPanel(
                                        tabPanel("Mappa", plotlyOutput("map", height="580px")), 
                                        tabPanel("Grafico", plotlyOutput("graph", height="600px")), 
                                        tabPanel("Tabella", textOutput("title_table"), htmlOutput("table")),
                                        tabPanel("Metadati", textOutput("metadati")),
                                        tabPanel("Quadro d'insieme", htmlOutput("quadro"))
                                      )
                            )
                 )),
                 
                 tabPanel("La Citta' di Palermo",
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style("#title_palermo{color: darkblue;
                              font-size: 24px;
                              }"
                              )
                              ),
                              
                              helpText("La scheda completa di variabili e
                              indicatori statistici riferiti all'intera Citta' di Palermo, 
                              basati sui dati dell'anagrafe al 31/12/2020 
                              o del Censimento della popolazione del 2011.", br(), br(), 
                              "N.B.: se non diversamente indicato, i dati sono frutto di elaborazioni 
                              statistiche sull'archivio anagrafico, e possono differire 
                              dai dati demografici pubblicati dall'Istat")),
                              
                          mainPanel(h1("L'identita' statistica del territorio", 
                                       align = "left", style = "color:darkblue"),
                            textOutput("title_palermo"), htmlOutput("palermo")
                          ))),

                 
                 tabPanel("Le 8 Circoscrizioni",
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style("#title_circ{color: darkblue;
                              font-size: 24px;
                              }"
                              )
                              ),
                              

                              helpText("Seleziona la Circoscrizione e ottieni una scheda completa di variabili e indicatori statistici, 
                              basati sui dati dell'anagrafe al 31/12/2020 
                              o del Censimento della popolazione del 2011", br(), br()),
                              
                              
                              selectInput("circ", 
                                          label = "Seleziona la Circoscrizione",
                                          choices = c("I Circoscrizione",
                                                      "II Circoscrizione",
                                                      "III Circoscrizione",
                                                      "IV Circoscrizione",
                                                      "V Circoscrizione",
                                                      "VI Circoscrizione",
                                                      "VII Circoscrizione",
                                                      "VIII Circoscrizione"),
                                          selected = "I Circoscrizione")
                              

                              ),
                            
                          mainPanel(h1("L'identita' statistica del territorio", align = "left", style = "color:darkblue"),
                                      textOutput("title_circ"), htmlOutput("table_circ")
                                      ))),
                 
                 tabPanel("I 25 Quartieri",
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style("#title_quart{color: darkblue;
                              font-size: 24px;
                              }"
                              )
                              ),
                              
                              helpText("Seleziona il Quartiere e ottieni una scheda completa di variabili e indicatori statistici, 
                              basati sui dati dell'anagrafe al 31/12/2020 
                              o del Censimento della popolazione del 2011", br(), br()),
                              
                              
                              selectInput("quart", 
                                          label = "Seleziona il Quartiere",
                                          choices = c("TRIBUNALI-CASTELLAMMARE",
                                                      "PALAZZO REALE - MONTE DI PIETA'",
                                                      "ORETO- STAZIONE",
                                                      "MONTEGRAPPA - S. ROSALIA",
                                                      "CUBA-CALATAFIMI",
                                                      "ZISA",
                                                      "NOCE",
                                                      "MALASPINA -PALAGONIA",
                                                      "LIBERTA'",
                                                      "POLITEAMA",
                                                      "SETTECANNOLI",
                                                      "BRANCACCIO-CIACULLI",
                                                      "VILLAGRAZIA-FALSOMIELE",
                                                      "MEZZOMONREALE -VILLA TASCA",
                                                      "ALTARELLO",
                                                      "BOCCADIFALCO",
                                                      "UDITORE-PASSO DI RIGANO",
                                                      "BORGO NUOVO",
                                                      "CRUILLAS - S.GIOVANNI APOSTOLO",
                                                      "RESUTTANA - S.LORENZO",
                                                      "TOMMASO NATALE - SFERRACAVALLO",
                                                      "PARTANNA MONDELLO",
                                                      "PALLAVICINO",
                                                      "MONTEPELLEGRINO",
                                                      "ARENELLA - VERGINE MARIA"),
                                          selected = "TRIBUNALI-CASTELLAMMARE")),
                              
                             
                          mainPanel(h1("L'identita' statistica del territorio", align = "left", style = "color:darkblue"),
                                      textOutput("title_quart"), htmlOutput("table_quart")
                                    ))),
                       

                 tabPanel("Le 55 Unita' di primo livello",
                          sidebarLayout(
                            sidebarPanel(
                              tags$head(tags$style("#title_upl{color: darkblue;
                              font-size: 24px;
                              }"
                              )
                              ),
                              
                              
                              helpText("Seleziona l'Unita' di primo livello e ottieni una scheda completa di variabili e indicatori statistici, 
                              basati sui dati dell'anagrafe al 31/12/2020 
                              o del Censimento della popolazione del 2011", br(), br()),
                              
                              selectInput("upl", 
                                          label = "Seleziona l'Unita' di primo livello",
                                          choices = c("Tribunali o Kalsa",
                                                      "Palazzo Reale o Albergaria",
                                                      "Monte di Pieta o Seralcadi",
                                                      "Castellammare o Loggia",
                                                      "Corso dei Mille - S.Erasmo",
                                                      "Settecannoli",
                                                      "Roccella Acqua dei Corsari",
                                                      "Brancaccio Conte Federico",
                                                      "Ciaculli Croce Verde",
                                                      "Oreto Perez",
                                                      "Oreto Guadagna",
                                                      "Falsomiele Borgo Ulivia",
                                                      "Bonagia",
                                                      "Chiavelli - S.Maria di Gesu",
                                                      "Villagrazia",
                                                      "Montegrappa",
                                                      "S.Rosalia",
                                                      "Cuba - Calatafimi",
                                                      "Villa Tasca",
                                                      "Mezzomonreale",
                                                      "Zisa - Ingastone",
                                                      "Zisa - 4 Camere",
                                                      "Altarello - Tasca Lanza",
                                                      "Boccadifalco - Baida",
                                                      "Borgo Vecchio - Principe Scordia",
                                                      "Croci - Ruggiero Settimo",
                                                      "S.Francesco di Paola - Terrasanta",
                                                      "Olivuzza",
                                                      "Parlatore - Serradifalco",
                                                      "Noce",
                                                      "Leonardo da Vinci - Di Blasi",
                                                      "Malaspina - Leonardo da Vinci",
                                                      "Principe di Palagonia",
                                                      "Uditore",
                                                      "Passo di Rigano",
                                                      "Borgo Nuovo",
                                                      "San Giovanni Apostolo",
                                                      "Cruillas",
                                                      "Notarbartolo - Giardino Inglese",
                                                      "Villa Sperlinga",
                                                      "Vittorio Veneto",
                                                      "Marchese di Villabianca - Sampolo",
                                                      "Resuttana",
                                                      "San Lorenzo",
                                                      "Patti - Villaggio Ruffini",
                                                      "Pallavicino",
                                                      "San Filippo Neri",
                                                      "Tommaso Natale - Sant'Ambrogio - Cardillo",
                                                      "Sferracavallo",
                                                      "Partanna Mondello",
                                                      "Cantieri",
                                                      "Montepellegrino",
                                                      "Acquasanta",
                                                      "Arenella",
                                                      "Vergine Maria"),
                                          selected = "Tribunali o Kalsa")),
                            
                            mainPanel(h1("L'identita' statistica del territorio", align = "left", style = "color:darkblue"),
                                      textOutput("title_upl"), htmlOutput("table_upl")
                                      ))))
                

# Server logic ----
server <- function(input, output, session) {
  output$map <- renderPlotly( {
    data <- switch(input$terr, 
                   "Unita' di primo livello" = upl_fortified,
                   "Quartiere" = q_fortified,
                   "Circoscrizione" = circo_fortified)
    
    area <- switch(input$terr, 
                    "Unita' di primo livello" = upl_fortified$Upl,
                    "Quartiere" = q_fortified$Quartiere,
                    "Circoscrizione" = circo_fortified$Circoscrizione)
    
    indice <- switch(input$var,
                     "Densita' (ab. per ettaro)"=data$`Densita' (ab. per ettaro)`,
                     "Perc. Stranieri"=data$`Perc. Stranieri`,
                     "perc. 0-14 anni"=data$`perc. 0-14 anni`,
                     "perc. 65 anni e piu'"=data$`perc. 65 anni e piu'`,
                     "Indice di dipendenza strutturale dei giovani"=data$`Indice di dipendenza strutturale dei giovani`,
                     "Indice di dipendenza strutturale degli anziani"=data$`Indice di dipendenza strutturale degli anziani`,
                     "Indice di vecchiaia"=data$`Indice di vecchiaia`,
                     "Indice di ricambio generazionale"=data$`Indice di ricambio generazionale`,
                     "n. medio componenti"=data$`n. medio componenti`,
                     "perc. 1 componente"=data$`perc. 1 componente`,
                     "perc. 5 componenti o piu'"=data$`perc. 5 componenti o piu'`,
                     "Perc. analfabeti"=data$`Perc. analfabeti`,
                     "Perc. laureati"=data$`Perc. laureati`,
                     "Tasso di attivita'"=data$`Tasso di attivita'`,
                     "Tasso di occupazione"=data$`Tasso di occupazione`,
                     "Tasso di disoccupazione"=data$`Tasso di disoccupazione`,
                     "Perc. edifici ottimo o buono stato conservazione"=data$`Perc. edifici ottimo o buono stato conservazione`,
                     "Perc. edifici pessimo o mediocre stato conservazione"=data$`Perc. edifici pessimo o mediocre stato conservazione`)
    
    titolo <- switch(input$var,
                     "Densita' (ab. per ettaro)"= "Densita' della popolazione (abitanti per ettaro)",
                     "Perc. Stranieri"="Percentuale di Stranieri sul totale della popolazione",
                     "perc. 0-14 anni"="Popolazione residente di età 0-14 anni in percentuale sul totale",
                     "perc. 65 anni e piu'"="Popolazione residente di 65 anni e piu' in percentuale sul totale",
                     "Indice di dipendenza strutturale dei giovani"="Indice di dipendenza strutturale dei giovani",
                     "Indice di dipendenza strutturale degli anziani"="Indice di dipendenza strutturale degli anziani",
                     "Indice di vecchiaia"="Indice di vecchiaia",
                     "Indice di ricambio generazionale"="Indice di ricambio generazionale",
                     "n. medio componenti"="Famiglie: numero medio componenti",
                     "perc. 1 componente"="Percentuale di famiglie con un componente sul totale",
                     "perc. 5 componenti o piu'"="Percentuale di famiglie con 5 o piu' componenti sul totale",
                     "Perc. analfabeti"="Percentuale di analfabeti su totale popolazione di 6 anni e piu'",
                     "Perc. laureati"="Percentuale di laureati su totale popolazione di 6 anni e piu'",
                     "Tasso di attivita'"="Mercato del lavoro: tasso di attivita'",
                     "Tasso di occupazione"="Mercato del lavoro: tasso di occupazione",
                     "Tasso di disoccupazione"="Mercato del lavoro: tasso di disoccupazione",
                     "Perc. edifici ottimo o buono stato conservazione"="Percentuale edifici in ottimo o buono stato di conservazione",
                     "Perc. edifici pessimo o mediocre stato conservazione"="Percentuale edifici in pessimo o mediocre stato di conservazione")
    
    sottotitolo <- switch(input$terr, 
                   "Unita' di primo livello" = "per Unita' di primo livello",
                   "Quartiere" = "per Quartiere",
                   "Circoscrizione" = "per Circoscrizione")
    
    fonte <- switch(input$var,
                    "Densita' (ab. per ettaro)"= "Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. Stranieri"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 0-14 anni"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 65 anni e piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale dei giovani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale degli anziani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di vecchiaia"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di ricambio generazionale"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "n. medio componenti"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 1 componente"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 5 componenti o piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. analfabeti"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. laureati"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di attivita'"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di occupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di disoccupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici ottimo o buono stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici pessimo o mediocre stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011")
    
    
    a <- ggplot() +
      geom_polygon(data = data, aes(label = area, fill = indice, x = long, y = lat, group = group ), color="white") +
      theme_void() +
      labs(title = titolo,
           subtitle = sottotitolo,
           caption = fonte) +
      theme(text = element_text(color = "#22211d"),
            plot.title = element_text(size= 15, hjust=0.0, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            plot.subtitle = element_text(size= 15, hjust=0.0, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            plot.caption = element_text( size=15, color = "#4e4d47", margin = margin(b = 0.3, r = -99, unit = "cm"))) +
      scale_fill_gradient(name="",low = "light blue", high = "dark blue")+
      coord_map()
    
    ggplotly(a, tooltip = c("indice", "area")) %>%
      layout(title = list(text = paste0(titolo,
                                        '<br>',
                                        '<sup>',
                                        sottotitolo,
                                        '</sup>'))) %>%
      layout(annotations = 
               list(x = 1, y = 0, text = fonte, 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=12, color="darkblue"))
      )
  
  })
  
  output$graph <- renderPlotly({
    data <- switch(input$terr, 
                   "Unita' di primo livello" = tavola_complessiva_upl,
                   "Quartiere" = tavola_complessiva_quart,
                   "Circoscrizione" = tavola_complessiva_circ)
    
    area <- switch(input$terr, 
                   "Unita' di primo livello" = tavola_complessiva_upl$Upl,
                   "Quartiere" = tavola_complessiva_quart$Quartiere,
                   "Circoscrizione" = tavola_complessiva_circ$Circoscrizione)
    
    indice <- switch(input$var,
                     "Densita' (ab. per ettaro)"=data$`Densita' (ab. per ettaro)`,
                     "Perc. Stranieri"=data$`Perc. Stranieri`,
                     "perc. 0-14 anni"=data$`perc. 0-14 anni`,
                     "perc. 65 anni e piu'"=data$`perc. 65 anni e piu'`,
                     "Indice di dipendenza strutturale dei giovani"=data$`Indice di dipendenza strutturale dei giovani`,
                     "Indice di dipendenza strutturale degli anziani"=data$`Indice di dipendenza strutturale degli anziani`,
                     "Indice di vecchiaia"=data$`Indice di vecchiaia`,
                     "Indice di ricambio generazionale"=data$`Indice di ricambio generazionale`,
                     "n. medio componenti"=data$`n. medio componenti`,
                     "perc. 1 componente"=data$`perc. 1 componente`,
                     "perc. 5 componenti o piu'"=data$`perc. 5 componenti o piu'`,
                     "Perc. analfabeti"=data$`Perc. analfabeti`,
                     "Perc. laureati"=data$`Perc. laureati`,
                     "Tasso di attivita'"=data$`Tasso di attivita'`,
                     "Tasso di occupazione"=data$`Tasso di occupazione`,
                     "Tasso di disoccupazione"=data$`Tasso di disoccupazione`,
                     "Perc. edifici ottimo o buono stato conservazione"=data$`Perc. edifici ottimo o buono stato conservazione`,
                     "Perc. edifici pessimo o mediocre stato conservazione"=data$`Perc. edifici pessimo o mediocre stato conservazione`)
    
    titolo <- switch(input$var,
                     "Densita' (ab. per ettaro)"= "Densita' della popolazione (abitanti per ettaro)",
                     "Perc. Stranieri"="Percentuale di Stranieri sul totale della popolazione",
                     "perc. 0-14 anni"="Popolazione residente di età 0-14 anni in percentuale sul totale",
                     "perc. 65 anni e piu'"="Popolazione residente di 65 anni e piu' in percentuale sul totale",
                     "Indice di dipendenza strutturale dei giovani"="Indice di dipendenza strutturale dei giovani",
                     "Indice di dipendenza strutturale degli anziani"="Indice di dipendenza strutturale degli anziani",
                     "Indice di vecchiaia"="Indice di vecchiaia",
                     "Indice di ricambio generazionale"="Indice di ricambio generazionale",
                     "n. medio componenti"="Famiglie: numero medio componenti",
                     "perc. 1 componente"="Percentuale di famiglie con un componente sul totale",
                     "perc. 5 componenti o piu'"="Percentuale di famiglie con 5 o piu' componenti sul totale",
                     "Perc. analfabeti"="Percentuale di analfabeti su totale popolazione di 6 anni e piu'",
                     "Perc. laureati"="Percentuale di laureati su totale popolazione di 6 anni e piu'",
                     "Tasso di attivita'"="Mercato del lavoro: tasso di attivita'",
                     "Tasso di occupazione"="Mercato del lavoro: tasso di occupazione",
                     "Tasso di disoccupazione"="Mercato del lavoro: tasso di disoccupazione",
                     "Perc. edifici ottimo o buono stato conservazione"="Percentuale edifici in ottimo o buono stato di conservazione",
                     "Perc. edifici pessimo o mediocre stato conservazione"="Percentuale edifici in pessimo o mediocre stato di conservazione")
    
    sottotitolo <- switch(input$terr, 
                          "Unita' di primo livello" = "per Unita' di primo livello",
                          "Quartiere" = "per Quartiere",
                          "Circoscrizione" = "per Circoscrizione")
    
    fonte <- switch(input$var,
                    "Densita' (ab. per ettaro)"= "Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. Stranieri"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 0-14 anni"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 65 anni e piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale dei giovani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale degli anziani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di vecchiaia"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di ricambio generazionale"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "n. medio componenti"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 1 componente"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 5 componenti o piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. analfabeti"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. laureati"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di attivita'"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di occupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di disoccupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici ottimo o buono stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici pessimo o mediocre stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011")
    
    
    grafico <- data %>%
      #mutate(decentr = fct_reorder(decentr, n_uqc)) %>%
      ggplot(aes(x=area, y=indice))+
      geom_bar(stat="identity", fill="darkblue", alpha=1, width=.8) +
      #coord_flip()+
      xlab("") +
      theme_grey()+
      labs(title = titolo, 
           subtitle = sottotitolo,
           y="",
           caption = fonte) +
      theme(text = element_text(color = "#22211d"),axis.text.x = element_text(angle = 90, hjust = 1, size = 7),
            plot.title = element_text(size= 15, hjust=0.0, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            plot.subtitle = element_text(size= 15, hjust=0.0, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
            plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r = -99, unit = "cm")))

    
    print(ggplotly(grafico)) %>%
      layout(title = list(text = paste0(titolo,
                                        '<br>',
                                        '<sup>',
                                        sottotitolo,
                                        '</sup>'))) %>%
      layout(annotations = 
               list(x = 1, y = 1, text = fonte, 
                    showarrow = F, xref='paper', yref='paper', 
                    xanchor='right', yanchor='auto', xshift=0, yshift=0,
                    font=list(size=12, color="darkblue"))
      )
    

  })
  
  output$title_table <- renderText({
    titolo <- switch(input$var,
                     "Densita' (ab. per ettaro)"= "Densita' della popolazione (abitanti per ettaro)",
                     "Perc. Stranieri"="Percentuale di Stranieri sul totale della popolazione",
                     "perc. 0-14 anni"="Popolazione residente di età 0-14 anni in percentuale sul totale",
                     "perc. 65 anni e piu'"="Popolazione residente di 65 anni e piu' in percentuale sul totale",
                     "Indice di dipendenza strutturale dei giovani"="Indice di dipendenza strutturale dei giovani",
                     "Indice di dipendenza strutturale degli anziani"="Indice di dipendenza strutturale degli anziani",
                     "Indice di vecchiaia"="Indice di vecchiaia",
                     "Indice di ricambio generazionale"="Indice di ricambio generazionale",
                     "n. medio componenti"="Famiglie: numero medio componenti",
                     "perc. 1 componente"="Percentuale di famiglie con un componente sul totale",
                     "perc. 5 componenti o piu'"="Percentuale di famiglie con 5 o piu' componenti sul totale",
                     "Perc. analfabeti"="Percentuale di analfabeti su totale popolazione di 6 anni e piu'",
                     "Perc. laureati"="Percentuale di laureati su totale popolazione di 6 anni e piu'",
                     "Tasso di attivita'"="Mercato del lavoro: tasso di attivita'",
                     "Tasso di occupazione"="Mercato del lavoro: tasso di occupazione",
                     "Tasso di disoccupazione"="Mercato del lavoro: tasso di disoccupazione",
                     "Perc. edifici ottimo o buono stato conservazione"="Percentuale edifici in ottimo o buono stato di conservazione",
                     "Perc. edifici pessimo o mediocre stato conservazione"="Percentruale edifici in pessimo o mediocre stato di conservazione")
    
    paste(titolo, "per ", input$terr)
    
  })

  
  output$table <- renderText({
    data <- switch(input$terr, 
                   "Unita' di primo livello" = tavola_complessiva_upl,
                   "Quartiere" = tavola_complessiva_quart,
                   "Circoscrizione" = tavola_complessiva_circ)
    
    indice <- switch(input$var,
                     "Densita' (ab. per ettaro)"=data$`Densita' (ab. per ettaro)`,
                     "Perc. Stranieri"=data$`Perc. Stranieri`,
                     "perc. 0-14 anni"=data$`perc. 0-14 anni`,
                     "perc. 65 anni e piu'"=data$`perc. 65 anni e piu'`,
                     "Indice di dipendenza strutturale dei giovani"=data$`Indice di dipendenza strutturale dei giovani`,
                     "Indice di dipendenza strutturale degli anziani"=data$`Indice di dipendenza strutturale degli anziani`,
                     "Indice di vecchiaia"=data$`Indice di vecchiaia`,
                     "Indice di ricambio generazionale"=data$`Indice di ricambio generazionale`,
                     "n. medio componenti"=data$`n. medio componenti`,
                     "perc. 1 componente"=data$`perc. 1 componente`,
                     "perc. 5 componenti o piu'"=data$`perc. 5 componenti o piu'`,
                     "Perc. analfabeti"=data$`Perc. analfabeti`,
                     "Perc. laureati"=data$`Perc. laureati`,
                     "Tasso di attivita'"=data$`Tasso di attivita'`,
                     "Tasso di occupazione"=data$`Tasso di occupazione`,
                     "Tasso di disoccupazione"=data$`Tasso di disoccupazione`,
                     "Perc. edifici ottimo o buono stato conservazione"=data$`Perc. edifici ottimo o buono stato conservazione`,
                     "Perc. edifici pessimo o mediocre stato conservazione"=data$`Perc. edifici pessimo o mediocre stato conservazione`)
    
    titolo <- switch(input$var,
                     "Densita' (ab. per ettaro)"= "Densita' della popolazione (abitanti per ettaro)",
                     "Perc. Stranieri"="Percentuale di Stranieri sul totale della popolazione",
                     "perc. 0-14 anni"="Popolazione residente di età 0-14 anni in percentuale sul totale",
                     "perc. 65 anni e piu'"="Popolazione residente di 65 anni e piu' in percentuale sul totale",
                     "Indice di dipendenza strutturale dei giovani"="Indice di dipendenza strutturale dei giovani",
                     "Indice di dipendenza strutturale degli anziani"="Indice di dipendenza strutturale degli anziani",
                     "Indice di vecchiaia"="Indice di vecchiaia",
                     "Indice di ricambio generazionale"="Indice di ricambio generazionale",
                     "n. medio componenti"="Famiglie: numero medio componenti",
                     "perc. 1 componente"="Percentuale di famiglie con un componente sul totale",
                     "perc. 5 componenti o piu'"="Percentuale di famiglie con 5 o piu' componenti sul totale",
                     "Perc. analfabeti"="Percentuale di analfabeti su totale popolazione di 6 anni e piu'",
                     "Perc. laureati"="Percentuale di laureati su totale popolazione di 6 anni e piu'",
                     "Tasso di attivita'"="Mercato del lavoro: tasso di attivita'",
                     "Tasso di occupazione"="Mercato del lavoro: tasso di occupazione",
                     "Tasso di disoccupazione"="Mercato del lavoro: tasso di disoccupazione",
                     "Perc. edifici ottimo o buono stato conservazione"="Percentuale edifici in ottimo o buono stato di conservazione",
                     "Perc. edifici pessimo o mediocre stato conservazione"="Percentruale edifici in pessimo o mediocre stato di conservazione")
    
    sottotitolo <- switch(input$terr, 
                          "Unita' di primo livello" = "per Unita' di primo livello",
                          "Quartiere" = "per Quartiere",
                          "Circoscrizione" = "per Circoscrizione")
    
    fonte <- switch(input$var,
                    "Densita' (ab. per ettaro)"= "Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. Stranieri"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 0-14 anni"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 65 anni e piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale dei giovani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di dipendenza strutturale degli anziani"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di vecchiaia"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Indice di ricambio generazionale"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "n. medio componenti"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 1 componente"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "perc. 5 componenti o piu'"="Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                    "Perc. analfabeti"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. laureati"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di attivita'"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di occupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Tasso di disoccupazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici ottimo o buono stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011",
                    "Perc. edifici pessimo o mediocre stato conservazione"="Fonte: elaborazioni su risultati Censimento della popolazione 2011")
    
    tavola <- data %>%
      select(c(1,2), input$var)
    
    tavola %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F, fixed_thead = T, position = "left") %>%
      scroll_box(height = "550px") %>%
      footnote(general = fonte)
      
    
  })
  
  output$metadati <- renderText({
    titolo <- switch(input$var,
                     "Densita' (ab. per ettaro)"= "Densita' della popolazione (abitanti per ettaro)",
                     "Perc. Stranieri"="Percentuale di Stranieri sul totale della popolazione",
                     "perc. 0-14 anni"="Popolazione residente di età 0-14 anni in percentuale sul totale",
                     "perc. 65 anni e piu'"="Popolazione residente di 65 anni e piu' in percentuale sul totale",
                     "Indice di dipendenza strutturale dei giovani"="Indice di dipendenza strutturale dei giovani",
                     "Indice di dipendenza strutturale degli anziani"="Indice di dipendenza strutturale degli anziani",
                     "Indice di vecchiaia"="Indice di vecchiaia",
                     "Indice di ricambio generazionale"="Indice di ricambio generazionale",
                     "n. medio componenti"="Famiglie: numero medio componenti",
                     "perc. 1 componente"="Percentuale di famiglie con un componente sul totale",
                     "perc. 5 componenti o piu'"="Percentuale di famiglie con 5 o piu' componenti sul totale",
                     "Perc. analfabeti"="Percentuale di analfabeti su totale popolazione di 6 anni e piu'",
                     "Perc. laureati"="Percentuale di laureati su totale popolazione di 6 anni e piu'",
                     "Tasso di attivita'"="Mercato del lavoro: tasso di attivita'",
                     "Tasso di occupazione"="Mercato del lavoro: tasso di occupazione",
                     "Tasso di disoccupazione"="Mercato del lavoro: tasso di disoccupazione",
                     "Perc. edifici ottimo o buono stato conservazione"="Percentuale edifici in ottimo o buono stato di conservazione",
                     "Perc. edifici pessimo o mediocre stato conservazione"="Percentruale edifici in pessimo o mediocre stato di conservazione")
    
    metadato <- switch(input$var,
                     "Densita' (ab. per ettaro)"="popolazione residente al 31/12/2020 su superficie (espressa in ettari). 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Perc. Stranieri"="residenti con cittadinanza straniera su totale residenti al 31/12/2020, per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "perc. 0-14 anni"="residenti di eta' compresa fra 0 e 14 anni su totale residenti al 31/12/2020, per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "perc. 65 anni e piu'"="residenti di 65 anni e oltre su totale residenti al 31/12/2020, per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Indice di dipendenza strutturale dei giovani"="residenti di eta' compresa fra 0 e 14 anni (giovani) 
                     su residenti di eta' compresa fra 15 e 64 anni (persone in eta' lavorativa), per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Indice di dipendenza strutturale degli anziani"="residenti di 65 anni e oltre (anziani) 
                     su residenti di eta' compresa fra 15 e 64 anni (persone in eta' lavorativa), per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Indice di vecchiaia"="residenti di 65 anni e oltre (anziani) 
                     su residenti di eta' compresa fra 0 e 14 anni (giovani), per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Indice di ricambio generazionale"="residenti di eta' compresa fra 0 e 14 anni (giovani)
                     su residenti di 65 anni e oltre (anziani), per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "n. medio componenti"="Residenti in famiglia su numero complessivo di famiglie. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "perc. 1 componente"="numero di famiglie con un componente 
                     su numero complessivo di famiglie, per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "perc. 5 componenti o piu'"="numero di famiglie con 5 o piu' componenti 
                     su numero complessivo di famiglie, per 100. 
                     Fonte: elaborazioni su archivio anagrafico al 31/12/2020",
                     "Perc. analfabeti"="residenti di 6 anni e piu' che non sanno 
                     leggere o scrivere su totale residenti di 6 anni e piu', per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Perc. laureati"="residenti di 6 anni e piu' che hanno conseguito la laurea
                     su totale residenti di 6 anni e piu', per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Tasso di attivita'"="residenti di 15 anni e piu' appartenenti alle forze di lavoro 
                     (occupati + persone in cerca di occupazione)
                     su totale residenti di 15 anni e più, per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Tasso di occupazione"="residenti di 15 anni e piu' occupati
                     su totale residenti di 15 anni e più, per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Tasso di disoccupazione"="residenti di 15 anni e piu' in cerca di occupazione
                     su totale residenti di 15 anni e più, per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Perc. edifici ottimo o buono stato conservazione"="edifici residenziali in ottimo
                     o buono stato di conservazione su totale edifici residenziali, per 100. 
                     Fonte: Censimento generale della popolazione 2011",
                     "Perc. edifici pessimo o mediocre stato conservazione"="edifici residenziali in pessimo
                     o mediocre di conservazione su totale edifici residenziali, per 100. 
                     Fonte: Censimento generale della popolazione 2011")
    paste(titolo, ": ", metadato)
    
  })
  
  output$quadro <- renderText({
    tavola <- quadroucq
    
    tavola %>%
      knitr::kable("html") %>%
      kable_styling("striped", full_width = F, fixed_thead = T, position = "left") %>%
      scroll_box(height = "550px")
  })
  
  
  output$title_palermo <- renderText({
    
    paste("la Citta' di Palermo")
    
  })
  
  output$palermo <- renderTable({
    
    data <- tavola_generale_Palermo  
    
    data %>%
      rownames_to_column %>% 
      gather(var, value, -rowname) %>% 
      spread(rowname, value) %>%filter(var!="Palermo")%>%
      rename("Variabili"=var)%>%rename(" "=`1`)%>%
      arrange(factor(Variabili, levels = names(data)[-c(1)])) 

    
  })
  
  output$title_circ <- renderText({
    
    paste(input$circ)
    
  })
  
  output$table_circ <- renderTable({
    
    data <- tavola_generale_circ  
    
    circ <- input$circ
    
    
    data %>%dplyr::filter(`Circoscrizione` == input$circ)%>%
      rownames_to_column %>% 
      gather(var, value, -rowname) %>% 
      spread(rowname, value) %>%filter(var!="Circoscrizione")%>%
      filter(var!="N. Circ.")%>%
      rename("Variabili"=var)%>%rename(" "=`1`)%>%
      arrange(factor(Variabili, levels = names(data)[-c(1:2)])) 
    
  })
  
  
  output$title_quart <- renderText({
    
    paste("Quariere: ", input$quart)
    
  })
  
  output$table_quart <- renderTable({
    
    data <- tavola_generale_quart   
    
    quart <- input$quart
    
    
    data %>%dplyr::filter(`Quartiere` == input$quart)%>%
      rownames_to_column %>% 
      gather(var, value, -rowname) %>% 
      spread(rowname, value) %>%filter(var!="Quartiere")%>%
      filter(var!="N. Quart.")%>%
      rename("Variabili"=var)%>%rename(" "=`1`)%>%
      arrange(factor(Variabili, levels = names(data)[-c(1:2)])) 
    
  })
  
  
  output$title_upl <- renderText({
    
    paste("Unita' di primo livello: ", input$upl)
    
  })
  
  output$table_upl <- renderTable({
    
    data <- tavola_generale_upl  
    
    upl <- input$upl
    
    
    data %>%dplyr::filter(`Unita' di primo livello` == input$upl)%>%
      rownames_to_column %>% 
      gather(var, value, -rowname) %>% 
      spread(rowname, value) %>%filter(var!="Unita' di primo livello")%>%
      filter(var!="N. Upl")%>%
      rename("Variabili"=var)%>%rename(" "=`1`)%>%
      arrange(factor(Variabili, levels = names(data)[-c(1:2)])) 
    
  })
  

  
}

# Run app ----
shinyApp(ui, server)