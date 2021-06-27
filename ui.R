library(shiny)
library(shinydashboard)
library(ggplot2)
library(modeest)
library(ggfortify)
library(FactoMineR)

list.of.packages <- c("shiny","shinydashboard","DT", "ggplot2", "modeest","GGally",
                      "psych","Hmisc","MASS","ggfortify","car","factoextra","tidyverse"
)


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# load all these
lapply(list.of.packages, require, character.only = TRUE)

dashboardPage(
    dashboardHeader(),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Data", tabName = "data", icon = icon("folder-open")),
            menuItem("Summary", tabName = "summary"),
            menuItem("Descriptius", tabName = "descriptius"),
            menuItem("Plots", tabName = "plots",icon = icon("line-chart"),
                     menuSubItem("Histograma", tabName = "histograma"),
                     menuSubItem("Boxplot", tabName = "boxplot"),
                     menuSubItem("Densitat", tabName = "densitat"),
                     menuSubItem("Sectors", tabName = "sectors"),
                     menuSubItem("Barres", tabName = "barres")

            ),
            menuItem("Matriu gràfiques de dispersió", tabName = "correlacio"),
            menuItem("Regressió Lineal", tabName = "regresio"),
            menuItem("PCA", tabName = "pca",
                     menuSubItem("PCA Plots", tabName = "pca_plot"),
                     menuSubItem("PCA Info", tabName = "pca_info")
            )

        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "data",
                    fileInput("file1", "Escull l'arxiu",
                              multiple = FALSE,
                              accept = c("text/csv",
                                         "text/comma-separated-values,text/plain",
                                         ".csv")),
                    helpText("Els arxius .csv s'ha de seleccionar separador punt i coma"),

                    #tags$hr(),
                    checkboxInput("header", "Capçalera", TRUE),
                    radioButtons("sep", "Separador",
                                 choices = c(Comma = ",",
                                             "Punt i coma" = ";",
                                             Tab = "\t"),
                                 selected = ","),


                    DT::dataTableOutput("contents")

            ),
            tabItem(tabName = "summary",
                    verbatimTextOutput("summary")
            ),
            tabItem(tabName = "descriptius",
                    uiOutput("select_variable"),
                    tableOutput("univ_table")
            ),
            tabItem(tabName = "plots"
            ),
            tabItem(tabName = "histograma",
                    uiOutput("hist_var"),

                    sliderInput(inputId = "bins",
                                label = "Number of bins:",
                                min = 1,
                                max = 50,
                                value = 30),

                    plotOutput("hist_plot"),
                    radioButtons(inputId = "tipo_hist", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "hist_down", label = "Descarregar")
            ),
            tabItem(tabName = "boxplot",
                    uiOutput("box_var"),
                    plotOutput("box_plot"),
                    radioButtons(inputId = "tipo_box", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "box_down", label = "Descarregar")
            ),
            tabItem(tabName = "densitat",
                    uiOutput("dens_var"),
                    plotOutput("dens_plot"),
                    radioButtons(inputId = "tipo_dens", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "dens_down", label = "Descarregar")
            ),
            tabItem(tabName = "sectors",
                    uiOutput("sec_var"),
                    plotOutput("sec_plot"),
                    radioButtons(inputId = "tipo_sect", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "sect_down", label = "Descarregar")
            ),
            tabItem(tabName = "barres",
                    uiOutput("bar_var"),
                    plotOutput("bar_plot"),
                    radioButtons(inputId = "tipo_bar", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "bar_down", label = "Descarregar")
            ),
            tabItem(tabName = "correlacio",
                    uiOutput("corr_var"),
                    helpText("El procès pot trigar."),
                    tableOutput("taula_corr"),
                    plotOutput("corr_plot"),
                    radioButtons(inputId = "tipo_corr", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "corr_down", label = "Descarregar")
            ),
            tabItem(tabName = "regresio",
                    uiOutput("xvariable"),
                    uiOutput("yvariable"),
                    verbatimTextOutput('lmSummary'),
                    plotOutput('diagnosticPlot'),
                    radioButtons(inputId = "tipo_regre", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "regre_down", label = "Descarregar")
            ),
            tabItem(tabName = "pca"),
            tabItem(tabName = "pca_plot",
                    uiOutput("choose_columns_pca"),
                    radioButtons(inputId = 'center',
                                 label = 'Centrar',
                                 choices = c('Centrar les variables a media 0'='Yes',
                                             'No centrar les variables'='No'),
                                 selected = 'Yes'),

                    radioButtons('scale.', 'Escalar',
                                 choices = c('Escalar les variables perquè tinguin variància unitària'='Yes',
                                             'No escalar les variables'='No'),
                                 selected = 'Yes'),
                    h4("Visualització dels valors propis (scree plot)."),
                    actionButton("info_scree", "Més informació"),
                    # h5("Mostra el % de variància explicat per cada component principal. Quan el gràfic
                    #    sedimenta (es fa pla), és a dir, ja no s'incorpora un % de variància suficient,
                    #    aquestes variables es descarten perquè es consideren \"soroll\". A la comopnent on
                    #    hi hagi acumulat un 70% o més, es para."),
                    plotOutput("eig_plot"),
                    radioButtons(inputId = "tipo_eig", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "eig_down", label = "Descarregar"),

                    h4("Gràfic de variables en l'espai de les dues primeres components."),
                    actionButton("info_var", "Més informació"),
                    # h5("Les variables amb correlació positiva tenen entre sí un angle
                    # més petit, en canvi quan l'angle s'aproxima a pi/2 vol dir que estan incorrelacionades.
                    # Quan l'angle s'aproxima a pi les variables estan correlacionades però negativament.
                    #    En referència a la llargada de la fletxa, quan més llarga és, més qualitat de
                    #    representació té aquella variable."),
                    plotOutput("var_pca_plot"),
                    radioButtons(inputId = "tipo_var", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "var_pca_down", label = "Descarregar"),

                    h4("Gràfic d'individus amb les dos primeres components."),
                    actionButton("info_ind", "Més informació"),
                    # h5("Es representa el seu cos2. Els individus amb més alta qualitat bidimensional es poden
                    # interpretar, els altres els veiem centrats però tenen una part en dimensió 3 o altres.
                    # La graduació de color representa la qualitat"),
                    plotOutput("ind_plot"),
                    radioButtons(inputId = "tipo_ind", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "ind_down", label = "Descarregar"),

                    h4("Gràfic d'individus interactuable."),
                    uiOutput("the_grouping_variable"),
                    uiOutput("the_pcs_to_plot_x"),
                    uiOutput("the_pcs_to_plot_y"),
                    actionButton("info_ind_grup", "Més informació"),
                    # h5("Es dibuixen unes elipses que intenten recollir la majoria de les dades de cada grup.
                    #    Si estan molt superposades vol dir que els grups no estan bé discriminats en aquesta
                    #    dimensió. Les elipses intenten reproduir una gaussiana, ja que les components principals
                    #    es distribueixen de forma gausiana."),
                    plotOutput ("z_plot1", height = 400),
                    radioButtons(inputId = "tipo", label="Selecciona el format de l'arxiu a descarregar", choices = list("png", "pdf")),
                    downloadButton(outputId = "z_down", label = "Descarregar")

            ),

            tabItem(tabName = "pca_info",
                    uiOutput("choose_pca_var"),
                        actionButton("infovar", "Més informació"),
                        verbatimTextOutput("pca_var"),
                    uiOutput("choose_pca_ind"),
                        actionButton("infoind", "Més informació"),
                        verbatimTextOutput("pca_ind")



            )


        )
    )
)






