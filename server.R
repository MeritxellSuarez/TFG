library(shiny)
library(shinydashboard)
library(ggplot2)
library(modeest)
library(ggfortify)
library(FactoMineR)

function(input, output) {
    myData <- reactive({
        inFile <- input$file1
        if (is.null(inFile)) return(NULL)
        data <- read.csv(inFile$datapath, header = input$header,
                         sep = input$sep)
        data
    })

    output$contents <- DT::renderDataTable({
        DT::datatable(myData(),options = list(scrollX = TRUE))
    })

    output$summary <- renderPrint({
        summary(myData())
    })
    output$select_variable <- renderUI({
        df <- myData()
        # we only want to show numeric cols
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        # exclude cols with zero variance
        #the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        selectInput("numerical", "Esculli la variable:",colnames, selected = NULL)
    })

    output$univ_table <- renderTable({
        df <- myData()
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        #the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        # selectInput("numerical", "Select Numerical:",colnames, selected = NULL)
        df <- df[,input$numerical]

        data.frame("Variable"=input$numerical, "Mitjana"=mean(df), "Mediana"=median(df),
                   "Desviació típica"=sd(df), "Variància"=var(df))

    })

    output$hist_var <- renderUI({
        df <- myData()
        # we only want to show numeric cols
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        # exclude cols with zero variance
        # the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        selectInput("hist", "Esculli la variable:",colnames)

    })

    output$hist_plot <- renderPlot({
        df <- myData()
        df <- df[,input$hist]
        bins <- seq(min(df), max(df), length.out = input$bins + 1)
        hist(x=df, breaks=bins, main="Histograma"  ,xlab=input$hist,ylab="Freqüència", col = "#75AADB", border = "white")
    })

    output$hist_down <- downloadHandler(
        filename = function(){
            paste("histograma", input$tipo_hist, sep = ".")
        },
        content = function(file){
            if(input$tipo_hist == "png"){
                png(file)} else {
                    pdf(file) }
            df <- myData()
            df <- df[,input$hist]
            bins <- seq(min(df), max(df), length.out = input$bins + 1)
            hist(x=df, breaks=bins, main="Histograma"  ,xlab=input$hist,ylab="Freqüència", col = "#75AADB", border = "white")
            dev.off()


        }
    )

    output$box_var <- renderUI({
        df <- myData()
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        selectInput("box", "Esculli la variable:",colnames)

    })

    output$box_plot <- renderPlot({
        df <- myData()
        df <- df[,input$box]
        boxplot(x=df, main="Diagrama de caixes", col = "#75AADB")


    })

    output$box_down <- downloadHandler(
        filename = function(){
            paste("boxplot", input$tipo_box, sep = ".")
        },
        content = function(file){
            if(input$tipo_box == "png") {
                png(file) }else{
                    pdf(file)}
            df <- myData()
            df <- df[,input$box]
            boxplot(x=df, main="Diagrama de caixes", col = "#75AADB")
            dev.off()

        })

    output$dens_var <- renderUI({
        df <- myData()
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        selectInput("dens", "Esculli la variable:",colnames)
    })

    output$dens_plot <- renderPlot({
        df <- myData()
        df <- df[,input$dens]
        d <- density(x=df)
        plot(d, main="Densitat")
        polygon(d, col = "#75AADB", border="black")
    })

    output$dens_down <- downloadHandler(
        filename = function(){
            paste("densitat", input$tipo_dens, sep = ".")
        },
        content = function(file){
            if(input$tipo_dens == "png"){
                png(file)} else {
                    pdf(file)}
            df <- myData()
            df <- df[,input$dens]
            d <- density(x=df)
            plot(d, main="Densitat")
            polygon(d, col = "#75AADB", border="black")
            dev.off()
        })

    output$sec_var <- renderUI({
        df <- myData()
        the_data_cat <- sapply(seq(1, ncol(df)), function(i) length(unique(df[,i])) < nrow(df)/10 )
        the_data_cat_cols <- df[, the_data_cat, drop = FALSE]
        selectInput("sect", "Esculli la variable:",names(the_data_cat_cols))
    })

    output$sec_plot <- renderPlot({
        df <- myData()
        df <- df[,input$sect]
        pie(table(df), main="Diagrama de sectors",
            col=c("skyblue1","steelblue4", "steelblue2", "steelblue3", "steelblue", "steelblue1"))
    })

    output$sect_down <- downloadHandler(
        filename = function(){
            paste("sectors", input$tipo_sect, sep = ".")
        },
        content = function(file){
            if(input$tipo_sect == "png") {
                png(file) } else {
                    pdf(file) }
            df <- myData()
            df <- df[,input$sect]
            pie(table(df), main="Diagrama de sectors",
                col=c("skyblue1","steelblue4", "steelblue2", "steelblue3", "steelblue", "steelblue1"))
            dev.off()
        })


    output$bar_var <- renderUI({
        df <- myData()
        the_data_cat <- sapply(seq(1, ncol(df)), function(i) length(unique(df[,i])) < nrow(df)/10 )
        the_data_cat_cols <- df[, the_data_cat, drop = FALSE]
        selectInput("bar", "Esculli la variable:",names(the_data_cat_cols))
    })

    output$bar_plot <- renderPlot({
        df <- myData()
        df <- df[,input$bar]
        barplot(table(df), main = "Gràfica de barres", col = "#75AADB", border="black")
    })

    output$bar_down <- downloadHandler(
        filename = function(){
            paste("barres", input$tipo_bar, sep = ".")
        },
        content = function(file){
            if(input$tipo_bar == "png") {
                png(file) } else {
                    pdf(file) }
            df <- myData()
            df <- df[,input$bar]
            barplot(table(df), main = "Gràfica de barres", col = "#75AADB", border="black")
            dev.off()
        })

    output$corr_var <- renderUI({
        df <- myData()
        grouping_cols <- sapply(seq(1, ncol(df)), function(i) length(unique(df[,i])) < nrow(df)/10 )
        the_data_group_cols <- df[, grouping_cols, drop = FALSE]
        selectInput(inputId = "var_qualit",
                    label = "Esculli la variable agrupadora:",
                    choices=c("Cap", names(the_data_group_cols)))
    })

    output$taula_corr <- renderTable({
        df <- myData()
        if (input$var_qualit=='Cap') {
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            head(data.frame(the_data_num))
        } else{
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            Variable_Agrupadora <- df[,input$var_qualit]
            head(data.frame(the_data_num,Variable_Agrupadora))
        }
    })



    output$corr_plot <- renderPlot({
        if(input$var_qualit=='Cap'){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            # columns_biplot <-input$columns_biplot
            # the_data_subset_biplot <- the_data_num[, columns_biplot, drop = FALSE]
            p <- ncol(the_data_num)
            scatterplotMatrix(the_data_num[,1:(p)],groups=F)
        } else{
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            Variable_Agrupadora <- df[,input$var_qualit]
            # columns_biplot <-input$columns_biplot
            taula1<-data.frame(the_data_num,Variable_Agrupadora)

            # the_data_subset_biplot <- df[, columns_biplot, drop = FALSE]
            p <- ncol(taula1)
            scatterplotMatrix(taula1[,1:(p-1)],groups = as.factor(taula1[,p]),by.groups = T)
            #ggpairs(the_data_subset_biplot)

        }
    })
    output$corr_down <- downloadHandler(
        filename = function(){
            paste("correlacio", input$tipo_corr, sep = ".")
        },
        content = function(file){
            if(input$tipo_corr == "png") {
                png(file)} else {
                    pdf(file)}
            grouping <- input$var_qualit
            if(input$var_qualit=='Cap'){
                df <- myData()
                the_data_num <- na.omit(df[,sapply(df,is.numeric)])
                # columns_biplot <-input$columns_biplot
                # the_data_subset_biplot <- the_data_num[, columns_biplot, drop = FALSE]
                p <- ncol(the_data_num)
                scatterplotMatrix(the_data_num[,1:(p)],groups=F)
            } else{
                df <- myData()
                the_data_num <- na.omit(df[,sapply(df,is.numeric)])
                Variable_Agrupadora <- df[,input$var_qualit]
                # columns_biplot <-input$columns_biplot
                taula1<-data.frame(the_data_num,Variable_Agrupadora)

                # the_data_subset_biplot <- df[, columns_biplot, drop = FALSE]
                p <- ncol(taula1)
                scatterplotMatrix(taula1[,1:(p-1)],groups = as.factor(taula1[,p]),by.groups = T)
                #ggpairs(the_data_subset_biplot)

            }
            dev.off()
        })



    output$xvariable <- renderUI({
        df <- myData()
        req(df)
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        xa<-colnames(the_data_num)
        selectInput(inputId = 'xvar',
                    label = "Selecciona la variable de l'eix de les x",
                    choices = c(xa[1:length(xa)]), selected=xa[1])

    })
    output$yvariable <- renderUI({
        df <- myData()
        req(df)
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        ya<-colnames(the_data_num)
        selectInput(inputId = 'yvar',
                    label = "Selecciona la variable de l'eix de les y",
                    choices = c(ya[1:length(ya)]), selected=ya[2]
        )
    })

    lmModel <- reactive({
        req(myData(),input$xvar,input$yvar)
        x <- as.numeric(myData()[[as.name(input$xvar)]])
        y <- as.numeric(myData()[[as.name(input$yvar)]])
        if (length(x) == length(y)){
            model <- lm(x ~ y, data = myData(), na.action=na.exclude)
        }else model <- NULL
        return(model)
    })

    output$lmSummary <- renderPrint({
        req(lmModel())
        summary(lmModel())
    })

    output$diagnosticPlot <- renderPlot({
        req(lmModel())
        par(mfrow = c(2,2))
        plot(lmModel())
    })

    output$regre_down <- downloadHandler(
        filename = function(){
            paste("regressio", input$tipo_regre, sep = ".")
        },
        content = function(file){
            if(input$tipo_regre == "png"){
                png(file)} else {
                    pdf(file) }
            lmModel <- reactive({
                req(myData(),input$xvar,input$yvar)
                x <- as.numeric(myData()[[as.name(input$xvar)]])
                y <- as.numeric(myData()[[as.name(input$yvar)]])
                if (length(x) == length(y)){
                    model <- lm(x ~ y, data = myData(), na.action=na.exclude)
                }else model <- NULL
                return(model)
            })
            par(mfrow = c(2,2))
            plot(lmModel())
            dev.off()


        }
    )





    output$choose_columns_pca <- renderUI({

        df <- myData()
        the_data_num <- na.omit(df[,sapply(df,is.numeric)])
        the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
        colnames <- names(the_data_num)
        checkboxGroupInput("columns", "Esculli les variables",
                           choices  = colnames,
                           selected = colnames)
    })


    output$the_grouping_variable <- renderUI({
        df <- myData()
        # for grouping we want to see only cols where the number of unique values are less than
        # 10% the number of observations
        grouping_cols <- sapply(seq(1, ncol(df)), function(i) length(unique(df[,i])) < nrow(df)/10 )
        the_data_group_cols <- df[, grouping_cols, drop = FALSE]
        selectInput(inputId = "the_grouping_variable",
                    label = "Variable agrupadora:",
                    choices=c("Cap", names(the_data_group_cols)))

    })


    pca_objects <- reactive({
        columns <-input$columns
        df <- na.omit(myData())
        the_data_subset <- na.omit(df[, columns, drop = FALSE])

        pca_output <- prcomp(na.omit(the_data_subset),
                             center = (input$center == 'Yes'),
                             scale. = (input$scale. == 'Yes'))
        pcs_df <- cbind(df, pca_output$x)

        return(list(df = df,
                    the_data_subset = the_data_subset,
                    pca_output = pca_output,
                    pcs_df = pcs_df))

    })

    output$the_pcs_to_plot_x <- renderUI({
        pca_output <- pca_objects()$pca_output$x

        selectInput(inputId = "the_pcs_to_plot_x",
                    label = "Eix X:",
                    choices= colnames(pca_output),
                    selected = 'PC1')
    })

    output$the_pcs_to_plot_y <- renderUI({
        pca_output <- pca_objects()$pca_output$x

        selectInput(inputId = "the_pcs_to_plot_y",
                    label = "Eix Y:",
                    choices= colnames(pca_output),
                    selected = 'PC2')
    })



    output$eig_plot <- renderPlot({
        pca_output <- pca_objects()$pca_output
        fviz_eig(pca_output,addlabels = TRUE)
    })

    observeEvent(input$info_scree, {
        showModal(modalDialog(
            title = "Scree plot",
            "Mostra el % de variància explicat per cada component principal. Quan el gràfic
                 sedimenta (es fa pla), és a dir, ja no s'incorpora un % de variància suficient,
                 aquestes variables es descarten perquè es consideren \"soroll\". Quan amb \"k\"
      components s'acumula un percentatge determinat de variància (al voltant 70%, 80% o altre
      segons els context experimental), es considera un nombre suficient de components principals.",
            footer = actionButton("tancar", "Tancar")
        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })

    output$eig_down <- downloadHandler(
        filename = function(){
            paste("valors_propis", input$tipo_eig, sep = ".")
        },
        content = function(file){
            if(input$tipo_eig == "png") {
                png(file)} else {
                    pdf(file) }
            pca_output <- pca_objects()$pca_output
            fviz_eig(pca_output,addlabels = TRUE)
            dev.off()
        })

    output$ind_plot <- renderPlot({
        pca_output <- pca_objects()$pca_output
        fviz_pca_ind(pca_output,
                     col.ind = "cos2", # Color by the quality of representation
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE     # Avoid text overlapping
        )
    })

    observeEvent(input$info_var, {
        showModal(modalDialog(
            title = "Gràfic de variables",
            "Les variables amb correlació positiva tenen entre sí un angle més petit, en canvi quan l'angle
     s'aproxima a pi/2 vol dir que estan incorrelacionades. Quan l'angle s'aproxima a pi les
     variables estan correlacionades però negativament. En referència a la llargada de la fletxa,
     quan més llarga és, més qualitat de representació té aquella variable en aquest pla, perquè la
     fletxa indica la projecció sobre aquest pla, essent de mida 1 la projecció màxima per a dades
     estandarditzades (solució per defecte).",
            footer = actionButton("tancar", "Tancar")

        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })

    output$ind_down <- downloadHandler(
        filename = function(){
            paste("individus", input$tipo_ind, sep = ".")
        },
        content = function(file){
            if(input$tipo_ind == "png") {
                png(file)} else {
                    pdf(file)}
            pca_output <- pca_objects()$pca_output
            fviz_pca_ind(pca_output,
                         col.ind = "cos2", # Color by the quality of representation
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     # Avoid text overlapping
            )
            dev.off()
        })

    output$var_pca_plot <- renderPlot({
        pca_output <- pca_objects()$pca_output
        fviz_pca_var(pca_output,
                     col.var = "cos2", # Color by contributions to the PC
                     gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                     repel = TRUE     # Avoid text overlapping
        )
    })

    observeEvent(input$info_ind, {
        showModal(modalDialog(
            title = "Gràfic d'individus",
            "Es representa el seu cos2. Els individus amb més alta qualitat bidimensional es poden
              interpretar, els altres els veiem centrats però tenen una part en dimensió 3 o altres.
              La graduació de color representa la qualitat.",
            footer = actionButton("tancar", "Tancar")

        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })

    output$var_pca_down <- downloadHandler(
        filename = function(){
            paste("variables", input$tipo_var, sep = ".")
        },
        content = function(file){
            if(input$tipo_var == "png") {
                png(file)} else {
                    pdf(file)}
            pca_output <- pca_objects()$pca_output
            fviz_pca_var(pca_output,
                         col.var = "contrib", # Color by contributions to the PC
                         gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
                         repel = TRUE     # Avoid text overlapping
            )
            dev.off()
        })




    # PC plot
    pca_biplot <- reactive({
        pcs_df <- pca_objects()$pcs_df
        pca_output <-  pca_objects()$pca_output

        var_expl_x <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_x))]^2/sum(pca_output$sdev^2), 1)
        var_expl_y <- round(100 * pca_output$sdev[as.numeric(gsub("[^0-9]", "", input$the_pcs_to_plot_y))]^2/sum(pca_output$sdev^2), 1)
        labels <- rownames(pca_output$x)
        grouping <- input$the_grouping_variable

        if(grouping == 'Cap'){
            # plot without grouping variable
            pc_plot_no_groups  <- ggplot(pcs_df,
                                         aes_string(input$the_pcs_to_plot_x,
                                                    input$the_pcs_to_plot_y
                                         )) +


                geom_text(aes(label = labels),  size = 5) +
                theme_bw(base_size = 14) +
                coord_equal() +
                xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
                ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)"))
            # the plot
            pc_plot_no_groups


        } else {
            # plot with grouping variable

            pcs_df$Grups <-  as.character(pcs_df[, grouping, drop = TRUE])
            pc_plot_groups  <- ggplot(pcs_df, aes_string(input$the_pcs_to_plot_x,
                                                         input$the_pcs_to_plot_y,
                                                         fill = 'Grups',
                                                         colour = 'Grups'
            )) +
                stat_ellipse(geom = "polygon", alpha = 0.1) +

                geom_text(aes(label = labels),  size = 5) +
                theme_bw(base_size = 14) +
                scale_color_discrete()+
                coord_equal() +
                xlab(paste0(input$the_pcs_to_plot_x, " (", var_expl_x, "% explained variance)")) +
                ylab(paste0(input$the_pcs_to_plot_y, " (", var_expl_y, "% explained variance)"))
            # the plot
            pc_plot_groups
        }

    })

    observeEvent(input$info_ind_grup, {
        showModal(modalDialog(
            title = "Gràfic d'individus",
            "Quan hi ha una variable suplementària que classifica els casos en grups, es dibuixen unes
      el·lipses que intenten recollir la majoria de les dades de cada grup. Si estan molt superposades
      vol dir que els grups no estan ben discriminats en aquest pla. Les el·lipses intenten reproduir
      una mixtura de Gaussianes bidimensionals, si la distribució dins dels grups no s'acosta a la
      Gaussiana, les el·lipses no ajustaran bé les dades.",
            footer = actionButton("tancar", "Tancar")

        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })

    output$z_plot1 <- renderPlot({

        pca_biplot()

    })






    output$choose_pca_var <- renderUI({
        selectInput(inputId = "pca_var_tool", label = "Escull el resultat de les variables per visualitzar",
                    choices=c("Coordenades de les variables"="coord",
                              "Variables de correlacions"="cor",
                              "cos2 de les variables"="cos2",
                              "Contribucions de les variables"="contrib"
                    ), multiple=FALSE,selected = "coord")
    })

    observeEvent(input$infovar, {
        showModal(modalDialog(
            title = "Informació sobre les variables",
            "Les coordenades són el que es representa en la gràfica amb la qualitat acumulada.
      La correlació és la que hi ha entre cada variable i cada component i coincideix amb les coordenades
      si es fa el PCA a partir de la matriu de correlacions. Si es fa amb la matriu de covariància
      aleshores no és cert, coordenades i correlacions són diferents.
      Els cos2 es mira per files i indica la qualitat i la projecció en cada dimensió. Si es suma
      la primera a la segona fila s'obté la llargada de la fletxa.
      Les contribucions es miren per columnes, de cada una es veu quina variable contribueix més a cada dimensió.",
            footer = actionButton("tancar", "Tancar")

        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })


    output$pca_var <- renderPrint({

        accio <- input$pca_var_tool
        if(accio=="coord"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$var$coord)
        }
        else if(accio=="cor"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$var$cor)
        }
        else if(accio=="cos2"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$var$cos2)
        }    else if(accio=="contrib"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$var$contrib)
        }


        #summary(pca_objects()$pca_output)

    })


    observeEvent(input$infoind, {
        showModal(modalDialog(
            title = "Informació sobre els individus",
            "Les coordenades són el que es representa en la gràfica amb la qualitat acumulada.
      Els cos2 es mira per files i indica la qualitat i la projecció en cada dimensió. Si es suma
      la primera a la segona fila s'obté la llargada de la fletxa.
      Les contribucions es miren per columnes, de cada una es veu quin individu
      contribueix més a cada dimensió.",
            footer = actionButton("tancar", "Tancar")

        ))
    })

    observeEvent(input$tancar,{
        removeModal()

    })

    output$choose_pca_ind <- renderUI({
        selectInput(inputId = "pca_ind_tool", label = "Escull el resultat dels individus per visualitzar",
                    choices=c("Coordenades dels individus"="coord",
                              "cos2 dels individus"="cos2",
                              "Contribucions dels individus"="contrib"
                    ), multiple=FALSE,selected = "coord")
    })


    output$pca_ind <- renderPrint({

        accio <- input$pca_ind_tool
        if(accio=="coord"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$ind$coord)
        }

        else if(accio=="cos2"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$ind$cos2)
        }    else if(accio=="contrib"){
            df <- myData()
            the_data_num <- na.omit(df[,sapply(df,is.numeric)])
            the_data_num <- the_data_num[,!apply(the_data_num, MARGIN = 2, function(x) max(x, na.rm = TRUE) == min(x, na.rm = TRUE))]
            pca_var <- PCA(the_data_num)
            print(pca_var$ind$contrib)
        }


        #summary(pca_objects()$pca_output)

    })






}
