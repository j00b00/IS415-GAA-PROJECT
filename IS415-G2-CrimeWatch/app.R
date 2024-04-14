pacman::p_load(shiny, shinythemes, tidyverse, leaflet, tmap, sf, sp, GWmodel, plotly, ClustGeo, 
               dendextend, GGally, ggdendro, corrplot, DT, shinyWidgets, RColorBrewer, spdep, waiter, bslib)




combined_data <- read_rds("data/rds/combined_data.rds")
combined_data_sp <- as_Spatial(combined_data)


#Global Parameters

#List of Measures
varMeasure1 <- c(
  "Security" = "security_ind", 
  "Theft" = "theft_ind", 
  "Police" = "police_ind", 
  "Gangster" = "gangster_ind"
  )


ui <- fluidPage(
  column(width = 2, 
         a(href = "https://i.postimg.cc/nVd0Ymhx/transparent-bg.png", 
           img(src = "https://i.postimg.cc/nVd0Ymhx/transparent-bg.png", height = "40px"))),
  useWaiter(),
  tags$head(
    tags$style(
      HTML("
      :root {
          --primary-color: #FCC780;
          --background-color: #fefee2;
          --foreground-color: black;
          --navbar-color: #fefee2
          --sidebar-color: #fefee2
        }
        body {
          font-family: 'Space Mono', monospace;
          background-color: var(--background-color);
          color: var(--foreground-color);
          
        }
        .navbar {
          background-color: var(--navbar-color);
        }
        
        "
      )
    )
  ),
                navbarPage("CrimeWatch", id = "navbar",
                           fluid = TRUE, 
                           windowTitle = "CrimeWatch: Vietnam Law and Order Geo-Spatial Analysis",
                           selected="eda",

                           
                           #Data Panel
                           tabPanel("Data", value="data", fluid=TRUE, icon=icon("database"),
                                    sidebarLayout(position="left", fluid=TRUE,
                                                  sidebarPanel(width=3,
                                                               pickerInput(inputId="inDataSelect",
                                                                           label="Columns",
                                                                           choices=c("Shape_Leng" = "Shape_Leng", "Shape_Area" = "Shape_Area", "Province" = "Province", "ADM1_PCODE" = "ADM1_PCODE", "overall_index" = "overall_index", varMeasure1),
                                                                           selected=c("Shape_Leng" = "Shape_Leng", 
                                                                                      "Shape_Area" = "Shape_Area", 
                                                                                      "Province" = "Province", 
                                                                                      "ADM1_PCODE" = "ADM1_PCODE", 
                                                                                      "overall_index" = "overall_index",
                                                                                      varMeasure1),
                                                                                      # "Security" = "security_ind",
                                                                                      # "Theft" = "theft_ind",
                                                                                      # "Police" = "police_ind",
                                                                                      # "Gangster" = "gangster_ind"),
                                                                           options = list(`actions-box` = TRUE),
                                                                           multiple=TRUE,
                                                                           width="100%"
                                                                           ),
                                                               ),
                                                               mainPanel("Data Panel", width=9,
                                                                         id = 'dataset',
                                                                           DT::dataTableOutput("data_table")
                                                                           )
                                                                         )
                                                               ),





                           #EDA Panel
                           tabPanel("EDA", value = "eda", fluid=TRUE, icon=icon("search"),
                                    sidebarLayout(sidebarPanel(width=3, fluid=TRUE,
                                                               conditionalPanel(
                                                                 'input.EDAset === "Histogram"',
                                                                 selectInput(inputId = "varMeasure1",
                                                                               label = "Variables",
                                                                               choices = varMeasure1,
                                                                               selected = "security_ind",
                                                                               multiple = FALSE,
                                                                               width = "100%"),

                                                                 sliderInput(inputId = "bins",
                                                                             label = "Number of bins",
                                                                             min = 1,
                                                                             max = 10,
                                                                             value = 5,
                                                                             width = "100%")
                                                                 ),

                                                               conditionalPanel(
                                                                 'input.EDAset === "Box Plot"',
                                                                 selectInput(inputId = "varMeasure",
                                                                               label = "Variables",
                                                                               choices = varMeasure1,
                                                                               selected = "security_ind",
                                                                               multiple = FALSE,
                                                                               width = "100%")
                                                               )
                                                               ),

                                                               mainPanel(width=9, fluid=TRUE,
                                                                         tabsetPanel(
                                                                           id="EDAset",
                                                                           tabPanel("Histogram",
                                                                                    fluidRow(
                                                                                      plotOutput(outputId="eda1"),
                                                                                      leafletOutput("percentileMap")
                                                                                             )
                                                                                      ),
                                                                           tabPanel("Box Plot",
                                                                                    fluidRow(
                                                                                      plotOutput(outputId="eda2"),
                                                                                      leafletOutput("BoxMap")
                                                                                    )
                                                                           )
                                                                         )
                                                               )
                                    )
                           ),
                           
                           


                           #ESDA Panel
                           tabPanel("ESDA", value="esda", fluid=TRUE, icon=icon("globe-americas"),
                                    sidebarLayout(position="left", fluid=TRUE,
                                                  sidebarPanel(width=3, fluid=TRUE,
                                                               fluidRow(
                                                                 selectInput(inputId="inMeasure",
                                                                             label="Select Variable",
                                                                             choices=varMeasure1,
                                                                             selected="security_ind",
                                                                             multiple=FALSE,
                                                                             width="100%"),

                                                                 selectInput(inputId="inLisaMethod",
                                                                           label="Analysis Method",
                                                                           choices=c("Contiguity Queen"="q",
                                                                                     "Contiguity Rook"="r",
                                                                                     "K Nearest Neighbours"="knn"
                                                                                     ),
                                                                           selected="q",
                                                                           multiple=FALSE,
                                                                           width="100%"),

                                                                 conditionalPanel(condition="input.inLisaMethod=='knn'",
                                                                                  sliderInput(inputId="k",
                                                                                              label="Select K",
                                                                                              min=2,
                                                                                              max=30,
                                                                                              value=5,
                                                                                              width="100%")
                                                                                  ),

                                                                 selectInput(inputId="inLisaSignificance",
                                                                             label="Confidence Level",
                                                                             choices=c("90%"=0.1,
                                                                                       "95%"=0.05,
                                                                                       "99%"=0.01,
                                                                                       "99.9%"=0.001),
                                                                             selected=0.1,
                                                                             multiple=FALSE,
                                                                             width="100%"),
                                                                 )),
                                                  mainPanel(fluidRow(
                                                    leafletOutput("lisa"),
                                                    ))),
                                    ),
                           
                        
                           

                           #Cluster Panel
                           tabPanel("Clustering", value="clustering", fluid=TRUE, icon=icon("globe-asia"),
                                    sidebarLayout(position="left", fluid=TRUE,
                                                  sidebarPanel(width=3, fluid=TRUE,
                                                               selectInput(inputId="inMeasureCluster",
                                                                           label="Variable",
                                                                           choices=varMeasure1,
                                                                           selected=varMeasure1,
                                                                           multiple=TRUE,
                                                                           width="100%"
                                                               ),
                                                               sliderInput(inputId="inClusterSize", label="Cluster Size", min=1, max=10,
                                                                           value=5, step=1, round=TRUE
                                                               ),
                                                               radioButtons(inputId = "clustMethod",
                                                                            label = "Select Clustering Method",
                                                                            inline=TRUE,
                                                                            choiceNames = c("Hierarchical","ClustGEO","Skater"),
                                                                            choiceValues = c("HC","GS","SK"),
                                                                            selected = "HC"),

                                                               conditionalPanel(condition="input.clustMethod=='HC'",
                                                                                selectInput(inputId="inAggloMethod",
                                                                                            label="Agglomeration Method",
                                                                                            choices=c("ward.D"="ward.D",
                                                                                                      "ward.D2"="ward.D2",
                                                                                                      "single"="single",
                                                                                                      "complete"="complete",
                                                                                                      "average"="average",
                                                                                                      "mcquitty"="mcquitty",
                                                                                                      "median"="median",
                                                                                                      "centroid"="centroid"
                                                                                            ),
                                                                                            selected="ward.D"
                                                                                ),
                                                                                checkboxInput(inputId="inShowAggloMethod", label="Show Method Suggestion", value=FALSE),
                                                                                conditionalPanel(condition="input.inShowAggloMethod",
                                                                                                 tableOutput("aggloplot")
                                                                                )
                                                               ),
                                                               conditionalPanel(condition="input.clustMethod=='GS'",
                                                                                sliderInput(inputId = "inAlpha", label = "Mixing Factor", min = 0,
                                                                                            max = 1, value = 0.4,width="100%", step=0.05),
                                                                                checkboxInput(inputId="inShowMixFactor", label="Show Mix Factor Suggestion", value=FALSE),
                                                                                conditionalPanel(condition="input.inShowMixFactor",
                                                                                                 plotOutput("alphaplot", height = 300)
                                                                                )
                                                               ),
                                                               conditionalPanel(condition="input.clustMethod=='SK'",
                                                                                selectInput(inputId="inSkaterMethod",
                                                                                            label="Method",
                                                                                            choices=c("euclidean"="euclidean",
                                                                                                      "maximum"="maximum",
                                                                                                      "manhattan"="manhattan",
                                                                                                      "canberra"="canberra",
                                                                                                      "binary"="binary",
                                                                                                      "minkowski"="minkowski"
                                                                                            ),
                                                                                            selected="euclidean"
                                                                                ),
                                                               ),
                                                  ),
                                                  mainPanel(width=9,
                                                            fluidRow(
                                                              tabsetPanel(
                                                                tabPanel("Correlation Analysis",
                                                                         fluidRow(
                                                                           plotOutput("corrplot", height= 500)
                                                                           )
                                                                         ),
                                                                tabPanel("Clustering",
                                                                         fluidRow(
                                                                           column(6,
                                                                                  leafletOutput("ClusterMap"),
                                                                                  plotOutput("findkplot")
                                                                           ),
                                                                           column(6,
                                                                                  plotlyOutput("dendoplot")
                                                                           )
                                                                         )
                                                                )
                                                              )
                                                            )
                                                  )
                                    )
                           )
                )
)
                

server <- function(input, output, session){
  keep_alive <- shiny::reactiveTimer(intervalMs = 10000, session = shiny::getDefaultReactiveDomain())
  
  # Observer to trigger the reactive timer periodically
  observe({
    keep_alive()
  })

  rv = reactiveValues()

  #Data Function
  output$data_table <- DT::renderDataTable({
    DF <- as.data.frame(combined_data)
    DT::datatable(DF[, input$inDataSelect, drop = FALSE], style="bootstrap")
    
  })

  # EDA Function
  output$percentileMap <- renderLeaflet({
    waiter_show(html = spin_fading_circles())
    get.var <- function(vname, df) {
      v <- df[vname] %>%
        st_set_geometry(NULL)
      v <- unname(v[,1])
      return(v)
    }

    percentile <- c(0, 0.01, 0.1, 0.5, 0.9, 0.99, 1)

    var <- get.var(input$varMeasure1, combined_data)
    bperc <- quantile(var, percentile)

    pmap <- tm_shape(combined_data) +
      tm_polygons() +
      tm_shape(combined_data) +
      tm_fill(input$varMeasure1,
              breaks=bperc,
              palette="Blues",
              labels=c("< 1%", "1% - 10%", "10% - 50%", "50% - 90%", "90% - 99%", "> 99%")) +
      tm_borders() +
      tm_layout(main.title = "Percentile Map", title.position = c("right", "bottom"))

    tmap_leaflet(pmap, in.shiny=TRUE)

  })

  output$eda1 <- renderPlot({
    hist(combined_data_sp[[input$varMeasure1]],
         main = paste("Histogram of", input$varMeasure1),
         xlab = input$varMeasure1,
         col = "grey",
         breaks = input$bins)
    waiter_hide()
  })

  output$BoxMap <- renderLeaflet({
    # waiter_show(html = spin_fading_circles())
    boxbreaks <- function(v,mult=1.5) {
      qv <- unname(quantile(v))
      iqr <- qv[4] - qv[2]
      upfence <- qv[4] + mult * iqr
      lofence <- qv[2] - mult * iqr
      bb <- vector(mode="numeric",length=7)
      if (lofence < qv[1]) {
        bb[1] <- lofence
        bb[2] <- floor(qv[1])
      } else {
        bb[2] <- lofence
        bb[1] <- qv[1]
      }
      if (upfence > qv[5]) {
        bb[7] <- upfence
        bb[6] <- ceiling(qv[5])
      } else {
        bb[6] <- upfence
        bb[7] <- qv[5]
      }
      bb[3:5] <- qv[2:4]
      return(bb)
    }

    get.var <- function(vname, df) {
      v <- df[vname] %>%
        st_set_geometry(NULL)
      v <- unname(v[,1])
      return(v)
    }

    var <- get.var(input$varMeasure, combined_data)
    bb <- boxbreaks(var)

    bmap <- tm_shape(combined_data) +
      tm_polygons() +
      tm_shape(combined_data) +
      tm_fill(input$varMeasure,title="Box Map",
              breaks=bb,
              palette="Blues",
              labels = c("lower outlier",
                         "< 25%",
                         "25% - 50%",
                         "50% - 75%",
                         "> 75%",
                         "upper outlier"))  +
        tm_borders() +
        tm_layout(main.title = "Box Plot",
                  title.position = c("left",
                                     "top"))

    tmap_leaflet(bmap, in.shiny=TRUE)
  })

  output$eda2 <- renderPlot({
    boxplot(combined_data_sp[[input$varMeasure]],
            main = paste("Boxplot of", input$varMeasure),
            ylab = input$varMeasure)
    waiter_hide()
  })



  #ESDA functions
  legend <- c("insignificant","low-low", "low-high", "high-low", "high-high")
  colorsRd <- c("#ffffff","#fcae91","#fb6a4a","#de2d26","#a50f15")
  colorsBu <- c("#ffffff","#bdd7e7","#6baed6","#3182bd","#08519c")
  colorsBu4 <- c("#bdd7e7","#6baed6","#3182bd","#08519c")
  colorsNBu <- c("#08519c","#3182bd","#6baed6","#bdd7e7","#ffffff")
  colorsLi <- c("#ffffff","#08519c","#6baed6","#fb6a4a","#a50f15")
  colorsNLi <- c("#fddbc7","#f4a582","#d6604d","#b2182b","#2166ac","#4393c3","#92c5de","#d1e5f0")

  output$lisa <- renderLeaflet({
    waiter_show(html = spin_fading_circles())

    indicator <- pull(combined_data_sp@data, input$inMeasure)

    if (input$inLisaMethod=="q") {
      wm <- poly2nb(combined_data_sp, queen=TRUE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="r") {
      wm <- poly2nb(combined_data_sp, queen=FALSE)
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="knn") {
      wm <- knn2nb(knearneigh(coordinates(combined_data_sp), k=input$k), row.names=row.names(combined_data_sp$Province))
      rswm <- nb2listw(wm, zero.policy=TRUE)
    }
    else if (input$inLisaMethod=="idw-q") {
      wm <- poly2nb(combined_data_sp, queen=TRUE)
      dist <- nbdists(wm, coordinates(combined_data_sp), longlat=FALSE)
      idw <- lapply(dist, function(x) 1/(x/1000))
      rswm <- nb2listw(wm, glist=idw, style="B", zero.policy=TRUE)
    }
    else {
      wm <- poly2nb(combined_data_sp, queen=FALSE)
      dist <- nbdists(wm, coordinates(combined_data_sp), longlat=FALSE)
      idw <- lapply(dist, function(x) 1/(x/1000))
      rswm <- nb2listw(wm, glist=idw, style="B", zero.policy=TRUE)
    }

    rv$lmoran <- localmoran(indicator, rswm)

    quadrant <- vector(mode = "numeric", length = nrow(rv$lmoran))
    DV <- indicator - mean(indicator)
    C_mI <- rv$lmoran[,1] - mean(rv$lmoran[,1])

    quadrant[DV >0 & C_mI>0] <- 4
    quadrant[DV >0 & C_mI<0] <- 3
    quadrant[DV <0 & C_mI>0] <- 2
    quadrant[DV <0 & C_mI<0] <- 1
    quadrant[rv$lmoran[,5] > as.numeric(input$inLisaSignificance)] <- 0


    qText <- vector(mode = "character", length = nrow(rv$lmoran))
    qText[quadrant==0] <- "insignificant"
    qText[quadrant==1] <- "low-low"
    qText[quadrant==2] <- "low-high"
    qText[quadrant==3] <- "high-low"
    qText[quadrant==4] <- "high-high"

    combined_data_sp$DV <- DV
    combined_data_sp$C_mI <- C_mI
    combined_data_sp$qText <- qText
    combined_data_sp$quadrant <- as.numeric(quadrant)

    lisaPlot <- tm_shape(combined_data_sp) +
      tm_fill("quadrant",
              title="LISA Cluster",
              style="cat",
              n=5,
              palette=colorsLi,
              midpoint=NA,
              labels=legend,
              popup.vars=c("DV:"="DV","C_mI:"="C_mI","Quadrant:"="qText"),
              alpha=0.8,
              legend.format=list(digits=2)
      ) +
      tm_borders(alpha=0.8
      ) +
      tm_view(view.legend.position=c("right","top"),
              control.position=c("left","bottom"),
              colorNA="Black"
      ) +
      tmap_options(basemaps=c("Esri.WorldGrayCanvas","OpenStreetMap"),
                   basemaps.alpha=c(0.8,0.5,0.7)
      ) +
      tm_shape(combined_data_sp) +
      tm_borders(col="black",
                 lwd=3)

    tmap_leaflet(lisaPlot, in.shiny=TRUE)
  })
  observe({
    invalidateLater(1000)  # Adjust delay as needed
    waiter_hide()
  })
  

  

  #Clustering functions
  clusterset <- "global"

  output$ClusterMap <- renderLeaflet({
    # waiter_show(html = spin_fading_circles())

    clusterset <<- combined_data_sp

    if (input$clustMethod=='SK'){
      ## Skater ##
      vars <- input$inMeasureCluster
      sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))

      clusterset.nb <- poly2nb(clusterset)

      lcosts <- nbcosts(clusterset.nb,sdat)

      clusterset.w <- nb2listw(clusterset.nb,lcosts,style="B")

      mpsz.mst <- mstree(clusterset.w)

      clusterGrp <- skater(mpsz.mst[,1:2],sdat,input$inClusterSize-1,method = input$inSkaterMethod,p=input$inMinkowski)

      groups <- as.factor(clusterGrp$groups)

      ## Skater ##
    } else {
      ## H Clustering ##
      vars <- input$inMeasureCluster
      sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))

      D0 <- dist(sdat, method = 'euclidean')

      if (input$clustMethod=='HC'){
        hclust_scaled <- hclust(D0, method = input$inAggloMethod)
      } else {
        list.nb <- spdep::poly2nb(clusterset)
        # Create adjacency matrix
        A <- spdep::nb2mat(list.nb,style="W", zero.policy = TRUE)
        diag(A) <- 1
        colnames(A) <- rownames(A)
        D1 <- as.dist(1-A)
        hclust_scaled <- hclustgeo(D0,D1,alpha=input$inAlpha)
      }

      groups <- as.factor(cutree(hclust_scaled, k=input$inClusterSize))
    }

    clusterset$cluster <<- as.matrix(groups)

    clusterPlot <- tm_shape(clusterset) +
      tm_polygons("cluster", title="Clusters", plalette=brewer.pal(n = input$inClusterSize, name = "RdYlBu")) +
      tm_format("World")

    tmap_leaflet(clusterPlot, in.shiny=TRUE)
  })

  output$findkplot <- renderPlot({

    clusterset <- combined_data_sp

    vars <- input$inMeasureCluster
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))

    D0 <- dist(sdat, method = 'euclidean')
    tesco_clust <- hclust(D0, method = input$inAggloMethod)
    num_k <- find_k(tesco_clust)
    plot(num_k)

  })

  output$alphaplot <- renderPlot({

    clusterset <- combined_data_sp

    ## H Clustering ##
    vars <- input$inMeasureCluster
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))

    D0 <- dist(sdat, method = 'euclidean')


    list.nb <- spdep::poly2nb(clusterset)
    A <- spdep::nb2mat(list.nb,style="W", zero.policy = TRUE)
    diag(A) <- 1
    colnames(A) <- rownames(A)
    D1 <- as.dist(1-A)

    range.alpha <- seq(0,1,0.1)
    choicealpha(D0,D1,range.alpha,input$inClusterSize,graph=TRUE)
  })

  output$aggloplot <- renderTable({

    clusterset <- combined_data_sp

    vars <- input$inMeasureCluster
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))

    tesco_matrix <- data.matrix(sdat)
    tesco_d <- dist((tesco_matrix), method = "euclidean")

    dend_expend(tesco_d)[[3]]

  })

  output$dendoplot <- renderPlotly({

    dendro_data_k <- function(hc, k) {

      hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
      seg       <-  hcdata$segments
      labclust  <-  cutree(hc, k)[hc$order]
      segclust  <-  rep(0L, nrow(seg))
      heights   <-  sort(hc$height, decreasing = TRUE)
      height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)

      for (i in 1:k) {
        xi      <-  hcdata$labels$x[labclust == i]
        idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
        idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
        idx3    <-  seg$yend < height
        idx     <-  idx1 & idx2 & idx3
        segclust[idx] <- i
      }

      idx                    <-  which(segclust == 0L)
      segclust[idx]          <-  segclust[idx + 1L]
      hcdata$segments$clust  <-  segclust
      hcdata$segments$line   <-  as.integer(segclust < 1L)
      hcdata$labels$clust    <-  labclust

      hcdata
    }

    set_labels_params <- function(nbLabels,
                                  direction = c("tb", "bt", "lr", "rl"),
                                  fan       = FALSE) {
      if (fan) {
        angle       <-  360 / nbLabels * 1:nbLabels + 90
        idx         <-  angle >= 90 & angle <= 270
        angle[idx]  <-  angle[idx] + 180
        hjust       <-  rep(0, nbLabels)
        hjust[idx]  <-  1
      } else {
        angle       <-  rep(0, nbLabels)
        hjust       <-  0
        if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
        if (direction %in% c("tb", "rl")) { hjust <- 1 }
      }
      list(angle = angle, hjust = hjust, vjust = 0.5)
    }
    plot_ggdendro <- function(hcdata,
                              direction   = c("lr", "rl", "tb", "bt"),
                              fan         = FALSE,
                              scale.color = NULL,
                              branch.size = 1,
                              label.size  = 3,
                              nudge.label = 0.01,
                              expand.y    = 0.1) {

      direction <- match.arg(direction) # if fan = FALSE
      ybreaks   <- pretty(segment(hcdata)$y, n = 5)
      ymax      <- max(segment(hcdata)$y)

      ## branches
      p <- ggplot() +
        geom_segment(data         =  segment(hcdata),
                     aes(x        =  x,
                         y        =  y,
                         xend     =  xend,
                         yend     =  yend,
                         linetype =  factor(line),
                         colour   =  factor(clust)),
                     lineend      =  "round",
                     show.legend  =  FALSE,
                     size         =  branch.size)

      ## orientation
      if (fan) {
        p <- p +
          coord_polar(direction = -1) +
          scale_x_continuous(breaks = NULL,
                             limits = c(0, nrow(label(hcdata)))) +
          scale_y_reverse(breaks = ybreaks)
      } else {
        p <- p + scale_x_continuous(breaks = NULL)
        if (direction %in% c("rl", "lr")) {
          p <- p + coord_flip()
        }
        if (direction %in% c("bt", "lr")) {
          p <- p + scale_y_reverse(breaks = ybreaks)
        } else {
          p <- p + scale_y_continuous(breaks = ybreaks)
          nudge.label <- -(nudge.label)
        }
      }

      # labels
      labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
      hcdata$labels$angle <- labelParams$angle

      p <- p +
        geom_text(data        =  label(hcdata),
                  aes(x       =  x,
                      y       =  y,
                      label   =  label,
                      colour  =  factor(clust),
                      angle   =  angle),
                  vjust       =  labelParams$vjust,
                  hjust       =  labelParams$hjust,
                  nudge_y     =  ymax * nudge.label,
                  size        =  label.size,
                  show.legend =  FALSE)

      # colors and limits
      if (!is.null(scale.color)) {
        p <- p + scale_color_manual(values = scale.color)
      }

      ylim <- -round(ymax * expand.y, 1)
      p    <- p + expand_limits(y = ylim)

      p
    }

    clusterset <- combined_data_sp

    vars <- input$inMeasureCluster
    sdat <- data.frame(scale(as.data.frame(clusterset)[,vars]))
    row.names(sdat) <- clusterset$Province

    D <- dist(sdat, method = 'euclidean')

    hc  <- hclust(D, input$inAggloMethod)
    hc$labels <- as.factor(clusterset$Province)

    hcdata <- dendro_data_k(hc, input$inClusterSize)

    p <- plot_ggdendro(hcdata,
                       direction   = "rl",
                       expand.y    = 0.1,
                       label.size=2,
                       branch.size=0.5)

    p <- p+theme(
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    )
    p <- p + ggtitle("Dendogram")
    ggplotly(p,tooltip = NULL)
    hide_legend(p)

  })

  output$corrplot <- renderPlot({

    vars <- input$inMeasureCluster

    sdat_corr <- data.frame(scale(as.data.frame(combined_data_sp)[,vars]))
    clust.cor = cor(sdat_corr)

    corrplot.mixed(clust.cor,
                   lower = "ellipse",
                   upper = "number",
                   sig.level = .05,
                   tl.pos = "lt",
                   bg = "white",
                   diag = "l",
                   order="AOE",
                   tl.col = "black")
    waiter_hide()
  })
}

shinyApp(ui = ui, server = server)