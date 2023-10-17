library(shiny)
library(excelR)
library(RColorBrewer)
library(shinyWidgets)
library(reshape)
library(DescTools)
library(threeforest)

source("afmodel_funcs.R", local = TRUE)

server <- function(input, output, session) {
  options(shiny.maxRequestSize = 300 * 1024 ^ 2)
  color_list <-
    c(brewer.pal(12, "Set3"),
      brewer.pal(8, "Set2"),
      brewer.pal(9, "Set1"))
  field_color <- c("ColorRed", "ColorGreen", "ColorBlue")
  field_desc <- c("select", parameters[[1]]$fields, "color")
  field_labels <- c("color", "select", "Label")
  field_map <-
    data.frame(
      f20 = c(
        "LightFlexi",
        "LightSensi",
        "LightFlexiLateral",
        "LightSensiLateral",
        "RegenerationMetaCompotition"
      ),
      f21 = c(
        "LightFlexiTree",
        "LightSensiTree",
        "LightFlexiCrown",
        "LightSensiCrown",
        "RegenerationMetaComposition"
      )
    )
  pnlist <- names(parameters)
  default_species <- read_species_file("www/default_species.trs")
  default_species$color <- color_list[1]
  default_species$select <- F
  species_df <- default_species[0, ]
  loaded_sp_df <- NULL
  
  v <- reactiveValues(dataTable = NULL, dataPlot = NULL)
  
  
  output$home_page <- renderUI({
    threeforestOutput("home_forest", height = "600px")
  })
  
  output$home_forest <- renderThreeforest({
    plot(plant(1+runif(50)*50, area = c(50, 50)), setting = list(camera_dist = 50))
  })
  
  ## Output UI
  column_sett_desc <- data.frame(
    title = c(' ', parameters[[1]]$headers, 'Color Legend'),
    type = c('checkbox', 'text', 'text', 'color'),
    render = c(NA, NA, NA, 'square'),
    align = c("center", "left", "left", "center"),
    wordWrap = c(NA, NA, T, NA),
    width = c(NA, NA, 300, 120)
  )
  
  get_column_sett <- function(titles) {
    n <- length(titles)
    w <- nchar(titles) * 10
    df <- data.frame(
      title = c(" ", " ", parameters[[1]]$headers[1], titles),
      type = c("color", "checkbox", "text", rep("text", n)),
      render = c("square", rep(NA, n + 2)),
      readOnly = c(T, F, F, rep(F, n)),
      align = c("center", "center", "left", rep("right", n)),
      width = c(NA, NA, NA, w)
      # mask =c(NA, NA, NA, rep('#.#', n))
    )
    return(df)
  }
  
  isDataEmpty <-
    function() {
      is.null(v$dataTable) || nrow(v$dataTable) == 0
    }
  
  create_xltable <-
    function(fields,
             colDef,
             display_scale = NULL,
             tableHeight = "300px") {
      if (isDataEmpty())
        return()
      data <- v$dataTable[fields]
      
      if (!is.null(display_scale)) {
        dsi <- which(!is.na(display_scale))
        s <-
          as.data.frame(t(replicate(nrow(data), display_scale[dsi])))
        data[dsi + 3] <- data[dsi + 3] * s
      }
      excelTable(
        data = data,
        columns = colDef,
        allowDeleteColumn = F,
        allowRenameColumn = F,
        columnSorting = F,
        allowInsertRow = F,
        allowDeleteRow = F,
        tableHeight = tableHeight,
        rowDrag = F,
        csvFileName = paste(fields, sep = "_"),
        includeHeadersOnDownload = T,
        tableOverflow = T,
        tableWidth = "100%"
      )
    }
  
  output[[pnlist[1]]] <- renderExcel({
    create_xltable(field_desc, column_sett_desc, NULL, NULL)
  })
  
  lapply(2:4, function(i) {
    output[[pnlist[i]]] <- renderExcel(create_xltable(
      c(field_labels, parameters[[i]]$fields),
      get_column_sett(parameters[[i]]$headers),
      parameters[[i]]$display_scale
    ))
  })
  
  lapply(5:length(pnlist), function(i) {
    output[[pnlist[i]]] <- renderExcel(create_xltable(
      c(field_labels, parameters[[i]]$fields),
      get_column_sett(parameters[[i]]$headers),
      parameters[[i]]$display_scale,
      NULL
    ))
  })
  
  output$plot_2a <- renderPlot({
    if (isDataEmpty())
      return()
    d <- v$dataPlot[c(field_labels, parameters[[2]]$fields)]
    ds <- d[d$select == T, ]
    if (nrow(ds) > 0)
      d <- ds
    n <- nrow(d)
    par(mar = c(4.5, 4, 1, 1))
    suppressWarnings(dbh_range_max <-
                       max(d[is.finite(d$DbhMaximum) , "DbhMaximum"], na.rm = T) * 100)
    if (is.infinite(dbh_range_max))
      dbh_range_max <- 100
    plot(
      1,
      type = "n",
      xlab = "Time (years)",
      ylab = "DBH (cm)",
      xlim = c(0, 100),
      ylim = c(0, dbh_range_max)
    )
    grid(
      nx = NULL,
      ny = NULL,
      lty = 2,
      col = "lightgray",
      lwd = 1
    )
    for (r in c(1:n)) {
      dbh_max <- d[r, "DbhMaximum"]
      k <- d[r, "DbhK"]
      c <- d[r, "DbhC"]
      color <- d[r, "color"]
      curve(
        eq_dbh(x, dbh_max, k, c) * 100,
        from = 1,
        to = 100,
        add = T,
        col = color,
        lwd = 2
      )
    }
  })
  
  output$plot_2b <- renderPlot({
    if (isDataEmpty())
      return()
    d <- v$dataPlot[c(field_labels, parameters[[2]]$fields)]
    ds <- d[d$select == T, ]
    if (nrow(ds) > 0)
      d <- ds
    n <- nrow(d)
    par(mar = c(4.5, 4, 1, 1))
    suppressWarnings(dbh_range_max <-
                       max(d[is.finite(d$DbhMaximum) , "DbhMaximum"], na.rm = T) * 100)
    if (is.infinite(dbh_range_max))
      dbh_range_max <- 100
    d$dincmax <-
      eq_dbh_inc_max(d["DbhMaximum"], d["DbhK"], d["DbhC"])
    incm <- unlist(d$dincmax)
    incm <- incm[is.finite(incm)]
    dincmax <- 10
    if (length(incm) > 0)
      dincmax <- min(max(incm, na.rm = T) * 100, 100)
    plot(
      1,
      type = "n",
      xlab = "DBH (cm)",
      ylab = "DBH increment (cm)",
      xlim = c(1, dbh_range_max),
      ylim = c(0, dincmax)
    )
    grid(
      nx = NULL,
      ny = NULL,
      lty = 2,
      col = "lightgray",
      lwd = 1
    )
    for (r in c(1:n)) {
      dbh_init <- d[r, "DbhInitial"]
      if (is.infinite(dbh_init) || is.na(dbh_init))
        dbh_init <- 0.01
      dbh_max <- d[r, "DbhMaximum"]
      k <- d[r, "DbhK"]
      c <- d[r, "DbhC"]
      color <- d[r, "color"]
      curve(
        eq_dbh_inc(x / 100, dbh_max, k, c) * 100,
        from = dbh_init,
        to = dbh_range_max,
        add = T,
        col = color,
        lwd = 2
      )
    }
  })
  
  output$page_2 <- renderUI({
    if (isDataEmpty())
      return()
    tagList(
      excelOutput(get_params_groups()[2], height = "300px"),
      br(),
      br(),
      p(
        "The parameters above are applied to a classical DBH function model
        by Chapman Richards."
      ),
      withMathJax(
        helpText(
          "DBH function over time (t):
                 $$dbh = dbh_{max}\\left(1-e^{-k \\cdot t}\\right)^c$$"
        )
      ),
      p("The DBH function curve:"),
      plotOutput("plot_2a"),
      p(
        "The DBH annual increment is estimated by the first derivative of
        the DBH function above with respect to time (t)."
      ),
      withMathJax(
        helpText(
          "Yearly DBH increment:
                 $$dbh_{incr} = dbh_{init} \\cdot c \\cdot k\\left[\\left(
                 \\frac{dbh_{init}}{dbh_{max}}\\right)^
                 \\frac{-1}{c}-1\\right]$$"
        )
      ),
      p("The DBH increment curve:"),
      plotOutput("plot_2b")
    )
  })
  
  output$plot_3a <- renderPlot({
    if (isDataEmpty())
      return()
    d <-
      v$dataPlot[c(field_labels, c(parameters[[3]]$fields, "DbhMaximum"))]
    ds <- d[d$select == T, ]
    if (nrow(ds) > 0)
      d <- ds
    n <- nrow(d)
    par(mar = c(4.5, 4, 1, 1))
    suppressWarnings(dbh_range_max <-
                       max(d[is.finite(d$DbhMaximum) , "DbhMaximum"], na.rm = T) * 100)
    if (is.infinite(dbh_range_max))
      dbh_range_max <- 100
    d$h_range_max <-
      eq_allometry(d["DbhMaximum"], d["HeightA"], d["HeightB"])
    incm <- unlist(d$h_range_max)
    incm <- incm[is.finite(incm)]
    h_range_max <- 10
    if (length(incm) > 0)
      h_range_max <- min(max(incm, na.rm = T), 500)
    plot(
      1,
      type = "n",
      xlab = "DBH (cm)",
      ylab = "Height (m)",
      xlim = c(0, dbh_range_max),
      ylim = c(0, h_range_max)
    )
    grid(
      nx = NULL,
      ny = NULL,
      lty = 2,
      col = "lightgray",
      lwd = 1
    )
    for (r in c(1:n)) {
      dbh_max <- d[r, "DbhMaximum"] * 100
      if (is.infinite(dbh_max) ||
          is.na(dbh_max))
        dbh_max <- dbh_range_max
      a <- d[r, "HeightA"]
      b <- d[r, "HeightB"]
      color <- d[r, "color"]
      curve(
        eq_allometry(x / 100, a, b),
        from = 0,
        to = dbh_max,
        add = T,
        col = color,
        lwd = 2
      )
    }
  })
  
  output$page_3 <- renderUI({
    if (isDataEmpty())
      return()
    tagList(
      excelOutput(get_params_groups()[3], height = "300px"),
      br(),
      br(),
      p("The allometric function relates tree height to tree DBH:"),
      withMathJax(helpText(
        "$$height = \\alpha \\cdot dbh^\\beta$$"
      )),
      p("The height allometric function curve:"),
      plotOutput("plot_3a")
    )
  })
  
  output$plot_4a <- renderPlot({
    if (isDataEmpty())
      return()
    d <-
      v$dataPlot[c(field_labels, c(parameters[[4]]$fields, "DbhMaximum"))]
    ds <- d[d$select == T, ]
    if (nrow(ds) > 0)
      d <- ds
    n <- nrow(d)
    par(mar = c(4.5, 4, 1, 1))
    suppressWarnings(dbh_range_max <-
                       max(d[is.finite(d$DbhMaximum) , "DbhMaximum"], na.rm = T) * 100)
    if (is.infinite(dbh_range_max))
      dbh_range_max <- 100
    d$h_range_max <-
      eq_crown_width(d["DbhMaximum"], d["CrownWidthA"], d["CrownWidthB"])
    incm <- unlist(d$h_range_max)
    incm <- incm[is.finite(incm)]
    h_range_max <- 10
    if (length(incm) > 0)
      h_range_max <- min(max(incm, na.rm = T), 500)
    plot(
      1,
      type = "n",
      xlab = "DBH (cm)",
      ylab = "Crown Width (m)",
      xlim = c(0, dbh_range_max),
      ylim = c(0, h_range_max)
    )
    grid(
      nx = NULL,
      ny = NULL,
      lty = 2,
      col = "lightgray",
      lwd = 1
    )
    for (r in c(1:n)) {
      dbh_max <- d[r, "DbhMaximum"] * 100
      if (is.infinite(dbh_max) ||
          is.na(dbh_max))
        dbh_max <- dbh_range_max
      a <- d[r, "CrownWidthA"]
      b <- d[r, "CrownWidthB"]
      color <- d[r, "color"]
      curve(
        eq_crown_width(x / 100, a, b),
        from = 0,
        to = dbh_max,
        add = T,
        col = color,
        lwd = 2
      )
    }
  })
  
  eq_crown_width <- function(dbh, a, b) {
    a + b * dbh
  }
  
  output$page_4 <- renderUI({
    if (isDataEmpty())
      return()
    tagList(
      excelOutput(get_params_groups()[4], height = "300px"),
      br(),
      br(),
      p(
        "The crown width is linearly related to tree dbh by the following function:"
      ),
      withMathJax(helpText("$${crown\\_width} = a + b \\cdot dbh$$")),
      p("The function curve:"),
      plotOutput("plot_4a")
    )
  })
  
  output$page_5 <- renderUI({
    if (isDataEmpty())
      return()
    tagList(excelOutput(get_params_groups()[5], height = "300px"),
            br(),
            br(),
            p(
              tags$ul(
                tags$li(
                  HTML("<b>Minimum</b> is the minimum light level for a tree to grows.")
                ),
                tags$li(
                  HTML("<b>Optimum</b> is the optimum light level for a tree to grows.")
                ),
                tags$li(
                  HTML(
                    "<b>Tree Flexi</b> is a parameter which measures the ratio of
                     height growth rates under the most contrasted light
                     conditions (between 0 and 1)."
                  )
                ),
                tags$li(
                  HTML(
                    "<b>Tree Sensi</b> is a measure of how sensitive the
                     species is to shading, sensi > 1 (e.g. 2) typical of
                     a shade avoiding species and sensi < 1 (e.g. 0.5) of
                     a shade tolerant species."
                  )
                ),
                tags$li(
                  HTML(
                    "<b>Crown Flexi</b> is a parameter which measures the ratio of
                     horizontal crown elongation under the contrasted light
                     conditions (between 0 and 1)."
                  )
                ),
                tags$li(
                  HTML(
                    "<b>Crown Sensi</b> is a measure of how sensitive the
                     species crown horizontal elongation to shading"
                  )
                )
              )
            ))
  })
  
  lapply(6:9, function(i) {
    output[[paste0("page_", i)]] <- renderUI({
      if (isDataEmpty())
        return()
      excelOutput(get_params_groups()[i])
    })
  })
  
  ## Add species data
  addSpeciesData <- function(species_data_df, isReplace = F) {
    if (isReplace)
      species_df <<- species_df[0, ]
    species_df <<- dplyr::bind_rows(species_df, species_data_df)
    isUpdateTable <<- TRUE
    v$dataPlot <- species_df
    v$dataTable <- species_df
  }
  
  observeEvent(input$add_species, {
    n <- nrow(species_df)
    i <- n %% length(color_list) + 1
    default_species$color <<- color_list[i]
    addSpeciesData(default_species)
  })
  
  observeEvent(input$file1, {
    ori_loaded_sp_df <<- read_species_file(input$file1$datapath)
    n_load <- names(ori_loaded_sp_df)
    n_sp <- names(species_df)
    
    n_load_int <- intersect(n_load, n_sp)
    loaded_sp_df <<- ori_loaded_sp_df[n_load_int]
    
    n_load_dif <- setdiff(n_load, n_sp)
    if (length(n_load_dif) > 0) {
      f_old <- intersect(field_map$f20, n_load_dif)
      in_sp_map_df <- ori_loaded_sp_df[f_old]
      names(in_sp_map_df) <-
        field_map[field_map$f20 %in% f_old, ]$f21
      if (length(f_old) > 0) {
        loaded_sp_df <<- cbind(loaded_sp_df, in_sp_map_df)
      }
    }
    
    if (F %in% c(field_color %in% n_sp)) {
      n <- nrow(species_df)
      if (n + nrow(loaded_sp_df) + 1 > length(color_list))
        n <- 0
      loaded_sp_df$color <<-
        color_list[n + 1:n + 1 + nrow(loaded_sp_df)]
    } else {
      cdf <-
        apply(loaded_sp_df[field_color], 2, function(x)
          as.numeric(as.character(x)))
      if (nrow(loaded_sp_df) == 1) {
        loaded_sp_df$color <<- rgb(matrix(cdf / 255, ncol = 3))
      } else {
        loaded_sp_df$color <<- rgb(cdf / 255)
      }
    }
    
    loaded_sp_df$select <<- F
    
    if (nrow(species_df) == 0) {
      addSpeciesData(loaded_sp_df)
    } else {
      inputSweetAlert(
        session = session,
        "load_species",
        input = "radio",
        inputOptions = load_sp_options,
        title = "Loading Species",
        text = "What to do with the newly loaded species to the current list?"
      )
    }
  })
  
  load_sp_options <- c("Appends" , "Replace")
  
  observeEvent(input$load_species, {
    if (input$load_species == load_sp_options[1]) {
      addSpeciesData(loaded_sp_df)
    } else if (input$load_species == load_sp_options[2]) {
      addSpeciesData(loaded_sp_df, T)
    }
  })
  
  ## Input data
  update_species_df <- function(pars_name) {
    df_input <- excel_to_R(input[[pars_name]])
    if (pars_name == pnlist[1]) {
      names(df_input) <- field_desc
      species_df[field_desc] <<- df_input
    } else {
      pf <- parameters[[pars_name]]$fields
      col_names <- c(field_labels, pf)
      names(df_input) <- col_names
      t <-
        tryCatch(
          df <-
            sapply(df_input[pf], function(x)
              as.numeric(as.character(x))),
          error = function(e)
            e,
          warning = function(w)
            w
        )
      if (is(t, "warning")) {
        showNotification("Input error", type = "error")
        v$dataTable <- NULL
        v$dataTable <- species_df
      } else {
        df_input[pf] <- df
        display_scale <- parameters[[pars_name]]$display_scale
        if (!is.null(display_scale)) {
          dsi <- which(!is.na(display_scale))
          s <-
            as.data.frame(t(replicate(
              nrow(df_input), display_scale[dsi]
            )))
          df_input[dsi + 3] <- df_input[dsi + 3] / s
        }
        species_df[col_names] <<- df_input
        v$dataPlot <- species_df
      }
    }
    
  }
  
  lapply(pnlist, function(n) {
    observeEvent(input[[n]], {
      update_species_df(n)
    })
  })
  
  ## Remove species
  observeEvent(input$remove, {
    if (nrow(species_df[species_df$select == T,]) == 0) {
      showNotification("Nothing is selected", type = "message")
      return()
    }
    ask_confirmation(
      inputId = "myconfirmation",
      title = "Removing Species",
      text = "The selected species will be removed. Are you sure?",
      type = "warning"
    )
  })
  
  observeEvent(input$myconfirmation, {
    if (isTRUE(input$myconfirmation)) {
      species_df <<- species_df[species_df$select == F,]
      v$dataTable <- species_df
      v$dataPlot <- species_df
    }
  })
  
  ## Duplicate species
  observeEvent(input$duplicate, {
    select_df <- species_df[species_df$select == T,]
    if (nrow(select_df) == 0) {
      showNotification("Nothing is selected", type = "message")
      return()
    }
    species_df <<- rbind(species_df, select_df)
    v$dataTable <- species_df
    v$dataPlot <- species_df
  })
  
  ## Download data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste('species_data-', Sys.Date(), '.trs', sep = '')
    },
    content = function(con) {
      rgb <- as.data.frame(t(col2rgb(species_df$color)))
      species_df[field_color] <- rgb
      df_out <- subset(species_df, select = -c(select, color))
      write_xml(create_xml_species(df_out), con)
    }
  )
  
  ###################################################################
  ##### SExI-FS Output ##############################################
  ###################################################################
  
  sexi_data <- reactiveVal(NULL)
  sexi_data_c <- reactiveVal(NULL)
  
  # Chave J, Andalo C, Brown S, et al (2005) Tree allometry and improved estimation of carbon stocks and balance in tropical forests. Oecologia 145:87-99. https://doi.org/10.1007/s00442-005-0100-x
  # unit: cm, m, g/cm3, agb in kg
  agb <- function(d, h, wd) {
    # 0.0509 * p * D^2 * H
    0.0509 * wd * d ^ 2 * h
  }
  
  output$sexi_output <- renderUI({
    tagList(
      fileInput(
        "sexi_file",
        "Load SExI-FS output file",
        accept = c("SExI-FS output file", ".txt")
      ),
      setSliderColor(c("#6c584c", "#6c584c"), c(1, 2)),
      div(class = "borderbox",
          fluidRow(
            column(
              6,
              sliderInput(
                "aggregate_input",
                label = "Time aggregate (years)",
                min = 1,
                max = 10,
                value = 5
              )
            ),
            column(
              6,
              sliderInput(
                "max_time",
                label = "Maximum time (years)",
                value = 30,
                min = 1,
                max = 30
              )
            )
          )),
      plotOutput("plot_sexi_carbon"),
      hr(class = "brown"),
      plotOutput("plot_sexi_seq"),
      hr(class = "brown"),
      plotOutput("plot_sexi_ntree"),
      hr(class = "brown"),
      h4("Carbon stock data (t/ha)", style = "text-align: center;"),
      excelOutput("table_sexi_output")
    )
  })
  
  observeEvent(input$sexi_file, {
    if (is.null(loaded_sp_df))
      return()
    df <- read.delim(input$sexi_file$datapath)
    tdf <- merge(df, loaded_sp_df, by.x = "SPECIES", by.y = "Label")
    tdf$agb <- agb(tdf$DBH * 100, tdf$HEIGHT, tdf$WoodDensity)
    tdf$c <- tdf$agb * 0.5 / 1000
    sexi_data(tdf)
    max_t <- max(tdf$SIM_AGE)
    updateSliderInput(session,
                      inputId = "max_time",
                      max = max_t,
                      value = max_t)
    update_disp_data(input$aggregate_input, max_t)
  })
  
  update_disp_data <- function(agg_t, max_t = NULL) {
    isolate(tdf <- sexi_data())
    if (is.null(tdf))
      return()
    if (is.null(max_t))
      max_t <- max(tdf$tdf$SIM_AGE)
    t5 <- seq(agg_t, max_t, agg_t)
    tdf_5 <- tdf[tdf$SIM_AGE %in% t5,]
    
    c_df <-
      aggregate(tdf_5$c,
                by = list(tdf_5$SIM_AGE, tdf_5$SPECIES),
                FUN = sum)
    names(c_df) <- c("t", "sp", "c")
    c_bar <- cast(c_df, sp ~ t, value = "c")
    sexi_data_c(c_bar)
  }
  
  observeEvent(input$aggregate_input | input$max_time, {
    if (is.null(input$aggregate_input) |
        is.null(input$max_time))
      return()
    updateSliderInput(session,
                      inputId = "max_time",
                      min = input$aggregate_input * 2)
    if (input$aggregate_input > input$max_time)
      return()
    update_disp_data(input$aggregate_input, input$max_time)
  })
  
  
  output$plot_sexi_carbon <- renderPlot({
    c_bar <- sexi_data_c()
    if (is.null(c_bar))
      return()
    if (ncol(c_bar) <= 2)
      return()
    colors <- color_list[1:length(c_bar$sp)]
    par(mar = c(4, 4, 4, 2) + 0.1)
    v_bar <- apply(c_bar[-1], 2, rev)
    y <- round(colSums(c_bar[-1]), 2)
    lab = "Total carbon stock (t/ha)"
    ytop <- max(y) * 1.05
    x <- barplot(
      v_bar,
      ylab = lab,
      names.arg = colnames(c_bar[-1]),
      ylim = c(0, ytop),
      col = rev(colors),
      xlab = "Simulation time (years)",
      main = lab
    )
    ConnLines(v_bar, col = "grey50")
    legend(
      "topleft",
      legend = c_bar$sp,
      inset = 0.01,
      bty = "n",
      title = "Species",
      fill = colors,
      cex = 1
    )
    text(x, y + ytop * 0.03, labels = as.character(y))
  })
  
  output$plot_sexi_seq <- renderPlot({
    c_bar <- sexi_data_c()
    if (is.null(c_bar))
      return()
    if (ncol(c_bar) <= 2)
      return()
    isolate(agg_t <- input$aggregate_input)
    colors <- color_list[1:length(c_bar$sp)]
    
    par(mar = c(4, 4, 3, 2) + 0.1)
    
    v_bar <- apply(c_bar[-1], 2, rev)
    init <- cbind(0, v_bar[,-ncol(v_bar)])
    v_bar2 <- v_bar - init
    y2 <- round(colSums(v_bar2), 2)
    ytop2 <- max(y2) * 1.05
    lab <-
      paste("Carbon sequestration within", agg_t, "years period")
    x2 <- barplot(
      v_bar2,
      ylab = paste0("Carbon sequestration (t/ha.", agg_t, "yr)") ,
      names.arg = colnames(c_bar[-1]),
      ylim = c(0, ytop2),
      col = rev(colors),
      xlab = "Simulation time (years)",
      main = lab,
      sub =
    )
    
    legend(
      "topleft",
      legend = c_bar$sp,
      inset = 0.01,
      bty = "n",
      title = "Species",
      fill = colors,
      cex = 1
    )
    
    text(x2, y2 + ytop2 * 0.03, labels = as.character(y2))
  })
  
  output$plot_sexi_ntree <- renderPlot({
    tdf <- sexi_data()
    if (is.null(tdf))
      return()
    c_bar <- sexi_data_c()
    agg_t <- input$aggregate_input
    max_t <- input$max_time
    t5 <- seq(agg_t, max_t, agg_t)
    if (length(t5) <= 1)
      return()
    tdf_5 <- tdf[tdf$SIM_AGE %in% t5,]
    n_df <-
      aggregate(tdf_5$c,
                by = list(tdf_5$SIM_AGE, tdf_5$SPECIES),
                FUN = length)
    names(n_df) <- c("t", "sp", "n")
    
    n_bar <- cast(n_df, sp ~ t, value = "n")
    vn_bar <- apply(n_bar[-1], 2, rev)
    y <- round(colSums(n_bar[-1]), 2)
    ytop <- max(y) * 1.05
    
    par(mar = c(4, 4, 3, 2) + 0.1)
    colors <- color_list[1:length(unique(n_df$sp))]
    x <- barplot(
      vn_bar,
      ylab = "Number of trees",
      names.arg = colnames(n_bar[-1]),
      ylim = c(0, ytop),
      col = rev(colors),
      xlab = "Simulation time (years)",
      main = "Number of individual trees"
    )
    ConnLines(vn_bar, col = "grey50")
    legend(
      "topright",
      legend = n_bar$sp,
      inset = 0.01,
      bty = "n",
      title = "Species",
      fill = colors,
      cex = 1
    )
    yd <- ytop * 0.03
    text(x, y + yd, labels = as.character(y))
    
  })
  
  output$table_sexi_output <- renderExcel({
    d <- sexi_data_c()
    if (is.null(d))
      return()
    n <- ncol(d)
    out_column <- data.frame(
      title = colnames(d),
      type = c("text", rep("numeric", n - 1)),
      align = c("left", rep("right", n - 1))
    )
    excelTable(
      data = d,
      columns = out_column,
      editable = F,
      allowDeleteColumn = F,
      allowRenameColumn = F,
      allowInsertRow = F,
      allowDeleteRow = F,
      tableHeight = "800",
      rowDrag = F,
      csvFileName = "SExI-FS_output_table",
      includeHeadersOnDownload = T,
      tableOverflow = T,
      tableWidth = "100%"
    )
  })
}