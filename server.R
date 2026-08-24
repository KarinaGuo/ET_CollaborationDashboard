# server.R

server <- function(input, output, session) {
  
  # Call the secure server module
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  # Reactive file reader to monitor Species_tracking.csv for updates
  tracking_data <- reactiveFileReader(
    intervalMillis = 1000, 
    session = session, 
    filePath = file.path(data_dir, "Species_tracking.csv"),
    readFunc = read.csv,
    stringsAsFactors = FALSE
  )
  
  # Reactive file reader for Seed collections
  seed_collections_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = file.path(data_dir, "1_Seed_collections.csv"),
    readFunc = read.csv,
    stringsAsFactors = FALSE
  )
  
  # Reactive file reader for Seedling growth
  seedling_growth_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = file.path(data_dir, "2_Seedling_growth.csv"),
    readFunc = read.csv,
    stringsAsFactors = FALSE
  )
  
  # Reactive file reader for Scoring results
  scoring_results_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = file.path(data_dir, "3a_Scoring_results_COI.csv"),
    readFunc = read.csv,
    stringsAsFactors = FALSE
  )
  
  # Reactive file reader for Monitoring results
  monitoring_results_data <- reactiveFileReader(
    intervalMillis = 2000,
    session = session,
    filePath = file.path(data_dir, "4_Monitoring_results.csv"),
    readFunc = read.csv,
    stringsAsFactors = FALSE
  )
  
  # Read phenology data once per user session (app load)
  phenology_data <- {
    data <- read.csv(file.path(data_dir, "Phenology_data.csv"), stringsAsFactors = FALSE)
    
    # Ignore placeholder rows with NA months
    data <- data[!is.na(data$StartMonth) & !is.na(data$EndMonth), ]
    
    end_day <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    
    new_data <- list()
    for (i in seq_len(nrow(data))) {
      row <- data[i, ]
      if (row$StartMonth <= row$EndMonth) {
        row$Start <- as.Date(paste0("2024-", sprintf("%02d", row$StartMonth), "-01"))
        row$End <- as.Date(paste0("2024-", sprintf("%02d", row$EndMonth), "-", end_day[row$EndMonth]))
        new_data[[length(new_data) + 1]] <- row
      } else {
        # Split cross-year phenology (e.g. Sept to March) into two intervals
        row1 <- row
        row1$Start <- as.Date(paste0("2024-", sprintf("%02d", row$StartMonth), "-01"))
        row1$End <- as.Date("2024-12-31")
        
        row2 <- row
        row2$Start <- as.Date("2024-01-01")
        row2$End <- as.Date(paste0("2024-", sprintf("%02d", row$EndMonth), "-", end_day[row$EndMonth]))
        
        new_data[[length(new_data) + 1]] <- row1
        new_data[[length(new_data) + 1]] <- row2
      }
    }
    do.call(rbind, new_data)
  }
  
  # Filtered data based on user permissions
  user_tracking_data <- reactive({
    req(res_auth$user) # Wait until authentication is complete
    
    # Find the user's allowed species
    user_row <- credentials %>% filter(user == res_auth$user)
    if (nrow(user_row) == 0) return(NULL)
    
    # Parse the semicolon separated string
    allowed_species_str <- user_row$Species[1]
    allowed_species <- strsplit(allowed_species_str, ";\\s*")[[1]]
    
    # Add a Permitted column instead of filtering out
    data <- tracking_data()
    data %>% mutate(Permitted = Species %in% allowed_species)
  })

  # Reactive for user allowed sites
  user_allowed_sites <- reactive({
    req(res_auth$user)
    user_row <- credentials %>% filter(user == res_auth$user)
    if (nrow(user_row) == 0 || is.null(user_row$Sites) || is.na(user_row$Sites)) return(character(0))
    strsplit(user_row$Sites, "[;,]\\s*")[[1]]
  })
  
  # Render the All Species table
  output$species_table <- renderReactable({
    data <- user_tracking_data()
    req(data)
    
    reactable(
      data,
      selection = "single",
      onClick = "select",
      highlight = TRUE,
      filterable = TRUE,
      rowStyle = JS("function(rowInfo) {
        if (rowInfo && !rowInfo.values['Permitted']) {
          return { color: '#adb5bd', cursor: 'not-allowed' }
        }
      }"),
      columns = list(
        Permitted = colDef(show = FALSE)
      )
    )
  })
  
  # Eager observer to push the selected species to JS (to break conditionalPanel deadlock)
  observe({
    data <- user_tracking_data()
    req(data)
    selected_idx <- getReactableState("species_table", "selected")
    
    if (is.null(selected_idx) || !data$Permitted[selected_idx]) {
      runjs("Shiny.setInputValue('selected_species', null);")
    } else {
      species_name <- data$Species[selected_idx]
      runjs(sprintf("Shiny.setInputValue('selected_species', '%s');", species_name))
      
      # Automatically navigate to the Species Insight tab!
      nav_select("main_nav", selected = "tab_insight")
    }
  })

  # Reactive to capture the currently selected species for internal outputs
  selected_species <- reactive({
    data <- user_tracking_data()
    req(data)
    selected_idx <- getReactableState("species_table", "selected")
    
    if (is.null(selected_idx) || !data$Permitted[selected_idx]) {
      return(NULL)
    }
    
    return(data$Species[selected_idx])
  })
  
  # Title for the subset tabs
  output$subset_tabs_title <- renderUI({
    sp <- selected_species()
    req(sp)
    tags$h4(paste("Details for:", sp))
  })
  
  # Render the dynamic Subset Tabs
  output$subset_tabs_ui <- renderUI({
    sp <- selected_species()
    req(sp)
    
    # Get the progress status for the selected species
    data <- user_tracking_data()
    sp_data <- data %>% filter(Species == sp)
    
    # Helper to check if we should disable a tab
    is_disabled <- function(stage) {
      if (nrow(sp_data) == 0 || !(stage %in% names(sp_data))) return(TRUE)
      status <- sp_data[[stage]][1]
      return(is.na(status) || status == "Not yet started")
    }
    
    # Build tabs
    tabsetPanel(
      id = "subset_tabs",
      
      tabPanel(
        title = "1. Seed Collection",
        value = "tab_seed",
        br(),
        if(is_disabled("SeedCollection")) h5("Not yet conducted") else leafletOutput("seed_map", height = "800px")
      ),
      
      tabPanel(
        title = "2. Seedling Growth & Scoring",
        value = "tab_growth_scoring",
        br(),
        if(is_disabled("SeedlingGrowth") && is_disabled("Scoring")) {
          h5("Not yet conducted")
        } else {
          open_panels <- if (!is_disabled("Scoring")) "Scoring" else "Seedling Growth"
          
          accordion(
            open = open_panels,
            multiple = TRUE,
            accordion_panel(
              title = "Seedling Growth",
              if(is_disabled("SeedlingGrowth")) p("Not yet conducted") else tagList(
                downloadButton("download_growth", "Download CSV", class = "btn-sm mb-2", style = "padding: 2px 8px;"),
                reactableOutput("growth_table")
              )
            ),
            accordion_panel(
              title = "Scoring",
              if(is_disabled("Scoring")) p("Not yet conducted") else tagList(
                plotOutput("scoring_density_plot", height = "300px"),
                br(),
                downloadButton("download_scoring", "Download CSV", class = "btn-sm mb-2", style = "padding: 2px 8px;"),
                reactableOutput("scoring_table")
              )
            )
          )
        }
      ),
      
      tabPanel(
        title = "3. Genetic Studies",
        value = "tab_genetics",
        br(),
        if(is_disabled("GeneticStudies")) h5("Not yet conducted") else uiOutput("genetic_figures")
      ),
      
      tabPanel(
        title = "4. Deployment & Monitoring",
        value = "tab_monitoring",
        br(),
        if(is_disabled("Monitoring")) h5("Not yet conducted") else tagList(
          downloadButton("download_monitoring", "Download CSV", class = "btn-sm mb-2", style = "padding: 2px 8px;"),
          reactableOutput("monitoring_table")
        )
      )
    )
  })
  
  # Map rendering for Seed Collection
  output$seed_map <- renderLeaflet({
    sp <- selected_species()
    req(sp)
    
    # Check if disabled
    data <- user_tracking_data()
    sp_data <- data %>% filter(Species == sp)
    if (nrow(sp_data) == 0 || !( "SeedCollection" %in% names(sp_data) )) return(NULL)
    if (is.na(sp_data$SeedCollection[1]) || sp_data$SeedCollection[1] == "Not yet started") return(NULL)
    
    # 1. Read ALA occurrence data
    ala_file <- file.path(data_dir, paste0(gsub(" ", "_", sp), "_Occurrence_Data.csv"))
    ala_data <- NULL
    if (file.exists(ala_file)) {
      ala_data <- read.csv(ala_file)
    }
    
    # 2. Get seed collections for this species
    seed_data <- seed_collections_data() %>% filter(Species == sp)
    
    # Build map and set initial zoom to bounding box
    m <- leaflet() %>% addProviderTiles(providers$CartoDB.Positron)
    
    lngs <- numeric(0)
    lats <- numeric(0)
    
    if (!is.null(ala_data) && nrow(ala_data) > 0) {
      lngs <- c(lngs, ala_data$decimalLongitude)
      lats <- c(lats, ala_data$decimalLatitude)
    }
    if (nrow(seed_data) > 0) {
      lngs <- c(lngs, seed_data$Longitude)
      lats <- c(lats, seed_data$Latitude)
    }
    
    if (length(lngs) > 0 && length(lats) > 0) {
      m <- m %>% fitBounds(
        lng1 = min(lngs, na.rm = TRUE),
        lat1 = min(lats, na.rm = TRUE),
        lng2 = max(lngs, na.rm = TRUE),
        lat2 = max(lats, na.rm = TRUE)
      )
    }
    
    # Add ALA dots
    if (!is.null(ala_data) && nrow(ala_data) > 0) {
      m <- m %>% addCircleMarkers(
        data = ala_data,
        lat = ~decimalLatitude,
        lng = ~decimalLongitude,
        color = "grey",
        radius = 1,
        stroke = FALSE,
        fillOpacity = 0.1
      )
    }
    
    # Add seedlot dots
    if (nrow(seed_data) > 0) {
      # Fallback to Unsure if missing, just in case
      seed_data$SpeciesConfidence[is.na(seed_data$SpeciesConfidence)] <- "Unsure"
      
      pal <- colorFactor(
        palette = c("#4b7d4d", "#9c4f44"),
        levels = c("Confident", "Unsure")
      )
      
      m <- m %>% addCircleMarkers(
        data = seed_data,
        lat = ~Latitude,
        lng = ~Longitude,
        color = "white",
        weight = 1,
        fillColor = ~pal(SpeciesConfidence),
        fillOpacity = 1,
        radius = 4,
        label = ~MaternalLine
      ) %>%
        addLegend(
          position = "bottomright",
          pal = pal,
          values = c("Confident", "Unsure"),
          title = "Species Confidence",
          opacity = 1
        )
    }
    
    m
  })
  
  # Seedling Growth Table
  output$growth_table <- renderReactable({
    sp <- selected_species()
    req(sp)
    
    # Check if disabled
    data <- user_tracking_data()
    sp_data <- data %>% filter(Species == sp)
    if (nrow(sp_data) == 0 || !( "SeedlingGrowth" %in% names(sp_data) )) return(NULL)
    if (is.na(sp_data$SeedlingGrowth[1]) || sp_data$SeedlingGrowth[1] == "Not yet started") return(NULL)
    
    growth_df <- seedling_growth_data()
    
    # Filter for the selected species
    sp_growth <- growth_df %>% filter(Species == sp)
    
    # Ensure it's an empty dataframe with correct columns if no data
    if (nrow(sp_growth) == 0) {
      sp_growth <- growth_df[0, ]
    }
    
    reactable(
      sp_growth,
      highlight = TRUE,
      compact = TRUE,
      pagination = FALSE,
      filterable = TRUE
    )
  })
  
  # Scoring Plots and Table
  
  output$scoring_density_plot <- renderPlot({
    sp <- selected_species()
    req(sp)
    df <- scoring_results_data() %>% filter(Species == sp)
    req(nrow(df) > 0)
    
    ggplot(df, aes(x = summary_score)) +
      geom_histogram(fill = "#4b7d4d", color = "white", bins = 30, alpha = 0.8) +
      theme_minimal(base_size = 14) +
      labs(
        title = "Overall Species Distribution",
        x = "Susceptibility Score",
        y = "Frequency"
      )
  })
  
  output$scoring_table <- renderReactable({
    sp <- selected_species()
    req(sp)
    df <- scoring_results_data() %>% filter(Species == sp)
    
    # Hide the first column if it is just a row index from write.csv
    if (names(df)[1] == "X" || names(df)[1] == "") {
      df <- df[, -1, drop = FALSE]
    }
    
    reactable(
      df,
      highlight = TRUE,
      compact = TRUE,
      pagination = TRUE,
      filterable = TRUE
    )
  })
  
  # Monitoring Table
  output$monitoring_table <- renderReactable({
    sp <- selected_species()
    req(sp)
    df <- monitoring_results_data() %>% filter(Species == sp)
    
    # Filter by user permitted sites
    allowed_sites <- user_allowed_sites()
    if (length(allowed_sites) > 0 && !all(allowed_sites == "") && !all(tolower(allowed_sites) == "all")) {
      df <- df %>% filter(Site %in% allowed_sites)
    }
    
    if (names(df)[1] == "X" || names(df)[1] == "") {
      df <- df[, -1, drop = FALSE]
    }
    
    if (nrow(df) == 0) {
      df <- monitoring_results_data()[0, ]
      if (names(df)[1] == "X" || names(df)[1] == "") {
        df <- df[, -1, drop = FALSE]
      }
    }
    
    reactable(
      df,
      highlight = TRUE,
      compact = TRUE,
      pagination = TRUE,
      filterable = TRUE
    )
  })
  
  # Download Handlers
  output$download_species <- downloadHandler(
    filename = function() { "Species_Overview.csv" },
    content = function(file) {
      write.csv(user_tracking_data(), file, row.names = FALSE)
    }
  )
  
  output$download_growth <- downloadHandler(
    filename = function() { paste0(gsub(" ", "_", selected_species()), "_Seedling_Growth.csv") },
    content = function(file) {
      growth_df <- seedling_growth_data()
      sp_growth <- growth_df %>% filter(Species == selected_species())
      if (nrow(sp_growth) == 0) sp_growth <- growth_df[0, ]
      write.csv(sp_growth, file, row.names = FALSE)
    }
  )
  
  output$download_scoring <- downloadHandler(
    filename = function() { paste0(gsub(" ", "_", selected_species()), "_Scoring.csv") },
    content = function(file) {
      df <- scoring_results_data() %>% filter(Species == selected_species())
      if (names(df)[1] == "X" || names(df)[1] == "") {
        df <- df[, -1, drop = FALSE]
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  output$download_monitoring <- downloadHandler(
    filename = function() { paste0(gsub(" ", "_", selected_species()), "_Monitoring.csv") },
    content = function(file) {
      df <- monitoring_results_data() %>% filter(Species == selected_species())
      
      # Filter by user permitted sites
      allowed_sites <- user_allowed_sites()
      if (length(allowed_sites) > 0 && !all(allowed_sites == "") && !all(tolower(allowed_sites) == "all")) {
        df <- df %>% filter(Site %in% allowed_sites)
      }
      
      if (names(df)[1] == "X" || names(df)[1] == "") df <- df[, -1, drop = FALSE]
      if (nrow(df) == 0) {
        df <- monitoring_results_data()[0, ]
        if (names(df)[1] == "X" || names(df)[1] == "") df <- df[, -1, drop = FALSE]
      }
      write.csv(df, file, row.names = FALSE)
    }
  )
  
  # Genetic Studies Figures
  output$genetic_figures <- renderUI({
    # Find all images starting with 3_ in Figures directory
    img_files <- list.files("Figures", pattern = "^3_.*\\.(png|jpg|jpeg)$", full.names = FALSE)
    
    if (length(img_files) == 0) {
      return(p("No genetic study figures found."))
    }
    
    # Create HTML img tags for each
    img_tags <- lapply(img_files, function(f) {
      card(
        card_body(
          padding = 0,
          tags$img(src = paste0("figures/", f), style = "width: 100%; height: auto;")
        )
      )
    })
    
    # Use bslib to wrap them dynamically into a patchwork grid
    do.call(layout_column_wrap, c(list(width = 1), img_tags))
  })
  
  # Phenology Gantt Chart
  output$phenology_plot <- renderPlotly({
    allowed_sp <- user_tracking_data()$Species
    req(allowed_sp)
    
    p_data <- phenology_data %>% filter(Species %in% allowed_sp)
    req(nrow(p_data) > 0)
    
    # Convert Species to numeric for geom_rect to create a 2D bar area for hovering
    sp_levels <- unique(p_data$Species)
    p_data$SpeciesNum <- as.numeric(factor(p_data$Species, levels = sp_levels))
    
    # Current date as a dummy in 2024
    curr_month <- as.numeric(format(Sys.Date(), "%m"))
    curr_day <- as.numeric(format(Sys.Date(), "%d"))
    curr_date_dummy <- as.Date(paste0("2024-", sprintf("%02d", curr_month), "-", sprintf("%02d", curr_day)))
    
    p <- ggplot(p_data, aes(
        xmin = Start, xmax = End, 
        ymin = SpeciesNum - 0.2, ymax = SpeciesNum + 0.2, 
        fill = PhenologyEvent, 
        text = paste("Reference:", ShortRef)
      )) +
      geom_rect(alpha = 0.5) +
      geom_vline(xintercept = as.numeric(curr_date_dummy), linetype = "dashed", color = "black") +
      scale_x_date(date_labels = "%b", date_breaks = "1 month", limits = as.Date(c("2024-01-01", "2024-12-31"))) +
      scale_y_continuous(breaks = seq_along(sp_levels), labels = sp_levels) +
      theme_minimal() +
      theme(
        axis.title = element_blank(),
        panel.grid.minor = element_blank()
      ) +
      scale_fill_manual(values = c("Flowering" = "#e74c3c", "Fruiting" = "#2ecc71"))
      
    ggplotly(p, tooltip = "text") %>%
      layout(
        legend = list(orientation = "h", x = 0.5, y = -0.3, xanchor = "center", yanchor = "top"),
        margin = list(b = 60),
        xaxis = list(fixedrange = TRUE),
        yaxis = list(fixedrange = TRUE)
      )
  })
  
  # Phenology References
  output$phenology_refs <- renderUI({
    refs <- unique(phenology_data$FullRef)
    tagList(
      tags$h6("References:", class = "text-muted", style = "font-size: 0.85rem; margin-bottom: 0.2rem;"),
      tags$ul(class = "text-muted", style = "font-size: 0.75rem;",
        lapply(refs, tags$li)
      )
    )
  })
  
}
