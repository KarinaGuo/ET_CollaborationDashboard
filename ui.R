# ui.R

# Define the main UI
ui <- page_navbar(
  title = "Myrtle Rust Experimental Progress",
  id = "main_nav",
  theme = bs_theme(version = 5, preset = "lux"), # Using a sleek bslib theme
  
  # Initialize shinyjs for disabling/enabling tabs dynamically
  header = tagList(
    useShinyjs(),
    tags$style(HTML("
      /* Custom CSS for disabled tabs */
      .nav-link.disabled {
        opacity: 0.5;
        cursor: not-allowed;
      }
    "))
  ),
  
  nav_panel(
    title = "Species Overview",
    
    # Top section: All species table
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header(
        "All Species Progress",
        downloadButton("download_species", "Download CSV", class = "btn-sm float-end", style = "padding: 2px 8px;")
      ),
      reactableOutput("species_table")
    ),
    
    # Middle section: Phenology Timelines
    card(
      card_header("Expected Phenology"),
      plotlyOutput("phenology_plot", height = "300px"),
      hr(),
      uiOutput("phenology_refs")
    )
  ),
  
  nav_panel(
    title = "Species Insight",
    value = "tab_insight",
    
    conditionalPanel(
      condition = "input.selected_species == null || input.selected_species == ''",
      div(
        class = "text-center mt-5",
        h4("Select a species in the Species Overview tab to see details about progress.", class = "text-muted")
      )
    ),
    
    conditionalPanel(
      condition = "input.selected_species != null && input.selected_species != ''",
      card(
        card_header(uiOutput("subset_tabs_title")),
        # We render the tabs from the server so we can dynamically disable them based on progress
        uiOutput("subset_tabs_ui")
      )
    )
  ),
  
  nav_panel(
    title = "About",
    p("This dynamically updating dashboard tracks the experimental progress of Myrtle Rust susceptible species.")
  )
)

# Wrap UI in secure_app for authentication
secure_app(ui)
