library(shiny)
library(shinyjs)
library(ggplot2)
library(RColorBrewer)
# Conditionally load ggdist if available
if (!requireNamespace("ggdist", quietly = TRUE)) {
  message("Package 'ggdist' not available; falling back to standard ggplot visualization")
}



ORIGINAL_INCOME_MEAN <- 2943.008
ORIGINAL_INCOME_SD <-   1713.834
  
# Coefficients (largely same as before, but be precise about variable definitions)
coeffs_class1 <- list(
  intercept = 4.48,
  age = -0.03,       # Raw age in years
  female = -0.31,    # 1 if female, 0 otherwise
  income_scaled = 0.05, # Coefficient for SCALED income
  england = 0.22,    # 1 if England, 0 otherwise
  citizen_science_not_involved = -1.32 # 1 if NOT involved, 0 if involved
)

coeffs_class2 <- list(
  intercept = -0.19,
  age = 0.00,
  female = 0.48,
  income_scaled = -0.03,
  england = -0.31,
  citizen_science_not_involved = 0.29
)


calculate_class_probabilities <- function(age_input, gender_input, income_bracket_input, 
                                          country_input, citizen_science_input,
                                          orig_inc_mean, orig_inc_sd) {
  # Age (raw)
  model_age <- as.numeric(age_input)
  
  # Female (1 if Female, 0 otherwise)
  model_female <- ifelse(gender_input == "Female", 1, 0)
  
  # Income:
  # 1. Get midpoint from bracket (as your app currently does)
  #    input$income gives the value attribute, which is already the midpoint or NA
  raw_income_midpoint <- as.numeric(income_bracket_input) 
  
  # 2. Apply the <500 recode
  recoded_income <- ifelse(is.na(raw_income_midpoint), 
                           NA, # Handle 'Prefer not to say' for income
                           ifelse(raw_income_midpoint < 500, 250, raw_income_midpoint))
  
  # 3. Scale it using the original dataset's mean and sd of this recoded variable
  if (is.na(recoded_income)) {
    model_income_scaled <- 0 # Or some other imputation for NA. 
    # Using 0 means they are at the mean of the scaled variable.
  } else {
    model_income_scaled <- (recoded_income - orig_inc_mean) / orig_inc_sd
  }
  
  # England (1 if England, 0 otherwise)
  model_england <- ifelse(country_input == "England", 1, 0)
  
  # Citizen Science (Not Involved = 1, Involved = 0)
  # input$citizen_science: "No"=0, "Yes"=1
  model_cs_not_involved <- ifelse(citizen_science_input == "0", 1, 0) 
  
  # --- Calculate log-odds ---
  log_odds1_vs_3 <- coeffs_class1$intercept +
    (coeffs_class1$age * model_age) +
    (coeffs_class1$female * model_female) +
    (coeffs_class1$income_scaled * model_income_scaled) + # Use scaled income
    (coeffs_class1$england * model_england) +
    (coeffs_class1$citizen_science_not_involved * model_cs_not_involved)
  
  log_odds2_vs_3 <- coeffs_class2$intercept +
    (coeffs_class2$age * model_age) +
    (coeffs_class2$female * model_female) +
    (coeffs_class2$income_scaled * model_income_scaled) + # Use scaled income
    (coeffs_class2$england * model_england) +
    (coeffs_class2$citizen_science_not_involved * model_cs_not_involved)
  
  # --- Convert to probabilities ---
  # (same as before)
  denom <- (1 + exp(log_odds1_vs_3) + exp(log_odds2_vs_3))
  prob_class1 <- exp(log_odds1_vs_3) / denom
  prob_class2 <- exp(log_odds2_vs_3) / denom
  prob_class3 <- 1 / denom # Corrected slightly for numerical stability if any exp() is huge
  
  return(list(
    prob_pro_insect = prob_class1, 
    prob_insect_averse = prob_class2, 
    prob_ambivalent = prob_class3
  ))
}
  
# BIOWELL Multi-Page Survey Shiny App
# Parameters for pages
ages <- as.character(0:100)
genders <- c("Female", "Male", "Prefer not to say", "In another way")
countries <- c("England", "Scotland", "Wales")








# Slider endpoints
dims <- list(
  c("Physically relaxed", "Physically tense"),
  c("Joyful", "Sad"),
  c("Clear minded", "Muddled"),
  c("Open to people", "Closed to people"),
  c("Part of something bigger than myself", "Not part of something bigger than myself")
)

# Stem × insect questions
stems <- c(
  "Knowing that I can encounter beetles when I am outside makes me feel…",
  "Knowing that I can encounter bees when I am outside makes me feel…",
  "Knowing that I can encounter wasps when I am outside makes me feel…",
  "Knowing that beetles exist now makes me feel…",
  "Knowing that bees exist now makes me feel…",
  "Knowing that wasps exist now makes me feel…",
  "Knowing that beetles will exist in the future makes me feel…",
  "Knowing that bees will exist in the future makes me feel…",
  "Knowing that wasps will exist in the future makes me feel…"
)

n_pages <- 1 + length(stems) + 1  # demographics + each stem + final

ui <- fluidPage(
  useShinyjs(),
  titlePanel("DRUID Insect Survey: Wellbeing"),
  sidebarLayout(
    sidebarPanel(
      h4("Progress"),
      textOutput("page_label"),
      width = 3
    ),
    mainPanel(
      uiOutput("page_ui")
    )
  )
)

# Process the historical data
process_historical_data <- function(data) {
  # Calculate means for beetle questions across all contexts
  beetle_encounter <- rowMeans(data[, grep("Biowell_Beetle1_Item", names(data))], na.rm = TRUE)
  beetle_now <- rowMeans(data[, grep("Biowell_Beetle2_Item", names(data))], na.rm = TRUE)
  beetle_future <- rowMeans(data[, grep("Biowell_Beetle3_Item", names(data))], na.rm = TRUE)
  
  # Calculate means for bee questions across all contexts
  bee_encounter <- rowMeans(data[, grep("Biowell_Bee1_Item", names(data))], na.rm = TRUE)
  bee_now <- rowMeans(data[, grep("Biowell_Bee2_Item", names(data))], na.rm = TRUE)
  bee_future <- rowMeans(data[, grep("Biowell_Bee3_Item", names(data))], na.rm = TRUE)
  
  # Calculate means for wasp questions across all contexts
  wasp_encounter <- rowMeans(data[, grep("Biowell_Wasp1_Item", names(data))], na.rm = TRUE)
  wasp_now <- rowMeans(data[, grep("Biowell_Wasp2_Item", names(data))], na.rm = TRUE)
  wasp_future <- rowMeans(data[, grep("Biowell_Wasp3_Item", names(data))], na.rm = TRUE)
  
  # Overall means for each insect
  beetle_overall <- rowMeans(cbind(beetle_encounter, beetle_now, beetle_future), na.rm = TRUE)
  bee_overall <- rowMeans(cbind(bee_encounter, bee_now, bee_future), na.rm = TRUE)
  wasp_overall <- rowMeans(cbind(wasp_encounter, wasp_now, wasp_future), na.rm = TRUE)
  
  # Return as a list for easy access
  list(
    beetle = list(
      encounter = beetle_encounter,
      now = beetle_now,
      future = beetle_future,
      overall = beetle_overall
    ),
    bee = list(
      encounter = bee_encounter,
      now = bee_now,
      future = bee_future,
      overall = bee_overall
    ),
    wasp = list(
      encounter = wasp_encounter,
      now = wasp_now,
      future = wasp_future,
      overall = wasp_overall
    )
  )
}

server <- function(input, output, session) {
  # Initialize reactive values
  rv <- reactiveValues(
    page = 1, 
    user_scores = NULL,
    res = list(),
    class_probabilities = NULL, 
    assigned_class_name = NULL 
  )
  
  # rv <- reactiveValues(
  #   page = 1, 
  #   user_scores = NULL,
  #   res = list()  # Added missing initialization
  # )
  
  # Load historical data
  historical_data <- reactive({
    readRDS("Biowell_PlotData.rds")  # Your 1684 observations dataset
  })
  
  # Process historical data
  historical_stats <- reactive({
    process_historical_data(historical_data())
  })
  
  # Display page progress
  output$page_label <- renderText({
    paste0("Page ", rv$page, " of ", n_pages)
  })
  
  # Function to generate comparison plots
  generate_comparison_plots <- function() {
    # Define text setup for consistent styling
    TextSetup <- element_text(size = 12)
    
    # Create overall comparison
    output$overall_plot <- renderPlot({
      # Create data frame for historical and user data
      
      # Create contexts and prefixes
      contexts <- c("Encounter", "Now", "Future")
      insects <- c("Beetle", "Bee", "Wasp")
      prefixes <- expand.grid(Insect = insects, Context = contexts)
      prefixes$Prefix <- paste0(prefixes$Insect, "_", prefixes$Context)
      
      # Get historical data in long format
      historical_long <- data.frame(
        Prefix = rep(prefixes$Prefix, each = length(historical_stats()$beetle$encounter)),
        Response = c(
          historical_stats()$beetle$encounter, historical_stats()$beetle$now, historical_stats()$beetle$future,
          historical_stats()$bee$encounter, historical_stats()$bee$now, historical_stats()$bee$future,
          historical_stats()$wasp$encounter, historical_stats()$wasp$now, historical_stats()$wasp$future
        )
      )
      
      # Get user scores
      user_scores <- data.frame(
        Prefix = prefixes$Prefix,
        Response = c(
          rv$user_scores$beetle$encounter, rv$user_scores$beetle$now, rv$user_scores$beetle$future,
          rv$user_scores$bee$encounter, rv$user_scores$bee$now, rv$user_scores$bee$future,
          rv$user_scores$wasp$encounter, rv$user_scores$wasp$now, rv$user_scores$wasp$future
        )
      )
      
      # Define custom colors
      custom_colors <- c(
        "#e31a1c", "#fc8d62", "#fb9a99", # Beetle colors (encounter, now, future)
        "#1f78b4", "#7fc97f", "#beaed4", # Bee colors
        "#33a02c", "#fdc086", "#ffff99"  # Wasp colors
      )
      
      prefix_labels <- c(
        "Beetle:\nEncountering",
        "Beetle:\nExist now (existence value)",
        "Beetle:\nExist in the future (bequest value)",
        "Bee:\nEncountering",
        "Bee:\nExist now (existence value)",
        "Bee:\nExist in the future (beequest value)",
        "Wasp:\nEncountering",
        "Wasp:\nExist now (existence value)",
        "Wasp:\nExist in the future (bequest value)"
      )
      
      # Create prefix labels for better readability
      # prefix_labels <- c(
      #   "Beetle - Encounter", "Beetle - Now", "Beetle - Future",
      #   "Bee - Encounter", "Bee - Now", "Bee - Future",
      #   "Wasp - Encounter", "Wasp - Now", "Wasp - Future"
      # )
      
      # Create factor with preferred order
      historical_long$Prefix <- factor(historical_long$Prefix, 
                                       levels = prefixes$Prefix,
                                       labels = prefix_labels)
      user_scores$Prefix <- factor(user_scores$Prefix, 
                                   levels = prefixes$Prefix,
                                   labels = prefix_labels)
      
      # Load ggdist if available, otherwise use standard ggplot
      if (requireNamespace("ggdist", quietly = TRUE)) {
        p <- ggplot(historical_long, aes(
          x = Response,
          y = Prefix,
          group = Prefix,
          fill = Prefix
        )) +
          ggdist::stat_histinterval(outline_bars = TRUE,
                                    slab_colour = "black",
                                    slab_linewidth = 0.45,
                                    shape = 21,  # this point shape has a fill and outline
                                    point_color = "white",
                                    point_fill = "black",
                                    point_size = 5) +
          # Add user data points
          geom_point(data = user_scores, aes(x = Response), 
                     size = 5, color = "blue", fill = "white", shape = 21)
      } else {
        # Fallback to standard ggplot if ggdist not available
        p <- ggplot(historical_long, aes(
          x = Response,
          fill = Prefix
        )) +
          geom_density(alpha = 0.7, color = "black") +
          facet_grid(Prefix ~ ., scales = "free_y") +
          # Add user data points
          geom_point(data = user_scores, aes(x = Response, y = 0), 
                     size = 5, color = "blue", fill = "white", shape = 21)
      }
      
      # Complete the plot with styling
      p +
        # Custom x-axis labels
        scale_x_continuous(
          name = "Wellbeing",
          limits = c(0, 100),
          breaks = seq(0, 100, 10),
          labels = c("0\n(Low)", "10", 
                     "20",
                     "30",
                     "40",
                     "50", 
                     "60",
                     "70",
                     "80", 
                     "90",
                     "100\n(High)")
        ) +
        
        # Custom y-axis labels
        scale_y_discrete(name = "Variable") +
        
        # Adding vertical line at 50
        geom_vline(xintercept = 50, alpha = 0.5) +
        
        theme_bw() +
        
        # Custom fill colors
        scale_fill_manual(
          name = "Wellbeing\nscores", 
          values = custom_colors,
          guide = guide_legend(nrow = 3, ncol = 3, reverse = TRUE)
        ) +
        
        labs(
          title = "Your Insect Wellbeing Scores Compared to Other Respondents",
          subtitle = "Your score is the white circle\nThe median public score is in black"
        ) +
        
        # Style
        theme(
          legend.position = "none",
          legend.background = element_blank(),
          legend.box.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(),
          axis.text = TextSetup,
          axis.title = TextSetup,
          strip.text = TextSetup,
          plot.title = element_text(face = "bold", size = 14),
          plot.subtitle = element_text(size = 12)
        )
    })
    
    # Generate individual distribution plots for each insect type (one for beetles, one for bees, one for wasps)
    insect_types <- c("beetle", "bee", "wasp")
    insect_names <- c("Beetles", "Bees", "Wasps")
    insect_colors <- c("#e31a1c", "#1f78b4", "#33a02c")

    # for (i in seq_along(insect_types)) {
    #   insect <- insect_types[i]
    #   insect_name <- insect_names[i]
    #   fill_color <- insect_colors[i]
    # 
    #   # Create comparison plots showing all contexts together
    #   output[[paste0(insect, "_dist_plot")]] <- renderPlot({
    #     # Get all context data for this insect
    #     encounter_data <- historical_stats()[[insect]]$encounter
    #     now_data <- historical_stats()[[insect]]$now
    #     future_data <- historical_stats()[[insect]]$future
    # 
    #     # Prepare data in long format
    #     context_data <- data.frame(
    #       Response = c(encounter_data, now_data, future_data),
    #       Context = factor(rep(c("Encounter", "Now", "Future"),
    #                            c(length(encounter_data), length(now_data), length(future_data))),
    #                        levels = c("Encounter", "Now", "Future"))
    #     )
    # 
    #     # User's scores for this insect
    #     user_data <- data.frame(
    #       Response = c(
    #         rv$user_scores[[insect]]$encounter,
    #         rv$user_scores[[insect]]$now,
    #         rv$user_scores[[insect]]$future
    #       ),
    #       Context = factor(c("Encounter", "Now", "Future"),
    #                        levels = c("Encounter", "Now", "Future"))
    #     )
    # 
    #     # Context-specific colors (three shades of the insect color)
    #     context_colors <- if(insect == "beetle") {
    #       c("#e31a1c", "#fc8d62", "#fb9a99")  # Red shades
    #     } else if(insect == "bee") {
    #       c("#1f78b4", "#7fc97f", "#beaed4")  # Blue shades
    #     } else {
    #       c("#33a02c", "#fdc086", "#ffff99")  # Green shades
    #     }
    # 
    #     # Create plot with density curves by context
    #     ggplot(context_data, aes(x = Response, fill = Context)) +
    #       geom_density(alpha = 0.7, color = "black") +
    #       # Add points for user scores
    #       geom_point(data = user_data, aes(x = Response, y = 0, color = Context),
    #                  size = 4, shape = 18) +
    #       # Custom styling
    #       scale_x_continuous(
    #         name = "Wellbeing Score",
    #         limits = c(0, 100),
    #         breaks = seq(0, 100, 20),
    #         labels = c("0\n(Low)", "20", "40", "60", "80", "100\n(High)")
    #       ) +
    #       scale_y_continuous(name = "Density") +
    #       scale_fill_manual(values = context_colors) +
    #       scale_color_manual(values = context_colors) +
    #       labs(
    #         title = paste0("Your wellbeing scores for ", insect_name, " across contexts"),
    #         subtitle = "Diamonds show your scores"
    #       ) +
    #       theme_bw() +
    #       theme(
    #         legend.position = "bottom",
    #         legend.title = element_blank(),
    #         panel.grid.major = element_blank(),
    #         panel.grid.minor = element_blank(),
    #         axis.text = TextSetup,
    #         axis.title = TextSetup,
    #         plot.title = element_text(face = "bold", size = 14),
    #         plot.subtitle = element_text(size = 12)
    #       )
    #   })
    # }
  }
  # Render dynamic UI based on current page
  output$page_ui <- renderUI({
    p <- rv$page
    if (p == 1) {
      tagList(
        h3("Demographics"),
        h5("I do not save this data! It is used to estimate your latent class"),
        selectInput("age", "Current age (years):", choices = ages),
        selectInput("income", "Monthly household income before tax:", choices = c(
          "£0-£1,000" = 500,
          "£1,001-£1,500" = 1250,
          "£1,501-£2,000" = 1750,
          "£2,001-£2,500" = 2250,
          "£2,501-£3,000" = 2750,
          "£3,001-£3,500" = 3250,
          "£3,501-£4,000" = 3750,
          "£4,001-£6,000" = 5000,
          "£6,001+" = 6001,
          "Prefer not to say" = NA
        )),
        selectInput("gender", "Gender:", choices = genders),
        selectInput("Country", "Country:", choices = countries),
        radioButtons("citizen_science", "Member of conservation org (past 5 yrs)?", choices = c("No"=0, "Yes"=1), inline = TRUE),
        actionButton("btn_next", "Next")
      )
    } else if (p >= 2 && p <= (1 + length(stems))) {
      idx <- p - 1
      stem_text <- stems[idx]
      
      insect_images <- list(
        "beetles" = "thebeetles_3_small.png",
        "bees" = "thebees_3_small.png", 
        "wasps" = "thewasps_3_small.png"
      )
      
      insect_name <- gsub(".*?(beetles|bees|wasps).*", "\\1", stem_text)
      insect_img <- insect_images[[insect_name]]
      
      sliders <- lapply(seq_along(dims), function(i) {
        tagList(
          div(style = "margin-bottom: 25px;"),
          div(
            span(dims[[i]][1], style = "float:left; font-weight: bold;"),
            span(dims[[i]][2], style = "float:right; font-weight: bold;"),
            div(style = "clear:both; margin-bottom: 5px;")
          ),
          div(style = "text-align: center;", # Center the slider
              div(style = "display: inline-block; width: 90%;",
                  sliderInput(inputId = paste0("q", idx, "_", i), label = NULL,
                              min = 0, max = 100, value = 50, ticks = FALSE,
                              width = "100%")
              )
          ),
          div(style = "text-align: center; margin-top: -15px;",
              span("0", style = "float:left; margin-left: 5%;"),
              span("50", style = "display: inline-block;"),
              span("100", style = "float:right; margin-right: 5%;")
          )
        )
      })
      
      tagList(
        h3(stem_text),
        # Display insect image if available
        if (!is.null(insect_img)) {
          div(style = "text-align: center; margin-bottom: 20px;",
              img(src = insect_img, height = "150px", alt = paste("Image of", insect_img))
          )
        },
        sliders,
        fluidRow(
          column(6, if (p > 2) actionButton("btn_prev", "Previous")),
          column(6, actionButton("btn_next", "Next"), align = "right")
        )
      )
    } else if (p == n_pages) {
      if (is.null(rv$class_probabilities)) { 
        probs <- calculate_class_probabilities(
          age_input = input$age, # This assumes input$age etc. are still available
          # which they should be in the Shiny session.
          gender_input = input$gender,
          income_bracket_input = input$income, 
          country_input = input$Country,
          citizen_science_input = input$citizen_science,
          orig_inc_mean = ORIGINAL_INCOME_MEAN, 
          orig_inc_sd = ORIGINAL_INCOME_SD     
        )
        rv$class_probabilities <- probs
        all_probs <- c(probs$prob_pro_insect, probs$prob_insect_averse, probs$prob_ambivalent)
        class_names <- c("Pro-Insect", "Insect-Averse", "Ambivalent")
        rv$assigned_class_name <- class_names[which.max(all_probs)]
      }
      
      # Define the class descriptions based on your paper's findings
      class_details <- list(
        "Pro-Insect" = list(
          description = "This group generally expresses the most positive wellbeing in relation to insects. They tend to be younger and less likely to be involved in citizen science.",
          # Mean RAW scores from your table (lower is more positive for raw scores)
          bee_score = 51.98, 
          beetle_score = 49.35,
          wasp_score = 44.18,
          overall_score = 48.50 
        ),
        "Insect-Averse" = list(
          description = "This group typically reports less positive wellbeing from insects and is more likely to be female. They also tend to report less confidence that survey choices will be considered.",
          bee_score = 70.04,
          beetle_score = 58.56,
          wasp_score = 47.58,
          overall_score = 58.73
        ),
        "Ambivalent" = list(
          description = "This group shows mixed or moderate wellbeing responses to insects. They tend to be older.",
          bee_score = 71.89,
          beetle_score = 63.98,
          wasp_score = 53.57,
          overall_score = 63.15
        )
      )
      # Check if rv$assigned_class_name is populated before trying to use it
      user_assigned_class_info <- NULL
      if (!is.null(rv$assigned_class_name)) {
        user_assigned_class_info <- class_details[[rv$assigned_class_name]]
      } else {
        # Handle case where class name is not yet assigned (e.g., for debugging)
        # This should ideally not happen if logic in observeEvent is correct.
        return(p("Calculating class information... Please wait or ensure demographic data was entered."))
      }
      
      # Calculate overall user score for comparison text
      # Ensure rv$user_scores is populated before accessing its elements
      user_overall_wellbeing_score <- NA # Default
      if(!is.null(rv$user_scores) && !is.null(rv$user_scores$beetle) && 
         !is.null(rv$user_scores$bee) && !is.null(rv$user_scores$wasp)) {
        user_overall_wellbeing_score <- mean(c(rv$user_scores$beetle$overall, 
                                               rv$user_scores$bee$overall, 
                                               rv$user_scores$wasp$overall), na.rm = TRUE)
        
        # Part 2: Classify wellbeing score
        user_wellbeing_category_name <- if (is.na(user_overall_wellbeing_score)) {
          "Being calculated" # Or some placeholder if score is NA
        } else if (user_overall_wellbeing_score >= 60) {
          "Pro-Insect" # Using your class names for consistency
        } else if (user_overall_wellbeing_score >= 40) {
          "Ambivalent"
        } else {
          "Insect-Averse"
        }
      }
      
      
      tagList(
        h3("Thank you for completing the survey!"),
        
        # --- PART 1: DEMOGRAPHIC CLASS ALIGNMENT ---
        # h4("Your Profile Based on Demographics:"),
        p(HTML(paste0("People in your sociodemographic group (based on age, gender, income, etc.) from our previous research most closely align with the latent class we call: <strong>", 
                      rv$assigned_class_name, "</strong>."))),
        # if (!is.null(rv$class_probabilities)) {
        #   p(HTML(paste0("Your individual probabilities for this demographic group alignment: Pro-Insect: ", round(rv$class_probabilities$prob_pro_insect*100), "%, ",
        #                 "Insect-Averse: ", round(rv$class_probabilities$prob_insect_averse*100), "%, ",
        #                 "Ambivalent: ", round(rv$class_probabilities$prob_ambivalent*100), "%")))
        # },
        
        # --- MODIFIED SECTION FOR GENERAL CLASS PREVALENCE ---
        # --- PART 2: WELLBEING SCORE CLASS ALIGNMENT ---
        # h4("Your Reported Wellbeing Score from Insects:"),
        p(HTML(paste0("Your average overall wellbeing score is ", (round(user_overall_wellbeing_score,1))))),

        p(HTML(paste0("Based on this wellbeing score, your responses align most closely with the latent class we call: <strong>", 
                 user_wellbeing_category_name, "</strong>"))),
        
        hr(),
        
        
        hr(), # Optional: adds a horizontal line for separation
        h5("About These Latent Classes in Our Previous Research:"),
        p("In our original study involving 1,684 respondents, we identified three distinct profiles based on demographics and attitudes:"),
        tags$ul( # Using an unordered list for clarity
          tags$li(HTML("<strong>‘Pro-insect’ (41.84% of the sample):</strong> Willing to pay for more insects. Wellbeing scores typically 60-100.")),
          tags$li(HTML("<strong>‘Insect-averse’ (25.73% of the sample):</strong> Negative willingness to pay for more insects. Wellbeing scores typically 0-39")),
          tags$li(HTML("<strong>‘Ambivalent’ (32.43% of the sample):</strong> Preferences depend; typically love bees but hate wasps. Wellbeing scores typically 40-59"))
        ),
        # if(!is.na(user_overall_wellbeing_score)){ # Keep the display of their own score
        #   p(paste0("Your average overall wellbeing score from the sliders was: ", round(user_overall_wellbeing_score,1), 
        #            " (on a 0-100 scale. We generally found that 0-40 is insect-averse, 40-60 is ambivalent, 60-100 is pro-insect)."))
        # } else {
        #   p("Your average overall wellbeing score is being calculated.")
        # },
        # --- END MODIFIED SECTION ---
        
        hr(),
        p("Here's how your wellbeing from insects compare to our database of 1,684 respondents:"),
        div(style = "margin-top: 20px;",
            plotOutput("overall_plot", height = "600px")
        ),
        
        hr(),
        
        p(HTML(paste0("Thanks for getting involved!\nAny questions please email: p.king1@leeds.ac.uk"))),
        # # Individual distribution plots for each insect
        # div(style = "margin-top: 30px;",
        #     h4("Detailed Comparisons by Insect Type", style = "text-align: center;"),
        #     tabsetPanel(
        #       tabPanel("Beetles", plotOutput("beetle_dist_plot", height = "350px")),
        #       tabPanel("Bees", plotOutput("bee_dist_plot", height = "350px")),
        #       tabPanel("Wasps", plotOutput("wasp_dist_plot", height = "350px"))
        #     )
        # ),
        # 
        # div(style = "margin-top: 30px; text-align: center;",
        #     downloadButton("download_report", "Download Your Results")
        # )
      )
    }
  })
  
  # Handle navigation to next page
  observeEvent(input$btn_next, {
    current_page_before_increment <- rv$page # Capture the page number *before* any increment
    
    # --- Logic to advance to the next page ---
    if (current_page_before_increment < n_pages) {
      rv$page <- rv$page + 1 # Increment page number
    }
    
    # --- Calculate and store class probabilities AFTER demographic page (Page 1) ---
    # This block executes when the user clicks "Next" on Page 1.
    # At this point, `current_page_before_increment` is 1.
    # The demographic inputs (input$age, input$gender, etc.) are from Page 1.
    if (current_page_before_increment == 1) { 
      class_probs_results <- calculate_class_probabilities(
        age_input = input$age, 
        gender_input = input$gender,
        income_bracket_input = input$income, 
        country_input = input$Country,
        citizen_science_input = input$citizen_science,
        orig_inc_mean = ORIGINAL_INCOME_MEAN, 
        orig_inc_sd = ORIGINAL_INCOME_SD      
      )
      rv$class_probabilities <- class_probs_results # Store the list of probabilities
      
      # Determine and store the assigned class name
      all_individual_probs <- c(class_probs_results$prob_pro_insect, 
                                class_probs_results$prob_insect_averse, 
                                class_probs_results$prob_ambivalent)
      defined_class_names <- c("Pro-Insect", "Insect-Averse", "Ambivalent")
      rv$assigned_class_name <- defined_class_names[which.max(all_individual_probs)]
    }
    
    # --- Logic for when user reaches the FINAL page (n_pages) ---
    # This block executes when `rv$page` (after potential increment) equals `n_pages`.
    # This means the user has just clicked "Next" on the last page of sliders.
    if (rv$page == n_pages) {
      # Calculate user's mean scores for each insect × context from the sliders
      # q1_*: stem 1 (beetle encounter)
      # q2_*: stem 2 (bee encounter)
      # q3_*: stem 3 (wasp encounter)
      # q4_*: stem 4 (beetle now)
      # q5_*: stem 5 (bee now)
      # q6_*: stem 6 (wasp now)
      # q7_*: stem 7 (beetle future)
      # q8_*: stem 8 (bee future)
      # q9_*: stem 9 (wasp future)
      
      
      # Step 1: Collect raw inputs into a vector
      raw_q1_vals <- c(input$q1_1, input$q1_2, input$q1_3, input$q1_4, input$q1_5)
      raw_q2_vals <- c(input$q2_1, input$q2_2, input$q2_3, input$q2_4, input$q2_5)
      raw_q3_vals <- c(input$q3_1, input$q3_2, input$q3_3, input$q3_4, input$q3_5)
      raw_q4_vals <- c(input$q4_1, input$q4_2, input$q4_3, input$q4_4, input$q4_5)
      raw_q5_vals <- c(input$q5_1, input$q5_2, input$q5_3, input$q5_4, input$q5_5)
      raw_q6_vals <- c(input$q6_1, input$q6_2, input$q6_3, input$q6_4, input$q6_5)
      raw_q7_vals <- c(input$q7_1, input$q7_2, input$q7_3, input$q7_4, input$q7_5)
      raw_q8_vals <- c(input$q8_1, input$q8_2, input$q8_3, input$q8_4, input$q8_5)
      raw_q9_vals <- c(input$q9_1, input$q9_2, input$q9_3, input$q9_4, input$q9_5)
      
      
      # Step 2: Invert each raw score
      inv_q1_vals <- 100 - raw_q1_vals
      
      inv_q1_vals <- 100 - raw_q1_vals
      inv_q2_vals <- 100 - raw_q2_vals
      inv_q3_vals <- 100 - raw_q3_vals
      inv_q4_vals <- 100 - raw_q4_vals
      inv_q5_vals <- 100 - raw_q5_vals
      inv_q6_vals <- 100 - raw_q6_vals
      inv_q7_vals <- 100 - raw_q7_vals
      inv_q8_vals <- 100 - raw_q8_vals
      inv_q9_vals <- 100 - raw_q9_vals
      
      
      # Step 3: Calculate the mean of the inverted scores
      # user_scores_beetle_encounter <- mean(inv_q1_vals, na.rm = TRUE)
      
user_scores_beetle_encounter <- mean(inv_q1_vals, na.rm = TRUE)
user_scores_bee_encounter    <- mean(inv_q2_vals, na.rm = TRUE)
user_scores_wasp_encounter   <- mean(inv_q3_vals, na.rm = TRUE)
user_scores_beetle_now       <- mean(inv_q4_vals, na.rm = TRUE)
user_scores_bee_now          <- mean(inv_q5_vals, na.rm = TRUE)
user_scores_wasp_now         <- mean(inv_q6_vals, na.rm = TRUE)
user_scores_beetle_future    <- mean(inv_q7_vals, na.rm = TRUE)
user_scores_bee_future       <- mean(inv_q8_vals, na.rm = TRUE)
user_scores_wasp_future      <- mean(inv_q9_vals, na.rm = TRUE)

# Calculate overall mean scores for each insect type
user_scores_beetle_overall <- mean(c(user_scores_beetle_encounter, user_scores_beetle_now, user_scores_beetle_future), na.rm = TRUE)
user_scores_bee_overall    <- mean(c(user_scores_bee_encounter, user_scores_bee_now, user_scores_bee_future), na.rm = TRUE)
user_scores_wasp_overall   <- mean(c(user_scores_wasp_encounter, user_scores_wasp_now, user_scores_wasp_future), na.rm = TRUE)

      # Store these calculated scores in rv$user_scores for the plotting function
      rv$user_scores <- list(
        beetle = list(
          encounter = user_scores_beetle_encounter,
          now       = user_scores_beetle_now,
          future    = user_scores_beetle_future,
          overall   = user_scores_beetle_overall
        ),
        bee = list(
          encounter = user_scores_bee_encounter,
          now       = user_scores_bee_now,
          future    = user_scores_bee_future,
          overall   = user_scores_bee_overall
        ),
        wasp = list(
          encounter = user_scores_wasp_encounter,
          now       = user_scores_wasp_now,
          future    = user_scores_wasp_future,
          overall   = user_scores_wasp_overall
        )
      )
      
      # Generate comparison plots (this function will use rv$user_scores and historical_stats)
      generate_comparison_plots()
      
      # Store detailed results per stem for potential download/analysis
      # rv$res will store the mean of the 5 dimension sliders for each of the 9 stems
      for (idx in seq_along(stems)) { # idx goes from 1 to 9
        slider_values_for_stem <- sapply(seq_along(dims), function(dim_idx) { # dim_idx goes from 1 to 5
          input[[paste0("q", idx, "_", dim_idx)]] 
        })
        rv$res[[paste0("stem_q", idx, "_mean")]] <- mean(slider_values_for_stem, na.rm = TRUE)
      }
      
      # Create a data frame for CSV download including demographics, class, and all scores
      # (This can be expanded as needed)
      demographics_data <- data.frame(
        age = input$age,
        gender = input$gender,
        income = input$income, # Store the bracket value
        country = input$Country,
        citizen_science = input$citizen_science,
        assigned_class = rv$assigned_class_name,
        prob_pro_insect = rv$class_probabilities$prob_pro_insect,
        prob_insect_averse = rv$class_probabilities$prob_insect_averse,
        prob_ambivalent = rv$class_probabilities$prob_ambivalent,
        stringsAsFactors = FALSE
      )
      
      # Combine with stem means and overall insect scores
      user_scores_flat <- data.frame(
        beetle_encounter_score = rv$user_scores$beetle$encounter,
        beetle_now_score = rv$user_scores$beetle$now,
        beetle_future_score = rv$user_scores$beetle$future,
        beetle_overall_score = rv$user_scores$beetle$overall,
        bee_encounter_score = rv$user_scores$bee$encounter,
        bee_now_score = rv$user_scores$bee$now,
        bee_future_score = rv$user_scores$bee$future,
        bee_overall_score = rv$user_scores$bee$overall,
        wasp_encounter_score = rv$user_scores$wasp$encounter,
        wasp_now_score = rv$user_scores$wasp$now,
        wasp_future_score = rv$user_scores$wasp$future,
        wasp_overall_score = rv$user_scores$wasp$overall
      )
      
      results_df_for_download <- cbind(demographics_data, 
                                       as.data.frame(as.list(rv$res)), # Converts named list of stem means to df columns
                                       user_scores_flat) 
      
      # Define file name for download
      temp_csv_path <- file.path(tempdir(), paste0("survey_results_", gsub(":", "-", Sys.time()), ".csv"))
      write.csv(results_df_for_download, temp_csv_path, row.names = FALSE)
      
      # Enable download handler (content now refers to results_df_for_download)
      output$download_report <- downloadHandler(
        filename = function() {
          paste0("Insect_Wellbeing_Survey_Results_", Sys.Date(), ".csv")
        },
        content = function(file) {
          write.csv(results_df_for_download, file, row.names = FALSE)
        }
      )
    }
  })
  # Handle navigation to previous page
  observeEvent(input$btn_prev, {
    if (rv$page > 1) rv$page <- rv$page - 1
  })
}

shinyApp(ui, server)