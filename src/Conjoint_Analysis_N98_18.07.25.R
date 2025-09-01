# Install packages
install.packages("tidyverse")  # for data cleaning and visualization 
install.packages("readr")      # for reading files 
install.packages("janitor")    # for cleaning column names 
install.packages("summarytools") # for easy descriptive stats 
install.packages("ggplot2") # for easy bar chart
install.packages('ggstance')
install.packages("cjoint")
install.packages("scales")


# Load installed packages
library(tidyverse) 
library(readr) 
library(janitor) 
library(summarytools) 
library(marginaleffects)
library(broom.helpers)
library(ggforce)  
library(scales) 
library(haven)
library(broom) 
library(cregg) 
library(survey) 
library(brms) 
library(tidybayes)       
library(ggdist)
library(patchwork)  
library(ggplot2)
library(dplyr)  
library(ggstance)  
library(cjoint)
library(patchwork)
library(dplyr)
library(tidyr)
library(ggplot2)
library(forcats)
library(scales)
library(ggforce)
library(purrr)
library(showtext)


# 1. Add regular and bold Calibri fonts
font_add(family = "Calibri", 
         regular = "/Users/anggitadewayani/Library/Fonts/calibri-regular.ttf", 
         bold = "/Users/anggitadewayani/Library/Fonts/calibri-bold.ttf")

# 2. Activate showtext
showtext_auto(enable = TRUE)

# Open Data using pathname
data <- read_delim("/Users/anggitadewayani/Documents/ANG/MIM SS25/THESIS 2025/Data/Qualtrics Result/July_18_total_98_AGE_UTF8.csv", delim = ";") %>% clean_names() 

# Load installed packages
library(tidyverse)
library(ggplot2)
library(summarytools)

# Silent delim notifications
read_delim("/Users/anggitadewayani/Documents/ANG/MIM SS25/THESIS 2025/Data/Qualtrics Result/July_18_total_98_AGE_UTF8.csv", delim = "\t", show_col_types = FALSE)

# Show all true column names
colnames(data)

# separate column with semicolon
data <- read_delim("/Users/anggitadewayani/Documents/ANG/MIM SS25/THESIS 2025/Data/Qualtrics Result/July_18_total_98_AGE_UTF8.csv", delim = ";") %>% clean_names()

# Show contents of Column
library(summarytools) 
freq(data$q3)  

  # ----------  01 CONJOINT AMCE ----------

library(cjoint)
library(tidyverse)
library(ggstance)
library(cregg)

df_profiles_raw <- read_delim("/Users/anggitadewayani/Documents/ANG/MIM SS25/THESIS 2025/Data/Qualtrics Result/July_18_total_98_AGE_UTF8.csv")

glimpse(df_profiles_raw)

df_attributes_raw <- df_profiles_raw %>%
       select(ResponseId, starts_with("F-"))
 
   # Step 2b: Pivot to long format — one row per respondent-task-profile-attribute
   df_attributes_long <- df_attributes_raw %>%
       pivot_longer(
             cols = starts_with("F-"),
             names_to = "attr_code",   # This will contain values like "F-1-1-1"
             values_to = "attr_text"   # This contains the descriptive level (e.g. "Childcare available...")
         )

   df_attributes_long <- df_attributes_long %>%
          mutate(
                task = str_extract(attr_code, "(?<=F-)[0-6]"),
                profile = str_extract(attr_code, "(?<=F-[0-6]-)[0-2]"),
                attribute_position = str_extract(attr_code, "(?<=F-[0-6]-[0-2]-)[0-6]"),
                attribute_position = as.integer(attribute_position),
                task = as.integer(task),
                profile = as.integer(profile)
            )
   
   df_profiles_long <- df_attributes_long %>%
          mutate(attribute_name = case_when(
                attribute_position == 1 ~ "ChildcareSupport",
                attribute_position == 2 ~ "WorkingHours",
                attribute_position == 3 ~ "WorkplaceFlexibility",
                attribute_position == 4 ~ "CareerGrowth",
                attribute_position == 5 ~ "GenderEquality",
                attribute_position == 6 ~ "FinancialBenefits"
            )) %>%
          select(ResponseId, task, profile, attribute_name, attr_text) %>%
          pivot_wider(
                names_from = attribute_name,
               values_from = attr_text
            ) 

   colSums(is.na(df_profiles_long))
   
   #3
   
   choice_cols <- c("Q39", "Q41", "Q43", "Q45", "Q47", "Q49")
   
   df_choices_long <- df_profiles_raw %>%
     select(ResponseId, all_of(choice_cols)) %>%
     pivot_longer(
       cols = -ResponseId,
       names_to = "task_code",
       values_to = "chosen_job"
     ) %>%
     mutate(
       task = case_when(
         task_code == "Q39" ~ 1,
         task_code == "Q41" ~ 2,
         task_code == "Q43" ~ 3,
         task_code == "Q45" ~ 4,
         task_code == "Q47" ~ 5,
         task_code == "Q49" ~ 6
       ),
       chosen_profile = if_else(chosen_job == "Job A", 1,
                                if_else(chosen_job == "Job B", 2, NA_real_))
     ) %>%
     select(ResponseId, task, chosen_profile)
   
   
   #3B
   
   df_profiles_ready <- df_profiles_long %>%
     left_join(df_choices_long, by = c("ResponseId", "task")) %>%
     mutate(
       chosen = if_else(profile == chosen_profile, 1, 0)
     )
   
   table(df_profiles_ready$chosen, useNA = "ifany")
   
   df_profiles_final <- df_profiles_ready %>%
     drop_na(ChildcareSupport, WorkingHours, WorkplaceFlexibility,
             CareerGrowth, GenderEquality, FinancialBenefits, chosen)
   
#4 Clean before AMCE
   df_amce <- df_profiles_ready %>%
     drop_na(ChildcareSupport, WorkingHours, WorkplaceFlexibility,
             CareerGrowth, GenderEquality, FinancialBenefits, chosen)
   
   nrow(df_amce)  
   
   
#4b Convert all attributes to factors before running cj()
   
   df_amce <- df_amce %>%
     mutate(across(c(
       ChildcareSupport, WorkingHours, WorkplaceFlexibility,
       CareerGrowth, GenderEquality, FinancialBenefits
     ), as.factor))
   
#5  Run the AMCE using cregg::cj()
   library(cregg)
   
   results_amce <- cj(
     data = df_amce,
     formula = chosen ~ ChildcareSupport + WorkingHours + WorkplaceFlexibility +
       CareerGrowth + GenderEquality + FinancialBenefits,
     id = ~ResponseId
   )
   
   summary(amce_results)
   print(amce_results)
      str(amce_summary)
   
   
#reorder and rename correct levels
   
   library(forcats)
   
   df_amce <- df_amce %>%
     mutate(
       ChildcareSupport = fct_recode(
         ChildcareSupport,
         "03 High Support" = "Childcare available; equal partner support",
         "02 Moderate Support" = "Limited childcare; partial partner support",
         "01 Low Support" = "Very difficult childcare access; no support"
       ),
       WorkingHours = fct_recode(
         WorkingHours,
         "03 Balanced" = "20-30 h/week",
         "02 Demanding" = "30-35 h/week",
         "01 Intensive" = "40+ h/week"
       ),
       WorkplaceFlexibility = fct_recode(
         WorkplaceFlexibility,
         "01 Low Flexibility" = "Fixed hours; on-site work",
         "02 Moderate Flexibility" = "Fixed hours; limited remote days",
         "03 High Flexibility" = "Flexible hours; remote work allowed"
       ),
       CareerGrowth = fct_recode(
         CareerGrowth,
         "03 High Opportunities" = "Clear promotion path",
         "02 Moderate Opportunities" = "Limited promotion path",
         "01 Low Opportunities" = "No promotion path"
       ),
       GenderEquality = fct_recode(
         GenderEquality,
         "02 Neutral" = "Few women in leadership roles",
         "03 Supportive" = "Gender-equal leadership",
         "01 Unsupportive" = "Male-dominated leadership"
       ),
       FinancialBenefits = fct_recode(
         FinancialBenefits,
         "02 Moderate" = "Average salary & basic benefits",
         "01 Low" = "Below average salary without bonuses",
         "03 High" = "High salary, bonuses & benefits"
       )
     )
   
   df_amce <- df_amce %>%
     mutate(
       ChildcareSupport = fct_relevel(ChildcareSupport, "01 Low Support", "02 Moderate Support", "03 High Support"),
       WorkingHours = fct_relevel(WorkingHours, "01 Intensive", "02 Demanding", "03 Balanced"),
       WorkplaceFlexibility = fct_relevel(WorkplaceFlexibility, "01 Low Flexibility", "02 Moderate Flexibility", "03 High Flexibility"),
       CareerGrowth = fct_relevel(CareerGrowth, "01 Low Opportunities", "02 Moderate Opportunities", "03 High Opportunities"),
       GenderEquality = fct_relevel(GenderEquality, "01 Unsupportive", "02 Neutral", "03 Supportive"),
       FinancialBenefits = fct_relevel(FinancialBenefits, "01 Low", "02 Moderate", "03 High")
     )
   
#AMCE re-calculate using cregg::cj()

      library(cregg)
   
   results_amce <- cj(
     data = df_amce,
     formula = chosen ~ ChildcareSupport + WorkingHours + WorkplaceFlexibility +
       CareerGrowth + GenderEquality + FinancialBenefits,
     id = ~ResponseId
   )
   
   summary(amce_results)
   print(amce_results)
   
#PLOT AGAIN
   plot(results_amce) +
     labs(
       title = "AMCE Estimates with 95% Confidence Intervals
        .
        Reference levels shown with zero effects\nSubgroup N ≈ 98 × 6 × 2 = 1,176",
       y = "Attribute Levels",
       x = "Average Marginal Component Effect (AMCE)
        .
        Percentage point change in probability
        .
        "
     )
   


# ----------   P VALUE and Confidence Interval  ----------
 
   amce_summary <- summary(results_amce)
   print(amce_summary, digits = 3)
   
   
   # ----------   Prettier GGPlot with calibri and grey bg  ----------
   library(dplyr)
   library(ggplot2)
   library(ggforce)
   library(scales)
   library(forcats)
   
   plot_data <- results_amce %>%
     as_tibble() %>%
     mutate(
       variable_nice = recode(feature,
                              "ChildcareSupport" = "Childcare & Family Support",
                              "WorkingHours" = "Working Hours",
                              "WorkplaceFlexibility" = "Workplace Flexibility",
                              "CareerGrowth" = "Career Growth",
                              "GenderEquality" = "Workplace Gender Equality",
                              "FinancialBenefits" = "Financial Benefits"
       ),
       level = fct_rev(level),
       
       # Identify baseline rows: usually estimate == 0 and NA for lower/upper
       is_baseline = is.na(lower) | is.na(upper)
     ) %>%
     # For baseline rows, replace NA lower and upper with estimate (0) for plotting points
     mutate(
       lower = ifelse(is_baseline, estimate, lower),
       upper = ifelse(is_baseline, estimate, upper)
     )
   
   
   library(forcats)
   
   plot_data <- results_amce %>%
     as_tibble() %>%
     mutate(
       variable_nice = recode(feature,
                              "ChildcareSupport" = "Childcare & Family Support",
                              "WorkingHours" = "Working Hours",
                              "WorkplaceFlexibility" = "Workplace Flexibility",
                              "CareerGrowth" = "Career Growth",
                              "GenderEquality" = "Workplace Gender Equality",
                              "FinancialBenefits" = "Financial Benefits"
       ),
       # Alphabetically order the levels **within each attribute**
       level = factor(level),  # ensure factor
       level = fct_reorder(level, level, .fun = function(x) x),  # reorder alphabetically
       is_baseline = is.na(lower) | is.na(upper)
     ) %>%
     mutate(
       lower = ifelse(is_baseline, estimate, lower),
       upper = ifelse(is_baseline, estimate, upper)
     )
   
   

# 6. Final plot
   # Plot with separate geoms for baseline and others:
   ggplot(plot_data, aes(x = estimate, y = level, color = variable_nice)) +
     
     geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
     
     # Points for baseline (zero-length error bars)
     geom_point(
       data = filter(plot_data, is_baseline),
       size = 2,
       shape = 16
     ) +
     
     # Pointrange for non-baseline rows
     geom_pointrange(
       data = filter(plot_data, !is_baseline),
       aes(xmin = lower, xmax = upper),
       fatten = 2,
       size = 0.5
     ) +
     
     scale_x_continuous(
       labels = label_number(accuracy = 0.01, scale = 1, suffix = " pp")
     ) +
     
     scale_y_discrete(expand = expansion(mult = c(0.3, 0.3))) +
     
     labs(
       x = ".
       Percentage point change in probability
       .",
       y = NULL,
       title = "Average Marginal Component Effects (AMCE) with 95% Confidence Intervals",
       subtitle = "Reference levels shown with zero effects\nSubgroup N ≈ 98 × 6 × 2 = 1,176       "
     ) +
     
     ggforce::facet_col(facets = "variable_nice", scales = "free_y", space = "free") +
     
     guides(color = "none") +
     
     theme_minimal(base_size = 8, base_family = "calibri") +
     theme(
       strip.text = element_text(face = "bold", size = 10, hjust = 0),
       strip.background = element_rect(fill = "lightgrey", color = NA),
       axis.text.y = element_text(size = 7)
     )
   
# 7 Print to TXT CSV
   
   library(dplyr)
   library(readr)
   
   amce_full <- results_amce %>%
     as_tibble() %>%
     mutate(
       variable_nice = recode(feature,
                              "ChildcareSupport" = "Childcare & Family Support",
                              "WorkingHours" = "Working Hours",
                              "WorkplaceFlexibility" = "Workplace Flexibility",
                              "CareerGrowth" = "Career Growth",
                              "GenderEquality" = "Workplace Gender Equality",
                              "FinancialBenefits" = "Financial Benefits"
       )
     ) %>%
     rename(
       Attribute = feature,
       Level = level,
       Estimate = estimate,
       StdError = std.error,
       Zvalue = z,
       Pvalue = p,
       LowerCI = lower,
       UpperCI = upper
     ) %>%
     select(variable_nice, Attribute, Level, Estimate, StdError, Zvalue, Pvalue, LowerCI, UpperCI)
   
   
  
   write_delim(amce_full, "/Users/anggitadewayani/Documents/ANG/MIM SS25/THESIS 2025/Data/RStudio /AMCE Plot/18.07.25/AMCE table 18.07.25.csv", delim = "\t")
   library(readr)
   library(dplyr)
   
   amce_full_3dec <- amce_full %>%
     mutate(across(where(is.numeric), ~ round(.x, 3)))
   
      write_delim(
     amce_full_3dec,
     file = file.path(dir_path, "Print_AMCE_table_primary_conjoint_18_07_25.txt"),
     delim = "\t"
   )
  
      print(amce_full, n = Inf)  # prints all rows in console
   
# ---------- 02 SUBGROUP ANALYSIS ----------

       # ---------- Plot Subgroup comparison 1: Mothers vs Non-Mothers ----------
      
      library(dplyr)
      library(ggplot2)
      library(ggforce)
      library(forcats)
      library(scales)
      
      # Add group label to both datasets
      plot_data_parent <- plot_data_parent %>% mutate(Group = "Mothers")
      plot_data_nonparent <- plot_data_nonparent %>% mutate(Group = "Non-Mothers")
      
      # Combine the two datasets
      combined_plot_data <- bind_rows(plot_data_parent, plot_data_nonparent) %>%
        mutate(
          variable_nice = factor(variable_nice,
                                 levels = c(
                                   "Childcare & Family Support",
                                   "Working Hours",
                                   "Workplace Flexibility",
                                   "Career Growth",
                                   "Workplace Gender Equality",
                                   "Financial Benefits"
                                 )
          ),
          Group = factor(Group, levels = c("Mothers", "Non-Mothers")),
          
          # Reverse factor levels for each attribute
          level = case_when(
            variable_nice == "Childcare & Family Support" ~ factor(level, levels = rev(c("03 High Support", "02 Moderate Support", "01 Low Support"))),
            variable_nice == "Working Hours" ~ factor(level, levels = rev(c("03 Balanced", "02 Demanding", "01 Intensive"))),
            variable_nice == "Workplace Flexibility" ~ factor(level, levels = rev(c("03 High Flexibility", "02 Moderate Flexibility", "01 Low Flexibility"))),
            variable_nice == "Career Growth" ~ factor(level, levels = rev(c("03 High Opportunities", "02 Moderate Opportunities", "01 Low Opportunities"))),
            variable_nice == "Workplace Gender Equality" ~ factor(level, levels = rev(c("03 Supportive", "02 Neutral", "01 Unsupportive"))),
            variable_nice == "Financial Benefits" ~ factor(level, levels = rev(c("03 High", "02 Moderate", "01 Low"))),
            TRUE ~ factor(level)
          )
        ) %>%
        group_by(variable_nice) %>%
        mutate(
          y_numeric = as.numeric(level),
          y_offset = ifelse(Group == "Mothers", 0.15, -0.15),
          y_pos = y_numeric + y_offset
        ) %>%
        ungroup()
      
      # Define colors for groups
      colors_groups <- c(
        "Mothers" = "#fc9fb0",
        "Non-Mothers" = "#f5e08e"
      )
      
      # Plot
      ggplot(combined_plot_data, aes(x = estimate, y = y_pos, color = Group)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        
        geom_point(
          data = filter(combined_plot_data, is_baseline),
          size = 1.5,
          shape = 16
        ) +
        
        geom_errorbarh(
          data = filter(combined_plot_data, !is_baseline),
          aes(xmin = lower, xmax = upper),
          height = 0,
          size = 0.6
        ) +
        
        geom_point(
          data = filter(combined_plot_data, !is_baseline),
          size = 1.5,
          shape = 16
        ) +
        
        scale_x_continuous(
          limits = c(-0.2, 0.6),
          labels = label_number(accuracy = 0.01, scale = 1, suffix = " pp")
        ) +
        
        scale_y_continuous(
          breaks = unique(combined_plot_data$y_numeric),
          labels = levels(droplevels(combined_plot_data$level)),
          expand = expansion(mult = c(0.3, 0.3))
        ) +
        
        scale_color_manual(values = colors_groups, name = "Group") +
        
        labs(
          x = ".\nPercentage point change in probability\n.",
          y = NULL,
          title = "AMCE Estimates – Parental Status Subgroups",
          subtitle = "Reference levels shown with zero effects\nMothers (N ≈ 53 x 6 x 2 = 636), Non-Mothers (N ≈ 45 x 6 x 2 = 540)"
        ) +
        
        ggforce::facet_col(facets = "variable_nice", scales = "free_y", space = "free") +
        
        theme_minimal(base_size = 8, base_family = "calibri") +
        theme(
          strip.text = element_text(face = "bold", size = 10, hjust = 0),
          strip.background = element_rect(fill = "lightgrey", color = NA),
          axis.text.y = element_text(size = 7),
          legend.position = "right"
        )
      
      # ---------- Plot Subgroup comparison 2: Working vs NonWorking Women ----------
      
      library(dplyr)
      library(ggplot2)
      library(ggforce)
      library(forcats)
      library(scales)
      
      # Add group label to both datasets
      plot_data_employed <- plot_data_employed %>% mutate(Group = "Working Women")
      plot_data_unemployed <- plot_data_unemployed %>% mutate(Group = "Non-Working Women")
      
      # Combine the two datasets
      combined_plot_data <- bind_rows(plot_data_employed, plot_data_unemployed) %>%
        mutate(
          variable_nice = factor(variable_nice,
                                 levels = c(
                                   "Childcare & Family Support",
                                   "Working Hours",
                                   "Workplace Flexibility",
                                   "Career Growth",
                                   "Workplace Gender Equality",
                                   "Financial Benefits"
                                 )
          ),
          Group = factor(Group, levels = c("Working Women", "Non-Working Women")),
          
          # Reverse factor levels for each attribute
          level = case_when(
            variable_nice == "Childcare & Family Support" ~ factor(level, levels = rev(c("03 High Support", "02 Moderate Support", "01 Low Support"))),
            variable_nice == "Working Hours" ~ factor(level, levels = rev(c("03 Balanced", "02 Demanding", "01 Intensive"))),
            variable_nice == "Workplace Flexibility" ~ factor(level, levels = rev(c("03 High Flexibility", "02 Moderate Flexibility", "01 Low Flexibility"))),
            variable_nice == "Career Growth" ~ factor(level, levels = rev(c("03 High Opportunities", "02 Moderate Opportunities", "01 Low Opportunities"))),
            variable_nice == "Workplace Gender Equality" ~ factor(level, levels = rev(c("03 Supportive", "02 Neutral", "01 Unsupportive"))),
            variable_nice == "Financial Benefits" ~ factor(level, levels = rev(c("03 High", "02 Moderate", "01 Low"))),
            TRUE ~ factor(level)
          )
        ) %>%
        group_by(variable_nice) %>%
        mutate(
          y_numeric = as.numeric(level),
          y_offset = ifelse(Group == "Working Women", 0.15, -0.15),
          y_pos = y_numeric + y_offset
        ) %>%
        ungroup()
      
      # Define colors for groups
      colors_groups <- c(
        "Working Women" = "#3ce0e6",
        "Non-Working Women" = "#1b82d1"
      )
      
      # Plot
      ggplot(combined_plot_data, aes(x = estimate, y = y_pos, color = Group)) +
        geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
        
        geom_point(
          data = filter(combined_plot_data, is_baseline),
          size = 1.5,
          shape = 16
        ) +
        
        geom_errorbarh(
          data = filter(combined_plot_data, !is_baseline),
          aes(xmin = lower, xmax = upper),
          height = 0,
          size = 0.6
        ) +
        
        geom_point(
          data = filter(combined_plot_data, !is_baseline),
          size = 1.5,
          shape = 16
        ) +
        
        scale_x_continuous(
          limits = c(-0.2, 0.6),
          labels = label_number(accuracy = 0.01, scale = 1, suffix = " pp")
        ) +
        
        scale_y_continuous(
          breaks = unique(combined_plot_data$y_numeric),
          labels = levels(droplevels(combined_plot_data$level)),
          expand = expansion(mult = c(0.3, 0.3))
        ) +
        
        scale_color_manual(values = colors_groups, name = "Group") +
        
        labs(
          x = ".\nPercentage point change in probability\n.",
          y = NULL,
          title = "AMCE Estimates – Working Status Subgroups",
          subtitle = "Reference levels shown with zero effects\nWorking Women (N ≈ 63 x 6 x 2 = 756), Non-Working Women (N ≈ 35 x 6 x 2 = 420)"
        ) +
        
        ggforce::facet_col(facets = "variable_nice", scales = "free_y", space = "free") +
        
        theme_minimal(base_size = 8, base_family = "calibri") +
        theme(
          strip.text = element_text(face = "bold", size = 10, hjust = 0),
          strip.background = element_rect(fill = "lightgrey", color = NA),
          axis.text.y = element_text(size = 7),
          legend.position = "right"
        )
      
      # ---------- 03 LIKERT SCALE THEMATIC GROUPING ----------
      # Load required libraries
      library(dplyr)
      library(tidyr)
      library(ggplot2)
      library(forcats)
      library(scales)
      library(ggforce)
      library(purrr)
      
      # ----------------------------  Define theme labels (renamed) ----------------------------
      theme_labels <- c(
        "Theme 1" = "Current Workplace Experience",
        "Theme 2" = "Current Financial Benefits",
        "Theme 3" = "Priorities in Making Career Decision",
        "Theme 4" = "Cultural and Family Expectations",
        "Theme 5" = "Childcare Challenges in Germany",
        "Theme 6" = "Overall German Workplace Experiences",
        "Theme 7" = "Balancing Professional, Social & Cultural Challenges"
      )
      
      # ---------------------------- Define updated question labels per theme ----------------------------
      theme_questions <- list(
        "Theme 1" = c(
          "Good Career Growth Opportunities",
          "Gender Equality Actively Supported",
          "Has Women in Leadership",
          "Good Work-Life Balance"
        ),
        "Theme 2" = c(
          "Current Salary Fits Experience",
          "Current Financial Benefits Gives Sense of Stability",
          "Satisfied with Current Financial Benefits"
        ),
        "Theme 3" = c(
          "Work-Life Balance",
          "Working Flexibility",
          "Financial Security",
          "Career Growth",
          "Workplace Supports Women",
          "Job & Personal Value Alignment",
          "Family Friendly Policies",
          "Partner/Family Support"
        ),
        "Theme 4" = c(
          "Expected to Prioritize Family Over Career",
          "Pressure to Maintain Traditional Gender Roles",
          "Conflict Between Career Ambitions & Cultural Expectations",
          "Germany Influenced Me to Support Women's Careers"
        ),
        "Theme 5" = c(
          "Difficulty in Finding Childcare",
          "Childcare Cost Impacts Career Decision"
        ),
        "Theme 6" = c(
          "Experienced Gender Bias",
          "Germany is More Supportive for Working Mothers than Indonesia",
          "Need to Work Harder Than Germans",
          "Indonesian Background is Valued & Respected"
        ),
        "Theme 7" = c(
          "Balancing Professional & Family Responsibilities",
          "Meeting Social Expectations",
          "Adapting to German Workplace",
          "Building Professional Networks",
          "Cross-Cultural Communication",
          "Staying Connected to Indonesian Identity"
        )
      )
      
      # -------- Theme summaries list (must exist beforehand) --------
      theme_summaries <- list(
        theme1_summary, theme2_summary, theme3_summary,
        theme4_summary, theme5_summary, theme6_summary, theme7_summary
      )
      
      # -------- Create tidy summary for each theme ---------
      
      tidy_theme <- function(summary_df, theme_name, questions) {
        long_df <- summary_df %>%
          pivot_longer(everything(), names_to = "variable", values_to = "percent_agree") %>%
          mutate(
            question_text = factor(questions, levels = questions),
            theme = theme_name,
            survey_order = seq_along(questions)
          ) %>%
          select(theme, question_text, percent_agree, survey_order)
        return(long_df)
      }
      
      # Apply to all themes
      combined_df <- map2_df(theme_summaries, names(theme_questions), ~ tidy_theme(.x, .y, theme_questions[[.y]]))
      
      # -------- Define groupings and new theme order --------
      
      theme_order <- c("Theme 3", "Theme 4", "Theme 5", "Theme 1", "Theme 2", "Theme 6", "Theme 7")
      
      theme_groups <- list(
        "All participants (N=98):" = c("Theme 3", "Theme 4"),
        "Mothers (N=53):" = "Theme 5",
        "Working women (N=57):" = c("Theme 1", "Theme 2", "Theme 6", "Theme 7")
      )
      
      # Create a lookup table for group assignment
      theme_to_group <- unlist(lapply(names(theme_groups), function(group) {
        setNames(rep(group, length(theme_groups[[group]])), theme_groups[[group]])
      }))
      
      # Add group column
      combined_df <- combined_df %>%
        mutate(
          theme = factor(theme, levels = theme_order),
          group = theme_to_group[as.character(theme)]
        )
      
      # ---------------------------- Recalculate means per theme ----------------------------
      theme_means <- combined_df %>%
        group_by(theme, group) %>%
        summarise(mean_agree = mean(percent_agree, na.rm = TRUE), .groups = "drop")
      
      # ----------------------------  Define theme fill colors ----------------------------
      theme_colors <- c(
        "Theme 1" = "#a9d3eb", 
        "Theme 2" = "#b2d1ce",
        "Theme 3" = "#dfd1e8",
        "Theme 4" = "#e6c69c",
        "Theme 5" = "#d2debf",
        "Theme 6" = "#a4b0bd",
        "Theme 7" = "#deb4b8"
      )
      
      # ---------------------------- Final plot ----------------------------
      ggplot(combined_df, aes(x = percent_agree, y = question_text, fill = theme)) +
        geom_col() +
        
        geom_text(aes(label = paste0(round(percent_agree, 1), "%")),
                  hjust = 1.1, size = 7/3, family = "Calibri", color = "black") +
        
        geom_vline(data = theme_means,
                   aes(xintercept = mean_agree),
                   linetype = "dotted", size = 0.4, color = "gray30") +
        
        geom_text(data = theme_means,
                  aes(x = mean_agree + 1.5,
                      y = 0.25,
                      label = paste0(round(mean_agree, 1), "%")),
                  inherit.aes = FALSE,
                  size = 7/3, family = "Calibri", hjust = 0, vjust = 1, color = "gray30") +
        
        scale_fill_manual(values = theme_colors) +
        
        scale_x_continuous(
          limits = c(0, 100),
          expand = expansion(add = c(0, 15)),
          labels = label_number(suffix = "%")
        ) +
        
        scale_y_discrete(expand = expansion(add = c(1.5, 0))) +
        
        ggforce::facet_col(
          facets = vars(group, theme),
          scales = "free_y",
          space = "free",
          labeller = labeller(theme = theme_labels)
        ) +
        
        labs(
          x = "Percentage Rating as Agreed (%)",
          y = NULL,
          title = "Ratings Across Career Decision Themes",
          fill = "Theme"
        ) +
        
        theme_minimal(base_size = 8, base_family = "Calibri") +
        theme(
          strip.text.x = element_text(face = "bold", size = 9, hjust = 0),  # theme titles (smaller)
          strip.text.y = element_text(face = "bold", size = 10, hjust = 0), # group titles (default size)
          strip.background = element_rect(fill = "lightgrey", color = NA),
          axis.text.y = element_text(size = 7),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_text(size = 8),
          legend.position = "none",
          panel.spacing = unit(1.2, "lines"),
          plot.title = element_text(face = "bold", size = 11)
        )
      
      
      
      