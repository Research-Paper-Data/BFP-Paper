# R code for the paper: Global implementation of biodiversity finance plans


##------Description--------:  
# RQs：What instruments are included in BFPs and what are implemented? 
# What factors influence the implementation of instruments in BFP?



# Section A: Prep Steps ---------------------------

# 1. Setup and load data----


# Load (and install if missing) required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, readr, dplyr, tidyr, janitor, stringr, ggplot2, patchwork, tibble, e1071, cluster,
  factoextra, reshape2, scales)

setwd(
  "/Users/juliamao/Library/CloudStorage/OneDrive-LundUniversity/LU_Papers/BIOFIN paper/Revision_Nature Communication 22 July 2026/Rcode_biofin_paper"
)
getwd()
file.exists("Data/Database_27Aug2025.xlsx")

# Load biofin data (columns B to T, skipping header row 1)
biofin_data <- read_excel("Data/Database_27Aug2025.xlsx", sheet = "Aug27", range = "B2:T1000") %>%
  clean_names()

# Load catalogue data
catalogue <- read_excel("Data/Database_27Aug2025.xlsx", sheet = "Catalogue") %>%
  clean_names()

# 2. Clean & Process `biofin_data`----

# Preview raw unique values in status
unique(raw_status <- str_trim(tolower(biofin_data$status)))

biofin_data <- biofin_data %>%
  filter(!is.na(country)) %>%
  filter(is.na(replacement) | str_trim(str_to_lower(replacement)) != "yes") %>%  # Excluding duplicates by removing the rows with "Yes" in the replacements column 
  mutate(
    status = str_trim(tolower(status)),
    status = case_when(
      str_detect(status, "under implem") ~ "under_implementation",
      str_detect(status, "not implem") ~ "not_implemented",
      status == "implemented" ~ "implemented",
      status == "completed" ~ "completed",
      status == "pending" ~ "pending",
      TRUE ~ NA_character_
    ),
    status_num = case_when(
      is.na(status) ~ 0,
      status %in% c("pending", "not_implemented") ~ 0,
      status %in% c("under_implementation", "implemented", "completed") ~ 1 
    ),
    index = catalogue_index,
    in_bfp_flag = str_trim(str_to_lower(in_bfp_or_not)) == "yes"
  )

unique(raw_status <- str_trim(tolower(biofin_data$status)))

# BIOFIN launched first  Workbook in 2014; (data current through 2024-12-31)
analysis_year   <- 2024L
min_bfp_year    <- 2014L

biofin_data <- biofin_data %>%
  mutate(
    # extract a 4-digit year if present; else NA
    bfp_year = suppressWarnings(
      as.integer(stringr::str_extract(as.character(year_of_publication_of_bfp), "\\b(19\\d{2}|20\\d{2})\\b"))
    ),
    # keep only plausible BIOFIN years
    bfp_year = dplyr::if_else(bfp_year >= min_bfp_year & bfp_year <= analysis_year, bfp_year, NA_integer_),
    # exposure since BFP (Post-negative; uses 2024)
    years_since_bfp = dplyr::if_else(!is.na(bfp_year), pmax(0L, analysis_year - bfp_year), NA_integer_)
  )

# use the cleaned fields downstream
biofin_data2 <- biofin_data %>%
  select(
    region, country, in_bfp_flag,
    bfp_year, years_since_bfp,
    index, catalogue_solution_type_level_a, catalogue_solution_type_level_b,
    status, status_num
  )


# 3. Clean & Process `catalogue`----

# Define categories
type_mechanism <- c("grant", "debt_equity", "risk", "fiscal", "market", "regulatory")
type_result <- c("generate", "realign", "avoid", "deliver")
type_source <- c("private", "public")
type_all <- c(type_mechanism, type_result, type_source)

catalogue2 <- catalogue %>%
  select(index, solution_type, level, all_of(type_all)) %>%
  mutate(across(all_of(type_all), ~ str_trim(str_to_lower(as.character(.x))) == "yes"))

# 4. Join Data on A-Level Instruments---- 
#(Builds a relational database that links each instrument in a country with its classification and implementation status.)

catalogue_a <- catalogue2 %>% filter(level == "A")

biofin_data_joined <- biofin_data2 %>%
  left_join(catalogue_a, by = "index")

#creating a “implemented flag” + a 3-way group variable
biofin_data_joined <- biofin_data_joined %>%
  mutate(
    implemented_flag = dplyr::coalesce(status_num == 1, FALSE),
    
    bfp_impl_group = case_when(
      in_bfp_flag & implemented_flag  ~ "In-BFP: implemented",
      in_bfp_flag & !implemented_flag ~ "In-BFP: not implemented",
      !in_bfp_flag & implemented_flag ~ "Post-BFP: implemented",
      TRUE                            ~ NA_character_
    ),
    
    bfp_impl_group = factor(
      bfp_impl_group,
      levels = c("In-BFP: implemented", "In-BFP: not implemented", "Post-BFP: implemented")
    )
  )

# Define ONE canonical region order (based on BFP universe),
#    so Plots have identical x-axis ordering

region_levels <- c("Africa","Asia and the Pacific","Europe and Central Asia","Latin America and Caribbean")

# 5. Sample accounting for manuscript and reviewer response ----

dir.create("Tables", recursive = TRUE, showWarnings = FALSE)

Instrument_overview <- bind_rows(
  biofin_data_joined %>%
    summarise(
      sample = "Complete database",
      n_country_instrument_observations = n(),
      n_unique_instrument_categories = n_distinct(index),
      n_countries = n_distinct(country),
      n_implemented = sum(implemented_flag),
      n_not_implemented = sum(!implemented_flag)
    ),

  biofin_data_joined %>%
    filter(in_bfp_flag) %>%
    summarise(
      sample = "BFP-listed observations",
      n_country_instrument_observations = n(),
      n_unique_instrument_categories = n_distinct(index),
      n_countries = n_distinct(country),
      n_implemented = sum(implemented_flag),
      n_not_implemented = sum(!implemented_flag)
    ),

  biofin_data_joined %>%
    filter(!in_bfp_flag) %>%
    summarise(
      sample = "Post-BFP observations",
      n_country_instrument_observations = n(),
      n_unique_instrument_categories = n_distinct(index),
      n_countries = n_distinct(country),
      n_implemented = sum(implemented_flag),
      n_not_implemented = sum(!implemented_flag)
    )
)

print(Instrument_overview)

write_csv(
  Instrument_overview,
  "Tables/Instrument_overview.csv"
)

# Section B: Descriptive analysis for RQ1 ---------------------------

# 1. Instruments in countries’ BFPs----
# *1.1 country-instrument in BFPs ----
# Build country × instrument presence matrix for BFP only
mat_bfp_presence <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  group_by(region, country, solution_type) %>%
  summarise(in_bfp_any = any(in_bfp_flag), .groups = "drop") %>%
  mutate(
    cell = if_else(in_bfp_any, "In BFP", NA_character_)
  ) %>%
  filter(!is.na(cell))

# Order countries within region
mat_bfp_presence <- mat_bfp_presence %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  arrange(region, country) %>%
  mutate(country = factor(country, levels = unique(country)))

# Order instruments by frequency in BFPs
instr_levels_presence <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  count(solution_type, sort = TRUE) %>%
  pull(solution_type)

mat_bfp_presence <- mat_bfp_presence %>%
  mutate(solution_type = factor(solution_type, levels = instr_levels_presence))

# Plot
p_bfp_presence <- ggplot(mat_bfp_presence, aes(x = solution_type, y = country, fill = cell)) +
  geom_tile(color = "grey90", linewidth = 0.2) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c("In BFP" = "#6baed6")) +
  labs(
    title = "Country × Instrument Matrix — Instruments included in BFPs",
    subtitle = "Blue cells indicate that an instrument appears in a country’s BFP.",
    x = "Instrument",
    y = "Country",
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p_bfp_presence)

ggsave(
  filename = "Figures/Figure B.png",
  plot = p_bfp_presence,
  width = 15,
  height = 10,
  dpi = 300
)


# *1.2 Instrument type distribution in BFP, by country ----
# Cell = % of instrument entries in a country's BFP tagged with that type
# Multi-tag allowed; totals within a group can exceed 100%

library(grid)   # for unit()

# Type-group lookup table
type_group <- tibble::tibble(
  type = type_all,
  group = c(
    rep("Mechanism", length(type_mechanism)),
    rep("Result",    length(type_result)),
    rep("Source",    length(type_source))
  )
)

# Total number of BFP instrument entries in each country
country_bfp_total <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  count(region, country, name = "n_bfp_entries") %>%
  mutate(region = factor(region, levels = region_levels))

# Template of all valid country × type combinations
country_type_template <- country_bfp_total %>%
  select(region, country) %>%
  distinct() %>%
  tidyr::crossing(type_group)

# Observed tagged entries by country × type
country_type_obs <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  pivot_longer(
    cols = all_of(type_all),
    names_to = "type",
    values_to = "tagged"
  ) %>%
  filter(tagged) %>%
  left_join(type_group, by = "type") %>%
  count(region, country, group, type, name = "n_type") %>%
  mutate(region = factor(region, levels = region_levels))

# Complete with zeros using only valid combinations
country_type_bfp <- country_type_template %>%
  left_join(country_type_obs, by = c("region", "country", "group", "type")) %>%
  left_join(country_bfp_total, by = c("region", "country")) %>%
  mutate(
    n_type   = replace_na(n_type, 0),
    pct_type = 100 * n_type / n_bfp_entries,
    group    = factor(group, levels = c("Mechanism", "Result", "Source"))
  )

# Keep original type order within each group
country_type_bfp <- country_type_bfp %>%
  mutate(
    type = case_when(
      group == "Mechanism" ~ factor(type, levels = type_mechanism),
      group == "Result"    ~ factor(type, levels = type_result),
      group == "Source"    ~ factor(type, levels = type_source)
    )
  )

# Order countries by region, then alphabetically Z → A,
# but reverse factor levels so Africa is at the top in the plot
country_levels <- country_type_bfp %>%
  distinct(region, country) %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  arrange(region, desc(country)) %>%
  pull(country)

country_type_bfp <- country_type_bfp %>%
  mutate(country = factor(country, levels = rev(country_levels)))

# Plot: percentage of country BFP instruments carrying each type tag.
# Tags are not mutually exclusive; percentages within a group may therefore
# sum to more than 100%.
p_country_type_bfp <- ggplot(
  country_type_bfp,
  aes(x = type, y = country, fill = pct_type)
) +
  geom_tile(color = "white", linewidth = 0.25) +
  facet_grid(
    region ~ group,
    scales = "free",
    space = "free",
    switch = "y",
    labeller = labeller(
      region = label_wrap_gen(width = 18),
      group = label_wrap_gen(width = 14)
    )
  ) +
  scale_fill_gradient(
    low = "grey92",
    high = "#2171B5",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    labels = function(x) paste0(x, "%"),
    na.value = "white",
    name = "Share of BFP instruments tagged"
  ) +
  labs(
    title = "Instrument types represented in country BFPs",
    subtitle = paste(
      "Percentage of instruments in each country’s BFP carrying each",
      "mechanism, expected-result or financing-source tag"
    ),
    x = NULL,
    y = NULL,
    caption = paste(
      "% tagged = share of a country’s BFP instruments carrying the tag.",
      "Tags are not mutually exclusive, so totals within a group may exceed 100%.",
      "Thus, 100% public means every BFP instrument carries the public tag."
    )
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 9),
    panel.grid = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1,
      size = 9
    ),
    axis.text.y = element_text(size = 9),
    strip.placement = "outside",
    strip.background = element_blank(),
    strip.text.x = element_text(
      size = 11,
      face = "bold"
    ),
    strip.text.y.left = element_text(
      size = 10,
      face = "bold",
      angle = 0
    ),
    panel.spacing.y = unit(0.9, "lines"),
    plot.caption = element_text(
      hjust = 0,
      size = 9,
      colour = "grey25"
    )
  )

print(p_country_type_bfp)

ggsave(
  filename = "Figures/Figure 2_country_type_bfp.png",
  plot = p_country_type_bfp,
  width = 14,
  height = 11,
  dpi = 300,
  bg = "white"
)




# 2. Implementation----
# *2.1 Impl rate by region : In-BFP vs Post-BFP implemented----
#   Left  = BFP (stacked implemented/not)
#   Right = Post-BFP implemented (single)

# BFP stacked data (grey+green)
bfp_region_plot2 <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  mutate(
    fill = if_else(implemented_flag, "In-BFP: implemented", "In-BFP: not implemented")
  ) %>%
  count(region, fill, name = "n") %>%
  group_by(region) %>%
  mutate(
    bfp_total = sum(n),
    bfp_impl  = sum(n[fill == "In-BFP: implemented"]),
    bfp_rate  = if_else(bfp_total > 0, bfp_impl / bfp_total, NA_real_)
  ) %>%
  ungroup() %>%
  mutate(region = factor(region, levels = region_levels))

bfp_lbl2 <- bfp_region_plot2 %>%
  distinct(region, bfp_total, bfp_rate)

# Post-BFP implemented data (orange), padded to include all regions
postbfp_region <- biofin_data_joined %>%
  filter(!in_bfp_flag, implemented_flag) %>%
  count(region, name = "n") %>%
  right_join(tibble(region = region_levels), by = "region") %>%
  mutate(
    n = replace_na(n, 0),
    fill = "Post-BFP: implemented",
    region = factor(region, levels = region_levels)
  )

# Build region index + x positions (guarantees side-by-side bars) 
region_key <- tibble(region = factor(region_levels, levels = region_levels),
                     region_id = seq_along(region_levels))

bar_gap  <- 0.18
bfp_x    <- -bar_gap
postbfp_x <-  bar_gap

bfp_region2 <- bfp_region_plot2 %>%
  left_join(region_key, by = "region") %>%
  mutate(x = region_id + bfp_x)

bfp_lbl2a <- bfp_lbl2 %>%
  left_join(region_key, by = "region") %>%
  mutate(x = region_id + bfp_x)

postbfp_region2 <- postbfp_region %>%
  left_join(region_key, by = "region") %>%
  mutate(x = region_id + postbfp_x)

p_region_side <- ggplot() +
  # Left bar (stacked BFP)
  geom_col(
    data = bfp_region2,
    aes(x = x, y = n, fill = fill),
    width = 0.30,
    position = "stack"
  ) +
  geom_text(
    data = bfp_lbl2a,
    aes(x = x, y = bfp_total, label = paste0("BFP impl: ", percent(bfp_rate, accuracy = 1))),
    vjust = -0.4,
    size = 3
  ) +
  # Right bar (Post-BFP implemented)
  geom_col(
    data = postbfp_region2,
    aes(x = x, y = n, fill = fill),
    width = 0.30
  ) +
  # Region labels centered between the two bars
  scale_x_continuous(
    breaks = region_key$region_id,
    labels = as.character(region_levels)
  ) +
  scale_fill_manual(values = c(
    "In-BFP: not implemented" = "grey70",
    "In-BFP: implemented"     = "forestgreen",
    "Post-BFP: implemented"    = "darkorange"
  )) +
    labs(title = "Implementation by region: In-BFP instruments vs Post-BFP implemented",
    subtitle = "left = In-BFP instruments (stacked), right =  Post-BFP implemented (single).",
    x = "Region", y = "Number of instruments (rows)", fill = "Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 0, hjust = 0.5)
  )

print(p_region_side)

ggsave(
  filename = "Figures/Figure 3_impl_rate_region.png",
  plot = p_region_side,
  width = 8,
  height = 5,
  dpi = 300
)


# *2.2 Country × Instrument Matrix: Implementation (In-BFP vs Post-BFP)----
library(forcats)

# Build matrix data (BFP only)
mat_bfp <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  group_by(region, country, solution_type) %>%
  summarise(implemented_any = any(implemented_flag), .groups = "drop") %>%
  mutate(
    cell = if_else(implemented_any, "In-BFP: implemented", "In-BFP: not implemented")
  )

# Order countries within region (and keep regions in your paper order)
mat_bfp <- mat_bfp %>%
  mutate(
    region  = factor(region, levels = region_levels),
    country = fct_inorder(country)  # will be re-leveled after arrange
  ) %>%
  arrange(region, country) %>%
  mutate(country = factor(country, levels = unique(country)))


# Ordered y frequency in BFP
instr_levels_bfp <- mat_bfp %>% count(solution_type, sort = TRUE) %>% pull(solution_type)

mat_bfp <- mat_bfp %>%
  mutate(solution_type = factor(solution_type, levels = instr_levels_bfp))

# Plot in_post_bfp (not included in the paper)
p_in_post_bfp <- ggplot(mat_bfp, aes(x = solution_type, y = country, fill = cell)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    "In-BFP: not implemented" = "grey70",
    "In-BFP: implemented"     = "forestgreen"
  )) +
  labs(
    title = "Country × Instrument Matrix — BFP instruments only",
    x = "Instrument", y = "Country", fill = "Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p_in_post_bfp) #This figure is not report in the paper 


# Country × Instrument matrix (In BFP split + Post-BFP implemented)
# Build matrix data (BFP + Post-BFP implemented)
mat_all <- biofin_data_joined %>%
  filter(in_bfp_flag | (!in_bfp_flag & implemented_flag)) %>%   # keep BFP rows + implemented outside BFP
  group_by(region, country, solution_type) %>%
  summarise(
    in_bfp_any       = any(in_bfp_flag),
    implemented_any  = any(implemented_flag),
    .groups = "drop"
  ) %>%
  mutate(
    cell = case_when(
      in_bfp_any & implemented_any  ~ "In-BFP: implemented",
      in_bfp_any & !implemented_any ~ "In-BFP: not implemented",
      !in_bfp_any & implemented_any ~ "Post-BFP: implemented",
      TRUE                          ~ NA_character_
    )
  ) %>%
  filter(!is.na(cell))

# Order countries within region (and keep regions in your paper order)
mat_all <- mat_all %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  arrange(region, country) %>%
  mutate(country = factor(country, levels = unique(country)))

# Order by frequency in this combined universe
instr_levels_all <- mat_all %>% count(solution_type, sort = TRUE) %>% pull(solution_type)

mat_all <- mat_all %>%
  mutate(solution_type = factor(solution_type, levels = instr_levels_all))

# **plot figure 4
p_impl_matrix <- ggplot(mat_all, aes(x = solution_type, y = country, fill = cell)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(region ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    "In-BFP: not implemented" = "grey70",
    "In-BFP: implemented"     = "forestgreen",
    "Post-BFP: implemented"    = "darkorange"
  )) +
  labs(
    title = "Country × Instrument Matrix — In-BFP implemented, In-BFP not implemented, Post-BFP implemented",
    x = "Instrument", y = "Country", fill = "Status"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

print(p_impl_matrix)
ggsave(
  filename = "Figures/Figure 4_impl_matrix.png",
  plot = p_impl_matrix,
  width = 12,
  height = 8,
  dpi = 300
)


# *2.3 Type distribution----

# IN-BFP type distribution by region, split by implementation + label implementation rate----

# Interpretation: Counts are tag hits (multi-tag allowed). 
# For each region × type, the bar is the total BFP tag hits split into implemented vs not implemented. 
# Label shows the implementation rate within BFP for that type.

bfp_type_split <- biofin_data_joined %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  filter(in_bfp_flag) %>%
  pivot_longer(cols = all_of(type_all), names_to = "type", values_to = "value") %>%
  filter(value) %>%
  left_join(type_group, by = "type") %>%
  mutate(
    impl_status = if_else(implemented_flag, "Implemented", "Not implemented"),
    type  = factor(type, levels = type_all),
    group = factor(group, levels = c("Mechanism","Result","Source"))
  ) %>%
  count(region, group, type, impl_status, name = "n") %>%
  group_by(region, group, type) %>%
  mutate(
    total_bfp_type = sum(n),
    impl_n_type    = sum(n[impl_status == "Implemented"]),
    impl_rate_type = if_else(total_bfp_type > 0, impl_n_type / total_bfp_type, NA_real_)
  ) %>%
  ungroup()

bfp_type_lbl <- bfp_type_split %>%
  distinct(region, group, type, total_bfp_type, impl_rate_type) %>%
  # optional: reduce clutter by labeling only when there is something to label
  filter(total_bfp_type > 0)

p_type_bfp_split <- ggplot(bfp_type_split, aes(x = type, y = n, fill = impl_status)) +
  geom_col(position = "stack", width = 0.7) +
  geom_text(
    data = bfp_type_lbl,
    aes(x = type, y = total_bfp_type, label = percent(impl_rate_type, accuracy = 1)),
    vjust = -0.35,
    size = 3,
    inherit.aes = FALSE,
    check_overlap = TRUE
  ) +
  facet_grid(region ~ group, scales = "free_x", space = "free_y") +
  scale_fill_manual(values = c("Not implemented" = "grey70", "Implemented" = "forestgreen")) +
  labs(
    title = "In-BFP: type distribution split by implementation (counts of tag hits)",
    subtitle = "Each bar = total BFP tag hits for that type; label = implementation rate within BFP for that type.",
    x = "Type", y = "Count of tag hits", fill = "BFP implementation"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_type_bfp_split) #figure not included in the paper


# Merged type plot: In-BFP implemented, not Implemented vs Post-BFP implemented, SIDE-BY-SIDE (same style as above)
#    For each region x type, two bars:
#      - BFP (stacked)
#      - Post-BFP implemented (single)
#    Label = BFP implementation rate (for that type)

# FP side (stacked) 
bfp_type_counts <- biofin_data_joined %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  filter(in_bfp_flag) %>%
  pivot_longer(cols = all_of(type_all), names_to = "type", values_to = "value") %>%
  filter(value) %>%
  left_join(type_group, by = "type") %>%
  mutate(
    fill = if_else(implemented_flag, "In-BFP: implemented", "In-BFP: not implemented"),
    type  = factor(type, levels = type_all),
    group = factor(group, levels = c("Mechanism","Result","Source"))
  ) %>%
  count(region, group, type, fill, name = "n") %>%
  group_by(region, group, type) %>%
  mutate(
    bfp_total = sum(n),
    bfp_impl  = sum(n[fill == "In-BFP: implemented"]),
    bfp_rate  = if_else(bfp_total > 0, bfp_impl / bfp_total, NA_real_)
  ) %>%
  ungroup()

bfp_type_lbl <- bfp_type_counts %>%
  distinct(region, group, type, bfp_total, bfp_rate) %>%
  filter(bfp_total > 0)

# Post-BFP implemented side (single orange bar)
postbfp_type_counts <- biofin_data_joined %>%
  mutate(region = factor(region, levels = region_levels)) %>%
  filter(!in_bfp_flag, implemented_flag) %>%
  pivot_longer(cols = all_of(type_all), names_to = "type", values_to = "value") %>%
  filter(value) %>%
  left_join(type_group, by = "type") %>%
  mutate(
    fill = "Post-BFP: implemented",
    type  = factor(type, levels = type_all),
    group = factor(group, levels = c("Mechanism","Result","Source"))
  ) %>%
  count(region, group, type, fill, name = "n")

# Numeric-x trick to get true side-by-side bars per type
type_key <- tibble(type = factor(type_all, levels = type_all),
                   type_id = seq_along(type_all))

bar_gap  <- 0.18
bfp_x    <- -bar_gap
postbfp_x <-  bar_gap

bfp_type_counts2 <- bfp_type_counts %>%
  left_join(type_key, by = "type") %>%
  mutate(x = type_id + bfp_x)

bfp_type_lbl2 <- bfp_type_lbl %>%
  left_join(type_key, by = "type") %>%
  mutate(x = type_id + bfp_x)

postbfp_type_counts2 <- postbfp_type_counts %>%
  left_join(type_key, by = "type") %>%
  mutate(x = type_id + postbfp_x)

# Plot
p_type_merged <- ggplot() +
  # Left: BFP stacked
  geom_col(
    data = bfp_type_counts2,
    aes(x = x, y = n, fill = fill),
    width = 0.30,
    position = "stack"
  ) +
  geom_text(
    data = bfp_type_lbl2,
    aes(x = x, y = bfp_total, label = percent(bfp_rate, accuracy = 1)),
    vjust = -0.35,
    size = 3,
    check_overlap = TRUE
  ) +
  # Right: Post-BFP implemented
  geom_col(
    data = postbfp_type_counts2,
    aes(x = x, y = n, fill = fill),
    width = 0.30
  ) +
  facet_grid(region ~ group, scales = "free_x", space = "free_y") +
  scale_x_continuous(
    breaks = type_key$type_id,
    labels = type_key$type
  ) +
  scale_fill_manual(values = c(
    "In-BFP: not implemented" = "grey70",
    "In-BFP: implemented"     = "forestgreen",
    "Post-BFP: implemented"    = "darkorange"
  )) +
  labs(
    title = "Type distribution: In-BFP implemented/not implemented vs Post-BFP implemented",
    subtitle = "Per region×type: left = In-BFP (stacked), right = Post-BFP (single). Label = BFP implementation rate.",
    x = "Type", y = "Count of tag hits", fill = "Category"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(p_type_merged)

ggsave(
  filename = "Figures/Figure 5_type.png",
  plot = p_type_merged,
  width = 8,
  height = 10,
  dpi = 300
)

# Section C: Inmport and Pre-process data for RQ2 --------

# RQ2 outcome: Reported implementation activity among BFP-listed instrument interventions as of 31 December 2024.
# Country-context variables are summarized around the publication year of the particular BFP edition to which each intervention belongs.

#Clean up any wrong package that may be attached
if ("package:brm" %in% search()) detach("package:brm", unload = TRUE)

# Install brms if needed
if (!requireNamespace("brms", quietly = TRUE)) install.packages("brms")

if (!require("pacman")) install.packages("pacman")
pacman::p_load(
  readxl, readr, dplyr, tidyr, stringr, tibble, janitor,
  countrycode, WDI,
  lme4,            # for glmer()
  brms,            # for brm()
  tidybayes, modelr,
  ggplot2, patchwork, scales, zoo
)


# 1. One clean block for ISO3/BFP window---- 

# ---- ISO3, BFP timing fields, and TA are present
DATA_CUTOFF_YEAR <- 2024L
BFP_YEAR_MIN     <- 2014L

# ISO3
if (!"iso3c" %in% names(biofin_data_joined)) {
  biofin_data_joined <- biofin_data_joined %>% mutate(iso3c = NA_character_)
}
biofin_data_joined <- biofin_data_joined %>%
  mutate(
    iso3c = dplyr::if_else(
      is.na(iso3c) | iso3c == "",
      countrycode::countrycode(country, "country.name", "iso3c", warn = TRUE),
      iso3c
    ))

# BFP year & exposure 
biofin_data_joined <- biofin_data_joined %>%
  mutate(
    bfp_year = suppressWarnings(as.integer(bfp_year)),
    bfp_year = dplyr::if_else(
      !is.na(bfp_year) & bfp_year >= BFP_YEAR_MIN & bfp_year <= DATA_CUTOFF_YEAR,
      bfp_year, NA_integer_
    ),
    years_since_bfp = dplyr::if_else(!is.na(bfp_year),
                                     pmax(0L, DATA_CUTOFF_YEAR - bfp_year),
                                     NA_integer_))

dir.create("Data", showWarnings = FALSE)
readr::write_csv(biofin_data_joined, "Data/biofin_data_joined.csv")

# 2. External covariates from WDI, WGI, ODA----

# build a country–year panel (2010–2024) and then roll it up to pre-BFP and post-BFP summaries per country.


# *2.1 World Bank indicators via WDI (auto-download)----
WB_IND <- c(
  gdppc = "NY.GDP.PCAP.CD", # Income level: GDP per capita, current US$ 
  tax_gdp = "GC.TAX.TOTL.GD.ZS",  # Fiscal capacity: Tax revenue (% of GDP)
  pop = "SP.POP.TOTL"  # Population (optional scaling) 
)
years  <- 2010:2024

wb_raw <- WDI(country = "all", indicator = WB_IND, 
              start = min(years), end = max(years), extra = TRUE) %>%
  as_tibble() %>%
  clean_names() %>%
  rename(iso3c = iso3c, year = year)

wb_panel <- wb_raw %>%
  filter(!is.na(iso3c), region != "Aggregates") %>%
  select(iso3c, year, gdppc, tax_gdp, pop)

View(wb_panel)

#if needed, update.packages("WDI")

# save
dir.create("Data/External", showWarnings = FALSE)
write_csv(wb_panel, "Data/External/wb_panel.csv")


# *2.2 Worldwide Governance Indicators (WGI) — manual download then read----
# (WGI isn’t in WDI. Download the country–year CSV from: https://www.worldbank.org/en/publication/worldwide-governance-indicators)

# (1) Government Effectiveness (GE.EST)
# (Government effectiveness captures perceptions of the quality of public services, the quality of the civil service 
# and the degree of its independence from political pressures, the quality of policy formulation and implementation, 
# and the credibility of the government's commitment to such policies. see: https://www.worldbank.org/content/dam/sites/govindicators/doc/ge.pdf)

# (2) Regulatory Quality (RQ.EST)
# Regulatory quality captures perceptions of the ability of the government to formulate and implement sound policies and regulations 
#that permit and promote private sector development. See here: https://www.worldbank.org/content/dam/sites/govindicators/doc/rq.pdf

# (3) Political Stability (PV.EST)
# Political Stability and Absence of Violence/Terrorism measures perceptions of the likelihood of political instability 
# and/or politicallymotivated violence, including terrorism. See here:https://www.worldbank.org/content/dam/sites/govindicators/doc/pv.pdf

wgi_raw <- read_csv("Data/External/wgi_full.csv")  # keep original header
# WGI has columns like: Country Name, Country Code, Indicator Code, Indicator Name, 2010, 2011, ... 2024

# Identify columns like "2010 [YR2010]", "2011 [YR2011]", … "2024 [YR2024]"
year_cols <- grep("^\\d{4} \\[YR\\d{4}\\]$", names(wgi_raw), value = TRUE)

wgi_long <- wgi_raw %>%
  pivot_longer(
    cols = all_of(year_cols),
    names_to = "year_label",
    values_to = "value"
  ) %>%
  # Extract the leading 4 digits as the numeric year
  mutate(year = as.integer(str_extract(year_label, "^\\d{4}"))) %>%
  # Keep the three WGI “Estimate” series we need and the 2010–2024 window
  filter(year >= 2010, year <= 2024,
         `Series Code` %in% c("GE.EST", "RQ.EST", "PV.EST")) %>%
  # Standardize columns and go wide to GE, RQ, PV
  transmute(
    iso3c    = `Country Code`,
    year,
    indicator = `Series Code`,
    value = as.numeric(value)
  ) %>%
  pivot_wider(names_from = indicator, values_from = value) %>%
  rename(GE = `GE.EST`, RQ = `RQ.EST`, PV = `PV.EST`)

View(wgi_long)

# *2.3 Biodiversity-related ODA (OECD CRS) — manual export, read and process----
# Input: Data/oda_bio.csv  (one row per donor × recipient × year × score, USD)
# Output: oda_bio_recipient_year (iso3c × year totals) + joins into 'panel'

#read raw data
oda_raw <- read_csv("Data/External/oda_bio.csv", show_col_types = FALSE) %>% clean_names()

# Clean data
oda_selet <- oda_raw %>%
  select(donor, recipient, score, time_period, obs_value, unit_mult
  ) %>%
  rename(iso3c=recipient, year=time_period)

#View(oda_selet)

# Build recipient–year totals
signif_w <- 0.40
oda_bio_recipient_year <- oda_selet %>%
  mutate(
    # 1. Rio marker weights
    weight = case_when(
      score == 2 ~ 1,
      score == 1 ~ signif_w,
      TRUE       ~ 0
    ),
    # 2. Fix for unit multiplier 
    value_usd = obs_value * (10 ^ unit_mult),
    # 3. Biodiversity-weighted value
    bio_value = value_usd * weight
  ) %>%
  group_by(iso3c, year) %>%
  summarise(
    # total biodiversity-relevant ODA (weighted principal+significant). --Current version, but tbd
    oda_bio_total   = sum(bio_value, na.rm = TRUE),
    
    # optional: total gross ODA (unweighted)
    # oda_gross_total = sum(value_usd, na.rm = TRUE),
    # optional: split principal vs significant
    #oda_bio_principal   = sum(value_usd[score == 2], na.rm = TRUE),
    #oda_bio_significant = sum(value_usd[score == 1] * signif_w, na.rm = TRUE),
    .groups = "drop"
  )

View(oda_bio_recipient_year)      

# save
dir.create("Data/External", showWarnings = FALSE)
write_csv(oda_bio_recipient_year, "Data/External/oda_bio_recipient_year.csv")



# *2.4 Build a country–year panel & derive BFP window [-2,+2] co-variates----

# BFP-time capacity/governance/donor = average over years [bfp_year-2, bfp_year+2] (truncate to 2010 lower bound).
# Convert ODA to per capita for comparability.

# ---- Country–year backbone for the 32 countries 
years <- 2010:2024

cty_list <- biofin_data_joined %>%
  mutate(country = trimws(as.character(country))) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(
    country = first(country),
    region = first(region),
    .groups = "drop"
  )

panel <- tidyr::expand_grid(iso3c = cty_list$iso3c, year = years) %>%
  left_join(cty_list, by = "iso3c", relationship = "many-to-one")  # safe join


# Merge WDI, WGI, ODA 
# Expect:
#   wb_panel: iso3c, year, gdppc, tax_gdp, pop
#   wgi_long: iso3c, year, GE, RQ, PV
#   oda_bio_recipient_year: iso3c, year, oda_bio_total 

panel <- panel %>%
  left_join(wb_panel,               by = c("iso3c","year")) %>%
  left_join(wgi_long,               by = c("iso3c","year")) %>%
  left_join(oda_bio_recipient_year, by = c("iso3c","year")) %>%
  mutate(
    oda_bio_weighted_pc = dplyr::if_else(!is.na(pop) & pop > 0,
                                         oda_bio_total/ pop, NA_real_)
  )

View(panel)
# save
dir.create("Data/External", showWarnings = FALSE)
write_csv(panel, "Data/External/panel.csv")

# BFP window covatiates 
bfp_years <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%
  distinct(iso3c, bfp_year)

#view(bfp_years)
# *Note that Thailand has two BPF
# BFP_window value: average of (bfp_year−2 … bfp_year+2)

bfp_editions <- biofin_data_joined %>%
  filter(
    in_bfp_flag,
    !is.na(iso3c),
    !is.na(bfp_year)
  ) %>%
  distinct(iso3c, bfp_year)

bfp_window_cap <- panel %>%
  inner_join(
    bfp_editions,
    by = "iso3c",
    relationship = "many-to-many"
  ) %>%
  mutate(
    year_lower = pmax(
      2010L,
      bfp_year - 2L
    ),
    year_upper = pmin(
      2024L,
      bfp_year + 2L
    )
  ) %>%
  filter(
    year >= year_lower,
    year <= year_upper
  ) %>%
  group_by(iso3c, bfp_year) %>%
  summarise(
    gdppc_bfp_window = mean(gdppc, na.rm = TRUE),
    tax_bfp_window = mean(tax_gdp, na.rm = TRUE),
    GE_bfp_window = mean(GE, na.rm = TRUE),
    RQ_bfp_window = mean(RQ, na.rm = TRUE),
    PV_bfp_window = mean(PV, na.rm = TRUE),
    oda_bfp_window = mean(oda_bio_total, na.rm = TRUE),
    oda_bfp_window_pc = mean(
      oda_bio_weighted_pc,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    across(
      where(is.numeric),
      ~ if_else(is.nan(.x), NA_real_, .x)
    )
  )

View(bfp_window_cap)

# *2.5 save the data---- 
covars_country <- cty_list %>%
  select(country, iso3c, region) %>%
  left_join(bfp_window_cap,   by = "iso3c", relationship = "one-to-many")

View(covars_country)

dir.create("Outputs", showWarnings = FALSE)
write_csv(covars_country, "Outputs/covars_country.csv")

saveRDS(covars_country, file = "Data/cp.rds")



# Section D: Modelling, visualization, and RQ2 -----------------------------
#
# RQ2: Among instrument interventions recorded in countries's BFPs, which
# instrument-design characteristics and country conditions are associated
# with reported implementation activity by 31 December 2024?
#
# IMPORTANT:
# - One row is one distinct, separately implementable intervention.
# - The inherited outcome coding equals 1 for under implementation, implemented, or completed, and 0 otherwise. It therefore measures reported implementation activity, not verified completion.
# - years_since_bfp measures BFP age, not instrument-specific implementation time.
# - All models below use the same complete-case sample for valid comparison.


# 1. Set up ----------------------------------------------------------------

required_packages_d <- c(
  "brms", "cmdstanr", "posterior", "bayesplot", "loo",
  "dplyr", "tidyr", "tibble", "readr", "ggplot2", "scales", "stringr"
)

missing_packages_d <- required_packages_d[
  !vapply(required_packages_d, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages_d) > 0L) {
  stop(
    "Install the following packages before running Section D: ",
    paste(missing_packages_d, collapse = ", ")
  )
}

suppressPackageStartupMessages({
  library(brms)
  library(cmdstanr)
  library(posterior)
  library(bayesplot)
  library(loo)
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(ggplot2)
  library(scales)
  library(stringr)
})

detected_cores <- parallel::detectCores(logical = FALSE)
if (is.na(detected_cores)) {
  detected_cores <- parallel::detectCores(logical = TRUE)
}
if (is.na(detected_cores)) {
  detected_cores <- 1L
}
MODEL_CORES <- max(1L, min(4L, as.integer(detected_cores)))

options(mc.cores = MODEL_CORES)
bayesplot::color_scheme_set("brightblue")

dir.create("Outputs/model_audits", recursive = TRUE, showWarnings = FALSE)
dir.create("Outputs/model_results", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures/model_results", recursive = TRUE, showWarnings = FALSE)
dir.create("Tables", recursive = TRUE, showWarnings = FALSE)

MODEL_SEED <- 20240827L

# If running section D seperately: 
# Read the prepared datasets directly----
# (check that the working directory is correct and that the expected files exist)
# setwd("/Users/juliamao/Library/CloudStorage/OneDrive-LundUniversity/LU_Papers/BIOFIN paper/Revision_Nature Communication 22 July 2026/Rcode_biofin_paper")
# getwd()
# file.exists("Data/Database_27Aug2025.xlsx")

# biofin_data_joined <- read_csv("Data/biofin_data_joined.csv")  
# covars_country <- readRDS("Data/cp.rds")

# region_levels <- c("Africa", "Asia and the Pacific", "Europe and Central Asia", "Latin America and Caribbean")

# 2. Construct one common RQ2 analysis sample ------------------------------

# Define the instrument-design variables
mechanism_vars <- c(
  "grant", "debt_equity", "risk", "fiscal", "market", "regulatory"
)

result_vars <- c("generate", "realign", "avoid", "deliver")

context_vars_raw <- c(
  "gdppc_bfp_window", "GE_bfp_window", "oda_bfp_window_pc"
)

# Use the corrected Section C object directly. covars_country must contain one
# row per country x BFP edition (33 rows: 32 countries and two Thai editions).
if (!"bfp_year" %in% names(covars_country)) {
  stop(
    paste(
      "covars_country must retain bfp_year.",
      "In Section C, summarize and join covariates by iso3c + bfp_year."
    )
  )
}

context_reference <- covars_country %>%
  select(
    iso3c, bfp_year,
    gdppc_bfp_window,
    GE_bfp_window,
    oda_bfp_window_pc
  ) %>%
  distinct()

if (anyDuplicated(context_reference[c("iso3c", "bfp_year")]) > 0L) {
  stop("covars_country has duplicate iso3c x bfp_year records.")
}

bfp_editions <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(iso3c), !is.na(bfp_year)) %>%
  distinct(iso3c, bfp_year)

if (nrow(context_reference) != nrow(bfp_editions)) {
  stop("covars_country does not contain exactly one row per country x BFP edition.")
}

write_csv(
  context_reference,
  "Outputs/model_audits/rq2_context_by_bfp_edition_raw.csv"
)

# Standardize contextual covariates using unique country x BFP editions as
# the units. The number of interventions in an edition therefore does not
# affect the calculation of the mean and SD.

context_scaling <- context_reference %>%
  summarise(
    across(
      all_of(context_vars_raw),
      list(mean = ~ mean(.x, na.rm = TRUE), sd = ~ stats::sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )

scale_with_reference <- function(x, center, spread) {
  if (!is.finite(spread) || spread <= 0) {
    stop("A standardization SD is missing or non-positive.")
  }
  (x - center) / spread
}

context_reference <- context_reference %>%
  mutate(
    gdppc_bfp_window_z = scale_with_reference(
      gdppc_bfp_window,
      context_scaling$gdppc_bfp_window_mean,
      context_scaling$gdppc_bfp_window_sd
    ),
    GE_bfp_window_z = scale_with_reference(
      GE_bfp_window,
      context_scaling$GE_bfp_window_mean,
      context_scaling$GE_bfp_window_sd
    ),
    oda_bfp_window_pc_z = scale_with_reference(
      oda_bfp_window_pc,
      context_scaling$oda_bfp_window_pc_mean,
      context_scaling$oda_bfp_window_pc_sd
    )
  )

# BFP age can vary by BFP edition (Thailand has two BFP years). Standardize it
# over unique country x BFP-year combinations, rather than over all rows.
bfp_age_reference <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year), !is.na(years_since_bfp)) %>%
  distinct(country, bfp_year, years_since_bfp)

bfp_age_mean <- mean(bfp_age_reference$years_since_bfp, na.rm = TRUE)
bfp_age_sd <- stats::sd(bfp_age_reference$years_since_bfp, na.rm = TRUE)

if (!is.finite(bfp_age_sd) || bfp_age_sd <= 0) {
  stop("years_since_bfp has a missing or non-positive standard deviation.")
}

bfp_age_reference <- bfp_age_reference %>%
  mutate(years_since_bfp_z = (years_since_bfp - bfp_age_mean) / bfp_age_sd)

df_bfp_rq2_pre <- biofin_data_joined %>%
  filter(in_bfp_flag) %>%
  select(
    region, country, iso3c, bfp_year, years_since_bfp,
    status_num, any_of("status"),
    all_of(mechanism_vars), all_of(result_vars), private, public
  ) %>%
  left_join(
    bfp_age_reference %>% select(country, bfp_year, years_since_bfp_z),
    by = c("country", "bfp_year"),
    relationship = "many-to-one"
  ) %>%
  left_join(
    context_reference %>%
      select(
        iso3c, bfp_year, gdppc_bfp_window_z, GE_bfp_window_z,
        oda_bfp_window_pc_z
      ),
    by = c("iso3c", "bfp_year"),
    relationship = "many-to-one"
  ) %>%
  mutate(
    #status_num was coded upstream as 1 for reported implementation activity
    # and 0 otherwise, including records with missing status.
   implemented = as.integer(status_num),
    across(all_of(c(mechanism_vars, result_vars)), as.integer),
    source_group = case_when(
      public & private  ~ "blended",
      public & !private ~ "public",
      !public & private ~ "private",
      TRUE              ~ NA_character_
    ),
    source_group = factor(
      source_group,
      levels = c("public", "private", "blended")
    ),
    country = factor(country),
    region = factor(region, levels = region_levels)
  )

if (nrow(df_bfp_rq2_pre) != sum(biofin_data_joined$in_bfp_flag, na.rm = TRUE)) {
  stop("The RQ2 joins changed the number of BFP intervention rows.")
}

analysis_variables <- c(
  "implemented", "country", "region", "source_group",
  mechanism_vars, result_vars,
  "years_since_bfp_z", "gdppc_bfp_window_z",
  "GE_bfp_window_z", "oda_bfp_window_pc_z"
)

missingness_rq2 <- df_bfp_rq2_pre %>%
  summarise(across(all_of(analysis_variables), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "n_missing")

write_csv(missingness_rq2, "Outputs/model_audits/rq2_missingness.csv")
print(missingness_rq2)
#view(df_bfp_rq2_pre)

if ("status" %in% names(df_bfp_rq2_pre)) {
  outcome_status_audit <- df_bfp_rq2_pre %>%
    count(status, implemented, name = "n") %>%
    arrange(desc(n))

  print(outcome_status_audit)
  write_csv(
    outcome_status_audit,
    "Outputs/model_audits/rq2_outcome_status_audit.csv"
  )
} else {
  warning(
    paste(
      "The cleaned status field was dropped in Section A.",
      "Add status to biofin_data2 so the outcome coding can be audited."
    )
  )
}

df_bfp_models <- df_bfp_rq2_pre %>%
  filter(if_all(all_of(analysis_variables), ~ !is.na(.x))) %>%
  droplevels()

sample_audit_rq2 <- tibble(
  sample = c("All BFP-listed interventions", "Complete-case RQ2 sample"),
  n_observations = c(nrow(df_bfp_rq2_pre), nrow(df_bfp_models)),
  n_implemented = c(
    sum(df_bfp_rq2_pre$implemented == 1L, na.rm = TRUE),
    sum(df_bfp_models$implemented == 1L)
  ),
  n_countries = c(
    n_distinct(df_bfp_rq2_pre$country),
    n_distinct(df_bfp_models$country)
  )
)

print(sample_audit_rq2)
write_csv(sample_audit_rq2, "Tables/rq2_analysis_sample.csv")
saveRDS(df_bfp_models, "Data/df_bfp_models_revised.rds")
view(df_bfp_models)


# 3. Descriptive results ---------------------------------------------------

summarise_binary_factor <- function(data, variable, group_name) {
  data %>%
    group_by(level = .data[[variable]]) %>%
    summarise(
      n = n(),
      n_countries = n_distinct(country),
      n_implemented = sum(implemented),
      implementation_rate = mean(implemented),
      .groups = "drop"
    ) %>%
    mutate(group = group_name, variable = variable, .before = 1)
}

desc_source <- df_bfp_models %>%
  group_by(level = source_group) %>%
  summarise(
    n = n(),
    n_countries = n_distinct(country),
    n_implemented = sum(implemented),
    implementation_rate = mean(implemented),
    .groups = "drop"
  ) %>%
  mutate(group = "Source", variable = "source_group", .before = 1)

desc_mechanism <- bind_rows(lapply(
  mechanism_vars,
  function(v) summarise_binary_factor(df_bfp_models, v, "Mechanism")
))

desc_result <- bind_rows(lapply(
  result_vars,
  function(v) summarise_binary_factor(df_bfp_models, v, "Result")
))

desc_bfp_age <- df_bfp_models %>%
  group_by(years_since_bfp) %>%
  summarise(
    n = n(),
    n_countries = n_distinct(country),
    n_implemented = sum(implemented),
    implementation_rate = mean(implemented),
    .groups = "drop"
  )

# print(desc_source)
# print(desc_mechanism)
# print(desc_result)
# print(desc_bfp_age)

write_csv(desc_source, "Tables/rq2_descriptive_source.csv")
write_csv(desc_mechanism, "Tables/rq2_descriptive_mechanism.csv")
write_csv(desc_result, "Tables/rq2_descriptive_result_tags.csv")
write_csv(desc_bfp_age, "Tables/rq2_descriptive_bfp_age.csv")

# A sparse-cell audit is essential because risk is rare in the current data.
design_cell_audit <- bind_rows(
  desc_mechanism,
  desc_result
) %>%
  filter(level == 1L) %>%
  select(group, variable, n, n_countries, n_implemented, implementation_rate)

print(design_cell_audit)
write_csv(design_cell_audit, "Outputs/model_audits/rq2_design_cell_audit.csv")

if (design_cell_audit$n[design_cell_audit$variable == "risk"] < 10L) {
  warning(
    "The risk mechanism has fewer than 10 observations; interpret its estimate as data-limited and prior-sensitive."
  )
}

# Correlations among contextual predictors are calculated across BFP editions,
# not across 337 interventions, to avoid weighting editions by their row count.
context_correlation <- context_reference %>%
  select(
    gdppc_bfp_window_z,
    GE_bfp_window_z,
    oda_bfp_window_pc_z
  ) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable")

write_csv(
  context_correlation,
  "Outputs/model_audits/rq2_context_correlation.csv"
)

p_desc_bfp_age <- ggplot(
  desc_bfp_age,
  aes(x = years_since_bfp, y = implementation_rate)
) +
  geom_line(color = "#2C7FB8", linewidth = 0.8) +
  geom_point(aes(size = n), color = "#2C7FB8") +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_size_continuous(name = "Interventions") +
  labs(
    x = "Years since publication of the BFP",
    y = "Observed proportion with reported implementation activity",
    caption = "Unadjusted proportions; points are based on country/BFP cohorts."
  ) +
  theme_minimal()

print(p_desc_bfp_age)
ggsave(
  "Figures/model_results/rq2_descriptive_bfp_age.png",
  p_desc_bfp_age, width = 7, height = 5, dpi = 300
)


# 4. Model definitions and priors -----------------------------------------
# Four Bayesian multilevel logistic models are defined:
#   1) baseline model
#   2) country-context model
#   3) instrument-design model
#   4) full combined model

formula_baseline <- bf(
  implemented ~ 1 + (1 | country)
)

formula_context <- bf(
  implemented ~
    years_since_bfp_z +
    gdppc_bfp_window_z +
    GE_bfp_window_z +
    oda_bfp_window_pc_z +
    (1 | country)
)

formula_design <- bf(
  implemented ~
    source_group +
    grant + debt_equity + risk + fiscal + market + regulatory +
    generate + realign + avoid + deliver +
    (1 | country)
)

formula_full <- bf(
  implemented ~
    source_group +
    grant + debt_equity + risk + fiscal + market + regulatory +
    generate + realign + avoid + deliver +
    years_since_bfp_z +
    gdppc_bfp_window_z +
    GE_bfp_window_z +
    oda_bfp_window_pc_z +
    (1 | country)
)

# Regularizing priors are especially important for the rare risk tag and for
# correlated, overlapping mechanism/result tags.
priors_rq2 <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(normal(0, 0.7), class = "b"),
  prior(exponential(1), class = "sd")
)

priors_rq2_baseline <- c(
  prior(student_t(3, 0, 2.5), class = "Intercept"),
  prior(exponential(1), class = "sd")
)

fit_rq2_model <- function(formula, data, seed, model_prior = priors_rq2) {
  brm(
    formula = formula,
    data = data,
    family = bernoulli(link = "logit"),
    prior = model_prior,
    backend = "cmdstanr",
    chains = 4,
    iter = 4000,
    warmup = 2000,
    cores = MODEL_CORES,
    seed = seed,
    control = list(adapt_delta = 0.99, max_treedepth = 15),
    save_pars = save_pars(all = TRUE),
    refresh = 500
  )
}


# 5. Fit the prespecified model sequence ----------------------------------

fit_rq2_baseline <- fit_rq2_model(
  formula_baseline, df_bfp_models, MODEL_SEED + 1L,
  model_prior = priors_rq2_baseline
)

# Context model comes before the instrument-design model, as requested.
fit_rq2_context <- fit_rq2_model(
  formula_context, df_bfp_models, MODEL_SEED + 2L
)

fit_rq2_design <- fit_rq2_model(
  formula_design, df_bfp_models, MODEL_SEED + 3L
)

fit_rq2_full <- fit_rq2_model(
  formula_full, df_bfp_models, MODEL_SEED + 4L
)

saveRDS(fit_rq2_baseline, "Outputs/model_results/fit_rq2_baseline.rds")
saveRDS(fit_rq2_context, "Outputs/model_results/fit_rq2_context.rds")
saveRDS(fit_rq2_design, "Outputs/model_results/fit_rq2_design.rds")
saveRDS(fit_rq2_full, "Outputs/model_results/fit_rq2_full.rds")


# 6. Model summaries and diagnostics --------------------------------------
rq2_fits <- list(
  baseline = fit_rq2_baseline,
  context = fit_rq2_context,
  design = fit_rq2_design,
  full = fit_rq2_full
)

for (model_name in names(rq2_fits)) {
  fit_now <- rq2_fits[[model_name]]

  capture.output(
    summary(fit_now),
    file = file.path(
      "Outputs/model_audits",
      paste0("rq2_", model_name, "_summary.txt")
    )
  )

  ppc_now <- pp_check(fit_now, type = "bars", ndraws = 100)
  ggsave(
    file.path(
      "Figures/model_results",
      paste0("rq2_", model_name, "_ppcheck.png")
    ),
    ppc_now, width = 7, height = 5, dpi = 300
  )
}

# Fixed effects from the full model, on log-odds and odds-ratio scales.
full_fixef <- as.data.frame(fixef(fit_rq2_full, probs = c(0.025, 0.975))) %>%
  rownames_to_column("term") %>%
  rename(
    estimate_log_odds = Estimate,
    se_log_odds = Est.Error,
    lower_log_odds = Q2.5,
    upper_log_odds = Q97.5
  ) %>%
  mutate(
    odds_ratio = exp(estimate_log_odds),
    odds_ratio_lower = exp(lower_log_odds),
    odds_ratio_upper = exp(upper_log_odds)
  )

write_csv(full_fixef, "Tables/rq2_full_model_coefficients.csv")

# NUTS diagnostics for all four primary models.
nuts_diagnostics <- bind_rows(lapply(names(rq2_fits), function(model_name) {
  np <- nuts_params(rq2_fits[[model_name]])
  tibble(
    model = model_name,
    divergent_transitions = sum(np$Parameter == "divergent__" & np$Value == 1),
    max_treedepth_hits = sum(np$Parameter == "treedepth__" & np$Value >= 15)
  )
}))

print(nuts_diagnostics)
write_csv(nuts_diagnostics, "Outputs/model_audits/rq2_nuts_diagnostics.csv")


# 7. Population-level predicted probabilities and contrasts ---------------
# These are average standardized predictions with country random effects set to zero. 
# They describe an average country and retain the observed values of all other covariates.

summarise_draw_vector <- function(x) {
  tibble(
    estimate = mean(x),
    lower_95 = unname(stats::quantile(x, 0.025)),
    upper_95 = unname(stats::quantile(x, 0.975)),
    posterior_prob_positive = mean(x > 0)
  )
}

average_prediction_draws <- function(fit, newdata) {
  rowMeans(
    posterior_epred(
      fit,
      newdata = newdata,
      re_formula = NA
    )
  )
}

binary_probability_contrast <- function(fit, data, variable, group) {
  data_0 <- data
  data_1 <- data
  data_0[[variable]] <- 0L
  data_1[[variable]] <- 1L

  p0 <- average_prediction_draws(fit, data_0)
  p1 <- average_prediction_draws(fit, data_1)

  bind_rows(
    summarise_draw_vector(p0) %>%
      mutate(contrast = "absent", posterior_prob_positive = NA_real_),
    summarise_draw_vector(p1) %>%
      mutate(contrast = "present", posterior_prob_positive = NA_real_),
    summarise_draw_vector(p1 - p0) %>%
      mutate(contrast = "present minus absent")
  ) %>%
    mutate(group = group, variable = variable, .before = 1)
}

pred_binary <- bind_rows(lapply(
  c(mechanism_vars, result_vars),
  function(v) binary_probability_contrast(
    fit_rq2_full,
    df_bfp_models,
    v,
    ifelse(v %in% mechanism_vars, "Mechanism", "Result")
  )
))

source_levels <- levels(df_bfp_models$source_group)
source_prediction_draws <- lapply(source_levels, function(source_level) {
  newdata <- df_bfp_models
  newdata$source_group <- factor(
    source_level,
    levels = source_levels
  )
  average_prediction_draws(fit_rq2_full, newdata)
})
names(source_prediction_draws) <- source_levels

pred_source_levels <- bind_rows(lapply(source_levels, function(source_level) {
  summarise_draw_vector(source_prediction_draws[[source_level]]) %>%
    mutate(
      group = "Source",
      variable = "source_group",
      contrast = source_level,
      posterior_prob_positive = NA_real_,
      .before = 1
    )
}))

pred_source_differences <- bind_rows(lapply(
  setdiff(source_levels, "public"),
  function(source_level) {
    difference_draws <-
      source_prediction_draws[[source_level]] - source_prediction_draws[["public"]]

    summarise_draw_vector(difference_draws) %>%
      mutate(
        group = "Source",
        variable = "source_group",
        contrast = paste(source_level, "minus public"),
        .before = 1
      )
  }
))

pred_source <- bind_rows(pred_source_levels, pred_source_differences)

write_csv(pred_binary, "Tables/rq2_adjusted_binary_probabilities.csv")
write_csv(pred_source, "Tables/rq2_adjusted_source_probabilities.csv")

# Combined adjusted-probability figure for RQ2
rq2_binary_plot <- read_csv(
  "Tables/rq2_adjusted_binary_probabilities.csv",
  show_col_types = FALSE
) %>%
  filter(contrast == "present") %>%
  mutate(
    category = case_when(
      variable == "grant" ~ "Grant",
      variable == "debt_equity" ~ "Debt/equity",
      variable == "risk" ~ "Risk",
      variable == "fiscal" ~ "Fiscal",
      variable == "market" ~ "Market",
      variable == "regulatory" ~ "Regulatory",
      variable == "generate" ~ "Generate",
      variable == "realign" ~ "Realign",
      variable == "avoid" ~ "Avoid",
      variable == "deliver" ~ "Deliver",
      TRUE ~ variable
    ),
    factor_group = recode(
      group,
      "Mechanism" = "Mechanism",
      "Result" = "Expected result"
    ),
    category = factor(
      category,
      levels = c(
        "Grant", "Debt/equity", "Risk", "Fiscal",
        "Market", "Regulatory",
        "Generate", "Realign", "Avoid", "Deliver"
      )
    )
  ) %>%
  select(
    factor_group, category,
    estimate, lower_95, upper_95
  )

rq2_source_plot <- read_csv(
  "Tables/rq2_adjusted_source_probabilities.csv",
  show_col_types = FALSE
) %>%
  filter(contrast %in% c("public", "private", "blended")) %>%
  mutate(
    factor_group = "Financing source",
    category = case_when(
      contrast == "public" ~ "Public",
      contrast == "private" ~ "Private",
      contrast == "blended" ~ "Blended",
      TRUE ~ contrast
    ),
    category = factor(
      category,
      levels = c("Public", "Private", "Blended")
    )
  ) %>%
  select(
    factor_group, category,
    estimate, lower_95, upper_95
  )

rq2_plot_data <- bind_rows(
  rq2_source_plot,
  rq2_binary_plot
) %>%
  mutate(
    factor_group = factor(
      factor_group,
      levels = c("Financing source", "Mechanism", "Expected result")
    )
  )

# Exclude the risk category from the main figure because it is based on
# only three observations. It can be reported in the supplementary table.
rq2_plot_data_main <- rq2_plot_data %>%
  filter(category != "Risk")

overall_implementation_rate <- mean(df_bfp_models$implemented)

p_rq2_combined <- ggplot(
  rq2_plot_data_main,
  aes(
    x = category,
    y = estimate,
    ymin = lower_95,
    ymax = upper_95
  )
) +
  geom_pointrange(
    linewidth = 0.35,
    size = 0.65,
    colour = "#2166AC"
  ) +
  geom_hline(
    yintercept = overall_implementation_rate,
    linetype = "dashed",
    linewidth = 0.35,
    colour = "grey50"
  ) +
  facet_wrap(
    ~ factor_group,
    scales = "free_x",
    nrow = 1
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.2),
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  labs(
    title = "Adjusted predicted probabilities of implementation, by instrument types",
    x = NULL,
    y = "Adjusted probability of implementation",
    caption = stringr::str_wrap(
      paste(
      "Note:Points show posterior mean population-level predicted probabilities and 
      vertical bars show 95% credible intervals from the full Bayesian multilevel model. 
      Predictions account for the remaining covariates and country-level clustering. 
      The dashed line indicates the overall observed implementation proportion. 
      The risk mechanism is excluded because it is based on three observations and is reported in Appendix."
    ), width = 190)
  ) +
  theme_minimal(base_size = 11) +
  theme(
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    strip.text = element_text(face = "bold"),
    plot.caption = element_text(
      hjust = 0,
      size = 8.5,
      colour = "grey30"
    ),
    legend.position = "none"
  )

print(p_rq2_combined)

ggsave(
  "Figures/model_results/rq2_combined_adjusted_probabilities.png",
  p_rq2_combined,
  width = 11,
  height = 5.5,
  dpi = 300
)



# Adjusted probability curve for BFP age. This is an associational contrast;
# it must not be described as instrument-specific implementation duration.
bfp_age_values <- seq(
  min(df_bfp_models$years_since_bfp),
  max(df_bfp_models$years_since_bfp),
  by = 1
)

pred_bfp_age <- bind_rows(lapply(bfp_age_values, function(age_value) {
  newdata <- df_bfp_models
  newdata$years_since_bfp_z <- (age_value - bfp_age_mean) / bfp_age_sd
  probability_draws <- average_prediction_draws(fit_rq2_full, newdata)

  summarise_draw_vector(probability_draws) %>%
    transmute(
      years_since_bfp = age_value,
      predicted_probability = estimate,
      lower_95,
      upper_95
    )
}))

write_csv(pred_bfp_age, "Tables/rq2_adjusted_bfp_age_probabilities.csv")

p_pred_bfp_age <- ggplot(
  pred_bfp_age,
  aes(x = years_since_bfp, y = predicted_probability)
) +
  geom_ribbon(
    aes(ymin = lower_95, ymax = upper_95),
    fill = "#9ECAE1", alpha = 0.45
  ) +
  geom_line(color = "#08519C", linewidth = 0.9) +
  scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    x = "Years since publication of the BFP",
    y = "Adjusted probability of reported implementation activity",
    caption = "Population-level predictions for an average country; ribbon is the 95% credible interval."
  ) +
  theme_minimal()

print(p_pred_bfp_age)
ggsave(
  "Figures/model_results/rq2_adjusted_bfp_age_probabilities.png",
  p_pred_bfp_age, width = 7, height = 5, dpi = 300
)


# 8. Blockwise predictive comparison --------------------------------------
# PSIS-LOO here evaluates prediction for another intervention while retaining
# the fitted country structure. All models use exactly the same rows.

loo_baseline <- loo(fit_rq2_baseline, moment_match = TRUE)
loo_context <- loo(fit_rq2_context, moment_match = TRUE)
loo_design <- loo(fit_rq2_design, moment_match = TRUE)
loo_full <- loo(fit_rq2_full, moment_match = TRUE)

loo_comparison <- loo_compare(list(
  baseline = loo_baseline,
  context = loo_context,
  design = loo_design,
  full = loo_full
))

loo_comparison_table <- as.data.frame(loo_comparison) %>%
  rownames_to_column("model")

print(loo_comparison_table)
write_csv(loo_comparison_table, "Tables/rq2_loo_model_comparison.csv")

bayes_r2_table <- bind_rows(lapply(names(rq2_fits), function(model_name) {
  r2_draws <- as.numeric(bayes_R2(rq2_fits[[model_name]], summary = FALSE))
  summarise_draw_vector(r2_draws) %>%
    transmute(
      model = model_name,
      bayes_r2 = estimate,
      lower_95,
      upper_95
    )
}))

write_csv(bayes_r2_table, "Tables/rq2_bayes_r2.csv")

# Pareto-k values are summarized by country to identify observations/countries
# that deserve a targeted sensitivity refit.
pareto_by_country <- tibble(
  row_id = seq_len(nrow(df_bfp_models)),
  country = df_bfp_models$country,
  pareto_k = loo::pareto_k_values(loo_full)
) %>%
  group_by(country) %>%
  summarise(
    n = n(),
    max_pareto_k = max(pareto_k),
    mean_pareto_k = mean(pareto_k),
    n_above_0_7 = sum(pareto_k > 0.7),
    .groups = "drop"
  ) %>%
  arrange(desc(max_pareto_k))

print(pareto_by_country)
write_csv(pareto_by_country, "Outputs/model_audits/rq2_pareto_k_by_country.csv")


# 9. Prespecified sensitivity analyses ------------------------------------

# 9.1 Add region as a fixed effect. Country random intercepts remain because
# region and country capture different levels of systematic variation.
formula_full_region <- update(
  formula_full,
  . ~ . + region
)

fit_rq2_full_region <- fit_rq2_model(
  formula_full_region, df_bfp_models, MODEL_SEED + 5L
)

# 9.2 Permit a simple nonlinear association with BFP age. The squared term is
# deliberately modest because BFP age is identified across only 32 countries
# (and two BFP editions for Thailand).
df_bfp_models <- df_bfp_models %>%
  mutate(years_since_bfp_z_sq = years_since_bfp_z^2)

formula_full_age_quadratic <- bf(
  implemented ~
    source_group +
    grant + debt_equity + risk + fiscal + market + regulatory +
    generate + realign + avoid + deliver +
    years_since_bfp_z + years_since_bfp_z_sq +
    gdppc_bfp_window_z +
    GE_bfp_window_z +
    oda_bfp_window_pc_z +
    (1 | country)
)

fit_rq2_full_age_quadratic <- fit_rq2_model(
  formula_full_age_quadratic, df_bfp_models, MODEL_SEED + 6L
)

saveRDS(
  fit_rq2_full_region,
  "Outputs/model_results/fit_rq2_full_region_sensitivity.rds"
)
saveRDS(
  fit_rq2_full_age_quadratic,
  "Outputs/model_results/fit_rq2_full_age_quadratic_sensitivity.rds"
)

capture.output(
  summary(fit_rq2_full_region),
  file = "Outputs/model_audits/rq2_full_region_sensitivity_summary.txt"
)
capture.output(
  summary(fit_rq2_full_age_quadratic),
  file = "Outputs/model_audits/rq2_full_age_quadratic_sensitivity_summary.txt"
)

sensitivity_loo_comparison <- loo_compare(list(
  main_full = loo_full,
  with_region = loo(fit_rq2_full_region, moment_match = TRUE),
  quadratic_bfp_age = loo(fit_rq2_full_age_quadratic, moment_match = TRUE)
))

write_csv(
  as.data.frame(sensitivity_loo_comparison) %>% rownames_to_column("model"),
  "Tables/rq2_sensitivity_loo_comparison.csv"
)


# 10. Optional: grouped country-level cross-validation --------------------
# This is computationally expensive because it refits each model K times.
# It evaluates generalization to countries not used for model estimation,
# unlike ordinary observation-level PSIS-LOO above.

RUN_GROUPED_COUNTRY_KFOLD <- FALSE

if (RUN_GROUPED_COUNTRY_KFOLD) {
  set.seed(MODEL_SEED + 100L)
  country_fold_id <- loo::kfold_split_grouped(
    K = 8,
    x = df_bfp_models$country
  )

  kfold_baseline <- kfold(fit_rq2_baseline, folds = country_fold_id)
  kfold_context <- kfold(fit_rq2_context, folds = country_fold_id)
  kfold_design <- kfold(fit_rq2_design, folds = country_fold_id)
  kfold_full <- kfold(fit_rq2_full, folds = country_fold_id)

  grouped_kfold_comparison <- loo_compare(list(
    baseline = kfold_baseline,
    context = kfold_context,
    design = kfold_design,
    full = kfold_full
  ))

  print(grouped_kfold_comparison)
  write_csv(
    as.data.frame(grouped_kfold_comparison) %>%
      rownames_to_column("model"),
    "Tables/rq2_grouped_country_kfold_comparison.csv"
  )
}


# 11. Session information and interpretation reminders --------------------

capture.output(
  sessionInfo(),
  file = "Outputs/model_audits/rq2_session_info.txt"
)
