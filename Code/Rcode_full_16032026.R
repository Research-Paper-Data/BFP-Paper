# R code for the paper: Global implementation of biodiversity finance plans


##------Description--------:  
# RQs：What instruments are included in BFPs and what are implemented? 
# What factors influence the implementation of instruments in BFP?



# Section A: Prep Steps ---------------------------

# 1. Setup and load data----


# Load (and install if missing) required packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, dplyr, tidyr, janitor, stringr, ggplot2, patchwork, tibble, e1071, cluster, factoextra, ggplot2, reshape2, scales)


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
    status_num
  )


# 3. Clean & Process `catalogue`----

# Define categories
type_mechanism <- c("grant", "debt_equity", "risk", "fiscal", "market", "regulatory")
type_result <- c("generate", "realign", "avoid", "deliver")
type_source <- c("private", "public")
type_all <- c(type_mechanism, type_result, type_source)

catalogue2 <- catalogue %>%
  select(index, solution_type, level, all_of(type_all)) %>%
  mutate(across(all_of(type_all), ~ .x == "Yes"))

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

# Plot
p_country_type_bfp <- ggplot(country_type_bfp, aes(x = type, y = country, fill = pct_type)) +
  geom_tile(color = "white", linewidth = 0.2) +
  facet_grid(
    region ~ group,
    scales = "free",
    space  = "free",
    switch = "y"
  ) +
  scale_fill_gradient(
    low = "grey90",
    high = "#6baed6",
    limits = c(0, 100),
    breaks = c(0, 25, 50, 75, 100),
    na.value = "white",
    name = "% tagged"
  ) +
  labs(
    title = "Instrument Type Distribution, by Country BFP (% of instrument numbers in BFP)",
    x = "Type",
    y = "Country",
    caption = "Multi-tag allowed; Total within a group can exceed 100%."
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.placement = "outside",
    strip.background = element_blank(),
    panel.spacing.y = unit(0.7, "lines")   # increase space between regional groups
  )

print(p_country_type_bfp)

ggsave(
  filename = "Figures/Figure 2_country_type_bfp.png",
  plot = p_country_type_bfp,
  width = 10,
  height = 9,
  dpi = 300
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
  labs(
    title = "Implementation by region: In-BFP instruments vs Post-BFP implemented",
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



# Section B: Inmport and Pre-process data for RQ2 --------

# We prepare data for two stages: 
# Stage-1: BFP conversion (binary)--> Among in-BFP instruments, what factor predict implementation?
# Stage-2: Implementation type multinomial --> how countrycontext shape implementation of certain types of instrument


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
    )
  )

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
                                     NA_integer_)
  )

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
years  <- 20120:2024

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

#View(oda_bio_recipient_year)      

# save
dir.create("Data/External", showWarnings = FALSE)
write_csv(oda_bio_recipient_year, "Data/External/oda_bio_recipient_year.csv")


# *2.4 Build a country–year panel & derive BFP window [-2,+2] co-variates----

# BFP-time capacity/governance/donor = average over years [bfp_year-2, bfp_year+2] (truncate to 2010 lower bound).
# Convert ODA to per capita for comparability.

# ---- Country–year backbone for the 32 countries 
years <- 2010:2024

cty_list <- biofin_data_joined %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c", warn = TRUE),
         country = trimws(as.character(country))) %>%
  filter(!is.na(iso3c)) %>%
  group_by(iso3c) %>%
  summarise(
    country = dplyr::first(country),
    region  = dplyr::first(region),
    .groups = "drop"
  )

panel <- tidyr::expand_grid(iso3c = cty_list$iso3c, year = years) %>%
  left_join(cty_list, by = "iso3c", relationship = "many-to-one")  # safe join


# Merge WDI, WGI, ODA 
# Expect:
#   wb_panel: iso3c, year, gdppc, tax_gdp, pop
#   wgi_long: iso3c, year, GE, RQ, PV
#   oda_bio_recipient_year: iso3c, year, oda_bio_weighted_usd 

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
# *(Note that Thailand has two BPF, which might casue problems

# BFP time value: average of (bfp_year−2 … bfp_year+2)-----replacing "pre" with "bfp_window"
bfp_window_cap <- panel %>%
  inner_join(bfp_years, by = "iso3c") %>%
  mutate(
    year_lower     = pmax(2010L, bfp_year - 2L),
    year_upper     = pmin(2024L, bfp_year + 2L),
    in_bfp_window  = year >= year_lower & year <= year_upper
  ) %>%
  filter(in_bfp_window) %>%
  group_by(iso3c) %>%
  summarise(
    gdppc_bfp_window  = mean(gdppc,  na.rm = TRUE),
    tax_bfp_window    = mean(tax_gdp, na.rm = TRUE),
    GE_bfp_window     = mean(GE,     na.rm = TRUE),
    RQ_bfp_window     = mean(RQ,     na.rm = TRUE),
    PV_bfp_window     = mean(PV,     na.rm = TRUE),
    oda_bfp_window    = mean(oda_bio_total,     na.rm = TRUE),
    oda_bfp_window_pc = mean(oda_bio_weighted_pc,   na.rm = TRUE),
    .groups = "drop"
  )

View(bfp_window_cap)

# *2.5 save the data---- 
covars_country <- cty_list %>%
  select(country, iso3c, region) %>%
  left_join(bfp_window_cap,   by = "iso3c")

View(covars_country)

dir.create("Outputs", showWarnings = FALSE)
write_csv(covars_country, "Outputs/covars_country.csv")

saveRDS(covars_country, file = "Data/cp.rds")


# Section D Modelling, Visulization and RQ2 -------------------------------

# 1. Set up----

#install cmdstanr directly to make the model run quicker: 
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))

library(cmdstanr)
library(posterior)
library(bayesplot)
color_scheme_set("brightblue")

check_cmdstan_toolchain()
install_cmdstan(cores = 2)

#To check the path to the CmdStan installation and the CmdStan version number：use cmdstan_path() and cmdstan_version():
cmdstan_path()
cmdstan_version()

# Start

library(tidyverse)
library(lme4)

# 2. Read the prepared datasets directly----

biofin_data_joined <- read_csv("Data/biofin_data_joined.csv")  
covars_country <- readRDS("Data/cp.rds")


# 3. Explore: Among in-BFP instruments, what factor predict implementation? ----

#This is identified because the denominator is clear: all BFP-listed instruments 
#Interpretation: factors associated with proposal → implementation within BFP.

# What predicts whether an instrument gets implemented at all?  Level: Binary (yes/no)

# *3.1 explore step 1 (binary: implemented vs not)----

df_stage1 <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%
  mutate(
    implemented = case_when(
      status_num == 0 ~ 0,
      status_num == 1 ~ 1
    )
  ) %>%
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  mutate(
    region = as.factor(region)
  )

df_stage1 <- df_stage1 %>%
  mutate(
    region = relevel(region, ref = "Asia and the Pacific")
  )

# model: BFP conversion (multilevel logit, no instrument tags)

# Helper for z-scoring continuous covariates
z <- function(x) as.numeric(scale(x))

m_stage1_glm <- glm(
  implemented ~
    z(years_since_bfp) +
    z(gdppc_bfp_window) + 
    #z(tax_bfp_window) + # (*note that if include tax_pre, "Indonesia" "Vietnam"   "Cuba"  will be dropped)
    z(GE_bfp_window) + z(RQ_bfp_window) + z(PV_bfp_window) +
    z(oda_bfp_window_pc) +
    region,
  data = df_stage1, family = binomial(link = "logit"))

summary(m_stage1_glm)
#the results suggest years_since_bfp and oda_bfp_window_pc influence the implementations negatively


# Explore step follow-up
    #test HP from interview: generative instruments has higher likelyhood to be implemented

# Holding political capacity, economic capacity, and external resources constant, 
# do generate-type instruments have a higher probability of implementation?

# Using  one clean multilevel logistic regression with only:
# Outcome: implemented (1/0)
# Key predictor: generate (TRUE/FALSE)
# Controls: politics, economy, external resources (your BFP-window covariates)
# Random intercept: country

df_gen_test <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%      # only instruments with a BFP
  mutate(
    implemented = status_num,                    # already coded 0/1
    generate   = as.integer(generate)            # TRUE/FALSE -> 1/0
  ) %>%
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  filter(!is.na(generate))                       # safety

# Run a logistic model
m_generate_simple <- glm(
  implemented ~
    generate +                            # <-- Key variable of interest
    z(gdppc_bfp_window) +
    #z(tax_bfp_window) +
    z(GE_bfp_window) + 
    z(RQ_bfp_window) 
  + z(PV_bfp_window) +
    z(oda_bfp_window_pc)+
    region,
  data   = df_gen_test,
  family = binomial(link = "logit")
)

summary(m_generate_simple)


# test other types 
# Standardize helper
z <- function(x) as.numeric(scale(x))

df_gen_test3 <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%      # only instruments with a BFP
  mutate(
    implemented = status_num,                    # already coded 0/1
    avoid    = as.integer(avoid),
    realign  = as.integer(realign),
    deliver  = as.integer(deliver),
    generate = as.integer(generate),
    private  = as.integer(private),
    public   = as.integer(public)
  ) %>%
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  filter(!is.na(generate))


# Run a logistic model
m_generate_test3 <- glm(
  implemented ~
    avoid + generate + realign + deliver + public + private+             # <-- Key variable of interest
    z(gdppc_bfp_window) +
    z(tax_bfp_window) +
    z(GE_bfp_window) + 
    z(RQ_bfp_window) 
  + z(PV_bfp_window) +
    z(oda_bfp_window_pc)
  ,                         # random intercept
  data   = df_gen_test,
  family = binomial(link = "logit")
)

summary(m_generate_test3)


# Note the follow-up model does not show that generative instruments have significantly higher chance of implementation. 
# This may be caused by the "self-section" factor in making BFP already,
#-- perhaps, generative types of instruments are more likely to be included in BFP, but ofc we ddo not have data to test it



# *3.2 Model 1----
# country context--> what types of instruments get implemented (public/private/blended) 

  # It estimates how country governance, capacity, external financing, regional context, and BIOFIN support,
  # shape implementation of the (public/private/blended) types of biodiversity finance instrument? 

library(brms)
library(rstan)
rstan_options(auto_write = TRUE)

# Build modeling datasets (multinomial: not_impl vs public/private/blended)
# Multinomial outcome across the BFP risk set (not only implemented)

df_stage2 <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%
  mutate(
    implemented = status_num == 1L,
    source_type = case_when(
      !implemented                     ~ "not_impl",
      implemented & public  & !private ~ "impl_public",
      implemented & !public & private  ~ "impl_private",
      implemented & public  & private  ~ "impl_blended",
      TRUE                             ~ NA_character_
    ),
    source_type = factor(
      source_type,
      levels = c("not_impl", "impl_public", "impl_private", "impl_blended")
    )
  ) %>%
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  mutate(
    region = factor(region)
  ) %>%
  filter(!is.na(source_type))

df_stage2 <- df_stage2 %>%
  mutate(
    region = relevel(region, ref = "Asia and the Pacific"),
    years_since_bfp_z   = (years_since_bfp - mean(years_since_bfp, na.rm = TRUE)) / sd(years_since_bfp, na.rm = TRUE),
    gdppc_bfp_window_z  = (gdppc_bfp_window - mean(gdppc_bfp_window, na.rm = TRUE)) / sd(gdppc_bfp_window, na.rm = TRUE),
    tax_bfp_window_z    = (tax_bfp_window - mean(tax_bfp_window, na.rm = TRUE)) / sd(tax_bfp_window, na.rm = TRUE),
    GE_bfp_window_z     = (GE_bfp_window - mean(GE_bfp_window, na.rm = TRUE)) / sd(GE_bfp_window, na.rm = TRUE),
    RQ_bfp_window_z     = (RQ_bfp_window - mean(RQ_bfp_window, na.rm = TRUE)) / sd(RQ_bfp_window, na.rm = TRUE),
    PV_bfp_window_z     = (PV_bfp_window - mean(PV_bfp_window, na.rm = TRUE)) / sd(PV_bfp_window, na.rm = TRUE),
    oda_bfp_window_pc_z = (oda_bfp_window_pc - mean(oda_bfp_window_pc, na.rm = TRUE)) / sd(oda_bfp_window_pc, na.rm = TRUE)
  )

## Runnng Model 1: multinomial ----
#  priors_multinom <- c(
#   prior(normal(0, 1.5), class = "Intercept"),
#    prior(normal(0, 1),class = "b"),
#   prior(exponential(1), class = "sd", group = "country")
# )

# TODO: to get speed up things install cmdstanr directly: https://mc-stan.org/cmdstanr/articles/cmdstanr.html 

form_stage2 <- brmsformula(
  source_type ~
    years_since_bfp_z +
    gdppc_bfp_window_z + 
    # tax_bfp_window_z + #missingness creates a problem
    GE_bfp_window_z + RQ_bfp_window_z +
    PV_bfp_window_z +
    oda_bfp_window_pc_z +
    region # + (1 | country)    not multilevel in this case 
)        

fit_stage2 <- brm(
  formula = form_stage2,
  data    = df_stage2,
  family  = categorical(refcat = "not_impl"),
  # prior   = priors_multinom,
  chains  = parallel::detectCores(logical = FALSE), cores =  parallel::detectCores(logical = FALSE), 
  iter = 2000, warmup = 1000, seed = 123,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  stan_model_args = list(stanc_options = list("O1"))
)

summary(fit_stage2)

conditional_effects(fit_stage2, categorical = TRUE)


## Plot and save ---------------------
library(patchwork)
library(ggplot2)
library(stringr)

# Conditional effects plots
ce_stage2 <- conditional_effects(fit_stage2, categorical = TRUE)

# show all plots in R
plot(ce_stage2)

# create output folders
dir.create("Figures/model_result", recursive = TRUE, showWarnings = FALSE)
dir.create("Figures/model_result_pdf", recursive = TRUE, showWarnings = FALSE)

# extract plots as ggplot objects
ce_plots <- plot(ce_stage2, plot = FALSE)

# if names missing, copy from ce_stage2
if (is.null(names(ce_plots)) || any(names(ce_plots) == "")) {
  names(ce_plots) <- names(ce_stage2)
}

print(names(ce_plots))

# Rename x-axis labels but REMOVE subplot titles
var_labels <- c(
  years_since_bfp_z   = "Years since BFP",
  gdppc_bfp_window_z  = "GDP PPP",
  GE_bfp_window_z     = "Government effectiveness",
  RQ_bfp_window_z     = "Regulatory quality",
  PV_bfp_window_z     = "Political stability",
  oda_bfp_window_pc_z = "Biodiversity ODA per capita"
)

for (nm in names(ce_plots)) {
  base_nm <- str_remove(nm, ":.*$")
  
  if (base_nm %in% names(var_labels)) {
    
    ce_plots[[nm]] <- ce_plots[[nm]] +
      labs(
        title = NULL,                     # remove subplot title
        x = var_labels[[base_nm]],        # keep x-axis label
        y = "Predicted probability"
      )
    
  } else {
    
    ce_plots[[nm]] <- ce_plots[[nm]] +
      labs(
        title = NULL,
        y = "Predicted probability"
      )
    
  }
}

# Save each plot individually

for (nm in names(ce_plots)) {
    safe_nm <- gsub("[^[:alnum:]_]+", "_", nm)
  
  ggsave(
    filename = paste0("Figures/model_result/model1_", safe_nm, ".png"),
    plot = ce_plots[[nm]],
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    filename = paste0("Figures/model_result_pdf/model1_", safe_nm, ".pdf"),
    plot = ce_plots[[nm]],
    width = 7,
    height = 5
  )
}

# Build Panel A (exclude region plot)
region_idx <- grep("region", names(ce_plots), ignore.case = TRUE)

if (length(region_idx) != 1) {
  stop("Could not uniquely identify the region plot in ce_plots.")
}

panelA_idx <- setdiff(seq_along(ce_plots), region_idx)
panelA_plots <- ce_plots[panelA_idx]

print(length(panelA_plots))   # should be 6

# combine into Panel A
p_panelA <- wrap_plots(
  plots = panelA_plots,
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(
    title = "Panel A"
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_panelA)

# save Panel A
ggsave(
  filename = "Figures/model_result/panelA.png",
  plot = p_panelA,
  width = 12,
  height = 6,
  dpi = 300
)

ggsave(
  filename = "Figures/model_result_pdf/model1_panelA.pdf",
  plot = p_panelA,
  width = 12,
  height = 6
)

# Save region plot separately

p_region <- ce_plots[[region_idx]]

print(p_region)

ggsave(
  filename = "Figures/Figure C_region_source_type.png",
  plot = p_region,
  width = 7,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "Figures/model_result_pdf/model1_region.pdf",
  plot = p_region,
  width = 7,
  height = 5
)


# *3.3 Model 2 ----
# country context --> what result types of instruments get implemented

  # It estimates how country governance, capacity, external resources,and regional context shape which result type
  # ("generate", "realign", "avoid", "deliver") of instruments gets implemented.

# Build modelling datasets: Result types
# (multinomial: generate / realign / avoid / deliver)

df_stage3 <- biofin_data_joined %>%
  # Only instruments in the BFP risk set with known BFP year
  filter(in_bfp_flag, !is.na(bfp_year)) %>%
  
  # Keep only implemented instruments (1 = implemented)
  filter(status_num == 1L) %>%
  
  mutate(
    # Collapse the four logical flags into a single categorical result_type
    # Priority rule if multiple are TRUE: generate > realign > avoid > deliver
    result_type = case_when(
      generate ~ "generate",
      realign  ~ "realign",
      avoid    ~ "avoid",
      deliver  ~ "deliver",
      TRUE     ~ NA_character_
    ),
    
    # Set explicit order and baseline; here "avoid" is the reference category
    result_type = factor(
      result_type,
      levels = c("avoid", "generate", "realign", "deliver")
    )
  ) %>%
  
  # Attach country-level covariates
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  
  mutate(
    region = factor(region)
  ) %>%
  
  # Keep only rows with a defined result_type
  filter(!is.na(result_type)) %>%
  
  # Set reference category and standardize continuous predictors
  mutate(
    region = relevel(region, ref = "Asia and the Pacific"),
    years_since_bfp_z   = (years_since_bfp - mean(years_since_bfp, na.rm = TRUE)) / sd(years_since_bfp, na.rm = TRUE),
    gdppc_bfp_window_z  = (gdppc_bfp_window - mean(gdppc_bfp_window, na.rm = TRUE)) / sd(gdppc_bfp_window, na.rm = TRUE),
    tax_bfp_window_z    = (tax_bfp_window - mean(tax_bfp_window, na.rm = TRUE)) / sd(tax_bfp_window, na.rm = TRUE),
    GE_bfp_window_z     = (GE_bfp_window - mean(GE_bfp_window, na.rm = TRUE)) / sd(GE_bfp_window, na.rm = TRUE),
    RQ_bfp_window_z     = (RQ_bfp_window - mean(RQ_bfp_window, na.rm = TRUE)) / sd(RQ_bfp_window, na.rm = TRUE),
    PV_bfp_window_z     = (PV_bfp_window - mean(PV_bfp_window, na.rm = TRUE)) / sd(PV_bfp_window, na.rm = TRUE),
    oda_bfp_window_pc_z = (oda_bfp_window_pc - mean(oda_bfp_window_pc, na.rm = TRUE)) / sd(oda_bfp_window_pc, na.rm = TRUE)
  )

## running Model3: multilevel multinational (result types) ----

form_stage3 <- bf(
  result_type ~    
    years_since_bfp_z +
    gdppc_bfp_window_z + 
    # tax_bfp_window_z + #missingness creates a problem 
    GE_bfp_window_z + 
    RQ_bfp_window_z +
    PV_bfp_window_z +
    oda_bfp_window_pc_z +
    region 
  # + (1 | country)    not multilevel in this case 
)

# Start with default priors (simplest and robust, given the prior issues before)
fit_stage3 <- brm(
  formula = form_stage3,
  data    = df_stage3,
  family  = categorical(refcat = "avoid"),  # baseline result type
  # prior   = priors_multinom,
  chains  = parallel::detectCores(logical = FALSE), cores =  parallel::detectCores(logical = FALSE), 
  iter = 2000, warmup = 1000, seed = 123,
  backend = "cmdstanr",
  control = list(adapt_delta = 0.95, max_treedepth = 12),
  stan_model_args = list(stanc_options = list("O1"))
)

summary(fit_stage3)
conditional_effects(fit_stage3, categorical = TRUE)

## Plot and save ---------------------

library(patchwork)
library(ggplot2)
library(stringr)

# Conditional effects plots
ce_stage3 <- conditional_effects(fit_stage3, categorical = TRUE)

# show all plots in R
plot(ce_stage3)

# extract plots as ggplot objects
ce_plots_stage3 <- plot(ce_stage3, plot = FALSE)

# if names missing, copy from ce_stage3
if (is.null(names(ce_plots_stage3)) || any(names(ce_plots_stage3) == "")) {
  names(ce_plots_stage3) <- names(ce_stage3)
}

# check plot names
print(names(ce_plots_stage3))


# Rename x-axis labels but REMOVE subplot titles
var_labels_stage3 <- c(
  years_since_bfp_z   = "Years since BFP",
  gdppc_bfp_window_z  = "GDP PPP",
  GE_bfp_window_z     = "Government effectiveness",
  RQ_bfp_window_z     = "Regulatory quality",
  PV_bfp_window_z     = "Political stability",
  oda_bfp_window_pc_z = "Biodiversity ODA per capita"
)

for (nm in names(ce_plots_stage3)) {
  
  base_nm <- str_remove(nm, ":.*$")
  
  if (base_nm %in% names(var_labels_stage3)) {
    
    ce_plots_stage3[[nm]] <- ce_plots_stage3[[nm]] +
      labs(
        title = NULL,
        x = var_labels_stage3[[base_nm]],
        y = "Predicted probability"
      )
    
  } else {
    
    ce_plots_stage3[[nm]] <- ce_plots_stage3[[nm]] +
      labs(
        title = NULL,
        y = "Predicted probability"
      )
    
  }
}


# Save each plot individually
for (nm in names(ce_plots_stage3)) {
  
  safe_nm <- gsub("[^[:alnum:]_]+", "_", nm)
  
  ggsave(
    filename = paste0("Figures/model_result/model2_", safe_nm, ".png"),
    plot = ce_plots_stage3[[nm]],
    width = 7,
    height = 5,
    dpi = 300
  )
  
  ggsave(
    filename = paste0("Figures/model_result_pdf/model2_", safe_nm, ".pdf"),
    plot = ce_plots_stage3[[nm]],
    width = 7,
    height = 5
  )
}


# Build Panel B (exclude region plot)

region_idx_stage3 <- grep("region", names(ce_plots_stage3), ignore.case = TRUE)

if (length(region_idx_stage3) != 1) {
  stop("Could not uniquely identify the region plot in ce_plots_stage3.")
}

panelB_idx <- setdiff(seq_along(ce_plots_stage3), region_idx_stage3)
panelB_plots <- ce_plots_stage3[panelB_idx]

# safety check
print(length(panelB_plots))   # should be 6
print(names(panelB_plots))

# combine into Panel B
p_panelB <- wrap_plots(
  plots = panelB_plots,
  ncol = 3,
  guides = "collect"
) +
  plot_annotation(
    title = "Panel B"
  ) &
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  )

print(p_panelB)

# save Panel B
ggsave(
  filename = "Figures/model_result/panelB.png",
  plot = p_panelB,
  width = 13,
  height = 8,
  dpi = 300
)

ggsave(
  filename = "Figures/model_result_pdf/panelB.pdf",
  plot = p_panelB,
  width = 13,
  height = 8
)

# Save region plot separately

p_region_stage3 <- ce_plots_stage3[[region_idx_stage3]]

print(p_region_stage3)

ggsave(
  filename = "Figures/Figure D_region_result_type.png",
  plot = p_region_stage3,
  width = 8,
  height = 5,
  dpi = 300
)

ggsave(
  filename = "Figures/model_result_pdf/model2_region.pdf",
  plot = p_region_stage3,
  width = 8,
  height = 5
)

## Combine panel A and Panel B into Figure 6----
library(patchwork)
library(grid)

# Rebuild clean panel objects WITHOUT panel titles
p_panelA_clean <- wrap_plots(
  plots = panelA_plots,
  ncol = 3,
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

p_panelB_clean <- wrap_plots(
  plots = panelB_plots,
  ncol = 3,
  guides = "collect"
) &
  theme(
    legend.position = "bottom"
  )

# Text rows
g_title <- wrap_elements(
  full = textGrob(
    "Conditional effects of country context on instrument implementation",
    x = 0, hjust = 0,
    gp = gpar(fontsize = 14, fontface = "bold")
  )
)

g_panelA <- wrap_elements(
  full = textGrob(
    "Panel A",
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12, fontface = "bold")
  )
)

g_panelB <- wrap_elements(
  full = textGrob(
    "Panel B",
    x = 0, hjust = 0,
    gp = gpar(fontsize = 12, fontface = "bold")
  )
)

# Wrap panels for safe stacking
p_panelA_wrap <- wrap_elements(full = p_panelA_clean)
p_panelB_wrap <- wrap_elements(full = p_panelB_clean)

# Final figure
p_figure6 <- g_title /
  g_panelA /
  p_panelA_wrap /
  g_panelB /
  p_panelB_wrap +
  plot_layout(heights = c(0.06, 0.05, 1, 0.05, 1))

print(p_figure6)

# Save
dir.create("Figures", showWarnings = FALSE)

ggsave(
  filename = "Figures/Figure 6_model.png",
  plot = p_figure6,
  width = 13,
  height = 16,
  dpi = 300
)

ggsave(
  filename = "Figures/model_result_pdf/Figure 6_model.pdf",
  plot = p_figure6,
  width = 13,
  height = 16
)

# 4. Table----
# Descriptive statistics table for focal countries and regions

library(dplyr)
library(tidyr)
library(readr)

# Create output folder
dir.create("Outputs", recursive = TRUE, showWarnings = FALSE)

# 4.1 Build a country-level summary dataset
#    One row per focal country in the BFP sample

df_country_summary <- biofin_data_joined %>%
  filter(in_bfp_flag, !is.na(bfp_year)) %>%
  distinct(country, iso3c, region, bfp_year, years_since_bfp) %>%
  left_join(covars_country, by = c("country", "iso3c", "region")) %>%
  mutate(
    region = factor(region, levels = region_levels)
  ) %>%
  select(
    country,
    iso3c,
    region,
    bfp_year,
    years_since_bfp,
    gdppc_bfp_window,
    tax_bfp_window,
    GE_bfp_window,
    RQ_bfp_window,
    PV_bfp_window,
    oda_bfp_window,
    oda_bfp_window_pc
  )

# Quick check
print(df_country_summary)
print(n_distinct(df_country_summary$country))

write_csv(df_country_summary, "Outputs/country_summary.csv")

# 4.2. Overall descriptive statistics


desc_overall_wide <- df_country_summary %>%
  summarise(
    across(
      .cols = c(
        years_since_bfp,
        gdppc_bfp_window,
        tax_bfp_window,
        GE_bfp_window,
        RQ_bfp_window,
        PV_bfp_window,
        oda_bfp_window,
        oda_bfp_window_pc
      ),
      .fns = list(
        mean = ~ mean(.x, na.rm = TRUE),
        sd   = ~ sd(.x, na.rm = TRUE),
        min  = ~ min(.x, na.rm = TRUE),
        max  = ~ max(.x, na.rm = TRUE),
        n    = ~ sum(!is.na(.x))
      ),
      .names = "{.col}__{.fn}"
    )
  )

desc_overall <- desc_overall_wide %>%
  pivot_longer(
    cols = everything(),
    names_to = c("variable", "stat"),
    names_sep = "__",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = stat,
    values_from = value
  ) %>%
  mutate(
    variable = recode(
      variable,
      years_since_bfp   = "Years since BFP",
      gdppc_bfp_window  = "GDP per capita (PPP)",
      tax_bfp_window    = "Tax revenue (% GDP)",
      GE_bfp_window     = "Government effectiveness",
      RQ_bfp_window     = "Regulatory quality",
      PV_bfp_window     = "Political stability",
      oda_bfp_window    = "Biodiversity ODA (total)",
      oda_bfp_window_pc = "Biodiversity ODA per capita"
    ),
    mean = round(mean, 2),
    sd   = round(sd, 2),
    min  = round(min, 2),
    max  = round(max, 2),
    n    = as.integer(n)
  ) %>%
  select(variable, mean, sd, min, max, n)

print(desc_overall)

# Save overall table
write_csv(desc_overall, "Outputs/descriptive_statistics_overall.csv")


# 4.3. Descriptive statistics by region
#    Mean values by region, plus number of countries


desc_by_region <- df_country_summary %>%
  group_by(region) %>%
  summarise(
    n_countries = n_distinct(country),
    years_since_bfp   = mean(years_since_bfp, na.rm = TRUE),
    gdppc_bfp_window  = mean(gdppc_bfp_window, na.rm = TRUE),
    tax_bfp_window    = mean(tax_bfp_window, na.rm = TRUE),
    GE_bfp_window     = mean(GE_bfp_window, na.rm = TRUE),
    RQ_bfp_window     = mean(RQ_bfp_window, na.rm = TRUE),
    PV_bfp_window     = mean(PV_bfp_window, na.rm = TRUE),
    oda_bfp_window    = mean(oda_bfp_window, na.rm = TRUE),
    oda_bfp_window_pc = mean(oda_bfp_window_pc, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    years_since_bfp   = round(years_since_bfp, 2),
    gdppc_bfp_window  = round(gdppc_bfp_window, 2),
    tax_bfp_window    = round(tax_bfp_window, 2),
    GE_bfp_window     = round(GE_bfp_window, 2),
    RQ_bfp_window     = round(RQ_bfp_window, 2),
    PV_bfp_window     = round(PV_bfp_window, 2),
    oda_bfp_window    = round(oda_bfp_window, 2),
    oda_bfp_window_pc = round(oda_bfp_window_pc, 2)
  ) %>%
  rename(
    `Region`                    = region,
    `N countries`               = n_countries,
    `Years since BFP`           = years_since_bfp,
    `GDP per capita (PPP)`      = gdppc_bfp_window,
    `Tax revenue (% GDP)`       = tax_bfp_window,
    `Government effectiveness`  = GE_bfp_window,
    `Regulatory quality`        = RQ_bfp_window,
    `Political stability`       = PV_bfp_window,
    `Biodiversity ODA (total)`  = oda_bfp_window,
    `Biodiversity ODA per capita` = oda_bfp_window_pc
  )

print(desc_by_region)

# Save by-region table
write_csv(desc_by_region, "Tables/descriptive_statistics_by_region.csv")

# 4.4. Optional: country list table for transparency

country_list_table <- df_country_summary %>%
  arrange(region, country) %>%
  select(country, iso3c, region, bfp_year, years_since_bfp) %>%
  rename(
    Country = country,
    ISO3C = iso3c,
    Region = region,
    `BFP year` = bfp_year,
    `Years since BFP` = years_since_bfp
  )

print(country_list_table)

write_csv(country_list_table, "Outputs/countries_list.csv")


