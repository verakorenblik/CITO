#### 1. Preparation ####
## Define name for db
name_db <- "cito_VeraKorenblik.db"

## Load packages
packages <- c(
  "here", # for dynamic coding of directory
  "utils", # for loading data
  "dplyr", # for data wrangling
  "tidyr", # for data wrangling
  "dexter", # for CTT and IRT
  "ggplot2", # for plotting
  "lme4" # for multi-level analysis
)
invisible(lapply(packages, library, character.only = TRUE))

## Load csv's and all helpfiles
source(here::here("load_csvs.R"))

## Source all the helpfiles
source(here::here("define_items.R"))
source(here::here("prepare_db.R"))
wd <- here::here()

## Combine all csv's into one dataframe
data <- dplyr::left_join(responses, person, by = "person_id") %>%
  dplyr::left_join(., information, by = c("booklet_id", "item_id"))

## Create vectors containing items
item_sets <- get_booklet_item_sets(information)
item_sets

## Prepare the dexter database
responses <- responses %>%
  mutate(response = na_if(response, "M"))

# Make the scoring rules file
scoring_rules <- create_scoring_rules(information, responses = c("A", "B", "C"))

# Start new dexter project, or open if already made
db <- start_new_project(scoring_rules, name_db, person_properties = list(gender = "unknown"))
db <- open_project(name_db)
str(db)

# Create booklets and add to db
responses_wide_1 <- responses %>%
  filter(booklet_id == 1)%>%
  select(person_id, item_id, response) %>%
  pivot_wider(
    names_from = item_id,
    values_from = response
  ) %>%
  select(person_id, 
         matches("^schuur|^helpdesk|^hondlijn|^strandv"))

responses_wide_2 <- responses %>%
  filter(booklet_id == 2)%>%
  select(person_id, item_id, response) %>%
  pivot_wider(
    names_from = item_id,
    values_from = response
  ) %>% 
  select(person_id, 
         matches("^schuur_|^orderwerk_|^orderverw|^poezenm_|^strandv_"))

add_booklet(db, responses_wide_1, "booklet_1",auto_add_unknown_rules = TRUE)
add_booklet(db, responses_wide_2, "booklet_2",auto_add_unknown_rules = TRUE)

# Add item and person properties
add_item_properties(db, information[, c("item_id", "domain", "year", "text")])
add_person_properties(db, person[, c("person_id", "gender")])

# Quick Inspection
get_booklets(db)
head(get_items(db))
get_persons(db) |>
  glimpse()


#### 2. General statistics ####
# Information about booklets
information %>%
  group_by(text, booklet_id) %>%
  summarise(n_items = n()) # Items per text x booklet

information %>%
  group_by(domain, booklet_id) %>%
  summarise(n_domain = n()) # Domain x booklet

information %>%
  group_by(text, domain, booklet_id) %>%
  summarise(n_domain = n()) %>%
  arrange(booklet_id) # Text x domain x booklet

# Information about population
data %>%
  group_by(booklet_id, gender) %>%
  summarise(b_persons = n_distinct(person_id)) # Men vs women

# Distribution of total scores per person x booklet
person_scores <- data %>%
  group_by(person_id, booklet_id, gender) %>%
  summarise(
    n_correct = sum(item_score, na.rm = TRUE),
    n_items = n(),
    .groups = "drop"
  )

dist.plot <- ggplot(
  person_scores,
  aes(
    x = n_correct,
    color = factor(booklet_id)
  )
) +
  geom_density(adjust = 1) +
  labs(
    x = "Aantal vragen goed",
    y = "Frequentie",
    color = "Toets"
  ) +
  theme_minimal(base_size = 16)
dist.plot

ggsave(paste0(wd, "/density_plot.svg"), dist.plot, width = 25, height = 15, units = "cm")

person_scores %>%
  mutate(
    voldoende = n_correct >= 20
  ) %>%
  group_by(booklet_id) %>%
  summarise(perc_voldoende = mean(voldoende) * 100)

#### 3. Test and item analysis (TIA) ####
tt <- tia_tables(db)

# Cronbach's alpha, discriminatory potential (rit and rir), spread
tt$booklets
tt$items %>%
  arrange(rir) # Check if some items are exceptionally low

tt$items %>%
  dplyr::group_by(booklet_id) %>%
  dplyr::summarise(
    min_rir = min(rir, na.rm = TRUE),
    max_rir = max(rir, na.rm = TRUE),
    min_rit = min(rit, na.rm = TRUE),
    max_rit = max(rit, na.rm = TRUE),
    .groups = "drop"
  )

# Mean test scores per booklet
tt$booklets %>%
  dplyr::select(booklet_id, mean_booklet_score) %>%
  dplyr::rename(mean_score = mean_booklet_score)

# Create and check distractor plots
for (it in item_sets$items) {
  print(distractor_plot(db, it))
}

#### 4. Item Response Theory (IRT) ####
## Difficulty parameters (beta) 
parms <- fit_enorm(db, method = "CML") # Using CML, reduces to Rasch model for dichotomous items since all item scores are 0 or 1
head(coef(parms))
parm_gibbs <- fit_enorm(db, method = "Bayes") # and using Bayes
head(coef(parm_gibbs))

# Add variables to coefficient table, used for stats and plotting later
gibbs <- coef(parm_gibbs) %>%
  dplyr::select(item_id, mean_beta) %>%
  dplyr::left_join(
    get_items(db) %>% dplyr::select(item_id, text, domain),
    by = "item_id"
  )

# Compare mean betas per BOOKLET (summarise over the items). PLEASE NOTE: this includes betas over shared items
b1 <- gibbs %>%
  filter(item_id %in% item_sets$items_1) %>%
  select(mean_beta)
b2 <- gibbs %>%
  filter(item_id %in% item_sets$items_2) %>%
  select(mean_beta)
t.test(b1, b2) # do not differ significantly in difficulty

# Compare mean betas per item domains -> grammar items are more difficult
b_grammar <- gibbs %>%
  filter(domain == "Grammatica") %>%
  pull(mean_beta)
b_interpretation <- gibbs %>%
  filter(domain == "Interpretatie") %>%
  pull(mean_beta)
t.test(b_grammar, b_interpretation) # do not differ significantly in difficulty

domain_means <- gibbs %>%
  group_by(domain) %>%
  summarise(mean_beta = mean(mean_beta), .groups = "drop")
domain_means # used for plotting later

# Inspect mean betas per text types
text_means <- gibbs %>%
  group_by(text) %>%
  summarise(mean_beta = mean(mean_beta), .groups = "drop")
text_means # used for plotting later

# Compare mean betas per differing items
b1_unique <- gibbs %>%
  filter(item_id %in% item_sets$items_1_only) %>%
  select(mean_beta)
b2_unique <- gibbs %>%
  filter(item_id %in% item_sets$items_2_only) %>%
  select(mean_beta)
t.test(b1_unique, b2_unique) # do not differ significantly in difficulty

# Create overview blot of beta's per text, colored by domain
# labels <- setNames(
#   paste0(text_means$text, "\n(", round(text_means$mean_beta, 2), ")"),
#   text_means$text
# )

# Plot: note that these beta's ignoring theta
beta.plot <- ggplot(gibbs, aes(x = text, y = mean_beta)) +
  geom_boxplot(outlier.shape = NA, fill = "grey90") +
  geom_jitter(aes(color = domain),
    width = 0.15,
    alpha = 0.7,
    size = 2
  ) +
  geom_hline(
    data = domain_means,
    aes(yintercept = mean_beta, color = domain),
    linetype = "dashed",
    linewidth = 0.8,
    show.legend = FALSE
  ) +
  theme_minimal(base_size = 14) +
  labs(
    # title = "Item moeilijkheid per tekst type (items gekleurd door domein)",
    y = "Moeilijkheid (beta)",
    x = "Tekst type"
  )
# scale_x_discrete(labels = labels)
beta.plot
ggsave(paste0(wd, "/beta_plot.svg"), beta.plot, width = 33, height = 15, units = "cm")

## Ability distributions: plausible values of (theta)
npv <- 5
pv <- plausible_values(db, parm_gibbs, nPV = npv)
plot(density(pv$PV1), bty = "l")
pv <- merge(pv, get_persons(db))

# Compare males versus females --> small but significantly higher plausible ability of males > females
for (i in 1:npv) {
  print(i)
  form <- as.formula(paste0("PV", i, "~ gender"))
  print(t.test(form, data = pv))
  boxplot(form, data = pv)
} # Check if they are similar amongst different PV's

## Differential item functioning (DIF) 
# DIF occurs when items that are intended to measure a trait are unfair, favoring one group over another
dd <- DIF(db, "gender")
print(dd)
plot(dd) # --> evidence for DIF

## Profile plot. Compare males and females with similar test scores on each domain
svg(file.path(wd, "/profile_plot.svg"),
  width = 33.87 / 2.54,
  height = 19.05 / 2.54
)

profile_plot(db, item_property = "domain", covariate = "gender", predicate = booklet_id == "booklet_1"|"booklet_2")

dev.off()

# Individuals from booklet 1
profile_plot(db,
  item_property = "domain",
  covariate = "gender",
  predicate = booklet_id == "booklet_1"
)

# Individuals from booklet 2
profile_plot(db,
  item_property = "domain",
  covariate = "gender",
  predicate = booklet_id == "booklet_2"
)

# Check if there are individual differences in abilities
dexter::individual_differences(db)

## Estimate ability, "best" guess for theta. Can be used later for calibrating scores
ability_tables(parms)
abl <- ability(db, parms, method = "MLE") %>%
  dplyr::left_join(person %>% select(person_id, gender),
    by = "person_id"
  )

#### 5. Multilevel modeling ####
# To corroborate the IRT findings
resp <- dplyr::left_join(responses, person, by = "person_id") %>%
  dplyr::left_join(., information, by = c("booklet_id", "item_id"))
resp$booklet_id <- factor(resp$booklet_id)
str(resp)

m_item <- glmer(
  item_score ~ gender * domain + booklet_id + (1 | person_id) + (1 | item_id) + (1 | text),
  data = resp,
  family = binomial
)
summary(m_item)

# Close db
close_project(db)
sessionInfo()
