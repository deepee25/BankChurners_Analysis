# ============================================================
# Bank Churners: Customer Segmentation & Churn Analysis
# Dataset: BankChurners.csv (10,127 credit card customers)
# Methods: Feature Engineering + K-Means Clustering
# ============================================================

library(dplyr)
library(ggplot2)
library(cluster)
# library(factoextra)
library(scales)

# ── 1. LOAD DATA ─────────────────────────────────────────────
df <- read.csv("BankChurners.csv", stringsAsFactors = FALSE)

# Drop Naive Bayes classifier columns (leakage columns added by source)
df <- df %>%
  select(-starts_with("Naive_Bayes"))

cat("Dataset dimensions:", nrow(df), "rows x", ncol(df), "columns\n")
cat("Churn rate:", round(mean(df$Attrition_Flag == "Attrited Customer") * 100, 1), "%\n\n")


# ── 2. FEATURE ENGINEERING (10+ behavior-based features) ─────

df <- df %>%
  mutate(
    # 1. Monthly Spend: average spend per month as a customer
    monthly_spend = Total_Trans_Amt / Months_on_book,

    # 2. Revolving Dependency: balance carried relative to total spend
    #    (high = customer relies on credit; different from utilization)
    revolving_dependency = Total_Revolving_Bal / (Total_Trans_Amt + 1),

    # 3. Utilization: average card utilization ratio
    utilization = Avg_Utilization_Ratio,

    # 4. Tenure: months as a customer (proxy for loyalty)
    tenure = Months_on_book,

    # 5. Transaction Frequency: transactions per month
    txn_frequency = Total_Trans_Ct / Months_on_book,

    # 6. Avg Transaction Value: spend per transaction
    avg_txn_value = ifelse(Total_Trans_Ct > 0,
                           Total_Trans_Amt / Total_Trans_Ct, 0),

    # 7. Inactivity Rate: share of last 12 months inactive
    inactivity_rate = Months_Inactive_12_mon / 12,

    # 8. Spend Growth (Q4 vs Q1 change in transaction amount)
    spend_growth = Total_Amt_Chng_Q4_Q1,

    # 9. Transaction Count Growth (Q4 vs Q1)
    txn_growth = Total_Ct_Chng_Q4_Q1,

    # 10. Contact-to-Transaction Ratio: contacts relative to activity
    #     (high = potential dissatisfaction or issue-driven calls)
    contact_to_txn_ratio = Contacts_Count_12_mon / (Total_Trans_Ct + 1),

    # 11. Relationship Breadth: number of products held
    relationship_breadth = Total_Relationship_Count,

    # 12. Contact Intensity: contacts per month in last year
    contact_intensity = Contacts_Count_12_mon / 12,

    # Binary churn label
    churned = ifelse(Attrition_Flag == "Attrited Customer", 1, 0)
  )

cat("Engineered features (12 total):\n")
engineered_cols <- c("monthly_spend","revolving_dependency","utilization","tenure",
                     "txn_frequency","avg_txn_value","inactivity_rate",
                     "spend_growth","txn_growth","contact_to_txn_ratio",
                     "relationship_breadth","contact_intensity")
print(engineered_cols)


# ── 3. PREPARE CLUSTERING MATRIX ─────────────────────────────

cluster_features <- df %>%
  select(all_of(engineered_cols))

# Scale all features (K-means is distance-based)
cluster_scaled <- scale(cluster_features)


# ── 4. DETERMINE OPTIMAL K (Elbow Method) ────────────────────

set.seed(42)
wss <- sapply(2:9, function(k) {
  kmeans(cluster_scaled, centers = k, nstart = 25, iter.max = 100)$tot.withinss
})

elbow_df <- data.frame(k = 2:9, wss = wss)

p0 <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(size = 3, color = "steelblue") +
  labs(title = "Elbow Method — Optimal Number of Clusters",
       x = "Number of Clusters (K)", y = "Total Within-Cluster SS") +
  theme_minimal()

print(p0)
ggsave("elbow_plot.png", plot = p0, width = 7, height = 4, dpi = 150)
cat("\nElbow plot saved: elbow_plot.png\n")


# ── 5. FIT K-MEANS (K = 3) ───────────────────────────────────

set.seed(42)
km <- kmeans(cluster_scaled, centers = 3, nstart = 50, iter.max = 200)
df$segment <- as.factor(km$cluster)

cat("\nCluster sizes:\n")
print(table(df$segment))


# ── 6. SEGMENT PROFILES ──────────────────────────────────────

segment_profiles <- df %>%
  group_by(segment) %>%
  summarise(
    n_customers       = n(),
    pct_customers     = round(n() / nrow(df) * 100, 1),
    churn_rate        = round(mean(churned) * 100, 1),
    avg_monthly_spend = round(mean(monthly_spend), 1),
    avg_utilization   = round(mean(utilization), 3),
    avg_txn_freq      = round(mean(txn_frequency), 2),
    avg_inactivity    = round(mean(inactivity_rate), 3),
    avg_tenure_mo     = round(mean(tenure), 1),
    avg_credit_limit  = round(mean(Credit_Limit), 0),
    avg_spend_growth  = round(mean(spend_growth), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(churn_rate))

cat("\n── Segment Profiles (sorted by churn rate) ──\n")
print(as.data.frame(segment_profiles))


# ── 7. LABEL SEGMENTS ────────────────────────────────────────
# Assign descriptive labels based on profile characteristics

label_map <- segment_profiles %>%
  mutate(label = case_when(
    avg_monthly_spend == max(avg_monthly_spend) ~ "High-Value Active",
    avg_utilization   == max(avg_utilization)   ~ "Revolving Heavy Users",
    TRUE                                        ~ "Stable Moderate Users"
  )) %>%
  select(segment, label)

df <- df %>% left_join(label_map, by = "segment")

cat("\nSegment Labels:\n")
print(label_map)


# ── 8. KEY FINDING: HIGH-VALUE SEGMENT ───────────────────────

high_value <- df %>% filter(label == "High-Value Active")
total_churners <- sum(df$churned)
hv_churners <- sum(high_value$churned)

cat("\n── Key Finding ──\n")
cat(sprintf("High-Value Active segment: %d customers (%.1f%% of total)\n",
            nrow(high_value), nrow(high_value) / nrow(df) * 100))
cat(sprintf("Their churn rate: %.1f%%\n",
            mean(high_value$churned) * 100))


# ── 9. VISUALISATIONS ────────────────────────────────────────

# 9a. Churn rate by segment
p1 <- df %>%
  group_by(label) %>%
  summarise(churn_rate = mean(churned) * 100, .groups = "drop") %>%
  ggplot(aes(x = reorder(label, churn_rate), y = churn_rate, fill = churn_rate)) +
  geom_col() +
  coord_flip() +
  scale_fill_gradient(low = "steelblue", high = "firebrick") +
  labs(title = "Churn Rate by Customer Segment",
       x = NULL, y = "Churn Rate (%)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)
ggsave("churn_by_segment.png", plot = p1, width = 8, height = 4, dpi = 150)

# 9b. Segment size vs churn rate (bubble chart)
p2 <- df %>%
  group_by(label) %>%
  summarise(n = n(), churn_rate = mean(churned) * 100,
            avg_spend = mean(monthly_spend), .groups = "drop") %>%
  ggplot(aes(x = avg_spend, y = churn_rate, size = n, color = label)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(5, 20), name = "Customers") +
  labs(title = "Segment Map: Monthly Spend vs Churn Rate",
       x = "Avg Monthly Spend ($)", y = "Churn Rate (%)", color = "Segment") +
  theme_minimal()

print(p2)
ggsave("segment_map.png", plot = p2, width = 9, height = 5, dpi = 150)

cat("\nCharts saved: churn_by_segment.png, segment_map.png\n")


# ── 10. EXPORT FOR TABLEAU ───────────────────────────────────

tableau_export <- df %>%
  select(CLIENTNUM, Attrition_Flag, Customer_Age, Gender, Income_Category,
         Card_Category, Credit_Limit, Total_Trans_Amt, Avg_Utilization_Ratio,
         Months_on_book, Months_Inactive_12_mon,
         all_of(engineered_cols), segment, label, churned)

write.csv(tableau_export, "BankChurners_Segmented.csv", row.names = FALSE)
cat("Tableau-ready export saved: BankChurners_Segmented.csv\n")


# ── 11. RETENTION RECOMMENDATIONS ────────────────────────────

cat("\n", strrep("=", 60), "\n")
cat("RETENTION RECOMMENDATIONS\n")
cat(strrep("=", 60), "\n\n")

cat("Segment: Revolving Heavy Users\n")
cat("  Profile : High utilization, low open-to-buy; financially stressed\n")
cat("  Actions :\n")
cat("    • Offer credit limit review or balance consolidation\n")
cat("    • Financial wellness tools to improve perceived value of the relationship\n\n")

cat("Segment: High-Value Active\n")
cat("  Profile : High spend, low churn risk — protect and grow\n")
cat("  Actions :\n")
cat("    • Premium rewards tier upgrade or concierge benefit\n")
cat("    • Early access to new card features to deepen loyalty\n\n")

cat(strrep("=", 60), "\n")
cat("Analysis complete.\n")
