# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Author: Derrick Burt
# Title: Online Retailer E-commerce Analysis
# Date Created: 3/17/2024
# Last updated: 3/19/2024
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Install packages (un-comment 10 - 15 to install).

# install.packages( c("tidyverse",
#                     "openxlsx",
#                     "RColorBrewer",
#                     "forcats"
#                     "ggstream",
#                     "ggthemes",
#                     "scales
#                   )
#                 )

# Load packages.
library(forcats) #1.0.0
library(ggthemes) # 5.1.0
library(ggstream) # 0.1.0
library(openxlsx) # 4.2.5.2
library(RColorBrewer) # 1.1-3
library(scales) # 1.3.0
library(tidyverse) # tidyverse-2.0.0 | dplyr-1.1.4  | ggplot2-3.5.0 | 
                   # magrritr-2.0.3  | stringr 1.5.3| tidyr-1.3.1 
                   # lubridate-1.9.3

# ##################################################################
# ------------------------------------------------------------------
# 01: Load, inspect, and clean data.
# ------------------------------------------------------------------
# ##################################################################


# ------------------------------------------------------------
# 01.1: Load the csv files.
# ------------------------------------------------------------

## Check working directory (un-comment to check).
# getwd()

# "Adds to cart" csv file.
adds_to_cart <- read.csv("DataAnalyst_Ecom_data_addsToCart.csv")

# "Session counts" csv file.
session_counts <- read.csv("DataAnalyst_Ecom_data_sessionCounts.csv")


# ------------------------------------------------------------
# 01.2: Check for missing values.
# ------------------------------------------------------------

# Check for missing values in adds_to_cart data.
cat("Missing values in adds_to_cart:\n")
print(colSums(is.na(adds_to_cart)))

# Check for missing values in session_counts data.
cat("\nMissing values in session_counts:\n")
print(colSums(is.na(session_counts)))

### No missing data in either data set ###


# ------------------------------------------------------------
# 01.3: Check for duplicates values.
# ------------------------------------------------------------

# Check for duplicates in adds_to_cart data.
cat("Duplicate rows in adds_to_cart dataframe:\n")
print(adds_to_cart[duplicated(adds_to_cart), ])

# Check for duplicates in session_counts data.
cat("Duplicate rows in session_counts dataframe:\n")
print(session_counts[duplicated(session_counts), ])

### No duplicates in either data set ###


# ------------------------------------------------------------
# 01.4: Check temporal scale & standardize date format.
# ------------------------------------------------------------

## Inspect date range in adds_to_cart.

# Convert dim_year and dim_month columns to "YYYY-MM-DD" date format.
# *** Substitute 01 as day to get date format
adds_to_cart$date <- as.Date(paste(
                                adds_to_cart$dim_year,   # Year
                                adds_to_cart$dim_month,  # Month
                                "01",                    # Day
                                sep = "-"
                              ),
                          format = "%Y-%m-%d"           # ***
                          ) 


# Calculate expected length from first month to last month.
expected_length_adds <- length(seq(
                                min(adds_to_cart$date), # from
                                max(adds_to_cart$date), # to
                                by = "month")           # unit
                              )

# Calculate number of expected months from actual months.
missing_months <- expected_length_adds - n_distinct(adds_to_cart$date)

# Confirm there are no missing months.
if(missing_months == 0) {
  cat(expected_length_adds, "consecutive months in adds_to_cart.\n")
} else {
  cat(missing_months, "missing months in adds_to_cart.\n")
}


## Inspect date range in sessions_counts.

# Convert dimonth column to "YYYY-MM-DD" date format.
session_counts$date <- as.Date(session_counts$dim_date, format = "%m/%d/%y")

# Calculate expected length from first day to last day
expected_length_sessions <- length(
                                seq(
                                  min(session_counts$date),      # from
                                  max(session_counts$date),      # to
                                  by = "day"                     # unit
                                )
                            )

# Calculate number of expected days from actual days
missing_days <- expected_length_sessions - n_distinct(session_counts$date)


# Print number of consecutive days (conditional: no missing days).
if(missing_days == 0) {
  cat(expected_length_sessions, "consecutive days in session_counts.\n")
} else {
  cat(missing_days, "missing days in session_counts.")
}

## Confirm adds_to_cart and session_counts are temporally consistent.

# True if year and month ranges match between adds and sessions.
identical(c(format(min(adds_to_cart$date), "%Y-%m"),    # first month adds
            format(max(adds_to_cart$date), "%Y-%m")),   # last month adds
          c(format(min(session_counts$date), "%Y-%m"), # first month sessions
            format(max(session_counts$date), "%Y-%m"))  # last month sessions
) # Both data sets are temporally consistent


# ------------------------------------------------------------
# 01.5: Inspect Browser and Device in "Session counts".
# ------------------------------------------------------------

# Find and count unique devices. Check for Nulls.
table(session_counts$dim_deviceCategory, useNA = "ifany")

# Sum distinct.
n_distinct(session_counts$dim_deviceCategory) # No nulls, 3 unique devices. 

### desktop: 2672 | mobile: 3013 | tablet: 2049 ###

# Find and count unique devices. Check for Nulls.
table(session_counts$dim_browser, useNA = "ifany")

# Sum distinct.
n_distinct(session_counts$dim_browser) # No nulls, 57 unique devices.

### Clean/collapse like-browsers? (i.e 1 category for "Android) ###
### Many different unknowns. (i.e."error", "anonymous" )

# ------------------------------------------------------------
# 01.6: Inspect, Sessions, transactions, and quantities.
# ------------------------------------------------------------

# Sources for "assumptions"
## https://www.optimizesmart.com/why-google-analytics-show-zero-sessions/

## Anomalies:
### If discrepancies exist b/w sessions, transactions, & quantities (i.e
### transaction w/o a session) the data should be removed and investigated 
### separately from baseline website performance / user experience reporting.
  # a) glitch/error in GA reporting
  # b) browser specific-troubles
  # c) suspicious / user hacked (?)

## Investigate empty sessions
zero_sessions <- session_counts %>% 
  filter(sessions == 0)                # select where sessions == 0.
  
## Investigate for empty sessions with transactions or quantities.
# Assumption: if session = 0, transaction and QTY should == 0.
zero_sessions_wSale <- zero_sessions %>% 
  filter(QTY > 0 | transactions > 0)   # select where transactions or QTY > 0.

## Investigate for issues b/w transaction and QTY
# Assumption: if Transaction = 0, QTY should = 0.
zero_trns_wQty<- session_counts %>% 
  filter(transactions == 0) %>%        # select where transactions == 0.
  filter(QTY > 0)                      # select where QTY > 0.

## Investigate for issues b/w transaction and QTY
# Assumption: if QTY = 0, Transaction should = 0.
zeroQty_wTrns <- session_counts %>% 
  filter(QTY == 0) %>%                 # select where QTY == 0.
  filter(transactions > 0)             # select where transactions > 0.

# Bind removed data into one dataframe.
removed <- rbind(zero_sessions, zero_trns_wQty, zeroQty_wTrns)

# Remove duplicates.
removed <- removed[!duplicated(removed),]

# ------------------------------------------------------------
# 01.7: Clean session counts for aggregation.
# ------------------------------------------------------------

# Filter out session / sales issues and re-format dates.
# Include multiple date options for flexibility in data visualizations.
session_ct_clean <- session_counts %>%
  mutate(
    ym_date = format(session_counts$date, "%Y-%m"), 
    year = as.numeric(format(session_counts$date, "%Y")), 
    month = as.numeric(format(session_counts$date, "%m"))) %>% 
  filter(sessions > 0) %>% 
  filter(!(transactions == 0 & QTY > 0)) %>% 
  filter(!(QTY == 0 & transactions > 0)) %>% 
  subset(select = -c(dim_date))                               # drop dim date.

# Reorder cols by dates, browser, & category.
session_ct_clean <-session_ct_clean[, c(6, 7, 8, 9, 1, 2, 3, 4, 5)]

# Calculated percent of data points removed and remaining.
pct_removed <- ((nrow(session_counts) - nrow(session_ct_clean)) / 
                  nrow(session_counts)) * 100

pct_remain <- 100 - pct_removed

# Print the percentage of rows removed
cat("Percent removed:", pct_removed, "%\n",
    "Percent remaining:", pct_remain, "%\n")

# ------------------------------------------------------------
# 01.8: Clean adds to cart month by month and join.
# ------------------------------------------------------------

# Filter out session / sales issues and re-format dates.
# Match dates: join flexibility (yr-mo to reuse script w/ longer time periods).
adds_to_cart_clean <- adds_to_cart %>%
  mutate(ym_date = format(adds_to_cart$date, "%Y-%m")) %>%  
  subset(select = -c(dim_year, dim_month, date))  

# Reorder cols by dates, then adds, drop
adds_to_cart_clean <- adds_to_cart_clean[, c(2, 1)]  
  

# ##################################################################
# ------------------------------------------------------------------
# 02: Create Month * Device Aggregation.
# ------------------------------------------------------------------
# ##################################################################

# Create the aggregation. 
o1_month_by_device <- session_ct_clean %>%
  group_by(ym_date, year, month, dim_deviceCategory) %>%  # Grp dates/device.
  summarise(                              
    Sessions = sum(sessions),                 # total sessions.
    Transactions = sum(transactions),         # total transactions.
    QTY = sum(QTY),                           # total quantity.
    ECR = sum(transactions) / sum(sessions)   # calculate ECR.
  ) %>%
  arrange(ym_date, year, month, dim_deviceCategory)

 
# ##################################################################
# ------------------------------------------------------------------
# 03: Create Month over Month comparison (2 most recent months)
# ------------------------------------------------------------------
# ##################################################################

## For session_cts_clean (re-group without browser aggregation).
# Group session_cts_clean by month.
session_ctClean_month <- session_ct_clean %>% 
  group_by(ym_date, year, month) %>%  # Group by month 
  summarise(                              
    Sessions = sum(sessions),               # total sessions.
    Transactions = sum(transactions),       # total transactions.
    QTY = sum(QTY),                         # total quantity.
    ECR = sum(transactions) / sum(sessions) # calculate ECR.
  ) %>%
  arrange(ym_date, year, month)


# Join adds and sessions on year and month (can reuse for 12 mo data).
sessionsJ_adds <- session_ctClean_month %>% 
  left_join(
    adds_to_cart_clean, 
    by = c("ym_date" = "ym_date")) %>% 
  arrange(desc(year), desc(month))          # arrange on yr and month.



# Line 292: We want abs/rel vars calculated for reference month (most recent).
# Set a tag ref_mo column to use as conditional for abs/rel calculations.

# Calculate differences between the most recent month and the previous month
# m1 = most recent | m2 = prior to most recent
# Abs (diff) = m1 - m2
# Rel (diff) = abs / m1
month_by_month <- sessionsJ_adds %>%
  ungroup() %>%                            # un-group before calc abs/rel.
  head(., 2) %>%                           # select two most recent months.
  mutate(ref_mo = row_number() == 1) %>%       # identify most recent month.
  mutate(
    absDiff_adds = ifelse(ref_mo, addsToCart[1] - addsToCart[2], NA_real_),
    relDiff_adds = ifelse(ref_mo, absDiff_adds / addsToCart[2], NA_real_),
    absDiff_ECR = ifelse(ref_mo, ECR[1] - ECR[2], NA_real_),
    rel_ECR = ifelse(ref_mo, absDiff_ECR / ECR[2], NA_real_),
    absDiff_sessions = ifelse(ref_mo, Sessions[1] - Sessions[2], NA_real_),
    relDiff_sessions = ifelse(ref_mo, absDiff_sessions / Sessions[2], NA_real_),
    absDiff_trans = ifelse(ref_mo, Transactions[1] - Transactions[2], NA_real_),
    relDiff_trans = ifelse(ref_mo, absDiff_trans / Transactions[2], NA_real_),
    absDiff_QTY = ifelse(ref_mo, QTY[1] - QTY[2], NA_real_),
    relDiff_QTY = ifelse(ref_mo, absDiff_QTY / QTY[2], NA_real_)
    ) %>% 
  arrange(year, month) %>%          # re-arrange for ascending.
  subset(select = -ref_mo)          # drop reference month tag.

# Reorder cols for legibility.
o2_month_by_month <- month_by_month[, c(1,   # ym_date
                                        2,   # year
                                        3,   # month
                                        4,   # sessions
                                        13,  # abs sessions
                                        14,  # rel sessions
                                        5,   # transactions
                                        15,  # abs transactions
                                        16,  # rel transactions
                                        6,   # QTY
                                        17,  # abs QTY
                                        18,  # rel QTY
                                        7,   # ECR
                                        11,  # abs ECR
                                        12,  # rel ECR
                                        8,   # adss
                                        9,   # abs adds
                                        10)] # rel adds  


# ##################################################################
# ------------------------------------------------------------------
# 04: Export to XLSX Workbooks
# ------------------------------------------------------------------
# ##################################################################

# Create a new Excel workbook.
wb <- createWorkbook()

# Add sheets to the workbook.
addWorksheet(wb, "Sheet1")
addWorksheet(wb, "Sheet2")

# Write Month by Device to the Excel workbook.
writeData(wb, 
          "Sheet1", 
          o1_month_by_device, 
          startCol = 1,
          startRow = 1, 
          rowNames = FALSE)

# Write Month by Device to the Excel workbook.
writeData(wb, "Sheet2", 
          o2_month_by_month, 
          startCol = 1, 
          startRow = 1, 
          rowNames = FALSE)

# Save the workbook
saveWorkbook(wb, "exercise_results.xlsx", overwrite = TRUE)



# ##################################################################
# ------------------------------------------------------------------
# 05: Create Visualizations for Slide Deck
# ------------------------------------------------------------------
# ##################################################################

# ------------------------------------------------------------
# 05.1: Process for summary stats.
# ------------------------------------------------------------

# Group removed data by browser (sum)
sum_vars <- sessionsJ_adds %>%
  group_by() %>% 
  summarise(
    total_sessions = sum(Sessions),
    total_transactions = sum(Transactions),
    total_qty = sum(QTY),
    total_adds = sum(addsToCart)
  )
  

# Sum dropped.
sum_dropped <- removed %>% 
  group_by() %>% 
  summarise(
    total_sessions = sum(sessions),
    total_transactions = sum(transactions),
    total_qty = sum(QTY),  
    total_adds = 0
    )

# Flip.
vars_sum <- as.data.frame(t(sum_vars)) %>% mutate(keep = "kept")
drop_sum <-  as.data.frame(t(sum_dropped)) %>%  mutate(drop = "dropped")

# Merge on row names.
keeps <- merge(vars_sum, 
               drop_sum, 
               by = 'row.names', 
               all = TRUE) %>% 
  rename(obs = Row.names, keeps = V1.x, drops = V1.y)

# Convert obs to character.
keeps$obs <- as.character(keeps$obs)
 
# ------------------------------------------------------------
# 05.2: Viz variables
# ------------------------------------------------------------

# Set R color brewer palettes.
b_set3 <- brewer.pal(10, "Set3")


# ------------------------------------------------------------
# 05.3: Create Plots.
# ------------------------------------------------------------

# Summarize total vars.
keeps %>%
  ggplot() +
  geom_col(aes(y = reorder(obs, -keeps), x = keeps),
           fill = "grey") +
  theme_classic() +
  ggtitle("Yearly Summary") +
  xlab("Month") +
  ylab("Number") +
  scale_x_continuous(labels = scales::label_number()) +
  theme(axis.title = element_text(size = 15, face = "bold"),
        axis.text = element_text(size = 10),
        plot.title = element_text(hjust = 0.5))

# Summarize Transactions, Sessions, & ECR.
o1_month_by_device %>%
  mutate(date = make_date(year, month)) %>%
  group_by(date) %>%                          # Grp dates
  summarise(
    sessions = sum(Sessions),                 # total sessions.
    transactions = sum(Transactions),         # total transactions.
    QTY = sum(QTY),                           # total quantity.
    ECR = sum(Transactions) / sum(Sessions)   # calculate ECR.
  ) %>%
  ggplot(aes(x = date, y = sessions)) +
  geom_col(fill = "azure3",
           alpha = 0.8,
           show.legend = TRUE) +
  geom_text(aes(label = sessions, y = sessions),
            color = "azure3",
            position = position_dodge(0.9),
            vjust = -0.5) +
  geom_col(aes(y = transactions),
           fill = "darkcyan",
           alpha = 0.8,
           show.legend = TRUE) +
  geom_text(aes(label = transactions, y = transactions + 0.15),
            color = "darkcyan",
            position = position_dodge(0.9),
            vjust = -0.5) +
  geom_line(aes(y = ECR * 2E7), color = "coral1", size  = 1.2) +
  annotate("text", 
           x = as.Date("2012-07-01"), 
           y = 1350000, 
           label = "Session",
           size = 5,
           color = "azure3") +
  annotate("text", 
           x = as.Date("2012-07-08"), 
           y = 1300000, 
           label = "Transaction",
           size = 5,
           color = "darkcyan") +
  annotate("text", 
           x = as.Date("2012-07-18"), 
           y = 1250000, 
           label = "Conversion Rate",
           size = 5,
           color = "coral1") +
  xlab("Month") +
  ylab("Number") +
  ggtitle("Yearly Sessions, Transactions, & Conversion Rates") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%b-%y") +
  scale_y_continuous(name = " Sessions",
                     labels = scales::label_number(),
                     sec.axis = sec_axis(~ . / 2E7, name="ECR")) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size=13),
        axis.title.y.right = element_text(size=13),
        legend.title = element_blank())

# Summarize adds to cart by year.
adds_to_cart %>% 
  ggplot(aes(x = date, y = addsToCart)) +
  geom_col(fill = "darkslategrey",
           alpha = 0.8) +
  xlab("Month") +
  ylab("Adds to Cart") +
  ggtitle("Monthly Adds To Carts") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%b-%y") +
  scale_y_continuous() +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.y = element_text(size=13),
        axis.title.y.right = element_text(size=13),
        legend.title = element_blank())



# Stream chart for share of sessions by type.
o1_month_by_device  %>% 
  mutate(date = make_date(year, month)) %>% 
  arrange(ym_date, dim_deviceCategory) %>% 
  ggplot(aes(x = date,
             y = Sessions, 
             fill = forcats::fct_rev(dim_deviceCategory))) +
  geom_stream(type="proportional") +
  ggtitle("Share of Sessions by Device") +
  xlab("Month") +
  ylab("Proportion of Sessions") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%b-%y") +
  scale_fill_manual(values = c(b_set3[1], b_set3[3], b_set3[5]),
                    labels = c("Tablet", "Mobile", "Desktop"),
                    guide = guide_legend(reverse = TRUE))  +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom") 


# Stream chart for share of transaction by type.
o1_month_by_device  %>% 
  mutate(date = make_date(year, month)) %>% 
  arrange(ym_date) %>% 
  ggplot(aes(x = date,
             y = Transactions, 
             fill = forcats::fct_rev(dim_deviceCategory))) +
  geom_stream(type="proportional") +
  ggtitle("Share of Transactions by Device") +
  xlab("Month") +
  ylab("Proportion of Transactions") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%b-%y") +
  scale_fill_manual(values = c(b_set3[1], b_set3[3], b_set3[5]),
                    labels = c("Tablet", "Mobile", "Desktop"),
                    guide = guide_legend(reverse = TRUE))  +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

# Average ECR by Device
mean_ECR <- o1_month_by_device %>% 
  mutate(date = make_date(year, month)) %>% 
  group_by(date) %>% 
  summarize(meanECR = mean(ECR))
  
o1_month_by_device  %>% 
  mutate(date = make_date(year, month)) %>% 
  arrange(ym_date) %>% 
  ggplot(aes(x = date,
             y = ECR, 
             color = forcats::fct_rev(dim_deviceCategory))) +
  stat_summary(geom="line",
               fun = "mean",
               color = "grey45",
               size = 1,
               linetype="dashed") +
  geom_line(size=1.2) +
  ggtitle("Conversion Rate by Device") +
  xlab("Month") +
  ylab("Average ECR") +
  scale_x_date(date_breaks = "1 month" , date_labels = "%b-%y") +
  scale_color_manual(values = c(b_set3[1], b_set3[3], b_set3[5]),
                    labels = c("Tablet", "Mobile", "Desktop"),
                    guide = guide_legend(reverse = TRUE))  +
  scale_y_continuous(limits=c(0, 0.05)) +
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5),
        legend.title = element_blank())

