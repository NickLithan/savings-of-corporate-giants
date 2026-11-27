# =============================================================================
# THE SAVINGS OF CORPORATE GIANTS — PROBLEM SET 3
# Replication of Table 2, Panel A, Column (1)
# Authors: Olivier Darmouni and Lira Mota
# Course: Empirical Methods, University of Zurich, HS 2026
#
# PURPOSE
#   Reproduce a difference-in-differences regression of total financial assets
#   relative to total assets, with firm and year fixed effects.
#
# DATA
#   1. Hand-collected annual portfolio data provided by the paper's authors.
#   2. Authors' technology/pharma/other classification.
#   3. Compustat data retrieved from WRDS (requires authorized access).
#
# IMPORTANT REPLICATION NOTES
#   - This script follows our group workflow, not every step of the full authors'
#     replication package. Some variable definitions and filters are approximations.
#   - Authors' 'year' is matched to Compustat's fiscal year ('fyear').
#   - Historical SIC/NAICS are used where available; company codes are fallback.
#   - The published table has 1,331 observations. Our current sample has 1,330.
#   - WRDS-provided data may be subject to redistribution restrictions.
#
# USAGE
#   Open the RStudio project in this folder; run source('run_replication.R').
# =============================================================================

# -----------------------------------------------------------------------------
# 0. Packages, directories and helper functions
# -----------------------------------------------------------------------------
packages <- c('DBI', 'RPostgres', 'rstudioapi', 'fixest')
missing_packages <- packages[!vapply(packages, requireNamespace,
                                      logical(1), quietly = TRUE)]
if (length(missing_packages)) install.packages(missing_packages)

in_dir <- 'data'
out_dir <- 'output'
dir.create(in_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

format_gvkey <- function(x) sprintf('%06d', as.integer(x))

require_columns <- function(data, columns, label) {
  missing <- setdiff(columns, names(data))
  if (length(missing)) {
    stop(label, ' is missing these columns: ', paste(missing, collapse = ', '))
  }
}

# -----------------------------------------------------------------------------
# 1. Load original hand-collected data and sector classification
# -----------------------------------------------------------------------------
authors_file <- file.path(in_dir, 'fis_annual.csv')
sectors_file <- file.path(in_dir, 'firms_tech_pharma_other_details_filled.csv')

if (!file.exists(authors_file) || !file.exists(sectors_file)) {
  stop('Required authors\' CSV files not found in data/. See README.md.')
}

authors <- read.csv(authors_file, stringsAsFactors = FALSE)
sectors <- read.csv(sectors_file, stringsAsFactors = FALSE)
require_columns(authors, c('gvkey', 'year', 'net_total', 'total',
                           'cash_and_cash_equivalents_total', 'us_treas_agency',
                           'others_us_gov_debt', 'us_corporate',
                           'foreign_corporate'), 'Authors\' data')
require_columns(sectors, c('gvkey', 'test_sector'), 'Sector classification')

authors$gvkey <- format_gvkey(authors$gvkey)
sectors$gvkey <- format_gvkey(sectors$gvkey)
sectors <- unique(sectors[, c('gvkey', 'test_sector')])
if (anyDuplicated(sectors$gvkey)) {
  stop('Sector file assigns more than one sector to a firm (gvkey).')
}

firm_ids <- unique(authors$gvkey)

# -----------------------------------------------------------------------------
# 2. Load cached WRDS extracts, or download them once
#    (Credentials are entered locally and NEVER saved in the script.)
# -----------------------------------------------------------------------------
compustat_file <- file.path(in_dir, 'compustat_2011_2019.rds')
industry_file <- file.path(in_dir, 'industry_codes_2012_2019.rds')

fetch_missing_wrds_data <- function(firm_ids, compustat_file, industry_file) {
  wrds_user <- Sys.getenv('WRDS_USER', unset = '')
  if (!nzchar(wrds_user)) wrds_user <- readline('WRDS username: ')
  if (!nzchar(wrds_user)) stop('WRDS username is required.')

  if (!rstudioapi::isAvailable()) {
    stop('Use RStudio to enter the WRDS password securely.')
  }
  wrds_password <- rstudioapi::askForPassword('WRDS password')

  con <- DBI::dbConnect(
    RPostgres::Postgres(),
    host = 'wrds-pgdata.wharton.upenn.edu',
    port = 9737,
    dbname = 'wrds',
    sslmode = 'require',
    user = wrds_user,
    password = wrds_password
  )
  on.exit(DBI::dbDisconnect(con), add = TRUE)
  rm(wrds_password)

  id_list <- paste(DBI::dbQuoteString(con, firm_ids), collapse = ', ')

  if (!file.exists(compustat_file)) {
    message('Downloading selected Compustat fundamentals (2011–2019)...')
    comp_sql <- paste0(
      'SELECT gvkey, datadate, fyear, at, che, sale, pifo, ',
      'dvt, prstkc, sstk FROM comp.funda ',
      'WHERE fyear BETWEEN 2011 AND 2019 ',
      'AND gvkey IN (', id_list, ') ',
      "AND indfmt = 'INDL' AND datafmt = 'STD' ",
      "AND popsrc = 'D' AND consol = 'C'"
    )
    comp <- DBI::dbGetQuery(con, comp_sql)
    saveRDS(comp, compustat_file)
  }

  if (!file.exists(industry_file)) {
    message('Downloading historical SIC/NAICS codes (2012–2019)...')
    industry_sql <- paste0(
      'SELECT f.gvkey, f.fyear AS year, f.sich, f.naicsh, ',
      'c.sic AS sic_company, c.naics AS naics_company ',
      'FROM comp.funda AS f ',
      'LEFT JOIN comp.company AS c ON f.gvkey = c.gvkey ',
      'WHERE f.fyear BETWEEN 2012 AND 2019 ',
      'AND f.gvkey IN (', id_list, ') ',
      "AND f.indfmt = 'INDL' AND f.datafmt = 'STD' ",
      "AND f.popsrc = 'D' AND f.consol = 'C'"
    )
    codes <- DBI::dbGetQuery(con, industry_sql)
    saveRDS(codes, industry_file)
  }
}

if (!file.exists(compustat_file) || !file.exists(industry_file)) {
  fetch_missing_wrds_data(firm_ids, compustat_file, industry_file)
}

compustat <- readRDS(compustat_file)
industry <- readRDS(industry_file)
require_columns(compustat, c('gvkey', 'fyear', 'at', 'che', 'sale', 'pifo',
                             'dvt', 'prstkc', 'sstk'), 'Compustat extract')
require_columns(industry, c('gvkey', 'year', 'sich', 'naicsh',
                            'sic_company', 'naics_company'), 'Industry extract')
compustat$gvkey <- format_gvkey(compustat$gvkey)
industry$gvkey <- format_gvkey(industry$gvkey)
industry$year <- as.integer(industry$year)

if (anyDuplicated(compustat[c('gvkey', 'fyear')])) {
  stop('Compustat has duplicate gvkey/fyear observations; inspect the extract.')
}
if (anyDuplicated(industry[c('gvkey', 'year')])) {
  stop('Industry extract has duplicate gvkey/year observations.')
}

# -----------------------------------------------------------------------------
# 3. Merge hand-collected annual portfolios with Compustat
# -----------------------------------------------------------------------------
authors_recent <- subset(authors, year >= 2012 & year <= 2019)
compustat$in_compustat <- TRUE
merged_data <- merge(
  authors_recent, compustat,
  by.x = c('gvkey', 'year'),
  by.y = c('gvkey', 'fyear'),
  all.x = TRUE
)

# Preserve the sample restriction used in our original workflow:
# nonmissing net_total and strictly positive total assets.
reg_data <- subset(merged_data, !is.na(net_total) & !is.na(at) & at > 0)

# Join authors' classification of technology, pharma and other firms.
reg_data <- merge(reg_data, sectors, by = 'gvkey', all.x = TRUE)
if (anyNA(reg_data$test_sector)) {
  stop('Some observations have no authors\' sector classification.')
}
reg_data$tech_dummy <- as.integer(reg_data$test_sector == 'tech')
reg_data$pharma_dummy <- as.integer(reg_data$test_sector == 'pharma')

# -----------------------------------------------------------------------------
# 4. Construct financial asset ratios and payout / foreign-income diagnostics
# -----------------------------------------------------------------------------
diagnostic_fixed <- reg_data

# The relevant outcome in the published table is total / at (NOT net_total / at).
diagnostic_fixed$fa_ratio <- 100 * diagnostic_fixed$total / diagnostic_fixed$at

diagnostic_fixed$cash_ratio <- 100 *
  diagnostic_fixed$cash_and_cash_equivalents_total / diagnostic_fixed$at

diagnostic_fixed$mkt_ratio <- 100 *
  (diagnostic_fixed$total - diagnostic_fixed$cash_and_cash_equivalents_total) /
  diagnostic_fixed$at

# Treat missing individual financial-portfolio components as zero, following
# the authors' treatment of the hand-collected FIS series.
portfolio_parts <- c('us_treas_agency', 'others_us_gov_debt',
                     'us_corporate', 'foreign_corporate')
diagnostic_fixed[portfolio_parts] <- lapply(
  diagnostic_fixed[portfolio_parts],
  function(x) replace(x, is.na(x), 0)
)

diagnostic_fixed$gov_ratio <- 100 *
  (diagnostic_fixed$us_treas_agency + diagnostic_fixed$others_us_gov_debt) /
  diagnostic_fixed$at

diagnostic_fixed$corp_ratio <- 100 *
  (diagnostic_fixed$us_corporate + diagnostic_fixed$foreign_corporate) /
  diagnostic_fixed$at

# Lagged total assets from the prior fiscal year.
lag_assets <- compustat[, c('gvkey', 'fyear', 'at')]
names(lag_assets) <- c('gvkey', 'year', 'at_lag')
lag_assets$year <- lag_assets$year + 1L
diagnostic_fixed <- merge(
  diagnostic_fixed, lag_assets, by = c('gvkey', 'year'), all.x = TRUE
)

# Dividends + stock repurchases - stock issuance.
# 'na.rm = TRUE' preserves the group's original approximation.
diagnostic_fixed$payout <- rowSums(
  cbind(diagnostic_fixed$dvt, diagnostic_fixed$prstkc,
        -diagnostic_fixed$sstk),
  na.rm = TRUE
)
diagnostic_fixed$payout_ratio <- 100 *
  diagnostic_fixed$payout / diagnostic_fixed$at_lag

# Foreign income / sales; missing PIFO treated as zero.
diagnostic_fixed$pifo_share <- ifelse(
  diagnostic_fixed$sale > 0,
  100 * ifelse(is.na(diagnostic_fixed$pifo), 0,
              diagnostic_fixed$pifo) / diagnostic_fixed$sale,
  NA_real_
)

# Company-specific pre-reform average (2012–2016).
pre_data <- subset(diagnostic_fixed, year >= 2012 & year <= 2016)
pre_data$pifo[is.na(pre_data$pifo)] <- 0
pre_data$foreign_share <- with(pre_data, pifo / sale)
pifo_pre <- tapply(
  pre_data$foreign_share, pre_data$gvkey,
  function(x) mean(x, na.rm = TRUE)
)
diagnostic_fixed$pifo_avg_pre <- as.numeric(
  pifo_pre[diagnostic_fixed$gvkey]
)

# Complete cases for the variables considered in the group's workflow.
full_variables <- c('fa_ratio', 'cash_ratio', 'mkt_ratio', 'gov_ratio',
                    'corp_ratio', 'payout_ratio', 'pifo_share', 'pifo_avg_pre')
diagnostic_fixed$complete <- rowSums(
  is.finite(as.matrix(diagnostic_fixed[full_variables]))
) == length(full_variables)

# -----------------------------------------------------------------------------
# 5. Apply historical industry exclusions
# -----------------------------------------------------------------------------
data_industry <- merge(
  diagnostic_fixed, industry, by = c('gvkey', 'year'), all.x = TRUE
)

# Historical SIC/NAICS preferred; fall back to Compustat company codes.
data_industry$sic_code <- ifelse(
  !is.na(data_industry$sich),
  as.character(data_industry$sich),
  as.character(data_industry$sic_company)
)
data_industry$naics_code <- ifelse(
  !is.na(data_industry$naicsh),
  as.character(data_industry$naicsh),
  as.character(data_industry$naics_company)
)

sic <- data_industry$sic_code
naics <- data_industry$naics_code

# Financial services, utilities and government (as in our earlier checks).
data_industry$exclude_industry <- (
  (!is.na(sic) & substr(sic, 1, 1) %in% c('6', '9')) |
  (!is.na(sic) & substr(sic, 1, 2) == '49') |
  (!is.na(naics) & substr(naics, 1, 2) %in% c('22', '52'))
)

reg_final <- subset(data_industry, complete & !exclude_industry)
if (anyDuplicated(reg_final[c('gvkey', 'year')])) {
  stop('Final sample has duplicate firm-year observations.')
}
reg_final$post <- as.integer(reg_final$year >= 2018)

# -----------------------------------------------------------------------------
# 6. Main difference-in-differences regression and clustered SE robustness
# -----------------------------------------------------------------------------
models_tech_pharma <- list()
models_tech_pharma$fa <- fixest::feols(
  fa_ratio ~ post:tech_dummy + post:pharma_dummy | gvkey + year,
  data = reg_final,
  vcov = 'iid'
)
models_tech_pharma$cash <- fixest::feols(
  cash_ratio ~ post:tech_dummy + post:pharma_dummy | gvkey + year,
  data = reg_final,
  vcov = 'iid'
)
models_tech_pharma$mkt <- fixest::feols(
  mkt_ratio ~ post:tech_dummy + post:pharma_dummy | gvkey + year,
  data = reg_final,
  vcov = 'iid'
)
model_final <- models_tech_pharma$fa

cat('\n===== TABLE 2, PANEL A, COLUMN 1: IID STANDARD ERRORS =====\n')
print(summary(models_tech_pharma, vcov = 'iid'))
cat('\n===== ROBUSTNESS: STANDARD ERRORS CLUSTERED BY FIRM =====\n')
print(summary(models_tech_pharma, vcov = ~gvkey))
cat('\n===== COMPARATIVE REGRESSION TABLE =====\n')
print(fixest::etable(models_tech_pharma, vcov = list("each", 'iid', ~gvkey)))

# -----------------------------------------------------------------------------
# 7. Report sample sizes and save results
# -----------------------------------------------------------------------------
sample_flow <- data.frame(
  step = c('Authors, 2012–2019',
           'After merge: WRDS-matched observations',
           'Positive AT and nonmissing net_total',
           'Complete diagnostic variables',
           'After industry exclusions (final)'),
  observations = c(nrow(authors_recent),
                   sum(!is.na(merged_data$in_compustat)),
                   nrow(reg_data),
                   sum(diagnostic_fixed$complete),
                   nrow(reg_final))
)

cat('\n===== SAMPLE FLOW =====\n')
print(sample_flow, row.names = FALSE)
cat('Final unique firms:', length(unique(reg_final$gvkey)), '\n')
cat('Paper reports 1,331 observations; this workflow previously yielded 1,330.\n')
cat('Observations by year:\n')
print(table(reg_final$year))

write.csv(sample_flow, file.path(out_dir, 'sample_flow.csv'), row.names = FALSE)
saveRDS(reg_final, file.path(out_dir, 'regression_data_final.rds'))
saveRDS(model_final, file.path(out_dir, 'model_final.rds'))
saveRDS(models_tech_pharma, file.path(out_dir, 'models_tech_pharma.rds'))
capture.output(
  fixest::etable(model_final, vcov = list('iid', ~gvkey)),
  file = file.path(out_dir, 'table2_panelA_col1.txt')
)
capture.output(
  fixest::etable(models_tech_pharma, vcov = list("each", 'iid', ~gvkey)),
  file = file.path(out_dir, 'table2_panelA_col1-3.txt')
)


message('\nFinished. See output/ for the results.')
