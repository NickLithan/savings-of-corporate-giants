# =============================================================================
# OPTIONAL CHECKS — THE SAVINGS OF CORPORATE GIANTS
# Run AFTER run_replication.R in the same R session.
# These checks were useful for troubleshooting but are not needed for the model.
# =============================================================================

stopifnot(exists('authors_recent'), exists('compustat'),
          exists('reg_data'), exists('reg_final'), exists('model_final'))

# 1. Original author data / company matching
cat('Unique author firms:', length(unique(authors$gvkey)), '\n')
cat('Unique firms in downloaded Compustat data:',
    length(unique(compustat$gvkey)), '\n')
missing_gvkeys <- setdiff(unique(authors$gvkey), unique(compustat$gvkey))
cat('Author firms without a Compustat match:', length(missing_gvkeys), '\n')
print(missing_gvkeys)

# 2. Detect duplicate observations in the authors' file
cat('Duplicate author gvkey/year combinations (2012–2019):',
    sum(duplicated(authors_recent[c('gvkey', 'year')])), '\n')
author_duplicates <- authors_recent[
  duplicated(authors_recent[c('gvkey', 'year')]) |
    duplicated(authors_recent[c('gvkey', 'year')], fromLast = TRUE),
  c('gvkey', 'conm', 'year', 'net_total')
]
print(author_duplicates)

# 3. Compare portfolio definitions
cat('Observations where total differs from net_total:',
    sum(abs(reg_data$total - reg_data$net_total) > 0.0001,
        na.rm = TRUE), '\n')
cat('Zero net_total observations:', sum(reg_data$net_total == 0), '\n')

# 4. Check missing data and industry exclusions
cat('Nonfinite observations by diagnostic variable:\n')
print(sapply(diagnostic_fixed[full_variables],
             function(x) sum(!is.finite(x))))
cat('Excluded by industry among complete observations:',
    sum(data_industry$complete & data_industry$exclude_industry), '\n')

# 5. Compare published regression with this replication
comparison <- data.frame(
  term = c('Post x tech', 'Post x pharma', 'Observations'),
  published = c(-6.881, -4.878, 1331),
  our_workflow = c(unname(coef(model_final)[1]),
                   unname(coef(model_final)[2]), nobs(model_final))
)
print(comparison, row.names = FALSE)
