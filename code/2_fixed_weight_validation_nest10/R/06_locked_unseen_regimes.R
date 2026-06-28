# =============================================================================
# POST-DISCOVERY FIXED-WEIGHT VALIDATION — LOCKED UNSEEN SIMILAR REGIME CONSTRUCTION
# =============================================================================
# This module creates a confirmatory test regime that was not used by HPF1, HPF2,
# specialist halving, or the original held-out gate. It does not retrain or mutate
# GA weights. It only evaluates fixed discovery weights on neighboring scenarios.
# =============================================================================

q1_nearest_not_in <- function(value, grid, used) {
  grid <- sort(unique(as.numeric(grid)))
  used <- unique(as.numeric(used))
  cand <- grid[!grid %in% used]
  if (!length(cand)) cand <- grid
  cand[which.min(abs(cand - as.numeric(value)[1]))]
}

q1_type_group <- function(types) {
  types <- tolower(as.character(types))
  if (any(grepl("upper|clustered_upper", types))) return("upper")
  if (any(grepl("lower", types))) return("lower")
  if (any(grepl("sym", types))) return("symmetric")
  if (any(grepl("bimodal|mixture", types))) return("bimodal")
  if (any(grepl("point", types))) return("point")
  "generic"
}

q1_similar_type_candidates <- function(types, config) {
  all_types <- config$full_contamination_types
  used <- unique(tolower(as.character(types)))
  group <- q1_type_group(types)
  cand <- switch(group,
                 upper = c("upper_tail", "clustered_upper"),
                 lower = c("lower_tail"),
                 symmetric = c("symmetric_t", "clustered_symmetric"),
                 bimodal = c("bimodal_near", "bimodal_far"),
                 point = c("point_mass"),
                 all_types)
  cand <- intersect(cand, all_types)
  cand2 <- cand[!tolower(cand) %in% used]
  if (length(cand2)) cand2 else cand
}

q1_generate_locked_unseen_conditions <- function(row, config) {
  rates <- q1_parse_semicolon_values(row$exact_contamination_rates, numeric = TRUE)
  scales <- q1_parse_semicolon_values(row$exact_outlier_scales, numeric = TRUE)
  types <- q1_parse_semicolon_values(row$exact_contamination_types, numeric = FALSE)
  ns <- q1_parse_semicolon_values(row$exact_sample_sizes, numeric = TRUE)

  if (!length(rates)) rates <- 0
  if (!length(scales)) scales <- 3
  if (!length(types)) types <- "upper_tail"
  if (!length(ns)) ns <- config$sample_sizes_default

  base_rate <- stats::median(as.numeric(rates), na.rm = TRUE)
  base_scale <- stats::median(as.numeric(scales), na.rm = TRUE)
  unseen_rate <- q1_nearest_not_in(base_rate, config$full_contamination_rates, rates)
  unseen_scale <- q1_nearest_not_in(base_scale, config$full_outlier_scales, scales)
  unseen_types <- q1_similar_type_candidates(types, config)

  # Keep the unseen validation focused: one similar contamination structure and the
  # same sample-size support. This increases evidence without reopening discovery.
  out <- expand.grid(contamination_rate = unseen_rate,
                     outlier_scale = unseen_scale,
                     contamination_type = unseen_types[1],
                     sample_size = as.integer(ns),
                     KEEP.OUT.ATTRS = FALSE,
                     stringsAsFactors = FALSE)
  out <- out[seq_len(min(nrow(out), max(1L, config$unseen_conditions_per_regime * length(unique(out$sample_size))))), , drop = FALSE]
  out$locked_unseen_reason <- paste0(
    "similar_profile_not_in_discovery_exact_cell; rate=", unseen_rate,
    "; scale=", unseen_scale,
    "; type=", unseen_types[1]
  )
  out
}

q1_conditions_for_mode <- function(row, config, validation_mode = "original_regime") {
  validation_mode <- as.character(validation_mode)[1]
  if (identical(validation_mode, "locked_unseen_similar")) {
    return(q1_generate_locked_unseen_conditions(row, config))
  }
  out <- q1_expand_regime_conditions(row, config)
  out$locked_unseen_reason <- NA_character_
  out
}
