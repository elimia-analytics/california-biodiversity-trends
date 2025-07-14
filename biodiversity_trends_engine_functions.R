## Set things up
CACHE <-  "/tmp/Rtmp-urban" # "cache/"
duckdbfs::load_h3()
duckdbfs::load_spatial()
con <- duckdbfs::cached_connection()
DBI::dbExecute(con, "SET threads = 60;")
aws_public_endpoint <- "minio.carlboettiger.info"
aws_s3_endpoint <- "minio.carlboettiger.info"
#'
## Extract GBIF data
get_gbif_data <- memoise(function(aoi) {
  # identify h10 cells overlapping area of interest :
  focal_h3_cells <- h3jsr::polygon_to_cells(geometry = aoi, res = 8) %>% unlist()
  # identify parent h0 cell containing area of interest h10 cells
  h0 <- h3jsr::get_parent(focal_h3_cells, res = 0) %>% unique() %>% toupper()
  # Identify corresponding gbif h0 tile(s)
  tiles <- paste0(glue("https://{aws_public_endpoint}/public-gbif/hex/h0="), h0, "/part0.parquet")
  # Preload GBIF records for corresponding h0 tile(s)
  tile_records <- open_dataset(tiles, recursive = FALSE)
  aoi_bbox <- aoi %>% 
    sf::st_transform(4326) %>% 
    sf::st_bbox()
  bbxmin <- as.numeric(aoi_bbox[["xmin"]])
  bbxmax <- as.numeric(aoi_bbox[["xmax"]])
  bbymin <- as.numeric(aoi_bbox[["ymin"]])
  bbymax <- as.numeric(aoi_bbox[["ymax"]])
  gbif_data <- tile_records %>% 
    dplyr::filter(decimallongitude >= bbxmin & decimallongitude <= bbxmax & decimallatitude >= bbymin & decimallatitude <= bbymax,
                  year >= 1900,
                  !is.na(species),
                  collectioncode != "EBIRD"
    ) %>% 
    dplyr::filter(h8 %in% toupper(as.character(focal_h3_cells))) %>% 
    dplyr::mutate(geom = ST_Point(decimallongitude, decimallatitude)) %>% 
    duckdbfs::to_sf(crs = 4326)
  
  gbif_data <- gbif_data[which(purrr::map_int(st_intersects(gbif_data, aoi), length) > 0), ]
  
  return(gbif_data)
},
cache = cache_filesystem(CACHE)
)
## Function to generate hex summary metrics
#'
generate_h3_cells <- function(bbox_poly, res) {
  h3_indexes <- h3jsr::polygon_to_cells(bbox_poly, res)
  h3_cells <- h3jsr::cell_to_polygon(h3_indexes, simple = FALSE)
  h3_cells <- h3_cells %>% 
    dplyr::mutate(h3_address = toupper(h3_address))
  names(h3_cells)[1] <- paste0("h", res)
  h3_cells
}
#' 
## Function to generate hex summary metrics
get_count_summary <- memoise(function(records, base_hexes, metric = c("Records", "Species", "Observers", "Visits"), resolution = "h7"){

  # Ensure metric is one of the three options
  metric <- match.arg(metric)
  
  # Create result raster
  metric_hexes <- base_hexes
  
  # Calculate "records" metric (i.e. number of records per cell)
  if (metric == "Records"){

    summary_counts <- records %>% 
      dplyr::count(.data[[resolution]], name = "metric")
    
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
    
  }
  
  # Calculate "species" metric (i.e. number of taxa per cell)
  if (metric == "Species"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(species))
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  # Calculate "observers" metric (i.e. number of observers per cell)
  if (metric == "Observers"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(recordedby))
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  if (metric == "Visits"){
    summary_counts <- records %>%
      dplyr::group_by(.data[[resolution]]) %>%
      dplyr::summarise(metric = n_distinct(visitID))
    
    # Update values of results raster with metric counts
    summary_counts_sf <- metric_hexes %>% 
      dplyr::left_join(summary_counts, by = resolution)   
  }
  
  # Return output object
  return(summary_counts_sf)
  
},
cache = cache_filesystem(CACHE)
)

# Function to get species association matrix
get_species_association_matrix <- memoise(function(analysis_records, species = NULL){
  
  analysis_records <- analysis_records %>% sf::st_set_geometry(NULL)
  
  if (is.null(species)){
  common_species <- analysis_records %>%
    dplyr::group_by(species) %>%
    dplyr::count(species) %>% 
    dplyr::filter(n >= 50) %>%
    pull(species)
  } else {
    common_species <- species
  }
  
  count_data <- analysis_records %>%
    dplyr::filter(species %in% common_species) %>%
    dplyr::group_by(visitID, species) %>%
    dplyr::count(species) %>%
    dplyr::mutate(species_name = gsub(" ", "_", species)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-species) %>%
    tidyr::spread(key = species_name, value = n, fill = 0) 
  
  #### Generate a reduced set that includes only visits with at least two species and species with at least 50 observations across those visits
  count_data_reduced <- count_data[, -1]
  count_data_reduced <- count_data_reduced[which(rowSums(count_data_reduced) >= 2), ]
  # count_data_reduced <- count_data_reduced[, which(colSums(count_data_reduced) >= 50)]
  # count_data_reduced <- count_data_reduced[which(rowSums(count_data_reduced) >= 2), ]
  #' #### Convert count data to presence absence
  presabs <- count_data_reduced %>% 
    dplyr::mutate_all(~as.numeric(. > 0))
  #' #### Keep only visits with at least two species observed
  presabs <- presabs[which(rowSums(presabs) >= 2), ]
  #' #### Flip site x species matrix to obtain species x sites matrix
  #' #### This is necessary to estimate associations among species, not sites, within vegdist
  presabs_flipped <- t(presabs) %>% as.data.frame() 
  
  # Calculate species associations
  species_association_matrix <- mvabund(count_data_reduced %>% as.matrix())
  species_association_matrix <- manyglm(species_association_matrix ~ 1)
  species_association_matrix <- ecoCopula::cord(species_association_matrix)
  # species_association_matrix <- species_association_matrix$sigma
  
  return(species_association_matrix)
  
},
cache = cache_filesystem(CACHE)
)

# Function to extract associated species
get_associated_species <- function(focal_species, ### Select a focal species
                                   association_matrix = species_associations, ### Point to the pairwise association matrix with all species
                                   num_species = 100 ### Select the total number of associated species in output
){
  
  ### Update focal_species name
  focal_species <- gsub(" ", "_", focal_species)
  ### Extract the focal species' column from the association matrix 
  association_species_ordered <- association_matrix[focal_species, ]
  ### Order the associated species vector by decreasing association 
  association_species_ordered <- association_species_ordered %>% 
    stack() %>% 
    purrr::set_names(c("association", "species")) %>% 
    arrange(desc(association))
  ### Extract the most associated species
  #### Identify nonzero associations
  # most_associated_species <- association_species_ordered %>% dplyr::filter(association >= 0.2) %>% dplyr::pull(species)
  #### Extract the first n species associations
  most_associated_species <- association_species_ordered %>% dplyr::pull(species) %>% head(num_species)

  ### Return vector of most associated species
  return(most_associated_species)
  
}

# Function to get count data from records
get_count_data <- memoise(function(
    records, 
    focal_taxon_name, 
    species_association_matrix = NULL,
    by_reference_taxon = FALSE,
    reference_taxon_rank = c("genus", "family", "order", "class", "phylum", "kingdom"), 
    start_year = 1900,
    ...
    ){
  
  reference_taxon_rank <- match.arg(reference_taxon_rank)
  
  associated_species <- get_associated_species(
    focal_species = focal_taxon_name,
    association_matrix = species_association_matrix, 
    num_species = 50
  )
  
  if (by_reference_taxon | (length(associated_species) < 10)){
    
    # Identify reference taxon name
    reference_taxon <- records %>% dplyr::filter(species == focal_taxon_name) %>% dplyr::pull(reference_taxon_rank) %>% unique()
    
    # Filter records from reference taxon across relevant years
    reference_records <- records %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::filter(.data[[reference_taxon_rank]] == reference_taxon) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  } else {
    
    associated_species <- gsub("_", " ", associated_species)
    
    reference_records <- records %>% 
      sf::st_set_geometry(NULL) %>% 
      dplyr::filter(species %in% c(focal_taxon_name, associated_species)) %>% 
      dplyr::filter(year >= start_year & year < substr(Sys.Date()-365, 1, 4)) 
    
  }
  
  count_data <- reference_records %>%
    dplyr::group_by(visitID, h7, recordedby, observationdate, year) %>%
    dplyr::count(species) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = species, value = n, fill = 0) 

  return(count_data)
  
},
cache = cache_filesystem(CACHE)
)

# Function to get detection data from counts
calculate_detection_data <- memoise(function(counts = count_data, focal_taxon_name, ...){
  
  detection_data <- counts %>% 
    dplyr::mutate(across(all_of(setdiff(names(counts), c("visitID", "h7", "recordedby", "observationdate", "year"))), ~as.numeric(. > 0)))
  
  focal_species_detection_data <- counts %>% 
    dplyr::mutate(focal_species_count = .data[[focal_taxon_name]],
                  focal_species_detection = ifelse(focal_species_count > 0, 1, 0),
                  total_number_observations = counts %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year) %>% rowSums(),
                  species_list_length = detection_data %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year) %>% rowSums()
    )
  
  return(focal_species_detection_data)
  
},
cache = cache_filesystem(CACHE)
)

# Function to calculate spatiotemporal trends from detection history
calculate_trends <- memoise(function(detections = focal_species_detection_data, grp = NULL){

  detections <- detections %>% 
    dplyr::mutate(
      h6 = h3jsr::get_parent(h7, res = 6),
      h5 = h3jsr::get_parent(h7, res = 5)
    )
  
  if (is.null(grp)) grp = c("year", "h7")
    
  # # Group detections
  # trend_data <- detections %>%
  #   dplyr::group_by(year, h7) 
  # 
  # } else {
  #   
  #   # Group detections
  #   trend_data <- detections %>%
  #     dplyr::group_by(.data[[grp]]) 
  #   
  # }
  
  trend_data <- detections %>%
    dplyr::group_by(across(grp)) 
    
  trend_data <- trend_data %>%
    dplyr::summarise(
      focal_species_count = sum(focal_species_count),
      focal_species_detection = sum(focal_species_detection),
      total_number_observations = sum(total_number_observations),
      species_list_length = sum(species_list_length),
      n_visits = n()
    ) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(
      proportion_observations = focal_species_count/total_number_observations, # mean(proportion_observations),
      proportion_species = focal_species_detection/species_list_length,
      reporting_rate = focal_species_detection/n_visits
    )
  
  return(trend_data)
  
},
cache = cache_filesystem(CACHE)
)

# Get randomized versions of observed detection history
run_randomizations <- memoise(function(counts = count_dat, iterations = 100, focal_tax){
  randomized_data <- purrr::map(1:iterations, function(i){
    count_matrix <- data.matrix(counts %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year))
    rownames(count_matrix) <- counts$visitID
    count_dat_random <- randomizeMatrix(count_matrix, null.model="independentswap")
    count_dat_random <- cbind(counts %>% dplyr::select(visitID, h7, recordedby, observationdate, year), count_dat_random %>% as.data.frame())
    detection_dat_random <- calculate_detection_data(counts = count_dat_random, focal_taxon_name = focal_tax)
  })
  return(randomized_data)
}, 
cache = cache_filesystem(CACHE)
)

# Get randomized versions of focal species trends
get_randomized_metric <- memoise(function(randomized_detections, grp = "year", ...){
  
  if (is.null(grp)) grp = c("year", "h7")
  
  randomized_trends <- purrr::map(1:length(randomized_detections), function(i){
    grp_dat_random <- calculate_trends(detections = randomized_detections[[i]], grp = grp)
    out <- data.frame(grp_dat_random[, grp], 
                      proportion_observations = grp_dat_random$proportion_observations, 
                      proportion_species = grp_dat_random$proportion_species, 
                      reporting_rate = grp_dat_random$reporting_rate, 
                      randomization = i)
    names(out)[1:length(grp)] <- grp
    
    out
    
  }) %>% bind_rows()
  
  return(randomized_trends)
  
},
cache = cache_filesystem(CACHE)
)

# Calculate standardized difference
get_standardized_difference <- memoise(function(observed, randomized, metric = c("proportion_observations", "proportion_species", "reporting_rate"), grp = NULL){
  
  if (is.null(grp)) grp = c("year", "h7")
  
  metric <- match.arg(metric)
  
  randomized_values <- randomized %>% 
    group_by(across(grp)) %>% 
    dplyr::summarise(
      mn = mean(.data[[metric]], na.rm = TRUE),
      lower = quantile(.data[[metric]], .025, na.rm = TRUE),
      upper = quantile(.data[[metric]], .975, na.rm = TRUE),
      sdev = sd(.data[[metric]], na.rm = TRUE)
    ) 
  
  sd_dat <- observed %>% 
    dplyr::select(all_of(grp), all_of(metric)) %>% 
    purrr::set_names(c(grp, "observed")) %>% 
    dplyr::left_join(observed, by = grp) %>% 
    dplyr::left_join(randomized_values, by = grp) %>% 
    dplyr::mutate(
      sd = (observed - mn)/sdev
    )
  names(sd_dat)[which(names(sd_dat) == "mn")] <- paste0(metric, "_mn")
  names(sd_dat)[which(names(sd_dat) == "sdev")] <- paste0(metric, "_sdev")
  names(sd_dat)[which(names(sd_dat) == "upper")] <- paste0(metric, "_upper")
  names(sd_dat)[which(names(sd_dat) == "lower")] <- paste0(metric, "_lower")  
  names(sd_dat)[which(names(sd_dat) == "sd")] <- paste0(metric, "_sd")
  
  return(sd_dat)
  
},
cache = cache_filesystem(CACHE)
)

get_species_trends <- memoise(function(
    analysis_records = area_of_interest$gbif_data, 
    focal_taxon, 
    use_reference_taxon = FALSE, 
    species_association_matrix = area_of_interest$species_association_matrix, 
    resolution = "h6"
    ){
  
  # Identify appropriate taxonomic resolution
  # Identify relevant taxonomic scale
  tax_levels <- c("species", "genus", "family", "order", "class", "phylum", "kingdom")
  taxonomic_scale_counts <- purrr::map(tax_levels, function(tax){
    focal_taxon_gbif_occurrences <- analysis_records %>%
      dplyr::filter(species == focal_taxon)
    taxonomic_name <- focal_taxon_gbif_occurrences[1, tax] %>%  
      sf::st_set_geometry(NULL) %>% 
      as.character()
    out <- data.frame(
      taxonomic_level = tax,
      taxonomic_name = taxonomic_name,
      n = analysis_records %>% 
        sf::st_set_geometry(NULL) %>% 
        dplyr::select(all_of(tax)) %>% 
        filter_all(any_vars(grepl(taxonomic_name, .))) %>% 
        nrow() 
    )
  }) %>% 
    bind_rows()
  taxonomic_scale_counts <- taxonomic_scale_counts %>% 
    dplyr::filter(
      n != taxonomic_scale_counts$n[1] | taxonomic_level == "species"
    ) %>% 
    dplyr::filter(complete.cases(.))
  taxonomic_scale_counts %>% 
    dplyr::mutate(
      diff_from_species = abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)
    )
  suggested_baseline_taxon <- taxonomic_scale_counts[which.min(abs((taxonomic_scale_counts$n[1]*10)-taxonomic_scale_counts$n)[-1])+1,"taxonomic_level"]
  
  # if (taxonomic_scale_counts$n[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon] < (1.5*taxonomic_scale_counts$n[1])) suggested_baseline_taxon <- taxonomic_scale_counts$taxonomic_level[which(taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon)+1]
  
  # Get observed trends
  ## Get observed detection history
  counts_observed <- get_count_data(
    records = analysis_records, 
    focal_taxon_name = focal_taxon, 
    species_association_matrix = species_association_matrix,
    by_reference_taxon = use_reference_taxon, 
    reference_taxon_rank = suggested_baseline_taxon
    )
  
  counts_observed <- counts_observed[-which(rowSums(counts_observed %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ]
  
  detections_observed <- calculate_detection_data(counts = counts_observed, focal_taxon_name = focal_taxon)
  
  ## Get observed yearly trends
  yearly_trends_observed <- calculate_trends(detections = detections_observed, grp = "year")

  ## Get observed spatial trends
  spatial_trends_observed <- calculate_trends(detections = detections_observed, grp = resolution)

  ## Get observed spatiotemporal trends
  spatiotemporal_trends_observed <- calculate_trends(detections = detections_observed, grp = c("year", resolution))

  # Get randomized (i.e., expected) trends
  ## Get randomized detection history
  detections_random <- run_randomizations(counts = counts_observed, iterations = 100, focal_tax = focal_taxon)

  ## Get randomized yearly trends
  yearly_trends_random <- get_randomized_metric(randomized_detections = detections_random, grp = "year")

  # ## Get randomized spatiotemporal trends
  spatiotemporal_trends_random <- get_randomized_metric(randomized_detections = detections_random, grp = c("year", resolution))

  # Calculate standardized differences
  ## Yearly
  yearly_trends_difference <- get_standardized_difference(
      observed = yearly_trends_observed,
      randomized = yearly_trends_random,
      metric = "reporting_rate",
      grp = "year"
    ) %>%
    dplyr::mutate(focal_taxon = focal_taxon,
                  reference_taxon = paste(suggested_baseline_taxon, taxonomic_scale_counts$taxonomic_name[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon])
    )

  # difference_dat <- trends_metrics_list[[1]] %>% 
  #   cbind(trends_metrics_list[[2]] %>% 
  #           dplyr::select(ends_with(c("_mn", "_sdev", "_lower", "_upper", "_sd"))),
  #         trends_metrics_list[[3]] %>% 
  #           dplyr::select(ends_with(c("_mn", "_sdev", "_lower", "_upper", "_sd")))
  #   )

  ## Spatiotemporal
  spatiotemporal_trends_difference <- get_standardized_difference(
    observed = spatiotemporal_trends_observed,
    randomized = spatiotemporal_trends_random,
    metric = "reporting_rate",
    grp = c("year", resolution)
  ) %>% 
    dplyr::mutate(focal_taxon = focal_taxon,
                  reference_taxon = paste(suggested_baseline_taxon, taxonomic_scale_counts$taxonomic_name[taxonomic_scale_counts$taxonomic_level == suggested_baseline_taxon])
    )

  out <- list(
    spatial_pattern = spatial_trends_observed,
    yearly_trend = yearly_trends_difference,
    spatiotemporal_trend = spatiotemporal_trends_difference 
  )
  
  assign("out", out, pos = 1)
  
},
cache = cache_filesystem(CACHE)
)

get_yearly_trend_metrics <- function(trends_list){
  
  # Calculate trend metrics
  metrics_df <- purrr::map(1:length(trends_list), function(sp){
    dat <- trends_list[[names(trends_list)[sp]]]$yearly_trend
    out_metrics <- data.frame(
      species = names(trends_list)[sp],
      ## Calculate metrics
      # years above expected in the last 10
      reporting_rate_above_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) > 1.96) %>% tail(10) %>% sum(),
      # years below expected in the last 10
      reporting_rate_below_last_ten = ((dat %>% dplyr::pull(reporting_rate_sd)) < -1.96) %>% tail(10) %>% sum(),
      # 5 year correlation in the last 5
      reporting_rate_trend_last_five = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-5) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1),
      # 5 year correlation in the last 10
      reporting_rate_trend_last_ten = dat %>% dplyr::filter(year >= max(year, na.rm = TRUE)-10) %>% dplyr::mutate(new = cor(reporting_rate_sd, year, method = "spearman")) %>% dplyr::pull(new) %>% head(1)
    )
    out_metrics
  }) %>% bind_rows()
  
  return(metrics_df)
}

plot_yearly_trends <- memoise(function(sd_dat, metric = c("proportion_observations", "proportion_species", "reporting_rate"), ...){
  
  metric <- match.arg(metric)
  reference_taxon <- sd_dat$reference_taxon[1]
    
  # Number of focal observations
  p1 <- ggplot(data = sd_dat, aes(x = year, y = focal_species_count)) + 
    geom_col(fill = "tomato", colour = "transparent", alpha = 0.8, width = 1) + 
    ylab(paste0("Observations of \n", sd_dat$focal_taxon[1])) +
    xlab("") +
    # annotate("text", x = quantile(temporal_trend_data$year, .26), y = max(temporal_trend_data$focal_species_count)+(0.08 *max(temporal_trend_data$focal_species_count)), label = paste0("Observations of ", taxon_data$info$scientificName, rep(" ", num_chars_missing[[1]])), hjust = 1) +
    theme_linedraw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 8)
    )
  
  # Total number of observations of reference taxon
  p2 <- ggplot(data = sd_dat, aes(x = year, y = total_number_observations)) + 
    geom_col(fill = grey(.5), colour = "transparent", alpha = 0.8, width = 1) + 
    ylab(paste0("Observations of \n reference taxa")) +
    xlab("") +
    # annotate("text", x = quantile(temporal_trend_data$year, .24), y = max(temporal_trend_data$total_number_observations)+(0.08 *max(temporal_trend_data$total_number_observations)), label = paste0("Observations of ", referenceTaxon, " ", reference_taxon_name, rep(" ", num_chars_missing[[2]])), hjust = 1) +
    theme_linedraw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 9),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 8)        
    )
  
  metric_labs <- data.frame(metric_name = c("proportion_observations", "proportion_species", "reporting_rate"),
                            metric_lab = c("Proportion of \n observations", "Proportion of \n species", "Reporting rate")
  )
  
  p3 <- sd_dat %>% 
    ggplot(aes(x=year, y=reporting_rate_mn)) +
    geom_bar(stat = "identity", fill = grey(.5), width = 0.6, alpha = 0.8) +
    geom_errorbar(aes(x=year, ymin=reporting_rate_lower, ymax=reporting_rate_upper), width=0.5, colour=grey(.5), alpha=0.8, size=0.5) +
    geom_bar(data = sd_dat, mapping = aes(x=year, y=reporting_rate, group=NULL), stat = "identity", fill = "tomato", colour = "transparent", alpha = 0.5, width = 1) +
    ylab(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab)) +
    xlab("") +
    # annotate("text", x = quantile(temporal_trend_data$year, .21), y = max(temporal_trend_data$proportion_observations)+(0.08 *max(temporal_trend_data$proportion_observations)), label = paste0("Proportion of Observations"), hjust = 1) +
    theme_linedraw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 9),
          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text = element_text(size = 8)
    )
  
  p4 <- sd_dat %>% 
    ggplot(aes(x=year, y=reporting_rate_sd)) +
    geom_ribbon(aes(ymin = -1.96, ymax = 1.96), fill = grey(0.5), alpha = 0.4) +
    geom_hline(yintercept = 0, colour = "black", size = 1) +
    geom_hline(yintercept = 1.96, colour = grey(.5), size = .5) +
    geom_hline(yintercept = -1.96, colour = grey(.5), size = .5) +
    geom_line(colour = "tomato", size = 1.2, alpha = 1) +
    ylab(paste0(metric_labs %>% dplyr::filter(metric_name == metric) %>% dplyr::pull(metric_lab), "\n anomaly")) +
    xlab("Year") +
    # annotate("text", x = quantile(temporal_trend_data$year, .20), y = 1.08, label = paste0("Modeled probability of detection", rep(" ", num_chars_missing[[4]])), hjust = 1) +
    theme_linedraw() +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.title = element_text(size = 9),
          axis.text = element_text(size = 8)
    )
  
  p <- subplot(p1, p2, p3, p4, nrows = 4, shareX = TRUE, titleX = TRUE, titleY = TRUE, margin = 0.01, which_layout = 1)
  
  gg <- plotly_build(p) %>%
    config(displayModeBar = FALSE) %>%
    layout(# plot_bgcolor  = "rgba(0, 0, 0, 0)",
      paper_bgcolor = "rgba(0, 0, 0, 0)",
      font = list(family = "Helvetica", size = 14), 
      xaxis = list(titlefont = list(size = 13),
                   tickfont = list(size = 13)),
      yaxis = list(titlefont = list(size = 13),
                   tickfont = list(size = 13))
    )
  
  return(gg)
  
},
cache = cache_filesystem(CACHE)
)

# Wrapper to generate yearly trends figure
# get_yearly_trend_wrapper <- memoise(function(
#     analysis_records = area_of_interest$gbif_data, 
#     focal_taxon = focal_taxon_name, 
#     ref_taxon_rank = "family",
#     random_iterations = 999,
#     ...
# ){
#   
#   count_dat <- get_count_data(records = analysis_records, focal_taxon_name = focal_taxon, reference_taxon_rank = ref_taxon_rank)
#   detection_dat <- calculate_detection_data(counts = count_dat[-which(rowSums(count_dat %>% dplyr::select(-visitID, -h7, -recordedby, -observationdate, -year)) == 1), ], focal_taxon_name = focal_taxon)
#   yearly_dat <- calculate_trends(detections = detection_dat, grp = "year")
#   yearly_dat_random <- get_randomized_metric(counts = count_dat, iterations = random_iterations, focal_tax = focal_taxon, grp = "year")
#   
#   out <- list(
#     observed = yearly_dat,
#     random = yearly_dat_random
#   )
#   
#   return(out)
# },
# cache = cache_filesystem(CACHE)
# )

# Function to extract temporal bins
# get_temporal_bins <- function(occurrences, number_groups = 3, first_year = 1950){
#   occurrence_years <- occurrences %>% 
#     sf::st_set_geometry(NULL) %>% 
#     dplyr::filter(
#       complete.cases(year),
#       year >= first_year
#     ) %>% 
#     dplyr::pull(year) 
#   years_df <- data.frame(
#     year = occurrence_years,
#     period = ntile(occurrence_years, n = number_groups)
#   )
#   periods_summary <- years_df %>% group_by(period) %>% dplyr::summarize(range = paste0(min(year), "-", max(year)))
#   
#   return(periods_summary)
# }
# 
# # Wrapper to generate spatiotemporal trends data
# get_spatiotemporal_trend_wrapper <- memoise(function(
#     analysis_records = area_of_interest$gbif_data, 
#     focal_taxon = focal_taxon_name, 
#     ref_taxon_rank = "family",
#     random_iterations = 999,
#     resolution = "h6",
#     ...
# ){
# 
#     count_dat <- get_count_data(records = analysis_records, focal_taxon_name = focal_taxon, reference_taxon_rank = ref_taxon_rank)
#     detection_dat <- calculate_detection_data(counts = count_dat, focal_taxon_name = focal_taxon)
#     spatial_dat <- calculate_trends(detections = detection_dat, grp = c("year", resolution))
#     spatial_dat_random <- get_randomized_metric(counts = count_dat, iterations = random_iterations, focal_tax = focal_taxon, grp = c("year", resolution))
# 
#     spatiotemporal_trends <- get_standardized_difference(
#       observed = spatial_dat,
#       randomized = spatial_dat_random,
#       metric = "reporting_rate", 
#       grp = c("year", resolution)
#     ) 
#     
#     spatiotemporal_trends_wide <- spatiotemporal_trends %>% 
#       dplyr::select(all_of(c("year", resolution, "reporting_rate_sd"))) %>% 
#       pivot_wider(names_from = year, values_from = reporting_rate_sd)
#     base_hexes <- h3jsr::cell_to_polygon(spatiotemporal_trends_wide[[resolution]], simple = FALSE)
#     names(base_hexes)[which(names(base_hexes) == "h3_address")] <- resolution
#     spatiotemporal_trends_data <- base_hexes %>%
#       dplyr::left_join(spatiotemporal_trends_wide, by = resolution)
#     # hexes_data
#   
#   return(spatiotemporal_trends_data)
# },
# cache = cache_filesystem(CACHE)
# )
