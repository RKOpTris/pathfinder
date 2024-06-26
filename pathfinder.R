generate_points <- function(n, mean = 0, sd = 1){
  data.frame(waypoint = letters[1:n],
             x = rnorm(n, mean, sd),
             y = rnorm(n, mean, sd))
}

generate_paths <- function(){
  waypoints <- my_points$waypoint
  p1 <- c()
  while(length(unique(p1)) < length(waypoints)){
    p1 <- c(p1, sample(waypoints, 1))
  }
  p2 <- sample(waypoints, length(p1), replace = T)
  paths <- data.frame(p1, p2)
  #remove p1 == p2
  paths <- paths[-which(paths$p1 == paths$p2), ]
  row.names(paths) <- NULL
  #remove duplicates
  duplicates <- c()
  for(i in 1:(nrow(paths) - 1)){
    p2 <- paths$p1[i]
    p1 <- paths$p2[i]
    rem <- paths[(i + 1):nrow(paths), ]
    dup <- rem[rem$p1 == p1 & rem$p2 == p2, ]
    if(length(dup) < 0){
      next
    } else {
      duplicates <- c(duplicates, as.numeric(row.names(dup)))
    }
  }
  paths <- paths[-duplicates, ]
  paths
}

get_pos <- function(x, waypoint){
  as.numeric(x[x$waypoint == waypoint, c("x", "y")])
}

visualise_paths <- function(){
  plot(y ~ x, my_points, type = "n", las = 1)
  text(y ~ x, my_points, labels = waypoint, cex = 2)
  
  a2b <- my_paths
  for(i in 1:nrow(a2b)){
    p1 <- a2b$p1[i]
    p2 <- a2b$p2[i]
    a <- get_pos(my_points, p1)
    b <- get_pos(my_points, p2)
    points(a[1], a[2], col = "red", pch = 16)
    arrows(a[1], a[2], b[1], b[2], length = 0, lty = 2)
  }
  
}

reset_global_vars <- function(start_waypoint){
  global_vars$total_runs <- 0
  global_vars$total_attempts <- 0
  global_vars$max_runs <- 100000
  global_vars$successful_routes <- list()
  global_vars$successful_distances <- numeric()
  global_vars$successful_runs <- numeric()
  global_vars$run_code <- character()
  global_vars$start_waypoint <- start_waypoint
  global_vars$max_visits <- 3
  global_vars$number_successes <- 0
  global_vars$number_fails <- 0
  global_vars$best_distance_success <- Inf
  global_vars$search_runtime <- 0
}

reset_run_vars <- function(){
  run_vars$current_waypoint <- NA
  run_vars$distance_travelled <- 0
  run_vars$route_travelled <- c()
  run_vars$all_points_visited <- F
  run_vars$fail_reason <- ""
  run_vars$search_runs <- 0
  run_vars$waypoint_probs <- matrix(1, nrow = nrow(my_points), dimnames = list(my_points$waypoint))
}

fail_run <- function(record_all_codes){
  #message(paste("Search failed because", run_vars$fail_reason))
  #message(global_vars$total_runs)
  global_vars$number_fails <- global_vars$number_fails + 1
  if(record_all_codes){
    error_code <- ifelse(run_vars$fail_reason == "exceeded best distance", "F-XD", "F-MW")
    global_vars$run_code <- c(global_vars$run_code, error_code)
  }
  reset_run_vars()
  global_vars$total_attempts <- global_vars$total_attempts + 1
}

succeed_run <- function(record_all_codes){
  message(paste0(global_vars$total_runs, ": path found!"))
  global_vars$number_successes <- global_vars$number_successes + 1
  global_vars$successful_routes[[global_vars$number_successes]] <- c(global_vars$start_waypoint, run_vars$route_travelled)
  global_vars$successful_distances[global_vars$number_successes] <- run_vars$distance_travelled
  global_vars$successful_runs <- c(global_vars$successful_runs, global_vars$total_runs)
  if(record_all_codes){
    global_vars$run_code <- c(global_vars$run_code, "S")
  }
  reset_run_vars()
  global_vars$total_attempts <- global_vars$total_attempts + 1
}

global_vars <- new.env()
run_vars <- new.env()

init_points <- function(n_points = 10, my_seed = 1138, seed_n = 2){
  set.seed(my_seed)
  for(i in 1:seed_n){
    my_points <- generate_points(n_points, 0)
  }
  my_points
}

init_paths <- function(){
  my_paths <- generate_paths()
  distances <- my_points %>% tibble::column_to_rownames("waypoint") %>% dist(upper = T, diag = T) %>% as.matrix()
  for(i in 1:nrow(my_paths)){
    my_paths$distance[i] <- get_distance(my_paths$p1[i], my_paths$p2[i])
  }
  my_paths
}

get_distance <- function(p1, p2){
  distances <- my_points %>% tibble::column_to_rownames("waypoint") %>% dist(upper = T, diag = T) %>% as.matrix()
  distances[p1, p2]
}

report_success <- function(){
  list(
    search_runtime <- global_vars$search_runtime,
    total_runs = global_vars$total_runs,
    best_distance_success = global_vars$best_distance_success,
    best_route = global_vars$successful_routes[[length(global_vars$successful_routes)]],
    successful_distances = global_vars$successful_distances,
    successful_routes = global_vars$successful_routes,
    successful_runs = global_vars$successful_runs,
    run_code = global_vars$run_code
  )
}

find_path <- function(start_waypoint = "a", max_visits = 3, max_runs = 100000, record_all_codes = T, visit_penalty = 0.25){
  start_time <- Sys.time()
  reset_run_vars()
  reset_global_vars(start_waypoint)
  global_vars$start_waypoint <- start_waypoint
  global_vars$max_runs <- max_runs
  global_vars$max_visits <- max_visits
  #while(global_vars$total_runs < global_vars$max_runs){
  while(global_vars$number_successes < 20 & global_vars$total_runs < global_vars$max_runs){
    
    if(global_vars$total_runs == 0){
      reset_run_vars()
    }
    
    global_vars$total_runs <- global_vars$total_runs + 1
    run_vars$search_runs <- run_vars$search_runs + 1
    
    # travelling loop
    if(run_vars$distance_travelled == 0){
      (run_vars$current_waypoint <- global_vars$start_waypoint)
      run_vars$waypoint_probs[run_vars$current_waypoint, 1] <- run_vars$waypoint_probs[run_vars$current_waypoint, 1] * visit_penalty
    } else {
      (run_vars$current_waypoint <- run_vars$route_travelled[length(run_vars$route_travelled)])
      run_vars$waypoint_probs[run_vars$current_waypoint, 1] <- run_vars$waypoint_probs[run_vars$current_waypoint, 1] * visit_penalty
      ##### this may not be the best place to put this!
      if(run_vars$distance_travelled > global_vars$best_distance_success){
        run_vars$fail_reason <- "exceeded best distance"
        fail_run(record_all_codes)
        next
      }
    }
    
    successful_runs <- length(global_vars$successful_distances)
    if(successful_runs > 4 && length(table(global_vars$successful_distances[length(global_vars$successful_distances):(length(global_vars$successful_distances) - 2)])) == 1){
      report_success()
      print("Best solution likely found!")
      break
    }
    
    if(length(table(run_vars$route_travelled)) == nrow(my_points)){
      run_vars$all_points_visited <- T
    }
    
    if(run_vars$all_points_visited && run_vars$current_waypoint == global_vars$start_waypoint){
      global_vars$best_distance_success <- run_vars$distance_travelled
      succeed_run(record_all_codes)
      next
    }
    if(length(run_vars$route_travelled) > 0){
      if(any(table(run_vars$route_travelled) > global_vars$max_visits)){
        run_vars$fail_reason <- "visiting waypoint too many times"
        fail_run(record_all_codes)
        next
      }
    }
    # get waypoint options and select one at random
    all_paths <- rbind(my_paths,
                       rename(my_paths[c(2, 1, 3)], p1 = p2, p2 = p1))
    (possible_destinations <- all_paths %>% filter(p1 == run_vars$current_waypoint))
    run_vars$waypoint_probs
    (weighted_probs <- run_vars$waypoint_probs[possible_destinations$p2, 1])
    (weighted_probs <- weighted_probs / sum(weighted_probs))
    # travel
    (chosen_destination <- possible_destinations[sample(1:nrow(possible_destinations), 1, prob = weighted_probs), ])
    # update route_travelled
    (run_vars$route_travelled <- c(run_vars$route_travelled, chosen_destination$p2))
    # update distance_travelled
    (run_vars$distance_travelled <- run_vars$distance_travelled + chosen_destination$distance)
  }
  global_vars$search_runtime <- Sys.time() - start_time
  report_success()
}

str_to_waypoints <- function(string){
  string %>% str_split("") %>% unlist()
}

waypoints_to_str <- function(waypoints){
  paste(waypoints, collapse = "")
}

get_path <- function(charvec){
  data.frame(p1 = charvec[1:(length(charvec) - 1)],
             p2 = charvec[2:length(charvec)])
}

plot_path <- function(charvec){
  path_taken <- get_path(charvec)
  p1 <- path_taken$p1
  p2 <- path_taken$p2
  path_taken1 <- my_points[match(p1, my_points$waypoint), ]
  path_taken2 <- my_points[match(p2, my_points$waypoint), ]
  plot_cols <- scico::scico(length(charvec), palette = "hawaii")
  arrows(path_taken1$x, path_taken1$y, path_taken2$x, path_taken2$y, col = plot_cols, lwd = 3)
  points(path_taken1$x[1], path_taken1$y[1], cex = 8, lwd = 3)
}

plot_path(run2$best_route)

get_path_length <- function(charvec){
  path_taken <- get_path(charvec)
  for(i in 1:nrow(path_taken)){
    path_taken$distance[i] <- get_distance(path_taken$p1[i], path_taken$p2[i])
  }
  sum(path_taken$distance)
}

# set a seed within an environment without effecting the global environment

withSeed <- function(expr, seed = 42) {
  # inspired by Romain François, Theodore Lytras, Gwang-Jin Kim on Stack Exchange
  # https://stackoverflow.com/questions/14324096/setting-seed-locally-not-globally-in-r
  if(!exists(".Random.seed")){runif(1); existing.seed <- NULL}
  existing.seed <- .Random.seed
  on.exit({assign(".Random.seed", existing.seed, envir = .GlobalEnv)})
  set.seed(seed)
  expr
}

my_points <- init_points()
my_paths <- init_paths()
visualise_paths()
(run1 <- find_path(max_runs = 50000, visit_penalty = 1))
plot_path(run1$best_route)

my_points <- init_points()
my_paths <- init_paths()
my_paths <- init_paths()
visualise_paths()
run2 <- withSeed(find_path(start_waypoint = "i", max_visits = 3, max_runs = 50000))

# my answer
"ihcdfgfabjechi" %>% str_to_waypoints() %>% get_path_length()
"ihcdfgfabjechi" %>% str_to_waypoints() %>% plot_path()

c("adgejhcbdgfa") %>% str_to_waypoints() %>% get_path_length()
c("afgdeihfcbjigda") %>% str_to_waypoints() %>% get_path_length()

# discovered answer
run2$best_route %>% get_path_length()
visualise_paths()
plot_path(run2$best_route)

#measuring model performance

error_code_summary <- function(run_object){
  start_runs <- which(run_object$run_code == "S") - 1
  run_analysis <- run_object$run_code %>% paste(collapse = ",") %>% str_split(",S,") %>% sapply(str_split, ",")
  run_analysis <- sapply(run_analysis, table)
  lapply(run_analysis, function(x){
    y <- x %>% data.frame()
    names(y) <- c("Error", "Frequency")
    y$Proportion <- y$Frequency / sum(y$Frequency)
    y[y$Error != "S", ]
  })
}

error_code_for_plot <- function(error_code_summary_data){
  do.call(rbind, lapply(1:length(error_code_summary_data), function(x){
    y <- error_code_summary_data[[x]]
    y$Run <- x
    y
  }))
}

plot_error_codes <- function(x){
  x %>% 
    error_code_summary %>% 
    error_code_for_plot %>%
    ggplot(aes(Run, Proportion, fill = Error)) + 
    scale_y_continuous(breaks = seq(0, 1, 0.1)) +
    scale_x_continuous(breaks = seq(1, length(x$successful_runs))) +
    geom_bar(stat = "identity", position = "fill") +
    theme_bw() +
    theme(axis.title = element_text(size = 25),
          axis.text = element_text(size = 15),
          legend.title = element_text(size = 25),
          legend.text = element_text(size = 15))
}

plot_error_codes(run2)

run1$run_code

### weight choices so that waypoints that have not been visited are more preferable
### should get to start point by quickest means once all sites visited
### should the algorithm START by searching for paths just using 1 waypoint, then if it fails, expand to 2, etc.
### can the algorithm LEARN in this way?
### or could the algorithm learn in terms of best waypoint combinations plus distance covered, whilst minimising 
###### repeated waypoint visits, *particularly if they are immediate*. maybe implement a gradual cooldown as other
###### waypoints are visited?
### certainly don't let a ping pong, e.g., E, D, E, then D again. E, D, E, is okay if D is a "spoke"
### if a good solution has been found then stop
### can the code work faster?
### plot the path found
### include the original waypoint
### can I interpolate checkpoints at line crossings? (as an option)
### can I run this in parallel?

########### other uses
# fuel consumption
# military spreading movements based on static (at least to begin with) difficulty











