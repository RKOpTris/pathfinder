withSeed <- function(expr, seed = 42) {
  # inspired by Romain François, Theodore Lytras, Gwang-Jin Kim on Stack Exchange
  # https://stackoverflow.com/questions/14324096/setting-seed-locally-not-globally-in-r
  if(!exists(".Random.seed")){runif(1); existing.seed <- NULL}
  existing.seed <- .Random.seed
  on.exit({assign(".Random.seed", existing.seed, envir = .GlobalEnv)})
  set.seed(seed)
  expr
}