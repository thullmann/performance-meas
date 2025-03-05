# f = estimate, g = ground truth 

expectation = function(f, g, lower, upper, epsilon = 0.05, loss = "difference") {
  
  x = seq(lower + 0.00001, upper, by = 0.00001) 
  dx = 0.00001
  
  if (loss == "difference") {
    sum(dx * (f(x) - g(x))) # integral approximated by rectangles (right-hand Riemann sums) 
    # right-hand sums preferable to left-hand sums because fractional polynomials may not be defined at zero

  } else if (loss == "absolute") {
    sum(dx * abs(f(x) - g(x)))

  } else if (loss == "squared") {
    sum(dx * (f(x) - g(x))^2)
    
  } else if (loss == "epsilon") {
    sum(dx * ifelse(abs(f(x) - g(x)) <= epsilon, 1, 0))

  }
  
}

expectation_dFx = function(f, g, dens, lower, upper, epsilon = 0.05, loss = "difference") {
  x = seq(lower + 0.00001, upper, by = 0.00001)
  # x = seq(lower, upper, by = 0.00001)
  dx = 0.00001
  
  if (loss == "difference") {
    sum(dx * (f(x) - g(x))*dens(x)) # integral approximated by rectangles (right-hand Riemann sums) 
    
  } else if (loss == "absolute") {
    sum(dx * abs(f(x) - g(x))*dens(x))
    
  } else if (loss == "squared") {
    sum(dx * (f(x) - g(x))^2 *dens(x))
    
  } else if (loss == "epsilon") {
    sum(dx * ifelse(abs(f(x) - g(x)) <= epsilon, 1, 0)*dens(x))
    
  }
  
}

median_dFx = function(f, g, rand_x, epsilon = 0.05, loss = "difference") {
  
  if (loss == "difference") {
    median(f(rand_x) - g(rand_x))
    
  } else if (loss == "absolute") {
    median(abs(f(rand_x) - g(rand_x)))
    
  } else if (loss == "squared") {
    median((f(rand_x) - g(rand_x))^2)
    
  } else if (loss == "epsilon") {
    median(ifelse(abs(f(rand_x) - g(rand_x)) <= epsilon, 1, 0))

  }
  
}

quantile_dFx = function(f, g, rand_x, probs = 0.5, epsilon = 0.05, loss = "difference") {
  
  if (loss == "difference") {
    quantile(f(rand_x) - g(rand_x), probs = probs)
    
  } else if (loss == "absolute") {
    quantile(abs(f(rand_x) - g(rand_x)), probs = probs)
    
  } else if (loss == "squared") {
    quantile((f(rand_x) - g(rand_x))^2, probs = probs)
    
  } else if (loss == "epsilon") {
    quantile(ifelse(abs(f(rand_x) - g(rand_x)) <= epsilon, 1, 0), probs = probs)
    
  }
  
} 

maxmin = function(f, g, lower, upper, epsilon = 0.05, loss = "difference", maximum = TRUE) {
  
  if (is.nan(f(0))) {
    x = seq(lower + 0.00001, upper, by = 0.00001) # start at 0 + 0.00001, because fractional polynomials may not be defined at zero
  } else {
    x = seq(lower, upper, by = 0.00001)
  }
  
  if (is.infinite(f(0)) & lower == 0 & loss != "epsilon") {
    if (maximum & loss == "difference" & f(0) > 0) {
      Inf
    } else if (!maximum & loss == "difference" & f(0) > 0) {
      min(f(x) - g(x))
    } else if (maximum & loss != "difference" & f(0) > 0) {
      Inf
    } else if (!maximum & loss != "difference" & f(0) > 0) {
      ifelse(loss == "absolute", min(abs(f(x) - g(x))), min((f(x) - g(x))^2))
    } else if (maximum & loss == "difference" & f(0) < 0) {
      max(f(x) - g(x))
    } else if (!maximum & loss == "difference" & f(0) < 0) {
      -Inf
    } else if (maximum & loss != "difference" & f(0) < 0) {
      Inf
    } else if (!maximum & loss != "difference" & f(0) < 0) {
      ifelse(loss == "absolute", min(abs(f(x) - g(x))), min((f(x) - g(x))^2))
    }
  } else if (loss == "difference") {
    ifelse(maximum, max(f(x) - g(x)), min(f(x) - g(x)))
  } else if (loss == "absolute") {
    ifelse(maximum, max(abs(f(x) - g(x))), min(abs(f(x) - g(x))))  } 
  else if (loss == "squared") {
    ifelse(maximum, max((f(x) - g(x))^2), min((f(x) - g(x))^2))
  } else if (loss == "epsilon") {
    h = function(x) ifelse(abs(f(x) - g(x)) <= epsilon, 1, 0)
    ifelse(maximum, max(h(x)), min(h(x)))
  }
}

# roots_f, roots_g: vector of roots 
nr_roots = function(roots_f, roots_g, epsilon = 1, loss = "difference") {
  
  # convention for roots on a segment
  # if f, g are 0 on the whole interval [lower, upper]: length(roots_f) - length(roots_g) = 0 
  # if only one of f, g is 0 on the whole interval [lower, upper]: l_roots_f - l_roots_g = +- Inf  
  
  if (!grepl("segment", roots_f[1]) & !grepl("segment", roots_g[1])) {
    diff_length = length(roots_f) - length(roots_g)
  } else if (grepl("segment", roots_f[1]) & !grepl("segment", roots_g[1])) {
    diff_length = Inf
  } else if (grepl("segment", roots_g[1]) & !grepl("segment", roots_f[1])) {
    diff_length = -Inf
  } else if (grepl("segment_all", roots_f[1]) & grepl("segment_all", roots_g[1])) {
    diff_length = 0
  } else { # e.g., if both are "segment_other", or one is segment_all and the other one is segment_other
    diff_length = NA 
  }
  
  if (loss == "difference") {
    diff_length
  } else if (loss == "absolute") {
    abs(diff_length)
  } else if (loss == "squared") {
    (diff_length)^2
  } else if (loss == "epsilon") {
    ifelse(abs(diff_length) <= epsilon, 1, 0)
  }
}

# optimum_f, optimum_g: x-location of either GLOBAL maximum or GLOBAL minimum of f, g
location_max_min = function(f, g, lower, upper, epsilon = 0.05, loss = "difference", maximum = TRUE) {
  
  if (is.nan(f(0))) {
    search_grid = seq(lower + 0.00001, upper, 0.00001) # start at 0 + 0.00001, because fractional polynomials may not be defined at zero
  } else {
    search_grid = seq(lower, upper, 0.00001)
  }

  if (maximum) {
    optimum_f = search_grid[which(f(search_grid) == max(f(search_grid)))]
    optimum_g = search_grid[which(g(search_grid) == max(g(search_grid)))]
  } else {
    optimum_f = search_grid[which(f(search_grid) == min(f(search_grid)))]
    optimum_g = search_grid[which(g(search_grid) == min(g(search_grid)))]
  }
  
  # if more than one location (e.g. for piecewise constant functions), take mean of locations
  if (length(optimum_f) > 1) optimum_f = mean(optimum_f)
  if (length(optimum_g) > 1) optimum_g = mean(optimum_g)
  
  if (loss == "difference") {
    optimum_f - optimum_g
  } else if (loss == "absolute") {
    abs(optimum_f - optimum_g)
  } else if (loss == "squared") {
    (optimum_f - optimum_g)^2
  } else if (loss == "epsilon") {
    ifelse(abs(optimum_f - optimum_g) <= epsilon, 1, 0)
  }
}

local_at_x = function(f, g, x, epsilon = 0.05, loss = "difference") {
  if (loss == "difference") {
    f(x) - g(x)
  } else if (loss == "absolute") {
    abs(f(x) - g(x))
  } else if (loss == "squared") {
    (f(x) - g(x))^2
  } else if (loss == "epsilon") {
    ifelse(abs(f(x) - g(x)) <= epsilon, 1, 0)
  }
}
