# =========================
# Search strategy scoring ----
# =========================

# Adapted from Laura Rodríguez Peris et al., 2024
# Behavioural Brain Research, Volume 458, 26 February 2024, 114730

# Note: slight alterations were made to the strategy scoring scheme to account 
# for an 18-hole Barnes maze as opposed to the 20-hole maze used by the authors

search_tests <- function(detail) {

library(tidyverse)

# Example dataframe input
# detail <- data.frame(SHAPE.NAME = c(5, 4, 3, 2, 1, 7, 8, 17, 18, 1, 17, 18, 18, 2, 2, 1),
#                      START.TIME = c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6),
#                      VIDEO = c(rep(c("rat01","rat02"), each = 5), rep("rat03", times=6))
# )

result <- detail %>%
  arrange(VIDEO, START.TIME) %>%
  group_by(VIDEO) %>%
  
  
  summarise(
    SHAPE.ORDER = list({
      x <- SHAPE.NAME
      
      # Convert safely to numeric
      if (is.factor(x)) x <- as.numeric(as.character(x))
      else x <- as.numeric(x)
      
      # Remove consecutive duplicates
      x <- x[c(TRUE, diff(x) != 0)]
      
      # Truncate at first 1 (include it)
      if (any(x == 1)) {
        x <- x[seq_len(match(1, x))]
      }
      
      x
    }),
    .groups = "drop"
  )






# Direction Changes ----
directions <- function(vec, n = 18) {
  # Apply the logic to consecutive pairs in the vector
  sapply(seq_along(vec)[-1], function(i) {
    a <- vec[i - 1]
    b <- vec[i]
    if (b == a) return("N") # No change
    if (abs(b - a) == n / 2) return("O") # Opposite side
    if (b > a) {
      if (b - a > n / 2) return("C") # Clockwise
      if (b - a < n / 2) return("A") # Anticlockwise
    }
    if (b < a) {
      if (b - a > -n / 2) return("C") # Clockwise
      if (b - a < -n / 2) return("A") # Anticlockwise
    }
  })
}

# Step Size ----
stepsize <- function(vec, n = 18) {
  sapply(seq_along(vec)[-1], function(i) {
    a <- vec[i - 1]
    b <- vec[i]
    direction <- directions(c(a, b), n)
    if (direction == "N") return(0)
    if (direction == "O") return(n / 2)
    if (direction == "A" & b > a) return(b - a)
    if (direction == "A" & b < a) return((b + n) - a)
    if (direction == "C" & b < a) return(a - b)
    if (direction == "C" & b > a) return((a + n) - b)
  }
  )
}

# Serial search ----
# Determining length of a serial search where:
# 1) stepsize <= 3
# 2) direction is consistent (A or C as base)
#    ...except: a single opposite-direction step of size 1 is allowed
#    BUT ONLY if the very next step returns to the base direction.
#    If there's no next step (end of vector) or next step fails to return,
#    we include the opposite step and then stop.

relaxedserial <- function(vec, n = 18, count_skips = TRUE, return_details = TRUE) {
  m <- length(vec)
  if (m <= 1) {
    out <- list(index = if (m == 1) 1 else NA_integer_, length = m)
    return(if (return_details) c(out, list(end_index = if (m == 1) 1 else NA_integer_,
                                           elements_len = m,
                                           covered_len = m,
                                           base_dir = NA_character_)) else out)
  }
  
  # Precompute pairwise direction and stepsize using your helpers
  dir <- directions(vec, n)   # length m-1, values "A","C","N","O"
  stp <- stepsize(vec, n)     # length m-1, numeric >= 0
  
  best <- list(index = NA_integer_, length = 0,
               end_index = NA_integer_, elements_len = 0,
               covered_len = 0, base_dir = NA_character_)
  
  # Try every possible start s
  for (s in 1:(m - 1)) {
    elements  <- 1L           # starts with vec[s]
    extras    <- 0L           # sum of max(step-1, 0) across the run
    base_dir  <- NA_character_
    e         <- s            # end index (element position in vec)
    j         <- s            # pair index (step from vec[j] -> vec[j+1])
    
    while (j <= (m - 1)) {
      # Rule 1: stepsize must be <= 3
      if (stp[j] > 3) break
      
      d <- dir[j]
      force_break_after_this_step <- FALSE
      
      # Establish or enforce base direction
      if (is.na(base_dir)) {
        # Set base direction when we first see A or C
        if (d %in% c("A", "C")) {
          base_dir <- d
        }
        # "N" keeps base_dir unset; "O" cannot happen with stp[j] <= 3 (n/2 = 9)
      } else {
        # If we have a base direction, handle deviations
        if (d %in% c("A", "C") && d != base_dir) {
          # Opposite direction allowed ONLY if step == 1 AND we return immediately next
          if (stp[j] > 1) {
            # Wrong direction by more than 1 is not allowed
            break
          } else {
            # stp[j] == 1: check immediate correction on next step (if any)
            if (j == (m - 1)) {
              # No next step available — include this one-step opposite move and end
              force_break_after_this_step <- TRUE
            } else if (dir[j + 1] != base_dir) {
              # Next step does not return to base direction — include this opposite step and end
              force_break_after_this_step <- TRUE
            }
            # If next step DOES return to base_dir, we simply continue (include both steps)
          }
        }
        # If d == base_dir or d == "N", it's fine (subject to stp[j] <= 3)
      }
      
      # Include current step
      elements <- elements + 1L
      extras   <- extras + max(stp[j] - 1L, 0L)
      e        <- j + 1L
      j        <- j + 1L
      
      if (force_break_after_this_step) break
    }
    
    covered <- elements + extras
    metric  <- if (count_skips) covered else elements
    
    # Track the best (longest) run; tie-breaker keeps earliest start
    if (metric > best$length) {
      best <- list(index = s, length = metric,
                   end_index = e, elements_len = elements,
                   covered_len = covered, base_dir = base_dir)
    }
  }
  
  if (return_details) return(best) else return(list(index = best$index, length = best$length))
}



result <- result %>%
  mutate(
    serial_length = map_int(SHAPE.ORDER, ~ relaxedserial(.x)$length),
    serial_index  = map_int(SHAPE.ORDER, ~ relaxedserial(.x)$index)
  )


# Reversals ----

directionchanges <- function(vec) {
  dvec <- directions(vec)
  if(length(vec)<3) return(0)
  sapply(seq_along(dvec)[-1], function(i) {
    # Start with previous index
    j <- i - 1
    
    # Move backward until we find a non-"N" for 'a' or reach the start
    while (j > 0 && dvec[j] == "N") {
      j <- j - 1
    }
    
    # If we reached the start and everything was "N", return 0
    if (j == 0) return(0)
    
    a <- dvec[j]
    b <- dvec[i]
    
    # Apply your logic
    if (b == a || b == "N") return(0)
    if (b != a) return(1)
  })
}

reversals <- function(vec){
  sum(directionchanges(vec))
}

result <- result %>%
  mutate(reversals = sapply(SHAPE.ORDER, reversals))

# Score Video ----

score_video <- function(order) {
  bestscore <- 0
  bestreason <- "Not scored"
  
  # Condition 1: Exact match c(1)
  if (identical(order, c(1))) {
    bestscore <- 11
    bestreason <- "Direct"
  }
  
  # Condition 2: Exact match c(2, 1) or c(18, 1)
  if (identical(order, c(2, 1)) || identical(order, c(18, 1))) {
    currentscore <- 10
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Short correction 1"
    }
  }
    
  
  # Condition 3: Exact match c(3, 2, 1) or c(17, 18, 1) or c(3, 1) or c(17, 1)
  if (identical(order, c(3, 2, 1)) || identical(order, c(17, 18, 1)) ||
      identical(order, c(3, 1)) || identical(order, c(17, 1))
      ) {
    currentscore <- 9
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Short correction 1"
    }
  }
  
  
  # Condition 4: Long correction
  # Requires 1–3 numbers from 8:12, then ending with one of the acceptable endings
  acceptable_endings <- list(
    c(1),
    c(2, 1),
    c(18, 1),
    c(17, 1),
    c(17, 18, 1),
    c(3, 1),
    c(3, 2, 1)
  )
  
  is_long_correction <- function(order) {
    for (end in acceptable_endings) {
      Lend <- length(end)
      L <- length(order)
      prefix_len <- L - Lend
      
      # Require 1–3 numbers in the prefix, all drawn from 8:12
      if (prefix_len >= 1 && prefix_len <= 3 &&
          Lend <= L &&
          identical(tail(order, Lend), end) &&
          all(order[seq_len(prefix_len)] %in% 8:12)) {
        return(TRUE)
      }
    }
    FALSE
  }
  
  if (is_long_correction(order)) {
    currentscore <- 8
    if (currentscore > bestscore) {
      bestscore <- currentscore
      bestreason <- "Long correction"
    }
  }
  
  
  
  # Condition 5: Medium correction
  # (Right: first in 4:7, then 0–2 in 3:8) OR (Left: first in 13:16, then 0–2 in 12:17),
  # followed by an acceptable ending.
  
  is_medium_correction <- function(order) {
    right_quad <- 4:7
    right_quad_extended <- 3:8
    left_quad  <- 13:16
    left_quad_extended  <- 12:17
    
    for (end in acceptable_endings) {
      L     <- length(order)
      Lend  <- length(end)
      prefix_len <- L - Lend
      
      # Require 1–3 numbers in the quadrant+semicircle "suffix" before the ending
      if (prefix_len >= 1 && prefix_len <= 3 &&
          Lend <= L &&
          identical(tail(order, Lend), end)) {
        
        prefix <- order[seq_len(prefix_len)]
        
        # Right case: first in right quadrant, remaining (if any) in right semicircle
        right_ok <- (prefix[1] %in% right_quad) &&
          (prefix_len == 1 || all(prefix[-1] %in% right_quad_extended))
        
        # Left case: first in left quadrant, remaining (if any) in left semicircle
        left_ok  <- (prefix[1] %in% left_quad) &&
          (prefix_len == 1 || all(prefix[-1] %in% left_quad_extended))
        
        if (right_ok || left_ok) return(TRUE)
      }
    }
    FALSE
  }
  
  if (is_medium_correction(order)) {
    currentscore <- 7
    if (currentscore > bestscore) {
      bestscore <- currentscore
      bestreason <- "Medium correction"
    }
  }
  
  
  # Serial conditions:
  
  serial_length <- relaxedserial(order)$length
  serial_fraction <- relaxedserial(order)$elements_len / length(order)
  reversals <- reversals(order)
  
  
  serial <- ifelse(serial_fraction >= 0.8, TRUE, FALSE)
  
  if (serial == TRUE && serial_length %in% 4:5) {
    currentscore <- 8
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Short serial"
    }
  }
    
  if (serial == TRUE && serial_length %in% 6:7) {
    currentscore <- 7
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Serial 1"
    }
  }
  
  if (serial == TRUE && serial_length %in% 8:10) {
    currentscore <- 6
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Serial 2"
    }
  }
  
  if (serial == TRUE && serial_length %in% 11:14) {
    currentscore <- 5
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Serial 3"
    }
  }
  
  if (serial == TRUE && serial_length > 15) {
    currentscore <- 4
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Serial 4"
    }
  }
  
  if (serial_fraction >= 0.5 && reversals == 1) {
    currentscore <- 3
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Combined 1"
    }
  }
  
  if (serial_fraction >= 0.3 && reversals > 1) {
    currentscore <- 2
    if(currentscore > bestscore){
      bestscore <-  currentscore
      bestreason <- "Combined 2"
    }
  }
  
  currentscore <- 1
  if(currentscore > bestscore){
    bestscore <-  currentscore
    bestreason <- "Random"
  }
  
  
  return(list(score = bestscore, reason = bestreason))
}
  
  





# Output ----
result <- result %>%
mutate(
  score = map_int(SHAPE.ORDER, ~ score_video(.x)$score),
  reason  = map_chr(SHAPE.ORDER, ~ score_video(.x)$reason))
}
