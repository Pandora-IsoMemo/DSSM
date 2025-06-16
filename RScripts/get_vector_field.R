# Load necessary packages
library(terra)
library(ggplot2)
library(dplyr)

# Create an example raster ----

# Example data.frame
set.seed(123)  # Set seed for reproducibility

r_df <- data.frame(
  latitude = runif(100, min = 30, max = 70),
  longitude = runif(100, min = -10, max = 70),
  Est = runif(100, min = -22, max = -14)
)

# Order the data by latitude and then by longitude
r_df <- r_df %>% arrange(latitude, longitude)

# Convert data.frame to a SpatVector
r_vect <- vect(r_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Create an empty raster grid with the extent and resolution you want
r <- rast(
  extent = c(min(r_df$longitude), max(r_df$longitude), min(r_df$latitude), max(r_df$latitude)),
  resolution = c((max(r_df$longitude) - min(r_df$longitude)) / 10, (max(r_df$latitude) - min(r_df$latitude)) / 10)
)

# Rasterize the SpatVector onto the raster grid using the "Est" field
r <- terra::rasterize(r_vect, r, field = "Est", fun = mean)

# Define a function to calculate a vector field ----

# Calculate a vector field
#
# This function is a general implementation of the vector field calculation suggested by chatGPT.
# It calculates the differences between a cell and its neighbors and determines the direction and magnitude
# of the maximum difference. The function can handle different types of differences: absolute, positive, or negative.
# The function iterates over each cell in the raster and computes the differences with its neighbors.
# It then selects the neighbor with the maximum difference and calculates the direction and magnitude of the vector.
# The function returns a data frame with columns for longitude, latitude, direction, and magnitude.
# The function is not optimized for speed and may be slow for large rasters.
#
# @param r A raster object
# @param difference_type The type of difference to consider: "absolute", "positive", or "negative".
#  - "absolute": Considers the absolute value of differences, regardless of sign
#  - "positive": Considers positive differences, where the neighbor is greater than the current cell.
#  - "negative": Considers negative differences, where the neighbor is less than the current cell.
#
# @return A data frame with columns for longitude, latitude, direction, and magnitude.
get_vector_field <- function(r, difference_type = c("absolute", "positive", "negative")) {
  difference_type <- match.arg(difference_type)

  # Initialize matrices for direction and magnitude
  directions <- matrix(NA, nrow=nrow(r), ncol=ncol(r))
  magnitudes <- matrix(NA, nrow=nrow(r), ncol=ncol(r))

  # Define directions (D8 neighbors)
  d8_offsets <- expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1))
  d8_offsets <- d8_offsets[!(d8_offsets$dx == 0 & d8_offsets$dy == 0), ]

  # Iterate over each cell
  for (row in 2:(nrow(r)-1)) {
    for (col in 2:(ncol(r)-1)) {
      # Get current cell value
      val <- r[row, col]

      # Calculate differences for each neighbor
      diffs <- sapply(1:nrow(d8_offsets), function(i) {
        dr <- row + d8_offsets$dx[i]
        dc <- col + d8_offsets$dy[i]
        neighbor_val <- r[dr, dc]

        # Check if both the cell and neighbor are numeric values
        if (!is.na(val) && !is.na(neighbor_val)) {
          # Calculate weighted difference
          difference <- sqrt(d8_offsets$dx[i]^2 + d8_offsets$dy[i]^2) * (neighbor_val - val)

          # Apply difference_type filter
          if (difference_type == "positive" && difference > 0) {
            return(difference)
          } else if (difference_type == "negative" && difference < 0) {
            return(difference)
          } else if (difference_type == "absolute") {
            return(abs(difference))
          } else {
            return(NA)
          }
        } else {
          return(NA)  # Return NA if either cell is NA
        }
      }) %>% unlist(use.names = FALSE)  # Ensure it's a simple vector

      # Filter out NA values from diffs
      diffs <- diffs[!is.na(diffs)]

      # Only proceed if there are valid differences
      if (length(diffs) > 0) {
        max_diff_index <- which.max(abs(diffs))
        # Adjust direction calculation for raster grid by reversing y direction
        directions[row, col] <- atan2(-d8_offsets$dy[max_diff_index], d8_offsets$dx[max_diff_index])
        magnitudes[row, col] <- abs(diffs[max_diff_index])
      }
    }
  }

  # Convert directions and magnitudes to a data frame for plotting
  expand.grid(row=1:nrow(r), col=1:ncol(r)) %>%
    mutate(
      longitude = terra::xFromCol(r, col),
      latitude = terra::yFromRow(r, row),
      direction = directions[cbind(row, col)],
      magnitude = magnitudes[cbind(row, col)]
    ) %>%
    filter(!is.na(direction))
}

# Calculate a vector field (optimized version)
#
# This function is an optimized version of the get_vector_field function that uses vectorized operations
# to compute the vector field for a raster. See the get_vector_field function for more details on the
# calculation method. This optimized version is faster and more efficient for large rasters.
#
# @param r A raster object
# @param difference_type The type of difference to consider: "absolute", "positive", or "negative".
#  - "absolute": Considers the absolute value of differences, regardless of sign
#  - "positive": Considers positive differences, where the neighbor is greater than the current cell.
#  - "negative": Considers negative differences, where the neighbor is less than the current cell.
#
# @return A data frame with columns for longitude, latitude, direction, and magnitude.
get_vector_field_fast <- function(r, difference_type = c("absolute", "positive", "negative")) {
  difference_type <- match.arg(difference_type)

  # Define D8 neighbors
  d8_offsets <- expand.grid(dx = c(-1, 0, 1), dy = c(-1, 0, 1)) %>%
    filter(!(dx == 0 & dy == 0)) %>%
    as.matrix()

  # Initialize matrices for direction and magnitude
  directions <- matrix(NA, nrow = nrow(r), ncol = ncol(r))
  magnitudes <- matrix(NA, nrow = nrow(r), ncol = ncol(r))

  # Vectorized computation for all cells, excluding borders
  cells <- expand.grid(row = 2:(nrow(r) - 1), col = 2:(ncol(r) - 1))

  compute_difference <- function(cell) {
    row <- cell$row
    col <- cell$col
    val <- r[row, col]

    if (is.na(val)) return(c(NA, NA)) # Skip if central cell is NA

    # Compute differences with each neighbor
    diffs <- lapply(1:nrow(d8_offsets), function(i) {
      dr <- row + d8_offsets[i, "dx"]
      dc <- col + d8_offsets[i, "dy"]
      neighbor_val <- r[dr, dc]

      # Skip if neighbor is NA
      if (is.na(neighbor_val)) return(NA)

      # Weighted difference calculation
      diff <- sqrt(d8_offsets[i, "dx"]^2 + d8_offsets[i, "dy"]^2) * (neighbor_val - val)

      # Apply difference_type filter
      if ((difference_type == "positive" && diff > 0) ||
          (difference_type == "negative" && diff < 0) ||
          difference_type == "absolute") {
        return(diff)
      } else {
        return(NA)
      }
    })

    # Remove NA values and calculate max difference
    diffs <- unlist(diffs, use.names = FALSE)
    diffs <- diffs[!is.na(diffs)]

    if (length(diffs) == 0) return(c(NA, NA)) # No valid differences found

    # Determine direction and magnitude for the largest difference
    max_diff_index <- which.max(abs(diffs))
    direction <- atan2(-d8_offsets[max_diff_index, "dy"], d8_offsets[max_diff_index, "dx"])
    magnitude <- abs(diffs[max_diff_index])

    c(direction, magnitude)
  }

  # Apply to all cells
  results <- do.call(rbind, lapply(split(cells, seq(nrow(cells))), compute_difference))

  # Populate direction and magnitude matrices
  directions[as.matrix(cells)] <- results[, 1]
  magnitudes[as.matrix(cells)] <- results[, 2]

  # Create data frame for output
  output <- expand.grid(row = 1:nrow(r), col = 1:ncol(r)) %>%
    mutate(
      longitude = terra::xFromCol(r, col),
      latitude = terra::yFromRow(r, row),
      direction = directions[cbind(row, col)],
      magnitude = magnitudes[cbind(row, col)]
    ) %>%
    filter(!is.na(direction))

  return(output)
}


# Example usage ----

# Get vector field

# Example usage with absolute differences (default)
system.time({
  vector_field <- get_vector_field(r, difference_type = "absolute")
})

system.time({
  vector_field <- get_vector_field_fast(r, difference_type = "absolute")
})

# Example usage with positive differences
#vector_field <- get_vector_field(r, difference_type = "positive")

# Example usage with negative differences
#vector_field <- get_vector_field(r, difference_type = "negative")

# Plot the vector field
arrow_scaling <- 1
ggplot(vector_field, aes(x=longitude, y=latitude)) +
  geom_segment(aes(xend = longitude + arrow_scaling * magnitude * cos(direction),
                   yend = latitude + arrow_scaling * magnitude * sin(direction)),
               arrow = arrow(length = unit(0.05, "inches")),
               alpha = 0.7) +  # Adjust transparency for better visibility
  coord_fixed() +
  labs(title="Vector Field Map") +
  theme_minimal()

# Tests ----

library(terra)
library(testthat)

# Define a small test raster with known values
set.seed(123)  # Set seed for reproducibility

r_df <- data.frame(
  latitude = runif(100, min = 30, max = 70),
  longitude = runif(100, min = -10, max = 70),
  Est = runif(100, min = -22, max = -14)
)

# Convert data.frame to a SpatVector
r_vect <- vect(r_df, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Create an empty raster grid with the extent and resolution you want
r <- rast(
  extent = c(min(r_df$longitude), max(r_df$longitude), min(r_df$latitude), max(r_df$latitude)),
  resolution = c((max(r_df$longitude) - min(r_df$longitude)) / 10, (max(r_df$latitude) - min(r_df$latitude)) / 10)
)

# Rasterize the SpatVector onto the raster grid using the "Est" field
r <- terra::rasterize(r_vect, r, field = "Est", fun = mean)

# Load the get_vector_field functions here if not already loaded

# Test the get_vector_field functions
test_that("Both functions return the same result", {
  result1 <- get_vector_field(r, difference_type = "absolute")
  result2 <- get_vector_field_fast(r, difference_type = "absolute")

  expect_equal(result1, result2)
})

# General tests for each difference_type
test_that("Vector field with 'absolute' differences", {
  result <- get_vector_field_fast(r, difference_type = "absolute")

  # Check if result has expected values
  expect_true(all(result$magnitude >= 0)) # All magnitudes should be non-negative
  expect_true(nrow(result) > 0) # Ensure that result is non-empty
  expect_true(all(result$direction >= -pi & result$direction <= pi)) # Directions should be in valid range
  expect_equal(result[1,] %>% as.numeric(),
               c(4, 2, 2.53945421706885, 55.8597610187717, 1.5707963267949, 6.26725687801174))
})

test_that("Vector field with 'positive' differences", {
  result <- get_vector_field_fast(r, difference_type = "positive")

  # Check if result has only positive magnitudes
  expect_true(all(result$magnitude >= 0)) # Magnitude should be non-negative
  expect_true(nrow(result) > 0) # Ensure that result is non-empty
  expect_true(all(result$direction >= -pi & result$direction <= pi)) # Directions should be in valid range
  expect_equal(result[1,] %>% as.numeric(),
               c(4, 2, 2.53945421706885, 55.8597610187717, 1.5707963267949, 6.26725687801174))

  # Specific check: positive differences mean neighbors are greater, so values should not be NA
  # when a positive gradient exists
  expect_true(all(!is.na(result$magnitude)))
})

test_that("Vector field with 'negative' differences", {
  # Reverse the gradient to ensure we have cells with negative neighbors
  r_neg <- rast(nrows=3, ncols=3, xmin=0, xmax=3, ymin=0, ymax=3)
  values(r_neg) <- c(9, 8, 7,
                     6, 5, 4,
                     3, 2, 1)

  result <- get_vector_field_fast(r_neg, difference_type = "negative")

  # Check if result has only negative differences in magnitude (absolute values)
  expect_true(all(result$magnitude >= 0)) # Magnitude should be non-negative
  expect_true(nrow(result) > 0) # Ensure that result is non-empty
  expect_true(all(result$direction >= -pi & result$direction <= pi)) # Directions should be in valid range
  expect_equal(result[1,] %>% as.numeric(),
               c(2, 2, 1.5, 1.5, -3.14159265358979, 5.65685424949238))

  # Specific check: negative differences should not be NA if there are negative gradients
  expect_true(all(!is.na(result$magnitude)))
})
