# Created by Sydni M. Nadler (2025)
# Title: Experiment stimulus color perception calculator 

## -------------- ##

## R SET-UP ##

#Install necessary packages if not already installed

#install.packages(c("colorspace", "farver", "dplyr", "tibble", "DT"))


#Load packages

library(farver)
library(dplyr)
library(tibble)
library(grDevices)
library(DT)

## -------------- ##

## CHOOSE INPUT MODE ##

# Set to "HSL" or "RGB" to control your input mode
input_mode <- "HSL"  # Change to "RGB" if using RGB input


## -------------- ##

## COLOR CONVERSION FUNCTIONS ##

# Function to convert HSL to RGB (normalized [0–1])
hsl_to_rgb <- function(h, s, l) {
  h <- h %% 360
  s <- s / 100
  l <- l / 100
  c <- (1 - abs(2 * l - 1)) * s
  x <- c * (1 - abs((h / 60) %% 2 - 1))
  m <- l - c / 2
  
  if (h < 60) {
    r1 <- c; g1 <- x; b1 <- 0
  } else if (h < 120) {
    r1 <- x; g1 <- c; b1 <- 0
  } else if (h < 180) {
    r1 <- 0; g1 <- c; b1 <- x
  } else if (h < 240) {
    r1 <- 0; g1 <- x; b1 <- c
  } else if (h < 300) {
    r1 <- x; g1 <- 0; b1 <- c
  } else {
    r1 <- c; g1 <- 0; b1 <- x
  }
  
  c(r1 + m, g1 + m, b1 + m)
}


# Function for WCAG relative luminance
relative_luminance <- function(rgb) {
  linearize <- function(c) {
    ifelse(c <= 0.03928, c / 12.92, ((c + 0.055) / 1.055)^2.4)
  }
  r <- linearize(rgb[1])
  g <- linearize(rgb[2])
  b <- linearize(rgb[3])
  0.2126 * r + 0.7152 * g + 0.0722 * b
}


# Function to convert RGB (Red, green, blue; 0–255) to CMYK (Cyan, magenta, yellow, key)
rgb_to_cmyk <- function(r, g, b) {
  r <- r / 255
  g <- g / 255
  b <- b / 255
  
  k <- 1 - max(r, g, b)
  
  if (k < 1) {
    c <- (1 - r - k) / (1 - k)
    m <- (1 - g - k) / (1 - k)
    y <- (1 - b - k) / (1 - k)
  } else {
    c <- 0
    m <- 0
    y <- 0
  }
  
  round(c(c, m, y, k), 4)
}

## -------------- ##

## COLOR INPUT ##

# Add as many colors as you intend to use
# Rename color1-3 to your desired name for stimulus colors 1-3

if (input_mode == "HSL") {                  # Define HSL Inputs
  colors_input <- list(                     # Change the below hue, saturation, and lightness values according to your intended colors
    color1 = c(h = 180, s = 100, l = 50),    
    color2 = c(h = 45, s = 100, l = 80),
    color3 = c(h = 0, s = 0, l = 0)        # color3 is set as my intended background color
  )
  
  color_data_raw <- tibble(Color = names(colors_input)) %>%
    rowwise() %>%
    mutate(
      H = colors_input[[Color]]["h"],
      S = colors_input[[Color]]["s"],
      L = colors_input[[Color]]["l"],
      RGB_norm = list(hsl_to_rgb(H, S, L)),
      RGB_255 = list(round(unlist(RGB_norm) * 255)),
      R = RGB_255[[1]],
      G = RGB_255[[2]],
      B = RGB_255[[3]]
    ) %>%
    ungroup()
  
} else if (input_mode == "RGB") {          # Enter Red, Green, and Blue values here if in RGB mode
  colors_input <- list(
    color1 = c(r = 255, g = 100, b = 50),
    color2 = c(r = 255, g = 100, b = 0),
    color3 = c(r = 0, g = 0, b = 0)
  )
  
  color_data_raw <- tibble(Color = names(colors_input)) %>%
    rowwise() %>%
    mutate(
      R = colors_input[[Color]]["r"],
      G = colors_input[[Color]]["g"],
      B = colors_input[[Color]]["b"],
      RGB_255 = list(c(R, G, B)),
      RGB_norm = list(c(R, G, B) / 255),
      hsv_vals = rgb2hsv(matrix(c(R, G, B), nrow = 3), maxColorValue = 255)[, 1],
      H = round(hsv_vals[1] * 360, 1),
      S = round(hsv_vals[2] * 100, 1),
      L = round((max(R, G, B)/255 + min(R, G, B)/255) / 2 * 100, 1)
    ) %>%
    ungroup()
} else {
  stop("Invalid input_mode. Please choose 'HSL' or 'RGB' only.")
}

## -------------- ##

## PREPARE DATATABLE ##

# Processes data for columns with Relative Luminance values, Hex codes, HSV values, and color swatches
color_data <- color_data_raw %>%
  mutate(
    Hex = toupper(sapply(RGB_norm, function(rgb) {
      rgb(rgb[1], rgb[2], rgb[3], maxColorValue = 1)
    })),
    HSV = list(round(rgb2hsv(matrix(c(R, G, B), nrow = 3), maxColorValue = 255)[, 1], 4)),
    Luminance = sapply(RGB_norm, function(rgb) round(relative_luminance(rgb), 4)),
    Swatch = paste0('<div style="width: 50px; height: 20px; background-color:', Hex, ';"></div>')
  )


# Prepare matrix of RGB colors for Delta E calculations
rgb_matrix <- do.call(rbind, color_data$RGB_255)


# Calculate Delta E (per CIE2000) between each color and all three colors
# Note: farver::compare_colour takes colors as a matrix of R,G,B (0-255)

delta_e_mat <- matrix(NA_real_, nrow = nrow(rgb_matrix), ncol = 3)
for (i in seq_len(nrow(rgb_matrix))) {
  # Compute Delta E of color i from each of the 3 colors
  delta_e_mat[i, ] <- farver::compare_colour(
    from = matrix(rgb_matrix[i, ], nrow = 1),
    to = rgb_matrix,
    from_space = "rgb",
    to_space = "rgb",
    method = "CIE2000"
  )
}

colnames(delta_e_mat) <- paste0("ΔE from ", color_data$Color)


# Adds Delta E columns to data frame 
# Rounds to 3 decimals
color_data <- bind_cols(color_data, as_tibble(round(delta_e_mat, 3)))


# Combines R, G, B into one string column and H, S, V into another
# Adds HSL and CMYK columns
color_data <- color_data %>%
  mutate(
    RGB = sapply(RGB_255, function(x) paste0("(", paste(x, collapse = ", "), ")")),
    HSV = sapply(HSV, function(hsv) paste0("(", paste(hsv, collapse = ", "), ")")),
    HSL = paste0("(", H, ", ", S, ", ", L, ")"),
    CMYK = sapply(RGB_255, function(rgb) {
      cmyk <- rgb_to_cmyk(rgb[1], rgb[2], rgb[3])
      paste0("(", paste(cmyk, collapse = ", "), ")")
    })
  )

# Renders HTML datatable
datatable(
  color_data %>%
    select(Color, Swatch, Hex, Luminance, starts_with("ΔE"), HSL, HSV, RGB, CMYK), # List selections according to desired column order
  escape = FALSE,
  options = list(pageLength = 10),
  rownames = FALSE
) %>%
  formatRound(columns = c("Luminance", "ΔE from color1", "ΔE from color2", "ΔE from color3"), digits = 3) # Rounds calculated values to 3 decimal places

