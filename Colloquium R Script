# Define the paths to the folders
parent_folders <- c("/Users/meganogrady/Desktop/NEON_comm-microbe-benthic", "/Users/meganogrady/Desktop/NEON_group-abund-microbe-benthic", "/Users/meganogrady/Desktop/NEON_group-abund-microbe-surfacewater", "/Users/meganogrady/Desktop/NEON_comm-microbe-surfacewater")

# Function to list subfolders in a parent folder
list_subfolders <- function(parent_folder) {
  subfolders <- list.dirs(path = parent_folder, full.names = TRUE, recursive = FALSE)
  return(subfolders)
}

# Function to extract location name from subfolder names
extract_location <- function(subfolder_name) {
  parts <- strsplit(subfolder_name, ".", fixed = TRUE)[[1]]
  location <- parts[grep(paste(uppercase_letters_to_extract, collapse = "|"), parts)][1]  # Extract the first match of the specified uppercase letters
  return(location)
}

# Function to find overlapping location codes across all parent folders
find_overlapping_location_codes <- function(parent_folders) {
  all_subfolders <- lapply(parent_folders, list_subfolders)
  all_subfolder_names <- unlist(lapply(all_subfolders, basename))
  
  # Extract location codes from all subfolder names
  location_codes <- lapply(all_subfolder_names, extract_location)
  
  # Create a list to store the unique location codes that overlap in location across all parent folders
  overlapping_location_codes <- unique(unlist(sapply(location_codes, function(location) {
    # Check if the location code appears in all parent folders
    if (all(sapply(parent_folders, function(folder) {
      any(location == extract_location(list_subfolders(folder)))
    }))) {
      return(location)
    } else {
      return(NULL)
    }
  })))
  
  return(overlapping_location_codes)
}

# Function to find overlapping location codes across all parent folders
find_overlapping_location_codes <- function(parent_folders) {
  all_subfolders <- lapply(parent_folders, list_subfolders)
  all_subfolder_names <- unlist(lapply(all_subfolders, basename))
  
  # Extract location codes from all subfolder names
  location_codes <- lapply(all_subfolder_names, extract_location)
  
  # Create a list to store the unique location codes that overlap in location across all parent folders
  overlapping_location_codes <- unique(unlist(sapply(location_codes, function(location) {
    # Check if the location code appears in all parent folders
    if (all(sapply(parent_folders, function(folder) {
      subfolders <- list_subfolders(folder)
      if (length(subfolders) > 0) {
        any(location == extract_location(subfolders))
      } else {
        FALSE
      }
    }))) {
      return(location)
    } else {
      return(NULL)
    }
  })))
  
  return(overlapping_location_codes)
}

# Create new folders for each parent folder on the desktop
for (parent_folder in parent_folders) {
  new_folder_name <- file.path("~/Desktop", paste0("Parent_", basename(parent_folder)))
  dir.create(new_folder_name, showWarnings = FALSE)
  cat("Created folder:", new_folder_name, "\n")
}

# Move overlapping subfolders into their respective parent folders
for (parent_folder in parent_folders) {
  subfolders <- list_subfolders(parent_folder)
  overlapping_subfolders <- lapply(subfolders, function(subfolder) {
    location <- extract_location(basename(subfolder))
    if (location %in% overlapping_location_codes) {
      return(subfolder)
    } else {
      return(NULL)
    }
  })
  overlapping_subfolders <- Filter(Negate(is.null), overlapping_subfolders)
  parent_folder_name <- paste0("Parent_", basename(parent_folder))
  for (subfolder in overlapping_subfolders) {
    new_folder_name <- file.path("~/Desktop", parent_folder_name, basename(subfolder))
    dir.create(new_folder_name)
    file.copy(subfolder, new_folder_name, recursive = TRUE)
    cat("Moved subfolder:", basename(subfolder), "to:", new_folder_name, "\n")
  }
}


library(dplyr)
library(zoo)  

# Function to extract desired columns from CSV file
extract_columns <- function(csv_path, parent_folder) {
  # Read the CSV file
  data <- read.csv(csv_path)
  
  # Extract desired columns
  extracted_columns <- data[, c("class", "scientificName", "individualCount")]
  
  # Extract location from the first subfolder name
  subfolder <- basename(dirname(csv_path))
  location <- gsub(".*\\.(\\w{4})\\..*", "\\1", subfolder)
  
  # Add location column to the extracted data
  extracted_columns$location <- location
  
  # Determine if it's benthic or surface water based on the parent folder
  if (grepl("benthic", parent_folder, ignore.case = TRUE)) {
    extracted_columns$benthicOrSurfaceWater <- "benthic"
  } else if (grepl("surfacewater", parent_folder, ignore.case = TRUE)) {
    extracted_columns$benthicOrSurfaceWater <- "surface water"
  } else {
    extracted_columns$benthicOrSurfaceWater <- NA
  }
  
  return(extracted_columns)
}

# Create a list to store extracted data frames for all CSV files
all_data_frames <- list()

# Iterate through each parent folder
for (parent_folder in parent_folders) {
  print("Parent folder:")
  print(parent_folder)
  
  # Find all CSV files within the parent folder
  csv_files <- list.files(parent_folder, pattern = "16S.*\\.csv$", recursive = TRUE, full.names = TRUE)
  print("CSV files:")
  print(csv_files)
  
  # Iterate through each CSV file
  for (csv_file in csv_files) {
    print("CSV file:")
    print(csv_file)
    
    # Try to read the CSV file
    tryCatch({
      # Extract desired columns from the CSV file
      extracted_data <- extract_columns(csv_file, parent_folder)
      print("Extracted data:")
      print(head(extracted_data))
      
      # Append extracted data frame to the list
      all_data_frames[[csv_file]] <- extracted_data
    }, error = function(e) {
      # Print error message
      cat("Error:", conditionMessage(e), "\n")
      cat("Skipping file:", csv_file, "\n")
    })
  }
}

# Combine extracted data frames from all CSV files into a single data frame
combined_data <- do.call(rbind, all_data_frames)

# Fill empty cells in the class column with the corresponding class values
combined_data$class <- ifelse(combined_data$class == "", NA, combined_data$class)
combined_data$class <- na.locf(combined_data$class)  # Fill NAs with last observation

# Group by location, class, scientificName, and benthicOrSurfaceWater, and summarize individualCount
combined_data <- combined_data %>%
  group_by(location, class, scientificName, benthicOrSurfaceWater) %>%
  summarise(individualCount = sum(individualCount)) %>%
  ungroup()  # Remove grouping

# Define the path to save the new CSV file
output_csv <- "/Users/meganogrady/Desktop/grouped_data_with_location_combined_sorted.csv"

# Write the combined data to a new CSV file
write.csv(combined_data, file = output_csv, row.names = FALSE)
cat("CSV file saved to:", output_csv, "\n")



# Function to calculate Shannon Diversity Index
shannon_diversity_index <- function(x) {
  probabilities <- x / sum(x)
  shannon_index <- -sum(probabilities * log(probabilities, base = exp(1)))
  return(shannon_index)
}

# Function to calculate Simpson's Diversity Index
simpsons_diversity_index <- function(counts) {
  probabilities <- counts / sum(counts)
  simpsons_index <- 1 - sum(probabilities^2)
  return(simpsons_index)
}

# Function to extract desired columns from CSV file
extract_columns <- function(csv_path, parent_folder) {
  # Read the CSV file
  data <- read.csv(csv_path)
  
  # Extract desired columns
  extracted_columns <- data[, c("class", "scientificName", "individualCount")]
  
  # Extract location from the first subfolder name
  subfolder <- basename(dirname(csv_path))
  location <- gsub(".*\\.(\\w{4})\\..*", "\\1", subfolder)
  
  # Add location column to the extracted data
  extracted_columns$location <- location
  
  # Determine if it's benthic or surface water based on the parent folder
  if (grepl("benthic", parent_folder, ignore.case = TRUE)) {
    extracted_columns$benthicOrSurfaceWater <- "benthic"
  } else if (grepl("surfacewater", parent_folder, ignore.case = TRUE)) {
    extracted_columns$benthicOrSurfaceWater <- "surface water"
  } else {
    extracted_columns$benthicOrSurfaceWater <- NA
  }
  
  return(extracted_columns)
}

# Create a list to store extracted data frames for all CSV files
all_data_frames <- list()

# Iterate through each parent folder
for (parent_folder in parent_folders) {
  print("Parent folder:")
  print(parent_folder)
  
  # Find all CSV files within the parent folder
  csv_files <- list.files(parent_folder, pattern = "16S.*\\.csv$", recursive = TRUE, full.names = TRUE)
  print("CSV files:")
  print(csv_files)
  
  # Iterate through each CSV file
  for (csv_file in csv_files) {
    print("CSV file:")
    print(csv_file)
    
    # Try to read the CSV file
    tryCatch({
      # Extract desired columns from the CSV file
      extracted_data <- extract_columns(csv_file, parent_folder)
      print("Extracted data:")
      print(head(extracted_data))
      
      # Append extracted data frame to the list
      all_data_frames[[csv_file]] <- extracted_data
    }, error = function(e) {
      # Print error message
      cat("Error:", conditionMessage(e), "\n")
      cat("Skipping file:", csv_file, "\n")
    })
  }
}

# Combine extracted data frames from all CSV files into a single data frame
combined_data <- do.call(rbind, all_data_frames)

# Define the path to save the new CSV file
output_csv <- "/Users/meganogrady/Desktop/grouped_data_with_location.csv"

# Write the combined data to a new CSV file
write.csv(combined_data, file = output_csv, row.names = FALSE)
cat("CSV file saved to:", output_csv, "\n")

# Calculate Shannon Diversity Index
shannon_diversity_index <- function(x) {
  probabilities <- x / sum(x)
  shannon_index <- -sum(probabilities * log(probabilities, base = exp(1)), na.rm = TRUE)
  return(shannon_index)
}

# Calculate Simpson's Diversity Index
simpsons_diversity_index <- function(counts) {
  probabilities <- counts / sum(counts)
  simpsons_index <- 1 - sum(probabilities^2, na.rm = TRUE)
  return(simpsons_index)
}

# Calculate Shannon and Simpson's Diversity Indices
diversity_results <- data %>%
  group_by(location) %>%
  summarise(
    shannon_diversity_benthic = shannon_diversity_index(individualCount[benthicOrSurfaceWater == "benthic"]),
    shannon_diversity_surface_water = shannon_diversity_index(individualCount[benthicOrSurfaceWater == "surface water"]),
    simpsons_diversity_benthic = simpsons_diversity_index(individualCount[benthicOrSurfaceWater == "benthic"]),
    simpsons_diversity_surface_water = simpsons_diversity_index(individualCount[benthicOrSurfaceWater == "surface water"])
  )

# Write the results to a CSV file
write.csv(diversity_results, file = "/Users/meganogrady/Desktop/diversity_results.csv", row.names = FALSE)

# Print the results
print(diversity_results)

library(leaflet)
library(dplyr)

# Read diversity results data
diversity_data <- read.csv("/Users/meganogrady/Desktop/diversity_results.csv")

# Create a data frame with location coordinates
locations <- data.frame(
  location = c("HOPB", "LEWI", "POSE", "CUPE", "GUIL", "KING", "MCDI", "LECO", "WALK", "MAYF", "ARIK", "BLUE", "PRIN", "BLDE", "COMO", "WLOU", "SYCA", "REDB", "MART", "MCRA", "BIGC", "OKSR", "CARI"),
  lat = c(42.47194, 39.09564, 38.89431, 18.11352, 18.17406, 39.10506, 38.94586, 35.69043, 35.95738, 32.96037, 39.75821, 34.44422, 33.37852, 44.95011, 40.03496, 39.89137, 33.75099, 40.78393, 45.79084, 44.25960, 37.05972, 68.66975, 65.15322),
  lon = c(-72.32953, -77.98322, -78.14726, -66.98676, -66.79868, -96.60383, -96.44302, -83.50379, -84.27925, -87.40769, -102.44715, -96.62420, -97.78231, -110.58715, -105.54416, -105.91540, -111.50809, -111.79789, -121.93379, -122.16555, -119.25755, -149.14302, -147.50397)
)

# Merge diversity data with location coordinates
combined_data <- merge(diversity_data, locations, by = "location")

# Function to map Shannon diversity to colors
map_to_color <- function(x) {
  # Define color range from red to purple
  colors <- colorRampPalette(c("red", "purple"))(100)
  # Normalize x to range between 0 and 1
  normalized_x <- (x - min(x)) / (max(x) - min(x))
  # Map normalized x to color index
  color_index <- floor(normalized_x * 99) + 1
  # Return corresponding colors
  return(colors[color_index])
}

# Apply the color mapping function to Shannon diversity data
combined_data$color <- map_to_color(combined_data$shannon_diversity_benthic)

# Create map
map <- leaflet() %>%
  addTiles() %>%  # Add the default tile layer
  addCircleMarkers(data = combined_data,  # Add circle markers for each location
                   lat = ~lat,
                   radius = 8,  # Set radius to a constant value
                   color = ~color, fillOpacity = 0.8, stroke = FALSE,  # Set marker color based on diversity
                   popup = ~paste("Location:", location, "<br>",
                                  "Shannon Diversity (Benthic):", shannon_diversity_benthic, "<br>",
                                  "Shannon Diversity (Surface Water):", shannon_diversity_surface_water),
                   label = ~location)

# Display map
map
library(htmlwidgets)

# Define the file path for saving on the desktop
file_path <- file.path(Sys.getenv("HOME"), "Desktop", "interactive_map.html")

# Export the Leaflet map as an interactive HTML file
saveWidget(map, file = file_path, selfcontained = TRUE)


library(leaflet)
library(dplyr)

# Read diversity results data
diversity_data <- read.csv("/Users/meganogrady/Desktop/diversity_results.csv")

# Create a data frame with location coordinates
locations <- data.frame(
  location = c("HOPB", "LEWI", "POSE", "CUPE", "GUIL", "KING", "MCDI", "LECO", "WALK", "MAYF", "ARIK", "BLUE", "PRIN", "BLDE", "COMO", "WLOU", "SYCA", "REDB", "MART", "MCRA", "BIGC", "OKSR", "CARI"),
  lat = c(42.47194, 39.09564, 38.89431, 18.11352, 18.17406, 39.10506, 38.94586, 35.69043, 35.95738, 32.96037, 39.75821, 34.44422, 33.37852, 44.95011, 40.03496, 39.89137, 33.75099, 40.78393, 45.79084, 44.25960, 37.05972, 68.66975, 65.15322),
  lon = c(-72.32953, -77.98322, -78.14726, -66.98676, -66.79868, -96.60383, -96.44302, -83.50379, -84.27925, -87.40769, -102.44715, -96.62420, -97.78231, -110.58715, -105.54416, -105.91540, -111.50809, -111.79789, -121.93379, -122.16555, -119.25755, -149.14302, -147.50397)
)

# Merge diversity data with location coordinates
combined_data <- merge(diversity_data, locations, by = "location")

# Define color palette for Simpson's diversity
color_palette <- colorRampPalette(c("red", "purple"))(101)  # Generate a color palette from red to purple with 101 steps

# Normalize Simpson's diversity values between 0 and 1
combined_data$simpsons_diversity <- (combined_data$simpsons_diversity_benthic + combined_data$simpsons_diversity_surface_water) / 2
min_simpsons <- min(combined_data$simpsons_diversity, na.rm = TRUE)
max_simpsons <- max(combined_data$simpsons_diversity, na.rm = TRUE)
combined_data$color_index <- as.integer(cut(combined_data$simpsons_diversity, breaks = seq(min_simpsons, max_simpsons, length.out = 101), labels = FALSE))

# Map Simpson's diversity
map_simpsons <- leaflet() %>%
  addTiles() %>%  # Add the default tile layer
  addCircleMarkers(data = combined_data,  # Add circle markers for each location
                   lat = ~lat, lng = ~lon,  # Correct the argument name to "lng" for longitude
                   radius = 8,  # Set radius to a constant value
                   color = ~color_palette[color_index], fillOpacity = 0.8, stroke = FALSE,  # Set marker color based on diversity
                   popup = ~paste("Location:", location, "<br>",
                                  "Simpsons Diversity (Benthic):", simpsons_diversity_benthic, "<br>",
                                  "Simpsons Diversity (Surface Water):", simpsons_diversity_surface_water),
                   label = ~location)

# Display map
map_simpsons

###
library(tidyverse)
library(igraph)
library(ggraph)

# Read the data
data_combined <- read.csv("/Users/meganogrady/Desktop/grouped_data_with_location_combined_sorted.csv")

# Create a new column indicating habitat type for each species
data_combined <- data_combined %>%
  mutate(color = ifelse(benthicOrSurfaceWater == "benthic", "blue", "green"))

# Create a tidygraph object
g <- tbl_graph(data_combined, directed = FALSE)

# Find common species across locations
common_species <- data_combined %>%
  select(scientificName, location) %>%
  distinct() %>%
  group_by(scientificName) %>%
  filter(n_distinct(location) > 1) %>%
  distinct(scientificName)

# Create edges between common species
common_edges <- common_species %>%
  inner_join(data_combined, by = "scientificName") %>%
  inner_join(data_combined, by = "location", suffix = c(".x", ".y"), 
             copy = c("benthicOrSurfaceWater", "color")) %>%
  filter(benthicOrSurfaceWater.x != benthicOrSurfaceWater.y) %>%
  unite(edge, scientificName.x, scientificName.y, sep = " -- ")

# Convert to igraph object
g_edges <- graph_from_data_frame(common_edges, directed = FALSE)


# Plot the network
ggraph(g, layout = "nicely") +
  geom_edge_link() +
  geom_node_point(aes(color = color), size = .001) +
  scale_color_identity() +
  theme_void() +
  theme(legend.position = "none") +
  labs(title = "Network of Species in Benthic and Surface Water Habitats")

###
library(tidyverse)
library(igraph)
library(ggraph)

# Read the data
data_combined <- read.csv("/Users/meganogrady/Desktop/grouped_data_with_location_combined_sorted.csv")

# Function to create network for each location and save as PNG
create_and_save_network <- function(location) {
  # Subset data for the specific location
  location_data <- data_combined %>%
    filter(location == !!location)
  
  # Create a new column indicating habitat type for each species
  location_data <- location_data %>%
    mutate(color = ifelse(benthicOrSurfaceWater == "benthic", "blue", "green"))
  
  # Create a tidygraph object
  g <- tbl_graph(location_data, directed = FALSE)
  
  # Plot the network for the specific location
  p <- ggraph(g, layout = "nicely") +
    geom_edge_link() +
    geom_node_point(aes(color = color), size = 0.5) +
    scale_color_identity() +
    theme_void() +
    theme(legend.position = "none") +
    labs(title = paste("Network of Species in", location))
  
  # Save the plot as a PNG file
  ggsave(paste("/Users/meganogrady/Desktop/Colloquium/network_plots/", location, ".png", sep = ""), plot = p, width = 6, height = 6)
}

# Get unique locations
unique_locations <- unique(data_combined$location)
map(unique_locations, create_and_save_network)

###### Abundances for each community by site ########
library(colorspace)

grouped_data_with_location <- read.csv("/Users/meganogrady/Desktop/grouped_data_with_location_combined_sorted.csv")

heatmap_combined <- grouped_data_with_location %>%
  group_by(class, location, benthicOrSurfaceWater)%>%
  summarise(class_count = sum(individualCount))%>%
  # mutate(class_id = as.character(1:row_number(.)))%>%
  ggplot2:: ggplot(aes(x = location, y = class))+
  geom_tile(aes(fill = class_count))+
  scale_fill_continuous_sequential(palette = "Heat")+
  theme_bw()
  title("Heatmap combined benthic and surface water")


heatmap_benthic <- grouped_data_with_location %>%
  filter(benthicOrSurfaceWater == "benthic")%>%
  group_by(class, location, benthicOrSurfaceWater)%>%
  summarise(class_count = sum(individualCount))%>%
  # mutate(class_id = as.character(1:row_number(.)))%>%
  ggplot2:: ggplot(aes(x = location, y = class))+
  geom_tile(aes(fill = class_count))+
  scale_fill_continuous_sequential(palette = "Heat")+
  theme_bw()
  title("Heatmap benthic")

heatmap_surface <- grouped_data_with_location %>%
  filter(benthicOrSurfaceWater == "surface water")%>%
  group_by(class, location, benthicOrSurfaceWater)%>%
  summarise(class_count = sum(individualCount))%>%
  # mutate(class_id = as.character(1:row_number(.)))%>%
  ggplot2:: ggplot(aes(x = location, y = class))+
  geom_tile(aes(fill = class_count))+
  scale_fill_continuous_sequential(palette = "Heat")+
  theme_bw()
  title("Heatmap surface")

both_habitats <- grouped_data_with_location %>%
  group_by(class, benthicOrSurfaceWater) %>%
  summarise(count = n()) %>%
  ungroup()%>%
  group_by(class) %>%
  summarise(count = n()) %>%
  filter(count == 2)

heatmap_both <- grouped_data_with_location %>%
  filter(class %in% both_habitats$class)%>%
  group_by(class, location, benthicOrSurfaceWater)%>%
  summarise(class_count = sum(individualCount))%>%
  # mutate(class_id = as.character(1:row_number(.)))%>%
  ggplot2:: ggplot(aes(x = location, y = class))+
  geom_tile(aes(fill = class_count))+
  scale_fill_continuous_sequential(palette = "Heat")+
  theme_bw()
  title("Heatmap found in both environments")

