# Spatial Lags: Concept and Applications

# Spatial lags represent a way to incorporate spatial relationships into the analysis of spatial data. A spatial lag for a variable at a location is the weighted average of the values of that variable at neighboring locations. This concept is rooted in the idea of spatial dependence, where the value of a variable in one location depends on values in nearby locations. 

# **Why Use Spatial Lags?**
# Spatial lags are used to:
# - Capture spatial dependencies in data (e.g., areas with similar socio-economic characteristics are often geographically clustered).
# - Include spatial effects in regression models to improve model accuracy and interpretability.
# - Measure the influence of neighbors' characteristics on outcomes at a specific location.

# **When Should Spatial Lags Be Used?**
# Spatial lags are relevant when:
# - There is a clear spatial structure in the data (e.g., geographic proximity, adjacency).
# - Spatial autocorrelation exists, indicated by tests like Moran's I or Geary's C.
# - Spatial spillovers or diffusion effects are hypothesized (e.g., policies, economic trends, or environmental effects).

# **Utilities of Spatial Lags**
# 1. **Descriptive Analysis**: Understanding spatial patterns and clusters.
# 2. **Regression Models**: Including spatial lags as independent variables helps model spatial spillover effects (Spatial Lag Model).
# 3. **Policy Implications**: Assessing the impact of interventions or neighboring regions' influence.
# 4. **Visualization**: Creating maps of spatial lag variables to identify local trends.

# **Key References**:
# - Anselin, L. (1988). *Spatial Econometrics: Methods and Models*. Springer Science & Business Media.
# - LeSage, J. P., & Pace, R. K. (2009). *Introduction to Spatial Econometrics*. CRC Press.
# - Griffith, D. A. (1987). *Spatial Autocorrelation: A Primer*. Resource Publications in Geography.

# The following code demonstrates the creation, use, and analysis of spatial lags, including visualization and regression models, using R and freely available datasets.

# Install pacman if not already installed
if (!requireNamespace("pacman", quietly = TRUE)) {
  install.packages("pacman")
}
library(pacman)

# Use pacman to load and install libraries
pacman::p_load(
  spData, 
  sf, 
  spdep,
  ggplot2
)

# Load a sample dataset: the Columbus dataset
# This dataset contains socio-economic data and geometry for neighborhoods in Columbus, Ohio.
map <- st_read(system.file("shapes/columbus.shp", package = "spData"), quiet = TRUE)

# Inspect the dataset
print(head(map))

# Step 1: Create a neighbors list
# A neighbors list defines which spatial units are considered "neighbors".
# Here, we use the Queen contiguity method (shares at least one boundary or corner).
nb <- spdep::poly2nb(map, queen = TRUE)

# Step 2: Generate a spatial weights matrix
# Convert the neighbors list into a spatial weights matrix.
# The "W" style standardizes rows to sum to 1 (row-standardized matrix).
wmat <- nb2mat(nb, style = "W", zero.policy = TRUE)

# Step 3: Create a spatial lag
# A spatial lag is the weighted average of a variable for each spatial unit's neighbors.
# Here, we calculate the spatial lag of the "INC" variable (income).
map$spatial_lagged_INC <- as.vector(wmat %*% map$INC)

# Inspect the new spatial lag column
print(head(map$spatial_lagged_INC))

# Step 4: Visualize the spatial lag
# Plot the original income variable and its spatial lag side-by-side
map_plot <- st_as_sf(map)  # Ensure data is in sf format for plotting

ggplot() +
  geom_sf(data = map_plot, aes(fill = INC)) +
  ggtitle("Original Income (INC)") +
  theme_minimal()

# Plot the spatial lag of income
ggplot() +
  geom_sf(data = map_plot, aes(fill = spatial_lagged_INC)) +
  ggtitle("Spatial Lag of Income") +
  theme_minimal()

# Step 5: Regression analysis with spatial lags
# Simulate a dependent variable (e.g., crime rate, CRIME) for illustration
set.seed(123)  # For reproducibility
map$CRIME <- 50 + 0.5 * map$INC - 0.3 * map$spatial_lagged_INC + rnorm(nrow(map), sd = 10)

# Fit a linear regression model
lm_model <- lm(CRIME ~ INC + spatial_lagged_INC, data = map)
summary(lm_model)  # Summarize the model

# Step 6: Explore spatial autocorrelation
# Moran's I measures spatial autocorrelation in a variable.
# correlated with itself across space. It is calculated as:
#
#   I = (N / W) * (sum(i, j) w_ij (x_i - x_bar)(x_j - x_bar)) / (sum(i) (x_i - x_bar)^2)
#
# Where:
# - N: Number of spatial units.
# - W: Sum of all spatial weights.
# - x_i, x_j: Observed values at locations i and j.
# - x_bar: Mean of the observed values.
# - w_ij: Spatial weight between locations i and j.
#
# Interpretation:
# - Positive Moran’s I indicates clustering of similar values.
# - Negative Moran’s I indicates a dispersed pattern.
# - Values near zero suggest spatial randomness.
#
# Significance is assessed via p-values, with a small p-value (< 0.05) indicating significant 
# spatial autocorrelation.
morans_test <- moran.test(map$INC, listw = nb2listw(nb, style = "W"))
morans_test  # Print the results of Moran's I test

# Visualize residuals of the regression
map$residuals <- residuals(lm_model)
ggplot() +
  geom_sf(data = map_plot, aes(fill = residuals(map))) +
  ggtitle("Regression Residuals") +
  theme_minimal()

# Step 7: Advanced: Simulate data for further examples
# If no suitable dataset is available, simulate spatial data
simulate_spatial_data <- function(n_units = 50, seed = 42) {
  set.seed(seed)
  coords <- cbind(runif(n_units), runif(n_units))  # Random coordinates
  nb <- knn2nb(knearneigh(coords, k = 4))  # Nearest neighbors
  wmat <- nb2mat(nb, style = "W", zero.policy = TRUE)
  X <- rnorm(n_units)  # Simulate independent variable
  Y <- 0.6 * X + as.vector(wmat %*% X) + rnorm(n_units)  # Generate response with spatial lag
  data.frame(ID = 1:n_units, X = X, Y = Y, spatial_lagged_X = as.vector(wmat %*% X))
}

# Simulate data and analyze
simulated_data <- simulate_spatial_data()
lm_sim <- lm(Y ~ X + spatial_lagged_X, data = simulated_data)
summary(lm_sim)
