# RMB Constants
# Centralized constants to avoid magic numbers throughout the codebase

# Time constants
MINUTES_PER_DAY <- 1440L
MINUTES_PER_HOUR <- 60L
HOURS_PER_DAY <- 24L

# DAM Monitor constants
DAM_CHANNELS <- 32L
DAM_POSITIONS <- 15L  # Multi-beam positions (1-15)
DAM_HEADER_COLS <- 8L  # Columns before channel data

# Data type identifiers in DAM format
DAM_TYPE_MOVEMENT <- "MT"
DAM_TYPE_CUMULATIVE <- "CT"
DAM_TYPE_POSITION <- "Pn"
DAM_TYPE_CURRENT <- "Ct"

# Sleep definition (consecutive minutes without movement)
SLEEP_THRESHOLD_MINUTES <- 5L

# Default thresholds
DEFAULT_MIN_MOVEMENT <- 10L  # For dead fly detection
DEFAULT_MIN_COVERAGE <- 20L

# Default experimental parameters  
DEFAULT_TEMPERATURE <- 25L
DEFAULT_TUBE_TYPE <- "Normal"
DEFAULT_INCUBATOR <- "MB"
DEFAULT_LAB <- "Sehgal"
