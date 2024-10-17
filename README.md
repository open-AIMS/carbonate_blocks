Erosion and Accretion of carbonate blocks
======================================================================

# Dependencies

The following R packages are required:

- tidyverse
- validate

# Installation

1. Start by cloning the repository to a directory of your choice

~~~
git clone git@github.com:open-AIMS/carbonate_blocks.git .
OR
git clone https://github.com/open-AIMS/carbonate_blocks.git .
~~~

2. Establish a `data/raw/` folder and place a copy of the raw data (as
   show below)

~~~
\
|- README.md
|- data
|  |- raw
|     |- Blocks_eReefs_silt_biota_erosion_metab_2024Oct.csv
|- R
|  |- 00_main.R
~~~

3. Navigate to the `R` folder and run the `00_main.R` script

# Outputs

Early on, the scripts will expand on the directory structure to create
the following:

~~~
\
|- README.md
|- data
|  |- raw
|  |  |- Blocks_eReefs_silt_biota_erosion_metab_2024Oct.csv
|  |- primary
|  |- processed
|  |- modelled
|- R
|  |- 00_main.R
|- outputs
|  |- figures
|  |- tables
|
~~~

# Important global variables

- `DATA_PATH`: path to root of data folder
- `RAW_DATA_PATH`: path to location of raw data (data supplied by Katharina)
- `FIGURES_PATH`: path to location of all output figures
- `TABLES_PATH`: path to location of all output tables
