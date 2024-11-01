# Lyon MS Thesis - Field Data Tidying

Wrangling of all field-collected data collected for my MS thesis. This spans multiple projects conducted at different subsets of the sites in the Grand River Grasslands (GRG). Descriptions of these are included below.

## Script Explanations

- `00_raw-data-download.R` - Downloads all raw files from Google Drive (note that this does require prior access to the relevant folder)
- `01a_butterfly-project_tidy-butterflies.R` - Tidying butterfly data collected from 2007 to 2018
- `01b_butterfly-project_tidy-flowers.R` - Tidying floral resource (i.e., nectar-producing plant) data collected from the same years as the related butterfly dataset
- `01c_daubenmire-project_tidy-plants.R` - Tidying plant data collected in 2.5 x 0.5 meter "Daubenmire" quadrats
- `01d_bee-project_tidy-bees.R` - Tidying bee data from 2017 & 2018 collected as part of a 'bee project' focused on the sensitivity of native bee communities to management practices
- `01e_bee-project_tidy-flowers.R` - Tidying floral resource (i.e., nectar-producing plant) data from the same years as the related native bee datasets
- `02_tidy-data-upload.R` - Uploads all tidy files to Google Drive (note that this also requires prior access to the relevant folder)
