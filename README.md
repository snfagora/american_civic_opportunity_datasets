# MapAgora: Civic Opportunity Datasets for the Study of American Local Politics and Public Policy

**Authors:** Jae Yeon Kim, Milan de Vries, Hahrie Han

This repository contains datasets and source files used to produce the MapAgora civic opportunity datasets, developed for the study of American local politics, civil society, and public policy.

---

## Session Information

- R version 4.4.0 (2024-04-24)
- Platform: aarch64-apple-darwin20
- Running under: macOS 15.3.2

---

## Dataset Overview

This project provides three core datasets:

- **Dataset 1**: Organization-level dataset of de-identified nonprofit organizations  
- **Dataset 2**: Aggregated civic opportunity counts at the ZIP code and county levels  
- **Dataset 3**: Aggregated organizational type breakdowns at the ZIP code and county levels  

Datasets 2 and 3 are derived from Dataset 1 using [`01_dataset_generation.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/01_dataset_generation.Rmd), which also generates Figure 1 and Supplementary Figure S1.

---

### Dataset 1: Organization-Level Dataset (De-identified)

This dataset includes 1,774,798 de-identified nonprofit organizations. To protect privacy and reduce the risk of misinterpretation, all identifying information (e.g., organization names and EINs) is removed.

Each observation includes:

- Unique identifier:  
  - `id`: a row index used for reference; contains no identifying information.

- Geographic identifiers:  
  - `state`: two-letter state abbreviation  
  - `city`: city name listed in the IRS record  
  - `FIPS`: county FIPS code  
  - `ZCTA`: ZIP Code Tabulation Area

- Civic opportunity indicators:  
  - `membership`, `volunteer`, `events`, `take_action`: binary variables indicating whether the organization provides each type of civic opportunity (1 = provides the opportunity; 0 = does not provide it or information is unavailable)  
  - `opp_binary`: equals 1 if the organization provides at least one civic opportunity; 0 otherwise  
  - `opp_mean`: the mean of the four civic opportunity binary indicators

- Organizational type:  
  - `predicted`: machine-learned classification of the organization (e.g., religious, political, professional)

- Address metadata:  
  - `is_po`: indicates whether the organization lists a P.O. Box as its mailing address (1 = yes, 0 = no)  
  - `grouping_value`: an anonymized internal identifier used to track federated organizations (e.g., national networks with local chapters)

- Financial attributes:  
  - `asset_amt`: total assets  
  - `income_amt`: total income  
  - `revenue_amt`: total revenue

**Dimensions:** 1,774,798 rows $\times$ 17 columns

**File Access and Format Differences:**

| Format     | File Size | Available At                                                                 |
|------------|-----------|------------------------------------------------------------------------------|
| `.parquet` | 41.9 MB   | [GitHub](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/raw_data/ind_org_df.parquet) and [Harvard Dataverse](https://doi.org/10.7910/DVN/IRCA7C) |
| `.csv`     | 125.4 MB  | [Harvard Dataverse](https://doi.org/10.7910/DVN/IRCA7C) only (not hosted on GitHub due to file size limits) |
| `.rds`     | 286.1 MB  | [Harvard Dataverse](https://doi.org/10.7910/DVN/IRCA7C) only (not hosted on GitHub due to file size limits) |

---

### Dataset 2: ZIP Code- and County-Level Aggregated Civic Opportunity Counts

Derived from Dataset 1, this dataset aggregates civic opportunity indicators and socioeconomic characteristics at the ZIP code (ZCTA) and county levels. Each observation corresponds to a geographic unit and includes counts of civic opportunity types, a composite index, normalized indicators, and contextual variables from the American Community Survey (ACS).

Each observation includes:

- Geographic identifiers:  
  - `state`: two-letter state abbreviation  
  - `FIPS`: county FIPS code  
  - `ZCTA`: ZIP Code Tabulation Area

- Organizational counts:  
  - `n`: total nonprofit organizations  
  - `civic_org_sum`: total civic opportunity organizations  
  - `membership_sum`: total organizations providing membership opportunities  
  - `volunteer_sum`: total organizations providing volunteer opportunities  
  - `events_sum`: total organizations providing public event opportunities  
  - `take_action_sum`: total organizations providing political or civic action opportunities

- Composite civic opportunity scores:  
  - `civic_opp_sum`: total opportunity score

- Normalized civic opportunity indicators (per capita):  
  - `civic_org_sum_normalized`: total number of civic organizations per capita  
  - `civic_opp_sum_normalized`: total civic opportunities per capita  
  - `civic_opp_index`: quintile-based civic opportunity index, derived by dividing `civic_opp_sum_normalized` into five equal-sized bins  
  - `membership_sum_normalized`: total number of organizations providing membership opportunities per capita  
  - `volunteer_sum_normalized`: total number of organizations providing volunteer opportunities per capita  
  - `events_sum_normalized`: total number of organizations providing public event opportunities per capita  
  - `take_action_sum_normalized`: total number of organizations providing political or civic action opportunities per capita

- Sociodemographic indicators:  
  - `TotalPopulation`: total population  
  - `POV150`: poverty rate  
  - `SNGPNT`: single-parent households  
  - `BROAD`: households without broadband access  
  - `NOHSDP`: adults without a high school diploma  
  - `UNEMP`: unemployment rate  
  - `REMNRTY`: share of racial or ethnic minority residents

**Dimensions:**
- ZIP code level: 30,988 rows $\times$ 24 columns  
- County level: 3,281 rows $\times$ 24 columns

**ZIP Code Level Files:**
- [`zcta_counts_cov.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_counts_cov.csv)  
- [`zcta_counts_cov.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_counts_cov.rds)

**County Level Files:**
- [`cnty_counts_cov.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_counts_cov.csv)  
- [`cnty_counts_cov.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_counts_cov.rds)

---

### Dataset 3: ZIP Code- and County-Level Civic Opportunity Provider Types

Also derived from Dataset 1, this dataset summarizes the types of organizations that provide civic opportunities at the ZIP code and county levels. Each observation corresponds to a unique geography-organization type pair and enables analysis of regional patterns in the composition of civic infrastructure.

Each observation includes:

- Geographic identifiers:  
  - `FIPS`: county FIPS code  
  - `ZCTA`: ZIP Code Tabulation Area

- Organizational counts:  
  - `n`: number of civic opportunity???providing organizations of a given type in the geography

- Organization type classification:  
  - `class`: predicted organizational type (e.g., religious, political, professional)

- Relative frequency:  
  - `freq`: proportion of civic opportunity organizations in the geography that fall into the given class

- Primary provider type:  
  - `primary_org_cat`: the most common civic opportunity organization type in the geography; appears once per unit

**Dimensions:**
- ZIP code level: 150,162 rows $\times$ 5 columns  
- County level: 29,687 rows $\times$ 5 columns

**ZIP Code Level Files:**
- [`zcta_civic_org_type.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_civic_org_type.csv)  
- [`zcta_civic_org_type.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_civic_org_type.rds)

**County Level Files:**
- [`cnty_civic_org_type.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_civic_org_type.csv)  
- [`cnty_civic_org_type.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_civic_org_type.rds)

---

## Data Description and Validation

- **Data description:** [`02_description.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/02_description.Rmd)  
  - Produces Figures 2-3 and Tables 4-5  

- **Data validation:** [`03_validation.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/03_validation.Rmd)  
  - Produces Figures 4-7