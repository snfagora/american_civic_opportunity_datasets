# MapAgora: Civic Opportunity Datasets for the Study of American Local Politics and Public Policy

**Authors:** Jae Yeon Kim, Milan de Vries, Hahrie Han

This repository contains datasets and source files used to produce the MapAgora civic opportunity datasets, developed for the study of American local politics, civil society, and public policy.

---

## Session Information

- R version: 4.4.0 (2024-04-24)  
- Platform: aarch64-apple-darwin20  
- OS: macOS 15.1.1  

---

## Dataset Overview

This project provides three core datasets:

- **Dataset 1**: Organization-level dataset of de-identified nonprofit organizations  
- **Dataset 2**: Aggregated civic opportunity counts at the ZIP code and county levels  
- **Dataset 3**: Aggregated organizational type breakdowns at the ZIP code and county levels  

Datasets 2 and 3 are derived from Dataset 1 using [`01_dataset_generation.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/01_dataset_generation.Rmd), which also generates Figure 1 and Supplementary Figure S1.

---

### Dataset 1: Organization-Level Dataset (De-identified)

This dataset includes 1,363,701 de-identified nonprofit organizations. While the IRS Master Business File contains over 1.8 million registered tax-exempt organizations, this dataset includes only those for which we could reliably extract sufficient information to classify both the types of civic opportunities offered and the organizational type. This filtering step ensures high-quality coverage and consistent labeling.

To protect privacy and reduce the risk of misinterpretation, all identifying information (e.g., organization names and EINs) is removed. Each observation includes:

- A unique identifier (`id`, row index only; no identifying information)
- Geographic identifiers: `state`, `city`, `FIPS` (county), and `ZCTA` (ZIP Code Tabulation Area)
- Civic opportunity indicators:
  - `membership`, `volunteer`, `events`, `take_action`: binary variables indicating the types of civic opportunities provided
- Organizational classification:
  - `predicted`: machine-predicted organizational type (e.g., religious, political, professional)
- Address metadata:
  - `is_po`: indicates whether the organization lists a P.O. Box
  - `grouping_value`: anonymized internal ID for federated networks (e.g., local chapters)
- Financial attributes:
  - `asset_amt`, `income_amt`, `revenue_amt`: financial indicators from IRS filings

**Dimensions:** 1,363,701 rows × 15 columns

**File Access and Format Differences:**

| Format   | File Size | Available At                                                              |
|----------|-----------|---------------------------------------------------------------------------|
| `.parquet` | 31.7 MB   | [GitHub](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/raw_data/ind_org_df.parquet) and [Harvard Dataverse](https://doi.org/10.7910/DVN/IRCA7C) |
| `.csv`     | 105.4 MB  | [Harvard Dataverse](https://doi.org/10.7910/DVN/IRCA7C) only (not hosted on GitHub due to file size limits) |

The `.parquet` file is compressed, columnar, and efficient for large-scale analysis. The `.csv` version is more accessible and human-readable, though significantly larger in size.

---

### Dataset 2: ZIP Code– and County–Level Aggregated Civic Opportunity Counts

Derived from Dataset 1, this dataset aggregates civic opportunity indicators and community characteristics for approximately 31,000 ZIP codes and 3,200 counties. Each row represents a ZIP Code Tabulation Area (ZCTA) or county and includes:

- Organizational counts:
  - `n`: total nonprofit organizations
  - `civic_org_sum`: total civic opportunity organizations
  - `volunteer_sum`, `membership_sum`, `take_action_sum`, `events_sum`: specific opportunity types
- Civic opportunity scores:
  - `civic_opp_sum`: total score
  - `civic_opp_index`: quintile-based index
  - Normalized indicators (e.g., `civic_opp_sum_normalized`, `volunteer_sum_normalized`)
- Socioeconomic indicators from the American Community Survey (ACS):
  - `POV150`: poverty rate
  - `SNGPNT`: single-parent households
  - `BROAD`: no broadband access
  - `NOHSDP`: no high school diploma
  - `UNEMP`: unemployment rate
  - `REMNRTY`: share of racial or ethnic minority residents

**ZIP Code Level Files:**
- [`zcta_counts_cov.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_counts_cov.csv)  
- [`zcta_counts_cov.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_counts_cov.rds)

**County Level Files:**
- [`cnty_counts_cov.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_counts_cov.csv)  
- [`cnty_counts_cov.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_counts_cov.rds)

---

### Dataset 3: ZIP Code– and County–Level Civic Opportunity Provider Types

Also derived from Dataset 1, this dataset summarizes the types of organizations that provide civic opportunities at the ZIP code and county levels. Each row corresponds to a unique geography–organization type pair and includes:

- Geographic identifiers: `FIPS` (county) or `ZCTA` (ZIP)
- Organizational type classification: `class` (e.g., religious, political, professional)
- Organizational count: `n` (number of organizations of that type)
- Relative frequency: `freq` (share of total civic orgs that fall into that class)
- Primary provider type: `primary_org_cat` (most common type in that geography)

These datasets enable comparative analysis of which types of organizations dominate civic life in different regions.

**ZIP Code Level Files:**
- [`zcta_civic_org_type.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_civic_org_type.csv)  
- [`zcta_civic_org_type.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/zcta_civic_org_type.rds)

**County Level Files:**
- [`cnty_civic_org_type.csv`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_civic_org_type.csv)  
- [`cnty_civic_org_type.rds`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/data_outputs/cnty_civic_org_type.rds)

---

## Data Description and Validation

- **Data description:** [`02_description.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/02_description.Rmd)  
  - Produces Figures 2–3 and Tables 4–5  

- **Data validation:** [`03_validation.Rmd`](https://github.com/snfagora/american_civic_opportunity_datasets/blob/main/src/03_validation.Rmd)  
  - Produces Figures 4–7