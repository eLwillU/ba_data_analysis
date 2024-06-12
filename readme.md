# Data Analysis for Bachelor's Thesis

This repository contains scripts used for the data analysis of our bachelor's thesis.
The lookup table in the data folder contains the Question texts etc. to the corresponding questions.
Furhtermore some indices were set to calculate the values.

## Scripts

### 1. Keep_normal.R

**Purpose:**  
This script calculates scores according to the scape study methodology by assigning 1's and 0's to answer options.

### 2. Correlation.R

**Purpose:**  
This script generates correlation matrices without altering the original answers. This approach enhances the accuracy of additional evaluations such as linear regressions, which are also implemented within this script.

---

### Explanation:

- **Keep_normal.R:**
  - Calculates scores based on the scape study methodology.
  - Assigns binary values (1's and 0's) to answer options.
  
- **Correlation.R:**
  - Generates correlation matrices.
  - Preserves the original data for accurate subsequent analyses.
  - Includes implementation of linear regression models.
