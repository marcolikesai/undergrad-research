# UCSB EAOP Program Evaluation & Analysis

Welcome to the data repository for our evaluation of the UCSB Early Academic Outreach Program (EAOP) in Ventura County. This project combined R survey analysis, machine learning (LDA topic modeling), and custom data visualizations to assess student outcomes and support educational access.

##  Project Overview

- **Goal:** Evaluate EAOP’s impact on student understanding of A-G requirements and college readiness in Ventura County high schools.
- **Schools Studied:** Channel Islands, Pacifica, Hueneme, Rio Mesa, Santa Paula.
- **Methods:** R data cleaning, survey analysis, topic modeling (LDA), custom plotting, and report writing.

## 🏗 Project Structure

- `data/` — Summarized/cleaned datasets 
- `scripts/` — R scripts for reproducible analysis/visualization, data cleaning, survey analysis, and LDA topic modeling..
- `reports/` — Final PDF reports and sample research poster.
- `figures/` — Example plots and visualizations.

## ⚙ Technical Stack

- **R:** Data cleaning, survey analysis, and plotting (see `.Rmd` scripts).
- **Python (Jupyter):** LDA topic modeling and additional EDA.
- **Libraries:**  
  - R: `tidyverse`, `ggplot2`, `dplyr`, `tidyr`, `topicmodels`
  - Python: `pandas`, `sklearn`, `gensim`, `matplotlib`, `seaborn`

##  Roles & Contributions

- **Lead Analyst/Co-Author:** Marco Montenegro  
  - Designed and cleaned survey datasets  
  - Led statistical analysis and visualization in R  
  - Co-authored evaluation reports  
  - Performed topic modeling (LDA) in Python/Jupyter  
  - Created presentation visuals for stakeholders

- **Collaborators:**  
  - Travis Candieas, Mai Huynh, Sarah Sault, Ju Kim, Dalina Sinn, Alexei Kremliovsky, Elaine Barraza

##  Key Insights

- Strong student familiarity with A-G requirements, but notable gaps in financial aid and elective knowledge.
- High demand for information on college prep electives and financial aid.
- Qualitative focus group data highlights the value of EAOP’s support, especially for first-gen and low-income students.
- Recommendations: Earlier program engagement, expanded advertising, and hands-on scholarship guidance.

##  How to Use

1. **Review Analysis:**  
   - Open `notebooks/EAOP_survey_clean.Rmd` and `notebooks/LDA_topic_modeling.ipynb` for end-to-end analysis.
2. **Reproducibility:**  
   - All code/scripts use public R/Python packages (see scripts/notebooks for details).
3. **Data:**  
   - Only summarized/cleaned datasets are included for privacy.
4. **Reports:**  
   - See `reports/` for full evaluation results and sample outputs.


**Note:** All datasets are anonymized per IRB/data privacy guidelines.

