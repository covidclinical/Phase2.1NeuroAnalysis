# Neurological Diagnoses in Hospitalized COVID-19 Patients Associated With Adverse Outcomes: A Multinational Cohort Study

Meghan R Hutch BS\*, Jiyeon Son MD\*, Trang T Le PhD\*, Chuan Hong PhD\*, Xuan Wang PhD\*, Zahra Shakeri Hossein Abad PhD\*, Michele Morris BA, Alba Gutiérrez-Sacristán PhD, Jeffrey G Klann MEng, PhD, Anastasia Spiridou PhD, Ashley Batugo BS, Riccardo Bellazzi MS, PhD, Vincent Benoit PhD, Clara-Lea Bonzel MSc, William A Bryant PhD, Lorenzo Chiudinelli PhD, Kelly Cho, MPH, Priyam Das PhD, Tomás González González MD, David A Hanauer MD, MS, Darren W Henderson BS,Yuk-Lam Ho MPH, Ne Hooi Will Loh MBBS, Adeline Makoudjou MD, Simran Makwana MS, Alberto Malovini PhD, Bertrand Moal MD, PhD, Danielle L Mowery PhD, Antoine Neuraz MD, PhD, Malarkodi Jebathilagam Samayamuthu MD, Fernando J Sanz Vidorreta BS, Emily R Schriver MS, Petra Schubert MPH, Jeffery Talbert PhD, Amelia LM Tan BSc, PhD, Byorn W.L. Tan MBBS, Bryce W.Q. Tan MBBS, Valentina Tibollo MS, Patric Tippman MSc, Guillaume Verdy MSc, William Yuan PhD, Paul Avillach MD, PhD, Nils Gehlenborg PhD, Gilbert S Omenn MD, PhD, The Consortium for Clinical Characterization of COVID-19 by EHR (4CE), Shyam Visweswaran MD, PhD\*\*, Tianxi Cai ScD\*\*, Yuan Luo PhD\*\*, Zongqi Xia MD, PhD\*\*

$^{*}$*Share first co-authorship,* $^{**}$*Share senior co-authorship*

[A Consortium for Clinical Characterization of COVID-19 by EHR study (4CE)](https://covidclinical.net/)

# **Patient Population**

Our study leverages a multinational study of \> 106,000 hospitalized COVID-19 adult and pediatric positive patients from 21 healthcare systems across 6 countries. We examined and report the clinical health outcomes (length of hospital stay and mortality) of COVID-19 patients with concurrent neurological diagnoses.

Specifically, we used ICD codes to stratify patients into three groups of patients with 1) no neurological condition (NNC), 2) a central nervous system condition (CNS), or 3) a peripheral nervous system condition (PNS) in order to evaluate the risk of poor health outcomes by neurologic status.

# **View Code and Analysis:**

[Interactive website](https://covidclinical.github.io/Phase2.1NeuroAnalysis/)

## **Data**

To preserve patient confidentiality, the 4CE does not have permission from each contributing healthcare system to release patient-level electronic health records (EHR) data for public access. Indeed, our unique federated study design leveraged a meta-analysis approach for analyzing the summary statistics estimated at each healthcare system without directly using the patient-level EHR data, which are curated by the local informatics and clinical team of each healthcare system.

The summary statistics generated from each each healthcare system can be found in `/results` and `/results_comorbidity`.

These publicly available resources not only enable readers to reproduce all of our reported results, but also perform their own secondary analyses.

To our knowledge, we have curated and made available one of the largest multinational datasets containing the aggregated counts, proportions, and clinical trajectories of hospitalized acute COVID-19 adults and children with and without concurrent central and peripheral nervous system diagnoses.

## **Data pre-processing**

1.  `01_healthcare-system-metadata.Rmd` - computes the number of adult and pediatric patients at each healthcare system.

    a\. Used for **Supplementary Table 1. Description of participating healthcare systems**

    b\. Supplementary Table 1 also contains healthcare system data as reported on the [4CE Health Systems Participating google sheet](https://docs.google.com/spreadsheets/d/1Xl9juDBXt86P3xQtsoTaBl2zPl1BIiAG9DI3Rotyqp8/edit#gid=212461777) on Dec 26, 2022

2.  `02_demographic-preprocess.Rmd` - processes the demographic data by healthcare system.

    a\. Used to generate **Figure 2. Demographic profile at each participating healthcare system**

    b\. Figure 2 is generated by subsequently running `/demographic-figure/demographics.R` and creating final touch ups using Keynote on Mac iOS.

3.  `03_comorbidity-preprocess.Rmd` - this pre-processes the comorbidity data by healthcare system.

    *Of note, this script reads in healthcare system data stored in the `results_comorbidity/`. After most sites ran the initital package, we added an additional comorbidity table to the output file. For this reason, sites reran just the comorbidity analysis and generated the `[site]_comorb_results.rda` file.*

## **Meta-Analysis**

1.  `Patient-Characteristics.Rmd` - generate preliminary demographics, clinical course, and comorbidity analyses

2.  `Meta-Analysis-Cox-PH.Rmd` - performs the random-effects meta-analysis on the Cox-PH models at each site and plots forest plots to visualize the local and global risk estimates.

3.  `Meta-Analysis-KM-Survival-Curves.Rmd` - performs the random-effects meta-analysis on the covariate adjusted Kaplan-Meier survival curves.

4.  `Meta-Analysis: Cox-PH Sensitivity Analysis` - performs additional sensitivity analysies to evaluate the performance of Cox proportional hazard models constructed using three censor cutoff periods (30, 60, and 90 days) and three methods of adjusting for pre-admission health conditions. The analysis also contains a meta-analysis to evaluate the estimates of the individual covariates (e.g., age, comoridities, etc) on each outcome.

------------------------------------------------------------------------

[2021 Preprint Available](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4057133)

*Revised Manuscript currently under review*
