---
editor_options: 
  markdown: 
    wrap: sentence
---

# OTPH_TF

## Pieces of code for Temporal Oxytocin with time-frequencies paper

-   **code_MarieStuff.R** runs the main statistical analysis (LMM) and posthocs for *individual frequencies*.
    dependencies: nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, car, emmeans, effectsize, simr Note: I'm almost sure that some of these libraries are not strictly necessary and/or are redundant, but I know the code runs correctly with all of them installed.

-   **code_MarieStuff_ACC.R** runs the main statistical analysis (LMM) and posthocs for *cross-frequency-couplings*.
    It is very similar to code_MarieStuff.R, with the same dependencies.

-   **code_MarieStuff_behav.R** performs statistical analysis of *behavioral scales*: LMM, posthoc correlations, t-tests; plots of behavioral data.
    dependencies: nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, car, emmeans, effectsize, simr (same remark about redundancy of libraries)

-   **means_sd.R** calculates means and standard deviations of data.
    dependencies: matrixStats

NOTE: All this should run as of June 16, 2025.
Please contact me with questions or if you spot any mistakes: [marie.zelenina\@nih.gov](mailto:marie.zelenina@nih.gov){.email}.

NOTE1: EEG preprocessing was done in MATLAB; the code is published elsewhere.
See the paper below for reference: da Cruz, J. R., Chicherov, V., Herzog, M. H., & Figueiredo, P. (2018).
An automatic pre-processing pipeline for EEG analysis (APP) based on robust statistics.
Clinical Neurophysiology, 129(7), 1427-1437.

NOTE2: To fully reproduce all steps, you'll need MATLAB, Python, and R...
Sorry about that.
I find that for me Python works best for wrangling data, R for stats, and MATLAB for EEG analysis :-/
