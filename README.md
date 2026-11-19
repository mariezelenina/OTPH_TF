CODE used for analysis:
code_RR_bands_[rel/abs].rmd runs the main statistical analysis (LMM) and posthocs for each frequency band and each eye condition (eyes open/closed) and saves results in the output .html file. Dependencies: nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, car, emmeans, effectsize, simr.

code_RR_behav_FINAL_CLEANOUTPUT.R performs statistical analysis of behavioral scales: omnibus LMM, slopes estimation, posthoc analysis of timepoints separately; and saves formatted results in output .txt and .csv files. Dependencies: nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, car, emmeans, effectsize, simr

code_RR_behavBASE_FINAL_CLEANOUTPUT.R performs statistical analysis of the effect of baseline behavioral scales on EEG: omnibus LMM, slopes estimation, posthoc analysis of timepoints separately; and saves formatted results in output .txt and .csv files. Dependencies: nlme, CorrMixed, lme4, lmerTest, geepack, ggplot2, car, emmeans, effectsize, simr

means_sd.R calculates means and standard deviations of data. dependencies: matrixStats

DATA:
in NEW_data folder:

relative power data : [EC/EO]_relpower_behavbl.csv
absolute power data : [EC/EO]_abspower_behavbl.csv
Full spectrum data: ...FullSpectrum.csv
NOTE: Last tested Nov 15, 2026. marie.zelenina@gmail.com.
