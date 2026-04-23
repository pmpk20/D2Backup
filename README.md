Author: Dr Peter King ([p.king1\@leeds.ac.uk](mailto:p.king1@leeds.ac.uk){.email})

Last Change: 23/04/2026

## DRUID Insect survey

### Here you will find the survey design, experimental design, R code, raw responses, and model outputs.

### PLEASE NOTE: Data is not publicly available while we are in the review process.

#### Description:

-   `/Data/Main/` contains anonymised survey data for N = 1,684 online UK respondents from October 2024.
-   `/Survey/` has the full survey reproduced and explained alongside pictures used therein.
-   `/Biowell4/` is a backup for the Shiny app [here](https://pmpk20.shinyapps.io/biowell4/) demonstrating wellbeing scores from insects.
-   Run `00_D2_Replicator.R` to execute all replication scripts in order. Scripts are numbered 01--15 by execution sequence.
-   Data will be made publicly available at publication stage.

#### Directory structure (replication files only):

```         
D2Backup/
├── 00_D2_Replicator.R
├── Data/
│   └── Main/
│       ├── DRUID_resampling_DCE_d2_test2_2024-10-31.xlsx              # raw DCE responses (resampling wave)
│       ├── DRUID_resampling_covariates_anonymised_2024_10_31.xlsx     # raw covariates (resampling wave)
│       ├── DRUID_resampling_timestamps_2024-10-31.xlsx                # raw survey timestamps (resampling wave)
│       ├── Data_Covariates_Step1.csv                                  # [output of 01] wide-format covariates
│       ├── Data_Covariates_Step2.csv                                  # [output of 02] wide-format covariates
│       ├── Data_Covariates_Step3.csv                                  # [output of 03] wide-format covariates
│       ├── database_Step3.csv                                         # [output of 05] long-format CE data
│       ├── Data_Covariates_Spatial_Step5.csv                          # [output of 05] covariates with wellbeing LVs
│       └── Data_Covariates_Spatial_Step6_anonymised.csv               # [output of 06] covariates + class membership
├── Scripts/
│   ├── Setup/
│   │   ├── 01_Druid_Setup_CleaningMain.R
│   │   ├── 02_Druid_Setup_DiscountingMain.R
│   │   ├── 03_Druid_Setup_MergeCE.R
│   │   ├── 04_Druid_Setup_Postcodes.R
│   │   ├── 05_Druid_Setup_SlidersFactorAnalysis.R
│   │   └── 06_Druid_Setup_ClassMembership.R
│   ├── CEModelling/
│   │   ├── 07_Druid_Model_TruncatedLC3C.R
│   │   └── 08_Druid_Model_SimulatedMeanWTP.R
│   ├── Tables/
│   │   ├── 09_Druid_Table_SampleVsQuota.R                        # Table 1
│   │   ├── 14_Druid_Table_SampleVsPopulation.R                   # Table B2
│   │   └── 16_Druid_Table_WellbeingLVs.R                         # Table C3
│   └── Figures/
│       ├── 10_Druid_Figure_WellbeingDistribution.R               # Figure 2
│       ├── 11_Druid_Figure_WTPClasses.R                          # Figure 3
│       ├── 12_Druid_Figure_WellbeingWTP.R                        # Figure 4
│       ├── 13_Druid_Figure_CEDebrief.R                           # Figure B1
│       └── 15_Druid_Figure_WTPClassesDistribution.R              # Figure C1
├── CEOutput/
│   └── Main/
│       └── LCM/
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model.rds          # [output of 07] fitted model object
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_model_PiValues.rds # [output of 07] posterior class probabilities
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_estimates.csv       # [output of 07] parameter estimates
│           ├── D2_Truncated_LC_3C_MXL_NoDR_V3_output.txt          # [output of 07] full model output
│           └── D2_Truncated_LC_3C_MXL_NoDR_V3_SimulatedMeans_Wide.csv  # [output of 08] simulated WTP
└── OtherOutput/
    └── Figures/
        ├── D2_Figure2_WellbeingDistributions.jpg                   # [output of 10] Figure 2
        ├── D2_Figure3_WTPClasses.jpg                              # [output of 11] Figure 3
        ├── D2_Figure4_WellbeingWTP.jpg                            # [output of 12] Figure 4
        ├── D2_FigureB1_CEDebrief.jpg                              # [output of 13] Figure B1
        └── D2_FigureC1_WTPClassesDistribution.jpg                 # [output of 15] Figure C1
```

#### Acknowledgements:

-   Data provided by [SurveyEngine](https://surveyengine.com/).
-   Using RStudio and [Apollo](https://apollochoicemodelling.com/) (Hess and Palma, 2019).
-   This work was funded by the Natural Environment Research Council (NERC) through the [DRUID](https://druidproject.org.uk/) project.
-   Ethical approval for data collection was granted by the School of Earth and Environment Ethics Committee, University of Leeds (Ref: BESS+ FREC 2023-0769-1031).
-   Acknowledgement: Using specialist and High-Performance Computing systems provided by Information Services at the University of Kent, and by the Research Computing Team at the University of Leeds.
