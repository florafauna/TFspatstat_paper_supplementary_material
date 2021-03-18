# Supplementary material for the article: _Fast covariance parameter estimation of spatial Gaussian process models using neural networks_

A pre-print is available on arXiv: https://arxiv.org/abs/2012.15339

This repository contains the scripts and data to reproduce the results presented in the article (simulation study and data illustration).
It is structured as follows:

* **1_common_data/**: generate and prepare data needed by all models
* **2_model_NI_NI30/**: train the NI and NI30 models and use them to estimate parameters for the test data of the study
* **2_model_NV/**: train the NV model and use it to estimate parameters for the test data of the study
* **2_model_NV30/**: train the NV30 model and use it to estimate parameters for simulation study and the climate data
* **2_model_ML_ML30/**: use the ML and ML30 models to estimate parameters for simulation study and the climate data
* **2_model_GS30/**: use the GS30 model to estimate parameters for simulation study
* **2_model_CL30/**: use the CL30 model to estimate parameters for simulation study
* **3_make_figures/**: make the figures of the manuscript

We recommend running the scripts in this order because of dependencies. 


## A complete oververview:

Files marked by `#` are not included in the repository to reduce its size.
They can be generated using scripts in the (parent) folders.

```bash
├── 1_common_data
│   ├── makeTestData.R
│   ├── makeTrainingData.R
│   ├── npy
│   │   ├── # test_x.npy 
│   │   ├── test_y.npy
│   │   ├── # training_201_200_alphaMat.npy
│   │   ├── # training_201_200_chols.npy
│   │   ├── # training_201_200_df.npy
│   │   ├── # training_201_200_logLambdaMat.npy
│   │   ├── # training_201_200_thetaGrid.npy
│   │   └── # training_201_200_y.npy
│   ├── prepare_BRACEData.R
│   ├── rda
│   │   └── slope.rda
│   │   └── # BRACEData.rda
│   └── Rscripts
│       ├── makeDFGrid.R
│       ├── makeMaternTrainingData1.R
│       ├── prep_for_TFpython.R
│       └── to16x16.R
├── 2_model_ML_ML30
│   ├── rda
│   │   ├── slope_mle.rda
│   │   └── test_mle.rda
│   ├── Rscripts
│   │   └── MLESearchMatern.R
│   ├── BFGS_optim_timing.R
│   ├── run_data.R
│   └── run_sim.R
├── 2_model_GS30
│   ├── npy
│   │   └── test_pred_model_gstat_perfect_start_values.npy
│   └── run_sim.R
├── 2_model_CL30
│   ├── npy
│   │   └── test_pred_model_composite_perfect_start_values_lbfgsb.npy
│   └── run_sim.R
├── 2_model_NI_NI30
│   ├── model_NI
│   │   ├── assets
│   │   ├── saved_model.pb
│   │   └── variables
│   │       ├── variables.data-00000-of-00001
│   │       └── variables.index
│   ├── npy
│   │   ├── scaler_y_model_NI.npy
│   │   ├── scaler_y_model_NI.pkl
│   │   └── test_pred_model_NI.npy
│   ├── run_simulation.R
│   ├── scaler_to_npy.py
│   └── train_model_NI.ipynb
├── 2_model_NV
│   ├── images2multivario.py
│   ├── model_NV
│   │   ├── assets
│   │   ├── saved_model.pb
│   │   └── variables
│   │       ├── variables.data-00000-of-00001
│   │       └── variables.index
│   ├── model_summary.R
│   ├── npy
│   │   ├── # mvgs.npy
│   │   ├── scaler_y_model_NV.npy
│   │   ├── scaler_y_model_NV.pkl
│   │   ├── # subs.npy
│   │   └── test_pred_model_NV.npy
│   ├── run_simulation.R
│   ├── scaler_to_npy.py
│   └── train_model_NV.ipynb
├── 2_model_NV30
│   ├── images2multivario.py
│   ├── model_NV30
│   │   ├── assets
│   │   ├── saved_model.pb
│   │   └── variables
│   │       ├── # variables.data-00000-of-00001
│   │       ├── make_variables.data-00000-of-00001.sh
│   │       ├── variables.index
│   │       ├── xaa
│   │       └── xab
│   ├── model_summary.R
│   ├── npy
│   │   ├── # mvgs.npy
│   │   ├── scaler_y_model_NV30.npy
│   │   ├── scaler_y_model_NV30.pkl
│   │   ├── # subs.npy
│   │   └── test_pred_model_NV30.npy
│   ├── rda
│   │   └── slope_mvg.rda
│   ├── run_data.R
│   ├── run_simulation.R
│   ├── scaler_to_npy.py
│   └── train_model_NV30.ipynb
├── 3_make_figures
│   ├── fig_1.R
│   ├── fig_2.R
│   ├── fig_3.R
│   ├── fig_4.R
│   ├── fig_5_6.R
│   ├── figs
│   │   ├── fig_1.png
│   │   ├── fig_2_bottom.png
│   │   ├── fig_2_top.png
│   │   ├── fig_3.png
│   │   ├── fig_4.png
│   │   ├── fig_5.png
│   │   └── fig_6.png
│   └── Rscripts
│       └── ggplot_theme.R
└── README.md
```
