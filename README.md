# Experimental exploration of the GCHM by [Nico Lang](https://langnico.github.io/globalcanopyheight)

This repository tracks the exploration of the Model presented in [A high-resolution canopy height model of the Earth](https://arxiv.org/abs/2204.08322). **Goal** of this work is to understand how the model works and predicts, if any ecological relationships between spectral data and canopy height can be identified and how the model reacts to changes in input data.
For a full explanation of the installation, setup and deployment see the original instructions: [Installation](https://github.com/langnico/global-canopy-height-model#installation-and-credentials).


## Table of Contents
1. [Results](#results)
2. [Workflow](#workflow)
   - [2.1 Tiles](#tiles)
3. [Citation](https://github.com/ESA99/canopy_height#citation)

## Results
Example plot illustrating the impact of band value modifications on prediction outcomes. The x-axis represents the relative increment applied to each spectral band (e.g., 0.05 corresponds to a 5% increase: Band × 1.05), while the y-axis shows the average change in prediction values, measured in meters. Positive values indicate an increase in the predicted variable, and negative values indicate a decrease.

![Examplary result plot](plots/2025-06-03_3T_B03+08_lineplot.png)


## Workflow
The deploy.R script contains the full workflow and is deployed from bash after setting the correct conda environment and directory.
At the beginning Tile-Name, Bands, Increment and direction are Set and then the script is deployed. The original code (deploy.py etc.) were modified to fit the script.

Fuctions from the package "dandelion" (https://github.com/ESA99/dandelion) were used and written specifically for this usecase.

#### Working Time
At the moment the deployment on the Supercomputer at ILÖK takes on average 00:10:43 per loop (Tile-Band-increment-combination).

### Tiles
How many global tiles? 30, 50, 100?
-> select randomly

Corresponding Worldcover as tiles needed!


## Citation

This work is based on the paper and repository by
Lang, N., Jetz, W., Schindler, K., & Wegner, J. D. (2023). A high-resolution canopy height model of the Earth. Nature Ecology & Evolution, 1-12.
```
@article{lang2023high,
  title={A high-resolution canopy height model of the Earth},
  author={Lang, Nico and Jetz, Walter and Schindler, Konrad and Wegner, Jan Dirk},
  journal={Nature Ecology \& Evolution},
  pages={1--12},
  year={2023},
  publisher={Nature Publishing Group UK London}
}
```

