Archived working steps that are out of date or not needed anymore in the README.md overview.

[Plots](#plots)

**Proposed Tiles, overview:**

**First selection:**
|TILE| Continent   | Country | LAT | Biome    | 
|:-------:|:--------:|:-----:|:-----:|:--------------------------------|
| 34LCL | Africa   | Angola |    -13.2| Subtropical Grasslands, Savannas   |
| 50TPT |Asia      | Mongolia |    47.4| Temperate Grasslands + Temperate Conifer| 
| 54MTU |Asia      | Indonesia |    -5.9| Tropic. Moist Broadlwaf + Mangroves  | 
| 32TMT |Europe    | Switzerland |   47.4| Temp. Broadleaf Coniferous + Broadleaf |
| 35VML |Europe        | Finland|    63.5 | Boreal Forest   | 
| 10UFA |North America | Canada |   50.0 | Temperate Coniferous  | 
| 17SNB |North America | USA |   37.5| Tempered Broadleaf   |    
| 55HEV |Oceania       | Australia |  -36.6 | Temp. Broadleaf + Montane Grass/Shrub |  
| 21MYR |South America | Brazil |   -4.1| Trop. Moist Broadleaf  | 


**All proposed tiles:**
|Continent     | Latitude|Name  |Country                          | Biome | Centeroid_Elevation|Source | Selection |
|:-------------:|:--------:|:-----:|:--------------------------------|:------------|:-------------------:|:------:|:----:|
|Africa        |     33.8|30SUC |Morocco                          | Mediterranean Forest + Montane            |                894|MU     | |
|Africa        |    -13.2|34LCL |Angola                           | Subtropical Grasslands, Savannas          |               1449|MU     | X |
|Africa        |     -4.1|34MCA |Democratic Republic of the Congo | (SubTrop.) Moist Broadleaf + Grass/Shrub  |                366|MS     | |
|Africa        |    -21.3|38KQB |Madagascar                       | Moist Broadleaf Forests, mountains        |                775|MU     | |
|Asia          |     26.6|47RMK |Myanmar                          | Trop. Moist Broadleaf + Temperate Conifer |               2443|MU     | |
|Asia          |     14.0|48PXA |Cambodia                         | Tropical Dry Broadleaf                    |                109|MS     | |
|Asia          |      1.3|49NHB |Indonesia                        | Tropical Moist Broadleaf                  |                780|MU     | |
|Asia          |     47.4|50TNT |Mongolia                         | Temperate Grasslands + Temperate Conifer  |                687|MU     | 50TPT |
|Asia          |     -5.9|54MTU |Indonesia                        | Tropic. Moist Broadlwaf + Mangroves       |                 26|MS     |  |
|Europe        |     47.4|32TMT |Switzerland                      | Temp. Broadleaf Coniferous + Broadleaf    |                590|BOTH   | X |
|Europe        |     48.2|32UQU |Germany                          | Temperate Broadleaf + Coniferous          |                422|MU     | X |
|Europe        |     63.5|35VML |Finland                          | Boreal Forest                             |                201|MS     | X |
|Europe        |     53.0|34UFD |Poland                           | Temp. Broadleaf                           |                   |Lukas  | X |
|North America |     50.0|10UFA |Canada                           | Temperate Coniferous                      |                751|MS     |  |
|North America |     47.4|11TNN |United States of America         | Temperate Coniferous                      |               1092|MU     | |
|North America |     37.5|17SNB |United States of America         | Tempered Broadleaf                        |                682|BOTH   | X |
|Oceania       |    -13.2|52LFL |Australia                        | Subtropical Grasslands, Savannas          |                  8|MU     | |
|Oceania       |    -36.6|55HEV |Australia                        | Temp. Broadleaf + Montane Grass/Shrub     |                562|MS     | X |
|South America |    -16.8|19KGB |Bolivia                          | Trop. Moist Broadleaf + Montane Grasslands|               3888|MU     | |
|South America |     -1.4|20MMD |Brazil                           | Trop. Moist Broadleaf                     |                 56|MS     | X |
|South America |     -4.1|21MYR |Brazil                           | Trop. Moist Broadleaf                     |                147|MU     |  |
|North America |     |10TES |USA                           |  Temperate Broadleaf Forest                  |                |PAPER     | X |
|Africa |     |33NTG |Cameroon                           | Tropical Moist Broadleaf                      |                |PAPER     | X |
|Asia |     |49NHC |Canada                           |  Tropical Moist Broadleaf                     |                |PAPER     | X |




## Plots:

Following is the first example plot illustrating the impact of band value modifications on prediction outcomes. The x-axis represents the relative increment applied to each spectral band (e.g., 0.05 corresponds to a 5% increase: Band × 1.05), while the y-axis shows the average change in prediction values, measured in meters. Positive values indicate an increase in the predicted variable, and negative values indicate a decrease.

![Examplary result plot](plots/2025-06-03_3T_B03+08_lineplot.png)
