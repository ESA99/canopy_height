#### RESULTS ####

Result tables as output of the model deployment (deploy.R) are saved within their corresponding Folder in "/runs".
After postprocessing and merging of the data, the result tables are saved here and named by their date, type of Manipulation and "_main".
Currently these main result files exist:

2026_06_geo_main.csv -> Final results after manipulating latitude by +/- 100-15000 km
2026_06_shuffle_main.csv -> FInal results after manipulating spatial Arrangement of different patch sizes (1-64 px)
2026_06_spectral_main.csv -> Final results after manipulating 8 bands individually upt to +/- 25%

2025-10-20_main.csv -> Core result table of the master Thesis by E.Sánchez
2026-01-29_main_interactions.csv -> Also contains all Tiles but analysed 4 interaction groups and band blue for reference. The combined manipulated bands are grouped as: High, Low, RGB, All and Blue for reference.


