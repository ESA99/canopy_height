#### RESULTS ####

Result tables as output of the model deployment (deploy.R) are exported to this folder (/results). They are named after the date of their deployment and often further specified. Most are moved to archive as complete result tables were created. The final result tables to be used for analysis are named "main". Currently these main result files exist:

2025-10-20_main.csv -> Contains all Tiles combined with all Bands individually. This is the core result table of the whole analysis.

2026-01-29_main_interactions.csv -> Also contains all Tiles but analysed 4 interaction groups and band blue for reference. The combined manipulated bands are grouped as: High, Low, RGB, All and Blue for reference.


Additional subfolders contain backup results and unmanipulated predictions of the tiles.

- /fallback = Automatic backup system if exporting the complete result table of a deployment does not work.
- /loop_backup = Location for individual loop backup tables, if backup is set to TRUE (deploy.R)
- /originals = 11 unmanipulated predictions, of the selected tiles for the analysis.
