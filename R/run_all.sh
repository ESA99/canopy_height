#!/bin/bash

for manipulation in geographical spectral shuffle
do
    echo "Starting $manipulation"

    Rscript full_deploy.R "$manipulation" g21a &
    pid1=$!

    Rscript full_deploy.R "$manipulation" g20 &
    pid2=$!

    Rscript full_deploy.R "$manipulation" g21b &
    pid3=$!

    wait $pid1 $pid2 $pid3

    echo "Finished $manipulation"
done

# Run this once:
# chmod +x run_all.sh
# ./run_all.sh


# ### Put this in R: ###

# args <- commandArgs(trailingOnly = TRUE)

# manipulation <- args[1]
# tile_group <- args[2]

# tile_groups <- list(
#   g1 = c("10TES"),
#   g2 = c("32TMT"),
#   g3 = c("32UQU"),
#   g4 = c("17SNB", "49NHC", "55HEV"),
#   g5 = c("20MMD", "35VML", "49UCP"),
#   g6 = c("33NTG", "34UFD"),
#   g20  = c("32TMT", "33NTG", "35VML", "49NHC"), 
#   g21a = c("10TES", "17SNB", "20MMD", "49UCP"),
#   g21b = c("32UQU", "55HEV", "34UFD")
# )

# tiles <- tile_groups[[tile_group]]
# base_specs$tile <- tiles
# base_specs$manipulation <- manipulation




# #!/bin/bash

# for manipulation in geographical spectral shuffle
# do
#     echo "Starting manipulation: $manipulation"

#     Rscript full_deploy.R "$manipulation" g1 &
#     pid1=$!

#     Rscript full_deploy.R "$manipulation" g2 &
#     pid2=$!

#     Rscript full_deploy.R "$manipulation" g3 &
#     pid3=$!

#     Rscript full_deploy.R "$manipulation" g4 &
#     pid4=$!

#     Rscript full_deploy.R "$manipulation" g5 &
#     pid5=$!

#     Rscript full_deploy.R "$manipulation" g6 &
#     pid6=$!

#     wait $pid1 $pid2 $pid3 $pid4 $pid5 $pid6

#     echo "Finished manipulation: $manipulation"
# done





### Or with logs:

# #!/bin/bash

# for manipulation in geographical spectral shuffle
# do
#     echo "=== $manipulation ==="

#     for g in g1 g2 g3 g4 g5 g6
#     do
#         mkdir -p logs
#         Rscript full_deploy.R "$manipulation" "$g" \
#             > "logs/${manipulation}_${g}.log" 2>&1 &
#         pids+=($!)
#     done

#     wait "${pids[@]}"
#     unset pids

#     echo "Done: $manipulation"
# done