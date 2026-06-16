# Run this once:
# chmod +x run_all.sh
# ./run_all.sh

### Full deployment with logs ###
#!/bin/bash

for manipulation in spectral #geographical #shuffle
do
    echo "=== $manipulation ==="

    for g in g1 g2 g3 g4 g5 g6
    do
        mkdir -p logs
        Rscript full_deploy.R "$manipulation" "$g" \
            > "logs/${manipulation}_${g}.log" 2>&1 &
        pids+=($!)
    done

    wait "${pids[@]}"
    unset pids

    echo "Done: $manipulation"
done


### FULL DEPLOYMENT ###
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


### Deployment in 3 groups ###

# for manipulation in geographical spectral shuffle
# do
#     echo "Starting $manipulation"

#     Rscript full_deploy.R "$manipulation" g21a &
#     pid1=$!

#     Rscript full_deploy.R "$manipulation" g20 &
#     pid2=$!

#     Rscript full_deploy.R "$manipulation" g21b &
#     pid3=$!

#     wait $pid1 $pid2 $pid3

#     echo "Finished $manipulation"
# done


