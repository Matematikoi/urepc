#!/bin/bash

declare -a max_iter=499
declare -a nsamples=(300 500 700 900)

for idx in $(seq 1 $max_iter);
do
for nsample in "${nsamples[@]}";
do
	echo index $idx sample size: $nsample
	sbatch -A standby -N 1 -n 1 --time=00:05:00 -o results_sim_data/out1_%j.out ./cluster/launch_sim_r.sh -i $idx -n $nsample 
done
done

