#!/bin/bash

while getopts i:n: flag
do
    case "${flag}" in
        i) idx=${OPTARG};;
	n) nsample=${OPTARG};;
    esac
done

echo idx=$idx
echo nsample=$nsample

Rscript original_code_polished/compare_sim_results.R idx=$idx nsample=$nsample
