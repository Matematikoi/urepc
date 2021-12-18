#!/bin/bash
# FILENAME:  myjob.sub

while getopts r: flag
do
    case "${flag}" in
        r) ram=${OPTARG};;
    esac
done

module load r


echo ram : $ram GB


Rscript parallel/consume_ram.R gb_to_consume=$ram
