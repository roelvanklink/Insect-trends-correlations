#!/bin/bash

#SBATCH --time 0-06:00:00

#SBATCH --cpus-per-task=4

#SBATCH --mem-per-cpu=1G

#SBATCH --job-name=Roel_taxa_cors

#SBATCH -o /work/%u/%x-%A-%a.txt

#SBATCH --array=1-71

echo "processing chunk $SLURM_ARRAY_TASK_ID ..."

module load foss/2020b R/4.0.4-2

mkdir -p /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID
cd /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID

Rscript /data/idiv_ess/Roel/simple_correlations_brms.R
