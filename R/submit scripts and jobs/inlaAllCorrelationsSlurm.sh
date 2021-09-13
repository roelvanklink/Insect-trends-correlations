#!/bin/bash
 

#SBATCH --job-name==allCorrelationsInlaModels
#SBATCH --time 24:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

#SBATCH -o /work/%u/%x-%j-%a.txt

# send mail with start, end or abort for this job:
#SBATCH --mail-type=ALL     

#SBATCH --array=1-71

echo "processing chunk $SLURM_ARRAY_TASK_ID ..."
 
module load foss/2019b R/4.0.0
mkdir -p /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID
cd /work/$USER/$SLURM_JOB_NAME/$SLURM_ARRAY_JOB_ID
Rscript "array_taxa_correlations.R" 











