#!/bin/bash
 

#SBATCH --job-name=taxon_corr_try_1
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=8G
#SBATCH --cpus-per-task=4

#SBATCH -o /work/%u/%x-%j.txt

# send mail with start, end or abort for this job:
#SBATCH --mail-type=ALL     


 
module load foss/2020b R/4.0.4-2
output_dir=/work/$USER/$SLURM_JOB_NAME

mkdir -p "$output_dir"
cd /work/$USER/$SLURM_JOB_NAME/



Rscript /home/klinkr/taxa_correlations.R 

