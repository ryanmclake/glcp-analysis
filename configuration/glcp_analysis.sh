#!/bin/bash
#SBATCH --partition=katz                      ### Partition (like a queue in PBS)
#SBATCH --job-name=analyze_glcp               ### Job Name
#SBATCH --time=1-12:00:00                     ### Wall clock time limit in Days-HH:MM:SS
#SBATCH --nodes=1                             ### Node count required for the job
#SBATCH --ntasks-per-node=1                   ### Number of tasks to be launched per Node
#SBATCH --cpus-per-task=8                     ### Number of threads per task (OMP threads)
#SBATCH --mail-type=ALL                       # Email notification: BEGIN,END,FAIL,ALL
#SBATCH --mail-user=ryan.mcclure@wsu.edu      # Email address for notifications
#SBATCH --get-user-env                        ### Import your user environment setup
#SBATCH --verbose                             ### Increase informational messages
#SBATCH --mem=2048                            ### Amount of memory in MB


# Load R on compute node
module load r/4.0.2

echo "Runing package_download.R"
echo "Runing read_glcp_data.R"
echo "Runing Q1_direction_magnitude.R"
echo "Runing Q2_similarities_among_biomes.R"
echo "Runing Q3_correlation_among_region.R"

Rscript --vanilla package_download.R
Rscript --vanilla read_glcp_data.R
Rscript --vanilla Q1_direction_magnitude.R
Rscript --vanilla Q2_similarities_among_biomes.R
Rscript --vanilla Q3_correlation_among_region.R
