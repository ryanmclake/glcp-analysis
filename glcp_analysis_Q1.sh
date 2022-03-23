#!/bin/bash
#SBATCH --partition=kamiak         ### Partition (like a queue in PBS)
#SBATCH --job-name=analyze_glcp    ### Job Name
#SBATCH -o load_glcp.out          ### File in which to store job output
#SBATCH --time=1-12:00:00          ### Wall clock time limit in Days-HH:MM:SS
#SBATCH --nodes=2                  ### Node count required for the job
#SBATCH --ntasks-per-node=16        ### Number of tasks to be launched per Node
#SBATCH --cpus-per-task=32         ### Number of threads per task (OMP threads)
#SBATCH --mail-type=ALL # Email notification: BEGIN,END,FAIL,ALL
#SBATCH --mail-user=ryan.mcclure@wsu.edu # Email address for notifications
#SBATCH --get-user-env             ### Import your user environment setup
#SBATCH --verbose                  ### Increase informational messages
#SBATCH --mem=512000          ### Amount of memory in MB


cd "data/katz/projects/glcp-analysis/"
echo
echo "--- We are now in $PWD, running an R script ..."
echo

# Load R on compute node
module load r/4.1.0

echo "Run download_process_glcp.R"

Rscript --vanilla download_process_glcp.R