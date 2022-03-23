#!/bin/bash
#SBATCH --partition=kamiak             ### Partition (like a queue in PBS)
#SBATCH --job-name=analyze_glcp          ### Job Name
#SBATCH -o analyze_glcp.out              ### File in which to store job output
#SBATCH --time=7-00:00:00                ### Wall clock time limit in Days-HH:MM:SS
#SBATCH --nodes=1                        ### Node count required for the job
#SBATCH --ntasks-per-node=1              ### Number of tasks to be launched per Node
#SBATCH --cpus-per-task=16               ### Number of threads per task (OMP threads)
#SBATCH --mail-type=END                  # Email notification: BEGIN,END,FAIL,ALL
#SBATCH --mail-user=ryan.mcclure@wsu.edu # Email address for notifications
#SBATCH --get-user-env                   ### Import your user environment setup
#SBATCH --verbose                        ### Increase informational messages
#SBATCH --mem=2048000          			     ### Amount of memory in MB
#SBATCH --mem-per-cpu=12G

cd "data/katz/projects/glcp-analysis/"
echo
echo "--- We are now in $PWD, running an R script ..."
echo

# Load R on compute node
module load proj/5.1.0
module load geos/3.6.2
module load gdal/2.3.1
module load r/4.1.0

echo "Run Make_SQL.R"

Rscript --vanilla Make_SQL.R