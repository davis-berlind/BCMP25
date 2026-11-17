#!/bin/bash
#SBATCH --job-name=gibbs_sim
#SBATCH --chdir=.
#SBATCH --output=joblog.%A.%a
#SBATCH --error=joblog.%A.%a
#SBATCH --time=24:00:00
#SBATCH --mem=8G
#SBATCH --mail-type=FAIL,ARRAY_TASKS
#SBATCH --mail-user=dberlind@g.ucla.edu
#SBATCH --array=1-500

# Echo job information
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on: $(hostname -s)"
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on: $(date)"
echo " "

# load the job environment:
module load R/4.6.1

## Simulation Parameters
export PRIOR='var.weighted'

# Run code
mkdir -p ./output

echo "/usr/bin/time -v Rscript ./multi_prior_batch_job.R > ./output/output.prior${PRIOR}.j${SLURM_ARRAY_TASK_ID} 2>&1"

/usr/bin/time -v Rscript ./multi_prior_batch_job.R \
    > "./output/output.prior${PRIOR}.j${SLURM_ARRAY_TASK_ID}" 2>&1

# Echo job information
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} ended on: $(hostname -s)"
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} ended on: $(date)"
echo " "
