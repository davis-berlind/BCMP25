#!/bin/bash
#SBATCH --job-name=gibbs_sim
#SBATCH --chdir=.
#SBATCH --output=joblog.%A.%a
#SBATCH --error=joblog.%A.%a
#SBATCH --time=04:00:00
#SBATCH --mem=8G
#SBATCH --mail-type=FAIL
#SBATCH --array=1-1000

# Echo job information
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on: $(hostname -s)"
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} started on: $(date)"
echo " "

# load the job environment:
. /u/local/Modules/default/init/modules.sh
module load R/4.4.1

## Simulation Parameters
export C=10
export RHO=0
export ADAPT='FALSE'

# Run code
mkdir -p ./output

echo "/usr/bin/time -v Rscript ./gibbs_simulation_batch_job.R > ./output/output.C${C}.rho${RHO}.adapt${ADAPT}.j${SLURM_ARRAY_TASK_ID} 2>&1"

/usr/bin/time -v Rscript ./gibbs_simulation_batch_job.R \
    > "./output/output.C${C}.rho${RHO}.adapt${ADAPT}.j${SLURM_ARRAY_TASK_ID}" 2>&1

# Echo job information
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} ended on: $(hostname -s)"
echo "Job ${SLURM_ARRAY_JOB_ID}.${SLURM_ARRAY_TASK_ID} ended on: $(date)"
echo " "
#### gibbs_sim.sh STOP ####
