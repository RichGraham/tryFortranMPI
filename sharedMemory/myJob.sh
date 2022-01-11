#!/bin/bash

cd ${SLURM_SUBMIT_DIR}

mpirun -np 4 ./mpi_fortran.out
