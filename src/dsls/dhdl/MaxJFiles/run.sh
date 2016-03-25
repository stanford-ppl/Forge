#!/bin/bash
DIR=RunRules/Simulation

ARGS="$@"
make -C ${DIR} runsim RUNARGS="$ARGS"
