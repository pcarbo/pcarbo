# Makefile to generate the results for all simulation configurations
# given in the "sims" directory.

SIMS    = $(shell ls sims/sim*.csv)
RESULTS = $(patsubst sims/sim%.csv,results/sim%.RData,$(SIMS))

all: $(RESULTS)

# The > /dev/null part suppresses the console output. Can be removed
# if you would like to see the full output.
results/sim%.RData : sims/sim%.csv normmix.R 
	Rscript normmix.R $< $@ > /dev/null

clean :
	rm -Rf results/*.RData


