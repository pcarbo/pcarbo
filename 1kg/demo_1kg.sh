#!/bin/bash

URL_PLINK=http://s3.amazonaws.com/plink1-assets/plink_linux_x86_64_20190617.zip
URL_1KG=ftp://ftp.1000genomes.ebi.ac.uk/vol1/ftp/release/20130502/supporting/hd_genotype_chip/ALL.chip.omni_broad_sanger_combined.20140818.snps.genotypes.vcf.gz

# wget $URL_PLINK -O plink.zip
# unzip plink.zip
# wget $URL_1KG -O 1kg.vcf.gz

# Prepare the genotype data for R.
./plink --vcf 1kg.vcf.gz --make-bed --chr 1-22 --allow-extra-chr \
  --geno 0.01 --out 1kg
cut -f 1 20140625_related_individuals.txt > temp.txt
paste temp.txt temp.txt > samples.txt
./plink --bfile 1kg --make-bed --remove samples.txt --out 1kg_unrelated
./plink --bfile 1kg_unrelated --indep-pairwise 1000 500 0.08
./plink --bfile 1kg_unrelated --make-bed --extract plink.prune.in \
  --out 1kg_pruned
./plink --bfile 1kg_pruned --recode A --out 1kg_recoded

# Create the kinship matrix from the genotype data.
Rscript create.kinship.R
