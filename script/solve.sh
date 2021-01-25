for i in {1..20}
do
zcat Chr${i}.snp.vcf.gz | grep -v "^##" | grep -v "DEL" | grep -v "INS" | awk '$5!~/,/' | awk '$6 >= 100' > Chr${i}.snp.vcf
R --slave --args Chr${i}.snp.vcf < dp.R
rm Chr${i}.snp.vcf
perl get_geno.pl Chr${i}.snp.vcf.dp > Chr${i}.snp.vcf.dp.geno
rm Chr${i}.snp.vcf.dp
done
