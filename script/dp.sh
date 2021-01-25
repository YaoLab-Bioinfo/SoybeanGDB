for i in {1..1}
do
zcat Chr${i}.snp.vcf.gz | grep -v "^##" | grep -v "DEL" | grep -v "INS" | awk '$5!~/,/' | awk '$6 >= 100' > Chr${i}.snp.vcf
R --slave --args Chr${i}.snp.vcf < dp.R
gzip Chr${i}.snp.vcf.dp
rm Chr${i}.snp.vcf
done
