# Gene expression units

CPM, TPM and FPKM are three different mesuring units for expression data.

TPM and FPKM normalize for sequencing depth AND gene length, whereas CPM takes in count ONLY sequencing depth.

## CPM

Counts usually refers to the number of reads that align to a particular feature. **Since counts are NOT scaled by the length of the feature, they are not comparable within a sample without adjusting for the feature length**. Counts are often used by differential expression methods since they are naturally represented by a counting model, such as a negative binomial. Counts per million (CPM) mapped reads are counts scaled by the number of fragments you sequenced (N) times one million. This unit is related to the FPKM without length normalization and a factor of 10^3.

## TPM

Transcripts per million (TPM) is a measurement of the proportion of transcripts in your pool of RNA. When you use TPM, the sum of all TPMs in each sample are the same. This makes it easier to compare the proportion of reads that mapped to a gene in each sample. It also allows to compare different genes within a sample.

## FPKM

Reads per kilobase of exon per million reads mapped (RPKM), or the more generic FPKM (substitute reads with fragments) are essentially the same thing. It is also a measurement of the proportion of transcripts in your pool of RNA. It equally allows to compare different genes within a sample.

https://www.rna-seqblog.com/rpkm-fpkm-and-tpm-clearly-explained/
