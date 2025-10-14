# File formats

## Raw read counts

Mandatory column with gene names, either __Gene_name__ (with hugo gene symbols) or __Ensembl_ID__
Rows are genes, and columns are samples:

|  Gene_name | Sample1 | Sample2 | Sample3 | ... |
|------------|---------|---------|---------|-----|
|    name1   | value   | value   | value   | ... |
|    name2   | value   | value   | value   | ... |
|    name3   | value   | value   | value   | ... |
|    ...     | ...     | ...     | ...     | ... |
  
## Sample Plan

Mandatory column: __SampleID__ , followed by SampleGroup, and any additional grouping

|  SampleID  | SampleGroup | OtherGroups | OtherGroups2 | ... |
|------------|-------------|-------------|--------------|-----|
|  Sample1   | group1      | value       | value        | ... |
|  Sample2   | ...         | value       | value        | ... |
|  Sample3   | group2      | value       | value        | ... |
|    ...     | ...         | ...         | ...          | ... |