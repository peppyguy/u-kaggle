# Script to download Kaggle kernels

DEST="data"
REFS="kernel_refs.csv"

for REF in $(cat $REFS);
do
    kaggle kernels pull $REF -p $DEST
done
