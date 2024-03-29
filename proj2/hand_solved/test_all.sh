directory=$(dirname $0)
echo "$directory"

make test
output=test_results.out
echo "" > $output

for file in $directory/*.txt; do
    basename=$(basename $file)
    echo "Testing $basename:" | tee -a $output
    ./test-flp23-log < $file > result.tmp
    python $directory/compare_solutions.py $file result.tmp | tee -a $output
done
