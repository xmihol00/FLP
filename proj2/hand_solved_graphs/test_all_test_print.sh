directory=$(dirname $0)

make test
output=regular_results.out
echo "" > $output

for file in $directory/*.txt; do
    basename=$(basename $file)
    echo "Testing $basename:" | tee -a $output
    ./test-flp23-log < $file > result.tmp
    python3 $directory/compare_solutions.py -ot test $file result.tmp | tee -a $output
done

(grep "FAILED." $output > /dev/null && echo -e "\n\033[0;31mSome tests failed.\033[0m" || echo -e "\n\033[0;34mAll tests passed.\033[0m") | tee -a $output
