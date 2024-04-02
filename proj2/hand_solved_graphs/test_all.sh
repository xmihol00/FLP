directory=$(dirname $0)
echo "$directory"

make test
output=test_results.out
echo "" > $output

all_tests_passed=1
for file in $directory/*.txt; do
    basename=$(basename $file)
    echo "Testing $basename:" | tee -a $output
    ./test-flp23-log < $file > result.tmp
    python $directory/compare_solutions.py $file result.tmp | tee -a $output
    if [ $? -ne 0 ]; then
        all_tests_passed=0
    fi
done

if [ $all_tests_passed -eq 1 ]; then
    echo -e "\033[0;34mAll tests passed.\033[0m"
else
    echo -e "\033[0;31mSome tests failed.\033[0m"
fi
