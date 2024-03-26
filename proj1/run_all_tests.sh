#############################################################################################################################
#   project: flp-fun (1st project regarding decision trees to Functional and Logic Programming course at FIT, BUT)
#    author: David Mihola (xmihol00)
#     email: xmihol00@stud.fit.vutbr.cz
#      date: 31. 3. 2024
# file info: Script to execute the test_flp-fun.py test script with different parameters.
#############################################################################################################################
make clean
make
make flp-ref

rm test_log.txt
mkdir -p test_results
cd test_results
mkdir -p default
cd default
python3 ../../test_flp-fun.py | tee -a ../../test_log.txt
cd ..
mkdir -p max_depth
cd max_depth
python3 ../../test_flp-fun.py -ds p h w -md 4 8 -fa "-md 4" "-md 8" -tt t | tee -a ../../test_log.txt
cd ..
mkdir -p min_sample_split
cd min_sample_split
python3 ../../test_flp-fun.py -ds p h w -mss 3 8 -fa "-mss 3" "-mss 8" -tt t | tee -a ../../test_log.txt
cd ..
mkdir -p min_sample_leaf
cd min_sample_leaf
python3 ../../test_flp-fun.py -ds p h w -msl 4 7 -fa "-msl 4" "-msl 7" -tt t | tee -a ../../test_log.txt
cd ..
mkdir -p all_params
cd all_params
python3 ../../test_flp-fun.py -ds p h w -md 4 8 -mss 3 8 -msl 4 7 -fa "-md 4 -mss 3 -msl 4" "-md 8 -mss 7 -msl 6" -tt t | tee -a ../../test_log.txt
cd ../..
