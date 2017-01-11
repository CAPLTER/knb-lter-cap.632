#!/bin/bash

# assign dataset ID number that will pre-pend to file name
ds="632"

# generate sha1sum for each csv file, and rename file to: dataset_filename_sha.csv
for file in *.zip
do
        base=${file##*/}
        noext=${base%.*}
        sha1=$(md5sum "$file" | awk '{print $1}')
        newfile=${ds}_${noext}_${sha1} 
        mv $file $newfile.zip
done
