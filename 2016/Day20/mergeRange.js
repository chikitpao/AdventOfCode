function mergeRange(ranges){
    let mergedRange = new Array();
    for(let range of ranges){
        if(!mergedRange.length){
            mergedRange.push(range);
            continue;
        }

        // Process from the end of list, extend mergedRange if necessary
        let i = mergedRange.length - 1;
        while(i >= 0){
            if(i == mergedRange.length - 1){
                if(range[0] > mergedRange[i][1] + 1){
                    // New range is behind the last existing range and cannot be 
                    // merged to this range -> Append new range
                    mergedRange.push(range);
                } else if (range[1] >  mergedRange[i][1]) {
                    // New range is behind the last existing range and can be merged
                    // to this range -> Extend end of mergedRange
                    mergedRange[i][1] = range[1];
                }
            } else {
                if((range[0] > mergedRange[i][1] + 1) && (range[1] < mergedRange[i + 1][0] - 1)){
                    // New range is between current and next existing range and cannot be merged
                    // -> Insert new range
                    mergedRange.splice(i + 1, 0, range);
                } else {
                    if(range[1] >= mergedRange[i + 1][0] - 1){
                        if(range[0] > mergedRange[i][1] + 1){
                            if(range[0] <= mergedRange[i + 1][0]){
                                // Merge next range with new range
                                mergedRange[i + 1][0] = range[0];
                            }
                        } else {
                            // Merge next range with current range
                            mergedRange[i][1] = mergedRange[i + 1][1];
                            mergedRange.splice(i + 1, 1);
                        }
                    } else if ((range[1] > mergedRange[i][1]) && (range[0] <= mergedRange[i][1])) {
                        // Merge current range with new range
                        mergedRange[i][1] = range[1];
                    }
                }
            }
            
            if (i == 0)
            {
                if(range[1] < mergedRange[i][0] - 1){
                    // New range is in front of the first existing range and cannot be merged to this range
                    // -> Insert new range to the front
                    mergedRange.splice(0, 0, range);
                } else if((range[1] >= mergedRange[i][0] - 1) && (range[0] <= mergedRange[i][0])){
                    // New range is in front of the first existing range and can be merged to this range
                    // -> Extend start of mergedRange
                    mergedRange[i][0] = range[0];
                }
            }

            i--;
        }
    }
    return mergedRange;
}