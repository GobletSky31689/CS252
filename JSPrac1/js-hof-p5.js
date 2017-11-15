/**
   Given an array of integers and an array of strings, produce
   a string that associates the integers with the strings, like
   this: [3, 2, 1], ["french hens", "turtle doves", 
   "Partridge in a pear tree"] ->
   "3 french hens, 2 turtle doves, and 1 partridge in a pear tree"
   Note the spaces after the numbers, commas after the strings,
   and the word "and " before the last item.
   You can assume that the two arrays have equal length.
   Use reduceRight. No loops, no recursion. 
*/
function numsAndStrings(nums, strings) {    
    if (nums.length == 0) return "";
    return nums.reduceRight(function(acc, val, index){
        if(index == nums.length-1)
            return val +  " " + strings[index] + " " + acc;
        else if(index == nums.length-2)
            return val +  " " + strings[index] + " and " + acc;
        else
            return val +  " " + strings[index] + ", " + acc;
      }, "");
}

// console.log(numsAndStrings([3, 2, 1], ["french hens", "turtle doves", 
//    "Partridge in a pear tree"]));
