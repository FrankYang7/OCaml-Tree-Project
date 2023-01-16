# Timing Results

## Part 1

Enter the timing data for each of the following program. Place the
"real time" measurement in the specified place at the end of the line.

(It the program takes longer than 5 minutes you can terminate its
execution and just indicate "more than 5 minutes" as the time.)

- `time_HW_Lists_Sorted_Words.ml` real time is: 1m24.769s

- `time_HW_Lists_Unsorted_Words.ml` real time is: 1m34.929s

- `time_HW_BSTree_Sorted_Words.ml` real time is: 1m8.718s

- `time_HW_BSTree_Unsorted_Words.ml` real time is: 9.278s

- `time_HW_RBTree_Sorted_Words.ml` real time is: 8.730s

- `time_HW_RBTree_Unsorted_Words.ml` real time is: 8.409s

- `time_HW_TR_Lists_Sorted.ml` real time is: 1m23.742s

- `time_HW_TR_Lists_Unsorted.ml` real time is: 1m34.137s

- `time_HW_TR_BSTree_Sorted.ml` real time is: 1m25.743s

- `time_HW_TR_BSTree_Unsorted.ml` real time is: 7.659s

- `time_HW_TR_RBTree_Sorted.ml` real time is: 7.155s

- `time_HW_TR_RBTree_Unsorted.ml` real time is: 7.342s

## Part 2

Write your answer to the following questions just below the question.

### Question 1

Explain why is the time for `time_HW_Lists_Sorted_Words.ml` so similar to 
the time for `time_HW_Lists_Unsorted_Words.ml`. 

The main reason is that the order of the given lists doesn't matter for the lists search 
(linear search) algorithm. Since the average running time for linear search is O(n), there must always
be several best cases (or appromixely best cases) exist and several worst cases exist. Also, we do not do
any comparision during the linear algorithm running, the order of the words list is not a matter. This is
the reason of why is the time for the two test so similar.

### Question 2

Explain why is the time for `time_HW_BSTree_Sorted_Words.ml` different from
`time_HW_BSTree_Unsorted_Words.ml`. 

For the Binary search algorithm, the average search time is O(logN). However, during we insert the item
in the Binart search tree, if we use a sorted list as the input, the Nodes in the tree may all be placed on
the right side of the tree. Under this condition, the true running time for this binary search tree will similar with
the linear search algorithm. On the other word, the height of the sorted binary search tree will significant larger than
the unsorted binary search tree. Thus, this is the reason why the unsorted tree is much faster than the sorted tree.

### Question 3

Explain why is the time for `time_HW_RBTree_Sorted_Words.ml` so similar to 
the time for `time_HW_RBTree_Unsorted_Words.ml`. 

I think the main reason is that when we insert a new element in the tree, our insert function will 
balance the RB Tree and this would avoid the problem which we indicated in the sorted Binary search tree (all element
be placed on the single side). Thus, after we finish the insert task for the two RB tree, there should not be a large 
different on the height surface for the two tree. This is the main reason of why the running time for the two tests is 
very similar.

### Question 4

How different are the times for `time_HW_RBTree_Sorted_Words.ml` and
`time_HW_TR_RBTree_Sorted_Words.ml`?  Explain why these times are
related in this way.

Compared with the RBTree, TR RBTree is 1 second faster in my testing. 
The main reason which caused this result is that we do not access the memory 
for the list so far (I used the same variable name in each function). I guess this
rewrite may not have a very significant different between the normal recursion one
and the Tail recursion one since the given list is not large enough.

### Quetions 5

Given the effort to write tail recursive functions, does this
optimization seem worth the effort?

For the given list (no matter the sorted or unsorted), I think the result is yes.
The reason is that there is no significant different between the comparison testsing.
However, if we have a very large list (100,000 items or more), the running time different
may seems very obviously.