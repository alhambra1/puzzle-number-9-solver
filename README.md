# puzzle-number-9-solver

Usage:

Open a command prompt in the directory the file, "solve.exe" is in. (Windows Vista/7: hold down the Shift key and right-click a folder. The context menu will contain an entry, "Open command window here.")

Type, "solve"

Enter TARGET, the highest number to attain

Enter DENSITY, how many moves to choose from at each choice point; higher DENSITY means a more thorough but also
exponentially slower search (between 3 and 6 is recommended)

BOARD is indexed
1 2 3

4 5 6

7 8 9

so enter the number from each index in order

For example, "solve", "9", "6", "5 1 4 8 2 9 6 7 3" 
searches for a way to get a 9,
using 6 moves for each choice point,
on the board
5 1 4

8 2 9

6 7 3

and yields the result [[2,4]
