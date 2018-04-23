# kakuro-solver
A Fortran software to solve kakuro problems using logic

1) To compile the code :

gfortran Kakuro_logic.f90 moves-arithmetic.f90 Adrianne_thread.f90 moves-binding.f90 moves-chains.f90 moves-hidden-sets.f90 moves-unique-rectangle.f90 moves-wings.f90

2) To run the code : ./a.out > output

3) I have prepared the input files for 7X7 kakuro puzzles.. The code works with any rectangular
   grid kakuro puzzle. To work with other grid size puzzles, simply change the values of following
   2 variables in the file : Kakuro_logic.f90 (in MODULE global_variables)

  INTEGER,PARAMETER :: max_grid_size1 = 7
  INTEGER,PARAMETER :: max_grid_size2 = 7

  e.g. for 9X8 grid change the above variables to the following

  INTEGER,PARAMETER :: max_grid_size1 = 9
  INTEGER,PARAMETER :: max_grid_size2 = 8

4) The code needs a input file named : kakuro-initial-rect

5) I have added sample input files in the directory input-files. There are also solutions
   added in the directory.

6) To run the code simply copy one of the input files to "kakuro-initial-rect".
   e.g. cp 7X7/initial-183 kakuro-initial-rect

   To run one of the 9X8 input files simply change the variables in Kakuro_logic.f90
      INTEGER,PARAMETER :: max_grid_size1 = 9
      INTEGER,PARAMETER :: max_grid_size2 = 8
   and copy one of the input files to "kakuro-initial-rect"
   e.g. cp 9X8/initial-6102 kakuro-initial-rect


7) The input file is given in a specific format. The file "input_example" is a sample input
   file with the comment on how to prepare the input files

8) IMPORTANT :: I have commented out 1 subroutine calls as these subroutines take a long
   time. If you want to include them then uncomment the following 2 lines in
   Kakuro_logic.f90 file

   !  CALL Crisscross_arithmetic

   This  subroutine adds more logic to the Kakuro solver.. But it takes a long time to
   implement them.


I have also written a code to convert the kakuro puzzles from your format to my format

The code is jim-solution-format.f90.
change the following lines in the code accoring to the no. of rows and columns
  INTEGER,PARAMETER :: max_grid_size1 = 31
  INTEGER,PARAMETER :: max_grid_size2 = 21


I am attaching 2 files in your format and my format. The files are :

kakuro-initial-jim and kakuro-initial-jim-31X21
