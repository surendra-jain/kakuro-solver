MODULE global_variables

  INTEGER, DIMENSION(3:17,4,9),target :: cell_2
  INTEGER, DIMENSION(6:24,8,9),target :: cell_3
  INTEGER, DIMENSION(10:30,12,9),target :: cell_4
  INTEGER, DIMENSION(15:35,12,9),target :: cell_5
  INTEGER, DIMENSION(21:39,8,9),target :: cell_6
  INTEGER, DIMENSION(28:42,4,9),target :: cell_7
  INTEGER, DIMENSION(36:44,1,9),target :: cell_8
  INTEGER, DIMENSION(45:45,1,9),target  :: cell_9

  INTEGER,DIMENSION(4) :: Rcell_2,Rcell_3,Rcell_4,Rcell_5,Rcell_6,Rcell_7
  INTEGER,DIMENSION(9) :: Rcell_8
  INTEGER,DIMENSION(1) :: RCell_9

  ! The maximum grid size in a kakuro puzzle.. change the maximum grid size for the puzzle
  INTEGER,PARAMETER :: max_grid_size1 = 7
  INTEGER,PARAMETER :: max_grid_size2 = 7
  INTEGER,PARAMETER :: max_cages = 10000

  ! Parameters used in moves_arithmetic
  INTEGER,PARAMETER :: max_2cages = 80000
  INTEGER,PARAMETER :: max_3cages = 80000
  INTEGER,PARAMETER :: max_4cages = 500000

  ! Parameters used in moves-arithmetic
  INTEGER,PARAMETER :: non_overlap_cells_horizontal_max = 100
  INTEGER,PARAMETER :: non_overlap_cells_vertical_max = 100

  INTEGER,DIMENSION(max_2cages,2) :: horizontal_2cages
  INTEGER,DIMENSION(max_3cages,3) :: horizontal_3cages
  INTEGER,DIMENSION(max_4cages,4) :: horizontal_4cages
  INTEGER,DIMENSION(max_4cages,5) :: horizontal_5cages
  INTEGER,DIMENSION(max_4cages,6) :: horizontal_6cages
  INTEGER,DIMENSION(max_4cages,7) :: horizontal_7cages
  INTEGER,DIMENSION(max_4cages,8) :: horizontal_8cages
  INTEGER,DIMENSION(max_2cages,2) :: vertical_2cages
  INTEGER,DIMENSION(max_3cages,3) :: vertical_3cages
  INTEGER,DIMENSION(max_4cages,4) :: vertical_4cages
  INTEGER,DIMENSION(max_4cages,5) :: vertical_5cages
  INTEGER,DIMENSION(max_4cages,6) :: vertical_6cages
  INTEGER,DIMENSION(max_4cages,7) :: vertical_7cages
  INTEGER,DIMENSION(max_4cages,8) :: vertical_8cages
  INTEGER :: nhorizontal_2cages,nvertical_2cages
  INTEGER :: nhorizontal_3cages,nvertical_3cages
  INTEGER :: nhorizontal_4cages,nvertical_4cages
  INTEGER :: nhorizontal_5cages,nvertical_5cages
  INTEGER :: nhorizontal_6cages,nvertical_6cages
  INTEGER :: nhorizontal_7cages,nvertical_7cages
  INTEGER :: nhorizontal_8cages,nvertical_8cages


  ! Parameters used in moves-binding
  INTEGER,PARAMETER :: sum_array_max = 400000
          
  ! Parameters used in moves-hidden and Kakuro_logic..
  ! used in calculating the naked list..
  INTEGER,PARAMETER :: naked_list_max = 300
         
  ! Parameters used in Kakuro_logic..
  INTEGER,PARAMETER :: numbers_max = 9
  INTEGER,PARAMETER :: values_max = 9

  ! 9 is the maximum number of cells that a cage can have.. since all the numbers
  ! belonging to a cage must be unique..
  INTEGER, PARAMETER :: max_cells_in_cage = 9
  INTEGER,DIMENSION(max_cages,max_cells_in_cage,2) :: cage_to_cells
  INTEGER,DIMENSION(max_cages) :: cage_sum
  INTEGER,DIMENSION(max_cages) :: cage_no_of_cells
  INTEGER,DIMENSION(max_cages) :: check_cage
  INTEGER,DIMENSION(max_cages) :: check_cage_sum

  INTEGER,DIMENSION(max_cages) :: restricted_blocks
  INTEGER,DIMENSION(max_cages) :: restricted_blocks_order

  !Stores the cage number for each cell, which cages does each cell belong.
  !A cell can belong to maximum 2 cages
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,2) :: cell_to_cage
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,2) :: cell_to_cage_sum

  INTEGER :: total_no_of_cages,no_of_sudoku_cells

  INTEGER :: check_sudoku,count_restricted

  INTEGER :: solved_cells

  INTEGER, DIMENSION(max_grid_size1,max_grid_size2) :: sudoku
  INTEGER, DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_initial
  INTEGER, DIMENSION(max_grid_size1,max_grid_size2,9) :: sudoku_logic_1,sudoku_logic_2
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_logic_values_1,sudoku_logic_values_2
  INTEGER, DIMENSION(max_grid_size1,max_grid_size2,9) :: sudoku_logic
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_logic_values

  !Comprehensive forcing chains global variables
  INTEGER,DIMENSION(max_grid_size1) :: forcing_loop_stack_row
  INTEGER,DIMENSION(max_grid_size2) :: forcing_loop_stack_column
  INTEGER :: forcing_loop_stack_length
  INTEGER,PARAMETER :: forcing_chain_stack_length_max = 4
  INTEGER,DIMENSION(forcing_chain_stack_length_max) :: forcing_loop_stack_value
  INTEGER :: forcing_chain_check
!  INTEGER :: forcing_ncages_stack
!  INTEGER,DIMENSION(max_cages) :: forcing_loop_cage
  INTEGER,DIMENSION(forcing_chain_stack_length_max+1) :: forcing_ncages_stack
  INTEGER,DIMENSION(forcing_chain_stack_length_max+1,max_cages) :: forcing_loop_cage
  INTEGER :: forcing_chain_stack_check
  INTEGER,DIMENSION(max_cages) :: forcing_loop_cage_old
  INTEGER :: forcing_ncages_stack_old


  INTEGER, DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_cage

  INTEGER,DIMENSION(max_cages) :: horizontal_cages,vertical_cages
  INTEGER :: nhorizontal_cages,nvertical_cages
  INTEGER :: nempty_cells

   ! max_grid_size1*max_grid_size2 is the maximum number of states possible
   
   INTEGER,PARAMETER :: nAdrianneStates_max = 30
   INTEGER,PARAMETER :: Adrianne_grid_size = max_grid_size1*max_grid_size2
   INTEGER, DIMENSION(Adrianne_grid_size,max_grid_size1,max_grid_size2) :: sudoku_Adrianne
   INTEGER, DIMENSION(Adrianne_grid_size,max_grid_size1,max_grid_size2,9) :: sudoku_Adrianne_logic
   INTEGER, DIMENSION(Adrianne_grid_size,max_grid_size1,max_grid_size2) :: sudoku_Adrianne_logic_numbers
   INTEGER, DIMENSION(Adrianne_grid_size) :: Adrianne_state_row
   INTEGER, DIMENSION(Adrianne_grid_size) :: Adrianne_state_col
   INTEGER, DIMENSION(Adrianne_grid_size) :: Adrianne_state_sudoku_logic_number
   INTEGER :: nAdrianneStates

   INTEGER, DIMENSION(Adrianne_grid_size,Adrianne_grid_size,max_grid_size1,max_grid_size2) :: sudoku_snapshot_states
   INTEGER, DIMENSION(Adrianne_grid_size,max_grid_size1,max_grid_size2) :: sudoku_snapshot_initial
   INTEGER,DIMENSION(max_grid_size1*max_grid_size2) :: snapshots_initial
   INTEGER,DIMENSION(max_grid_size1*max_grid_size2) :: duplicate_snapshots


END MODULE global_variables

!---------------------------------------------------------------------------

SUBROUTINE Cage_table

  USE global_variables
  IMPLICIT NONE

  cell_2 = 0
  cell_3 = 0
  cell_4 = 0
  cell_5 = 0
  cell_6 = 0
  cell_7 = 0
  cell_8 = 0
  cell_9 = 0

  cell_2(3,1,1:2) = (/1,2/)
  cell_2(4,1,1:2) = (/1,3/)
  cell_2(5,1,1:2) = (/1,4/)
  cell_2(5,2,1:2) = (/2,3/)
  cell_2(6,1,1:2) = (/1,5/)
  cell_2(6,2,1:2) = (/2,4/)
  cell_2(7,1,1:2) = (/1,6/)
  cell_2(7,2,1:2) = (/2,5/)
  cell_2(7,3,1:2) = (/3,4/)
  cell_2(8,1,1:2) = (/1,7/)
  cell_2(8,2,1:2) = (/2,6/)
  cell_2(8,3,1:2) = (/3,5/)
  cell_2(9,1,1:2) = (/1,8/)
  cell_2(9,2,1:2) = (/2,7/)
  cell_2(9,3,1:2) = (/3,6/)
  cell_2(9,4,1:2) = (/4,5/)
  cell_2(10,1,1:2) = (/1,9/)
  cell_2(10,2,1:2) = (/2,8/)
  cell_2(10,3,1:2) = (/3,7/)
  cell_2(10,4,1:2) = (/4,6/)
  cell_2(11,1,1:2) = (/2,9/)
  cell_2(11,2,1:2) = (/3,8/)
  cell_2(11,3,1:2) = (/4,7/)
  cell_2(11,4,1:2) = (/5,6/)
  cell_2(12,1,1:2) = (/3,9/)
  cell_2(12,2,1:2) = (/4,8/)
  cell_2(12,3,1:2) = (/5,7/)
  cell_2(13,1,1:2) = (/4,9/)
  cell_2(13,2,1:2) = (/5,8/)
  cell_2(13,3,1:2) = (/6,7/)
  cell_2(14,1,1:2) = (/5,9/)
  cell_2(14,2,1:2) = (/6,8/)
  cell_2(15,1,1:2) = (/6,9/)
  cell_2(15,2,1:2) = (/7,8/)
  cell_2(16,1,1:2) = (/7,9/)
  cell_2(17,1,1:2) = (/8,9/)

  cell_3(6,1,1:3) = (/1,2,3/)
  cell_3(7,1,1:3) = (/1,2,4/)
  cell_3(8,1,1:3) = (/1,2,5/)
  cell_3(8,2,1:3) = (/1,3,4/)
  cell_3(9,1,1:3) = (/1,2,6/)
  cell_3(9,2,1:3) = (/1,3,5/)
  cell_3(9,3,1:3) = (/2,3,4/)
  cell_3(10,1,1:3) = (/1,2,7/)
  cell_3(10,2,1:3) = (/1,3,6/)
  cell_3(10,3,1:3) = (/1,4,5/)
  cell_3(10,4,1:3) = (/2,3,5/)
  cell_3(11,1,1:3) = (/1,2,8/)
  cell_3(11,2,1:3) = (/1,3,7/)
  cell_3(11,3,1:3) = (/1,4,6/)
  cell_3(11,4,1:3) = (/2,3,6/)
  cell_3(11,5,1:3) = (/2,4,5/)
  cell_3(12,1,1:3) = (/1,2,9/)
  cell_3(12,2,1:3) = (/1,3,8/)
  cell_3(12,3,1:3) = (/1,4,7/)
  cell_3(12,4,1:3) = (/1,5,6/)
  cell_3(12,5,1:3) = (/2,3,7/)
  cell_3(12,6,1:3) = (/2,4,6/)
  cell_3(12,7,1:3) = (/3,4,5/)
  cell_3(13,1,1:3) = (/1,3,9/)
  cell_3(13,2,1:3) = (/1,4,8/)
  cell_3(13,3,1:3) = (/1,5,7/)
  cell_3(13,4,1:3) = (/2,3,8/)
  cell_3(13,5,1:3) = (/2,4,7/)
  cell_3(13,6,1:3) = (/2,5,6/)
  cell_3(13,7,1:3) = (/3,4,6/)
  cell_3(14,1,1:3) = (/1,4,9/)
  cell_3(14,2,1:3) = (/1,5,8/)
  cell_3(14,3,1:3) = (/1,6,7/)
  cell_3(14,4,1:3) = (/2,3,9/)
  cell_3(14,5,1:3) = (/2,4,8/)
  cell_3(14,6,1:3) = (/2,5,7/)
  cell_3(14,7,1:3) = (/3,4,7/)
  cell_3(14,8,1:3) = (/3,5,6/)
  cell_3(15,1,1:3) = (/1,5,9/)
  cell_3(15,2,1:3) = (/1,6,8/)
  cell_3(15,3,1:3) = (/2,4,9/)
  cell_3(15,4,1:3) = (/2,5,8/)
  cell_3(15,5,1:3) = (/2,6,7/)
  cell_3(15,6,1:3) = (/3,4,8/)
  cell_3(15,7,1:3) = (/3,5,7/)
  cell_3(15,8,1:3) = (/4,5,6/)
  cell_3(16,1,1:3) = (/1,6,9/)
  cell_3(16,2,1:3) = (/1,7,8/)
  cell_3(16,3,1:3) = (/2,5,9/)
  cell_3(16,4,1:3) = (/2,6,8/)
  cell_3(16,5,1:3) = (/3,4,9/)
  cell_3(16,6,1:3) = (/3,5,8/)
  cell_3(16,7,1:3) = (/3,6,7/)
  cell_3(16,8,1:3) = (/4,5,7/)
  cell_3(17,1,1:3) = (/1,7,9/)
  cell_3(17,2,1:3) = (/2,6,9/)
  cell_3(17,3,1:3) = (/2,7,8/)
  cell_3(17,4,1:3) = (/3,5,9/)
  cell_3(17,5,1:3) = (/3,6,8/)
  cell_3(17,6,1:3) = (/4,5,8/)
  cell_3(17,7,1:3) = (/4,6,7/)
  cell_3(18,1,1:3) = (/1,8,9/)
  cell_3(18,2,1:3) = (/2,7,9/)
  cell_3(18,3,1:3) = (/3,6,9/)
  cell_3(18,4,1:3) = (/3,7,8/)
  cell_3(18,5,1:3) = (/4,5,9/)
  cell_3(18,6,1:3) = (/4,6,8/)
  cell_3(18,7,1:3) = (/5,6,7/)
  cell_3(19,1,1:3) = (/2,8,9/)
  cell_3(19,2,1:3) = (/3,7,9/)
  cell_3(19,3,1:3) = (/4,6,9/)
  cell_3(19,4,1:3) = (/4,7,8/)
  cell_3(19,5,1:3) = (/5,6,8/)
  cell_3(20,1,1:3) = (/3,8,9/)
  cell_3(20,2,1:3) = (/4,7,9/)
  cell_3(20,3,1:3) = (/5,6,9/)
  cell_3(20,4,1:3) = (/5,7,8/)
  cell_3(21,1,1:3) = (/4,8,9/)
  cell_3(21,2,1:3) = (/5,7,9/)
  cell_3(21,3,1:3) = (/6,7,8/)
  cell_3(22,1,1:3) = (/5,8,9/)
  cell_3(22,2,1:3) = (/6,7,9/)
  cell_3(23,1,1:3) = (/6,8,9/)
  cell_3(24,1,1:3) = (/7,8,9/)

 cell_4(10,1,1:4) = (/1,2,3,4/)
  cell_4(11,1,1:4) = (/1,2,3,5/)
  cell_4(12,1,1:4) = (/1,2,3,6/)
  cell_4(12,2,1:4) = (/1,2,4,5/)
  cell_4(13,1,1:4) = (/1,2,3,7/)
  cell_4(13,2,1:4) = (/1,2,4,6/)
  cell_4(13,3,1:4) = (/1,3,4,5/)
  cell_4(14,1,1:4) = (/1,2,3,8/)
  cell_4(14,2,1:4) = (/1,2,4,7/)
  cell_4(14,3,1:4) = (/1,2,5,6/)
  cell_4(14,4,1:4) = (/1,3,4,6/)
  cell_4(14,5,1:4) = (/2,3,4,5/)
  cell_4(15,1,1:4) = (/1,2,3,9/)
  cell_4(15,2,1:4) = (/1,2,4,8/)
  cell_4(15,3,1:4) = (/1,2,5,7/)
  cell_4(15,4,1:4) = (/1,3,4,7/)
  cell_4(15,5,1:4) = (/1,3,5,6/)
  cell_4(15,6,1:4) = (/2,3,4,6/)
  cell_4(16,1,1:4) = (/1,2,4,9/)
  cell_4(16,2,1:4) = (/1,2,5,8/)
  cell_4(16,3,1:4) = (/1,2,6,7/)
  cell_4(16,4,1:4) = (/1,3,4,8/)
  cell_4(16,5,1:4) = (/1,3,5,7/)
  cell_4(16,6,1:4) = (/1,4,5,6/)
  cell_4(16,7,1:4) = (/2,3,4,7/)
  cell_4(16,8,1:4) = (/2,3,5,6/)
  cell_4(17,1,1:4) = (/1,2,5,9/)
  cell_4(17,2,1:4) = (/1,2,6,8/)
  cell_4(17,3,1:4) = (/1,3,4,9/)
  cell_4(17,4,1:4) = (/1,3,5,8/)
  cell_4(17,5,1:4) = (/1,3,6,7/)
  cell_4(17,6,1:4) = (/1,4,5,7/)
  cell_4(17,7,1:4) = (/2,3,4,8/)
  cell_4(17,8,1:4) = (/2,3,5,7/)
  cell_4(17,9,1:4) = (/2,4,5,6/)
  cell_4(18,1,1:4) = (/1,2,6,9/)
  cell_4(18,2,1:4) = (/1,2,7,8/)
  cell_4(18,3,1:4) = (/1,3,5,9/)
  cell_4(18,4,1:4) = (/1,3,6,8/)
  cell_4(18,5,1:4) = (/1,4,5,8/)
  cell_4(18,6,1:4) = (/1,4,6,7/)
  cell_4(18,7,1:4) = (/2,3,4,9/)
  cell_4(18,8,1:4) = (/2,3,5,8/)
  cell_4(18,9,1:4) = (/2,3,6,7/)
  cell_4(18,10,1:4) = (/2,4,5,7/)
  cell_4(18,11,1:4) = (/3,4,5,6/)
  cell_4(19,1,1:4) = (/1,2,7,9/)
  cell_4(19,2,1:4) = (/1,3,6,9/)
  cell_4(19,3,1:4) = (/1,3,7,8/)
  cell_4(19,4,1:4) = (/1,4,5,9/)
  cell_4(19,5,1:4) = (/1,4,6,8/)
  cell_4(19,6,1:4) = (/1,5,6,7/)
  cell_4(19,7,1:4) = (/2,3,5,9/)
  cell_4(19,8,1:4) = (/2,3,6,8/)
  cell_4(19,9,1:4) = (/2,4,5,8/)
  cell_4(19,10,1:4) = (/2,4,6,7/)
  cell_4(19,11,1:4) = (/3,4,5,7/)
  cell_4(20,1,1:4) = (/1,2,8,9/)
  cell_4(20,2,1:4) = (/1,3,7,9/)
  cell_4(20,3,1:4) = (/1,4,6,9/)
  cell_4(20,4,1:4) = (/1,4,7,8/)
  cell_4(20,5,1:4) = (/1,5,6,8/)
  cell_4(20,6,1:4) = (/2,3,6,9/)
  cell_4(20,7,1:4) = (/2,3,7,8/)
  cell_4(20,8,1:4) = (/2,4,5,9/)
  cell_4(20,9,1:4) = (/2,4,6,8/)
  cell_4(20,10,1:4) = (/2,5,6,7/)
  cell_4(20,11,1:4) = (/3,4,5,8/)
  cell_4(20,12,1:4) = (/3,4,6,7/)
  cell_4(21,1,1:4) = (/1,3,8,9/)
  cell_4(21,2,1:4) = (/1,4,7,9/)
  cell_4(21,3,1:4) = (/1,5,6,9/)
  cell_4(21,4,1:4) = (/1,5,7,8/)
  cell_4(21,5,1:4) = (/2,3,7,9/)
  cell_4(21,6,1:4) = (/2,4,6,9/)
  cell_4(21,7,1:4) = (/2,4,7,8/)
  cell_4(21,8,1:4) = (/2,5,6,8/)
  cell_4(21,9,1:4) = (/3,4,5,9/)
  cell_4(21,10,1:4) = (/3,4,6,8/)
  cell_4(21,11,1:4) = (/3,5,6,7/)
  cell_4(22,1,1:4) = (/1,4,8,9/)
  cell_4(22,2,1:4) = (/1,5,7,9/)
  cell_4(22,3,1:4) = (/1,6,7,8/)
  cell_4(22,4,1:4) = (/2,3,8,9/)
  cell_4(22,5,1:4) = (/2,4,7,9/)
  cell_4(22,6,1:4) = (/2,5,6,9/)
  cell_4(22,7,1:4) = (/2,5,7,8/)
  cell_4(22,8,1:4) = (/3,4,6,9/)
  cell_4(22,9,1:4) = (/3,4,7,8/)
  cell_4(22,10,1:4) = (/3,5,6,8/)
  cell_4(22,11,1:4) = (/4,5,6,7/)
  cell_4(23,1,1:4) = (/1,5,8,9/)
  cell_4(23,2,1:4) = (/1,6,7,9/)
  cell_4(23,3,1:4) = (/2,4,8,9/)
  cell_4(23,4,1:4) = (/2,5,7,9/)
  cell_4(23,5,1:4) = (/2,6,7,8/)
  cell_4(23,6,1:4) = (/3,4,7,9/)
  cell_4(23,7,1:4) = (/3,5,6,9/)
  cell_4(23,8,1:4) = (/3,5,7,8/)
  cell_4(23,9,1:4) = (/4,5,6,8/)
  cell_4(24,1,1:4) = (/1,6,8,9/)
  cell_4(24,2,1:4) = (/2,5,8,9/)
  cell_4(24,3,1:4) = (/2,6,7,9/)
  cell_4(24,4,1:4) = (/3,4,8,9/)
  cell_4(24,5,1:4) = (/3,5,7,9/)
  cell_4(24,6,1:4) = (/3,6,7,8/)
  cell_4(24,7,1:4) = (/4,5,6,9/)
  cell_4(24,8,1:4) = (/4,5,7,8/)
  cell_4(25,1,1:4) = (/1,7,8,9/)
  cell_4(25,2,1:4) = (/2,6,8,9/)
  cell_4(25,3,1:4) = (/3,5,8,9/)
  cell_4(25,4,1:4) = (/3,6,7,9/)
  cell_4(25,5,1:4) = (/4,5,7,9/)
  cell_4(25,6,1:4) = (/4,6,7,8/)
  cell_4(26,1,1:4) = (/2,7,8,9/)
  cell_4(26,2,1:4) = (/3,6,8,9/)
  cell_4(26,3,1:4) = (/4,5,8,9/)
  cell_4(26,4,1:4) = (/4,6,7,9/)
  cell_4(26,5,1:4) = (/5,6,7,8/)
  cell_4(27,1,1:4) = (/3,7,8,9/)
  cell_4(27,2,1:4) = (/4,6,8,9/)
  cell_4(27,3,1:4) = (/5,6,7,9/)
  cell_4(28,1,1:4) = (/4,7,8,9/)
  cell_4(28,2,1:4) = (/5,6,8,9/)
  cell_4(29,1,1:4) = (/5,7,8,9/)
  cell_4(30,1,1:4) = (/6,7,8,9/)

  cell_5(15,1,1:5) = (/1,2,3,4,5/)
  cell_5(16,1,1:5) = (/1,2,3,4,6/)
  cell_5(17,1,1:5) = (/1,2,3,4,7/)
  cell_5(17,2,1:5) = (/1,2,3,5,6/)
  cell_5(18,1,1:5) = (/1,2,3,4,8/)
  cell_5(18,2,1:5) = (/1,2,3,5,7/)
  cell_5(18,3,1:5) = (/1,2,4,5,6/)
  cell_5(19,1,1:5) = (/1,2,3,4,9/)
  cell_5(19,2,1:5) = (/1,2,3,5,8/)
  cell_5(19,3,1:5) = (/1,2,3,6,7/)
  cell_5(19,4,1:5) = (/1,2,4,5,7/)
  cell_5(19,5,1:5) = (/1,3,4,5,6/)
  cell_5(20,1,1:5) = (/1,2,3,5,9/)
  cell_5(20,2,1:5) = (/1,2,3,6,8/)
  cell_5(20,3,1:5) = (/1,2,4,5,8/)
  cell_5(20,4,1:5) = (/1,2,4,6,7/)
  cell_5(20,5,1:5) = (/1,3,4,5,7/)
  cell_5(20,6,1:5) = (/2,3,4,5,6/)
  cell_5(21,1,1:5) = (/1,2,3,6,9/)
  cell_5(21,2,1:5) = (/1,2,3,7,8/)
  cell_5(21,3,1:5) = (/1,2,4,5,9/)
  cell_5(21,4,1:5) = (/1,2,4,6,8/)
  cell_5(21,5,1:5) = (/1,2,5,6,7/)
  cell_5(21,6,1:5) = (/1,3,4,5,8/)
  cell_5(21,7,1:5) = (/1,3,4,6,7/)
  cell_5(21,8,1:5) = (/2,3,4,5,7/)
  cell_5(22,1,1:5) = (/1,2,3,7,9/)
  cell_5(22,2,1:5) = (/1,2,4,6,9/)
  cell_5(22,3,1:5) = (/1,2,4,7,8/)
  cell_5(22,4,1:5) = (/1,2,5,6,8/)
  cell_5(22,5,1:5) = (/1,3,4,5,9/)
  cell_5(22,6,1:5) = (/1,3,4,6,8/)
  cell_5(22,7,1:5) = (/1,3,5,6,7/)
  cell_5(22,8,1:5) = (/2,3,4,5,8/)
  cell_5(22,9,1:5) = (/2,3,4,6,7/)
  cell_5(23,1,1:5) = (/1,2,3,8,9/)
  cell_5(23,2,1:5) = (/1,2,4,7,9/)
  cell_5(23,3,1:5) = (/1,2,5,6,9/)
  cell_5(23,4,1:5) = (/1,2,5,7,8/)
  cell_5(23,5,1:5) = (/1,3,4,6,9/)
  cell_5(23,6,1:5) = (/1,3,4,7,8/)
  cell_5(23,7,1:5) = (/1,3,5,6,8/)
  cell_5(23,8,1:5) = (/1,4,5,6,7/)
  cell_5(23,9,1:5) = (/2,3,4,5,9/)
  cell_5(23,10,1:5) = (/2,3,4,6,8/)
  cell_5(23,11,1:5) = (/2,3,5,6,7/)
  cell_5(24,1,1:5) = (/1,2,4,8,9/)
  cell_5(24,2,1:5) = (/1,2,5,7,9/)
  cell_5(24,3,1:5) = (/1,2,6,7,8/)
  cell_5(24,4,1:5) = (/1,3,4,7,9/)
  cell_5(24,5,1:5) = (/1,3,5,6,9/)
  cell_5(24,6,1:5) = (/1,3,5,7,8/)
  cell_5(24,7,1:5) = (/1,4,5,6,8/)
  cell_5(24,8,1:5) = (/2,3,4,6,9/)
  cell_5(24,9,1:5) = (/2,3,4,7,8/)
  cell_5(24,10,1:5) = (/2,3,5,6,8/)
  cell_5(24,11,1:5) = (/2,4,5,6,7/)
  cell_5(25,1,1:5) = (/1,2,5,8,9/)
  cell_5(25,2,1:5) = (/1,2,6,7,9/)
  cell_5(25,3,1:5) = (/1,3,4,8,9/)
  cell_5(25,4,1:5) = (/1,3,5,7,9/)
  cell_5(25,5,1:5) = (/1,3,6,7,8/)
  cell_5(25,6,1:5) = (/1,4,5,6,9/)
  cell_5(25,7,1:5) = (/1,4,5,7,8/)
  cell_5(25,8,1:5) = (/2,3,4,7,9/)
  cell_5(25,9,1:5) = (/2,3,5,6,9/)
  cell_5(25,10,1:5) = (/2,3,5,7,8/)
  cell_5(25,11,1:5) = (/2,4,5,6,8/)
  cell_5(25,12,1:5) = (/3,4,5,6,7/)
  cell_5(26,1,1:5) = (/1,2,6,8,9/)
  cell_5(26,2,1:5) = (/1,3,5,8,9/)
  cell_5(26,3,1:5) = (/1,3,6,7,9/)
  cell_5(26,4,1:5) = (/1,4,5,7,9/)
  cell_5(26,5,1:5) = (/1,4,6,7,8/)
  cell_5(26,6,1:5) = (/2,3,4,8,9/)
  cell_5(26,7,1:5) = (/2,3,5,7,9/)
  cell_5(26,8,1:5) = (/2,3,6,7,8/)
  cell_5(26,9,1:5) = (/2,4,5,6,9/)
  cell_5(26,10,1:5) = (/2,4,5,7,8/)
  cell_5(26,11,1:5) = (/3,4,5,6,8/)
  cell_5(27,1,1:5) = (/1,2,7,8,9/)
  cell_5(27,2,1:5) = (/1,3,6,8,9/)
  cell_5(27,3,1:5) = (/1,4,5,8,9/)
  cell_5(27,4,1:5) = (/1,4,6,7,9/)
  cell_5(27,5,1:5) = (/1,5,6,7,8/)
  cell_5(27,6,1:5) = (/2,3,5,8,9/)
  cell_5(27,7,1:5) = (/2,3,6,7,9/)
  cell_5(27,8,1:5) = (/2,4,5,7,9/)
  cell_5(27,9,1:5) = (/2,4,6,7,8/)
  cell_5(27,10,1:5) = (/3,4,5,6,9/)
  cell_5(27,11,1:5) = (/3,4,5,7,8/)
  cell_5(28,1,1:5) = (/1,3,7,8,9/)
  cell_5(28,2,1:5) = (/1,4,6,8,9/)
  cell_5(28,3,1:5) = (/1,5,6,7,9/)
  cell_5(28,4,1:5) = (/2,3,6,8,9/)
  cell_5(28,5,1:5) = (/2,4,5,8,9/)
  cell_5(28,6,1:5) = (/2,4,6,7,9/)
  cell_5(28,7,1:5) = (/2,5,6,7,8/)
  cell_5(28,8,1:5) = (/3,4,5,7,9/)
  cell_5(28,9,1:5) = (/3,4,6,7,8/)
  cell_5(29,1,1:5) = (/1,4,7,8,9/)
  cell_5(29,2,1:5) = (/1,5,6,8,9/)
  cell_5(29,3,1:5) = (/2,3,7,8,9/)
  cell_5(29,4,1:5) = (/2,4,6,8,9/)
  cell_5(29,5,1:5) = (/2,5,6,7,9/)
  cell_5(29,6,1:5) = (/3,4,5,8,9/)
  cell_5(29,7,1:5) = (/3,4,6,7,9/)
  cell_5(29,8,1:5) = (/3,5,6,7,8/)
  cell_5(30,1,1:5) = (/1,5,7,8,9/)
  cell_5(30,2,1:5) = (/2,4,7,8,9/)
  cell_5(30,3,1:5) = (/2,5,6,8,9/)
  cell_5(30,4,1:5) = (/3,4,6,8,9/)
  cell_5(30,5,1:5) = (/3,5,6,7,9/)
  cell_5(30,6,1:5) = (/4,5,6,7,8/)
  cell_5(31,1,1:5) = (/1,6,7,8,9/)
  cell_5(31,2,1:5) = (/2,5,7,8,9/)
  cell_5(31,3,1:5) = (/3,4,7,8,9/)
  cell_5(31,4,1:5) = (/3,5,6,8,9/)
  cell_5(31,5,1:5) = (/4,5,6,7,9/)
  cell_5(32,1,1:5) = (/2,6,7,8,9/)
  cell_5(32,2,1:5) = (/3,5,7,8,9/)
  cell_5(32,3,1:5) = (/4,5,6,8,9/)
  cell_5(33,1,1:5) = (/3,6,7,8,9/)
  cell_5(33,2,1:5) = (/4,5,7,8,9/)
  cell_5(34,1,1:5) = (/4,6,7,8,9/)
  cell_5(35,1,1:5) = (/5,6,7,8,9/)

  cell_6(21,1,1:6) = (/1,2,3,4,5,6/)
  cell_6(22,1,1:6) = (/1,2,3,4,5,7/)
  cell_6(23,1,1:6) = (/1,2,3,4,5,8/)
  cell_6(23,2,1:6) = (/1,2,3,4,6,7/)
  cell_6(24,1,1:6) = (/1,2,3,4,5,9/)
  cell_6(24,2,1:6) = (/1,2,3,4,6,8/)
  cell_6(24,3,1:6) = (/1,2,3,5,6,7/)
  cell_6(25,1,1:6) = (/1,2,3,4,6,9/)
  cell_6(25,2,1:6) = (/1,2,3,4,7,8/)
  cell_6(25,3,1:6) = (/1,2,3,5,6,8/)
  cell_6(25,4,1:6) = (/1,2,4,5,6,7/)
  cell_6(26,1,1:6) = (/1,2,3,4,7,9/)
  cell_6(26,2,1:6) = (/1,2,3,5,6,9/)
  cell_6(26,3,1:6) = (/1,2,3,5,7,8/)
  cell_6(26,4,1:6) = (/1,2,4,5,6,8/)
  cell_6(26,5,1:6) = (/1,3,4,5,6,7/)
  cell_6(27,1,1:6) = (/1,2,3,4,8,9/)
  cell_6(27,2,1:6) = (/1,2,3,5,7,9/)
  cell_6(27,3,1:6) = (/1,2,3,6,7,8/)
  cell_6(27,4,1:6) = (/1,2,4,5,6,9/)
  cell_6(27,5,1:6) = (/1,2,4,5,7,8/)
  cell_6(27,6,1:6) = (/1,3,4,5,6,8/)
  cell_6(27,7,1:6) = (/2,3,4,5,6,7/)
  cell_6(28,1,1:6) = (/1,2,3,5,8,9/)
  cell_6(28,2,1:6) = (/1,2,3,6,7,9/)
  cell_6(28,3,1:6) = (/1,2,4,5,7,9/)
  cell_6(28,4,1:6) = (/1,2,4,6,7,8/)
  cell_6(28,5,1:6) = (/1,3,4,5,6,9/)
  cell_6(28,6,1:6) = (/1,3,4,5,7,8/)
  cell_6(28,7,1:6) = (/2,3,4,5,6,8/)
  cell_6(29,1,1:6) = (/1,2,3,6,8,9/)
  cell_6(29,2,1:6) = (/1,2,4,5,8,9/)
  cell_6(29,3,1:6) = (/1,2,4,6,7,9/)
  cell_6(29,4,1:6) = (/1,2,5,6,7,8/)
  cell_6(29,5,1:6) = (/1,3,4,5,7,9/)
  cell_6(29,6,1:6) = (/1,3,4,6,7,8/)
  cell_6(29,7,1:6) = (/2,3,4,5,6,9/)
  cell_6(29,8,1:6) = (/2,3,4,5,7,8/)
  cell_6(30,1,1:6) = (/1,2,3,7,8,9/)
  cell_6(30,2,1:6) = (/1,2,4,6,8,9/)
  cell_6(30,3,1:6) = (/1,2,5,6,7,9/)
  cell_6(30,4,1:6) = (/1,3,4,5,8,9/)
  cell_6(30,5,1:6) = (/1,3,4,6,7,9/)
  cell_6(30,6,1:6) = (/1,3,5,6,7,8/)
  cell_6(30,7,1:6) = (/2,3,4,5,7,9/)
  cell_6(30,8,1:6) = (/2,3,4,6,7,8/)
  cell_6(31,1,1:6) = (/1,2,4,7,8,9/)
  cell_6(31,2,1:6) = (/1,2,5,6,8,9/)
  cell_6(31,3,1:6) = (/1,3,4,6,8,9/)
  cell_6(31,4,1:6) = (/1,3,5,6,7,9/)
  cell_6(31,5,1:6) = (/1,4,5,6,7,8/)
  cell_6(31,6,1:6) = (/2,3,4,5,8,9/)
  cell_6(31,7,1:6) = (/2,3,4,6,7,9/)
  cell_6(31,8,1:6) = (/2,3,5,6,7,8/)
  cell_6(32,1,1:6) = (/1,2,5,7,8,9/)
  cell_6(32,2,1:6) = (/1,3,4,7,8,9/)
  cell_6(32,3,1:6) = (/1,3,5,6,8,9/)
  cell_6(32,4,1:6) = (/1,4,5,6,7,9/)
  cell_6(32,5,1:6) = (/2,3,4,6,8,9/)
  cell_6(32,6,1:6) = (/2,3,5,6,7,9/)
  cell_6(32,7,1:6) = (/2,4,5,6,7,8/)
  cell_6(33,1,1:6) = (/1,2,6,7,8,9/)
  cell_6(33,2,1:6) = (/1,3,5,7,8,9/)
  cell_6(33,3,1:6) = (/1,4,5,6,8,9/)
  cell_6(33,4,1:6) = (/2,3,4,7,8,9/)
  cell_6(33,5,1:6) = (/2,3,5,6,8,9/)
  cell_6(33,6,1:6) = (/2,4,5,6,7,9/)
  cell_6(33,7,1:6) = (/3,4,5,6,7,8/)
  cell_6(34,1,1:6) = (/1,3,6,7,8,9/)
  cell_6(34,2,1:6) = (/1,4,5,7,8,9/)
  cell_6(34,3,1:6) = (/2,3,5,7,8,9/)
  cell_6(34,4,1:6) = (/2,4,5,6,8,9/)
  cell_6(34,5,1:6) = (/3,4,5,6,7,9/)
  cell_6(35,1,1:6) = (/1,4,6,7,8,9/)
  cell_6(35,2,1:6) = (/2,3,6,7,8,9/)
  cell_6(35,3,1:6) = (/2,4,5,7,8,9/)
  cell_6(35,4,1:6) = (/3,4,5,6,8,9/)
  cell_6(36,1,1:6) = (/1,5,6,7,8,9/)
  cell_6(36,2,1:6) = (/2,4,6,7,8,9/)
  cell_6(36,3,1:6) = (/3,4,5,7,8,9/)
  cell_6(37,1,1:6) = (/2,5,6,7,8,9/)
  cell_6(37,2,1:6) = (/3,4,6,7,8,9/)
  cell_6(38,1,1:6) = (/3,5,6,7,8,9/)
  cell_6(39,1,1:6) = (/4,5,6,7,8,9/)

  cell_7(28,1,1:7) = (/1,2,3,4,5,6,7/)
  cell_7(29,1,1:7) = (/1,2,3,4,5,6,8/)
  cell_7(30,1,1:7) = (/1,2,3,4,5,6,9/)
  cell_7(30,2,1:7) = (/1,2,3,4,5,7,8/)
  cell_7(31,1,1:7) = (/1,2,3,4,5,7,9/)
  cell_7(31,2,1:7) = (/1,2,3,4,6,7,8/)
  cell_7(32,1,1:7) = (/1,2,3,4,5,8,9/)
  cell_7(32,2,1:7) = (/1,2,3,4,6,7,9/)
  cell_7(32,3,1:7) = (/1,2,3,5,6,7,8/)
  cell_7(33,1,1:7) = (/1,2,3,4,6,8,9/)
  cell_7(33,2,1:7) = (/1,2,3,5,6,7,9/)
  cell_7(33,3,1:7) = (/1,2,4,5,6,7,8/)
  cell_7(34,1,1:7) = (/1,2,3,4,7,8,9/)
  cell_7(34,2,1:7) = (/1,2,3,5,6,8,9/)
  cell_7(34,3,1:7) = (/1,2,4,5,6,7,9/)
  cell_7(34,4,1:7) = (/1,3,4,5,6,7,8/)
  cell_7(35,1,1:7) = (/1,2,3,5,7,8,9/)
  cell_7(35,2,1:7) = (/1,2,4,5,6,8,9/)
  cell_7(35,3,1:7) = (/1,3,4,5,6,7,9/)
  cell_7(35,4,1:7) = (/2,3,4,5,6,7,8/)
  cell_7(36,1,1:7) = (/1,2,3,6,7,8,9/)
  cell_7(36,2,1:7) = (/1,2,4,5,7,8,9/)
  cell_7(36,3,1:7) = (/1,3,4,5,6,8,9/)
  cell_7(36,4,1:7) = (/2,3,4,5,6,7,9/)
  cell_7(37,1,1:7) = (/1,2,4,6,7,8,9/)
  cell_7(37,2,1:7) = (/1,3,4,5,7,8,9/)
  cell_7(37,3,1:7) = (/2,3,4,5,6,8,9/)
  cell_7(38,1,1:7) = (/1,2,5,6,7,8,9/)
  cell_7(38,2,1:7) = (/1,3,4,6,7,8,9/)
  cell_7(38,3,1:7) = (/2,3,4,5,7,8,9/)
  cell_7(39,1,1:7) = (/1,3,5,6,7,8,9/)
  cell_7(39,2,1:7) = (/2,3,4,6,7,8,9/)
  cell_7(40,1,1:7) = (/1,4,5,6,7,8,9/)
  cell_7(40,2,1:7) = (/2,3,5,6,7,8,9/)
  cell_7(41,1,1:7) = (/2,4,5,6,7,8,9/)
  cell_7(42,1,1:7) = (/3,4,5,6,7,8,9/)

  cell_8(36,1,1:8) = (/1,2,3,4,5,6,7,8/)
  cell_8(37,1,1:8) = (/1,2,3,4,5,6,7,9/)
  cell_8(38,1,1:8) = (/1,2,3,4,5,6,8,9/)
  cell_8(39,1,1:8) = (/1,2,3,4,5,7,8,9/)
  cell_8(40,1,1:8) = (/1,2,3,4,6,7,8,9/)
  cell_8(41,1,1:8) = (/1,2,3,5,6,7,8,9/)
  cell_8(42,1,1:8) = (/1,2,4,5,6,7,8,9/)
  cell_8(43,1,1:8) = (/1,3,4,5,6,7,8,9/)
  cell_8(44,1,1:8) = (/2,3,4,5,6,7,8,9/)

  cell_9(45,1,1:9) = (/1,2,3,4,5,6,7,8,9/)


! Restricted sums
  Rcell_2(1) = 3
  Rcell_2(2) = 4
  Rcell_2(3) = 16
  Rcell_2(4) = 17

  Rcell_3(1) = 6
  Rcell_3(2) = 7
  Rcell_3(3) = 23
  Rcell_3(4) = 24

  Rcell_4(1) = 10
  Rcell_4(2) = 11
  Rcell_4(3) = 29
  Rcell_4(4) = 30

  Rcell_5(1) = 15
  Rcell_5(2) = 16
  Rcell_5(3) = 34
  Rcell_5(4) = 35

  Rcell_6(1) = 21
  Rcell_6(2) = 22
  Rcell_6(3) = 38
  Rcell_6(4) = 39

  Rcell_7(1) = 28
  Rcell_7(2) = 29
  Rcell_7(3) = 41
  Rcell_7(4) = 42

  Rcell_8(1) = 36
  Rcell_8(2) = 37
  Rcell_8(3) = 38
  Rcell_8(4) = 39
  Rcell_8(5) = 40
  Rcell_8(6) = 41
  Rcell_8(7) = 42
  Rcell_8(8) = 43
  Rcell_8(9) = 44

  Rcell_9(1) = 45


END SUBROUTINE Cage_table

!-------------------------------------------------------------------------------------
SUBROUTINE Initialize_Puzzle

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j,no_of_cells
  INTEGER :: row_number,column_number
  INTEGER :: icage,row1,row2,col1,col2,ncells
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: cage3h,cage4h,cage3h_real,cage4h_real

  sudoku = 0
  sudoku_initial = 0
  sudoku_logic = 0
  sudoku_logic_values = 0

  cage_to_cells = 0
  cage_sum = 0
  cell_to_cage = 0
  cell_to_cage_sum = 0
  check_cage = 0
  check_cage_sum = 0
  sudoku_cage = 0

  OPEN(UNIT=10,FILE='kakuro-initial-rect',STATUS='OLD',ACCESS='SEQUENTIAL',ACTION='READ')

  READ(10,*) total_no_of_cages
!  write(*,*) 'total_no_of_cages is :',total_no_of_cages
  DO i=1,total_no_of_cages
!	write(*,*) i
        READ(10,*)
        READ(10,*) cage_sum(i),no_of_cells
!	write(*,*) cage_sum(i),no_of_cells
        cage_no_of_cells(i) = no_of_cells
        DO j=1,no_of_cells
                READ(10,*) cage_to_cells(i,j,1),cage_to_cells(i,j,2)
                row_number = cage_to_cells(i,j,1)
                column_number = cage_to_cells(i,j,2)
                IF (cell_to_cage(row_number,column_number,1) .eq. 0) THEN
                        cell_to_cage(row_number,column_number,1) = i
                ELSE
                        cell_to_cage(row_number,column_number,2) = i
                END IF
                IF (cell_to_cage_sum(row_number,column_number,1) .eq. 0) THEN
                        cell_to_cage_sum(row_number,column_number,1) = cage_sum(i)
                ELSE
                        cell_to_cage_sum(row_number,column_number,2) = cage_sum(i)
                END IF
        END DO
  END DO

  no_of_sudoku_cells = 0
  sudoku = 0
  DO i=1,max_grid_size1
        DO j=1,max_grid_size2
                IF (cell_to_cage(i,j,1) .eq. 0) THEN
                        sudoku_initial(i,j) = -1
			sudoku(i,j) = -1
                ELSE
                        no_of_sudoku_cells = no_of_sudoku_cells+1
                END IF
        END DO
  END DO

  nhorizontal_cages = 0
  nvertical_cages = 0
  DO icage=1,total_no_of_cages
    ncells = cage_no_of_cells(icage)
    row1 = cage_to_cells(icage,1,1)
    col1 = cage_to_cells(icage,1,2)
    row2 = cage_to_cells(icage,2,1)
    col2 = cage_to_cells(icage,2,2)   
    IF (row2 .eq. row1+1) THEN
	nvertical_cages = nvertical_cages + 1
	vertical_cages(nvertical_cages) = icage
    ELSE IF (col2 .eq. col1+1) THEN
	nhorizontal_cages = nhorizontal_cages + 1
        horizontal_cages(nhorizontal_cages) = icage
    END IF
  END DO

!  write(*,*) 'nhorizontal_cages is :',nhorizontal_cages
!  write(*,*) 'nvertical_cages is :',nvertical_cages

  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells = solved_cells + 1
     END IF
   END DO
 END DO

  nhorizontal_2cages = 0
  nhorizontal_3cages = 0
  nhorizontal_4cages = 0
  nvertical_2cages = 0
  nvertical_3cages = 0
  nvertical_4cages = 0

  IF (nhorizontal_cages .ge. 2) THEN
    CALL Find_2permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 3) THEN
    CALL Find_3permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 4) THEN
    CALL Find_4permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 5) THEN
    CALL Find_5permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 6) THEN
    CALL Find_6permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 7) THEN
    CALL Find_7permutation_horizontalcage
  END IF
  IF (nhorizontal_cages .ge. 8) THEN
    CALL Find_8permutation_horizontalcage
  END IF

  IF (nvertical_cages .ge. 2) THEN
     CALL Find_2permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 3) THEN
    CALL Find_3permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 4) THEN
    CALL Find_4permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 5) THEN
    CALL Find_5permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 6) THEN
    CALL Find_6permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 7) THEN
    CALL Find_7permutation_verticalcage
  END IF
  IF (nvertical_cages .ge. 8) THEN
    CALL Find_8permutation_verticalcage
  END IF

GO TO 113

  write(*,*) 'nhorizontal_2cages is :',nhorizontal_2cages
  DO icage=1,nhorizontal_2cages
     cage1h=horizontal_2cages(icage,1)
     cage2h=horizontal_2cages(icage,2)
     cage1h_real=horizontal_cages(cage1h)
     cage2h_real=horizontal_cages(cage2h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*)
  END DO
  write(*,*) 'nhorizontal_3cages is :',nhorizontal_3cages
  DO icage=1,nhorizontal_3cages
     cage1h=horizontal_3cages(icage,1)
     cage2h=horizontal_3cages(icage,2)
     cage3h=horizontal_3cages(icage,3)
     cage1h_real=horizontal_cages(cage1h)
     cage2h_real=horizontal_cages(cage2h)
     cage3h_real=horizontal_cages(cage3h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*) 'cage_sum(cage3h_real) is :',cage_sum(cage3h_real)
     write(*,*)
  END DO
  write(*,*) 'nhorizontal_4cages is :',nhorizontal_4cages
  DO icage=1,nhorizontal_4cages
     cage1h=horizontal_4cages(icage,1)
     cage2h=horizontal_4cages(icage,2)
     cage3h=horizontal_4cages(icage,3)
     cage4h=horizontal_4cages(icage,4)
     cage1h_real=horizontal_cages(cage1h)
     cage2h_real=horizontal_cages(cage2h)
     cage3h_real=horizontal_cages(cage3h)
     cage4h_real=horizontal_cages(cage4h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*) 'cage_sum(cage3h_real) is :',cage_sum(cage3h_real)
     write(*,*) 'cage_sum(cage4h_real) is :',cage_sum(cage4h_real)
     write(*,*)
  END DO

  write(*,*) 'nvertical_2cages is :',nvertical_2cages
  DO icage=1,nvertical_2cages
     cage1h=vertical_2cages(icage,1)
     cage2h=vertical_2cages(icage,2)
     cage1h_real=vertical_cages(cage1h)
     cage2h_real=vertical_cages(cage2h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*)
  END DO

  write(*,*) 'nvertical_3cages is :',nvertical_3cages
  DO icage=1,nvertical_3cages
     cage1h=vertical_3cages(icage,1)
     cage2h=vertical_3cages(icage,2)
     cage3h=vertical_3cages(icage,3)
     cage1h_real=vertical_cages(cage1h)
     cage2h_real=vertical_cages(cage2h)
     cage3h_real=vertical_cages(cage3h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*) 'cage_sum(cage3h_real) is :',cage_sum(cage3h_real)
     write(*,*)
  END DO

  write(*,*) 'nvertical_4cages is :',nvertical_4cages
  DO icage=1,nvertical_4cages
     cage1h=vertical_4cages(icage,1)
     cage2h=vertical_4cages(icage,2)
     cage3h=vertical_4cages(icage,3)
     cage4h=vertical_4cages(icage,4)
     cage1h_real=vertical_cages(cage1h)
     cage2h_real=vertical_cages(cage2h)
     cage3h_real=vertical_cages(cage3h)
     cage4h_real=vertical_cages(cage4h)
     write(*,*) 'cage_sum(cage1h_real) is :',cage_sum(cage1h_real)
     write(*,*) 'cage_sum(cage2h_real) is :',cage_sum(cage2h_real)
     write(*,*) 'cage_sum(cage3h_real) is :',cage_sum(cage3h_real)
     write(*,*) 'cage_sum(cage4h_real) is :',cage_sum(cage4h_real)
     write(*,*)
  END DO

113 CONTINUE

END SUBROUTINE Initialize_Puzzle

!-------------------------------------------------------------------------------------

PROGRAM solve_kakuro

  USE global_variables
  IMPLICIT NONE

  INTEGER :: pass,i,j,icage
  INTEGER :: nfilled_cells,nfilled_cells_final
  INTEGER :: isSolution

  INTEGER :: check
  INTEGER :: solved_cells_initial,solved_cells_final
  INTEGER :: i_bowman,i_nishio
  INTEGER :: icell,jcell,check_number
  INTEGER :: check_adrianne
  INTEGER :: snapshots
  INTEGER :: count_snapshots
  INTEGER :: ii_cell,jj_cell,kk_cell,ll_cell
  INTEGER :: flag


  CALL Initialize_Puzzle
  CALL Cage_table
  CALL find_restricted_blocks

  CALL Eliminate_impossible

                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

 nfilled_cells_final = 0
 DO i=1,max_grid_size1
  DO j=1,max_grid_size2
    IF (sudoku(i,j) .gt. 0) THEN
       nfilled_cells_final = nfilled_cells_final+1
    END IF
  END DO
 END DO

 IF (nfilled_cells_final .eq. no_of_sudoku_cells) THEN
	CALL show_solution
	GO TO 100
	STOP
 END IF

  snapshots = 0
333 CONTINUE

   pass = 0

 DO WHILE (pass .lt. 5) 

!GO TO 555

  CALL update_sudoku_logic_values
  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN
	CALL show_solution
	GO TO 100
	STOP
   END IF


!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

  CALL update_cage_cells

  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN  
	CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

  CALL Eliminate_hidden_impossible

  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN  
	CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------


  DO icage=1,total_no_of_cages
    CALL Hidden_sets(icage)
  END DO

  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        ! Just for debugging purposes
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN  
	CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

 DO icage=1,total_no_of_cages
   CALL Binding_squares(icage)
 END DO

  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN  
	CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

  CALL Eliminate_hidden_impossible

  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final
  solved_cells_initial=solved_cells
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN
        CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

  CALL Unique_rectangle_type1
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  CALL Unique_rectangle_type2
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  CALL Unique_rectangle_type3
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  CALL Unique_rectangle_type3and2
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  CALL Unique_rectangle_type4
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF
!------------------------------------------------

!  CALL Crisscross_arithmetic

  solved_cells_initial=solved_cells

                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

  solved_cells_final=0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
        solved_cells_final = solved_cells_final + 1
     END IF
   END DO
 END DO
   IF (solved_cells_final .gt. solved_cells_initial) THEN
        check = 0
        CALL update_snapshots(snapshots)
  END IF
  solved_cells = solved_cells_final

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN  
	CALL show_solution
	GO TO 100
        STOP
   END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

   CALL comprehensive_forcing_chains
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF
!-------------------------------------------------------

  pass = pass +1
END DO

!--------------------------------------------------------
! Just for debugging purposes
  write(*,*) 'CALLING SHOW SOLUTION'
  CALL show_solution
  write(*,*) 'END SHOW SOLUTION'
  DO icell=1,max_grid_size1
    DO jcell=1,max_grid_size2
     IF (sudoku(icell,jcell) .eq. 0) THEN
	write(*,*) 'row and col are :',icell,jcell
        write(*,"(9I3)") sudoku_logic(icell,jcell,:)
     END IF
    END DO
  END DO
  STOP

!-------------------------------------------------------

  CALL Adrianne_thread_state
  snapshots_initial(nAdrianneStates) = snapshots
  Adrianne_state_sudoku_logic_number(nAdrianneStates) = 1
  icell = Adrianne_state_row(nAdrianneStates)
  jcell = Adrianne_state_col(nAdrianneStates)
  IF (sudoku_logic_values(icell,jcell) .eq. 0) THEN
        GO TO 444
  END IF
  check_number = sudoku_logic(icell,jcell,1)
  sudoku(icell,jcell) = check_number
  sudoku_logic_values(icell,jcell) = 1
  sudoku_logic(icell,jcell,1) = check_number
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
	IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
		GO TO 222
	END IF
      END IF
    END DO
  END DO
!-----------------------------------------------
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------
111  CONTINUE
  check_adrianne = 0
  CALL check_Contradiction_Adrianne(sudoku,sudoku_logic_values,sudoku_logic,check_adrianne,max_grid_size1,max_grid_size2)

  IF (check_adrianne .eq. 1) THEN
222  CONTINUE

        snapshots = snapshots_initial(nAdrianneStates)
        Adrianne_state_sudoku_logic_number(nAdrianneStates) = &
          Adrianne_state_sudoku_logic_number(nAdrianneStates)+1
          icell = Adrianne_state_row(nAdrianneStates)
          jcell = Adrianne_state_col(nAdrianneStates)

        IF (Adrianne_state_sudoku_logic_number(nAdrianneStates) .le. &
             sudoku_Adrianne_logic_numbers(nAdrianneStates,icell,jcell)) THEN
                check_number=sudoku_Adrianne_logic(nAdrianneStates,icell,jcell, &
                Adrianne_state_sudoku_logic_number(nAdrianneStates))
                sudoku = sudoku_Adrianne(nAdrianneStates,:,:)
                sudoku_logic = &
                        sudoku_Adrianne_logic(nAdrianneStates,:,:,:)
                sudoku_logic_values = &
                        sudoku_Adrianne_logic_numbers(nAdrianneStates,:,:)
                sudoku(icell,jcell) = check_number
                sudoku_logic_values(icell,jcell) = 1
                sudoku_logic(icell,jcell,1) = check_number
                flag = 0
                CALL Forced_moves(flag)
                IF (flag .eq. 1) THEN
                   GO TO 222
                END IF

!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

		flag = 0
                CALL Forced_moves(flag)
		IF (flag .eq. 1) THEN
		   GO TO 222
		END IF
!-----------------------------------------------
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      IF (sudoku(ii_cell,jj_cell) .eq. 0) THEN
        IF (sudoku_logic_values(ii_cell,jj_cell) .le. 0) THEN
                GO TO 222
        END IF
      END IF
    END DO
  END DO
!-----------------------------------------------

                GO TO 111
        ELSE
444  CONTINUE

                Adrianne_state_sudoku_logic_number(nAdrianneStates) = 0
                nAdrianneStates = nAdrianneStates-1
		IF (nAdrianneStates .lt. 0)  THEN
			write(*,*)'nAdrianneStates lt 0'
! Just for debugging purposes
  CALL show_solution
  DO ii_cell=1,max_grid_size1
    DO jj_cell=1,max_grid_size2
      write(*,*) sudoku_logic(ii_cell,jj_cell,:)
    END DO
  END DO
			STOP
		END IF
                sudoku = sudoku_Adrianne(nAdrianneStates,:,:)
                sudoku_logic = &
                        sudoku_Adrianne_logic(nAdrianneStates,:,:,:)
                sudoku_logic_values = &
                        sudoku_Adrianne_logic_numbers(nAdrianneStates,:,:)
                GO TO 222
        END IF
  ELSE
        GO TO 333
  END IF

!------------------------------------------------------------------

100 CONTINUE

   isSolution = 0
   CALL check_solution(isSolution)
   IF (isSolution .eq. 1) THEN
    duplicate_snapshots = 0
    CALL check_duplicate_snapshots(snapshots)
    count_snapshots=0
    DO j=1,max_grid_size1*max_grid_size2
        IF (duplicate_snapshots(j) .eq. 2) THEN
                count_snapshots = count_snapshots+1
                write(*,*) 'STEP :',j
                DO i=1,max_grid_size1
			write(*,*) sudoku_snapshot_initial(j,i,:)
                        write(*,*)
                END DO
        END IF
  END DO
    write(*,*) 'no of snapshots is :', count_snapshots
  END IF

  STOP

  
END PROGRAM solve_kakuro

!-----------------------------------------------------------------------------------

SUBROUTINE find_restricted_blocks

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,jcell,ncells
  INTEGER :: j,temp
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cage_cells
  INTEGER :: row1,column1,cage_sum_local
  INTEGER :: ivalue,icell,count,kcell
  INTEGER :: cage_number_local,lower_bound,upper_bound
  
  restricted_blocks = 0
  restricted_blocks_order = 0
  sudoku_logic_1 = 0
  sudoku_logic_2 = 0
  sudoku_logic_values_1 = 0
  sudoku_logic_values_2 = 0

iloop: DO i=1,total_no_of_cages
   ncells = cage_no_of_cells(i)

   IF (ncells .eq. 2) THEN
     DO jcell=1,4
       IF (Rcell_2(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO	
   ELSE IF (ncells .eq. 3) THEN
     DO jcell=1,4
       IF (Rcell_3(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO     
   ELSE IF (ncells .eq. 4) THEN
     DO jcell=1,4
       IF (Rcell_4(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   ELSE IF (ncells .eq. 5) THEN
     DO jcell=1,4
       IF (Rcell_5(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   ELSE IF (ncells .eq. 6) THEN
     DO jcell=1,4
       IF (Rcell_6(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   ELSE IF (ncells .eq. 7) THEN
     DO jcell=1,4
       IF (Rcell_7(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   ELSE IF (ncells .eq. 8) THEN
     DO jcell=1,9
       IF (Rcell_8(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   ELSE IF (ncells .eq. 9) THEN
     DO jcell=1,1
       IF (Rcell_9(jcell) .eq. cage_sum(i)) THEN
	 restricted_blocks(i) = 1
         EXIT 
       END IF
     END DO
   END IF

 END DO iloop

  count_restricted = 0
  DO i=1,total_no_of_cages
    IF (restricted_blocks(i) .eq. 1) THEN
       count_restricted = count_restricted + 1
       restricted_blocks_order(count_restricted) = i
    END IF
  END DO

  DO i=1,count_restricted-1
    DO j=i+1,count_restricted
     IF(cage_no_of_cells(restricted_blocks_order(i)) .GT. &
         cage_no_of_cells(restricted_blocks_order(j))) THEN
	   temp = restricted_blocks_order(i)
           restricted_blocks_order(i) = restricted_blocks_order(j)
           restricted_blocks_order(j) = temp
     END IF
    END DO
  END DO 

  DO i=1,count_restricted
     ncells = cage_no_of_cells(restricted_blocks_order(i))
     cage_sum_local = cage_sum(restricted_blocks_order(i))
     cage_number_local = restricted_blocks_order(i)

     cage_cells = ncells

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

      DO icell=1,ncells
        row1 = cage_to_cells(restricted_blocks_order(i),icell,1)
	column1 = cage_to_cells(restricted_blocks_order(i),icell,2)
	IF (cell_to_cage(row1,column1,1) .eq. restricted_blocks_order(i)) THEN
	  sudoku_logic_values_1(row1,column1) = ncells
          DO ivalue=1,ncells
            sudoku_logic_1(row1,column1,ivalue) = pa(cage_sum_local-lower_state+1,1,ivalue)
	  END DO
	ELSE
	  sudoku_logic_values_2(row1,column1) = ncells
          DO ivalue=1,ncells
            sudoku_logic_2(row1,column1,ivalue) = pa(cage_sum_local-lower_state+1,1,ivalue)
	  END DO
	END IF
      END DO

 END DO

 DO i=1,total_no_of_cages
     IF (restricted_blocks(i) .eq. 1) CYCLE

     ncells = cage_no_of_cells(i)
     cage_sum_local = cage_sum(i)
     cage_number_local = i

     cage_cells = ncells

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

      DO icell=1,ncells
        row1 = cage_to_cells(i,icell,1)
        column1 = cage_to_cells(i,icell,2)
	lower_bound = pa(cage_sum_local-lower_state+1,1,1)
        upper_bound = pa(cage_sum_local-lower_state+1,1,ncells)
        IF (cell_to_cage(row1,column1,1) .eq. i) THEN
          sudoku_logic_values_1(row1,column1) = upper_bound-lower_bound+1
	  count = 0
          DO ivalue=lower_bound,upper_bound
	    count = count + 1
            sudoku_logic_1(row1,column1,count) = ivalue
          END DO
        ELSE
          sudoku_logic_values_2(row1,column1) = upper_bound-lower_bound+1
	  count = 0
          DO ivalue=lower_bound,upper_bound
	    count = count + 1
            sudoku_logic_2(row1,column1,count) = ivalue
          END DO
        END IF
      END DO

  END DO

 
END SUBROUTINE find_restricted_blocks

!---------------------------------------------------------------------------------

SUBROUTINE Eliminate_impossible

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j
  INTEGER :: check_double,check_number
  INTEGER :: ii,jj,inumber,jnumber
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_logic_values_new
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2,9) :: sudoku_logic_new

  sudoku_logic_values_new = 0
  sudoku_logic_new = 0

  DO i=1,max_grid_size1
    DO j=1,max_grid_size2
      check_double = 0
      check_number = 0
	IF (sudoku_logic_values_1(i,j) .le. 0) THEN
	  sudoku_logic_values_new(i,j) = sudoku_logic_values_2(i,j)
	  sudoku_logic_new(i,j,:) = sudoku_logic_2(i,j,:)
	  CYCLE 
	END IF 
        IF (sudoku_logic_values_2(i,j) .le. 0) THEN
          sudoku_logic_values_new(i,j) = sudoku_logic_values_1(i,j)
          sudoku_logic_new(i,j,:) = sudoku_logic_1(i,j,:)
          CYCLE 
        END IF

      DO ii=1,sudoku_logic_values_1(i,j)
         inumber = sudoku_logic_1(i,j,ii)
         DO jj=1,sudoku_logic_values_2(i,j)
            jnumber = sudoku_logic_2(i,j,jj)
            IF (inumber .eq. jnumber) THEN
	       check_double = check_double + 1
	       check_number = jnumber
	       sudoku_logic_values_new(i,j) = sudoku_logic_values_new(i,j) + 1
	       sudoku_logic_new(i,j,sudoku_logic_values_new(i,j)) = jnumber
            END IF
         END DO
      END DO



      IF (check_double .eq. 1) THEN
         sudoku(i,j) = check_number
	 sudoku_logic_values_new(i,j) = 0
      END IF
    END DO
  END DO

 sudoku_logic = sudoku_logic_new
 sudoku_logic_values = sudoku_logic_values_new


END SUBROUTINE Eliminate_impossible

!-------------------------------------------------------------------------------

SUBROUTINE update_sudoku_logic_values

  USE global_variables
  IMPLICIT NONE
 
  INTEGER :: i,j,icage,icell,ncells
  INTEGER :: cage_number_local,cage_sum_local
  INTEGER :: row1,col1,check_numbers
  INTEGER :: temp_number
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: inumber,check_number_ij


  DO i=1,max_grid_size1
    DO j=1,max_grid_size2
      IF (sudoku(i,j) .LE. 0) CYCLE
      DO icage=1,2
         cage_number_local = cell_to_cage(i,j,icage)
	 cage_sum_local = cell_to_cage_sum(i,j,icage)

	 ncells = cage_no_of_cells(cage_number_local)
         DO icell=1,ncells
           row1 = cage_to_cells(cage_number_local,icell,1)
           col1 = cage_to_cells(cage_number_local,icell,2)

           IF ((row1 .eq. i) .AND. (col1 .eq. j)) CYCLE
	   IF (sudoku(row1,col1) .gt. 0) CYCLE
	   check_numbers = sudoku_logic_values(row1,col1)
	   temp_number = 0
	   temp_array = 0
	   DO inumber=1,check_numbers
     	     check_number_ij = sudoku_logic(row1,col1,inumber)
 	     IF (check_number_ij .eq. sudoku(i,j)) THEN
		CYCLE
             END IF
	     temp_number=temp_number+1
 	     temp_array(temp_number) = check_number_ij
           END DO
           DO inumber=1,sudoku_logic_values(row1,col1)
              sudoku_logic(row1,col1,inumber) = 0
           END DO
           sudoku_logic_values(row1,col1) = temp_number
           DO inumber=1,temp_number
             sudoku_logic(row1,col1,inumber) = temp_array(inumber)
           END DO
	END DO
      END DO
    END DO
  END DO



END SUBROUTINE update_sudoku_logic_values

!---------------------------------------------------------------------------

SUBROUTINE Forced_moves(flag)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: flag
  INTEGER :: i,j
  INTEGER :: sum_cage,cage_local,cage_no_of_cells_local
  INTEGER :: ii,jj,icell
  INTEGER,DIMENSION(9) :: temp_array,temp_array_new
  INTEGER :: ncount,cell_ij_numbers,cell_ij_logic_number
  INTEGER :: check_number,check_value,icage
  INTEGER :: irow,icol,inumber,jnumber,ncells
  INTEGER :: cage1h_real,cage2h_real,check_cage1
  INTEGER :: jcage,iicage

  DO i=1,max_grid_size1
jloop:    DO j=1,max_grid_size2
      flag = 0
      IF (sudoku_logic_values(i,j) .eq. 1) THEN
	sudoku_logic_values(i,j) = 0
	sudoku(i,j) = sudoku_logic(i,j,1)

!---------------------
  cage1h_real=cell_to_cage(i,j,1)
  cage2h_real=cell_to_cage(i,j,2)
!  ! check if the cage is already present in the forcin_cage_stack
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   icage=forcing_loop_cage(jcage)
!   IF (icage .eq. cage1h_real) THEN
!     check_cage1=1
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage1h_real
!  END IF
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   icage=forcing_loop_cage(jcage)
!   IF (icage .eq. cage2h_real) THEN
!     check_cage1=1
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage2h_real
!  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage1h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage1h_real
  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage2h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage2h_real
  END IF

!--------------------

	CALL update_sudoku_logic_values

      sum_cage = 0
      cage_local = cell_to_cage(i,j,1)
      cage_no_of_cells_local = cage_no_of_cells(cage_local)
      DO icell=1,cage_no_of_cells_local
        ii = cage_to_cells(cage_local,icell,1)
        jj = cage_to_cells(cage_local,icell,2)
        IF (sudoku(ii,jj) .le. 0) CYCLE jloop
	sum_cage = sum_cage + sudoku(ii,jj)
      END DO
      IF (sum_cage .ne. cage_sum(cage_local)) THEN
	flag = 1
	RETURN
      END IF

      sum_cage = 0
      cage_local = cell_to_cage(i,j,2)
      cage_no_of_cells_local = cage_no_of_cells(cage_local)
      DO icell=1,cage_no_of_cells_local
        ii = cage_to_cells(cage_local,icell,1)
        jj = cage_to_cells(cage_local,icell,2)
        IF (sudoku(ii,jj) .le. 0) CYCLE jloop
        sum_cage = sum_cage + sudoku(ii,jj)
      END DO
      IF (sum_cage .ne. cage_sum(cage_local)) THEN
        flag = 1
        RETURN
      END IF


      END IF
    END DO jloop
  END DO


! check that there are no duplicate numbers in a cage
  DO icage=1,total_no_of_cages
    temp_array=0
    ncells=cage_no_of_cells(icage)
    DO icell=1,ncells
      irow=cage_to_cells(icage,icell,1)
      icol=cage_to_cells(icage,icell,2)
      IF (sudoku(irow,icol) .le. 0) CYCLE
      temp_array(sudoku(irow,icol)) = temp_array(sudoku(irow,icol))+1
    END DO 
    DO inumber=1,9
      IF (temp_array(inumber) .gt. 1) THEN
	flag=1
	RETURN
      END IF
    END DO
  END DO

! check that there are some cells that do not have any sudoku logic values left

  DO irow=1,max_grid_size1
    DO icol=1,max_grid_size2
      IF (sudoku(irow,icol) .ne. 0) CYCLE
      IF (sudoku_logic_values(irow,icol) .lt. 1) THEN
	flag=1
	RETURN
      END IF
    END DO
  END DO

! it is possible that a number that has to be placed in a cell is not there in its
!  sudoku logic values.. then it is a contradiction.. 
!  check it with the combination.. that means a valid combination is not present..
!  check it with the numbers in a combination

! Make sure that the numbers that we have put in a cage has a valid combination
!  present in the total permutations.. else something is wrong.. contradiction..


! Make sure that if a sure number is present then put the number in the cell
  DO icage=1,total_no_of_cages
jnumber_loop:     DO jnumber=1,9
         check_number=0
	 ncount=0
	 temp_array_new=0
         CALL check_sure_number(icage,jnumber,check_number,ncount,temp_array_new)  
!	write(*,*) 'jnumber,ncount and check_number is :',jnumber,ncount,check_number
!	write(*,*) temp_array_new(:)
!	 IF (ncount .eq. 0) THEN
!	   write(*,*) 'SOMETHING IS WRONG - check sure number'
!	   flag = 1
!	   STOP
!         END IF
	 IF (check_number .eq. 0) CYCLE
	 ! If check_number == 1 then there jnumber is a sure number.. 
	 ! Now if jnumber is present in only one cell then put the number
	 ! in the cell and call update sudoku logic values of other cells
         temp_array=0
         ncells=cage_no_of_cells(icage)
         DO icell=1,ncells
      	   irow=cage_to_cells(icage,icell,1)
           icol=cage_to_cells(icage,icell,2)
	   IF (sudoku(irow,icol) .eq. jnumber) CYCLE jnumber_loop
           IF (sudoku(irow,icol) .eq. 0) THEN
	     cell_ij_numbers=sudoku_logic_values(irow,icol)
	     DO inumber=1,cell_ij_numbers
	 	cell_ij_logic_number=sudoku_logic(irow,icol,inumber)
                temp_array(cell_ij_logic_number) = temp_array(cell_ij_logic_number)+1
	     END DO
	   END IF
         END DO
         IF (temp_array(jnumber) .eq. 1) THEN
	   ! Find the cell with jnumber in sudoku_logic_values..
	   ! update the sudoku logic values of other cells
iloop:	   DO icell=1,ncells
	     irow=cage_to_cells(icage,icell,1)
	     icol=cage_to_cells(icage,icell,2)
	     cell_ij_numbers=sudoku_logic_values(irow,icol)
	     DO inumber=1,cell_ij_numbers
		cell_ij_logic_number=sudoku_logic(irow,icol,inumber)
		IF (cell_ij_logic_number .eq. jnumber) THEN
		  sudoku_logic_values(irow,icol) = 0
		  sudoku(irow,icol) = jnumber
!----------------------------------------------------------------
  cage1h_real=cell_to_cage(irow,icol,1)
  cage2h_real=cell_to_cage(irow,icol,2)
  ! check if the cage is already present in the forcin_cage_stack
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   iicage=forcing_loop_cage(jcage)
!   IF (iicage .eq. cage1h_real) THEN
!     check_cage1=1 
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage1h_real
!  END IF
!  check_cage1=0
!  DO jcage=1,forcing_ncages_stack
!   iicage=forcing_loop_cage(jcage)
!   IF (iicage .eq. cage2h_real) THEN
!     check_cage1=1
!     EXIT
!   END IF
!  END DO
!  IF (check_cage1 .eq. 0) THEN
!    forcing_ncages_stack=forcing_ncages_stack+1
!    forcing_loop_cage(forcing_ncages_stack)=cage2h_real
!  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage1h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage1h_real
  END IF

  check_cage1=0
  DO jj=1,forcing_ncages_stack(forcing_loop_stack_length)
    jcage=forcing_loop_cage(forcing_loop_stack_length,jj)
    IF (jcage .eq. cage2h_real) THEN
      check_cage1=1
      EXIT
    END IF
  END DO
  IF (check_cage1 .eq. 0) THEN
    forcing_ncages_stack(forcing_loop_stack_length) = &
        forcing_ncages_stack(forcing_loop_stack_length)+1
    forcing_loop_cage(forcing_loop_stack_length, &
        forcing_ncages_stack(forcing_loop_stack_length))=cage2h_real
  END IF


!---------------------------------------------------------------
		  CALL update_sudoku_logic_values
		  EXIT iloop
		END IF
	     END DO
	   END DO iloop
	 END IF
       END DO jnumber_loop
       ! The values ncount and temp_array_new are the same for all the calls to
       !  to check_sure_number subroutine.. Make sure that all the cells that are
       !  not filled have sudoku logic values present in temp_array_new
       IF (ncount .eq. 0) CYCLE
       DO icell=1,ncells
	irow=cage_to_cells(icage,icell,1)
	icol=cage_to_cells(icage,icell,2)
	IF (sudoku(irow,icol) .ne. 0) CYCLE
	cell_ij_numbers=sudoku_logic_values(irow,icol)
	check_value=0
	DO inumber=1,cell_ij_numbers
	  cell_ij_logic_number=sudoku_logic(irow,icol,inumber)
	  IF (temp_array_new(cell_ij_logic_number) .gt. 0) THEN
	    check_value=1
	  END IF
	END DO
	IF (check_value .eq. 0) THEN
	  flag=1
	  RETURN
	END IF
      END DO
    END DO

! COM: NEWLY ADDED
cage_loop:    DO cage_local=1,total_no_of_cages
      flag = 0
      sum_cage = 0
      cage_no_of_cells_local = cage_no_of_cells(cage_local)
      DO icell=1,cage_no_of_cells_local
        ii = cage_to_cells(cage_local,icell,1)
        jj = cage_to_cells(cage_local,icell,2)
        IF (sudoku(ii,jj) .le. 0) CYCLE cage_loop
        sum_cage = sum_cage + sudoku(ii,jj)
      END DO
      IF (sum_cage .ne. cage_sum(cage_local)) THEN
        flag = 1
        RETURN
      END IF
   END DO cage_loop

! COM: END NEWLY ADDED

   flag = 0
   CALL Unique_rectangle_situation(flag)

END SUBROUTINE Forced_moves

!------------------------------------------------------------------------

SUBROUTINE update_cage_cells

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,ncells,icell,row1,col1
  INTEGER :: empty_cells,filled_cells,cage_cells
  INTEGER,DIMENSION(9) :: empty,filled
  INTEGER,DIMENSION(9) :: check_sudoku_logic
  INTEGER :: check_sudoku_logic_values
  INTEGER :: inumber,total_permutations,jcell
  INTEGER :: temp_number,jnumber
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: check_numbers,cell_logic_number
  INTEGER :: i,j,kcell,lower_state
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: cage_sum_local

 DO icage=1,total_no_of_cages
   ncells = cage_no_of_cells(icage)

   cage_cells = ncells
   cage_sum_local = cage_sum(icage)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF


   empty_cells = 0
   filled_cells = 0
   check_sudoku_logic_values = 0
   check_sudoku_logic = 0
   filled = 0

   DO icell=1,ncells
     row1 = cage_to_cells(icage,icell,1)
     col1 = cage_to_cells(icage,icell,2)
     IF (sudoku(row1,col1) .LE. 0) empty_cells = empty_cells+1
     IF (sudoku(row1,col1) .GT. 0) THEN
	filled_cells = filled_cells+1
	filled(filled_cells) = sudoku(row1,col1)
     END IF
   END DO

   IF (empty_cells .eq. ncells) CYCLE
   IF (ncells .eq. 2) THEN
	total_permutations = 4
   ELSE IF (ncells .eq. 3) THEN
	total_permutations = 8
   ELSE IF (ncells .eq. 4) THEN
	total_permutations = 12
   ELSE IF (ncells .eq. 5) THEN
	total_permutations = 12
   ELSE IF (ncells .eq. 6) THEN
	total_permutations = 8
   ELSE IF (ncells .eq. 7) THEN
	total_permutations = 4
   ELSE IF (ncells .eq. 8) THEN
	total_permutations = 1
   ELSE IF (ncells .eq. 9) THEN
	total_permutations = 1
   END IF


   DO icell=1,filled_cells
inumber_loop:	DO inumber=1,total_permutations
          DO jcell=1,ncells
            IF(pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. filled(icell)) THEN
		!If the number is present in the combintaion then add the rest to 
		! check_sudoku_logic arrays
		DO kcell=1,ncells
		  check_sudoku_logic(pa(cage_sum_local-lower_state+1,inumber,kcell)) = 1
		END DO
	        CYCLE inumber_loop
	    END IF
	  END DO
	END DO inumber_loop
    END DO

    DO inumber=1,9
      DO icell=1,filled_cells
         IF (inumber .eq. filled(icell)) THEN
		check_sudoku_logic(inumber) = 0
         END IF
      END DO
    END DO


   DO icell=1,ncells
     row1 = cage_to_cells(icage,icell,1)
     col1 = cage_to_cells(icage,icell,2)
     IF (sudoku(row1,col1) .GT. 0) CYCLE
     temp_number = 0
     temp_array = 0
     check_numbers = sudoku_logic_values(row1,col1)
     DO inumber=1,check_numbers
       cell_logic_number = sudoku_logic(row1,col1,inumber)
       DO jnumber=1,9
         IF (check_sudoku_logic(jnumber) .eq. 1) THEN
	    IF (jnumber .eq. cell_logic_number) THEN
		temp_number=temp_number+1
		temp_array(temp_number) = jnumber
	     END IF
	  END IF
	END DO
     END DO
     DO inumber=1,sudoku_logic_values(row1,col1)
         sudoku_logic(row1,col1,inumber) = 0
     END DO
     sudoku_logic_values(row1,col1) = temp_number
     DO inumber=1,temp_number
         sudoku_logic(row1,col1,inumber) = temp_array(inumber)
     END DO
   END DO

 END DO


END SUBROUTINE update_cage_cells

!----------------------------------------------------------------------
 
SUBROUTINE Eliminate_hidden_impossible

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,ncells,icell,row1,col1
  INTEGER :: empty_cells,filled_cells,cage_cells
  INTEGER,DIMENSION(9) :: empty,filled
  INTEGER,DIMENSION(9) :: check_sudoku_logic
  INTEGER :: check_sudoku_logic_values
  INTEGER :: inumber,total_permutations,jcell
  INTEGER :: temp_number,jnumber
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: check_numbers,cell_logic_number
  INTEGER :: i,j,kcell,lower_state
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: cage_sum_local
  INTEGER :: temp_cell_value,temp_row,temp_col
  INTEGER,DIMENSION(9) :: empty_row,empty_col
  INTEGER,DIMENSION(90,90,9) :: empty_values
  INTEGER :: kvalue,knumber,check_cell
  INTEGER :: row2,col2
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER :: naked_cells
  INTEGER,DIMENSION(9) :: naked_values
  INTEGER,DIMENSION(numbers_max) :: numbers
  INTEGER,DIMENSION(values_max,3) :: values
  INTEGER :: row_test1,row_test2,col_test1,col_test2
  INTEGER :: ivalue,value1,value2
  INTEGER :: inaked,jnaked,count,isValid
  INTEGER :: check_all_permutations
  INTEGER :: row_test3,col_test3,value3
  INTEGER :: ncells1
  INTEGER :: check1,check2,check3,check5
  INTEGER :: temp_number1
  INTEGER,DIMENSION(9) :: temp_array1
  INTEGER :: cell_logic_numbers,cell_ij_logic_number
  INTEGER :: naked_cells1
  INTEGER :: knumber1
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: naked_cells_all
  

  DO icage=1,total_no_of_cages
    ncells = cage_no_of_cells(icage)

    cage_cells = ncells
    cage_sum_local = cage_sum(icage)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

   empty_cells = 0
   filled_cells = 0
   check_sudoku_logic_values = 0
   check_sudoku_logic = 0
   filled = 0
   naked_list = 0
   naked_cells = 0
   naked_values = 0
   empty_values = 0
   naked_cells_all = 0

    CALL Naked_pairs(icage,naked_list,naked_values)


iloop:   DO icell=1,ncells
     row1 = cage_to_cells(icage,icell,1)
     col1 = cage_to_cells(icage,icell,2)
     DO inaked=1,naked_list_max
      DO jnaked=1,4
	IF ((row1 .eq. naked_list(inaked,jnaked,1)) .AND. &
		col1 .eq. naked_list(inaked,jnaked,2)) THEN
	  naked_cells = naked_cells+1
	  naked_cells_all(row1,col1) = 1
	  CYCLE iloop
	END IF
       END DO
      END DO
     IF (sudoku(row1,col1) .LE. 0) THEN 
  	empty_cells = empty_cells+1
	empty_row(empty_cells) = row1
 	empty_col(empty_cells) = col1
	empty_values(row1,col1,:) = 0
     END IF
     IF (sudoku(row1,col1) .GT. 0) THEN
        filled_cells = filled_cells+1
        filled(filled_cells) = sudoku(row1,col1)
     END IF
   END DO iloop

   naked_cells1 = naked_cells

!  write(*,*) 'NAKED HERE--1 :naked cells is :',naked_cells
  naked_cells=0
  DO row1=1,max_grid_size1
    DO col1=1,max_grid_size2
      IF (naked_cells_all(row1,col1) .eq. 1) THEN
        naked_cells=naked_cells+1
      END IF
    END DO
  END DO
!  write(*,*) 'NAKED HERE--2 :naked cells is :',naked_cells

   IF (filled_cells+naked_cells .eq. ncells) CYCLE
   IF (filled_cells+naked_cells .gt. ncells) CYCLE

   IF (empty_cells .gt. 4) CYCLE

   temp_cell_value = 10
   temp_row = 0
   temp_col = 0
  IF (empty_cells .gt. 1) THEN
   DO kcell=1,empty_cells
     row2 = empty_row(kcell)
     col2 = empty_col(kcell)
     IF (sudoku_logic_values(row2,col2) .lt. temp_cell_value) THEN
	temp_cell_value = sudoku_logic_values(row2,col2)
        temp_row = row2
        temp_col = col2
 	empty_row(kcell) = empty_row(1)
	empty_col(kcell) = empty_col(1)
	empty_row(1) = temp_row
	empty_col(1) = temp_col	
     END IF
   END DO 
  END IF

  IF (empty_cells .eq. 1) THEN
	temp_row = empty_row(1)
	temp_col = empty_col(1)
  END IF
   
  
   IF (ncells .eq. 2) THEN
        total_permutations = 4
   ELSE IF (ncells .eq. 3) THEN
        total_permutations = 8
   ELSE IF (ncells .eq. 4) THEN
        total_permutations = 12
   ELSE IF (ncells .eq. 5) THEN
        total_permutations = 12
   ELSE IF (ncells .eq. 6) THEN
        total_permutations = 8
   ELSE IF (ncells .eq. 7) THEN
        total_permutations = 4
   ELSE IF (ncells .eq. 8) THEN
        total_permutations = 1
   ELSE IF (ncells .eq. 9) THEN
        total_permutations = 1
   END IF

     IF (ncells-filled_cells-naked_cells .lt. 0) CYCLE
     IF (ncells-filled_cells-naked_cells .le. 3) THEN
	ncells1 = ncells
	check_all_permutations = 0
	temp_number1 = 0
	temp_array = 0
kloop:       DO kvalue=1,sudoku_logic_values(temp_row,temp_col)
         knumber = sudoku_logic(temp_row,temp_col,kvalue)   
	 isValid = 0
	 CALL check_valid(temp_row,temp_col,knumber,isValid)
	 IF (isValid .eq. 0) CYCLE
	check5 = 0
inumber_loop:   DO inumber=1,total_permutations
	  IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
		EXIT
	  END IF
	  check_cell = 0
	  numbers = 0
	  values = 0
	  count = 0
	  naked_cells = naked_cells1
jloop:          DO jcell=1,ncells1
           IF (pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. knumber) THEN
	     check_cell = check_cell+1
	     CYCLE jloop
	   END IF
           DO icell=1,filled_cells
            IF(pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. filled(icell)) THEN 
		check_cell = check_cell + 1
		CYCLE jloop
            END IF
          END DO
	  DO icell=1,9
	   IF (naked_values(icell) .eq. 1) THEN
            IF(pa(cage_sum_local-lower_state+1,inumber,jcell) .eq. icell) THEN
                check_cell = check_cell + 1
                CYCLE jloop
            END IF
           END IF
          END DO
	  count = count + 1
	  numbers(count) = pa(cage_sum_local-lower_state+1,inumber,jcell)
         END DO jloop

	 IF (check_cell .eq. filled_cells+naked_cells+1) THEN
		check_all_permutations = check_all_permutations+1
		check5 = 1
		IF (empty_cells .eq. 1) THEN
		  row_test1 = empty_row(1)
		  col_test1 = empty_col(1)
		  value1 = knumber
		  isValid = 0
		  CALL check_valid(row_test1,col_test1,value1,isValid)
		  IF (isVAlid .eq. 1) THEN
		    empty_values(row_test1,col_test1,value1) = &
			empty_values(row_test1,col_test1,value1) + 1
		    EXIT kloop
		  END IF
		ELSE IF (empty_cells .eq. 2) THEN
		  row_test1 = empty_row(2)
		  col_test1 = empty_col(2)
                  value1 = numbers(1)
		  isValid = 0
		  CALL check_valid(row_test1,col_test1,value1,isValid)
		  IF (isValid .eq. 1) THEN
		    empty_values(row_test1,col_test1,value1) = &
			empty_values(row_test1,col_test1,value1) + 1
		  END IF 
		ELSE IF (empty_cells .eq. 3) THEN
		  CALL find_2permutations(numbers,values)
		  DO ivalue=1,2
		   row_test1 = empty_row(2)
		   col_test1 = empty_col(2)
		   value1 = values(ivalue,1)
		   row_test2 = empty_row(3)
		   col_test2 = empty_col(3)
		   value2 = values(ivalue,2)
		   isValid = 0
		   CALL check_valid(row_test1,col_test1,value1,isValid)
		   IF (isValid .eq. 1) THEN
		     empty_values(row_test1,col_test1,value1) = &
			empty_values(row_test1,col_test1,value1) + 1
		   END IF
		   isValid = 0
		   CALL check_valid(row_test2,col_test2,value2,isValid)
		   IF (isValid .eq. 1) THEN
		     empty_values(row_test2,col_test2,value2) = &
			empty_values(row_test2,col_test2,value2) + 1
		   END IF
		  END DO
		ELSE IF (empty_cells .eq. 4) THEN
		  CALL find_3permutations(numbers,values)
		  DO ivalue=1,6
		    row_test1 = empty_row(2)
		    col_test1 = empty_col(2)
		    value1 = values(ivalue,1)
		    row_test2 = empty_row(3)
		    col_test2 = empty_col(3)
		    value2 = values(ivalue,2)
		    row_test3 = empty_row(4)
		    col_test3 = empty_row(4)
		    value3 = values(ivalue,3)
		    isValid = 0
		    CALL check_valid(row_test1,col_test1,value1,isValid)
		    IF (isValid .eq. 1) THEN
		       empty_values(row_test1,col_test1,value1) = &
			  empty_values(row_test1,col_test1,value1) + 1
		    END IF
		    isValid = 0
		    CALL check_valid(row_test2,col_test2,value2)
		    IF (isValid .eq. 1) THEN
		       empty_values(row_test2,col_test2,value2) = &
			  empty_values(row_test2,col_test2,value2) + 1
		    END IF
                    isValid = 0
                    CALL check_valid(row_test3,col_test3,value3)
                    IF (isValid .eq. 1) THEN
                       empty_values(row_test3,col_test3,value3) = &
                          empty_values(row_test3,col_test3,value3) + 1
                    END IF
		  END DO
	 	ELSE 
		END IF	
	 END IF
        END DO inumber_loop
	IF (check5 .eq. 1) THEN
	  temp_number1 = temp_number1 + 1
	  knumber1 = knumber
	  temp_array(temp_number1) = knumber1
	END IF
      END DO kloop
      DO inumber=1,sudoku_logic_values(temp_row,temp_col) 
	sudoku_logic(temp_row,temp_col,inumber) = 0
      END DO
      sudoku_logic_values(temp_row,temp_col) = temp_number1
      DO inumber=1,temp_number1
	sudoku_logic(temp_row,temp_col,inumber) = temp_array(inumber)
      END DO

     IF (check_all_permutations .gt. 0) THEN
      IF (empty_cells .eq. 1) THEN
	check1 = 0
	DO ivalue=1,9
          IF (empty_values(row_test1,col_test1,ivalue) .eq. &
			check_all_permutations) THEN
		sudoku_logic_values(row_test1,col_test1) = 1
		sudoku_logic(row_test1,col_test1,1) = ivalue
		check1 = 1
	  END IF
	END DO
 	IF (check1 .eq. 0) THEN
	  temp_number = 0
	  temp_array = 0
	  cell_logic_numbers = sudoku_logic_values(row_test1,col_test1)
          DO inumber=1,cell_logic_numbers
	     cell_ij_logic_number = sudoku_logic(row_test1,col_test1,inumber)
	     IF (empty_values(row_test1,col_test1,cell_ij_logic_number) .eq. 0) CYCLE
	     temp_number = temp_number+1
	     temp_array(temp_number) = cell_ij_logic_number
	  END DO
 	  DO inumber=1,sudoku_logic_values(row_test1,col_test1)
	    sudoku_logic(row_test1,col_test1,inumber) = 0
          END DO
	  sudoku_logic_values(row_test1,col_test1) = temp_number
	  DO inumber=1,temp_number
	    sudoku_logic(row_test1,col_test1,inumber) = temp_array(inumber)
	  END DO
        END IF    
      END IF
      IF (empty_cells .eq. 2) THEN
	 check1 = 0
	 DO ivalue=1,9
	   IF (empty_values(row_test1,col_test1,ivalue) .eq. &
			check_all_permutations) THEN
		sudoku_logic_values(row_test1,col_test1) = 1
		sudoku_logic(row_test1,col_test1,1) = ivalue
		check1 = 1
	   END IF
	 END DO
        IF (check1 .eq. 0) THEN
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test1,col_test1)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test1,col_test1,inumber)
             IF (empty_values(row_test1,col_test1,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
	  DO inumber=1,sudoku_logic_values(row_test1,col_test1)
	    sudoku_logic(row_test1,col_test1,inumber) = 0
          END DO
          sudoku_logic_values(row_test1,col_test1) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test1,col_test1,inumber) = temp_array(inumber)
          END DO
        END IF
      END IF
      IF (empty_cells .eq. 3) THEN
		  check1 = 0
		  check2 = 0
                  DO ivalue=1,9
		   IF (empty_values(row_test1,col_test1,ivalue) .eq. &
			empty_values(row_test2,col_test2,ivalue)) THEN
				CYCLE
		   END IF
                   IF (empty_values(row_test1,col_test1,ivalue) .eq. &
				check_all_permutations) THEN
                        sudoku_logic_values(row_test1,col_test1) = 1
                        sudoku_logic(row_test1,col_test1,1) = ivalue
			check1 = 1
                   END IF
                   IF (empty_values(row_test2,col_test2,ivalue) .eq. &
				check_all_permutations) THEN
                        sudoku_logic_values(row_test2,col_test2) = 1
                        sudoku_logic(row_test2,col_test2,1) = ivalue
			check2 = 1
                   END IF
                  END DO
         IF (check1 .eq. 0) THEN
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test1,col_test1)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test1,col_test1,inumber)
             IF (empty_values(row_test1,col_test1,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
          DO inumber=1,sudoku_logic_values(row_test1,col_test1)
            sudoku_logic(row_test1,col_test1,inumber) = 0
          END DO
          sudoku_logic_values(row_test1,col_test1) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test1,col_test1,inumber) = temp_array(inumber)
          END DO
         END IF
         IF (check2 .eq. 0) THEN
          temp_number = 0
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test2,col_test2)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test2,col_test2,inumber)
             IF (empty_values(row_test2,col_test2,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
          DO inumber=1,sudoku_logic_values(row_test2,col_test2)
            sudoku_logic(row_test2,col_test2,inumber) = 0
          END DO
          sudoku_logic_values(row_test2,col_test2) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test2,col_test2,inumber) = temp_array(inumber)
          END DO
         END IF
	END IF
	IF (empty_cells .eq. 4) THEN
	  DO ivalue=1,9
	    check1 = 0
	    check2 = 0
  	    check3 = 0
            IF (empty_values(row_test1,col_test1,ivalue) .eq. &
		empty_values(row_test2,col_test2,ivalue)) THEN
			CYCLE
	    END IF
            IF (empty_values(row_test1,col_test1,ivalue) .eq. &
		empty_values(row_test3,col_test3,ivalue)) THEN
			CYCLE
	    END IF
            IF (empty_values(row_test2,col_test2,ivalue) .eq. &
		empty_values(row_test3,col_test3,ivalue)) THEN
			CYCLE
	    END IF

	    IF (empty_values(row_test1,col_test1,ivalue) .eq. &
			check_all_permutations) THEN
		sudoku_logic_values(row_test1,col_test1) = 1
		sudoku_logic(row_test1,col_test1,1) = ivalue
		check1 = 1
	    END IF
            IF (empty_values(row_test2,col_test2,ivalue) .eq. &
                        check_all_permutations) THEN
                sudoku_logic_values(row_test2,col_test2) = 1
                sudoku_logic(row_test2,col_test2,1) = ivalue
		check2 = 1
            END IF
            IF (empty_values(row_test3,col_test3,ivalue) .eq. &
                        check_all_permutations) THEN
                sudoku_logic_values(row_test3,col_test3) = 1
                sudoku_logic(row_test3,col_test3,1) = ivalue
		check3 = 1
            END IF
	  END DO
         IF (check1 .eq. 0) THEN
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test1,col_test1)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test1,col_test1,inumber)
             IF (empty_values(row_test1,col_test1,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
          DO inumber=1,sudoku_logic_values(row_test1,col_test1)
            sudoku_logic(row_test1,col_test1,inumber) = 0
          END DO
          sudoku_logic_values(row_test1,col_test1) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test1,col_test1,inumber) = temp_array(inumber)
          END DO
         END IF
         IF (check2 .eq. 0) THEN
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test2,col_test2)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test2,col_test2,inumber)
             IF (empty_values(row_test2,col_test2,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
          DO inumber=1,sudoku_logic_values(row_test1,col_test1)
            sudoku_logic(row_test2,col_test2,inumber) = 0
          END DO
          sudoku_logic_values(row_test2,col_test2) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test2,col_test2,inumber) = temp_array(inumber)
          END DO
         END IF
        IF (check3 .eq. 0) THEN
          temp_number = 0
          temp_array = 0
          cell_logic_numbers = sudoku_logic_values(row_test3,col_test3)
          DO inumber=1,cell_logic_numbers
             cell_ij_logic_number = sudoku_logic(row_test3,col_test3,inumber)
             IF (empty_values(row_test3,col_test3,cell_ij_logic_number) .eq. 0) CYCLE
             temp_number = temp_number+1
             temp_array(temp_number) = cell_ij_logic_number
          END DO
          DO inumber=1,sudoku_logic_values(row_test1,col_test1)
            sudoku_logic(row_test3,col_test3,inumber) = 0
          END DO
          sudoku_logic_values(row_test3,col_test3) = temp_number
          DO inumber=1,temp_number
            sudoku_logic(row_test3,col_test3,inumber) = temp_array(inumber)
          END DO
         END IF
	END IF
      END IF

    END IF

  END DO 


END SUBROUTINE Eliminate_hidden_impossible

!------------------------------------------------------------------- 

SUBROUTINE find_3permutations(numbers,values)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(numbers_max) :: numbers
  INTEGER,DIMENSION(values_max,3) :: values

  values(1,1) = numbers(1)
  values(1,2) = numbers(2)
  values(1,3) = numbers(3)

  values(2,1) = numbers(1)
  values(2,2) = numbers(3)
  values(2,3) = numbers(2)

  values(3,1) = numbers(2)
  values(3,2) = numbers(1)
  values(3,3) = numbers(3)

  values(4,1) = numbers(2)
  values(4,2) = numbers(3)
  values(4,3) = numbers(1)

  values(5,1) = numbers(3)
  values(5,2) = numbers(1)
  values(5,3) = numbers(2)

  values(6,1) = numbers(3)
  values(6,2) = numbers(2)
  values(6,3) = numbers(1)  
  

END SUBROUTINE find_3permutations

!-----------------------------------------------------------------

SUBROUTINE find_2permutations(numbers,values)

  USE global_variables
  IMPLICIT NONE

  INTEGER,DIMENSION(numbers_max) :: numbers
  INTEGER,DIMENSION(values_max,3) :: values

  values(1,1) = numbers(1)
  values(1,2) = numbers(2)
  values(1,3) = 0

  values(2,1) = numbers(2)
  values(2,2) = numbers(1)
  values(2,3) = 0

 
END SUBROUTINE find_2permutations

!---------------------------------------------------------------

SUBROUTINE check_valid(row_test,col_test,value,isValid)

  USE global_variables
  IMPLICIT NONE
 
  INTEGER :: row_test,col_test,value,isValid
  INTEGER :: icell,ncells,row1,col1,icage
  INTEGER :: cage_number_local,cage_sum_local

  isValid = 1


  DO icage=1,2
    cage_number_local = cell_to_cage(row_test,col_test,icage)
    cage_sum_local = cell_to_cage_sum(row_test,col_test,icage)
    ncells = cage_no_of_cells(cage_number_local)
    DO icell=1,ncells
           row1 = cage_to_cells(cage_number_local,icell,1)
           col1 = cage_to_cells(cage_number_local,icell,2)
	   IF ((row1 .eq. row_test) .AND. (col1 .eq. col_test)) CYCLE
	   IF (sudoku(row1,col1) .eq. value) THEN

		isValid = 0
		RETURN
	   END IF
     END DO
  END DO


END SUBROUTINE check_valid

!----------------------------------------------------------------

SUBROUTINE Naked_pairs(icage,naked_list,naked_values)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage,cage_sum_local,ncells
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values
  INTEGER :: i,j,k,l
  INTEGER :: logic_numbers_ij,logic_numbers_other_cell
  INTEGER :: row_small_cell,column_small_cell
  INTEGER :: small_cell_row,small_cell_column
  INTEGER,DIMENSION(9,9) :: naked_pair_cell
  INTEGER :: naked_pair_list_ij,naked_pair_list_kl,naked_pair_list_counter
  INTEGER,DIMENSION(9) :: naked_pair_list
  INTEGER :: row_cell,column_cell
  INTEGER :: naked_pair_start,naked_pair_end
  INTEGER :: ii,jj,kk
  INTEGER :: number_m,number_n
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: p,m,r,q,n
  INTEGER :: check,check1
  INTEGER :: logic_numbers_kl,check_cell
  INTEGER :: naked_pair_list_check
  INTEGER :: j_cell,k_cell,l_cell,m_cell
  INTEGER :: jnumber,knumber,lnumber,mnumber
  INTEGER :: check_j,check_k,check_l,check_m
  INTEGER :: cell_j_numbers,cell_k_numbers,cell_l_numbers,cell_m_numbers
  INTEGER :: cell_j_logic_number,cell_k_logic_number,cell_l_logic_number,cell_m_logic_number
  INTEGER :: cell_ij_numbers
  INTEGER :: i_row,j_column,mm,nn,row_k,column_k,ll
  INTEGER :: cell_other_logic_number,cell_other_numbers
  INTEGER :: ii_other,jj_other,inumber,temp_number,other_cell
  INTEGER :: row1,col1,row2,col2,row3,col3,row4,col4,row5,col5
  INTEGER :: naked_count

  cage_sum_local = cage_sum(icage)
  ncells = cage_no_of_cells(icage)
  naked_count = 0

  DO j=1,9
    DO k=1,9
      
jloop: DO j_cell = 1,ncells
        row1 = cage_to_cells(icage,j_cell,1)
	col1 = cage_to_cells(icage,j_cell,2)

        check_j = 0
	cell_j_numbers = sudoku_logic_values(row1,col1)
	IF (sudoku(row1,col1) .ne. 0) CYCLE
	IF (cell_j_numbers .ne. 2) CYCLE
	DO jnumber=1,cell_j_numbers
	  cell_j_logic_number = sudoku_logic(row1,col1,jnumber)
          IF ((cell_j_logic_number .eq. k) .OR. &
		(cell_j_logic_number .eq. j)) THEN
		check_j = 1
	  ELSE 
		CYCLE jloop
	  END IF
	END DO

kloop: DO k_cell=j_cell+1,ncells
	IF (k_cell .eq. j_cell) CYCLE
        check_k = 0
	row2 = cage_to_cells(icage,k_cell,1)
	col2 = cage_to_cells(icage,k_cell,2)
	
        cell_k_numbers = sudoku_logic_values(row2,col2)
	IF (cell_k_numbers .ne. 2) CYCLE
	IF (sudoku(row2,col2) .ne. 0) CYCLE
	DO knumber=1,cell_k_numbers
	  cell_k_logic_number = sudoku_logic(row2,col2,knumber)
	  IF ((cell_k_logic_number .eq. k) .OR. &
		(cell_k_logic_number .eq. j)) THEN
		check_k = 1
	  ELSE
		CYCLE kloop
	  END IF
	END DO
     
        IF ((check_j .eq. 1) .AND. (check_k .eq. 1)) THEN
	naked_count = naked_count+1
	naked_values(j) = 1
	naked_values(k) = 1
	naked_list(naked_count,1,1) = row1
	naked_list(naked_count,1,2) = col1
	naked_list(naked_count,2,1) = row2
	naked_list(naked_count,2,2) = col2
	 DO other_cell=1,ncells
	   temp_array = 0
	   temp_number = 0
	   row3 = cage_to_cells(icage,other_cell,1)
	   col3 = cage_to_cells(icage,other_cell,2)
	   IF (sudoku(row3,col3) .ne. 0) CYCLE
	   IF ((other_cell .eq. j_cell) .OR. (other_cell .eq. k_cell)) CYCLE
	   cell_other_numbers = sudoku_logic_values(row3,col3)
	   DO inumber=1,cell_other_numbers
	    cell_other_logic_number = sudoku_logic(row3,col3,inumber)
	    IF ((cell_other_logic_number .eq. j) .OR. &
			(cell_other_logic_number .eq. k)) CYCLE
	    temp_number = temp_number + 1
	    temp_array(temp_number) = cell_other_logic_number
	  END DO

	DO inumber=1,sudoku_logic_values(row3,col3)
	  sudoku_logic(row3,col3,inumber) = 0
	END DO
	sudoku_logic_values(row3,col3) = temp_number
	DO inumber=1,temp_number
	  sudoku_logic(row3,col3,inumber) = temp_array(inumber)
	END DO
       END DO

     END IF
    END DO kloop
   END DO jloop
  END DO
 END DO

  DO j=1,9
   DO k=j+1,9
    IF (k .eq. j) CYCLE
    DO l=k+1,9
     IF ((l .eq. j) .OR. (l .eq. k)) CYCLE

jloop1: DO j_cell=1,ncells
     check_j=0
     row1 = cage_to_cells(icage,j_cell,1)
     col1 = cage_to_cells(icage,j_cell,2)
     cell_j_numbers = sudoku_logic_values(row1,col1)
     IF (sudoku(row1,col1) .ne. 0) CYCLE
     IF (cell_j_numbers .gt. 3) CYCLE
     DO jnumber=1,cell_j_numbers
      cell_j_logic_number = sudoku_logic(row1,col1,jnumber)
      IF ((cell_j_logic_number .eq. j) .OR. &
		(cell_j_logic_number .eq. k) .OR. &
		(cell_j_logic_number .eq. l)) THEN
	  check_j = 1
      ELSE 
	CYCLE jloop1
      END IF
    END DO

kloop1: DO k_cell=j_cell+1,ncells
      IF (k_cell .eq. j_cell) CYCLE
      row2 = cage_to_cells(icage,k_cell,1)
      col2 = cage_to_cells(icage,k_cell,2)
      check_k = 0
      cell_k_numbers = sudoku_logic_values(row2,col2)
      IF (sudoku(row2,col2) .ne. 0) CYCLE
      IF (cell_k_numbers .gt. 3) CYCLE
      DO knumber=1,cell_k_numbers
        cell_k_logic_number = sudoku_logic(row2,col2,knumber)
        IF ((cell_k_logic_number .eq. j) .OR. &
		(cell_k_logic_number .eq. k) .OR. &
		(cell_k_logic_number .eq. l)) THEN
	   check_k = 1
        ELSE 
	   CYCLE kloop1
	END IF
      END DO

lloop1: DO l_cell=k_cell+1,ncells
      IF ((l_cell .eq. j_cell) .OR. (l_cell .eq. k_cell)) CYCLE
      check_l = 0
      row3 = cage_to_cells(icage,l_cell,1)
      col3 = cage_to_cells(icage,l_cell,2)
      check_l = 0
      cell_l_numbers = sudoku_logic_values(row3,col3)
      IF (sudoku(row3,col3) .ne. 0) CYCLE
      IF (cell_l_numbers .gt. 3) CYCLE
      DO lnumber=1,cell_l_numbers
        cell_l_logic_number = sudoku_logic(row3,col3,lnumber)
        IF ((cell_l_logic_number .eq. j) .OR. &
		(cell_l_logic_number .eq. k) .OR. &
		(cell_l_logic_number .eq. l)) THEN
	   check_l = 1
	ELSE
	   CYCLE lloop1
	END IF
      END DO

      IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1)) THEN
        naked_count = naked_count+1
	naked_values(j) = 1
	naked_values(k) = 1
	naked_values(l) = 1
        naked_list(naked_count,1,1) = row1
        naked_list(naked_count,1,2) = col1
        naked_list(naked_count,2,1) = row2
        naked_list(naked_count,2,2) = col2
  	naked_list(naked_count,3,1) = row3
	naked_list(naked_count,3,2) = col3
        DO other_cell=1,ncells
          temp_array = 0
	  temp_number = 0
          row4 = cage_to_cells(icage,other_cell,1)
          col4 = cage_to_cells(icage,other_cell,2)
          IF ((other_cell .eq. j_cell) .OR. (other_cell .eq. k_cell) .OR. &
		(other_cell .eq. l_cell)) CYCLE
	  cell_other_numbers = sudoku_logic_values(row4,col4)
	  DO inumber=1,cell_other_numbers
	    cell_other_logic_number = sudoku_logic(row4,col4,inumber)
            IF ((cell_other_logic_number .eq. j) .OR. &
			(cell_other_logic_number .eq. k) .OR. &
			(cell_other_logic_number .eq. l)) CYCLE
	    temp_number = temp_number + 1
	    temp_array(temp_number) = cell_other_logic_number
	  END DO

	  DO inumber=1,sudoku_logic_values(row4,col4)
	    sudoku_logic(row4,col4,inumber) = 0
  	  END DO
	  sudoku_logic_values(row4,col4) = temp_number
	  DO inumber=1,temp_number
	    sudoku_logic(row4,col4,inumber) = temp_array(inumber)
   	  END DO
        END DO

      END IF
     END DO lloop1
    END DO kloop1
   END DO jloop1
  ! End DO lloop
  END DO
 ! END DO kloop
 END DO
! END DO jloop
 END DO

 DO j=1,9
  DO k=j+1,9
   IF (k .eq. j) CYCLE
   DO l=k+1,9
    IF ((l .eq. k) .OR. (l .eq. j)) CYCLE
    DO m=l+1,9
     IF ((m .eq. l) .OR. (m .eq. k) .OR. (m .eq. j)) CYCLE

jloop2: DO j_cell=1,ncells
      check_j = 0
      row1 = cage_to_cells(icage,j_cell,1)
      col1 = cage_to_cells(icage,j_cell,2)
      cell_j_numbers = sudoku_logic_values(row1,col1)
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      IF (cell_j_numbers .gt. 4) CYCLE
      DO jnumber=1,cell_j_numbers
       cell_j_logic_number = sudoku_logic(row1,col1,jnumber)
       IF ((cell_j_logic_number .eq. j) .OR. &
		(cell_j_logic_number .eq. k) .OR. &
		(cell_j_logic_number .eq. l) .OR. &
		(cell_j_logic_number .eq. m)) THEN
	  check_j = 1
       ELSE
	  CYCLE jloop2	
       END IF
      END DO

kloop2: DO k_cell=j_cell+1,ncells
      IF (k_cell .eq. j_cell) CYCLE
      check_k = 0
      row2 = cage_to_cells(icage,k_cell,1)
      col2 = cage_to_cells(icage,k_cell,2)
      cell_k_numbers = sudoku_logic_values(row2,col2)
      IF (sudoku(row2,col2) .ne. 0) CYCLE
      IF (cell_k_numbers .gt. 4) CYCLE
      DO knumber=1,cell_k_numbers
       cell_k_logic_number = sudoku_logic(row2,col2,knumber)
       IF ((cell_k_logic_number .eq. j) .OR. &
		(cell_k_logic_number .eq. k) .OR. &
		(cell_k_logic_number .eq. l) .OR. &
		(cell_k_logic_number .eq. m)) THEN
	  check_k = 1
       ELSE 
	  CYCLE kloop2
       END IF
      END DO

lloop2: DO l_cell=k_cell+1,ncells
       IF ((l_cell .eq. j_cell) .OR. (l_cell .eq. k_cell)) CYCLE
       check_l = 0
       row3 = cage_to_cells(icage,l_cell,1)
       col3 = cage_to_cells(icage,l_cell,2)
       cell_l_numbers = sudoku_logic_values(row3,col3)
       IF (sudoku(row3,col3) .ne. 0) CYCLE
       IF (cell_l_numbers .gt. 4) CYCLE
       DO lnumber=1,cell_l_numbers
        cell_l_logic_number = sudoku_logic(row3,col3,lnumber)
        IF ((cell_l_logic_number .eq. j) .OR. &
		(cell_l_logic_number .eq. k) .OR. &
		(cell_l_logic_number .eq. l) .OR. &
		(cell_l_logic_number .eq. m)) THEN
	  check_l = 1
        ELSE
	  CYCLE lloop2
	END IF
       END DO

mloop2: DO m_cell=l_cell+1,ncells
      IF ((m_cell .eq. j_cell) .OR. (m_cell .eq. k_cell) .OR. (m_cell .eq. l_cell)) CYCLE
      check_m = 0
      row4 = cage_to_cells(icage,m_cell,1)
      col4 = cage_to_cells(icage,m_cell,2)
      cell_m_numbers = sudoku_logic_values(row4,col4)
      IF (sudoku(row4,col4) .ne. 0) CYCLE
      IF (cell_m_numbers .gt. 4) CYCLE
      DO mnumber=1,cell_m_numbers
       cell_m_logic_number = sudoku_logic(row4,col4,mnumber)
       IF ((cell_m_logic_number .eq. j) .OR. &
		(cell_m_logic_number .eq. k) .OR. &
		(cell_m_logic_number .eq. l) .OR. &
		(cell_m_logic_number .eq. m)) THEN
	  check_m = 1
       ELSE 	 
	  CYCLE mloop2
       END IF
      END DO

      IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1) .AND. &
		(check_m .eq. 1)) THEN
        naked_count = naked_count+1
	naked_values(j) = 1
	naked_values(k) = 1
	naked_values(l) = 1
	naked_values(m) = 1
        naked_list(naked_count,1,1) = row1
        naked_list(naked_count,1,2) = col1
        naked_list(naked_count,2,1) = row2
        naked_list(naked_count,2,2) = col2
 	naked_list(naked_count,3,1) = row3
	naked_list(naked_count,3,2) = col3
	naked_list(naked_count,4,1) = row4
	naked_list(naked_count,4,2) = col4
	DO other_cell=1,ncells
         temp_array = 0
         temp_number = 0
         row5 = cage_to_cells(icage,other_cell,1)
	 col5 = cage_to_cells(icage,other_cell,2)
         IF (sudoku(row5,col5) .ne. 0) CYCLE
         IF ((other_cell .eq. j_cell) .OR. &
		(other_cell .eq. k_cell) .OR. &
		(other_cell .eq. l_cell) .OR. &
		(other_cell .eq. m_cell)) CYCLE
         cell_other_numbers = sudoku_logic_values(row5,col5)
         DO inumber=1,cell_other_numbers
          cell_other_logic_number = sudoku_logic(row5,col5,inumber)
          IF ((cell_other_logic_number .eq. j) .OR. &
		(cell_other_logic_number .eq. k) .OR. &
		(cell_other_logic_number .eq. l) .OR. &
		(cell_other_logic_number .eq. m)) CYCLE
	  temp_number=temp_number + 1
          temp_array(temp_number) = cell_other_logic_number
         END DO
         DO inumber = 1,sudoku_logic_values(row5,col5)
	   sudoku_logic(row5,col5,inumber) = 0
         END DO
         sudoku_logic_values(row5,col5) = temp_number
         DO inumber=1,temp_number
           sudoku_logic(row5,col5,inumber) = temp_array(inumber)
         END DO
        END DO
       END IF

      END DO mloop2
     END DO lloop2
    END DO kloop2
   END DO jloop2
  END DO
 END DO
 END DO
 END DO


END SUBROUTINE Naked_pairs

!-------------------------------------------------------------

SUBROUTINE check_solution(isSolution)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: isSolution,ncells_final
  INTEGER :: i,j

  isSolution = 0
  ncells_final = 0
  DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku(i,j) .gt. 0) THEN
	ncells_final = ncells_final + 1
     END IF
   END DO
 END DO

 IF (ncells_final .eq. no_of_sudoku_cells) THEN
   isSolution = 1
 END IF

END SUBROUTINE check_solution

!------------------------------------------------------------

SUBROUTINE show_solution

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i,j

 DO i=1,max_grid_size1
!   write(*,*) sudoku(i,:)
   write(*,"(18I3)") sudoku(i,:)
 END DO

 write(*,*) 'nAdrianneStates is :',nAdrianneStates


END SUBROUTINE show_solution

!--------------------------------------------------------


SUBROUTINE update_snapshots(snapshots)

   USE global_variables
   IMPLICIT NONE

   INTEGER :: snapshots

        snapshots = snapshots + 1
        IF (snapshots .gt. max_grid_size1*max_grid_size2) THEN
                write(*,*) 'SOMETHING IS WRONG -- SNAPSHOTS :',snapshots
!               STOP
        END IF
                sudoku_snapshot_initial(snapshots,:,:) = &
                        sudoku(:,:)

END SUBROUTINE update_snapshots

!------------------------------------------------------------------------------

SUBROUTINE check_duplicate_snapshots(snapshots)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: snapshots

  INTEGER :: isnap,jsnap,i,j
  INTEGER :: check_snapshot
  INTEGER :: count

iloop:  DO isnap=1,snapshots-1
     IF (duplicate_snapshots(isnap) .eq. 1) CYCLE iloop
jloop:    DO jsnap=isnap+1,snapshots
    check_snapshot = 0
    count = 0
     IF (duplicate_snapshots(jsnap) .eq. 1) CYCLE iloop
        DO i=1,max_grid_size1
          DO j=1,max_grid_size2
            IF (sudoku_snapshot_initial(isnap,i,j) .eq. &
                        sudoku_snapshot_initial(jsnap,i,j)) THEN
                                count=count+1
            END IF
          END DO
        END DO
        IF (count .eq. max_grid_size1*max_grid_size2) THEN
                duplicate_snapshots(jsnap) = 1
                CYCLE jloop
        ELSE
                duplicate_snapshots(isnap) = 2
        END IF

   END DO jloop
 END DO iloop

 IF (duplicate_snapshots(snapshots) .ne. 1) THEN
        duplicate_snapshots(snapshots) = 2
 END IF

END SUBROUTINE check_duplicate_snapshots

!----------------------------------------------------------------------

