SUBROUTINE Adrianne_thread_state

   USE global_variables
   IMPLICIT NONE

   INTEGER :: i,j,istate
   INTEGER :: check_adrianne

   INTEGER :: free_cells,temp_number,icell
   INTEGER :: cell_numbers_i,cell_numbers_j
   INTEGER,DIMENSION(max_grid_size1*max_grid_size2) :: free_cells_row,free_cells_col

   nAdrianneStates = nAdrianneStates+1
   sudoku_Adrianne(nAdrianneStates,:,:) = sudoku
   sudoku_Adrianne_logic(nAdrianneStates,:,:,:) = sudoku_logic
   sudoku_Adrianne_logic_numbers(nAdrianneStates,:,:) = sudoku_logic_values

  IF (nAdrianneStates .gt. nAdrianneStates_max) THEN
    write(*,*) 'The puzzle cannot be solved by logic alone'
    write(*,*) 'It requires a lot of guessing and trial and error'
    write(*,*) 'Try increasing nAdrianneStates_max or use a brute force solver'
   STOP
 END IF


!  GO TO 110

   free_cells = 0
   DO i=1,max_grid_size1
     DO j=1,max_grid_size2
	IF (sudoku(i,j) .ne. 0) CYCLE
	  free_cells = free_cells+1
	  free_cells_row(free_cells) = i
	  free_cells_col(free_cells) = j
     END DO
   END DO

    DO i=1,free_cells
      cell_numbers_i = sudoku_logic_values(free_cells_row(i),free_cells_col(i))
      DO j=i+1,free_cells
	cell_numbers_j = sudoku_logic_values(free_cells_row(j),free_cells_col(j))
	IF (cell_numbers_j .lt. cell_numbers_i) THEN
	   temp_number = cell_numbers_j
	   cell_numbers_j = cell_numbers_i
	   cell_numbers_i = temp_number
	   
	   temp_number = free_cells_row(j)
	   free_cells_row(j) = free_cells_row(i)
	   free_cells_row(i) = temp_number

	   temp_number = free_cells_col(j)
	   free_cells_col(j) = free_cells_col(i)
	   free_cells_col(i) = temp_number
         END IF
        END DO
      END DO

! 110 CONTINUE

   GO TO 220

   ! Find a square and a number to start Adrianne's Thread
iloop:   DO i=1,max_grid_size1
     DO j=1,max_grid_size2
        IF (sudoku(i,j) .ne. 0) CYCLE
          ! Make sure it is not one of the previous cells of the
          ! Adrianne's Thread array
          check_adrianne = 0
          DO istate=1,nAdrianneStates-1
             IF ((i .eq. Adrianne_state_row(istate)) .AND. &
                  (j .eq. Adrianne_state_col(istate))) THEN
                        check_adrianne = 1
                        EXIT
             END IF
          END DO
          IF (check_adrianne .eq. 0) THEN
                Adrianne_state_row(nAdrianneStates) = i
                Adrianne_state_col(nAdrianneStates) = j
                EXIT iloop
          END IF
     END DO
  END DO iloop

! GO TO 111

220 CONTINUE

  DO icell=1,free_cells
    i = free_cells_row(icell)
    j = free_cells_col(icell)
      check_adrianne = 0
          DO istate=1,nAdrianneStates-1
             IF ((i .eq. Adrianne_state_row(istate)) .AND. &
                  (j .eq. Adrianne_state_col(istate))) THEN
                        check_adrianne = 1
                        EXIT
             END IF
          END DO
          IF (check_adrianne .eq. 0) THEN
                Adrianne_state_row(nAdrianneStates) = i
                Adrianne_state_col(nAdrianneStates) = j
                EXIT 
          END IF
  END DO

! 111 CONTINUE

END SUBROUTINE Adrianne_thread_state

!-----------------------------------------------------

SUBROUTINE check_contradiction_Adrianne(sudoku1,sudoku_logic_values1,sudoku_logic1,check_adrianne,max_grid_size11,max_grid_size21)

  USE global_variables
  IMPLICIT NONE

  INTEGER, DIMENSION(max_grid_size1,max_grid_size2) :: sudoku1
  INTEGER, DIMENSION(max_grid_size1,max_grid_size2,9) :: sudoku_logic1
  INTEGER,DIMENSION(max_grid_size1,max_grid_size2) :: sudoku_logic_values1

  INTEGER :: check_adrianne
  INTEGER :: max_grid_size11,max_grid_size21

  INTEGER,DIMENSION(9) :: row_check
  INTEGER,DIMENSION(9) :: col_check
  INTEGER :: i,j,inumber,k
  INTEGER :: small_cell_row,small_cell_column
  INTEGER :: row_k,column_k,ii_other,jj_other
  INTEGER :: ii,jj,cage_local,cage_no_of_cells_local,icell
  INTEGER :: cage_local_previous


  DO i=1,max_grid_size1
    cage_local_previous = 0
    DO j=1,max_grid_size2
      row_check = 0
      IF (sudoku1(i,j) .le. 0) CYCLE
      cage_local = cell_to_cage(i,j,1)
      IF (cage_local .eq. cage_local_previous) CYCLE
      cage_no_of_cells_local = cage_no_of_cells(cage_local)
      DO icell=1,cage_no_of_cells_local
	ii = cage_to_cells(cage_local,icell,1)
	jj = cage_to_cells(cage_local,icell,2) 
	IF (sudoku1(ii,jj) .le. 0) CYCLE
        row_check(sudoku1(ii,jj)) = row_check(sudoku1(ii,jj)) + 1
      END DO
      DO inumber=1,9
       IF(row_check(inumber) .gt. 1) THEN
         check_adrianne = 1
         RETURN
       END IF
      END DO
      cage_local_previous = cage_local
    END DO
  END DO


  DO j=1,max_grid_size2
    cage_local_previous = 0
    DO i=1,max_grid_size1
      col_check = 0
      IF (sudoku1(i,j) .le. 0) CYCLE
      cage_local = cell_to_cage(i,j,2)
      IF (cage_local .eq. cage_local_previous) CYCLE
      cage_no_of_cells_local = cage_no_of_cells(cage_local)
      DO icell=1,cage_no_of_cells_local
        ii = cage_to_cells(cage_local,icell,1)
        jj = cage_to_cells(cage_local,icell,2)
        IF (sudoku1(ii,jj) .le. 0) CYCLE
        col_check(sudoku1(ii,jj)) = col_check(sudoku1(ii,jj)) + 1
      END DO
      DO inumber=1,9
       IF(col_check(inumber) .gt. 1) THEN
         check_adrianne = 1
         RETURN
       END IF
      END DO
      cage_local_previous = cage_local
    END DO
  END DO

 DO i=1,max_grid_size1
   DO j=1,max_grid_size2
     IF (sudoku_logic_values1(i,j) .gt. 0) THEN
     END IF
   END DO
 END DO


END SUBROUTINE check_contradiction_Adrianne

!-----------------------------------------------------------------------------------

