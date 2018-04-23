SUBROUTINE Unique_rectangle_type1

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2

  ! Loop over all the possible combinations of horizontal_2cages.. find 2 cells in each
  ! of the two cages that might form a unique rectangle.. then find 2 vertical cages 
  ! that connect the 4 cells in the horizontal_2cages..

  DO i=1,nhorizontal_cages
!    cage1h = horizontal_2cages(i,1)
!    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(i)

    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.1'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
	temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
	row2 = cage_to_cells(cage1h_real,jcell,1)
	col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.2'
          STOP
        END IF
	IF (sudoku(row2,col2) .ne. 0) CYCLE
	cell_ij2_numbers=sudoku_logic_values(row2,col2)
	IF (cell_ij2_numbers .lt. 2) CYCLE
	DO jnumber=1,cell_ij2_numbers
	  cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
	  temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
	check_number=0
	DO ivalue=1,9
	  IF (temp_array2(ivalue) .eq. 2) THEN
	    check_number=check_number+1
	  END IF
   	END DO 
	IF (check_number .ne. 2) CYCLE
	IF ((cell_ij_numbers .eq. 2) .OR. (cell_ij2_numbers .eq. 2)) THEN
         DO j=i+1,nhorizontal_cages
          cage2h_real = horizontal_cages(j)
	  ncells2=cage_no_of_cells(cage2h_real)
	  DO icell1=1,ncells2
	    row11=cage_to_cells(cage2h_real,icell1,1)
	    col11=cage_to_cells(cage2h_real,icell1,2)
	    IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
	! check if 2 cells share a vertical cage.. the two horizontal cages with
	! the 2 cells are connected by a vertical cage.. else CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.3'
              STOP
            END IF
	    IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
			(vertical_cage2 .ne. vertical_cage12)) CYCLE
	    IF (sudoku(row11,col11) .ne. 0) CYCLE
	    cell_ij3_numbers=sudoku_logic_values(row11,col11)
	    IF (cell_ij3_numbers .lt. 2) CYCLE
	    DO jcell1=icell1+1,ncells2
	      row22=cage_to_cells(cage2h_real,jcell1,1)
	      col22=cage_to_cells(cage2h_real,jcell1,2)
	      IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
	! check if 2 cells share a vertical cage.. the two horizontal cages with
	! the 2 cells are connected by a vertical cage.. else CYCLE
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.4'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                        (vertical_cage2 .ne. vertical_cage12)) CYCLE
	      IF (sudoku(row22,col22) .ne. 0) CYCLE
	      ! We have 2 cells in cage2h_real that forms a rectangle
	      !  with the 2 cells in cage1h_real.. check if they form
	      !  a unique rectangle
	      temp_array3=temp_array2
	      check_total_numbers=0
	      cell_ij4_numbers=sudoku_logic_values(row22,col22)
	      IF (cell_ij4_numbers .lt. 2) CYCLE
	      IF (cell_ij_numbers .eq. 2) THEN
	        check_total_numbers=check_total_numbers+1
	      END IF
	      IF (cell_ij2_numbers .eq. 2) THEN
	        check_total_numbers=check_total_numbers+1
	      END IF
	      IF (cell_ij3_numbers .eq. 2) THEN
	     	check_total_numbers=check_total_numbers+1
	      END IF
	      IF (cell_ij4_numbers .eq. 2) THEN
	     	check_total_numbers=check_total_numbers+1
	      END IF
	      IF (check_total_numbers .ne. 3) CYCLE
	      DO knumber=1,cell_ij3_numbers
	       	cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
	        temp_array3(cell_ij3_logic_number) = &
			temp_array3(cell_ij3_logic_number)+1
	      END DO
	      DO knumber=1,cell_ij4_numbers
	 	cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
		temp_array3(cell_ij4_logic_number) = &
			temp_array3(cell_ij4_logic_number)+1
	      END DO

	      check_number=0
	      DO ivalue=1,9
		IF (temp_array3(ivalue) .eq. 4) THEN
		  check_number=check_number+1
		END IF
	      END DO
	      IF (check_number .ne. 2) CYCLE
	      ! Remove the 2 numbers from the square that contains more possibilities
	      IF (cell_ij_numbers .ne. 2) THEN
		temp_array=0
		temp_number=0
		DO knumber=1,cell_ij_numbers
		  cell_ij_logic_number=sudoku_logic(row1,col1,knumber)
		  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
		  temp_number=temp_number+1
		  temp_array(temp_number) = cell_ij_logic_number
		END DO
		DO knumber=1,cell_ij_numbers
		  sudoku_logic(row1,col1,knumber)=0
		END DO
		sudoku_logic_values(row1,col1)=temp_number
		DO knumber=1,temp_number
		  sudoku_logic(row1,col1,knumber)=temp_array(knumber)
		END DO
	      END IF
              IF (cell_ij2_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij2_numbers
                  cell_ij_logic_number=sudoku_logic(row2,col2,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij2_numbers
                  sudoku_logic(row2,col2,knumber)=0
                END DO
                sudoku_logic_values(row2,col2)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row2,col2,knumber)=temp_array(knumber)
                END DO
              END IF
              IF (cell_ij3_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij3_numbers
                  cell_ij_logic_number=sudoku_logic(row11,col11,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij3_numbers
                  sudoku_logic(row11,col11,knumber)=0
                END DO
                sudoku_logic_values(row11,col11)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row11,col11,knumber)=temp_array(knumber)
                END DO
              END IF
              IF (cell_ij4_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij4_numbers
                  cell_ij_logic_number=sudoku_logic(row22,col22,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij4_numbers
                  sudoku_logic(row22,col22,knumber)=0
                END DO
                sudoku_logic_values(row22,col22)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row22,col22,knumber)=temp_array(knumber)
                END DO
              END IF
	     END DO
	    END DO
	  END DO 
	 END IF
	END DO
      END DO
    END DO

   ! Loop over the vertical_2cages
   DO i=1,nvertical_cages
!    cage1h = vertical_2cages(i,1)
!    cage2h = vertical_2cages(i,2)
    cage1h_real = vertical_cages(i)

    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.5'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.6'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (cell_ij2_numbers .lt. 2) CYCLE
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
        END DO
        IF (check_number .ne. 2) CYCLE
        IF ((cell_ij_numbers .eq. 2) .OR. (cell_ij2_numbers .eq. 2)) THEN
	 DO j=i+1,nvertical_cages
	  cage2h_real=vertical_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.7'
              STOP
            END IF
            IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
	    IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (cell_ij3_numbers .lt. 2) CYCLE
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.8'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
	      IF (sudoku(row22,col22) .ne. 0) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              temp_array3=temp_array2
              check_total_numbers=0
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (cell_ij4_numbers .lt. 2) CYCLE
              IF (cell_ij_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij2_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij3_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij4_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (check_total_numbers .ne. 3) CYCLE
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number)+1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number)+1
              END DO

              check_number=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number=check_number+1
                END IF
              END DO
              IF (check_number .ne. 2) CYCLE
              ! Remove the 2 numbers from the square that contains more possibilities
              IF (cell_ij_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij_numbers
                  cell_ij_logic_number=sudoku_logic(row1,col1,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij_numbers
                  sudoku_logic(row1,col1,knumber)=0
                END DO
                sudoku_logic_values(row1,col1)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row1,col1,knumber)=temp_array(knumber)
                END DO
              END IF
              IF (cell_ij2_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij2_numbers
                  cell_ij_logic_number=sudoku_logic(row2,col2,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij2_numbers
                  sudoku_logic(row2,col2,knumber)=0
                END DO

                sudoku_logic_values(row2,col2)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row2,col2,knumber)=temp_array(knumber)
                END DO
              END IF
              IF (cell_ij3_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij3_numbers
                  cell_ij_logic_number=sudoku_logic(row11,col11,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij3_numbers
                  sudoku_logic(row11,col11,knumber)=0
                END DO
                sudoku_logic_values(row11,col11)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row11,col11,knumber)=temp_array(knumber)
                END DO
              END IF
              IF (cell_ij4_numbers .ne. 2) THEN
                temp_array=0
                temp_number=0
                DO knumber=1,cell_ij4_numbers
                  cell_ij_logic_number=sudoku_logic(row22,col22,knumber)
                  IF (temp_array3(cell_ij_logic_number) .eq. 4) CYCLE
                  temp_number=temp_number+1
                  temp_array(temp_number) = cell_ij_logic_number
                END DO
                DO knumber=1,cell_ij4_numbers
                  sudoku_logic(row22,col22,knumber)=0
                END DO
                sudoku_logic_values(row22,col22)=temp_number
                DO knumber=1,temp_number
                  sudoku_logic(row22,col22,knumber)=temp_array(knumber)
                END DO
              END IF
            END DO
	   END DO
          END DO
         END IF
        END DO
      END DO
    END DO


END SUBROUTINE Unique_rectangle_type1

!-----------------------------------------------------------------

SUBROUTINE Unique_rectangle_type2

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cell_ij_other_logic_number,cell_ij_other_numbers
  INTEGER :: check_number2,check_number4,check_square,check_value
  INTEGER :: rowk,colk,kcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cell_value,cage_cells,cage_sum_local
  INTEGER :: check1,total_permutations
  INTEGER :: j_cell,k_cell


  ! Loop over horizontal_2cages
  DO i=1,nhorizontal_cages
!    cage1h = horizontal_2cages(i,1)
!    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.9'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF ((cell_ij_numbers .lt. 2) .OR. (cell_ij_numbers .gt. 3)) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
	check_square=0
      ELSE IF (cell_ij_numbers .eq. 3) THEN
	check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
	cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
	temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1) 
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.10'
          STOP
        END IF
	IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
	IF (cell_ij2_numbers .ne. cell_ij_numbers) CYCLE
	DO jnumber=1,cell_ij2_numbers
	  cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
	  temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
        END DO
	IF (check_square .eq. 0) THEN
	  IF (check_number .ne. 2) CYCLE
	ELSE IF (check_square .eq. 1) THEN
	  IF (check_number .ne. 3) CYCLE
	END IF
	DO j=i+1,nhorizontal_cages
	 cage2h_real=horizontal_cages(j)
	 ncells2=cage_no_of_cells(cage2h_real)
         DO icell1=1,ncells2
          row11=cage_to_cells(cage2h_real,icell1,1)
          col11=cage_to_cells(cage2h_real,icell1,2)
          IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
          cage_temp1=cell_to_cage(row11,col11,1)
          cage_temp2=cell_to_cage(row11,col11,2)
          IF (cage_temp1 .eq. cage2h_real) THEN
            vertical_cage2 = cage_temp2
          ELSE IF (cage_temp2 .eq. cage2h_real) THEN
            vertical_cage2 = cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.11'
            STOP
          END IF
          IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                      (vertical_cage2 .ne. vertical_cage12)) CYCLE
          IF (sudoku(row11,col11) .ne. 0) CYCLE
          cell_ij3_numbers=sudoku_logic_values(row11,col11)
	  IF (check_square .eq. 0) THEN
	    IF (cell_ij3_numbers .ne. 3) CYCLE
	  ELSE IF (check_square .eq. 1) THEN
	    IF (cell_ij3_numbers .ne. 2) CYCLE
	  END IF
	  DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.12'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                        (vertical_cage2 .ne. vertical_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              temp_array3=temp_array2
	      cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .ne. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
	      DO knumber=1,cell_ij3_numbers
		cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
		temp_array3(cell_ij3_logic_number) = &
			temp_array3(cell_ij3_logic_number) + 1
	      END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number) + 1
              END DO
	      check_number4=0
	      check_number2=0
	      DO ivalue=1,9
		IF (temp_array3(ivalue) .eq. 4) THEN
		  check_number4=check_number4+1
		END IF
		IF (temp_array3(ivalue) .eq. 2) THEN
		  check_number2=check_number2+1
		  check_value=ivalue
		END IF
	      END DO
	      IF ((check_number4 .eq. 2) .AND. (check_number2 .eq. 1)) THEN
		! Remove the extra possibility from the row
		IF (cell_ij_numbers .eq. 3) THEN
		  ! remove from cage1h_real
		  DO kcell=1,ncells1
		    temp_array=0
		    temp_number=0
      		    rowk = cage_to_cells(cage1h_real,kcell,1)
      		    colk = cage_to_cells(cage1h_real,kcell,2)
		    IF (colk .eq. col1) CYCLE
		    IF (colk .eq. col2) CYCLE
		    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
		    DO inumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
		      IF (cell_ij_other_logic_number .eq. check_value) CYCLE
		      temp_number=temp_number+1
		      temp_array(temp_number)=cell_ij_other_logic_number
		    END DO
		    DO inumber=1,cell_ij_other_numbers
		      sudoku_logic(rowk,colk,inumber)=0
		    END DO
		    sudoku_logic_values(rowk,colk)=temp_number
		    DO inumber=1,temp_number
		      sudoku_logic(rowk,colk,inumber)=temp_array(inumber)
		    END DO
		  END DO	
		  ! check for all the possible combination for the cage sum with the
		  ! given possibility
		  cage_cells=ncells1
 		  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
	total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
	total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
	total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
	total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
	total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
	total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
	total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
	total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
		 temp_array=0
		 DO inumber=1,total_permutations
		   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
		     EXIT
		   END IF
		   check1=0
		   DO k_cell=1,cage_cells
		     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
		     IF (cell_value .eq. check_value) THEN
		       check1=1
		       EXIT
		     END IF
		   END DO
		   IF (check1 .eq. 1) THEN
		     DO j_cell=1,cage_cells
		       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
		       temp_array(cell_value)=1
		     END  DO 
		   END IF
		 END DO
		 ! Now remove the numbers that are not present in the combinations..
		 ! i.e. not present in temp_array
		 DO k_cell=1,cage_cells
		   rowk=cage_to_cells(cage1h_real,k_cell,1)
		   colk=cage_to_cells(cage1h_real,k_cell,2)
		   cell_ij_numbers=sudoku_logic_values(rowk,colk)
		   temp_number=0
		   temp_array1=0
		   DO inumber=1,cell_ij_numbers
		     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
		     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
	 	     temp_number=temp_number+1
		     temp_array1(temp_number)=cell_ij_logic_number
		   END DO
		   DO inumber=1,cell_ij_numbers
		     sudoku_logic(rowk,colk,inumber)=0
		   END DO
		   sudoku_logic_values(rowk,colk)=temp_number
		   DO inumber=1,temp_number
		     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
		   END DO	
		 END DO

		ELSE IF (cell_ij_numbers .eq. 2) THEN
		  ! remove from cage2h_real
		  DO kcell=1,ncells2
		    temp_array=0  
		    temp_number=0
		    rowk=cage_to_cells(cage2h_real,kcell,1)
		    colk=cage_to_cells(cage2h_real,kcell,2)
		    IF (colk .eq. col11) CYCLE
		    IF (colk .eq. col22) CYCLE
		    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
		    DO inumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value) CYCLE
                      temp_number=temp_number+1
                      temp_array(temp_number)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(rowk,colk,inumber)=temp_array(inumber)
                    END DO
                  END DO
                  ! check for all the possible combination for the cage sum with the
                  ! given possibility
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (cell_value .eq. check_value) THEN
                       check1=1
                       EXIT
                     END IF
                   END DO
                   IF (check1 .eq. 1) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO
		END IF
	      END IF
	     END DO
	    END DO
          END DO
        END DO
      END DO
    END DO

    ! Loop over vertical cages
  DO i=1,nvertical_cages
!    cage1h = vertical_2cages(i,1)
!    cage2h = vertical_2cages(i,2)
    cage1h_real = vertical_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.13'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF ((cell_ij_numbers .lt. 2) .OR. (cell_ij_numbers .gt. 3)) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
        check_square=0
      ELSE IF (cell_ij_numbers .eq. 3) THEN
        check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.14'
          STOP
        END IF
	IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (cell_ij2_numbers .ne. cell_ij_numbers) CYCLE
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
        END DO
        IF (check_square .eq. 0) THEN
          IF (check_number .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (check_number .ne. 3) CYCLE
        END IF
	DO j=i+1,nvertical_cages
	 cage2h_real=vertical_cages(j)
         ncells2=cage_no_of_cells(cage2h_real)
         DO icell1=1,ncells2
          row11=cage_to_cells(cage2h_real,icell1,1)
          col11=cage_to_cells(cage2h_real,icell1,2)
          IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
          cage_temp1=cell_to_cage(row11,col11,1)
          cage_temp2=cell_to_cage(row11,col11,2)
          IF (cage_temp1 .eq. cage2h_real) THEN
            horizontal_cage2 = cage_temp2
          ELSE IF (cage_temp2 .eq. cage2h_real) THEN
            horizontal_cage2 = cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.15'
            STOP
          END IF
          IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                      (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
	  IF (sudoku(row11,col11) .ne. 0) CYCLE
          cell_ij3_numbers=sudoku_logic_values(row11,col11)
          IF (check_square .eq. 0) THEN
            IF (cell_ij3_numbers .ne. 3) CYCLE
          ELSE IF (check_square .eq. 1) THEN
            IF (cell_ij3_numbers .ne. 2) CYCLE
          END IF
          DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.16'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
	      IF (sudoku(row22,col22) .ne. 0) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              temp_array3=temp_array2
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .ne. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number) + 1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number) + 1
              END DO
              check_number4=0
              check_number2=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number4=check_number4+1
                END IF
                IF (temp_array3(ivalue) .eq. 2) THEN
                  check_number2=check_number2+1
                  check_value=ivalue
                END IF
              END DO
              IF ((check_number4 .eq. 2) .AND. (check_number2 .eq. 1)) THEN
                ! Remove the extra possibility from the row
                IF (cell_ij_numbers .eq. 3) THEN
                  ! remove from cage1h_real
                  DO kcell=1,ncells1
                    temp_array=0
                    temp_number=0
                    rowk = cage_to_cells(cage1h_real,kcell,1)
                    colk = cage_to_cells(cage1h_real,kcell,2)
                    IF (rowk .eq. row1) CYCLE
                    IF (rowk .eq. row2) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value) CYCLE
                      temp_number=temp_number+1
                      temp_array(temp_number)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(rowk,colk,inumber)=temp_array(inumber)
                    END DO
                  END DO
                  ! check for all the possible combination for the cage sum with the
                  ! given possibility
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF

                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (cell_value .eq. check_value) THEN
                       check1=1
                       EXIT
                     END IF
                   END DO
                   IF (check1 .eq. 1) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                ELSE IF (cell_ij_numbers .eq. 2) THEN
                  ! remove from cage2h_real
                  DO kcell=1,ncells2
                    temp_array=0
                    temp_number=0
                    rowk=cage_to_cells(cage2h_real,kcell,1)
                    colk=cage_to_cells(cage2h_real,kcell,2)
                    IF (rowk .eq. row11) CYCLE
                    IF (rowk .eq. row22) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value) CYCLE
                      temp_number=temp_number+1
                      temp_array(temp_number)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(rowk,colk,inumber)=temp_array(inumber)
                    END DO
                  END DO

                  ! check for all the possible combination for the cage sum with the
                  ! given possibility
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (cell_value .eq. check_value) THEN
                       check1=1
                       EXIT
                     END IF
                   END DO
                   IF (check1 .eq. 1) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO
                END IF
              END IF
	     END DO
            END DO
          END DO
        END DO
      END DO
    END DO


END SUBROUTINE Unique_rectangle_type2

!------------------------------------------------------------------

SUBROUTINE Unique_rectangle_type3

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cell_ij_other_logic_number,cell_ij_other_numbers
  INTEGER :: check_number2,check_number4,check_square
  INTEGER,DIMENSION(9) :: check_value
  INTEGER :: rowk,colk,kcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2
  INTEGER :: cell_j_logic_number,cell_j_numbers
  INTEGER :: cell_other_logic_number,cell_other_numbers
  INTEGER :: check_j,col_other,row_other,jj,kk,other_cell
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cell_value,cage_cells,cage_sum_local
  INTEGER :: check1,total_permutations
  INTEGER :: j_cell,k_cell



  DO i=1,nhorizontal_cages
    cage1h_real=horizontal_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2) 
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.17'
        STOP
      END IF 
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF ((cell_ij_numbers .eq. 2) .OR. (cell_ij_numbers .eq. 4)) THEN
	IF (cell_ij_numbers .eq. 2) THEN
	  check_square=0
	ELSE IF (cell_ij_numbers .eq. 4) THEN
	  check_square=1
	END IF
        DO inumber=1,cell_ij_numbers
          cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
          temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
        END DO
        DO jcell=icell+1,ncells1
          temp_array2=temp_array1
          row2 = cage_to_cells(cage1h_real,jcell,1)
          col2 = cage_to_cells(cage1h_real,jcell,2)
          cage_temp1=cell_to_cage(row2,col2,1)
          cage_temp2=cell_to_cage(row2,col2,2)
          IF (cage_temp1 .eq. cage1h_real) THEN
            vertical_cage12 = cage_temp2
          ELSE IF (cage_temp2 .eq. cage1h_real) THEN
            vertical_cage12 = cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.18'
            STOP
          END IF
          IF (sudoku(row2,col2) .ne. 0) CYCLE
          cell_ij2_numbers=sudoku_logic_values(row2,col2)
          IF (cell_ij2_numbers .ne. cell_ij_numbers)  CYCLE
          DO jnumber=1,cell_ij2_numbers
            cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
            temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
          END DO
          check_number=0
          DO ivalue=1,9
            IF (temp_array2(ivalue) .eq. 2) THEN
              check_number=check_number+1
            END IF
          END DO
          IF (check_square .eq. 0) THEN
            IF (check_number .ne. 2) CYCLE
          ELSE IF (check_square .eq. 1) THEN
            IF (check_number .ne. 4) CYCLE
          END IF
          DO j=i+1,nhorizontal_cages
            cage2h_real=horizontal_cages(j)
            ncells2=cage_no_of_cells(cage2h_real)
            DO icell1=1,ncells2
              row11=cage_to_cells(cage2h_real,icell1,1)
              col11=cage_to_cells(cage2h_real,icell1,2)
              IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
              cage_temp1=cell_to_cage(row11,col11,1)
              cage_temp2=cell_to_cage(row11,col11,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.19'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                      (vertical_cage2 .ne. vertical_cage12)) CYCLE
              IF (sudoku(row11,col11) .ne. 0) CYCLE
              cell_ij3_numbers=sudoku_logic_values(row11,col11)
              IF (check_square .eq. 0) THEN
                IF (cell_ij3_numbers .ne. 4) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij3_numbers .ne. 2) CYCLE
              END IF
              DO jcell1=icell1+1,ncells2
                row22=cage_to_cells(cage2h_real,jcell1,1)
                col22=cage_to_cells(cage2h_real,jcell1,2)
                IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
                ! We have 2 cells in cage2h_real that forms a rectangle
                !  with the 2 cells in cage1h_real.. check if they form
                !  a unique rectangle
                cage_temp1=cell_to_cage(row22,col22,1)
                cage_temp2=cell_to_cage(row22,col22,2)
                IF (cage_temp1 .eq. cage2h_real) THEN
                  vertical_cage2 = cage_temp2
                ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                  vertical_cage2 = cage_temp1
                ELSE
                  write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.20'
                  STOP
                END IF
                IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                        (vertical_cage2 .ne. vertical_cage12)) CYCLE
                IF (sudoku(row22,col22) .ne. 0) CYCLE
                temp_array3=temp_array2
                cell_ij4_numbers=sudoku_logic_values(row22,col22)
                IF (check_square .eq. 0) THEN
                  IF (cell_ij4_numbers .ne. 4) CYCLE
                ELSE IF (check_square .eq. 1) THEN
                  IF (cell_ij4_numbers .ne. 2) CYCLE
                END IF
                DO knumber=1,cell_ij3_numbers
                  cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                  temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number) + 1
                END DO
                DO knumber=1,cell_ij4_numbers
                  cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                  temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number) + 1
                END DO
                check_number4=0
                check_number2=0
		check_value=0
                DO ivalue=1,9
                  IF (temp_array3(ivalue) .eq. 4) THEN
                    check_number4=check_number4+1
                  END IF
                  IF (temp_array3(ivalue) .eq. 2) THEN
                    check_number2=check_number2+1
                    check_value(check_number2)=ivalue
                  END IF
                END DO
		IF ((check_number4 .eq. 2) .AND. (check_number2 .eq. 2)) THEN
		  ! Find NAKED PAIR with the two extra possibilities in the row..
		  ! Remove the extra possibilities from row
		  IF (cell_ij_numbers .eq. 4) THEN
		    jj=check_value(1)
		    kk=check_value(2)
jloop:		    DO kcell=1,ncells1
		      check_j=0
		      rowk=cage_to_cells(cage1h_real,kcell,1)
		      colk=cage_to_cells(cage1h_real,kcell,2)
		      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col1) CYCLE
                      IF (colk .eq. col2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
		      IF (cell_j_numbers .ne. 2) CYCLE
		      DO jnumber=1,cell_j_numbers
			cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
			IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
			  check_j=1
			ELSE
			  CYCLE jloop
		        END IF
		      END DO
		      IF (check_j .eq. 1) THEN
			DO other_cell=1,ncells1
			  temp_number=0
			  temp_array=0
			  row_other=cage_to_cells(cage1h_real,other_cell,1)
			  col_other=cage_to_cells(cage1h_real,other_cell,2)
			  IF (sudoku(row_other,col_other) .ne. 0) CYCLE
			  IF (col_other .eq. col1) CYCLE
			  IF (col_other .eq. col2) CYCLE
			  IF (col_other .eq. colk) CYCLE
			  cell_other_numbers=sudoku_logic_values(row_other,col_other)
			  DO inumber=1,cell_other_numbers
			    cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
			    IF ((cell_other_logic_number .eq. jj) .OR. &
					(cell_other_logic_number .eq. kk)) CYCLE
			    temp_number=temp_number+1
			    temp_array(temp_number)=cell_other_logic_number
			  END DO
			  DO inumber=1,sudoku_logic_values(row_other,col_other)
			    sudoku_logic(row_other,col_other,inumber)=0
			  END DO
			  sudoku_logic_values(row_other,col_other)=temp_number
			  DO inumber=1,temp_number
			    sudoku_logic(row_other,col_other,inumber) = &
					temp_array(inumber)
			  END DO
			END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

		      END IF
		    END DO jloop
		  ELSE IF (cell_ij_numbers .eq. 2) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop1:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col11) CYCLE
                      IF (colk .eq. col22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop1
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells2
			  temp_number=0
			  temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (col_other .eq. col11) CYCLE
                          IF (col_other .eq. col22) CYCLE
                          IF (col_other .eq. colk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop1
		  END IF
		END IF
	      END DO
            END DO
          END DO
        END DO
       END IF
      END DO
    END DO
 
  ! loop over vertical cages
  DO i=1,nvertical_cages
    cage1h_real=vertical_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.21'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF ((cell_ij_numbers .eq. 2) .OR. (cell_ij_numbers .eq. 4)) THEN
        IF (cell_ij_numbers .eq. 2) THEN
          check_square=0
        ELSE IF (cell_ij_numbers .eq. 4) THEN
          check_square=1
        END IF
        DO inumber=1,cell_ij_numbers
          cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
          temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
        END DO
        DO jcell=icell+1,ncells1
          temp_array2=temp_array1
          row2 = cage_to_cells(cage1h_real,jcell,1)
          col2 = cage_to_cells(cage1h_real,jcell,2)
          cage_temp1=cell_to_cage(row2,col2,1)
          cage_temp2=cell_to_cage(row2,col2,2)
          IF (cage_temp1 .eq. cage1h_real) THEN
            horizontal_cage12 = cage_temp2
          ELSE IF (cage_temp2 .eq. cage1h_real) THEN
            horizontal_cage12 = cage_temp1
          ELSE
            write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.22'
            STOP
          END IF
          IF (sudoku(row2,col2) .ne. 0) CYCLE
          cell_ij2_numbers=sudoku_logic_values(row2,col2)
          IF (cell_ij2_numbers .ne. cell_ij_numbers)  CYCLE
          DO jnumber=1,cell_ij2_numbers
            cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
            temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
          END DO
          check_number=0
          DO ivalue=1,9
            IF (temp_array2(ivalue) .eq. 2) THEN
              check_number=check_number+1
            END IF
          END DO
          IF (check_square .eq. 0) THEN
            IF (check_number .ne. 2) CYCLE
          ELSE IF (check_square .eq. 1) THEN
            IF (check_number .ne. 4) CYCLE
          END IF
          DO j=i+1,nvertical_cages
            cage2h_real=vertical_cages(j)
            ncells2=cage_no_of_cells(cage2h_real)
            DO icell1=1,ncells2
              row11=cage_to_cells(cage2h_real,icell1,1)
              col11=cage_to_cells(cage2h_real,icell1,2)
              IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
              cage_temp1=cell_to_cage(row11,col11,1)
              cage_temp2=cell_to_cage(row11,col11,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
               horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.23'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                      (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
              IF (sudoku(row11,col11) .ne. 0) CYCLE
              cell_ij3_numbers=sudoku_logic_values(row11,col11)
              IF (check_square .eq. 0) THEN
                IF (cell_ij3_numbers .ne. 4) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij3_numbers .ne. 2) CYCLE
              END IF
              DO jcell1=icell1+1,ncells2
                row22=cage_to_cells(cage2h_real,jcell1,1)
                col22=cage_to_cells(cage2h_real,jcell1,2)
                IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
                ! We have 2 cells in cage2h_real that forms a rectangle
                !  with the 2 cells in cage1h_real.. check if they form
                !  a unique rectangle
                cage_temp1=cell_to_cage(row22,col22,1)
                cage_temp2=cell_to_cage(row22,col22,2)
                IF (cage_temp1 .eq. cage2h_real) THEN
                  horizontal_cage2 = cage_temp2
                ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                  horizontal_cage2 = cage_temp1
                ELSE
                  write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.24'
                  STOP
                END IF
                IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
                IF (sudoku(row22,col22) .ne. 0) CYCLE
                temp_array3=temp_array2
                cell_ij4_numbers=sudoku_logic_values(row22,col22)
                IF (check_square .eq. 0) THEN
                  IF (cell_ij4_numbers .ne. 4) CYCLE
                ELSE IF (check_square .eq. 1) THEN
                  IF (cell_ij4_numbers .ne. 2) CYCLE
                END IF
                DO knumber=1,cell_ij3_numbers
                  cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                  temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number) + 1
                END DO
                DO knumber=1,cell_ij4_numbers
                  cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                  temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number) + 1
                END DO
                check_number4=0
                check_number2=0
                DO ivalue=1,9
                  IF (temp_array3(ivalue) .eq. 4) THEN
                    check_number4=check_number4+1
                  END IF
                  IF (temp_array3(ivalue) .eq. 2) THEN
                    check_number2=check_number2+1
                    check_value(check_number2)=ivalue
                  END IF
                END DO
                IF ((check_number4 .eq. 2) .AND. (check_number2 .eq. 2)) THEN
                  ! Find NAKED PAIR with the two extra possibilities in the row..
                  ! Remove the extra possibilities from row
                  IF (cell_ij_numbers .eq. 4) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop2:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row1) CYCLE
                      IF (rowk .eq. row2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop2
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells1
			  temp_number=0
			  temp_array=0
                          row_other=cage_to_cells(cage1h_real,other_cell,1)
                          col_other=cage_to_cells(cage1h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row1) CYCLE
                          IF (row_other .eq. row2) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop2
                  ELSE IF (cell_ij_numbers .eq. 2) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop3:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row11) CYCLE
                      IF (rowk .eq. row22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop3
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells2
			  temp_number=0
			  temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row11) CYCLE
                          IF (row_other .eq. row22) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop3
                  END IF
                END IF
              END DO
            END DO
          END DO
        END DO
       END IF
      END DO
    END DO


END SUBROUTINE Unique_rectangle_type3

!------------------------------------------------------------------

SUBROUTINE Unique_rectangle_type3and2

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cell_ij_other_logic_number,cell_ij_other_numbers
  INTEGER :: check_number2,check_number4,check_square
  INTEGER,DIMENSION(9) :: check_value
  INTEGER :: rowk,colk,kcell,lcell,mcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2
  INTEGER :: cell_j_logic_number,cell_j_numbers
  INTEGER :: cell_other_logic_number,cell_other_numbers
  INTEGER :: check_j,col_other,row_other,jj,kk,other_cell
  INTEGER :: cell_l_logic_number,cell_l_numbers,check_l,l_cell,lnumber
  INTEGER :: mm,cell_k_logic_number,cell_k_numbers,check_k
  INTEGER :: check_value_numbers,coll,rowl,colm,rowm,ll,check_number1
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cell_value,cage_cells,cage_sum_local
  INTEGER :: check1,total_permutations
  INTEGER :: j_cell,k_cell



  DO i=1,nhorizontal_cages
    cage1h_real=horizontal_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.25'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
        check_square=0
      ELSE IF (cell_ij_numbers .gt. 2) THEN
        check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.26'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
	IF (check_square .eq. 0) THEN
	  IF (cell_ij2_numbers .ne. 2) CYCLE
	ELSE IF (check_square .eq. 1) THEN
	  IF (cell_ij2_numbers .lt. 3) CYCLE
	END IF
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        check_number1=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
	  IF (temp_array2(ivalue) .eq. 1) THEN
	    check_number1=check_number1+1
	  END IF
        END DO
	IF (check_square .eq. 0) THEN
	  IF (check_number .ne. 2) CYCLE
	ELSE IF (check_square .eq. 1) THEN
	  IF (check_number .lt. 2) CYCLE
	END IF
        DO j=i+1,nhorizontal_cages
          cage2h_real=horizontal_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.27'
              STOP
            END IF
            IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                    (vertical_cage2 .ne. vertical_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (check_square .eq. 0) THEN
              IF (cell_ij3_numbers .lt. 3) CYCLE
            ELSE IF (check_square .eq. 1) THEN
              IF (cell_ij3_numbers .ne. 2) CYCLE
            END IF
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.28'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                      (vertical_cage2 .ne. vertical_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              temp_array3=temp_array2
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .lt. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                      temp_array3(cell_ij3_logic_number) + 1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                      temp_array3(cell_ij4_logic_number) + 1
              END DO
              check_number1=0
              check_number2=0
              check_number4=0
	      check_value_numbers=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 1) THEN
                  check_number1=check_number1+1
                END IF
                IF (temp_array3(ivalue) .eq. 2) THEN
                  check_number2=check_number2+1
                END IF
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number4=check_number4+1
                END IF
		IF ((temp_array3(ivalue) .ge. 1) .AND. (temp_array3(ivalue) .lt. 3)) THEN
                  check_value_numbers=check_value_numbers+1
                  check_value(check_value_numbers) = ivalue
                END IF
              END DO
	      IF (check_number4 .eq. 2) THEN
		IF (cell_ij_numbers .gt. 2) THEN
		  ! If it is a duple
		  IF (check_value_numbers .eq. 2) THEN
		    jj=check_value(1)
		    kk=check_value(2)
jloop:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col1) CYCLE
                      IF (colk .eq. col2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells1
			  temp_number=0
			  temp_array=0
                          row_other=cage_to_cells(cage1h_real,other_cell,1)
                          col_other=cage_to_cells(cage1h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (col_other .eq. col1) CYCLE
                          IF (col_other .eq. col2) CYCLE
                          IF (col_other .eq. colk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop
		  ELSE IF (check_value_numbers .eq. 3) THEN
		    ! We have a triple
		    jj=check_value(1)
		    kk=check_value(2)
		    ll=check_value(3)
jloop1:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col1) CYCLE
                      IF (colk .eq. col2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 3) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
				.OR. (cell_j_logic_number .eq. ll)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop1
                        END IF
                      END DO
kloop1:		      DO lcell=kcell+1,ncells1
		        rowl=cage_to_cells(cage1h_real,lcell,1)
			coll=cage_to_cells(cage1h_real,lcell,2)
			IF (sudoku(rowl,coll) .ne. 0) CYCLE
			IF (coll .eq. col1) CYCLE
			IF (coll .eq. col2) CYCLE
			IF (coll .eq. colk) CYCLE
			check_k=0
			cell_k_numbers=sudoku_logic_values(rowl,coll)
			IF (cell_k_numbers .gt. 3) CYCLE
			DO knumber=1,cell_k_numbers
			  cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
			  IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
				.OR. (cell_k_logic_number .eq. ll)) THEN
			    check_k=1
			  ELSE
			    CYCLE kloop1
			  END IF
			END DO
			IF ((check_j .eq. 1) .AND. (check_k .eq. 1)) THEN
                         DO other_cell=1,ncells1
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage1h_real,other_cell,1)
                          col_other=cage_to_cells(cage1h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (col_other .eq. col1) CYCLE
                          IF (col_other .eq. col2) CYCLE
                          IF (col_other .eq. colk) CYCLE
                          IF (col_other .eq. coll) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
					(cell_other_logic_number .eq. ll)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) &
				.OR. (cell_value .eq. ll)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 3) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
		     END DO kloop1
                    END DO jloop1
		  ELSE IF (check_value_numbers .eq. 4) THEN
		     !We have a quad
		     jj=check_value(1)
		     kk=check_value(2)
		     ll=check_value(3)
		     mm=check_value(4)
jloop2:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col1) CYCLE
                      IF (colk .eq. col2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 4) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll) .OR. &
				     (cell_j_logic_number .eq. mm)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop2
                        END IF
                      END DO
kloop2:               DO lcell=kcell+1,ncells1
                        rowl=cage_to_cells(cage1h_real,lcell,1)
                        coll=cage_to_cells(cage1h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (coll .eq. col1) CYCLE
                        IF (coll .eq. col2) CYCLE
                        IF (coll .eq. colk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 4) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll) .OR. &
				     (cell_k_logic_number .eq. mm)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop2
                          END IF
                        END DO
lloop2:                 DO mcell=lcell+1,ncells1
                          rowm=cage_to_cells(cage1h_real,mcell,1)
                          colm=cage_to_cells(cage1h_real,mcell,2)
                          IF (sudoku(rowm,colm) .ne. 0) CYCLE
                          IF (colm .eq. col1) CYCLE
                          IF (colm .eq. col2) CYCLE
                          IF (colm .eq. colk) CYCLE
                          IF (colm .eq. coll) CYCLE
                          check_l=0
                          cell_l_numbers=sudoku_logic_values(rowm,colm)
                          IF (cell_l_numbers .gt. 4) CYCLE
                          DO lnumber=1,cell_l_numbers
                            cell_l_logic_number=sudoku_logic(rowm,colm,lnumber)
                            IF ((cell_l_logic_number .eq. jj) .OR. (cell_l_logic_number .eq. kk) &
                                .OR. (cell_l_logic_number .eq. ll) .OR. &
                                     (cell_l_logic_number .eq. mm)) THEN
                              check_l=1
                            ELSE
                              CYCLE lloop2
                            END IF
                          END DO
                          IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1)) THEN
                           DO other_cell=1,ncells1
                            temp_number=0
                            temp_array=0
                            row_other=cage_to_cells(cage1h_real,other_cell,1)
                            col_other=cage_to_cells(cage1h_real,other_cell,2)
                            IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                            IF (col_other .eq. col1) CYCLE
                            IF (col_other .eq. col2) CYCLE
                            IF (col_other .eq. colk) CYCLE
                            IF (col_other .eq. coll) CYCLE
                            IF (col_other .eq. colm) CYCLE
                            cell_other_numbers=sudoku_logic_values(row_other,col_other)
                            DO inumber=1,cell_other_numbers
                              cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                              IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll) .OR. &
					(cell_other_logic_number .eq. mm)) CYCLE
                              temp_number=temp_number+1
                              temp_array(temp_number)=cell_other_logic_number
                            END DO
                            DO inumber=1,sudoku_logic_values(row_other,col_other)
                              sudoku_logic(row_other,col_other,inumber)=0
                            END DO
                            sudoku_logic_values(row_other,col_other)=temp_number
                            DO inumber=1,temp_number
                              sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                            END DO
                           END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) .OR. &
				(cell_value .eq. ll) .OR. (cell_value .eq. mm)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 4) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                          END IF
		        END DO lloop2
                     END DO kloop2
                    END DO jloop2
		  END IF
		ELSE IF (cell_ij_numbers .eq. 2) THEN
                  ! If it is a duple
                  IF (check_value_numbers .eq. 2) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop3:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col11) CYCLE
                      IF (colk .eq. col22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop3
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells2
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (col_other .eq. col11) CYCLE
                          IF (col_other .eq. col22) CYCLE
                          IF (col_other .eq. colk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop3
                  ELSE IF (check_value_numbers .eq. 3) THEN
                    ! We have a triple
                    jj=check_value(1)
                    kk=check_value(2)
                    ll=check_value(3)
jloop4:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col11) CYCLE
                      IF (colk .eq. col22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 3) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop4
                        END IF
                      END DO
kloop4:               DO lcell=kcell+1,ncells2
                        rowl=cage_to_cells(cage2h_real,lcell,1)
                        coll=cage_to_cells(cage2h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (coll .eq. col11) CYCLE
                        IF (coll .eq. col22) CYCLE
                        IF (coll .eq. colk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 3) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop4
                          END IF
                        END DO
                        IF ((check_j .eq. 1) .AND. (check_k .eq. 1)) THEN
                        DO other_cell=1,ncells2
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (col_other .eq. col11) CYCLE
                          IF (col_other .eq. col22) CYCLE
                          IF (col_other .eq. colk) CYCLE
                          IF (col_other .eq. coll) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) &
                                .OR. (cell_value .eq. ll)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 3) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                     END DO kloop4
                    END DO jloop4
                  ELSE IF (check_value_numbers .eq. 4) THEN
                     !We have a quad
                     jj=check_value(1)
                     kk=check_value(2)
                     ll=check_value(3)
                     mm=check_value(4)
jloop5:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (colk .eq. col11) CYCLE
                      IF (colk .eq. col22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 4) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll) .OR. &
                                     (cell_j_logic_number .eq. mm)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop5
                        END IF
                      END DO
kloop5:               DO lcell=kcell+1,ncells2
                        rowl=cage_to_cells(cage2h_real,lcell,1)
                        coll=cage_to_cells(cage2h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (coll .eq. col11) CYCLE
                        IF (coll .eq. col22) CYCLE
                        IF (coll .eq. colk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 4) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll) .OR. &
                                     (cell_k_logic_number .eq. mm)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop5
                          END IF
                        END DO
lloop5:                 DO mcell=lcell+1,ncells2
                          rowm=cage_to_cells(cage2h_real,mcell,1)
                          colm=cage_to_cells(cage2h_real,mcell,2)
                          IF (sudoku(rowm,colm) .ne. 0) CYCLE
                          IF (colm .eq. col11) CYCLE
                          IF (colm .eq. col22) CYCLE
                          IF (colm .eq. colk) CYCLE
                          IF (colm .eq. coll) CYCLE
                          check_l=0
                          cell_l_numbers=sudoku_logic_values(rowm,colm)
                          IF (cell_l_numbers .gt. 4) CYCLE
                          DO lnumber=1,cell_l_numbers
                            cell_l_logic_number=sudoku_logic(rowm,colm,lnumber)
                            IF ((cell_l_logic_number .eq. jj) .OR. (cell_l_logic_number .eq. kk) &
                                .OR. (cell_l_logic_number .eq. ll) .OR. &
                                     (cell_l_logic_number .eq. mm)) THEN
                              check_l=1
                            ELSE
                              CYCLE lloop5
                            END IF
                          END DO
                          IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1)) THEN
                           DO other_cell=1,ncells2
                            temp_number=0
                            temp_array=0
                            row_other=cage_to_cells(cage2h_real,other_cell,1)
                            col_other=cage_to_cells(cage2h_real,other_cell,2)
                            IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                            IF (col_other .eq. col11) CYCLE
                            IF (col_other .eq. col22) CYCLE
                            IF (col_other .eq. colk) CYCLE
                            IF (col_other .eq. coll) CYCLE
                            IF (col_other .eq. colm) CYCLE
                            cell_other_numbers=sudoku_logic_values(row_other,col_other)
                            DO inumber=1,cell_other_numbers
                              cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                              IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll) .OR. &
                                        (cell_other_logic_number .eq. mm)) CYCLE
                              temp_number=temp_number+1
                              temp_array(temp_number)=cell_other_logic_number
                            END DO
                            DO inumber=1,sudoku_logic_values(row_other,col_other)
                              sudoku_logic(row_other,col_other,inumber)=0
                            END DO
                            sudoku_logic_values(row_other,col_other)=temp_number
                            DO inumber=1,temp_number
                              sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                            END DO
                           END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) .OR. &
                                (cell_value .eq. ll) .OR. (cell_value .eq. mm)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 4) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                          END IF
                        END DO lloop5
                     END DO kloop5
                    END DO jloop5
                  END IF
		 END IF
	        END IF
	       END DO
	      END DO
	     END DO
	    END DO
	   END DO
	  END DO

  ! Loop over vertical cages
  DO i=1,nvertical_cages
    cage1h_real=vertical_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.29'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
        check_square=0
      ELSE IF (cell_ij_numbers .gt. 2) THEN
        check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.30'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (check_square .eq. 0) THEN
          IF (cell_ij2_numbers .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (cell_ij2_numbers .lt. 3) CYCLE
        END IF
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        check_number1=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
          IF (temp_array2(ivalue) .eq. 1) THEN
            check_number1=check_number1+1
          END IF
        END DO
        IF (check_square .eq. 0) THEN
          IF (check_number .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (check_number .lt. 2) CYCLE
        END IF
        DO j=i+1,nvertical_cages
          cage2h_real=vertical_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.31'
              STOP
            END IF
            IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                    (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (check_square .eq. 0) THEN
              IF (cell_ij3_numbers .lt. 3) CYCLE
            ELSE IF (check_square .eq. 1) THEN
              IF (cell_ij3_numbers .ne. 2) CYCLE
            END IF
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.32'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                      (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              temp_array3=temp_array2
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .lt. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                      temp_array3(cell_ij3_logic_number) + 1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                      temp_array3(cell_ij4_logic_number) + 1
              END DO
              check_number1=0
              check_number2=0
              check_number4=0
              check_value_numbers=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 1) THEN
                  check_number1=check_number1+1
                END IF
                IF (temp_array3(ivalue) .eq. 2) THEN
                  check_number2=check_number2+1
                END IF
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number4=check_number4+1
                END IF
                IF ((temp_array3(ivalue) .ge. 1) .AND. (temp_array3(ivalue) .lt. 3)) THEN
                  check_value_numbers=check_value_numbers+1
                  check_value(check_value_numbers) = ivalue
                END IF
              END DO
              IF (check_number4 .eq. 2) THEN
                IF (cell_ij_numbers .gt. 2) THEN
                  ! If it is a duple
                  IF (check_value_numbers .eq. 2) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop6:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row1) CYCLE
                      IF (rowk .eq. row2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop6
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells1
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage1h_real,other_cell,1)
                          col_other=cage_to_cells(cage1h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row1) CYCLE
                          IF (row_other .eq. row2) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop6
                  ELSE IF (check_value_numbers .eq. 3) THEN
                    ! We have a triple
                    jj=check_value(1)
                    kk=check_value(2)
                    ll=check_value(3)
jloop7:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row1) CYCLE
                      IF (rowk .eq. row2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 3) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop7
                        END IF
                      END DO
kloop7:               DO lcell=kcell+1,ncells1
                        rowl=cage_to_cells(cage1h_real,lcell,1)
                        coll=cage_to_cells(cage1h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (rowl .eq. row1) CYCLE
                        IF (rowl .eq. row2) CYCLE
                        IF (rowl .eq. rowk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 3) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop7
                          END IF
                        END DO
                        IF ((check_j .eq. 1) .AND. (check_k .eq. 1)) THEN
                         DO other_cell=1,ncells1
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage1h_real,other_cell,1)
                          col_other=cage_to_cells(cage1h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row1) CYCLE
                          IF (row_other .eq. row2) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          IF (row_other .eq. rowl) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) &
                                .OR. (cell_value .eq. ll)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 3) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                     END DO kloop7
                    END DO jloop7
                  ELSE IF (check_value_numbers .eq. 4) THEN
                     !We have a quad
                     jj=check_value(1)
                     kk=check_value(2)
                     ll=check_value(3)
                     mm=check_value(4)
jloop8:              DO kcell=1,ncells1
                      check_j=0
                      rowk=cage_to_cells(cage1h_real,kcell,1)
                      colk=cage_to_cells(cage1h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row1) CYCLE
                      IF (rowk .eq. row2) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 4) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll) .OR. &
                                     (cell_j_logic_number .eq. mm)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop8
                        END IF
                      END DO
kloop8:               DO lcell=kcell+1,ncells1
                        rowl=cage_to_cells(cage1h_real,lcell,1)
                        coll=cage_to_cells(cage1h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (rowl .eq. row1) CYCLE
                        IF (rowl .eq. row2) CYCLE
                        IF (rowl .eq. rowk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 4) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll) .OR. &
                                     (cell_k_logic_number .eq. mm)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop8
                          END IF
                        END DO
lloop8:                 DO mcell=lcell+1,ncells1
                          rowm=cage_to_cells(cage1h_real,mcell,1)
                          colm=cage_to_cells(cage1h_real,mcell,2)
                          IF (sudoku(rowm,colm) .ne. 0) CYCLE
                          IF (rowm .eq. row1) CYCLE
                          IF (rowm .eq. row2) CYCLE
                          IF (rowm .eq. rowk) CYCLE
                          IF (rowm .eq. rowl) CYCLE
                          check_l=0
                          cell_l_numbers=sudoku_logic_values(rowm,colm)
                          IF (cell_l_numbers .gt. 4) CYCLE
                          DO lnumber=1,cell_l_numbers
                            cell_l_logic_number=sudoku_logic(rowm,colm,lnumber)
                            IF ((cell_l_logic_number .eq. jj) .OR. (cell_l_logic_number .eq. kk) &
                                .OR. (cell_l_logic_number .eq. ll) .OR. &
                                     (cell_l_logic_number .eq. mm)) THEN
                              check_l=1
                            ELSE
                              CYCLE lloop8
                            END IF
                          END DO
                          IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1)) THEN
                           DO other_cell=1,ncells1
                            temp_number=0
                            temp_array=0
                            row_other=cage_to_cells(cage1h_real,other_cell,1)
                            col_other=cage_to_cells(cage1h_real,other_cell,2)
                            IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                            IF (row_other .eq. row1) CYCLE
                            IF (row_other .eq. row2) CYCLE
                            IF (row_other .eq. rowk) CYCLE
                            IF (row_other .eq. rowl) CYCLE
                            IF (row_other .eq. rowm) CYCLE
                            cell_other_numbers=sudoku_logic_values(row_other,col_other)
                            DO inumber=1,cell_other_numbers
                              cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                              IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll) .OR. &
                                        (cell_other_logic_number .eq. mm)) CYCLE
                              temp_number=temp_number+1
                              temp_array(temp_number)=cell_other_logic_number
                            END DO
                            DO inumber=1,sudoku_logic_values(row_other,col_other)
                              sudoku_logic(row_other,col_other,inumber)=0
                            END DO
                            sudoku_logic_values(row_other,col_other)=temp_number
                            DO inumber=1,temp_number
                              sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                            END DO
                           END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) .OR. &
                                (cell_value .eq. ll) .OR. (cell_value .eq. mm)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 4) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage1h_real,k_cell,1)
                   colk=cage_to_cells(cage1h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                          END IF
                        END DO lloop8
                     END DO kloop8
                    END DO jloop8
                  END IF
                ELSE IF (cell_ij_numbers .eq. 2) THEN
                  ! If it is a duple
                  IF (check_value_numbers .eq. 2) THEN
                    jj=check_value(1)
                    kk=check_value(2)
jloop9:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row11) CYCLE
                      IF (rowk .eq. row22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .ne. 2) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop9
                        END IF
                      END DO
                      IF (check_j .eq. 1) THEN
                        DO other_cell=1,ncells2
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row11) CYCLE
                          IF (row_other .eq. row22) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 2) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                    END DO jloop9
                  ELSE IF (check_value_numbers .eq. 3) THEN
                    ! We have a triple
                    jj=check_value(1)
                    kk=check_value(2)
                    ll=check_value(3)
jloop10:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row11) CYCLE
                      IF (rowk .eq. row22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 3) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop10
                        END IF
                      END DO
kloop10:               DO lcell=kcell+1,ncells2
                        rowl=cage_to_cells(cage2h_real,lcell,1)
                        coll=cage_to_cells(cage2h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (rowl .eq. row11) CYCLE
                        IF (rowl .eq. row22) CYCLE
                        IF (rowl .eq. rowk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 3) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop10
                          END IF
                        END DO
                        IF ((check_j .eq. 1) .AND. (check_k .eq. 1)) THEN
                        DO other_cell=1,ncells2
                          temp_number=0
                          temp_array=0
                          row_other=cage_to_cells(cage2h_real,other_cell,1)
                          col_other=cage_to_cells(cage2h_real,other_cell,2)
                          IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                          IF (row_other .eq. row11) CYCLE
                          IF (row_other .eq. row22) CYCLE
                          IF (row_other .eq. rowk) CYCLE
                          IF (row_other .eq. rowl) CYCLE
                          cell_other_numbers=sudoku_logic_values(row_other,col_other)
                          DO inumber=1,cell_other_numbers
                            cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                            IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll)) CYCLE
                            temp_number=temp_number+1
                            temp_array(temp_number)=cell_other_logic_number
                          END DO
                          DO inumber=1,sudoku_logic_values(row_other,col_other)
                            sudoku_logic(row_other,col_other,inumber)=0
                          END DO
                          sudoku_logic_values(row_other,col_other)=temp_number
                          DO inumber=1,temp_number
                            sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                          END DO
                        END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) &
                                .OR. (cell_value .eq. ll)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 3) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                      END IF
                     END DO kloop10
                    END DO jloop10
                  ELSE IF (check_value_numbers .eq. 4) THEN
                     !We have a quad
                     jj=check_value(1)
                     kk=check_value(2)
                     ll=check_value(3)
                     mm=check_value(4)
jloop11:              DO kcell=1,ncells2
                      check_j=0
                      rowk=cage_to_cells(cage2h_real,kcell,1)
                      colk=cage_to_cells(cage2h_real,kcell,2)
                      IF (sudoku(rowk,colk) .ne. 0) CYCLE
                      IF (rowk .eq. row11) CYCLE
                      IF (rowk .eq. row22) CYCLE
                      cell_j_numbers=sudoku_logic_values(rowk,colk)
                      IF (cell_j_numbers .gt. 4) CYCLE
                      DO jnumber=1,cell_j_numbers
                        cell_j_logic_number=sudoku_logic(rowk,colk,jnumber)
                        IF ((cell_j_logic_number .eq. jj) .OR. (cell_j_logic_number .eq. kk) &
                                .OR. (cell_j_logic_number .eq. ll) .OR. &
                                     (cell_j_logic_number .eq. mm)) THEN
                          check_j=1
                        ELSE
                          CYCLE jloop11
                        END IF
                      END DO
kloop11:               DO lcell=kcell+1,ncells2
                        rowl=cage_to_cells(cage2h_real,lcell,1)
                        coll=cage_to_cells(cage2h_real,lcell,2)
                        IF (sudoku(rowl,coll) .ne. 0) CYCLE
                        IF (rowl .eq. row11) CYCLE
                        IF (rowl .eq. row22) CYCLE
                        IF (rowl .eq. rowk) CYCLE
                        check_k=0
                        cell_k_numbers=sudoku_logic_values(rowl,coll)
                        IF (cell_k_numbers .gt. 4) CYCLE
                        DO knumber=1,cell_k_numbers
                          cell_k_logic_number=sudoku_logic(rowl,coll,knumber)
                          IF ((cell_k_logic_number .eq. jj) .OR. (cell_k_logic_number .eq. kk) &
                                .OR. (cell_k_logic_number .eq. ll) .OR. &
                                     (cell_k_logic_number .eq. mm)) THEN
                            check_k=1
                          ELSE
                            CYCLE kloop11
                          END IF
                        END DO
lloop11:                 DO mcell=lcell+1,ncells2
                          rowm=cage_to_cells(cage2h_real,mcell,1)
                          colm=cage_to_cells(cage2h_real,mcell,2)
                          IF (sudoku(rowm,colm) .ne. 0) CYCLE
                          IF (rowm .eq. row11) CYCLE
                          IF (rowm .eq. row22) CYCLE
                          IF (rowm .eq. rowk) CYCLE
                          IF (rowm .eq. rowl) CYCLE
                          check_l=0
                          cell_l_numbers=sudoku_logic_values(rowm,colm)
                          IF (cell_l_numbers .gt. 4) CYCLE
                          DO lnumber=1,cell_l_numbers
                            cell_l_logic_number=sudoku_logic(rowm,colm,lnumber)
                            IF ((cell_l_logic_number .eq. jj) .OR. (cell_l_logic_number .eq. kk) &
                                .OR. (cell_l_logic_number .eq. ll) .OR. &
                                     (cell_l_logic_number .eq. mm)) THEN
                              check_l=1
                            ELSE
                              CYCLE lloop11
                            END IF
                          END DO
                          IF ((check_j .eq. 1) .AND. (check_k .eq. 1) .AND. (check_l .eq. 1)) THEN
                           DO other_cell=1,ncells2
                            temp_number=0
                            temp_array=0
                            row_other=cage_to_cells(cage2h_real,other_cell,1)
                            col_other=cage_to_cells(cage2h_real,other_cell,2)
                            IF (sudoku(row_other,col_other) .ne. 0) CYCLE
                            IF (row_other .eq. row11) CYCLE
                            IF (row_other .eq. row22) CYCLE
                            IF (row_other .eq. rowk) CYCLE
                            IF (row_other .eq. rowl) CYCLE
                            IF (row_other .eq. rowm) CYCLE
                            cell_other_numbers=sudoku_logic_values(row_other,col_other)
                            DO inumber=1,cell_other_numbers
                              cell_other_logic_number=sudoku_logic(row_other,col_other,inumber)
                              IF ((cell_other_logic_number .eq. jj) .OR. &
                                        (cell_other_logic_number .eq. kk) .OR. &
                                        (cell_other_logic_number .eq. ll) .OR. &
                                        (cell_other_logic_number .eq. mm)) CYCLE
                              temp_number=temp_number+1
                              temp_array(temp_number)=cell_other_logic_number
                            END DO
                            DO inumber=1,sudoku_logic_values(row_other,col_other)
                              sudoku_logic(row_other,col_other,inumber)=0
                            END DO
                            sudoku_logic_values(row_other,col_other)=temp_number
                            DO inumber=1,temp_number
                              sudoku_logic(row_other,col_other,inumber) = &
                                        temp_array(inumber)
                            END DO
                           END DO
! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 temp_array=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF ((cell_value .eq. jj) .OR. (cell_value .eq. kk) .OR. &
                                (cell_value .eq. ll) .OR. (cell_value .eq. mm)) THEN
                       check1=check1+1
                     END IF
                   END DO
                   IF (check1 .eq. 4) THEN
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array(cell_value)=1
                     END  DO
                   END IF
                 END DO
                 ! Now remove the numbers that are not present in the combinations..
                 ! i.e. not present in temp_array
                 DO k_cell=1,cage_cells
                   rowk=cage_to_cells(cage2h_real,k_cell,1)
                   colk=cage_to_cells(cage2h_real,k_cell,2)
                   cell_ij_numbers=sudoku_logic_values(rowk,colk)
                   temp_number=0
                   temp_array1=0
                   DO inumber=1,cell_ij_numbers
                     cell_ij_logic_number=sudoku_logic(rowk,colk,inumber)
                     IF (temp_array(cell_ij_logic_number) .ne. 1) CYCLE
                     temp_number=temp_number+1
                     temp_array1(temp_number)=cell_ij_logic_number
                   END DO
                   DO inumber=1,cell_ij_numbers
                     sudoku_logic(rowk,colk,inumber)=0
                   END DO
                   sudoku_logic_values(rowk,colk)=temp_number
                   DO inumber=1,temp_number
                     sudoku_logic(rowk,colk,inumber)=temp_array1(inumber)
                   END DO
                 END DO

                          END IF
                        END DO lloop11
                     END DO kloop11
                    END DO jloop11
                  END IF
                 END IF
                END IF
               END DO
              END DO
             END DO
            END DO
           END DO
          END DO


END SUBROUTINE Unique_rectangle_type3and2

!-----------------------------------------------------------------

SUBROUTINE Unique_rectangle_type4

  USE global_variables
  IMPLICIT NONE

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cell_ij_other_logic_number,cell_ij_other_numbers
  INTEGER :: check_number2,check_number4,check_square
  INTEGER,DIMENSION(9) :: check_value,temp_array_other
  INTEGER :: rowk,colk,kcell,lcell,mcell
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2
  INTEGER :: cell_j_logic_number,cell_j_numbers
  INTEGER :: cell_other_logic_number,cell_other_numbers
  INTEGER :: check_j,col_other,row_other,jj,kk,other_cell
  INTEGER :: cell_l_logic_number,cell_l_numbers,check_l,l_cell,lnumber
  INTEGER :: mm,cell_k_logic_number,cell_k_numbers,check_k
  INTEGER :: check_value_numbers,coll,rowl,colm,rowm,ll,check_number1
  INTEGER :: temp_number_other,check,check_again
  INTEGER,DIMENSION(naked_list_max,4,2) :: naked_list
  INTEGER,DIMENSION(9) :: naked_values
  INTEGER, pointer :: pa(:,:,:)
  INTEGER :: lower_state,cell_value,cage_cells,cage_sum_local
  INTEGER :: check1,total_permutations
  INTEGER :: j_cell,k_cell
  INTEGER,DIMENSION(9) :: temp_check,temp_array_new
  INTEGER :: temp_check_count,ii_cell,inaked,ncount
  INTEGER :: cage_cells1,rowk1,colk1

  

  ! Loop over horizontal cages
  DO i=1,nhorizontal_cages
    cage1h_real=horizontal_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.33'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
        check_square=0
      ELSE IF (cell_ij_numbers .gt. 2) THEN
        check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.34'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (check_square .eq. 0) THEN
          IF (cell_ij2_numbers .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (cell_ij2_numbers .lt. 3) CYCLE
        END IF
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        check_number1=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
          IF (temp_array2(ivalue) .eq. 1) THEN
            check_number1=check_number1+1
          END IF
        END DO
        IF (check_square .eq. 0) THEN
          IF (check_number .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (check_number .lt. 2) CYCLE
        END IF
        DO j=i+1,nhorizontal_cages
          cage2h_real=horizontal_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.35'
              STOP
            END IF
            IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                    (vertical_cage2 .ne. vertical_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (check_square .eq. 0) THEN
              IF (cell_ij3_numbers .lt. 3) CYCLE
            ELSE IF (check_square .eq. 1) THEN
              IF (cell_ij3_numbers .ne. 2) CYCLE
            END IF
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.36'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                      (vertical_cage2 .ne. vertical_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              temp_array3=temp_array2
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .lt. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                      temp_array3(cell_ij3_logic_number) + 1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                      temp_array3(cell_ij4_logic_number) + 1
              END DO
              check_number1=0
              check_number2=0
              check_number4=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 1) THEN
                  check_number1=check_number1+1
                END IF
                IF (temp_array3(ivalue) .eq. 2) THEN
                  check_number2=check_number2+1
                END IF
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number4=check_number4+1
		  check_value(check_number4)=ivalue
                END IF
              END DO
	      IF (check_number4 .eq. 2) THEN
		IF (cell_ij_numbers .gt. 2) THEN
		  temp_array=0
		  DO kcell=1,ncells1
                    rowk=cage_to_cells(cage1h_real,kcell,1)
                    colk=cage_to_cells(cage1h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
		    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
		    DO inumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
		      temp_array(cell_ij_other_logic_number) = &
			temp_array(cell_ij_other_logic_number)+1
		    END DO
		  END DO
		  check=0
!--------------------------------------------------------------
! check if check_value(1) is a sure digit for all the possible combinations..
!  take into account other numbers that are filled in and also naked pair, triples,quads etc

! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

 		  temp_check=0
		  temp_check_count=0
		  DO ii_cell=1,cage_cells
		     rowk=cage_to_cells(cage1h_real,ii_cell,1)
		     colk=cage_to_cells(cage1h_real,ii_cell,2)
		     IF (sudoku(rowk,colk) .gt. 0) THEN
			temp_check(sudoku(rowk,colk))=1
			temp_check_count=temp_check_count+1
		     END IF
		  END DO
!		  naked_list=0
!		  naked_values=0
!		  CALL Naked_pairs(cage1h_real,naked_list,naked_values)
!		  DO inaked=1,9
!		    IF (naked_values(inaked) .eq. 1) THEN
!			temp_check(inaked) = 1
!			 temp_check_count=temp_check_count+1
!		    END IF
!		  END DO

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
		 jj=check_value(1)
		 kk=check_value(2)
		 temp_array_new=0
		 ncount=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
		     IF (temp_check(cell_value) .eq. 1) THEN
			check1=check1+1
		     END IF
                   END DO
		   IF (temp_check_count .gt. 1) THEN
                    IF (check1 .eq. temp_check_count) THEN
		     ncount=ncount+1
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
		       temp_array_new(cell_value)=temp_array_new(cell_value)+1
                     END  DO
                    END IF
		   END IF
                 END DO

		IF (ncount .ge. 1) THEN
		 IF (temp_array_new(check_value(1)) .eq. ncount) THEN
!-------------------------------------------------------------
		  IF (temp_array(check_value(1)) .eq. 2) THEN
		    cell_ij_other_numbers=sudoku_logic_values(row1,col1)
		    DO inumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
		      IF (cell_ij_other_logic_number .eq. check_value(1)) THEN
			check=1
			EXIT
		      END IF
		    END DO
		   END IF
		  END IF
		  check_again=0
		  IF (temp_array_new(check_value(2)) .eq. ncount) THEN
		   IF (temp_array(check_value(2)) .eq. 2) THEN
		    cell_ij_other_numbers=sudoku_logic_values(row1,col1)
		    DO inumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
		      IF (cell_ij_other_logic_number .eq. check_value(2)) THEN
			check_again=1
			EXIT
		      END IF
                    END DO
		    IF ((check .eq. 1) .AND. (check_again .eq. 1)) THEN
		      RETURN
		    ELSE IF ((check .eq. 0) .AND. (check_again .eq. 1)) THEN
		      check=2
		    END IF
		   END IF
		  END IF
		END IF
		  temp_array_other=0
		  temp_number_other=0
		  cell_ij_other_numbers=sudoku_logic_values(row1,col1)
iloop1:           DO inumber=1,cell_ij_other_numbers
		    cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
		    IF (check .eq. 1) THEN
		      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE iloop1
		    ELSE IF (check .eq. 2) THEN
		      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE iloop1
		    END IF
		    temp_number_other=temp_number_other+1
		    temp_array_other(temp_number_other)=cell_ij_other_logic_number
		  END DO iloop1
		  DO inumber=1,cell_ij_other_numbers
		    sudoku_logic(row1,col1,inumber)=0
		  END DO
		  sudoku_logic_values(row1,col1)=temp_number_other
		  DO inumber=1,temp_number_other
		    sudoku_logic(row1,col1,inumber)=temp_array_other(inumber)
		  END DO
		  temp_array_other=0
		  temp_number_other=0
		  cell_ij_other_numbers=sudoku_logic_values(row2,col2)
iloop2:           DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row2,col2,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE iloop2
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE iloop2
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO iloop2
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row2,col2,inumber)=0
                  END DO
                  sudoku_logic_values(row2,col2)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row2,col2,inumber)=temp_array_other(inumber)
                  END DO

!-------- CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!
		  DO kcell=1,ncells1
		    temp_number_other=0
		    temp_array_other=0
                    rowk=cage_to_cells(cage1h_real,kcell,1)
                    colk=cage_to_cells(cage1h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
		    IF (colk .eq. col1) CYCLE
		    IF (colk .eq. col2) CYCLE
		    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
		    DO knumber=1,cell_ij_other_numbers
		      cell_ij_other_logic_number=sudoku_logic(rowk,colk,knumber)
		      IF (check .eq. 1) THEN
		        IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE
		      ELSE IF (check .eq. 2) THEN
			IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE
		      END IF
		      temp_number_other=temp_number_other+1
		      temp_array_other(temp_number_other)=cell_ij_other_logic_number
		    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number_other
                    DO inumber=1,temp_number_other
                      sudoku_logic(rowk,colk,inumber)=temp_array_other(inumber)
                    END DO
		  END DO
!------------- END WE MAY NOT NEED THIS BLOCK------------------------------------!

		ELSE IF (cell_ij_numbers .eq. 2) THEN
                  temp_array=0
                  DO kcell=1,ncells2
                    rowk=cage_to_cells(cage2h_real,kcell,1)
                    colk=cage_to_cells(cage2h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      temp_array(cell_ij_other_logic_number) = &
                        temp_array(cell_ij_other_logic_number)+1
                    END DO
                  END DO
                  check=0
!--------------------------------------------------------------
! check if check_value(1) is a sure digit for all the possible combinations..
!  take into account other numbers that are filled in and also naked pair, triples,quads etc

! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

                  temp_check=0
                  temp_check_count=0
                  DO ii_cell=1,cage_cells
                     rowk=cage_to_cells(cage2h_real,ii_cell,1)
                     colk=cage_to_cells(cage2h_real,ii_cell,2)
                     IF (sudoku(rowk,colk) .gt. 0) THEN
                        temp_check(sudoku(rowk,colk))=1
                        temp_check_count=temp_check_count+1
                     END IF
                  END DO
!                  naked_list=0
!                  naked_values=0
!                  CALL Naked_pairs(cage2h_real,naked_list,naked_values)
!                  DO inaked=1,9
!                    IF (naked_values(inaked) .eq. 1) THEN
!                        temp_check(inaked) = 1
!                         temp_check_count=temp_check_count+1
!                    END IF
!                  END DO

     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 jj=check_value(1)
                 kk=check_value(2)
                 temp_array_new=0
                 ncount=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (temp_check(cell_value) .eq. 1) THEN
                        check1=check1+1
                     END IF
                   END DO
                   IF (temp_check_count .gt. 1) THEN
                    IF (check1 .eq. temp_check_count) THEN
                     ncount=ncount+1
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array_new(cell_value)=temp_array_new(cell_value)+1
                     END  DO
                    END IF
                   END IF
                 END DO

		IF (ncount .ge. 1) THEN
                 IF (temp_array_new(check_value(1)) .eq. ncount) THEN
!-------------------------------------------------------------
                  IF (temp_array(check_value(1)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row11,col11)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(1)) THEN
                        check=1
                        EXIT
                      END IF
                    END DO
		   END IF
                  END IF
                  check_again=0
		  IF (temp_array_new(check_value(2)) .eq. ncount) THEN
                   IF (temp_array(check_value(2)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row11,col11)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(2)) THEN
                        check_again=1
                        EXIT
                      END IF
                    END DO
                    IF ((check .eq. 1) .AND. (check_again .eq. 1)) THEN
                      RETURN
                    ELSE IF ((check .eq. 0) .AND. (check_again .eq. 1)) THEN
                      check=2
                    END IF
                   END IF
		  END IF
		END IF
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row11,col11)
iloop11:           DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE iloop11
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE iloop11
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO iloop11
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row11,col11,inumber)=0
                  END DO
                  sudoku_logic_values(row11,col11)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row11,col11,inumber)=temp_array_other(inumber)
                  END DO
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row22,col22)
iloop22:           DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row22,col22,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE iloop22
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE iloop22
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO iloop22
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row22,col22,inumber)=0
                  END DO
                  sudoku_logic_values(row22,col22)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row22,col22,inumber)=temp_array_other(inumber)
                  END DO

!-------- CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!
                  DO kcell=1,ncells2
                    temp_number_other=0
                    temp_array_other=0
                    rowk=cage_to_cells(cage2h_real,kcell,1)
                    colk=cage_to_cells(cage2h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    IF (colk .eq. col11) CYCLE
                    IF (colk .eq. col22) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO knumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,knumber)
                      IF (check .eq. 1) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE
                      ELSE IF (check .eq. 2) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE
                      END IF
                      temp_number_other=temp_number_other+1
                      temp_array_other(temp_number_other)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number_other
                    DO inumber=1,temp_number_other
                      sudoku_logic(rowk,colk,inumber)=temp_array_other(inumber)
                    END DO
		  END DO
!-------- CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!

		END IF
	      END IF
	    END DO
           END DO
          END DO
	 END DO
        END DO
       END DO

  ! Loop over vertical cages
  DO i=1,nvertical_cages
    cage1h_real=vertical_cages(i)
    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1=cage_to_cells(cage1h_real,icell,1)
      col1=cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.37'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .lt. 2) CYCLE
      IF (cell_ij_numbers .eq. 2) THEN
        check_square=0
      ELSE IF (cell_ij_numbers .gt. 2) THEN
        check_square=1
      END IF
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.38'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (check_square .eq. 0) THEN
          IF (cell_ij2_numbers .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (cell_ij2_numbers .lt. 3) CYCLE
        END IF
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        check_number1=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
          IF (temp_array2(ivalue) .eq. 1) THEN
            check_number1=check_number1+1
          END IF
        END DO
        IF (check_square .eq. 0) THEN
          IF (check_number .ne. 2) CYCLE
        ELSE IF (check_square .eq. 1) THEN
          IF (check_number .lt. 2) CYCLE
        END IF
        DO j=i+1,nvertical_cages
          cage2h_real=vertical_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.39'
              STOP
            END IF
            IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                    (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (check_square .eq. 0) THEN
              IF (cell_ij3_numbers .lt. 3) CYCLE
            ELSE IF (check_square .eq. 1) THEN
              IF (cell_ij3_numbers .ne. 2) CYCLE
            END IF
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1.40'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                      (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              temp_array3=temp_array2
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (check_square .eq. 0) THEN
                IF (cell_ij4_numbers .lt. 3) CYCLE
              ELSE IF (check_square .eq. 1) THEN
                IF (cell_ij4_numbers .ne. 2) CYCLE
              END IF
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                      temp_array3(cell_ij3_logic_number) + 1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                      temp_array3(cell_ij4_logic_number) + 1
              END DO
              check_number1=0
              check_number2=0
              check_number4=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 1) THEN
                  check_number1=check_number1+1
                END IF
                IF (temp_array3(ivalue) .eq. 2) THEN
                  check_number2=check_number2+1
                END IF
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number4=check_number4+1
                  check_value(check_number4)=ivalue
                END IF
              END DO
              IF (check_number4 .eq. 2) THEN
                IF (cell_ij_numbers .gt. 2) THEN
                  temp_array=0
                  DO kcell=1,ncells1
                    rowk=cage_to_cells(cage1h_real,kcell,1)
                    colk=cage_to_cells(cage1h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      temp_array(cell_ij_other_logic_number) = &
                        temp_array(cell_ij_other_logic_number)+1
                    END DO
                  END DO
                  check=0
!--------------------------------------------------------------
! check if check_value(1) is a sure digit for all the possible combinations..
!  take into account other numbers that are filled in and also naked pair, triples,quads etc

! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells1
                  cage_sum_local=cage_sum(cage1h_real)

                  temp_check=0
                  temp_check_count=0
                  DO ii_cell=1,cage_cells
                     rowk=cage_to_cells(cage1h_real,ii_cell,1)
                     colk=cage_to_cells(cage1h_real,ii_cell,2)
                     IF (sudoku(rowk,colk) .gt. 0) THEN
                        temp_check(sudoku(rowk,colk))=1
                        temp_check_count=temp_check_count+1
                     END IF
                  END DO
!                  naked_list=0
!                  naked_values=0
!                  CALL Naked_pairs(cage1h_real,naked_list,naked_values)
!                  DO inaked=1,9
!                    IF (naked_values(inaked) .eq. 1) THEN
!                        temp_check(inaked) = 1
!                         temp_check_count=temp_check_count+1
!                    END IF
!                  END DO
     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 jj=check_value(1)
                 kk=check_value(2)
                 temp_array_new=0
                 ncount=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (temp_check(cell_value) .eq. 1) THEN
                        check1=check1+1
                     END IF
                   END DO
                   IF (temp_check_count .gt. 1) THEN
                    IF (check1 .eq. temp_check_count) THEN
                     ncount=ncount+1
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array_new(cell_value)=temp_array_new(cell_value)+1
                     END  DO
                    END IF
                   END IF
                 END DO


		IF (ncount .ge. 1) THEN
                 IF (temp_array_new(check_value(1)) .eq. ncount) THEN
!-------------------------------------------------------------
                  IF (temp_array(check_value(1)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row1,col1)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(1)) THEN
                        check=1
                        EXIT
                      END IF
                    END DO
		   END IF
                  END IF
!		END IF
                  check_again=0
		  IF (temp_array_new(check_value(2)) .eq. ncount) THEN
                   IF (temp_array(check_value(2)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row1,col1)
                    cell_ij_other_numbers=sudoku_logic_values(row1,col1)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(2)) THEN
                        check_again=1
                        EXIT
                      END IF
                    END DO
                    IF ((check .eq. 1) .AND. (check_again .eq. 1)) THEN
                      RETURN
                    ELSE IF ((check .eq. 0) .AND. (check_again .eq. 1)) THEN
                      check=2
                    END IF
		   END IF
                  END IF
		END IF
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row1,col1)
	          DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row1,col1,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE 
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE 
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO 
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row1,col1,inumber)=0
                  END DO
                  sudoku_logic_values(row1,col1)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row1,col1,inumber)=temp_array_other(inumber)
                  END DO
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row2,col2)
                  DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row2,col2,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE 
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE 
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO 
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row2,col2,inumber)=0
                  END DO
                  sudoku_logic_values(row2,col2)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row2,col2,inumber)=temp_array_other(inumber)
                  END DO

!-------- CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!
                  DO kcell=1,ncells1
                    temp_number_other=0
                    temp_array_other=0
                    rowk=cage_to_cells(cage1h_real,kcell,1)
                    colk=cage_to_cells(cage1h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    IF (rowk .eq. row1) CYCLE
                    IF (rowk .eq. row2) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO knumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,knumber)
                      IF (check .eq. 1) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE
                      ELSE IF (check .eq. 2) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE
                      END IF
                      temp_number_other=temp_number_other+1
                      temp_array_other(temp_number_other)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number_other
                    DO inumber=1,temp_number_other
                      sudoku_logic(rowk,colk,inumber)=temp_array_other(inumber)
                    END DO
                  END DO
!-------- END CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!

                ELSE IF (cell_ij_numbers .eq. 2) THEN
                  temp_array=0
                  DO kcell=1,ncells2
                    rowk=cage_to_cells(cage2h_real,kcell,1)
                    colk=cage_to_cells(cage2h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,inumber)
                      temp_array(cell_ij_other_logic_number) = &
                        temp_array(cell_ij_other_logic_number)+1
                    END DO
                  END DO
                  check=0
!--------------------------------------------------------------
! check if check_value(1) is a sure digit for all the possible combinations..
!  take into account other numbers that are filled in and also naked pair, triples,quads etc

! COM: find all possible combinations that contain these duple numbers.. remove
!      all the numbers that are not present in the combination
                  cage_cells=ncells2
                  cage_sum_local=cage_sum(cage2h_real)

                  temp_check=0
                  temp_check_count=0
                  DO ii_cell=1,cage_cells
                     rowk=cage_to_cells(cage2h_real,ii_cell,1)
                     colk=cage_to_cells(cage2h_real,ii_cell,2)
                     IF (sudoku(rowk,colk) .gt. 0) THEN
                        temp_check(sudoku(rowk,colk))=1
                        temp_check_count=temp_check_count+1
                     END IF
                  END DO
!                  naked_list=0
!                  naked_values=0
!                  CALL Naked_pairs(cage2h_real,naked_list,naked_values)
!                  DO inaked=1,9
!                    IF (naked_values(inaked) .eq. 1) THEN
!                        temp_check(inaked) = 1
!                         temp_check_count=temp_check_count+1
!                    END IF
!                  END DO
     IF (cage_cells .eq. 2) THEN
        pa => cell_2(3:17,:,:)
        lower_state = 3
        total_permutations=4
     ELSE IF (cage_cells .eq. 3) THEN
        pa => cell_3(6:24,:,:)
        lower_state = 6
        total_permutations=8
     ELSE IF (cage_cells .eq. 4) THEN
        pa => cell_4(10:30,:,:)
        lower_state = 10
        total_permutations=12
     ELSE IF (cage_cells .eq. 5) THEN
        pa => cell_5(15:35,:,:)
        lower_state = 15
        total_permutations=12
     ELSE IF (cage_cells .eq. 6) THEN
        pa => cell_6(21:39,:,:)
        lower_state = 21
        total_permutations=8
     ELSE IF (cage_cells .eq. 7) THEN
        pa => cell_7(28:42,:,:)
        lower_state = 28
        total_permutations=4
     ELSE IF (cage_cells .eq. 8) THEN
        pa => cell_8(36:44,:,:)
        lower_state = 36
        total_permutations=1
     ELSE IF (cage_cells .eq. 9) THEN
        pa => cell_9(45:45,:,:)
        lower_state = 45
        total_permutations=1
     ELSE
        write(*,*) 'cage cells not between 2 and 9'
        STOP
     END IF
                 jj=check_value(1)
                 kk=check_value(2)
                 temp_array_new=0
                 ncount=0
                 DO inumber=1,total_permutations
                   IF (pa(cage_sum_local-lower_state+1,inumber,1) .eq. 0) THEN
                     EXIT
                   END IF
                   check1=0
                   DO k_cell=1,cage_cells
                     cell_value=pa(cage_sum_local-lower_state+1,inumber,k_cell)
                     IF (temp_check(cell_value) .eq. 1) THEN
                        check1=check1+1
                     END IF
                   END DO
                   IF (temp_check_count .gt. 1) THEN
                    IF (check1 .eq. temp_check_count) THEN
                     ncount=ncount+1
                     DO j_cell=1,cage_cells
                       cell_value=pa(cage_sum_local-lower_state+1,inumber,j_cell)
                       temp_array_new(cell_value)=temp_array_new(cell_value)+1
                     END  DO
                    END IF
                   END IF
                 END DO

		IF (ncount .ge. 1) THEN
                 IF (temp_array_new(check_value(1)) .eq. ncount) THEN
!-------------------------------------------------------------
                  IF (temp_array(check_value(1)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row11,col11)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(1)) THEN
                        check=1
                        EXIT
                      END IF
                    END DO
		   END IF
                  END IF		
 		  check_again=0
		  IF (temp_array_new(check_value(2)) .eq. ncount) THEN
                   IF (temp_array(check_value(2)) .eq. 2) THEN
                    cell_ij_other_numbers=sudoku_logic_values(row11,col11)
                    DO inumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                      IF (cell_ij_other_logic_number .eq. check_value(2)) THEN
                        check_again=1
                        EXIT
                      END IF
                    END DO
                    IF ((check .eq. 1) .AND. (check_again .eq. 1)) THEN
                      RETURN
                    ELSE IF ((check .eq. 0) .AND. (check_again .eq. 1)) THEN
                      check=2
                    END IF
		   END IF
                  END IF
		END IF
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row11,col11)
                  DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row11,col11,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE 
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE 
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO 
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row11,col11,inumber)=0
                  END DO
                  sudoku_logic_values(row11,col11)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row11,col11,inumber)=temp_array_other(inumber)
                  END DO
                  temp_array_other=0
                  temp_number_other=0
                  cell_ij_other_numbers=sudoku_logic_values(row22,col22)
                  DO inumber=1,cell_ij_other_numbers
                    cell_ij_other_logic_number=sudoku_logic(row22,col22,inumber)
                    IF (check .eq. 1) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE 
                    ELSE IF (check .eq. 2) THEN
                      IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE 
                    END IF
                    temp_number_other=temp_number_other+1
                    temp_array_other(temp_number_other)=cell_ij_other_logic_number
                  END DO 
                  DO inumber=1,cell_ij_other_numbers
                    sudoku_logic(row22,col22,inumber)=0
                  END DO
                  sudoku_logic_values(row22,col22)=temp_number_other
                  DO inumber=1,temp_number_other
                    sudoku_logic(row22,col22,inumber)=temp_array_other(inumber)
                  END DO

!-------- CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!
                  DO kcell=1,ncells2
                    temp_number_other=0
                    temp_array_other=0
                    rowk=cage_to_cells(cage2h_real,kcell,1)
                    colk=cage_to_cells(cage2h_real,kcell,2)
                    IF (sudoku(rowk,colk) .ne. 0) CYCLE
                    IF (rowk .eq. row11) CYCLE
                    IF (rowk .eq. row22) CYCLE
                    cell_ij_other_numbers=sudoku_logic_values(rowk,colk)
                    DO knumber=1,cell_ij_other_numbers
                      cell_ij_other_logic_number=sudoku_logic(rowk,colk,knumber)
                      IF (check .eq. 1) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(1)) CYCLE
                      ELSE IF (check .eq. 2) THEN
                        IF (cell_ij_other_logic_number .eq. check_value(2)) CYCLE
                      END IF
                      temp_number_other=temp_number_other+1
                      temp_array_other(temp_number_other)=cell_ij_other_logic_number
                    END DO
                    DO inumber=1,cell_ij_other_numbers
                      sudoku_logic(rowk,colk,inumber)=0
                    END DO
                    sudoku_logic_values(rowk,colk)=temp_number_other
                    DO inumber=1,temp_number_other
                      sudoku_logic(rowk,colk,inumber)=temp_array_other(inumber)
                    END DO
                  END DO
!-------- END CHECK WE MAY NOT NEED THIS BLOCK --------------------------------------!

                END IF
              END IF
            END DO
           END DO
          END DO
         END DO
        END DO
       END DO


END SUBROUTINE Unique_rectangle_type4

!------------------------------------------------------------------

SUBROUTINE Unique_rectangle_situation(flag)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: flag

  INTEGER :: i_row,j_column1,j_column2,i,j
  INTEGER :: cell_ij_numbers,inumber,knumber,jnumber,temp_number
  INTEGER,DIMENSION(9) :: temp_array1,temp_array2,temp_array3,temp_array
  INTEGER :: cell_ij_logic_number,cell_ij2_logic_number
  INTEGER :: cell_ij3_logic_number,cell_ij4_logic_number
  INTEGER :: small_cell_row1,small_cell_row2,small_cell_column1,small_cell_column2
  INTEGER :: cell_ij2_numbers,cell_ij3_numbers,cell_ij4_numbers
  INTEGER :: i_row_other,check_total_numbers,check_number
  INTEGER :: i_row1,i_row2,j_column,j_column_other
  INTEGER :: small_cell_row3,small_cell_column3
  INTEGER :: row1,col1,row2,col2,row11,col11,row22,col22
  INTEGER :: icell,jcell,icell1,jcell1
  INTEGER :: cage1h,cage2h,cage1h_real,cage2h_real
  INTEGER :: ivalue,ncells1,ncells2
  INTEGER :: cage_temp1,cage_temp2
  INTEGER :: vertical_cage11,vertical_cage12,vertical_cage2
  INTEGER :: horizontal_cage11,horizontal_cage12,horizontal_cage2

  ! Loop over all the possible combinations of horizontal_2cages.. find 2 cells in each
  ! of the two cages that might form a unique rectangle.. then find 2 vertical cages
  ! that connect the 4 cells in the horizontal_2cages..

  flag = 0
  DO i=1,nhorizontal_cages
!    cage1h = horizontal_2cages(i,1)
!    cage2h = horizontal_2cages(i,2)
    cage1h_real = horizontal_cages(i)

    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        vertical_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .ne. 2) CYCLE
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          vertical_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (cell_ij2_numbers .ne. 2) CYCLE
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
        END DO
        IF (check_number .ne. 2) CYCLE
        IF ((cell_ij_numbers .eq. 2) .AND. (cell_ij2_numbers .eq. 2)) THEN
         DO j=i+1,nhorizontal_cages
          cage2h_real = horizontal_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((col11 .ne. col1) .AND. (col11 .ne. col2)) CYCLE
        ! check if 2 cells share a vertical cage.. the two horizontal cages with
        ! the 2 cells are connected by a vertical cage.. else CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              vertical_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
              STOP
            END IF
            IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                        (vertical_cage2 .ne. vertical_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (cell_ij3_numbers .ne. 2) CYCLE
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((col22 .ne. col1) .AND. (col22 .ne. col2)) CYCLE
        ! check if 2 cells share a vertical cage.. the two horizontal cages with
        ! the 2 cells are connected by a vertical cage.. else CYCLE
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                vertical_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
                STOP
              END IF
              IF ((vertical_cage2 .ne. vertical_cage11) .AND. &
                        (vertical_cage2 .ne. vertical_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              temp_array3=temp_array2
              check_total_numbers=0
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (cell_ij4_numbers .ne. 2) CYCLE
              IF (cell_ij_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij2_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij3_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij4_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (check_total_numbers .ne. 4) CYCLE
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number)+1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number)+1
              END DO

              check_number=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number=check_number+1
                END IF
              END DO
              IF (check_number .eq. 2) THEN
		flag=1
		RETURN
	      END IF
             END DO
            END DO
          END DO
         END IF
        END DO
      END DO
    END DO


   flag = 0
   ! Loop over the vertical_2cages
   DO i=1,nvertical_cages
!    cage1h = vertical_2cages(i,1)
!    cage2h = vertical_2cages(i,2)
    cage1h_real = vertical_cages(i)

    ncells1=cage_no_of_cells(cage1h_real)
    DO icell=1,ncells1
      row1 = cage_to_cells(cage1h_real,icell,1)
      col1 = cage_to_cells(cage1h_real,icell,2)
      cage_temp1=cell_to_cage(row1,col1,1)
      cage_temp2=cell_to_cage(row1,col1,2)
      IF (cage_temp1 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp2
      ELSE IF (cage_temp2 .eq. cage1h_real) THEN
        horizontal_cage11 = cage_temp1
      ELSE
        write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
        STOP
      END IF
      temp_array1=0
      IF (sudoku(row1,col1) .ne. 0) CYCLE
      cell_ij_numbers=sudoku_logic_values(row1,col1)
      IF (cell_ij_numbers .ne. 2) CYCLE
      DO inumber=1,cell_ij_numbers
        cell_ij_logic_number=sudoku_logic(row1,col1,inumber)
        temp_array1(cell_ij_logic_number)=temp_array1(cell_ij_logic_number)+1
      END DO
      DO jcell=icell+1,ncells1
        temp_array2=temp_array1
        row2 = cage_to_cells(cage1h_real,jcell,1)
        col2 = cage_to_cells(cage1h_real,jcell,2)
        cage_temp1=cell_to_cage(row2,col2,1)
        cage_temp2=cell_to_cage(row2,col2,2)
        IF (cage_temp1 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp2
        ELSE IF (cage_temp2 .eq. cage1h_real) THEN
          horizontal_cage12 = cage_temp1
        ELSE
          write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
          STOP
        END IF
        IF (sudoku(row2,col2) .ne. 0) CYCLE
        cell_ij2_numbers=sudoku_logic_values(row2,col2)
        IF (cell_ij2_numbers .ne. 2) CYCLE
        DO jnumber=1,cell_ij2_numbers
          cell_ij2_logic_number=sudoku_logic(row2,col2,jnumber)
          temp_array2(cell_ij2_logic_number)=temp_array2(cell_ij2_logic_number)+1
        END DO
        check_number=0
        DO ivalue=1,9
          IF (temp_array2(ivalue) .eq. 2) THEN
            check_number=check_number+1
          END IF
        END DO
        IF (check_number .ne. 2) CYCLE
        IF ((cell_ij_numbers .eq. 2) .AND. (cell_ij2_numbers .eq. 2)) THEN
         DO j=i+1,nvertical_cages
          cage2h_real=vertical_cages(j)
          ncells2=cage_no_of_cells(cage2h_real)
          DO icell1=1,ncells2
            row11=cage_to_cells(cage2h_real,icell1,1)
            col11=cage_to_cells(cage2h_real,icell1,2)
            IF ((row11 .ne. row1) .AND. (row11 .ne. row2)) CYCLE
            cage_temp1=cell_to_cage(row11,col11,1)
            cage_temp2=cell_to_cage(row11,col11,2)
            IF (cage_temp1 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp2
            ELSE IF (cage_temp2 .eq. cage2h_real) THEN
              horizontal_cage2 = cage_temp1
            ELSE
              write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
              STOP
            END IF
            IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
            IF (sudoku(row11,col11) .ne. 0) CYCLE
            cell_ij3_numbers=sudoku_logic_values(row11,col11)
            IF (cell_ij3_numbers .ne. 2) CYCLE
            DO jcell1=icell1+1,ncells2
              row22=cage_to_cells(cage2h_real,jcell1,1)
              col22=cage_to_cells(cage2h_real,jcell1,2)
              IF((row22 .ne. row1) .AND. (row22 .ne. row2)) CYCLE
              cage_temp1=cell_to_cage(row22,col22,1)
              cage_temp2=cell_to_cage(row22,col22,2)
              IF (cage_temp1 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp2
              ELSE IF (cage_temp2 .eq. cage2h_real) THEN
                horizontal_cage2 = cage_temp1
              ELSE
                write(*,*) 'SOMETHING IS wrong: Find_2permutation_horizontalcage -1'
                STOP
              END IF
              IF ((horizontal_cage2 .ne. horizontal_cage11) .AND. &
                        (horizontal_cage2 .ne. horizontal_cage12)) CYCLE
              IF (sudoku(row22,col22) .ne. 0) CYCLE
              ! We have 2 cells in cage2h_real that forms a rectangle
              !  with the 2 cells in cage1h_real.. check if they form
              !  a unique rectangle
              temp_array3=temp_array2
              check_total_numbers=0
              cell_ij4_numbers=sudoku_logic_values(row22,col22)
              IF (cell_ij4_numbers .ne. 2) CYCLE
              IF (cell_ij_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij2_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij3_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (cell_ij4_numbers .eq. 2) THEN
                check_total_numbers=check_total_numbers+1
              END IF
              IF (check_total_numbers .ne. 4) CYCLE
              DO knumber=1,cell_ij3_numbers
                cell_ij3_logic_number=sudoku_logic(row11,col11,knumber)
                temp_array3(cell_ij3_logic_number) = &
                        temp_array3(cell_ij3_logic_number)+1
              END DO
              DO knumber=1,cell_ij4_numbers
                cell_ij4_logic_number=sudoku_logic(row22,col22,knumber)
                temp_array3(cell_ij4_logic_number) = &
                        temp_array3(cell_ij4_logic_number)+1
              END DO

              check_number=0
              DO ivalue=1,9
                IF (temp_array3(ivalue) .eq. 4) THEN
                  check_number=check_number+1
                END IF
              END DO
              IF (check_number .eq. 2) THEN
		flag=1
		RETURN
	      END IF
             END DO
            END DO
          END DO
         END IF
        END DO
      END DO
    END DO



END SUBROUTINE Unique_rectangle_situation

!------------------------------------------------------------------
