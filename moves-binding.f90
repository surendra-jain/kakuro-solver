SUBROUTINE Binding_squares(icage)

  USE global_variables
  IMPLICIT NONE

  INTEGER :: icage
  INTEGER,DIMENSION(9) :: ascending_cells
  INTEGER :: ncells,cage_sum_local
  INTEGER,DIMENSION(9) :: values
  INTEGER :: icell,row1,col1,cell_logic_numbers,temp_number
  INTEGER :: inumber,cell_i_number,jnumber,cell_j_number
  INTEGER :: jcell,ascending_value,kcell
  INTEGER :: bind_squares,real_cell,real_row,real_col
  INTEGER :: count_sum
  INTEGER,DIMENSION(sum_array_max) :: sum_array
  INTEGER :: cell_logic_numbers1,inumber1,cell_logic_value1
  INTEGER :: cell_logic_numbers2,inumber2,cell_logic_value2
  INTEGER :: cell_logic_numbers3,inumber3,cell_logic_value3
  INTEGER :: cell_logic_numbers4,inumber4,cell_logic_value4
  INTEGER :: cell_logic_numbers5,inumber5,cell_logic_value5
  INTEGER :: cell_logic_numbers6,inumber6,cell_logic_value6
  INTEGER :: cell_logic_numbers7,inumber7,cell_logic_value7
  INTEGER :: cell_logic_numbers8,inumber8,cell_logic_value8
  INTEGER :: cell_logic_numbers9,inumber9,cell_logic_value9
  INTEGER :: count_values,ivalue,sum,min,max
  INTEGER,DIMENSION(9) :: temp_array
  INTEGER :: cell_numbers,cell_logic_number,knumber
  INTEGER :: row2,row3,row4,row5,row6,row7,row8,row9
  INTEGER :: col2,col3,col4,col5,col6,col7,col8,col9
  INTEGER :: i,j,lnumber
  

  ncells = cage_no_of_cells(icage)
  cage_sum_local = cage_sum(icage)

  DO icell=1,ncells
    row1 = cage_to_cells(icage,icell,1)
    col1 = cage_to_cells(icage,icell,2)
    IF (sudoku(row1,col1) .gt. 0) CYCLE
    cell_logic_numbers = sudoku_logic_values(row1,col1)
    temp_number = 11
    DO inumber =1,cell_logic_numbers-1
      cell_i_number = sudoku_logic(row1,col1,inumber)
      DO jnumber=inumber+1,cell_logic_numbers
	cell_j_number = sudoku_logic(row1,col1,jnumber)
        IF (cell_i_number .gt. cell_j_number) THEN
	  temp_number = cell_i_number
	  sudoku_logic(row1,col1,inumber) = cell_j_number
          sudoku_logic(row1,col1,jnumber) = temp_number
	  cell_i_number = cell_j_number
	END IF
       END DO
     END DO
   END DO


   ascending_cells = 0
   DO jcell=1,ncells
     ascending_value = 0
iloop: DO icell=1,ncells
	DO kcell=1,jcell
	  IF (ascending_cells(kcell) .eq. icell) CYCLE iloop
	END DO
       row1 = cage_to_cells(icage,icell,1)
       col1 = cage_to_cells(icage,icell,2)
       IF (sudoku(row1,col1) .gt. 0) CYCLE iloop
       temp_number = sudoku_logic_values(row1,col1)
       IF (temp_number .gt. ascending_value) THEN
	  ascending_value = temp_number
	  ascending_cells(jcell) = icell
       END IF
      END DO iloop
    END DO

    bind_squares = 0
    DO icell=1,ncells
      IF (ascending_cells(icell) .ne. 0) THEN
         bind_squares = bind_squares + 1
      END IF
    END DO
 
       DO kcell=1,bind_squares
	 real_cell = ascending_cells(kcell)
	 icell = real_cell
         real_row = cage_to_cells(icage,real_cell,1)
         real_col = cage_to_cells(icage,real_cell,2)
	 values = 0
	 count_sum = 0
	 sum_array = 0

 	cell_logic_value1 = 0
 	cell_logic_value2 = 0
 	cell_logic_value3 = 0
 	cell_logic_value4 = 0
 	cell_logic_value5 = 0
 	cell_logic_value6 = 0
 	cell_logic_value7 = 0
 	cell_logic_value8 = 0
 	cell_logic_value9 = 0

        IF (icell .eq. 1) GO TO 100
	cell_logic_value1 = 0
         row1 = cage_to_cells(icage,1,1)
         col1 = cage_to_cells(icage,1,2)
	 IF (sudoku(row1,col1) .gt. 0) THEN
	  values(sudoku(row1,col1)) = 1
	  GO TO 100
         END IF
         cell_logic_numbers1 = sudoku_logic_values(row1,col1)
         DO inumber1 = 1,cell_logic_numbers1
           cell_logic_value1 = sudoku_logic(row1,col1,inumber1)
           IF (values(cell_logic_value1) .eq. 1) THEN
	     CYCLE
	   ELSE
             values(cell_logic_value1) = 1
           END IF
100 CONTINUE           
	   IF (ncells .lt. 2) GO TO 108
	    row2 = cage_to_cells(icage,2,1)
	    col2 = cage_to_cells(icage,2,2)
           IF (icell .eq. 2) GO TO 101
	    row2 = cage_to_cells(icage,2,1)
	    col2 = cage_to_cells(icage,2,2)
            IF (sudoku(row2,col2) .gt. 0) THEN
	      values(sudoku(row2,col2)) = 1
	      GO TO 101
            END IF
            cell_logic_numbers2 = sudoku_logic_values(row2,col2)
	    DO inumber2 = 1,cell_logic_numbers2
	      cell_logic_value2 = sudoku_logic(row2,col2,inumber2)
	      IF (values(cell_logic_value2) .eq. 1) THEN
		CYCLE
	      ELSE
		values(cell_logic_value2) = 1
	      END IF
101 CONTINUE
	      IF (ncells .lt. 3) GO TO 108
 	      row3 = cage_to_cells(icage,3,1)
	      col3 = cage_to_cells(icage,3,2)
              IF (icell .eq. 3) GO TO 102
 	      row3 = cage_to_cells(icage,3,1)
	      col3 = cage_to_cells(icage,3,2)
	      IF (sudoku(row3,col3) .gt. 0) THEN
	        values(sudoku(row3,col3)) = 1
		GO TO 102
	      END IF
	      cell_logic_numbers3 = sudoku_logic_values(row3,col3)
	      DO inumber3 = 1,cell_logic_numbers3
		cell_logic_value3 = sudoku_logic(row3,col3,inumber3)
	        IF (values(cell_logic_value3) .eq. 1) THEN
		  CYCLE
		ELSE
		  values(cell_logic_value3) = 1
		END IF
102 CONTINUE
		IF (ncells .lt. 4) GO TO 108
		row4 = cage_to_cells(icage,4,1)
		col4 = cage_to_cells(icage,4,2)
		IF (icell .eq. 4) GO TO 103
		row4 = cage_to_cells(icage,4,1)
		col4 = cage_to_cells(icage,4,2)
		IF (sudoku(row4,col4) .gt. 0) THEN
		  values(sudoku(row4,col4)) = 1
		  GO TO 103
		END IF
		cell_logic_numbers4 = sudoku_logic_values(row4,col4)
		DO inumber4 = 1,cell_logic_numbers4
		  cell_logic_value4 = sudoku_logic(row4,col4,inumber4)
		  IF (values(cell_logic_value4) .eq. 1) THEN
		    CYCLE
		  ELSE
		    values(cell_logic_value4) = 1
		  END IF
103 CONTINUE
		  IF (ncells .lt. 5) GO TO 108
		  row5 = cage_to_cells(icage,5,1)
		  col5 = cage_to_cells(icage,5,2)
		  IF (icell .eq. 5) GO TO 104
		  row5 = cage_to_cells(icage,5,1)
		  col5 = cage_to_cells(icage,5,2)
		  IF (sudoku(row5,col5) .gt. 0) THEN
		    values(sudoku(row5,col5)) = 1
		    GO TO 104
		  END IF
		  cell_logic_numbers5 = sudoku_logic_values(row5,col5)
		  DO inumber5 = 1,cell_logic_numbers5
		    cell_logic_value5 = sudoku_logic(row5,col5,inumber5)
		    IF (values(cell_logic_value5) .eq. 1) THEN
		      CYCLE
		    ELSE
		      values(cell_logic_value5) = 1
		    END IF
104 CONTINUE
		    IF (ncells .lt. 6) GO TO 108
		    row6 = cage_to_cells(icage,6,1)
		    col6 = cage_to_cells(icage,6,2)
		    IF (icell .eq. 6) GO TO 105
		    row6 = cage_to_cells(icage,6,1)
		    col6 = cage_to_cells(icage,6,2)
		    IF (sudoku(row6,col6) .gt. 0) THEN
		      values(sudoku(row6,col6)) = 1
		      GO TO 105
		    END IF
		    cell_logic_numbers6 = sudoku_logic_values(row6,col6)
		    DO inumber6 = 1,cell_logic_numbers6
		      cell_logic_value6 = sudoku_logic(row6,col6,inumber6)
		      IF (values(cell_logic_value6) .eq. 1) THEN
			CYCLE
		      ELSE
			values(cell_logic_value6) = 1
		      END IF
105 CONTINUE
		      IF (ncells .lt. 7) GO TO 108
		      row7 = cage_to_cells(icage,7,1)
		      col7 = cage_to_cells(icage,7,2)
		      IF (icell .eq. 7) GO TO 106
		      row7 = cage_to_cells(icage,7,1)
		      col7 = cage_to_cells(icage,7,2)
		      IF (sudoku(row7,col7) .gt. 0) THEN
			values(sudoku(row7,col7)) = 1
			GO TO 106
		      END IF
		      cell_logic_numbers7 = sudoku_logic_values(row7,col7)
		      DO inumber7 = 1,cell_logic_numbers7
			cell_logic_value7 = sudoku_logic(row7,col7,inumber7)
			IF (values(cell_logic_value7) .eq. 1) THEN
			  CYCLE
			ELSE
			  values(cell_logic_value7) = 1
			END IF
106 CONTINUE
			IF (ncells .lt. 8) GO TO 108
			row8 = cage_to_cells(icage,8,1)
			col8 = cage_to_cells(icage,8,2)
			IF (icell .eq. 8) GO TO 107
			row8 = cage_to_cells(icage,8,1)
			col8 = cage_to_cells(icage,8,2)
		 	IF (sudoku(row8,col8) .gt. 0) THEN
			  values(sudoku(row8,col8)) = 1
			  GO TO 107
			END IF
			cell_logic_numbers8 = sudoku_logic_values(row8,col8)
			DO inumber8 = 1,cell_logic_numbers8
			  cell_logic_value8 = sudoku_logic(row8,col8,inumber8)
			  IF (values(cell_logic_value8) .eq. 1) THEN
			    CYCLE
			  ELSE
			    values(cell_logic_value8) = 1
			  END IF
107 CONTINUE
			  IF (ncells .lt. 9) GO TO 108
			  row9 = cage_to_cells(icage,9,1)
			  col9 = cage_to_cells(icage,9,2)
			  IF (icell .eq. 9) GO TO 108
			  row9 = cage_to_cells(icage,9,1)
			  col9 = cage_to_cells(icage,9,2)
			  IF (sudoku(row9,col9) .gt. 0) THEN
			    values(sudoku(row9,col9)) = 1
			    GO TO 108
			  END IF
			  cell_logic_numbers9 = sudoku_logic_values(row9,col9)
			  DO inumber9 = 1,cell_logic_numbers9
			    cell_logic_value9 = sudoku_logic(row9,col9,inumber9)
			    IF (values(cell_logic_value9) .eq. 1) THEN
			      CYCLE
			    ELSE
			      values(cell_logic_value9) = 1
			    END IF
108 CONTINUE
			    count_values = 0
			    DO ivalue=1,9
			      IF (values(ivalue) .eq. 1) THEN
				count_values = count_values + 1
			      END IF
			    END DO
			    IF (count_values .eq. ncells-1) THEN
			      sum = 0
			      DO ivalue=1,9
				IF (values(ivalue) .eq. 1) THEN
				  sum = sum + ivalue
				END IF
			      END DO
			    IF (sum .lt. cage_sum_local) THEN
			      DO knumber = 1,sudoku_logic_values(real_row,real_col) 
				lnumber = sudoku_logic(real_row,real_col,knumber)
				IF (sum+lnumber .eq. cage_sum_local) THEN
				  count_sum = count_sum + 1
				  IF (count_sum .ge. sum_array_max) THEN
					write(*,*) 'sum_array exceeded'
					write(*,*) 'increse the size of sum_array_max'
					write(*,*) 'count_sum is :',count_sum
					STOP
				  END IF
			          sum_array(count_sum) = sum	
				END IF
			      END DO
				
			    END IF
			    END IF
			    

			    IF (ncells .eq. 8) GO TO 109
			    IF (ncells .eq. 7) GO TO 110
			    IF (ncells .eq. 6) GO TO 111
			    IF (ncells .eq. 5) GO TO 112
			    IF (ncells .eq. 4) GO TO 113
			    IF (ncells .eq. 3) GO TO 114
			    IF (ncells .eq. 2) GO TO 115
			    IF (ncells .eq. 1) GO TO 116


			    IF (sudoku(row9,col9) .gt. 0) GO TO 109
			    IF (icell .eq. 9) GO TO 109
			    values(cell_logic_value9) = 0
			   END DO
109 CONTINUE
			    IF (sudoku(row8,col8) .gt. 0) GO TO 110
			   IF (icell .eq. 8) GO TO 110 
		           values(cell_logic_value8) = 0
		          END DO 
110 CONTINUE 
			    IF (sudoku(row7,col7) .gt. 0) GO TO 111
			  IF (icell .eq. 7) GO TO 111
			  values(cell_logic_value7) = 0
			 END DO
111 CONTINUE
			    IF (sudoku(row6,col6) .gt. 0) GO TO 112
			 IF (icell .eq. 6) GO TO 112
		         values(cell_logic_value6) = 0
			END DO
112 CONTINUE
			    IF (sudoku(row5,col5) .gt. 0) GO TO 113
			IF (icell .eq. 5) GO TO 113
		        values(cell_logic_value5) = 0
		       END DO
113 CONTINUE
			    IF (sudoku(row4,col4) .gt. 0) GO TO 114
		       IF (icell .eq. 4) GO TO 114
		       values(cell_logic_value4) = 0
		      END DO
114 CONTINUE
			    IF (sudoku(row3,col3) .gt. 0) GO TO 115
		      IF (icell .eq. 3) GO TO 115
		      values(cell_logic_value3) = 0
		     END DO
115 CONTINUE
			    IF (sudoku(row2,col2) .gt. 0) GO TO 116
		     IF (icell .eq. 2) GO TO 116
		     values(cell_logic_value2) = 0
		    END DO
116 CONTINUE
			    IF (sudoku(row1,col1) .gt. 0) GO TO 117
		    IF (icell .eq. 1) GO TO 117
		    values(cell_logic_value1) = 0
		   END DO
117 CONTINUE

	min = 100
	max = 0
	IF (count_sum .le. 0) CYCLE
	CALL Find_min_max(cage_sum_local,count_sum,sum_array,min,max)
	temp_number = 0
	temp_array = 0
	cell_numbers = sudoku_logic_values(real_row,real_col)
	DO knumber=1,cell_numbers
	  cell_logic_number = sudoku_logic(real_row,real_col,knumber)
	  IF (cell_logic_number .lt. min) CYCLE
	  IF (cell_logic_number .gt. max) CYCLE
	  temp_number = temp_number + 1
	  temp_array(temp_number) = cell_logic_number
	END DO
      IF (temp_number .gt. 0) THEN
	IF (temp_number .lt. cell_numbers) THEN
                    DO inumber=1,sudoku_logic_values(real_row,real_col)
                      sudoku_logic(real_row,real_col,inumber) = 0
                    END DO
                    sudoku_logic_values(real_row,real_col) = temp_number
                    DO inumber=1,temp_number
                      sudoku_logic(real_row,real_col,inumber) = temp_array(inumber)
                    END DO

	END IF
      END IF

  END DO


END SUBROUTINE Binding_squares

!------------------------------------------------

SUBROUTINE Find_min_max(cage_sum_local,count_sum,sum_array,min,max)

   USE global_variables
   IMPLICIT NONE

   INTEGER :: cage_sum_local,count_sum,min,max
   INTEGER,DIMENSION(sum_array_max) :: sum_array
   INTEGER :: i,value_sum_i,j,value_sum_j

	 IF (count_sum .ge. sum_array_max) THEN
                 write(*,*) 'sum_array exceeded'
                 write(*,*) 'increse the size of sum_array_max'
                 write(*,*) 'count_sum is :',count_sum
                 STOP
         END IF


   DO i=1,count_sum-1
     value_sum_i = sum_array(i)
     DO j=i+1,count_sum
       value_sum_j = sum_array(j)
        IF (value_sum_i .gt. value_sum_j) THEN
	  sum_array(i) = value_sum_j
	  sum_array(j) = value_sum_i
	  value_sum_i = value_sum_j
	END IF
      END DO
    END DO

   min = cage_sum_local - sum_array(count_sum)
   max = cage_sum_local - sum_array(1)



END SUBROUTINE Find_min_max

!------------------------------------------------
